#' Consumption Functions for FB4 Model
#'
#' @name consumption-functions
#' @aliases consumption-functions
NULL

# ============================================================================
# LOW-LEVEL FUNCTIONS
# ============================================================================

#' Temperature function for consumption - Equation 1 (Low-level)
#'
#' Implements temperature equation 1 (simple exponential)
#'
#' @param temperature Water temperature (deg C)
#' @param CQ Temperature coefficient for consumption
#' @return Temperature factor
#' @keywords internal
consumption_temp_eq1 <- function(temperature, CQ) {
  ft <- safe_exp(CQ * temperature, param_name = "Temperature function for consumption - Eq 1")
  return(ft)
}

#' Temperature function for consumption - Equation 2 (Low-level)
#'
#' Implements temperature equation 2 (Kitchell et al. 1977)
#'
#' @param temperature Water temperature (deg C)
#' @param CTM Maximum temperature where consumption ceases
#' @param CTO Laboratory optimum temperature
#' @param CX Calculated parameter
#' @param warn Logical, whether to issue warnings about calculations, default TRUE
#' 
#' @return Temperature factor
#' 
#' @details
#' 
#' Special cases:
#' - When temperature ≥ CTM: returns 0 (typically approximated by the upper incipient lethal temperature)
#' - When V ≤ 0: returns 0 (mathematical fallback)
#' 
#' @keywords internal
consumption_temp_eq2 <- function(temperature, CTM, CTO, CX, warn = TRUE) {
  if (temperature >= CTM) {
    if (warn) {
      warning("consumption_temp_eq2: temperature (", temperature, "°C) ≥ CTM (", CTM, "°C), returning 0 (consumption ceases)", call. = FALSE)
    }
    return(0)
  }
  
  V <- (CTM - temperature) / (CTM - CTO)
  
  ft <- V^CX * safe_exp(CX * (1 - V), warn = warn, param_name = "Temperature function for consumption - Eq 2")
  
  return(pmax(0, ft))
}

#' Temperature function for consumption - Equation 3 (Low-level)
#'
#' Implements temperature equation 3 (Thornton and Lessem 1978)
#'
#' @param temperature Water temperature (deg C)
#' @param CQ Temperature coefficient
#' @param CG1 Calculated parameter 1
#' @param CK1 Small fraction of maximum rate
#' @param CG2 Calculated parameter 2
#' @param CTL Temperature with reduced rate
#' @param CK4 Reduced fraction of maximum rate
#' @return Temperature factor
#' @keywords internal
consumption_temp_eq3 <- function(temperature, CQ, CG1, CK1, CG2, CTL, CK4) {

  # Calculate first component
  L1 <- safe_exp(CG1 * (temperature - CQ), param_name = "L1 in Temperature function for consumption - Eq 3")
  KA <- (CK1 * L1) / (1 + CK1 * (L1 - 1))
  
  # Calculate second component
  L2 <- safe_exp(CG2 * (CTL - temperature), param_name = "L2 in Temperature function for consumption - Eq 3")
  KB <- (CK4 * L2) / (1 + CK4 * (L2 - 1))
  
  ft <- KA * KB
  return(ft)
}

#' Temperature function for consumption - Equation 4 (Low-level)
#'
#' Implements temperature equation 4 (polynomial)
#'
#' @param temperature Water temperature (deg C)
#' @param CQ Linear coefficient
#' @param CK1 Quadratic coefficient
#' @param CK4 Cubic coefficient
#' @return Temperature factor
#' @keywords internal
consumption_temp_eq4 <- function(temperature, CQ, CK1, CK4) {
  exponent <- CQ * temperature + CK1 * temperature^2 + CK4 * temperature^3
  ft <- safe_exp(exponent, param_name = "Temperature function for consumption - Eq 4")
  return(ft)
}

# ============================================================================
# MID-LEVEL FUNCTIONS: Coordination and Business Logic
# ============================================================================

#' Calculate temperature factor for consumption (Mid-level)
#'
#' Coordinates temperature equation selection and calculation
#'
#' @param temperature Water temperature (°C)
#' @param processed_consumption_params List with processed consumption parameters
#' @return Temperature factor
#' @keywords internal
calculate_temperature_factor_consumption <- function(temperature, processed_consumption_params) {
  
  CEQ <- processed_consumption_params$CEQ
  
  if (CEQ == 1) {
    return(consumption_temp_eq1(temperature, processed_consumption_params$CQ))
    
  } else if (CEQ == 2) {
    return(consumption_temp_eq2(temperature, processed_consumption_params$CTM, 
                                processed_consumption_params$CTO, processed_consumption_params$CX))
    
  } else if (CEQ == 3) {
    return(consumption_temp_eq3(temperature, processed_consumption_params$CQ, 
                                processed_consumption_params$CG1, processed_consumption_params$CK1,
                                processed_consumption_params$CG2, processed_consumption_params$CTL, 
                                processed_consumption_params$CK4))
    
  } else if (CEQ == 4) {
    return(consumption_temp_eq4(temperature, processed_consumption_params$CQ, 
                                processed_consumption_params$CK1, processed_consumption_params$CK4))
  }
}

#' Calculate daily consumption (Mid-level - Main function)
#'
#' Main consumption calculation function called from simulation loop
#'
#' @param temperature Water temperature (°C)
#' @param weight Fish weight (g)
#' @param p_value Proportion of maximum consumption (0-5)
#' @param processed_consumption_params List with processed consumption parameters
#' @param method Calculation method ("maximum", "rate", "specific")
#' @return Specific consumption (g prey/g fish/day)
#' @export
calculate_consumption <- function(temperature, weight, p_value, processed_consumption_params, method = "rate") {
  
  # Calculate maximum consumption
  Cmax <- processed_consumption_params$CA * weight^processed_consumption_params$CB
  
  # Calculate temperature factor
  ft <- calculate_temperature_factor_consumption(temperature, processed_consumption_params)
  
  # Apply method logic
  if (method == "maximum") {
    return(Cmax * ft)
  } else if (method == "rate") {
    return(Cmax * p_value * ft)
  } else if (method == "specific") {
    return(Cmax * ft)
  }
  
  # Default: rate method
  consumption_rate <- Cmax * p_value * ft
  return(pmax(0, consumption_rate))
}

#' Calculate additional parameters for consumption equation 2 (Mid-level)
#'
#' Calculates derived parameters needed for temperature equation 2
#'
#' @param CQ Temperature coefficient
#' @param CTM Maximum temperature
#' @param CTO Optimum temperature
#' @return List with CY, CZ, CX
#' @keywords internal
calculate_consumption_params_eq2 <- function(CQ, CTM, CTO, warn = TRUE) {
  CY <- log(CQ) * (CTM - CTO + 2)
  CZ <- log(CQ) * (CTM - CTO)
  
  # Safe calculation of CX
  if (CY == 0) {
    if (warn) {
      warning("calculate_consumption_params_eq2: CY = 0, setting CX = 1 (no thermal dependence)", call. = FALSE)
    }
    CX <- 1
  } else {
    discriminant <- 1 + 40/CY
    if (discriminant < 0) {
      if (warn) {
        warning("calculate_consumption_params_eq2: discriminant (1 + 40/CY) < 0, setting CX = 1 as mathematical fallback", call. = FALSE)
      }
      CX <- 1
    } else {
      CX <- (CZ^2 * (1 + safe_sqrt(discriminant, param_name = "CX for Consumption Equation 2"))^2) / 400
    }
  }
  
  return(list(CY = CY, CZ = CZ, CX = CX))
}

#' Calculate additional parameters for consumption equation 3 (Mid-level)
#'
#' Calculates derived parameters needed for temperature equation 3
#'
#' @param CTO Optimum temperature
#' @param CQ Temperature coefficient
#' @param CTL Temperature with reduced rate
#' @param CTM Maximum temperature
#' @param CK1 Small fraction of maximum rate
#' @param CK4 Reduced fraction of maximum rate
#' @return List with CG1, CG2
#' @keywords internal
calculate_consumption_params_eq3 <- function(CTO, CQ, CTL, CTM, CK1, CK4) {

  # Safe calculations
  numerator1 <- 0.98 * (1 - CK1)
  denominator1 <- CK1 * 0.02
  
  numerator2 <- 0.98 * (1 - CK4)
  denominator2 <- CK4 * 0.02
  
  if (denominator1 <= 0 || denominator2 <= 0) {
    stop("CK1 or CK4 parameters cause division by zero", call. = FALSE)
  }
  
  CG1 <- (1/(CTO - CQ)) * log(numerator1 / denominator1)
  CG2 <- (1/(CTL - CTM)) * log(numerator2 / denominator2)
  
  return(list(CG1 = CG1, CG2 = CG2))
}