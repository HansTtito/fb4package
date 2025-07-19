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
#' @param temperature Water temperature (°C)
#' @param CQ Temperature coefficient for consumption
#' @return Temperature factor
#' @keywords internal
consumption_temp_eq1 <- function(temperature, CQ) {
  safe_temp <- clamp(temperature, -50, 50)
  safe_CQ <- clamp(CQ, -1, 1)
  ft <- safe_exp(safe_CQ * safe_temp)
  return(ft)
}

#' Temperature function for consumption - Equation 2 (Low-level)
#'
#' Implements temperature equation 2 (Kitchell et al. 1977)
#'
#' @param temperature Water temperature (°C)
#' @param CTM Maximum temperature where consumption ceases
#' @param CTO Laboratory optimum temperature
#' @param CX Calculated parameter
#' @return Temperature factor
#' @keywords internal
consumption_temp_eq2 <- function(temperature, CTM, CTO, CX) {
  if (temperature >= CTM) return(0)
  
  V <- (CTM - temperature) / (CTM - CTO)
  if (V <= 0) return(0)
  
  safe_CX <- clamp(CX, 0, 10)
  ft <- V^safe_CX * safe_exp(safe_CX * (1 - V))
  
  return(pmax(0, ft))
}

#' Temperature function for consumption - Equation 3 (Low-level)
#'
#' Implements temperature equation 3 (Thornton and Lessem 1978)
#'
#' @param temperature Water temperature (°C)
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
  L1 <- safe_exp(CG1 * (temperature - CQ))
  safe_CK1 <- clamp(CK1, 0.001, 0.999)
  KA <- (safe_CK1 * L1) / (1 + safe_CK1 * (L1 - 1))
  
  # Calculate second component
  L2 <- safe_exp(CG2 * (CTL - temperature))
  safe_CK4 <- clamp(CK4, 0.001, 0.999)
  KB <- (safe_CK4 * L2) / (1 + safe_CK4 * (L2 - 1))
  
  ft <- KA * KB
  return(pmax(0, ft))
}

#' Temperature function for consumption - Equation 4 (Low-level)
#'
#' Implements temperature equation 4 (polynomial)
#'
#' @param temperature Water temperature (°C)
#' @param CQ Linear coefficient
#' @param CK1 Quadratic coefficient
#' @param CK4 Cubic coefficient
#' @return Temperature factor
#' @keywords internal
consumption_temp_eq4 <- function(temperature, CQ, CK1, CK4) {
  safe_temp <- clamp(temperature, -50, 50)
  exponent <- CQ * safe_temp + CK1 * safe_temp^2 + CK4 * safe_temp^3
  ft <- safe_exp(exponent)
  return(pmax(0, ft))
}

# ============================================================================
# MID-LEVEL FUNCTIONS: Coordination and Business Logic
# ============================================================================

#' Calculate temperature factor for consumption (Mid-level)
#'
#' Coordinates temperature equation selection and calculation
#'
#' @param temperature Water temperature (°C)
#' @param consumption_params List with consumption parameters
#' @return Temperature factor
#' @keywords internal
calculate_temperature_factor_consumption <- function(temperature, consumption_params) {
  CEQ <- consumption_params$CEQ %||% 1
  
  if (CEQ == 1) {
    CQ <- consumption_params$CQ
    return(consumption_temp_eq1(temperature, CQ))
    
  } else if (CEQ == 2) {
    CTM <- consumption_params$CTM
    CTO <- consumption_params$CTO
    CX <- consumption_params$CX
    
    # Calculate CX if not available
    if (is.null(CX) || is.na(CX)) {
      CQ <- consumption_params$CQ
      extra_params <- calculate_consumption_params_eq2(CQ, CTM, CTO)
      CX <- extra_params$CX
    }
    
    return(consumption_temp_eq2(temperature, CTM, CTO, CX))
    
  } else if (CEQ == 3) {
    CQ <- consumption_params$CQ
    CG1 <- consumption_params$CG1
    CK1 <- consumption_params$CK1
    CG2 <- consumption_params$CG2
    CTL <- consumption_params$CTL
    CK4 <- consumption_params$CK4
    
    # Calculate CG1 and CG2 if not available
    if (is.null(CG1) || is.null(CG2) || is.na(CG1) || is.na(CG2)) {
      CTO <- consumption_params$CTO
      CTM <- consumption_params$CTM
      extra_params <- calculate_consumption_params_eq3(CTO, CQ, CTL, CTM, CK1, CK4)
      CG1 <- extra_params$CG1
      CG2 <- extra_params$CG2
    }
    
    return(consumption_temp_eq3(temperature, CQ, CG1, CK1, CG2, CTL, CK4))
    
  } else if (CEQ == 4) {
    CQ <- consumption_params$CQ
    CK1 <- consumption_params$CK1
    CK4 <- consumption_params$CK4
    
    return(consumption_temp_eq4(temperature, CQ, CK1, CK4))
    
  } else {
    # Fallback to equation 1
    CQ <- consumption_params$CQ %||% 0.1
    return(consumption_temp_eq1(temperature, CQ))
  }
}

#' Calculate daily consumption (Mid-level - Main function)
#'
#' Main consumption calculation function called from simulation loop
#'
#' @param temperature Water temperature (°C)
#' @param weight Fish weight (g)
#' @param p_value Proportion of maximum consumption (0-5)
#' @param consumption_params List with consumption parameters
#' @param method Calculation method ("maximum", "rate", "specific")
#' @return Specific consumption (g prey/g fish/day)
#' @export
calculate_consumption <- function(temperature, weight, p_value, consumption_params, method = "rate") {

  # Extract parameters (assume they exist and are valid)
  CA <- consumption_params$CA
  CB <- consumption_params$CB
  
  # Calculate maximum consumption
  Cmax <- CA * weight^CB
  
  # Calculate temperature factor
  ft <- calculate_temperature_factor_consumption(temperature, consumption_params)
  
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
#' @export
calculate_consumption_params_eq2 <- function(CQ, CTM, CTO) {
  if (any(is.na(c(CQ, CTM, CTO)))) {
    stop("All parameters must be valid (not NA)", call. = FALSE)
  }
  
  if (CTM <= CTO) {
    stop("CTM must be greater than CTO", call. = FALSE)
  }
  
  if (CQ <= 0) {
    stop("CQ must be positive", call. = FALSE)
  }
  
  CY <- log(CQ) * (CTM - CTO + 2)
  CZ <- log(CQ) * (CTM - CTO)
  
  # Safe calculation of CX
  if (CY == 0) {
    CX <- 1
  } else {
    discriminant <- 1 + 40/CY
    if (discriminant < 0) {
      CX <- 1
    } else {
      CX <- (CZ^2 * (1 + safe_sqrt(discriminant))^2) / 400
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
#' @export
calculate_consumption_params_eq3 <- function(CTO, CQ, CTL, CTM, CK1, CK4) {
  if (any(is.na(c(CTO, CQ, CTL, CTM, CK1, CK4)))) {
    stop("All parameters must be valid (not NA)", call. = FALSE)
  }
  
  if (CTO <= CQ) {
    stop("CTO must be greater than CQ", call. = FALSE)
  }
  
  if (CTL <= CTM) {
    stop("CTL must be greater than CTM", call. = FALSE)
  }
  
  # Limit CK1 and CK4 to avoid numerical problems
  safe_CK1 <- clamp(CK1, 0.001, 0.999)
  safe_CK4 <- clamp(CK4, 0.001, 0.999)
  
  # Safe calculations
  numerator1 <- 0.98 * (1 - safe_CK1)
  denominator1 <- safe_CK1 * 0.02
  
  numerator2 <- 0.98 * (1 - safe_CK4)
  denominator2 <- safe_CK4 * 0.02
  
  if (denominator1 <= 0 || denominator2 <= 0) {
    stop("CK1 or CK4 parameters cause division by zero", call. = FALSE)
  }
  
  CG1 <- (1/(CTO - CQ)) * log(numerator1 / denominator1)
  CG2 <- (1/(CTL - CTM)) * log(numerator2 / denominator2)
  
  return(list(CG1 = CG1, CG2 = CG2))
}