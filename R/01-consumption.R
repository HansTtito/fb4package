#' Consumption Functions for FB4 Model
#'
#' @description
#' Functions implementing the four consumption temperature-dependence equations
#' (CEQ 1–4) and the allometric maximum-consumption function used in FB4.
#' Consumption is modelled as:
#'
#' \deqn{C = C_{\max} \cdot p \cdot F(T), \quad C_{\max} = CA \cdot W^{CB}}
#'
#' where \eqn{p} is the proportion of maximum consumption (P-value), \eqn{F(T)}
#' is a temperature-dependence function, \eqn{W} is body mass (g), and \eqn{CA},
#' \eqn{CB} are species-specific intercept and slope coefficients.
#'
#' \strong{CEQ 1} — simple Q10 exponential: \eqn{F(T) = e^{CQ \cdot T}}
#'
#' \strong{CEQ 2} — Kitchell et al. (1977):
#' \eqn{F(T) = V^{CX} \cdot e^{CX(1-V)}}, where \eqn{V = (CTM - T)/(CTM - CTO)}
#'
#' \strong{CEQ 3} — Thornton and Lessem (1978): two-part sigmoid using \eqn{CQ},
#' \eqn{CTO}, \eqn{CTL}, \eqn{CTM}, \eqn{CK1}, \eqn{CK4}
#'
#' \strong{CEQ 4} — polynomial: \eqn{F(T) = e^{CQ \cdot T + CK1 \cdot T^2 + CK4 \cdot T^3}}
#'
#' @references
#' Kitchell, J.F., Stewart, D.J. and Weininger, D. (1977). Applications of a
#' bioenergetics model to yellow perch and walleye.
#' \emph{Journal of the Fisheries Research Board of Canada}, 34(10), 1922–1935.
#' \doi{10.1139/f77-258}
#'
#' Thornton, K.W. and Lessem, A.S. (1978). A temperature algorithm for modifying
#' biological rates.
#' \emph{Transactions of the American Fisheries Society}, 107(2), 284–287.
#'
#' Hartman, K.J. and Hayward, R.S. (2007). Bioenergetics. In C.S. Guy and
#' M.L. Brown (eds.), \emph{Analysis and Interpretation of Freshwater Fisheries
#' Data}. American Fisheries Society, Bethesda, MD.
#'
#' Deslauriers, D., Chipps, S.R., Breck, J.E., Rice, J.A. and Madenjian, C.P.
#' (2017). Fish Bioenergetics 4.0: An R-based modeling application.
#' \emph{Fisheries}, 42(11), 586–596. \doi{10.1080/03632415.2017.1377558}
#'
#' @return No return value; this page documents the consumption functions module. See individual function documentation for return values.
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
#' - When temperature >= CTM: returns 0 (typically approximated by the upper incipient lethal temperature)
#' - When V <= 0: returns 0 (mathematical fallback)
#' 
#' @keywords internal
consumption_temp_eq2 <- function(temperature, CTM, CTO, CX, warn = TRUE) {
  if (temperature >= CTM) {
    if (warn) {
      warning("consumption_temp_eq2: temperature (", temperature, "\u00b0C) \u2265 CTM (", CTM, "\u00b0C), returning 0 (consumption ceases)", call. = FALSE)
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
  } else {
    stop("calculate_temperature_factor_consumption: unrecognized CEQ value (", CEQ,
         "). Must be 1, 2, 3, or 4.", call. = FALSE)
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
#' @return A non-negative numeric scalar giving the daily specific consumption
#'   rate in g prey per g fish per day. Returns \code{0} when the
#'   temperature-dependence factor is zero (e.g. temperature \eqn{\ge} CTM in
#'   equation 2). The value depends on \code{method}: \code{"rate"} (default)
#'   scales by \code{p_value} (\eqn{C_{\max} \cdot p \cdot F(T)}); \code{"maximum"}
#'   and \code{"specific"} return the unscaled value (\eqn{C_{\max} \cdot F(T)}).
#' @examples
#' # CEQ 1: simple exponential temperature dependence
#' params <- list(CEQ = 1, CA = 0.303, CB = -0.275, CQ = 0.06)
#' calculate_consumption(temperature = 15, weight = 100, p_value = 0.5,
#'                       processed_consumption_params = params)
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
#' @param warn Logical; if \code{TRUE} (default) issues a warning when
#'   \code{CY = 0} or the discriminant is negative.
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