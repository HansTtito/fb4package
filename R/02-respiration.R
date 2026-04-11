#' Respiration Functions for FB4 Model
#'
#' @description
#' Functions implementing the two respiration temperature-dependence equations
#' (REQ 1–2), activity correction, and conversion of oxygen consumption to
#' energy units. Respiration is modelled as:
#'
#' \deqn{R = RA \cdot W^{RB} \cdot F(T) \cdot \mathrm{ACT}}
#'
#' where \eqn{RA} and \eqn{RB} are species-specific intercept and slope
#' coefficients, \eqn{W} is body mass (g), \eqn{F(T)} is a temperature function,
#' and \eqn{\mathrm{ACT}} is an activity multiplier.
#'
#' \strong{REQ 1} — simple Q10 exponential with activity:
#' \eqn{F(T) = e^{RQ \cdot T}}; velocity-based activity from Kitchell et al. (1977).
#'
#' \strong{REQ 2} — Kitchell et al. (1977):
#' \eqn{F(T) = V^{RX} \cdot e^{RX(1-V)}}, where \eqn{V = (RTM - T)/(RTM - RTO)}.
#'
#' Oxygen consumption is converted to energy using the oxycalorific coefficient
#' (default 13 560 J g\eqn{^{-1}} O\eqn{_2}; Elliott and Davison 1975).
#'
#' @references
#' Kitchell, J.F., Stewart, D.J. and Weininger, D. (1977). Applications of a
#' bioenergetics model to yellow perch and walleye.
#' \emph{Journal of the Fisheries Research Board of Canada}, 34(10), 1922–1935.
#' \doi{10.1139/f77-258}
#'
#' Elliott, J.M. and Davison, W. (1975). Energy equivalents of oxygen consumption
#' in animal energetics.
#' \emph{Oecologia}, 19(3), 195–201.
#' \doi{10.1007/BF00345305}
#'
#' Hanson, P.C., Johnson, T.B., Schindler, D.E. and Kitchell, J.F. (1997).
#' \emph{Fish Bioenergetics 3.0}. University of Wisconsin Sea Grant Institute,
#' Madison, WI.
#'
#' Deslauriers, D., Chipps, S.R., Breck, J.E., Rice, J.A. and Madenjian, C.P.
#' (2017). Fish Bioenergetics 4.0: An R-based modeling application.
#' \emph{Fisheries}, 42(11), 586–596. \doi{10.1080/03632415.2017.1377558}
#'
#' @return No return value; this page documents the respiration functions module. See individual function documentation for return values.
#' @name respiration-functions
#' @aliases respiration-functions
NULL

# ============================================================================
# LOW-LEVEL FUNCTIONS
# ============================================================================

#' Temperature function for respiration - Equation 1 (Low-level)
#'
#' Implements temperature equation 1 (simple exponential)
#'
#' @param temperature Water temperature (deg C)
#' @param RQ Q10 rate for low temperatures
#' @return Temperature factor
#' @keywords internal
respiration_temp_eq1 <- function(temperature, RQ) {
  ft <- safe_exp(RQ * temperature, param_name = "Temperature function for respiration - Eq 1")
  return(ft)
}

#' Temperature function for respiration - Equation 2 (Low-level)
#'
#' @description
#' Implements temperature equation 2 (Kitchell et al. 1977).
#' With user notifications about edge cases and fallback values.
#'
#' @param temperature Water temperature (deg C)
#' @param RTM Maximum (lethal) temperature
#' @param RTO Optimum temperature for respiration
#' @param RX Calculated parameter
#' @param warn Logical, whether to issue warnings about calculations, default TRUE
#'
#' @return Temperature factor
#'
#' @details
#' This function calculates temperature effects on respiration using:
#' V = (RTM - temperature) / (RTM - RTO)
#' ft = V^RX × exp(RX × (1 - V))
#' 
#' Special cases:
#' - When temperature >= RTM: returns 0.000001 (lethal temperature)
#' - When ft < 0: returns 0.000001 (mathematical protection)
#' 
#' Note: Negative values can occur with certain RX parameters, hence the minimum bound.
#' @keywords internal
respiration_temp_eq2 <- function(temperature, RTM, RTO, RX, warn = TRUE) {
  if (temperature >= RTM) {
    if (warn) {
      warning("respiration_temp_eq2: temperature (", temperature, "deg C) >= RTM (",
              RTM, "deg C), returning 0.000001 (lethal temperature)", call. = FALSE)
    }
    return(0.000001)
  }
  
  V <- (RTM - temperature) / (RTM - RTO)
  ft <- V^RX * safe_exp(RX * (1 - V), warn = warn, param_name = "Temperature function for respiration - Eq 2")
  
  return(pmax(0.000001, ft))
}


# ============================================================================
# MID-LEVEL FUNCTIONS: Coordination and Business Logic
# ============================================================================

#' Calculate temperature factor for respiration (Mid-level)
#'
#' Coordinates temperature equation selection and calculation
#'
#' @param temperature Water temperature (°C)
#' @param processed_respiration_params List with processed respiration parameters
#' @return Temperature factor
#' @keywords internal
calculate_temperature_factor_respiration <- function(temperature, processed_respiration_params) {
  
  REQ <- processed_respiration_params$REQ
  
  if (REQ == 1) {
    return(respiration_temp_eq1(temperature, processed_respiration_params$RQ))
    
  } else if (REQ == 2) {
    return(respiration_temp_eq2(temperature, processed_respiration_params$RTM, 
                                processed_respiration_params$RTO, processed_respiration_params$RX))
  } else {
    stop("calculate_temperature_factor_respiration: unrecognized REQ value (", REQ,
         "). Must be 1 or 2.", call. = FALSE)
  }
}

#' Calculate activity factor for respiration (Mid-level)
#'
#' Calculates activity factor based on respiration equation with support for
#' both simple and complex activity calculations
#'
#' @param weight Fish weight (g)
#' @param temperature Water temperature (°C)
#' @param processed_respiration_params List with processed respiration parameters (includes activity data)
#' @return Activity factor
#' @keywords internal
calculate_activity_factor_respiration <- function(weight, temperature, processed_respiration_params) {
  
  REQ <- processed_respiration_params$REQ
  ACT <- processed_respiration_params$ACT
  
  if (REQ != 1) {
    return(ACT)
  }
  
  # Complex activity calculation (REQ == 1)
  RTL <- processed_respiration_params$RTL
  RK4 <- processed_respiration_params$RK4
  RK1 <- processed_respiration_params$RK1
  RK5 <- processed_respiration_params$RK5
  RTO <- processed_respiration_params$RTO
  BACT <- processed_respiration_params$BACT
  
  if (temperature <= RTL) {
    VEL <- ACT * weight^RK4 * safe_exp(BACT * temperature, param_name = "Activity factor - low temp")
  } else {
    VEL <- RK1 * weight^RK4 * safe_exp(RK5 * temperature, param_name = "Activity factor - high temp")
  }
  
  VEL <- clamp(VEL, 0, 100, param_name = "Activity velocity")
  ACTIVITY <- safe_exp(RTO * VEL, param_name = "Activity factor")
  
  return(pmax(1.0, ACTIVITY))
}

#' Calculate daily respiration (Mid-level - Main function)
#'
#' Main respiration calculation function called from simulation loop
#'
#' @param temperature Water temperature (°C)
#' @param weight Fish weight (g)
#' @param processed_respiration_params List with processed respiration parameters (includes activity params)
#' @return A positive numeric scalar giving the daily specific respiration rate
#'   in g O\eqn{_2} per g fish per day. Returns \code{0.000001} as a minimum
#'   safety floor when the result is non-finite or non-positive (e.g. at or
#'   above the lethal temperature \code{RTM}). The value accounts for both the
#'   temperature-dependence function (REQ 1 or 2) and the activity multiplier.
#' @examples
#' # REQ 2: Kitchell et al. (1977) temperature dependence
#' params <- list(REQ = 2, RA = 0.0033, RB = -0.227,
#'                RTM = 30, RTO = 18, RX = 0.5, ACT = 1.5)
#' calculate_respiration(temperature = 15, weight = 100,
#'                       processed_respiration_params = params)
#' @export
calculate_respiration <- function(temperature, weight, processed_respiration_params) {
  
  # Calculate maximum respiration
  Rmax <- processed_respiration_params$RA * weight^processed_respiration_params$RB
  
  # Calculate temperature factor
  ft <- calculate_temperature_factor_respiration(temperature, processed_respiration_params)
  
  # Calculate activity factor
  activity_factor <- calculate_activity_factor_respiration(weight, temperature, processed_respiration_params)
  
  # Total respiration
  total_respiration <- Rmax * ft * activity_factor
  
  # Ensure valid result (minimum safety check)
  if (!is.finite(total_respiration) || total_respiration <= 0) {
    return(0.000001)
  }
  
  return(total_respiration)
}

#' Calculate additional parameters for respiration equation 2 (Mid-level)
#'
#' Calculates derived parameters needed for temperature equation 2
#'
#' @param RQ Q10 rate
#' @param RTM Maximum temperature
#' @param RTO Optimum temperature
#' @return List with RY, RZ, RX
#' @keywords internal
calculate_respiration_params_eq2 <- function(RQ, RTM, RTO) {

  RY <- log(RQ) * (RTM - RTO + 2)
  RZ <- log(RQ) * (RTM - RTO)
  
  # Safe calculation of RX
  if (RY == 0) {
    RX <- 1
  } else {
    discriminant <- 1 + 40/RY
    if (discriminant < 0) {
      RX <- 1
    } else {
      RX <- (RZ^2 * (1 + safe_sqrt(discriminant))^2) / 400
    }
  }
  
  return(list(RY = RY, RZ = RZ, RX = RX))
}

# ============================================================================
# UTILITY FUNCTIONS: Conversions and Calculations
# ============================================================================

#' Calculate Specific Dynamic Action (SDA) (Low-level)
#'
#' Implements SDA calculation for metabolic cost of feeding and digestion
#'
#' @param consumption_energy Consumption in energy (J/g)
#' @param egestion_energy Egestion in energy (J/g)
#' @param SDA_coeff Specific dynamic action coefficient
#' @return SDA in energy (J/g)
#' 
#' @details
#' Calculates SDA using: SDA = SDA_coeff × (consumption - egestion)
#' 
#' Special cases:
#' - When egestion > consumption: egestion is capped at consumption value
#' - Result is always >= 0
#' 
#' @keywords internal
calculate_sda <- function(consumption_energy, egestion_energy, SDA_coeff) {

  # Ensure egestion is not greater than consumption (biologically impossible)
  egestion_energy <- pmin(egestion_energy, consumption_energy)

  sda <- SDA_coeff * (consumption_energy - egestion_energy)

  return(sda)
}

#' Convert respiration from O2 to energy units (Utility)
#'
#' Converts respiration from O2 consumption to energy equivalents
#'
#' @param respiration_o2 Respiration in g O2/g fish/day
#' @param oxycal Oxycalorific conversion factor (J/g O2)
#' @return Respiration in J/g fish/day
#' @keywords internal
convert_respiration_to_energy <- function(respiration_o2, oxycal = 13560) {
  return(respiration_o2 * oxycal)
}