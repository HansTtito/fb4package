#' Respiration Functions for FB4 Model
#'
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
#' @param temperature Water temperature (°C)
#' @param RQ Q10 rate for low temperatures
#' @return Temperature factor
#' @keywords internal
respiration_temp_eq1 <- function(temperature, RQ) {
  safe_temp <- clamp(temperature, -50, 50)
  safe_RQ <- clamp(RQ, -1, 1)
  ft <- safe_exp(safe_RQ * safe_temp)
  return(pmax(0.001, ft))  # Minimum value to avoid zero
}

#' Temperature function for respiration - Equation 2 (Low-level)
#'
#' Implements temperature equation 2 (Kitchell et al. 1977)
#'
#' @param temperature Water temperature (°C)
#' @param RTM Maximum (lethal) temperature
#' @param RTO Optimum temperature for respiration
#' @param RX Calculated parameter
#' @return Temperature factor
#' @keywords internal
respiration_temp_eq2 <- function(temperature, RTM, RTO, RX) {
  if (temperature >= RTM) return(0.001)
  
  V <- (RTM - temperature) / (RTM - RTO)
  if (V <= 0) return(0.001)
  
  safe_RX <- clamp(RX, 0, 10)
  ft <- V^safe_RX * safe_exp(safe_RX * (1 - V))
  
  return(pmax(0.001, ft))
}

#' Activity factor calculation (Low-level)
#'
#' Implements activity function with temperature component
#'
#' @param weight Fish weight (g)
#' @param temperature Water temperature (°C)
#' @param RTL Cutoff temperature for activity relationship change
#' @param ACT Activity multiplier
#' @param RK4 Mass dependence coefficient for swimming speed
#' @param BACT Temperature dependence coefficient for swimming speed
#' @param RK1 Intercept for swimming speed above cutoff temperature
#' @param RK5 Temperature coefficient for swimming speed
#' @param RTO Optimum temperature for respiration
#' @return Activity factor
#' @keywords internal
calculate_activity_factor <- function(weight, temperature, RTL, ACT, RK4, BACT, RK1, RK5, RTO) {

  # Calculate velocity based on temperature
  if (temperature <= RTL) {
    VEL <- ACT * weight^RK4 * safe_exp(BACT * temperature)
  } else {
    VEL <- RK1 * weight^RK4 * safe_exp(RK5 * temperature)
  }
  
  # Limit velocity to avoid extreme values
  VEL <- clamp(VEL, 0, 100)
  
  # Calculate activity factor
  ACTIVITY <- safe_exp(RTO * VEL)
  
  return(pmax(1.0, ACTIVITY))  # Minimum of 1.0
}

# ============================================================================
# MID-LEVEL FUNCTIONS: Coordination and Business Logic
# ============================================================================

#' Calculate temperature factor for respiration (Mid-level)
#'
#' Coordinates temperature equation selection and calculation
#'
#' @param temperature Water temperature (°C)
#' @param respiration_params List with respiration parameters
#' @return Temperature factor
#' @keywords internal
calculate_temperature_factor_respiration <- function(temperature, respiration_params) {
  REQ <- respiration_params$REQ
  
  if (REQ == 1) {
    RQ <- respiration_params$RQ
    return(respiration_temp_eq1(temperature, RQ))
    
  } else if (REQ == 2) {
    RTM <- respiration_params$RTM
    RTO <- respiration_params$RTO
    
    # Calculate RX if not present
    if (is.null(respiration_params$RX)) {
      RQ <- respiration_params$RQ
      extra_params <- calculate_respiration_params_eq2(RQ, RTM, RTO)
      RX <- extra_params$RX
    } else {
      RX <- respiration_params$RX
    }
    
    return(respiration_temp_eq2(temperature, RTM, RTO, RX))
    
  } else {
    # Fallback to equation 1
    RQ <- respiration_params$RQ %||% 0.1
    return(respiration_temp_eq1(temperature, RQ))
  }
}

#' Calculate activity factor for respiration (Mid-level)
#'
#' Coordinates activity calculation based on respiration equation
#'
#' @param weight Fish weight (g)
#' @param temperature Water temperature (°C)
#' @param respiration_params List with respiration parameters
#' @param activity_params List with activity parameters
#' @return Activity factor
#' @keywords internal
calculate_activity_factor_respiration <- function(weight, temperature, respiration_params, activity_params) {
  REQ <- respiration_params$REQ
  
  if (REQ == 1) {
    # Complex activity - requires BOTH parameter sets
    RTL <- respiration_params$RTL
    RK4 <- respiration_params$RK4
    RK1 <- respiration_params$RK1
    RK5 <- respiration_params$RK5
    RTO <- respiration_params$RTO
    ACT <- activity_params$ACT
    BACT <- activity_params$BACT
    
    return(calculate_activity_factor(weight, temperature, RTL, ACT, RK4, BACT, RK1, RK5, RTO))
    
  } else {
    # Fallback to simple activity
    return(activity_params$ACT %||% 1.0)
  }
}

#' Calculate daily respiration (Mid-level - Main function)
#'
#' Main respiration calculation function called from simulation loop
#'
#' @param temperature Water temperature (°C)
#' @param weight Fish weight (g)
#' @param respiration_params List with respiration parameters
#' @param activity_params List with activity parameters
#' @return Respiration (g O2/g fish/day)
#' @export
calculate_respiration <- function(temperature, weight, respiration_params, activity_params) {

  # Extract basic parameters (assume they exist and are valid)
  RA <- respiration_params$RA
  RB <- respiration_params$RB
  
  # Calculate maximum respiration
  Rmax <- RA * weight^RB
  
  # Calculate temperature factor
  ft <- calculate_temperature_factor_respiration(temperature, respiration_params)
  
  # Calculate activity factor
  activity_factor <- calculate_activity_factor_respiration(weight, temperature, respiration_params, activity_params)
  
  # Total respiration
  total_respiration <- Rmax * ft * activity_factor
  
  # Ensure valid result (minimum safety check)
  if (!is.finite(total_respiration) || total_respiration <= 0) {
    return(0.001)
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
#' @export
calculate_respiration_params_eq2 <- function(RQ, RTM, RTO) {
  if (any(is.na(c(RQ, RTM, RTO)))) {
    stop("All parameters must be valid (not NA)", call. = FALSE)
  }
  
  if (RTM <= RTO) {
    stop("RTM must be greater than RTO", call. = FALSE)
  }
  
  if (RQ <= 0) {
    stop("RQ must be positive", call. = FALSE)
  }
  
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

#' Calculate Specific Dynamic Action (SDA) (Utility)
#'
#' Calculates metabolic cost of feeding and digestion
#'
#' @param consumption_energy Consumption in energy (J/g)
#' @param egestion_energy Egestion in energy (J/g)
#' @param SDA_coeff Specific dynamic action coefficient
#' @return SDA in energy (J/g)
#' @export
calculate_sda <- function(consumption_energy, egestion_energy, SDA_coeff) {
  consumption_energy <- check_numeric_value(consumption_energy, "consumption_energy", min_val = 0)
  egestion_energy <- check_numeric_value(egestion_energy, "egestion_energy", min_val = 0)
  SDA_coeff <- check_numeric_value(SDA_coeff, "SDA_coeff", min_val = 0, max_val = 1)
  
  # Ensure egestion is not greater than consumption
  egestion_energy <- pmin(egestion_energy, consumption_energy)
  
  sda <- SDA_coeff * (consumption_energy - egestion_energy)
  
  return(pmax(0, sda))
}

#' Convert respiration from O2 to energy units (Utility)
#'
#' Converts respiration from O2 consumption to energy equivalents
#'
#' @param respiration_o2 Respiration in g O2/g fish/day
#' @param oxycal Oxycalorific conversion factor (J/g O2)
#' @return Respiration in J/g fish/day
#' @export
convert_respiration_to_energy <- function(respiration_o2, oxycal = 13560) {
  respiration_o2 <- check_numeric_value(respiration_o2, "respiration_o2", min_val = 0)
  oxycal <- check_numeric_value(oxycal, "oxycal", min_val = 1000, max_val = 20000)
  
  respiration_energy <- respiration_o2 * oxycal
  
  return(respiration_energy)
}