#' Egestion and Excretion Functions for FB4 Model
#'
#' @name egestion-excretion
#' @aliases egestion-excretion
NULL

# ============================================================================
# LOW-LEVEL FUNCTIONS
# ============================================================================

#' Egestion model 1 - Basic (Low-level)
#'
#' Simple egestion model with constant fraction of consumption
#'
#' @param consumption Consumption (J/g)
#' @param FA Egestion fraction
#' @return Egestion (J/g)
#' @keywords internal
egestion_model_1 <- function(consumption, FA) {
  egestion <- FA * consumption
  return(pmax(0, pmin(egestion, consumption)))
}

#' Egestion model 2 - Elliott (1976) (Low-level)
#'
#' Egestion model dependent on temperature and feeding level
#'
#' @param consumption Consumption (J/g)
#' @param temperature Water temperature (°C)
#' @param p_value Proportion of maximum consumption (p-value)
#' @param FA Base egestion parameter
#' @param FB Temperature dependence coefficient
#' @param FG Feeding level dependence coefficient
#' @return Egestion (J/g)
#' @keywords internal
egestion_model_2 <- function(consumption, temperature, p_value, FA, FB, FG) {
  safe_temp <- clamp(temperature, 0, 50)
  safe_p <- clamp(p_value, 0, 5)
  safe_FB <- clamp(FB, -5, 5)
  safe_FG <- clamp(FG, -5, 5)
  
  egestion <- FA * (safe_temp^safe_FB) * safe_exp(safe_FG * safe_p) * consumption
  return(pmax(0, pmin(egestion, consumption)))
}

#' Egestion model 3 - Stewart et al. (1983) (Low-level)
#'
#' Model that includes indigestible prey
#'
#' @param consumption Consumption (J/g)
#' @param temperature Water temperature (°C)
#' @param p_value Proportion of maximum consumption (p-value)
#' @param FA Base egestion parameter
#' @param FB Temperature dependence coefficient
#' @param FG Feeding level dependence coefficient
#' @param indigestible_fraction Indigestible fraction of prey
#' @return Egestion (J/g)
#' @keywords internal
egestion_model_3 <- function(consumption, temperature, p_value, FA, FB, FG, indigestible_fraction = 0) {
  safe_temp <- clamp(temperature, 0, 50)
  safe_p <- clamp(p_value, 0, 5)
  safe_FB <- clamp(FB, -5, 5)
  safe_FG <- clamp(FG, -5, 5)
  safe_indig <- clamp(indigestible_fraction, 0, 1)
  
  # Calculate base egestion efficiency
  PE <- FA * (safe_temp^safe_FB) * safe_exp(safe_FG * safe_p)
  
  # Adjust for indigestible prey
  PF <- ((PE - 0.1) / 0.9) * (1 - safe_indig) + safe_indig
  PF <- clamp(PF, 0, 1)
  
  egestion <- PF * consumption
  return(pmax(0, pmin(egestion, consumption)))
}

#' Egestion model 4 - Elliott (1976) without p-value (Low-level)
#'
#' Simplified model without feeding level dependence
#'
#' @param consumption Consumption (J/g)
#' @param temperature Water temperature (°C)
#' @param FA Base egestion parameter
#' @param FB Temperature dependence coefficient
#' @return Egestion (J/g)
#' @keywords internal
egestion_model_4 <- function(consumption, temperature, FA, FB) {
  safe_temp <- clamp(temperature, 0, 50)
  safe_FB <- clamp(FB, -5, 5)
  
  egestion <- FA * (safe_temp^safe_FB) * consumption
  return(pmax(0, pmin(egestion, consumption)))
}

#' Excretion model 1 - Basic (Low-level)
#'
#' Simple excretion model proportional to assimilated matter
#'
#' @param consumption Consumption (J/g)
#' @param egestion Egestion (J/g)
#' @param UA Excretion fraction
#' @return Excretion (J/g)
#' @keywords internal
excretion_model_1 <- function(consumption, egestion, UA) {
  assimilated <- pmax(0, consumption - egestion)
  excretion <- UA * assimilated
  return(pmax(0, pmin(excretion, assimilated)))
}

#' Excretion model 2 - With temperature and feeding dependence (Low-level)
#'
#' Excretion model dependent on temperature and feeding level
#'
#' @param consumption Consumption (J/g)
#' @param egestion Egestion (J/g)
#' @param temperature Water temperature (°C)
#' @param p_value Proportion of maximum consumption (p-value)
#' @param UA Base excretion parameter
#' @param UB Temperature dependence coefficient
#' @param UG Feeding level dependence coefficient
#' @return Excretion (J/g)
#' @keywords internal
excretion_model_2 <- function(consumption, egestion, temperature, p_value, UA, UB, UG) {
  assimilated <- pmax(0, consumption - egestion)
  if (assimilated <= 0) return(0)
  
  safe_temp <- clamp(temperature, 0, 50)
  safe_p <- clamp(p_value, 0, 5)
  safe_UB <- clamp(UB, -5, 5)
  safe_UG <- clamp(UG, -5, 5)
  
  excretion <- UA * (safe_temp^safe_UB) * safe_exp(safe_UG * safe_p) * assimilated
  return(pmax(0, pmin(excretion, assimilated)))
}

#' Excretion model 3 - Variant of model 2 (Low-level)
#'
#' Alternative implementation of temperature and feeding dependent model
#'
#' @param consumption Consumption (J/g)
#' @param egestion Egestion (J/g)
#' @param temperature Water temperature (°C)
#' @param p_value Proportion of maximum consumption (p-value)
#' @param UA Base excretion parameter
#' @param UB Temperature dependence coefficient
#' @param UG Feeding level dependence coefficient
#' @return Excretion (J/g)
#' @keywords internal
excretion_model_3 <- function(consumption, egestion, temperature, p_value, UA, UB, UG) {
  return(excretion_model_2(consumption, egestion, temperature, p_value, UA, UB, UG))
}

#' Excretion model 4 - Without feeding dependence (Low-level)
#'
#' Excretion model with only temperature dependence
#'
#' @param consumption Consumption (J/g)
#' @param egestion Egestion (J/g)
#' @param temperature Water temperature (°C)
#' @param UA Base excretion parameter
#' @param UB Temperature dependence coefficient
#' @return Excretion (J/g)
#' @keywords internal
excretion_model_4 <- function(consumption, egestion, temperature, UA, UB) {
  assimilated <- pmax(0, consumption - egestion)
  if (assimilated <= 0) return(0)
  
  safe_temp <- clamp(temperature, 0, 50)
  safe_UB <- clamp(UB, -5, 5)
  
  excretion <- UA * (safe_temp^safe_UB) * assimilated
  return(pmax(0, pmin(excretion, assimilated)))
}

# ============================================================================
# MID-LEVEL FUNCTIONS: Coordination and Business Logic
# ============================================================================

#' Calculate daily egestion (Mid-level - Main function)
#'
#' Main egestion calculation function called from simulation loop
#'
#' @param consumption Consumption (J/g)
#' @param temperature Water temperature (°C)
#' @param p_value Proportion of maximum consumption (p-value)
#' @param egestion_params List with egestion parameters
#' @param indigestible_fraction Indigestible fraction (for model 3)
#' @return Egestion (J/g)
#' @export
calculate_egestion <- function(consumption, temperature, p_value, egestion_params, indigestible_fraction = 0) {

  if (consumption == 0) return(0)
  
  # Extract parameters (assume they exist and are valid)
  EGEQ <- egestion_params$EGEQ %||% 1
  FA <- egestion_params$FA
  
  # Calculate egestion based on equation
  if (EGEQ == 1) {
    return(egestion_model_1(consumption, FA))
    
  } else if (EGEQ == 2) {
    FB <- egestion_params$FB
    FG <- egestion_params$FG
    return(egestion_model_2(consumption, temperature, p_value, FA, FB, FG))
    
  } else if (EGEQ == 3) {
    FB <- egestion_params$FB
    FG <- egestion_params$FG
    return(egestion_model_3(consumption, temperature, p_value, FA, FB, FG, indigestible_fraction))
    
  } else if (EGEQ == 4) {
    FB <- egestion_params$FB
    return(egestion_model_4(consumption, temperature, FA, FB))
    
  } else {
    # Fallback to model 1
    return(egestion_model_1(consumption, FA))
  }
}

#' Calculate daily excretion (Mid-level - Main function)
#'
#' Main excretion calculation function called from simulation loop
#'
#' @param consumption Consumption (J/g)
#' @param egestion Egestion (J/g)
#' @param temperature Water temperature (°C)
#' @param p_value Proportion of maximum consumption (p-value)
#' @param excretion_params List with excretion parameters
#' @return Excretion (J/g)
#' @export
calculate_excretion <- function(consumption, egestion, temperature, p_value, excretion_params) {

  if (consumption == 0) return(0)
  
  # Ensure egestion does not exceed consumption
  egestion <- pmin(egestion, consumption)
  
  # Extract parameters (assume they exist and are valid)
  EXEQ <- excretion_params$EXEQ %||% 1
  UA <- excretion_params$UA
  
  # Calculate excretion based on equation
  if (EXEQ == 1) {
    return(excretion_model_1(consumption, egestion, UA))
    
  } else if (EXEQ == 2) {
    UB <- excretion_params$UB
    UG <- excretion_params$UG
    return(excretion_model_2(consumption, egestion, temperature, p_value, UA, UB, UG))
    
  } else if (EXEQ == 3) {
    UB <- excretion_params$UB
    UG <- excretion_params$UG
    return(excretion_model_3(consumption, egestion, temperature, p_value, UA, UB, UG))
    
  } else if (EXEQ == 4) {
    UB <- excretion_params$UB
    return(excretion_model_4(consumption, egestion, temperature, UA, UB))
    
  } else {
    # Fallback to model 1
    return(excretion_model_1(consumption, egestion, UA))
  }
}
