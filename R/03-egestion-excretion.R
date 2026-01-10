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
  return(pmin(egestion, consumption))
}
#' Egestion model 2 - Elliott (1976) (Low-level)
#'
#' Egestion model dependent on temperature and feeding level
#'
#' @param consumption Consumption (J/g)
#' @param temperature Water temperature (deg C)
#' @param p_value Proportion of maximum consumption (p_value)
#' @param FA Base egestion parameter
#' @param FB Temperature dependence coefficient
#' @param FG Feeding level dependence coefficient
#' @return Egestion (J/g)
#' @keywords internal
egestion_model_2 <- function(consumption, temperature, p_value, FA, FB, FG) {
  egestion <- FA * (temperature^FB) * safe_exp(FG * p_value, param_name = "Egestion model 2") * consumption
  return(pmin(egestion, consumption))
}

#' Egestion model 3 - Stewart et al. (1983) (Low-level)
#'
#' Model that includes indigestible prey
#'
#' @param consumption Consumption (J/g)
#' @param temperature Water temperature (deg C)
#' @param p_value Proportion of maximum consumption (p_value)
#' @param FA Base egestion parameter
#' @param FB Temperature dependence coefficient
#' @param FG Feeding level dependence coefficient
#' @param indigestible_fraction Indigestible fraction of prey
#' @return Egestion (J/g)
#' @keywords internal
egestion_model_3 <- function(consumption, temperature, p_value, FA, FB, FG, indigestible_fraction) {
  PE <- FA * (temperature^FB) * safe_exp(FG * p_value, param_name = "Egestion model 3 - PE")
  PF <- ((PE - 0.1) / 0.9) * (1 - indigestible_fraction) + indigestible_fraction
  PF <- pmax(0, pmin(1, PF)) 
  egestion <- PF * consumption
  return(egestion)
}

#' Egestion model 4 - Elliott (1976) without p_value (Low-level)
#'
#' Simplified model without feeding level dependence
#'
#' @param consumption Consumption (J/g)
#' @param temperature Water temperature (deg C)
#' @param FA Base egestion parameter
#' @param FB Temperature dependence coefficient
#' @return Egestion (J/g)
#' @keywords internal
egestion_model_4 <- function(consumption, temperature, FA, FB) {
  egestion <- FA * (temperature^FB) * consumption
  return(pmin(egestion, consumption))
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
  assimilated <- consumption - egestion
  excretion <- UA * assimilated
  return(excretion)
}

#' Excretion model 2 - With temperature and feeding dependence (Low-level)
#'
#' Excretion model dependent on temperature and feeding level
#'
#' @param consumption Consumption (J/g)
#' @param egestion Egestion (J/g)
#' @param temperature Water temperature (deg C)
#' @param p_value Proportion of maximum consumption (p_value)
#' @param UA Base excretion parameter
#' @param UB Temperature dependence coefficient
#' @param UG Feeding level dependence coefficient
#' @return Excretion (J/g)
#' @keywords internal
excretion_model_2 <- function(consumption, egestion, temperature, p_value, UA, UB, UG) {
  assimilated <- consumption - egestion
  excretion <- UA * (temperature^UB) * safe_exp(UG * p_value, param_name = "Excretion model 2") * assimilated
  return(excretion)
}

#' Excretion model 3 - Variant of model 2 (Low-level)
#'
#' Alternative implementation of temperature and feeding dependent model
#'
#' @param consumption Consumption (J/g)
#' @param egestion Egestion (J/g)
#' @param temperature Water temperature (deg C)
#' @param p_value Proportion of maximum consumption (p_value)
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
#' @param temperature Water temperature (deg C)
#' @param UA Base excretion parameter
#' @param UB Temperature dependence coefficient
#' @return Excretion (J/g)
#' @keywords internal
excretion_model_4 <- function(consumption, egestion, temperature, UA, UB) {
  assimilated <- consumption - egestion
  excretion <- UA * (temperature^UB) * assimilated
  return(excretion)
}

# ============================================================================
# MID-LEVEL FUNCTIONS: Coordination and Business Logic
# ============================================================================

#' Calculate daily egestion (Mid-level - Main function)
#'
#' Main egestion calculation function called from simulation loop
#'
#' @param consumption Consumption (J/g)
#' @param temperature Water temperature (deg C)
#' @param p_value Proportion of maximum consumption (p_value)
#' @param processed_egestion_params List with processed egestion parameters
#' @return Egestion (J/g)
#' @export
calculate_egestion <- function(consumption, temperature, p_value, processed_egestion_params) {
  
  if (consumption == 0) return(0)
  
  EGEQ <- processed_egestion_params$EGEQ
  
  if (EGEQ == 1) {
    return(egestion_model_1(consumption, processed_egestion_params$FA))
    
  } else if (EGEQ == 2) {
    return(egestion_model_2(consumption, temperature, p_value, 
                            processed_egestion_params$FA, processed_egestion_params$FB, 
                            processed_egestion_params$FG))
    
  } else if (EGEQ == 3) {
    return(egestion_model_3(consumption, temperature, p_value, 
                            processed_egestion_params$FA, processed_egestion_params$FB, 
                            processed_egestion_params$FG, processed_egestion_params$indigestible_fraction))
    
  } else if (EGEQ == 4) {
    return(egestion_model_4(consumption, temperature, 
                            processed_egestion_params$FA, processed_egestion_params$FB))
  }
}


#' Calculate daily excretion (Mid-level - Main function)
#'
#' Main excretion calculation function called from simulation loop
#'
#' @param consumption Consumption (J/g)
#' @param egestion Egestion (J/g)
#' @param temperature Water temperature (deg C)
#' @param p_value Proportion of maximum consumption (p_value)
#' @param processed_excretion_params List with processed excretion parameters
#' @return Excretion (J/g)
#' @export
calculate_excretion <- function(consumption, egestion, temperature, p_value, processed_excretion_params) {
  
  if (consumption == 0) return(0)
  
  EXEQ <- processed_excretion_params$EXEQ
  
  if (EXEQ == 1) {
    return(excretion_model_1(consumption, egestion, processed_excretion_params$UA))
    
  } else if (EXEQ == 2) {
    return(excretion_model_2(consumption, egestion, temperature, p_value, 
                             processed_excretion_params$UA, processed_excretion_params$UB, 
                             processed_excretion_params$UG))
    
  } else if (EXEQ == 3) {
    return(excretion_model_3(consumption, egestion, temperature, p_value, 
                             processed_excretion_params$UA, processed_excretion_params$UB, 
                             processed_excretion_params$UG))
    
  } else if (EXEQ == 4) {
    return(excretion_model_4(consumption, egestion, temperature, 
                             processed_excretion_params$UA, processed_excretion_params$UB))
  }
}
