#' FB4 Main Simulation Engine
#'
#' @name simulation-engine
#' @aliases simulation-engine
NULL

# ============================================================================
# CORE DAILY SIMULATION FUNCTIONS
# ============================================================================

#' Calculate daily consumption
#'
#' @param current_weight Current fish weight (g)
#' @param temperature Water temperature (°C)
#' @param p_value Proportion of maximum consumption (0-5)
#' @param ration_percent Ration as percentage of body weight
#' @param ration_grams Ration in grams per day
#' @param method Calculation method ("p_value", "ration_percent", "ration_grams")
#' @param consumption_params Species consumption parameters
#' @param mean_prey_energy Mean prey energy density (J/g)
#' @return List with specific and energetic consumption
#' @keywords internal
calculate_daily_consumption <- function(current_weight, temperature, p_value = NULL,
                                        ration_percent = NULL, ration_grams = NULL,
                                        method = "p_value", consumption_params, mean_prey_energy) {
  
  # Validate parameters according to method
  if (method == "p_value" && is.null(p_value)) {
    stop("p_value required for p_value method")
  }
  if (method == "ration_percent" && is.null(ration_percent)) {
    stop("ration_percent required for ration_percent method")
  }
  if (method == "ration_grams" && is.null(ration_grams)) {
    stop("ration_grams required for ration_grams method")
  }
  
  # Calculate maximum consumption once
  max_consumption_gg <- calculate_consumption(temperature, current_weight, 1.0, 
                                              consumption_params, method = "maximum")
  
  if (method == "ration_percent") {
    # Ration as % of body weight
    consumption_gg <- ration_percent / 100  # g prey/g fish
    consumption_energy <- consumption_gg * mean_prey_energy  # J/g
    
    # Calculate equivalent p-value
    effective_p <- if (max_consumption_gg > 0) consumption_gg / max_consumption_gg else 0
    
  } else if (method == "ration_grams") {
    # Ration as grams of prey per day
    consumption_gg <- ration_grams / current_weight  # g prey/g fish
    consumption_energy <- consumption_gg * mean_prey_energy  # J/g
    
    # Calculate equivalent p-value
    effective_p <- if (max_consumption_gg > 0) consumption_gg / max_consumption_gg else 0
    
  } else {  # method == "p_value"
    # Use p-value directly
    effective_p <- p_value
    consumption_gg <- calculate_consumption(temperature, current_weight, effective_p, 
                                            consumption_params, method = "rate")
    consumption_energy <- consumption_gg * mean_prey_energy
  }
  
  # Validate results
  effective_p <- pmax(0, pmin(5, effective_p))  # Limit range
  
  return(list(
    consumption_gg = pmax(0, consumption_gg),
    consumption_energy = pmax(0, consumption_energy),
    effective_p = effective_p
  ))
}


#' Calculate egestion, excretion, respiration and SDA for one day
#'
#' @param consumption_energy Energy consumption (J/g/day)
#' @param current_weight Current weight (g)
#' @param temperature Temperature (°C)
#' @param p_value Effective p-value
#' @param species_params Species parameters
#' @param oxycal Oxycalorific coefficient (J/g O2)
#' @param diet_proportions Diet item proportions
#' @param indigestible_fractions Indigestible fractions by diet item
#' @return List with metabolic processes
#' @keywords internal
calculate_daily_metabolism <- function(consumption_energy, current_weight, temperature,
                                       p_value, species_params, oxycal = 13560, 
                                       diet_proportions = NULL, indigestible_fractions = NULL) {
  
  # Calculate total indigestible fraction if data provided
  total_indigestible_fraction <- 0
  if (!is.null(diet_proportions) && !is.null(indigestible_fractions)) {
    total_indigestible_fraction <- sum(diet_proportions * indigestible_fractions)
  }
  
  # Calculate egestion
  egestion_energy <- calculate_egestion(
    consumption = consumption_energy,
    temperature = temperature,
    p_value = p_value,
    egestion_params = species_params$egestion,
    indigestible_fraction = total_indigestible_fraction
  )
  
  # Calculate excretion
  excretion_energy <- calculate_excretion(
    consumption = consumption_energy,
    egestion = egestion_energy,
    temperature = temperature,
    p_value = p_value,
    excretion_params = species_params$excretion
  )
  
  # Calculate respiration
  respiration_o2 <- calculate_respiration(
    weight = current_weight,
    temperature = temperature,
    respiration_params = species_params$respiration,
    activity_params = species_params$activity
  )
  respiration_energy <- respiration_o2 * oxycal
  
  # Calculate SDA (Specific Dynamic Action)
  sda_energy <- calculate_sda(
    consumption = consumption_energy,
    egestion = egestion_energy,
    SDA_coeff = species_params$sda$SDA %||% 0.1
  )
  
  return(list(
    egestion_energy = egestion_energy,
    excretion_energy = excretion_energy,
    respiration_energy = respiration_energy,
    respiration_o2 = (respiration_energy + sda_energy)/oxycal,
    sda_energy = sda_energy,
    net_energy = consumption_energy - egestion_energy - excretion_energy - respiration_energy - sda_energy
  ))
}

#' Calculate final daily weight considering growth and reproduction
#'
#' @param current_weight Initial weight of the day (g)
#' @param net_energy Net energy available (J/g/day)
#' @param spawn_energy Energy lost to reproduction (J)
#' @param predator_energy_density Predator energy density (J/g)
#' @return List with final weight and weight change
#' @keywords internal
calculate_daily_growth <- function(current_weight, net_energy, spawn_energy,
                                   predator_energy_density) {
  
  # Total energy available for the day
  total_energy_gain <- net_energy * current_weight
  
  # Net energy after reproduction
  net_energy_after_spawn <- total_energy_gain - spawn_energy
  
  # Calculate weight change
  weight_change <- net_energy_after_spawn / predator_energy_density
  
  # Final weight
  final_weight <- pmax(0.01, current_weight + weight_change)  # Minimum 0.01g
  
  return(list(
    final_weight = final_weight,
    weight_change = weight_change,
    net_energy_gain = net_energy_after_spawn
  ))
}