#' Nutrient Regeneration Functions for FB4 Model
#'
#' @name nutrient-regeneration
#' @aliases nutrient-regeneration
NULL

# ============================================================================
# LOW-LEVEL FUNCTIONS
# ============================================================================

#' Generic nutrient allocation in bioenergetic model (Low-level)
#'
#' Calculates nutrient balance in consumption, growth, excretion and egestion
#'
#' @param consumption Vector of consumption by prey type (g/day)
#' @param prey_nutrient_concentrations Vector of nutrient concentrations in prey (g nutrient/g wet weight)
#' @param nutrient_assimilation_efficiency Vector of nutrient assimilation efficiencies (fraction 0-1)
#' @param weight_gain Predator weight gain (g/day)
#' @param predator_nutrient_concentration Nutrient concentration in predator (g nutrient/g wet weight)
#' @return List with nutrient fluxes
#' @keywords internal
calculate_nutrient_allocation <- function(consumption, prey_nutrient_concentrations, 
                                          nutrient_assimilation_efficiency, weight_gain,
                                          predator_nutrient_concentration) {
  
  # 1. Nutrient consumed by prey type (g nutrient/day)
  nutrient_consumption_by_prey <- consumption * prey_nutrient_concentrations
  
  # 2. Total nutrient consumed (g nutrient/day)
  nutrient_consumed_total <- sum(nutrient_consumption_by_prey, na.rm = TRUE)
  
  # 3. Nutrient incorporated in growth (g nutrient/day)
  nutrient_growth <- weight_gain * predator_nutrient_concentration
  
  # 4. Total assimilated nutrient (g nutrient/day)
  nutrient_assimilated <- sum(nutrient_assimilation_efficiency * nutrient_consumption_by_prey, na.rm = TRUE)
  
  # 5. Nutrient excretion (g nutrient/day)
  # Excreted = Assimilated - Growth
  nutrient_excretion <- nutrient_assimilated - nutrient_growth
  
  # 6. Nutrient egestion (g nutrient/day)
  # Egestion = Consumed - Assimilated
  nutrient_egestion <- nutrient_consumed_total - nutrient_assimilated
  
  return(list(
    consumed = nutrient_consumed_total,
    growth = nutrient_growth,
    excretion = nutrient_excretion,
    egestion = nutrient_egestion,
    assimilated = nutrient_assimilated,
    assimilation_efficiency = if (nutrient_consumed_total > 0) nutrient_assimilated / nutrient_consumed_total else 0
  ))
}

# ============================================================================
# MID-LEVEL FUNCTION (SIMPLIFIED)
# ============================================================================

#' Calculate nutrient balance (Mid-level - Main function)
#'
#' @description
#' Calculates daily nitrogen and phosphorus fluxes (ingestion, retention,
#' excretion) for a fish using prey and predator elemental concentrations.
#'
#' @param consumption Vector of consumption by prey type (g/day)
#' @param weight_gain Predator weight gain (g/day)
#' @param processed_nutrient_params List with processed nutrient parameters
#' @return List with nutrient results (N and P ingestion, retention, excretion)
#'
#' @section Experimental:
#' Nutrient regeneration modelling is an **experimental feature** under
#' active development. This function can be called directly to compute
#' daily N and P fluxes for a single time step, but it is **not yet
#' integrated** into the main `run_fb4()` simulation loop. Full integration
#' (automatic daily nutrient tracking, inclusion in `fb4_result` objects,
#' and TMB backend support) is planned for a future release. The API
#' may change.
#'
#' @export
calculate_nutrient_balance <- function(consumption, weight_gain, processed_nutrient_params) {
  
  # Extract processed parameters
  prey_n_concentrations <- processed_nutrient_params$prey_n_concentrations
  prey_p_concentrations <- processed_nutrient_params$prey_p_concentrations
  predator_n_concentration <- processed_nutrient_params$predator_n_concentration
  predator_p_concentration <- processed_nutrient_params$predator_p_concentration
  n_assimilation_efficiency <- processed_nutrient_params$n_assimilation_efficiency
  p_assimilation_efficiency <- processed_nutrient_params$p_assimilation_efficiency
  
  # Calculate nitrogen fluxes
  nitrogen_result <- calculate_nutrient_allocation(
    consumption = consumption,
    prey_nutrient_concentrations = prey_n_concentrations,
    nutrient_assimilation_efficiency = n_assimilation_efficiency,
    weight_gain = weight_gain,
    predator_nutrient_concentration = predator_n_concentration
  )
  
  # Calculate phosphorus fluxes
  phosphorus_result <- calculate_nutrient_allocation(
    consumption = consumption,
    prey_nutrient_concentrations = prey_p_concentrations,
    nutrient_assimilation_efficiency = p_assimilation_efficiency,
    weight_gain = weight_gain,
    predator_nutrient_concentration = predator_p_concentration
  )
  
  return(list(
    nitrogen = nitrogen_result,
    phosphorus = phosphorus_result,
    weight_gain = weight_gain
  ))
}


