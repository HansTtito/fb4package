#' Contaminant Accumulation Functions for FB4 Model
#'
#' @description
#' Experimental functions for modelling daily contaminant (e.g. methylmercury,
#' PCBs) dynamics in fish using three bioaccumulation models (CONTEQ 1–3).
#'
#' \strong{CONTEQ 1} — food uptake only, no elimination:
#' \eqn{\text{Burden}_{t+1} = \text{Burden}_t + \sum_i C_i \cdot [\text{prey}]_i \cdot \text{AE}_i}
#'
#' \strong{CONTEQ 2} — food uptake with temperature- and weight-dependent
#' elimination (Trudel and Rasmussen 1997):
#' \eqn{K_x = \exp(0.066 T - 0.2 \ln W - 6.56)}
#'
#' \strong{CONTEQ 3} — Arnot and Gobas (2004): uptake from both water
#' (via gill transfer) and food, elimination proportional to respiration.
#'
#' @references
#' Arnot, J.A. and Gobas, F.A.P.C. (2004). A food web bioaccumulation model
#' for organic chemicals in aquatic ecosystems.
#' \emph{Environmental Toxicology and Chemistry}, 23(10), 2343–2355.
#' \doi{10.1897/03-438}
#'
#' Trudel, M. and Rasmussen, J.B. (1997). Modeling the elimination of mercury
#' by fish. \emph{Environmental Science and Technology}, 31(6), 1716–1722.
#' \doi{10.1021/es960609t}
#'
#' @return No return value; this page documents the contaminant accumulation functions module. See individual function documentation for return values.
#' @name contaminant-accumulation
#' @aliases contaminant-accumulation
NULL

# ============================================================================
# LOW-LEVEL FUNCTIONS
# ============================================================================

#' Contaminant model 1 - Food uptake only (Low-level)
#'
#' Simple model without elimination, only accumulation from food
#'
#' @param consumption Vector of consumption by prey (g/day)
#' @param prey_concentrations Vector of concentrations in prey (ug/g)
#' @param transfer_efficiency Vector of transfer efficiencies
#' @param current_burden Current body burden (ug)
#' @return List with clearance, uptake, and new burden
#' @keywords internal
contaminant_model_1 <- function(consumption, prey_concentrations, transfer_efficiency, current_burden) {
  
  # Uptake from food (ug/day)
  uptake <- sum(consumption * prey_concentrations * transfer_efficiency, na.rm = TRUE)
  
  # No elimination in this model
  clearance <- 0
  
  # New body burden
  new_burden <- current_burden + uptake
  
  return(list(
    clearance = clearance,
    uptake = uptake,
    new_burden = new_burden
  ))
}


#' Contaminant model 2 - With temperature and weight dependent elimination (Low-level)
#'
#' Model with food uptake and elimination dependent on temperature and weight
#' Based on Trudel & Rasmussen (1997) for MeHg
#'
#' @param consumption Vector of consumption by prey (g/day)
#' @param weight Fish weight (g)
#' @param temperature Water temperature (deg C)
#' @param prey_concentrations Vector of concentrations in prey (ug/g)
#' @param assimilation_efficiency Vector of assimilation efficiencies
#' @param current_burden Current body burden (ug)
#' @return List with clearance, uptake, and new burden
#' @keywords internal
contaminant_model_2 <- function(consumption, weight, temperature, prey_concentrations, 
                                assimilation_efficiency, current_burden) {
  
  # Uptake from food (ug/day)
  uptake <- sum(consumption * prey_concentrations * assimilation_efficiency, na.rm = TRUE)
  
  # MeHg elimination coefficient (Trudel & Rasmussen 1997)
  # Kx = exp(0.066*T - 0.2*log(W) - 6.56)
  Kx <- safe_exp(0.066 * temperature - 0.2 * log(weight) - 6.56, param_name = "Contaminant model 2")
  
  # Elimination (ug/day)
  clearance <- Kx * current_burden
  
  # New body burden
  new_burden <- current_burden + uptake - clearance
  
  return(list(
    clearance = clearance,
    uptake = uptake,
    new_burden = new_burden
  ))
}


#' Contaminant model 3 - Arnot & Gobas (2004) (Low-level)
#'
#' Complete model with uptake from water and food, elimination proportional to respiration
#'
#' @param respiration_o2 Respiration (g O2/g/day)
#' @param consumption Vector of consumption by prey (g/day)
#' @param weight Fish weight (g)
#' @param temperature Water temperature (deg C)
#' @param prey_concentrations Vector of concentrations in prey (ug/g)
#' @param assimilation_efficiency Vector of assimilation efficiencies
#' @param current_burden Current body burden (ug)
#' @param gill_efficiency Gill uptake efficiency
#' @param fish_water_partition Fish:water partition coefficient
#' @param water_concentration Total concentration in water (mg/L)
#' @param dissolved_fraction Dissolved fraction
#' @param do_saturation Dissolved oxygen saturation (fraction)
#' @return List with clearance, uptake, and new burden
#' @keywords internal
contaminant_model_3 <- function(respiration_o2, consumption, weight, temperature,
                                prey_concentrations, assimilation_efficiency, current_burden,
                                gill_efficiency, fish_water_partition, water_concentration,
                                dissolved_fraction, do_saturation) {
  
  # Convert respiration to mg O2/g/day
  VOx <- 1000 * respiration_o2
  
  # Dissolved oxygen concentration (mg O2/L)
  # Arnot & Gobas (2004) equation
  COx <- (-0.24 * temperature + 14.04) * do_saturation
  
  # Water elimination rate (L/g/day)
  K1 <- gill_efficiency * VOx / COx
  
  # Uptake from water (ug/day)
  uptake_water <- weight * K1 * dissolved_fraction * water_concentration * 1000
  
  # Uptake from food (ug/day)
  uptake_food <- sum(consumption * prey_concentrations * assimilation_efficiency, na.rm = TRUE)
  
  # Total uptake
  uptake <- uptake_water + uptake_food
  
  # Elimination coefficient
  Kx <- K1 / fish_water_partition
  
  # Elimination (ug/day)
  clearance <- Kx * current_burden
  
  # New body burden
  new_burden <- current_burden + uptake - clearance
  
  return(list(
    clearance = clearance,
    uptake = uptake,
    uptake_water = uptake_water,
    uptake_food = uptake_food,
    new_burden = new_burden
  ))
}


# ============================================================================
# PARAMETER CALCULATION FUNCTIONS
# ============================================================================

#' Calculate gill uptake efficiency (Low-level)
#'
#' Calculates gill efficiency based on Kow according to Arnot & Gobas (2004)
#'
#' @param kow Octanol:water partition coefficient
#' @return Gill uptake efficiency
#' @keywords internal
calculate_gill_efficiency <- function(kow) {
  # Arnot & Gobas (2004) equation
  efficiency <- 1 / (1.85 + (155 / kow))
  return(efficiency)
}

#' Calculate fish:water partition coefficient (Low-level)
#'
#' Calculates Kbw based on body composition and Kow according to Arnot & Gobas (2004)
#'
#' @param fat_fraction Fat fraction in fish
#' @param protein_ash_fraction Protein + ash fraction
#' @param water_fraction Water fraction
#' @param kow Octanol:water partition coefficient
#' @return Fish:water partition coefficient
#' @keywords internal
calculate_fish_water_partition <- function(fat_fraction, protein_ash_fraction, water_fraction, kow) {
  # Arnot & Gobas (2004) equation
  partition_coeff <- fat_fraction * kow + protein_ash_fraction * 0.035 * kow + water_fraction
  return(partition_coeff)
}

#' Calculate dissolved fraction of contaminant (Low-level)
#'
#' Calculates dissolved fraction based on organic carbon according to Arnot & Gobas (2004)
#'
#' @param poc_concentration Particulate organic carbon concentration (kg/L)
#' @param doc_concentration Dissolved organic carbon concentration (kg/L)
#' @param kow Octanol:water partition coefficient
#' @return Dissolved fraction
#' @keywords internal
calculate_dissolved_fraction <- function(poc_concentration, doc_concentration, kow) {
  # Arnot & Gobas (2004) constants
  a_poc <- 0.35
  a_doc <- 0.08
  
  # Arnot & Gobas (2004) equation
  denominator <- 1 + poc_concentration * a_poc * kow + doc_concentration * a_doc * kow
  dissolved_fraction <- 1 / denominator
  
  return(dissolved_fraction)
}


# ============================================================================
# MID-LEVEL FUNCTIONS: Coordination and Business Logic
# ============================================================================

#' Calculate contaminant accumulation (Mid-level - Main function)
#'
#' @description
#' Calculates daily contaminant dynamics (uptake, elimination, body burden)
#' for a fish using one of three bioaccumulation models (CONTEQ 1-3).
#'
#' @param respiration_o2 Respiration in g O2/g/day
#' @param consumption Vector of consumption by prey type (g/day)
#' @param weight Fish weight (g)
#' @param temperature Water temperature (deg C)
#' @param current_concentration Current concentration in predator (ug/g)
#' @param processed_contaminant_params List with processed contaminant parameters
#' @return A named list with at least six elements (all numeric scalars unless
#'   noted):
#'   \describe{
#'     \item{clearance}{Daily elimination of contaminant (ug/day).}
#'     \item{uptake}{Total daily uptake from food (ug/day); for CONTEQ 3
#'       this is the sum of water and food uptake.}
#'     \item{new_burden}{Body burden at end of day (ug); floored at 0.}
#'     \item{new_concentration}{Whole-body concentration (ug/g wet weight);
#'       floored at 0.}
#'     \item{weight}{Fish weight (g), as supplied.}
#'     \item{model_used}{Integer. CONTEQ equation used (1, 2, or 3).}
#'   }
#'   CONTEQ 3 (Arnot & Gobas 2004) appends two additional elements:
#'   \code{uptake_water} (ug/day from water) and \code{uptake_food}
#'   (ug/day from food).
#'
#' @section Experimental:
#' Contaminant modelling is an **experimental feature** under active
#' development. This function can be called directly to compute daily
#' bioaccumulation for a single time step, but it is **not yet integrated**
#' into the main `run_fb4()` simulation loop. Full integration (automatic
#' contaminant tracking across all simulation days, inclusion in
#' `fb4_result` objects, and TMB backend support) is planned for a future
#' release. The API may change.
#'
#' @examples
#' # CONTEQ 1: food uptake only, no elimination
#' params <- list(
#'   CONTEQ = 1,
#'   prey_concentrations = c(0.05, 0.08),
#'   transfer_efficiency = c(0.8, 0.8)
#' )
#' calculate_contaminant_accumulation(
#'   respiration_o2 = 0.02, consumption = c(2.0, 1.0),
#'   weight = 100, temperature = 15,
#'   current_concentration = 0.1,
#'   processed_contaminant_params = params
#' )
#' @export
calculate_contaminant_accumulation <- function(respiration_o2, consumption, weight, temperature,
                                               current_concentration, processed_contaminant_params) {
  
  # Calculate initial body burden (ug)
  current_burden <- current_concentration * weight
  
  # Extract processed parameters
  CONTEQ <- processed_contaminant_params$CONTEQ
  prey_concentrations <- processed_contaminant_params$prey_concentrations
  
  # Calculate based on model
  if (CONTEQ == 1) {
    result <- contaminant_model_1(consumption, prey_concentrations, 
                                  processed_contaminant_params$transfer_efficiency, 
                                  current_burden)
    
  } else if (CONTEQ == 2) {
    result <- contaminant_model_2(consumption, weight, temperature, prey_concentrations,
                                  processed_contaminant_params$assimilation_efficiency, 
                                  current_burden)
    
  } else if (CONTEQ == 3) {
    result <- contaminant_model_3(respiration_o2, consumption, weight, temperature,
                                  prey_concentrations, 
                                  processed_contaminant_params$assimilation_efficiency,
                                  current_burden,
                                  processed_contaminant_params$gill_efficiency,
                                  processed_contaminant_params$fish_water_partition,
                                  processed_contaminant_params$water_concentration,
                                  processed_contaminant_params$dissolved_fraction,
                                  processed_contaminant_params$do_saturation)
  } else {
    stop("calculate_contaminant_accumulation: unrecognized CONTEQ value (", CONTEQ,
         "). Must be 1, 2, or 3.", call. = FALSE)
  }
  
  # Calculate new concentration
  new_concentration <- if (weight > 0) result$new_burden / weight else 0
  
  # Add additional information to result
  result$new_concentration <- new_concentration
  result$weight <- weight
  result$model_used <- CONTEQ
  
  # Ensure non-negative values
  result$new_burden <- pmax(0, result$new_burden)
  result$new_concentration <- pmax(0, result$new_concentration)
  
  return(result)
}
