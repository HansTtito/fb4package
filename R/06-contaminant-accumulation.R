#' Contaminant Accumulation Functions for FB4 Model
#'
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
#' @param prey_concentrations Vector of concentrations in prey (μg/g)
#' @param transfer_efficiency Vector of transfer efficiencies
#' @param current_burden Current body burden (μg)
#' @return List with clearance, uptake, and new burden
#' @keywords internal
contaminant_model_1 <- function(consumption, prey_concentrations, transfer_efficiency, current_burden) {

  # Uptake from food (μg/day)
  uptake <- sum(consumption * prey_concentrations * transfer_efficiency, na.rm = TRUE)
  
  # No elimination in this model
  clearance <- 0
  
  # New body burden
  new_burden <- current_burden + uptake
  
  return(list(
    clearance = clearance,
    uptake = uptake,
    new_burden = pmax(0, new_burden)
  ))
}

#' Contaminant model 2 - With temperature and weight dependent elimination (Low-level)
#'
#' Model with food uptake and elimination dependent on temperature and weight
#' Based on Trudel & Rasmussen (1997) for MeHg
#'
#' @param consumption Vector of consumption by prey (g/day)
#' @param weight Fish weight (g)
#' @param temperature Water temperature (°C)
#' @param prey_concentrations Vector of concentrations in prey (μg/g)
#' @param assimilation_efficiency Vector of assimilation efficiencies
#' @param current_burden Current body burden (μg)
#' @return List with clearance, uptake, and new burden
#' @keywords internal
contaminant_model_2 <- function(consumption, weight, temperature, prey_concentrations, 
                                assimilation_efficiency, current_burden) {

  # Uptake from food (μg/day)
  uptake <- sum(consumption * prey_concentrations * assimilation_efficiency, na.rm = TRUE)
  
  # MeHg elimination coefficient (Trudel & Rasmussen 1997)
  # Kx = exp(0.066*T - 0.2*log(W) - 6.56)
  safe_temp <- clamp(temperature, 0, 40)
  safe_weight <- pmax(0.1, weight)
  
  Kx <- safe_exp(0.066 * safe_temp - 0.2 * log(safe_weight) - 6.56)
  
  # Elimination (μg/day)
  clearance <- Kx * current_burden
  
  # New body burden
  new_burden <- current_burden + uptake - clearance
  
  return(list(
    clearance = clearance,
    uptake = uptake,
    new_burden = pmax(0, new_burden)
  ))
}

#' Contaminant model 3 - Arnot & Gobas (2004) (Low-level)
#'
#' Complete model with uptake from water and food, elimination proportional to respiration
#'
#' @param respiration_o2 Respiration (g O2/g/day)
#' @param consumption Vector of consumption by prey (g/day)
#' @param weight Fish weight (g)
#' @param temperature Water temperature (°C)
#' @param prey_concentrations Vector of concentrations in prey (μg/g)
#' @param assimilation_efficiency Vector of assimilation efficiencies
#' @param current_burden Current body burden (μg)
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
  safe_temp <- clamp(temperature, 0, 40)
  COx <- (-0.24 * safe_temp + 14.04) * do_saturation
  COx <- pmax(1, COx)  # Avoid division by zero
  
  # Water elimination rate (L/g/day)
  K1 <- gill_efficiency * VOx / COx
  
  # Uptake from water (μg/day)
  uptake_water <- weight * K1 * dissolved_fraction * water_concentration * 1000
  
  # Uptake from food (μg/day)
  uptake_food <- sum(consumption * prey_concentrations * assimilation_efficiency, na.rm = TRUE)
  
  # Total uptake
  uptake <- uptake_water + uptake_food
  
  # Elimination coefficient
  Kx <- K1 / fish_water_partition
  
  # Elimination (μg/day)
  clearance <- Kx * current_burden
  
  # New body burden
  new_burden <- current_burden + uptake - clearance
  
  return(list(
    clearance = clearance,
    uptake = uptake,
    uptake_water = uptake_water,
    uptake_food = uptake_food,
    new_burden = pmax(0, new_burden)
  ))
}

# ============================================================================
# MID-LEVEL FUNCTIONS: Coordination and Business Logic
# ============================================================================

#' Calculate contaminant accumulation (Mid-level - Main function)
#'
#' Main function for calculating contaminant dynamics
#'
#' @param respiration_o2 Respiration in g O2/g/day
#' @param consumption Vector of consumption by prey type (g/day)
#' @param weight Fish weight (g)
#' @param temperature Water temperature (°C)
#' @param current_concentration Current concentration in predator (μg/g)
#' @param contaminant_params List with contaminant parameters
#' @return List with contaminant results
#' @export
calculate_contaminant_accumulation <- function(respiration_o2, consumption, weight, temperature,
                                               current_concentration, contaminant_params) {

  # Calculate initial body burden (μg)
  current_burden <- current_concentration * weight
  
  # Determine model to use
  CONTEQ <- contaminant_params$CONTEQ %||% 1
  
  # Extract common parameters
  prey_concentrations <- contaminant_params$prey_concentrations %||% rep(1.0, length(consumption))
  
  # Adjust vector lengths if necessary
  if (length(prey_concentrations) != length(consumption)) {
    if (length(prey_concentrations) == 1) {
      prey_concentrations <- rep(prey_concentrations, length(consumption))
    } else {
      # Use shorter length as fallback
      min_length <- min(length(prey_concentrations), length(consumption))
      prey_concentrations <- prey_concentrations[1:min_length]
      consumption <- consumption[1:min_length]
    }
  }
  
  # Calculate based on model
  if (CONTEQ == 1) {
    # Model 1: Food uptake only
    transfer_efficiency <- contaminant_params$transfer_efficiency %||% rep(0.95, length(consumption))
    if (length(transfer_efficiency) != length(consumption)) {
      transfer_efficiency <- rep(transfer_efficiency[1], length(consumption))
    }
    
    result <- contaminant_model_1(consumption, prey_concentrations, transfer_efficiency, current_burden)
    
  } else if (CONTEQ == 2) {
    # Model 2: Uptake + elimination
    assimilation_efficiency <- contaminant_params$assimilation_efficiency %||% rep(0.80, length(consumption))
    if (length(assimilation_efficiency) != length(consumption)) {
      assimilation_efficiency <- rep(assimilation_efficiency[1], length(consumption))
    }
    
    result <- contaminant_model_2(consumption, weight, temperature, prey_concentrations,
                                  assimilation_efficiency, current_burden)
    
  } else if (CONTEQ == 3) {
    # Model 3: Arnot & Gobas
    assimilation_efficiency <- contaminant_params$assimilation_efficiency %||% rep(0.80, length(consumption))
    if (length(assimilation_efficiency) != length(consumption)) {
      assimilation_efficiency <- rep(assimilation_efficiency[1], length(consumption))
    }
    
    gill_efficiency <- contaminant_params$gill_efficiency %||% 0.5
    fish_water_partition <- contaminant_params$fish_water_partition %||% 1000
    water_concentration <- contaminant_params$water_concentration %||% 0.001
    dissolved_fraction <- contaminant_params$dissolved_fraction %||% 0.8
    do_saturation <- contaminant_params$do_saturation %||% 0.9
    
    # Check if model 3 parameters are available
    if (is.na(gill_efficiency) || is.na(fish_water_partition) || is.na(water_concentration)) {
      # Fallback to model 2
      result <- contaminant_model_2(consumption, weight, temperature, prey_concentrations,
                                    assimilation_efficiency, current_burden)
    } else {
      result <- contaminant_model_3(respiration_o2, consumption, weight, temperature,
                                    prey_concentrations, assimilation_efficiency, current_burden,
                                    gill_efficiency, fish_water_partition, water_concentration,
                                    dissolved_fraction, do_saturation)
    }
    
  } else {
    # Fallback to model 1
    transfer_efficiency <- contaminant_params$transfer_efficiency %||% rep(0.95, length(consumption))
    if (length(transfer_efficiency) != length(consumption)) {
      transfer_efficiency <- rep(transfer_efficiency[1], length(consumption))
    }
    result <- contaminant_model_1(consumption, prey_concentrations, transfer_efficiency, current_burden)
  }
  
  # Calculate new concentration
  new_concentration <- if (weight > 0) result$new_burden / weight else 0
  
  # Add additional information to result
  result$new_concentration <- new_concentration
  result$weight <- weight
  result$model_used <- CONTEQ
  
  return(result)
}

# ============================================================================
# UTILITY FUNCTIONS: Parameter Calculations and Validation
# ============================================================================

#' Calculate gill uptake efficiency (Utility)
#'
#' Calculates gill efficiency based on Kow according to Arnot & Gobas (2004)
#'
#' @param kow Octanol:water partition coefficient
#' @return Gill uptake efficiency
#' @export
calculate_gill_efficiency <- function(kow) {
  kow <- check_numeric_value(kow, "kow", min_val = 0.001)
  
  # Arnot & Gobas (2004) equation
  efficiency <- 1 / (1.85 + (155 / kow))
  
  # Limit to valid range
  efficiency <- clamp(efficiency, 0, 1)
  
  return(efficiency)
}

#' Calculate fish:water partition coefficient (Utility)
#'
#' Calculates Kbw based on body composition and Kow according to Arnot & Gobas (2004)
#'
#' @param fat_fraction Fat fraction in fish
#' @param protein_ash_fraction Protein + ash fraction
#' @param water_fraction Water fraction
#' @param kow Octanol:water partition coefficient
#' @return Fish:water partition coefficient
#' @export
calculate_fish_water_partition <- function(fat_fraction, protein_ash_fraction, water_fraction, kow) {
  fat_fraction <- check_numeric_value(fat_fraction, "fat_fraction", min_val = 0, max_val = 1)
  protein_ash_fraction <- check_numeric_value(protein_ash_fraction, "protein_ash_fraction", min_val = 0, max_val = 1)
  water_fraction <- check_numeric_value(water_fraction, "water_fraction", min_val = 0, max_val = 1)
  kow <- check_numeric_value(kow, "kow", min_val = 0.001)
  
  total_fraction <- fat_fraction + protein_ash_fraction + water_fraction
  if (abs(total_fraction - 1) > 0.1) {
    warning("Fractions deviate significantly from 1.0: ", round(total_fraction, 3), call. = FALSE)
  }
  
  # Arnot & Gobas (2004) equation
  partition_coeff <- fat_fraction * kow + protein_ash_fraction * 0.035 * kow + water_fraction
  
  return(pmax(1, partition_coeff))  # Minimum of 1
}

#' Calculate dissolved fraction of contaminant (Utility)
#'
#' Calculates dissolved fraction based on organic carbon according to Arnot & Gobas (2004)
#'
#' @param poc_concentration Particulate organic carbon concentration (kg/L)
#' @param doc_concentration Dissolved organic carbon concentration (kg/L)
#' @param kow Octanol:water partition coefficient
#' @return Dissolved fraction
#' @export
calculate_dissolved_fraction <- function(poc_concentration, doc_concentration, kow) {
  poc_concentration <- check_numeric_value(poc_concentration, "poc_concentration", min_val = 0)
  doc_concentration <- check_numeric_value(doc_concentration, "doc_concentration", min_val = 0)
  kow <- check_numeric_value(kow, "kow", min_val = 0.001)
  
  # Arnot & Gobas (2004) constants
  a_poc <- 0.35
  a_doc <- 0.08
  
  # Arnot & Gobas (2004) equation
  denominator <- 1 + poc_concentration * a_poc * kow + doc_concentration * a_doc * kow
  dissolved_fraction <- 1 / denominator
  
  # Limit to valid range
  dissolved_fraction <- clamp(dissolved_fraction, 0, 1)
  
  return(dissolved_fraction)
}

#' Validate contaminant parameters (Utility)
#'
#' Comprehensive validation of contaminant parameters
#'
#' @param contaminant_params List with parameters
#' @return List with validation results
#' @export
validate_contaminant_params <- function(contaminant_params) {

  validation <- list(
    valid = TRUE,
    warnings = character(),
    errors = character()
  )
  
  CONTEQ <- contaminant_params$CONTEQ %||% 1
  
  if (!CONTEQ %in% 1:3) {
    validation$errors <- c(validation$errors, "CONTEQ must be 1, 2, or 3")
    validation$valid <- FALSE
    return(validation)
  }
  
  # Validate prey concentrations
  if ("prey_concentrations" %in% names(contaminant_params)) {
    if (any(contaminant_params$prey_concentrations < 0, na.rm = TRUE)) {
      validation$errors <- c(validation$errors, "Prey concentrations cannot be negative")
      validation$valid <- FALSE
    }
    
    if (any(contaminant_params$prey_concentrations > 1000, na.rm = TRUE)) {
      validation$warnings <- c(validation$warnings, "Very high prey concentrations (>1000 μg/g)")
    }
  }
  
  # Validate efficiencies
  efficiency_params <- c("transfer_efficiency", "assimilation_efficiency")
  for (param in efficiency_params) {
    if (param %in% names(contaminant_params)) {
      values <- contaminant_params[[param]]
      if (any(values < 0 | values > 1, na.rm = TRUE)) {
        validation$errors <- c(validation$errors, paste(param, "must be between 0 and 1"))
        validation$valid <- FALSE
      }
    }
  }
  
  # Model 3 specific validations
  if (CONTEQ == 3) {
    model3_params <- c("gill_efficiency", "dissolved_fraction")
    for (param in model3_params) {
      if (param %in% names(contaminant_params)) {
        value <- contaminant_params[[param]]
        if (value < 0 || value > 1) {
          validation$errors <- c(validation$errors, paste(param, "must be between 0 and 1"))
          validation$valid <- FALSE
        }
      }
    }
    
    if ("fish_water_partition" %in% names(contaminant_params)) {
      if (contaminant_params$fish_water_partition <= 0) {
        validation$errors <- c(validation$errors, "fish_water_partition must be positive")
        validation$valid <- FALSE
      }
    }
  }
  
  return(validation)
}