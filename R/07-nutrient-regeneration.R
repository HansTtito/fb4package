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
#' @param nutrient_name Name of nutrient for error messages
#' @return List with nutrient fluxes
#' @keywords internal
calculate_nutrient_allocation <- function(consumption, prey_nutrient_concentrations, 
                                          nutrient_assimilation_efficiency, weight_gain,
                                          predator_nutrient_concentration, nutrient_name = "nutrient") {
  
  # Validate inputs
  if (length(consumption) != length(prey_nutrient_concentrations) || 
      length(consumption) != length(nutrient_assimilation_efficiency)) {
    stop("Input vectors must have the same length")
  }
  
  # Validate efficiency ranges
  if (any(nutrient_assimilation_efficiency < 0 | nutrient_assimilation_efficiency > 1, na.rm = TRUE)) {
    warning("Assimilation efficiencies outside range [0,1], correcting")
    nutrient_assimilation_efficiency <- clamp(nutrient_assimilation_efficiency, 0, 1)
  }
  
  # Validate concentrations
  if (any(prey_nutrient_concentrations < 0, na.rm = TRUE) || predator_nutrient_concentration < 0) {
    warning("Negative concentrations detected, correcting to 0")
    prey_nutrient_concentrations <- pmax(0, prey_nutrient_concentrations, na.rm = TRUE)
    predator_nutrient_concentration <- max(0, predator_nutrient_concentration)
  }
  
  # 1. Nutrient consumed by prey type (g nutrient/day)
  nutrient_consumption_by_prey <- consumption * prey_nutrient_concentrations
  
  # 2. Total nutrient consumed (g nutrient/day)
  nutrient_consumed_total <- sum(nutrient_consumption_by_prey, na.rm = TRUE)
  
  # 3. Nutrient incorporated in growth (g nutrient/day)
  nutrient_growth <- pmax(0, weight_gain) * predator_nutrient_concentration
  
  # 4. Total assimilated nutrient (g nutrient/day)
  nutrient_assimilated <- sum(nutrient_assimilation_efficiency * nutrient_consumption_by_prey, na.rm = TRUE)
  
  # 5. Nutrient excretion (g nutrient/day)
  # Excreted = Assimilated - Growth
  nutrient_excretion <- pmax(0, nutrient_assimilated - nutrient_growth)
  
  # 6. Nutrient egestion (g nutrient/day)
  # Egestion = Consumed - Assimilated
  nutrient_egestion <- pmax(0, nutrient_consumed_total - nutrient_assimilated)
  
  # Mass balance check
  balance_check <- abs(nutrient_consumed_total - (nutrient_growth + nutrient_excretion + nutrient_egestion))
  if (balance_check > 1e-8) {
    warning("Mass balance error in ", nutrient_name, ": ", signif(balance_check, 3))
  }
  
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
# MID-LEVEL FUNCTIONS: Coordination and Business Logic
# ============================================================================

#' Calculate nutrient balance (Mid-level - Main function)
#'
#' Main function for calculating nitrogen and phosphorus fluxes
#'
#' @param consumption Vector of consumption by prey type (g/day)
#' @param weight_gain Predator weight gain (g/day)
#' @param nutrient_params List with nutrient parameters
#' @return List with nutrient results
#' @export
calculate_nutrient_balance <- function(consumption, weight_gain, nutrient_params) {
  
  # Basic validations
  if (is.null(nutrient_params)) {
    stop("nutrient_params cannot be NULL")
  }
  
  weight_gain <- check_numeric_value(weight_gain, "weight_gain", min_val = -Inf)
  
  # Extract parameters with default values
  prey_n_concentrations <- nutrient_params$prey_n_concentrations %||% 
    rep(0.08, length(consumption))  # 8% N typical
  prey_p_concentrations <- nutrient_params$prey_p_concentrations %||% 
    rep(0.01, length(consumption))  # 1% P typical
  
  predator_n_concentration <- nutrient_params$predator_n_concentration %||% 0.09  # 9% N
  predator_p_concentration <- nutrient_params$predator_p_concentration %||% 0.015  # 1.5% P
  
  n_assimilation_efficiency <- nutrient_params$n_assimilation_efficiency %||% 
    rep(0.85, length(consumption))
  p_assimilation_efficiency <- nutrient_params$p_assimilation_efficiency %||% 
    rep(0.80, length(consumption))
  
  # Adjust vector lengths if necessary
  consumption_length <- length(consumption)
  
  # Standardize vector lengths
  prey_n_concentrations <- standardize_vector_length(prey_n_concentrations, consumption_length, "prey_n_concentrations")
  prey_p_concentrations <- standardize_vector_length(prey_p_concentrations, consumption_length, "prey_p_concentrations")
  n_assimilation_efficiency <- standardize_vector_length(n_assimilation_efficiency, consumption_length, "n_assimilation_efficiency")
  p_assimilation_efficiency <- standardize_vector_length(p_assimilation_efficiency, consumption_length, "p_assimilation_efficiency")
  
  # Calculate nitrogen fluxes
  nitrogen_result <- calculate_nutrient_allocation(
    consumption = consumption,
    prey_nutrient_concentrations = prey_n_concentrations,
    nutrient_assimilation_efficiency = n_assimilation_efficiency,
    weight_gain = weight_gain,
    predator_nutrient_concentration = predator_n_concentration,
    nutrient_name = "nitrogen"
  )
  
  # Calculate phosphorus fluxes
  phosphorus_result <- calculate_nutrient_allocation(
    consumption = consumption,
    prey_nutrient_concentrations = prey_p_concentrations,
    nutrient_assimilation_efficiency = p_assimilation_efficiency,
    weight_gain = weight_gain,
    predator_nutrient_concentration = predator_p_concentration,
    nutrient_name = "phosphorus"
  )
  
  # Calculate N:P ratios
  np_ratios <- calculate_np_ratios(nitrogen_result, phosphorus_result)
  
  # Calculate efficiencies
  efficiencies <- calculate_nutrient_efficiencies(nitrogen_result, phosphorus_result)
  
  return(list(
    nitrogen = nitrogen_result,
    phosphorus = phosphorus_result,
    np_ratios = np_ratios,
    efficiencies = efficiencies,
    weight_gain = weight_gain
  ))
}

# ============================================================================
# UTILITY FUNCTIONS: Parameter Calculations and Validation
# ============================================================================

#' Standardize vector length (Utility)
#'
#' Ensures vector has correct length for calculations
#'
#' @param vector Input vector
#' @param target_length Target length
#' @param param_name Parameter name for error messages
#' @return Vector with correct length
#' @keywords internal
standardize_vector_length <- function(vector, target_length, param_name) {
  if (length(vector) == 1) {
    return(rep(vector, target_length))
  } else if (length(vector) != target_length) {
    stop("Length of ", param_name, " does not match consumption vector")
  }
  return(vector)
}

#' Calculate N:P ratios for all processes (Utility)
#'
#' Calculates molar and mass N:P ratios for consumption, growth, excretion and egestion
#'
#' @param nitrogen_fluxes List result from calculate_nitrogen_allocation
#' @param phosphorus_fluxes List result from calculate_phosphorus_allocation
#' @param ratio_type Type of ratio ("mass" or "molar")
#' @return List with N:P ratios
#' @export
calculate_np_ratios <- function(nitrogen_fluxes, phosphorus_fluxes, ratio_type = "mass") {
  
  if (!ratio_type %in% c("mass", "molar")) {
    stop("ratio_type must be 'mass' or 'molar'")
  }
  
  # Conversion factors for molar ratios
  atomic_weight_N <- 14.007
  atomic_weight_P <- 30.974
  
  # Processes to calculate
  processes <- c("consumed", "growth", "excretion", "egestion")
  ratios <- numeric(length(processes))
  names(ratios) <- processes
  
  for (i in seq_along(processes)) {
    process <- processes[i]
    
    n_flux <- nitrogen_fluxes[[process]]
    p_flux <- phosphorus_fluxes[[process]]
    
    if (is.null(n_flux) || is.null(p_flux)) {
      ratios[i] <- NA
      next
    }
    
    if (p_flux == 0) {
      ratios[i] <- if (n_flux == 0) NaN else Inf
    } else {
      if (ratio_type == "mass") {
        ratios[i] <- n_flux / p_flux
      } else {  # molar
        mol_n <- n_flux / atomic_weight_N
        mol_p <- p_flux / atomic_weight_P
        ratios[i] <- mol_n / mol_p
      }
    }
  }
  
  return(list(
    ratios = ratios,
    ratio_type = ratio_type,
    redfield_ratio = if (ratio_type == "molar") 16 else 7.2
  ))
}

#' Calculate nutrient retention efficiencies (Utility)
#'
#' Calculates assimilation and retention efficiencies for N and P
#'
#' @param nitrogen_fluxes List result from calculate_nitrogen_allocation
#' @param phosphorus_fluxes List result from calculate_phosphorus_allocation
#' @return List with calculated efficiencies
#' @export
calculate_nutrient_efficiencies <- function(nitrogen_fluxes, phosphorus_fluxes) {
  
  # Nitrogen efficiencies
  n_consumed <- nitrogen_fluxes$consumed
  n_growth <- nitrogen_fluxes$growth
  n_excretion <- nitrogen_fluxes$excretion
  n_assimilated <- nitrogen_fluxes$assimilated
  
  n_retention_efficiency <- if (n_consumed > 0) n_growth / n_consumed else 0
  n_excretion_rate <- if (n_consumed > 0) n_excretion / n_consumed else 0
  n_growth_efficiency <- if (n_assimilated > 0) n_growth / n_assimilated else 0
  
  # Phosphorus efficiencies
  p_consumed <- phosphorus_fluxes$consumed
  p_growth <- phosphorus_fluxes$growth
  p_excretion <- phosphorus_fluxes$excretion
  p_assimilated <- phosphorus_fluxes$assimilated
  
  p_retention_efficiency <- if (p_consumed > 0) p_growth / p_consumed else 0
  p_excretion_rate <- if (p_consumed > 0) p_excretion / p_consumed else 0
  p_growth_efficiency <- if (p_assimilated > 0) p_growth / p_assimilated else 0
  
  return(list(
    nitrogen = list(
      assimilation_efficiency = nitrogen_fluxes$assimilation_efficiency,
      retention_efficiency = n_retention_efficiency,
      excretion_rate = n_excretion_rate,
      growth_efficiency = n_growth_efficiency
    ),
    phosphorus = list(
      assimilation_efficiency = phosphorus_fluxes$assimilation_efficiency,
      retention_efficiency = p_retention_efficiency,
      excretion_rate = p_excretion_rate,
      growth_efficiency = p_growth_efficiency
    ),
    # Relative efficiencies
    relative_n_retention = if (p_retention_efficiency > 0) n_retention_efficiency / p_retention_efficiency else NA,
    relative_n_excretion = if (p_excretion_rate > 0) n_excretion_rate / p_excretion_rate else NA
  ))
}

#' Validate nutrient concentrations (Utility)
#'
#' Verifies that concentrations are within biologically realistic ranges
#'
#' @param nutrient_concentrations List with N and P concentrations
#' @param organism_type Organism type for validation
#' @return List with validation results
#' @export
validate_nutrient_concentrations <- function(nutrient_concentrations, organism_type = "fish") {
  
  validation <- list(
    valid = TRUE,
    warnings = character(),
    errors = character()
  )
  
  # Typical ranges (g/g wet weight)
  typical_ranges <- list(
    fish = list(nitrogen = c(0.08, 0.12), phosphorus = c(0.01, 0.02)),
    zooplankton = list(nitrogen = c(0.07, 0.11), phosphorus = c(0.008, 0.015)),
    invertebrates = list(nitrogen = c(0.06, 0.10), phosphorus = c(0.006, 0.012))
  )
  
  if (!organism_type %in% names(typical_ranges)) {
    validation$warnings <- c(validation$warnings, 
                             paste("Unrecognized organism type:", organism_type))
    organism_type <- "fish"
  }
  
  ranges <- typical_ranges[[organism_type]]
  
  # Validate nitrogen
  if ("nitrogen" %in% names(nutrient_concentrations)) {
    n_values <- nutrient_concentrations$nitrogen
    
    if (any(n_values < 0, na.rm = TRUE)) {
      validation$errors <- c(validation$errors, "Negative nitrogen concentrations")
      validation$valid <- FALSE
    }
    
    if (any(n_values < ranges$nitrogen[1] | n_values > ranges$nitrogen[2], na.rm = TRUE)) {
      validation$warnings <- c(validation$warnings, 
                               paste("N concentrations outside typical range for", organism_type))
    }
  }
  
  # Validate phosphorus
  if ("phosphorus" %in% names(nutrient_concentrations)) {
    p_values <- nutrient_concentrations$phosphorus
    
    if (any(p_values < 0, na.rm = TRUE)) {
      validation$errors <- c(validation$errors, "Negative phosphorus concentrations")
      validation$valid <- FALSE
    }
    
    if (any(p_values < ranges$phosphorus[1] | p_values > ranges$phosphorus[2], na.rm = TRUE)) {
      validation$warnings <- c(validation$warnings, 
                               paste("P concentrations outside typical range for", organism_type))
    }
  }
  
  # Validate N:P ratio
  if ("nitrogen" %in% names(nutrient_concentrations) && 
      "phosphorus" %in% names(nutrient_concentrations)) {
    
    n_values <- nutrient_concentrations$nitrogen
    p_values <- nutrient_concentrations$phosphorus
    
    # Avoid division by zero
    valid_indices <- !is.na(p_values) & p_values > 0
    if (any(valid_indices)) {
      np_ratios <- n_values[valid_indices] / p_values[valid_indices]
      
      # Typical range for N:P ratios (mass): 4-15
      if (any(np_ratios < 2 | np_ratios > 20, na.rm = TRUE)) {
        validation$warnings <- c(validation$warnings, 
                                 "N:P ratios outside typical range (2-20)")
      }
    }
  }
  
  return(validation)
}

# ============================================================================
# HIGH-LEVEL ANALYSIS FUNCTIONS
# ============================================================================

#' Estimate ecosystem impact of nutrient excretion
#'
#' Calculates potential ecosystem impact of nutrient excretion
#'
#' @param daily_n_excretion Daily nitrogen excretion (g N/day)
#' @param daily_p_excretion Daily phosphorus excretion (g P/day)
#' @param fish_biomass Total fish biomass in system (g)
#' @param water_volume Water volume of system (L)
#' @param simulation_days Number of simulation days
#' @return List with ecosystem impact estimates
#' @export
calculate_ecosystem_impact <- function(daily_n_excretion, daily_p_excretion, fish_biomass,
                                       water_volume, simulation_days) {
  
  # Validate inputs
  fish_biomass <- check_numeric_value(fish_biomass, "fish_biomass", min_val = 0)
  water_volume <- check_numeric_value(water_volume, "water_volume", min_val = 1)
  simulation_days <- check_numeric_value(simulation_days, "simulation_days", min_val = 1)
  
  # Total excretion for entire fish population
  total_n_excretion_per_day <- daily_n_excretion * fish_biomass  # g N/day
  total_p_excretion_per_day <- daily_p_excretion * fish_biomass  # g P/day
  
  # Cumulative excretion during simulation period
  total_n_excretion <- total_n_excretion_per_day * simulation_days  # g N
  total_p_excretion <- total_p_excretion_per_day * simulation_days  # g P
  
  # Potential water concentrations (mg/L)
  # Assuming complete mixing and no losses
  n_concentration_mgL <- (total_n_excretion / water_volume) * 1000
  p_concentration_mgL <- (total_p_excretion / water_volume) * 1000
  
  # Daily nutrient addition (mg/L/day)
  daily_n_addition <- (total_n_excretion_per_day / water_volume) * 1000
  daily_p_addition <- (total_p_excretion_per_day / water_volume) * 1000
  
  # N:P ratio in excretion
  excretion_np_ratio <- if (total_p_excretion > 0) total_n_excretion / total_p_excretion else Inf
  
  # Simple trophic status estimation based on P
  trophic_status <- if (p_concentration_mgL < 0.01) {
    "Oligotrophic"
  } else if (p_concentration_mgL < 0.05) {
    "Mesotrophic"
  } else {
    "Eutrophic"
  }
  
  return(list(
    # Total excretion
    total_n_excretion_g = total_n_excretion,
    total_p_excretion_g = total_p_excretion,
    
    # Daily rates
    daily_n_excretion_g = total_n_excretion_per_day,
    daily_p_excretion_g = total_p_excretion_per_day,
    
    # Potential concentrations
    n_concentration_mgL = n_concentration_mgL,
    p_concentration_mgL = p_concentration_mgL,
    daily_n_addition_mgL = daily_n_addition,
    daily_p_addition_mgL = daily_p_addition,
    
    # Assessment
    excretion_np_ratio = excretion_np_ratio,
    trophic_status = trophic_status,
    
    # System parameters
    fish_biomass_g = fish_biomass,
    water_volume_L = water_volume,
    simulation_days = simulation_days
  ))
}

#' Compare with Redfield ratios
#'
#' Compares calculated N:P ratios with Redfield ratio
#'
#' @param np_ratios List result from calculate_np_ratios
#' @return Data frame with comparison
#' @export
compare_with_redfield <- function(np_ratios) {
  
  redfield_ratio <- np_ratios$redfield_ratio
  
  comparison <- data.frame(
    Process = names(np_ratios$ratios),
    NP_Ratio = np_ratios$ratios,
    Redfield_Ratio = redfield_ratio,
    Difference = np_ratios$ratios - redfield_ratio,
    Relative_Difference = ((np_ratios$ratios - redfield_ratio) / redfield_ratio) * 100,
    stringsAsFactors = FALSE
  )
  
  # Add interpretation
  comparison$Interpretation <- ifelse(
    is.infinite(comparison$NP_Ratio), "No P available",
    ifelse(is.nan(comparison$NP_Ratio), "No flux",
           ifelse(comparison$NP_Ratio > redfield_ratio,
                  "N-rich relative to P",
                  "N-poor relative to P"))
  )
  
  return(comparison)
}

#' Calculate stoichiometric balance
#'
#' Analyzes nutritional limitations based on N:P ratios
#'
#' @param nutrient_balance List result from calculate_nutrient_balance
#' @return List with stoichiometric analysis
#' @export
calculate_stoichiometric_balance <- function(nutrient_balance) {
  
  np_ratios <- nutrient_balance$np_ratios
  nitrogen <- nutrient_balance$nitrogen
  phosphorus <- nutrient_balance$phosphorus
  
  # Determine nutritional limitation in consumption
  consumption_np <- np_ratios$ratios["consumed"]
  redfield_ratio <- np_ratios$redfield_ratio
  
  if (is.finite(consumption_np)) {
    if (consumption_np > redfield_ratio) {
      nutrient_limitation <- "P-limited"
      limiting_nutrient <- "phosphorus"
    } else {
      nutrient_limitation <- "N-limited"
      limiting_nutrient <- "nitrogen"
    }
  } else {
    nutrient_limitation <- "Undetermined"
    limiting_nutrient <- "unknown"
  }
  
  # Calculate nutrient excess
  if (limiting_nutrient == "phosphorus") {
    # Excess N relative to P
    excess_factor <- consumption_np / redfield_ratio
    excess_nutrient <- "nitrogen"
  } else if (limiting_nutrient == "nitrogen") {
    # Excess P relative to N
    excess_factor <- redfield_ratio / consumption_np
    excess_nutrient <- "phosphorus"
  } else {
    excess_factor <- 1
    excess_nutrient <- "none"
  }
  
  # Efficiency of limiting nutrient use
  if (limiting_nutrient == "nitrogen") {
    limiting_efficiency <- nutrient_balance$efficiencies$nitrogen$retention_efficiency
  } else if (limiting_nutrient == "phosphorus") {
    limiting_efficiency <- nutrient_balance$efficiencies$phosphorus$retention_efficiency
  } else {
    limiting_efficiency <- NA
  }
  
  return(list(
    nutrient_limitation = nutrient_limitation,
    limiting_nutrient = limiting_nutrient,
    excess_nutrient = excess_nutrient,
    excess_factor = excess_factor,
    limiting_efficiency = limiting_efficiency,
    consumption_np_ratio = consumption_np,
    redfield_ratio = redfield_ratio,
    np_deviation = consumption_np - redfield_ratio
  ))
}