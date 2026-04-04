#' Nutritional Analysis Functions for FB4 Results
#'
#' @description
#' Specialized functions for nutritional analysis of FB4 simulation results.
#' Includes N:P ratio analysis, nutrient efficiency calculations, 
#' stoichiometric balance assessment, and body composition analysis.
#'
#' @name analysis-nutritional
#' @aliases analysis-nutritional
NULL

# ============================================================================
# N:P RATIO ANALYSIS FUNCTIONS
# ============================================================================

#' Calculate N:P ratios for all processes
#'
#' @description
#' Calculates molar and mass N:P ratios for consumption, growth, excretion and egestion.
#' Useful for understanding nutritional ecology and stoichiometric balance.
#'
#' @param nitrogen_fluxes List result from calculate_nutrient_balance (nitrogen component)
#' @param phosphorus_fluxes List result from calculate_nutrient_balance (phosphorus component)
#' @param ratio_type Type of ratio ("mass" or "molar"), default "mass"
#' @return List with N:P ratios for all processes
#' @export
#'
#' @examples
#' nitrogen   <- list(consumed = 10, growth = 3, excretion = 5, egestion = 2)
#' phosphorus <- list(consumed = 1.5, growth = 0.5, excretion = 0.6, egestion = 0.4)
#' np_ratios  <- calculate_np_ratios(nitrogen, phosphorus)
#' np_ratios$ratios
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

#' Compare N:P ratios with Redfield ratios
#'
#' @description
#' Compares calculated N:P ratios with the classical Redfield ratio.
#' Useful for understanding deviations from typical oceanic proportions.
#'
#' @param np_ratios List result from calculate_np_ratios
#' @return Data frame with comparison results and interpretation
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

# ============================================================================
# NUTRIENT EFFICIENCY FUNCTIONS
# ============================================================================

#' Compute efficiency metrics for a single nutrient
#'
#' @description
#' Internal helper that calculates retention, excretion, and growth efficiencies
#' from raw flux values.  Used by \code{calculate_nutrient_efficiencies()} to
#' avoid duplicating the identical calculation for nitrogen and phosphorus.
#'
#' @param consumed  Total nutrient consumed (same units as other flux args).
#' @param growth    Nutrient retained in growth.
#' @param excretion Nutrient lost via excretion.
#' @param assimilated Nutrient assimilated (consumed minus egested).
#'
#' @return Named list: \code{retention_efficiency}, \code{excretion_rate},
#'   \code{growth_efficiency}.
#' @keywords internal
nutrient_efficiency_block <- function(consumed, growth, excretion, assimilated) {
  list(
    retention_efficiency = if (consumed    > 0) growth    / consumed    else 0,
    excretion_rate       = if (consumed    > 0) excretion / consumed    else 0,
    growth_efficiency    = if (assimilated > 0) growth    / assimilated else 0
  )
}

#' Calculate nutrient retention efficiencies
#'
#' @description
#' Calculates assimilation and retention efficiencies for nitrogen and phosphorus.
#' These metrics are important for understanding nutrient use efficiency.
#'
#' @param nitrogen_fluxes List result from calculate_nutrient_balance (nitrogen component)
#' @param phosphorus_fluxes List result from calculate_nutrient_balance (phosphorus component)
#' @return List with calculated efficiencies for both nutrients
#' @export
calculate_nutrient_efficiencies <- function(nitrogen_fluxes, phosphorus_fluxes) {

  n_eff <- nutrient_efficiency_block(
    nitrogen_fluxes$consumed, nitrogen_fluxes$growth,
    nitrogen_fluxes$excretion, nitrogen_fluxes$assimilated
  )
  p_eff <- nutrient_efficiency_block(
    phosphorus_fluxes$consumed, phosphorus_fluxes$growth,
    phosphorus_fluxes$excretion, phosphorus_fluxes$assimilated
  )

  list(
    nitrogen = c(
      list(assimilation_efficiency = nitrogen_fluxes$assimilation_efficiency),
      n_eff
    ),
    phosphorus = c(
      list(assimilation_efficiency = phosphorus_fluxes$assimilation_efficiency),
      p_eff
    ),
    relative_n_retention = if (p_eff$retention_efficiency > 0) n_eff$retention_efficiency / p_eff$retention_efficiency else NA,
    relative_n_excretion = if (p_eff$excretion_rate       > 0) n_eff$excretion_rate       / p_eff$excretion_rate       else NA
  )
}

# ============================================================================
# STOICHIOMETRIC ANALYSIS FUNCTIONS
# ============================================================================

#' Calculate stoichiometric balance
#'
#' @description
#' Analyzes nutritional limitations based on N:P ratios and determines
#' which nutrient is limiting growth.
#'
#' @param nutrient_balance Complete list result from calculate_nutrient_balance with efficiencies and ratios
#' @return List with stoichiometric analysis and limitation assessment
#' @export
calculate_stoichiometric_balance <- function(nutrient_balance) {
  
  # Calculate N:P ratios if not already present
  if (is.null(nutrient_balance$np_ratios)) {
    np_ratios <- calculate_np_ratios(nutrient_balance$nitrogen, nutrient_balance$phosphorus)
  } else {
    np_ratios <- nutrient_balance$np_ratios
  }
  
  # Calculate efficiencies if not already present
  if (is.null(nutrient_balance$efficiencies)) {
    efficiencies <- calculate_nutrient_efficiencies(nutrient_balance$nitrogen, nutrient_balance$phosphorus)
  } else {
    efficiencies <- nutrient_balance$efficiencies
  }
  
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
    limiting_efficiency <- efficiencies$nitrogen$retention_efficiency
  } else if (limiting_nutrient == "phosphorus") {
    limiting_efficiency <- efficiencies$phosphorus$retention_efficiency
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
    np_deviation = consumption_np - redfield_ratio,
    efficiencies = efficiencies,
    np_ratios = np_ratios
  ))
}

# ============================================================================
# BODY COMPOSITION ANALYSIS FUNCTIONS
# ============================================================================

#' Build a body-composition data frame from a list of composition results
#'
#' @description
#' Internal helper shared by \code{analyze_composition_by_size()} and
#' \code{analyze_composition_changes()}.  Converts a list of
#' \code{calculate_body_composition()} outputs into a tidy data frame,
#' avoiding duplication of the identical \code{sapply} + \code{data.frame}
#' block.
#'
#' @param compositions List of results from \code{calculate_body_composition()},
#'   one element per weight point or time step.
#' @param weights Numeric vector of fish weights corresponding to each element.
#'
#' @return Data frame with columns \code{Weight}, \code{Water_g},
#'   \code{Protein_g}, \code{Ash_g}, \code{Fat_g}, and the corresponding
#'   \code{*_fraction} and \code{Energy_density} columns.
#' @keywords internal
build_composition_df <- function(compositions, weights) {
  data.frame(
    Weight           = weights,
    Water_g          = vapply(compositions, `[[`, numeric(1), "water_g"),
    Protein_g        = vapply(compositions, `[[`, numeric(1), "protein_g"),
    Ash_g            = vapply(compositions, `[[`, numeric(1), "ash_g"),
    Fat_g            = vapply(compositions, `[[`, numeric(1), "fat_g"),
    Water_fraction   = vapply(compositions, `[[`, numeric(1), "water_fraction"),
    Protein_fraction = vapply(compositions, `[[`, numeric(1), "protein_fraction"),
    Ash_fraction     = vapply(compositions, `[[`, numeric(1), "ash_fraction"),
    Fat_fraction     = vapply(compositions, `[[`, numeric(1), "fat_fraction"),
    Energy_density   = vapply(compositions, `[[`, numeric(1), "energy_density"),
    stringsAsFactors = FALSE
  )
}

#' Analyze body composition by size range
#'
#' @description
#' Analyzes body composition across a range of fish sizes to understand
#' allometric relationships and size-dependent changes.
#'
#' @param weight_range Weight range to analyze (2-element vector), default c(1, 500)
#' @param n_points Number of points to analyze, default 50
#' @param processed_composition_params Processed composition parameters
#' @return Data frame with composition analysis by size
#' @export
#'
#' @examples
#' comp_params <- process_composition_params(list())
#' comp_analysis <- analyze_composition_by_size(c(1, 500), 20, comp_params)
#' plot(comp_analysis$Weight, comp_analysis$Energy_density)
analyze_composition_by_size <- function(weight_range = c(1, 500), 
                                        n_points = 50, 
                                        processed_composition_params) {
  
  # Create weight sequence
  weights <- seq(weight_range[1], weight_range[2], length.out = n_points)
  
  compositions <- lapply(weights, function(w) {
    calculate_body_composition(w, processed_composition_params)
  })

  return(build_composition_df(compositions, weights))
}

#' Analyze composition changes with growth
#'
#' @description
#' Analyzes how body composition changes as fish grow during a simulation.
#' Useful for understanding ontogenetic changes in energy density and
#' macronutrient allocation.
#'
#' @param result FB4 result object with daily output
#' @param processed_composition_params Processed composition parameters
#' @return Data frame with composition time series
#' @export
analyze_composition_changes <- function(result, processed_composition_params) {
  
  if (!is.fb4_result(result)) {
    stop("Input must be an fb4_result object")
  }
  
  if (is.null(result$daily_output)) {
    stop("Daily output required for composition change analysis")
  }
  
  daily_data <- result$daily_output
  weights <- daily_data$Weight
  n_days <- length(weights)
  
  compositions <- lapply(weights, function(w) {
    calculate_body_composition(w, processed_composition_params)
  })

  composition_df <- build_composition_df(compositions, weights)
  composition_df$Day <- seq_len(n_days)
  
  # Calculate changes over time
  composition_df$Energy_density_change <- c(NA, diff(composition_df$Energy_density))
  composition_df$Fat_fraction_change <- c(NA, diff(composition_df$Fat_fraction))
  composition_df$Protein_fraction_change <- c(NA, diff(composition_df$Protein_fraction))
  
  return(composition_df)
}

# ============================================================================
# NUTRITIONAL QUALITY ASSESSMENT FUNCTIONS
# ============================================================================

#' Assess nutritional quality of diet
#'
#' @description
#' Assesses the nutritional quality of the diet based on energy density,
#' macronutrient composition, and digestibility of prey items.
#'
#' @param diet_data Diet composition data from FB4 simulation
#' @param prey_energies Energy densities of prey items
#' @param prey_digestibility Digestibility coefficients of prey items  
#' @return List with nutritional quality metrics
#' @export
assess_diet_quality <- function(diet_data, prey_energies, prey_digestibility = NULL) {
  
  # Calculate weighted average energy density
  if (is.matrix(diet_data$proportions) || is.data.frame(diet_data$proportions)) {
    # Time-varying diet
    daily_props <- as.matrix(diet_data$proportions[, -1])  # Remove Day column if present
    daily_energies <- as.matrix(prey_energies[, -1])       # Remove Day column if present
    
    daily_energy_density <- rowSums(daily_props * daily_energies, na.rm = TRUE)
    
    diet_quality <- list(
      mean_energy_density = mean(daily_energy_density, na.rm = TRUE),
      energy_density_sd = sd(daily_energy_density, na.rm = TRUE),
      energy_density_range = range(daily_energy_density, na.rm = TRUE),
      daily_energy_density = daily_energy_density
    )
    
    # Calculate digestibility if provided
    if (!is.null(prey_digestibility)) {
      if (is.matrix(prey_digestibility) || is.data.frame(prey_digestibility)) {
        daily_digest <- as.matrix(prey_digestibility[, -1])
        daily_digestibility <- rowSums(daily_props * daily_digest, na.rm = TRUE)
        
        diet_quality$mean_digestibility <- mean(daily_digestibility, na.rm = TRUE)
        diet_quality$digestibility_sd <- sd(daily_digestibility, na.rm = TRUE)
        diet_quality$daily_digestibility <- daily_digestibility
      }
    }
    
  } else {
    # Static diet
    props <- diet_data$proportions
    energies <- prey_energies
    
    diet_quality <- list(
      mean_energy_density = sum(props * energies, na.rm = TRUE),
      energy_density_sd = 0,  # No variation in static diet
      energy_density_range = rep(sum(props * energies, na.rm = TRUE), 2)
    )
    
    if (!is.null(prey_digestibility)) {
      diet_quality$mean_digestibility <- sum(props * prey_digestibility, na.rm = TRUE)
      diet_quality$digestibility_sd <- 0
    }
  }
  
  # Diet diversity (Shannon diversity index)
  if (is.matrix(daily_props) || is.data.frame(daily_props)) {
    shannon_diversity <- apply(daily_props, 1, function(p) {
      p <- p[p > 0]  # Remove zero proportions
      -sum(p * log(p), na.rm = TRUE)
    })
    
    diet_quality$diversity <- list(
      mean_shannon = mean(shannon_diversity, na.rm = TRUE),
      shannon_sd = sd(shannon_diversity, na.rm = TRUE),
      daily_shannon = shannon_diversity
    )
  } else {
    props_nonzero <- props[props > 0]
    diet_quality$diversity <- list(
      mean_shannon = -sum(props_nonzero * log(props_nonzero), na.rm = TRUE),
      shannon_sd = 0
    )
  }
  
  return(diet_quality)
}

# ============================================================================
# INTEGRATED NUTRITIONAL ANALYSIS
# ============================================================================

#' Comprehensive nutritional analysis
#'
#' @description
#' Performs a comprehensive nutritional analysis combining all nutritional
#' metrics including N:P ratios, nutrient efficiencies, body composition,
#' and diet quality assessment.
#'
#' @param result FB4 result object
#' @param nutrient_balance Nutrient balance results (if available)
#' @param composition_params Body composition parameters (if available)
#' @param diet_quality_data Diet quality data (if available)
#' @return List with comprehensive nutritional analysis
#' @export
comprehensive_nutritional_analysis <- function(result, 
                                              nutrient_balance = NULL,
                                              composition_params = NULL,
                                              diet_quality_data = NULL) {
  
  if (!is.fb4_result(result)) {
    stop("Input must be an fb4_result object")
  }
  
  analysis <- list(
    model_info = list(
      method = result$summary$method,
      has_daily_output = !is.null(result$daily_output)
    )
  )
  
  # Energy budget analysis (always available)
  analysis$energy_budget <- analyze_energy_budget(result)
  
  # N:P ratio analysis (if nutrient data available)
  if (!is.null(nutrient_balance)) {
    analysis$np_ratios <- calculate_np_ratios(nutrient_balance$nitrogen, nutrient_balance$phosphorus)
    analysis$redfield_comparison <- compare_with_redfield(analysis$np_ratios)
    analysis$nutrient_efficiencies <- calculate_nutrient_efficiencies(nutrient_balance$nitrogen, nutrient_balance$phosphorus)
    analysis$stoichiometric_balance <- calculate_stoichiometric_balance(nutrient_balance)
  }
  
  # Body composition analysis (if composition parameters available)
  if (!is.null(composition_params)) {
    # Static composition analysis based on initial and final weights
    initial_weight <- result$summary$initial_weight %||% NA
    final_weight <- result$summary$final_weight %||% result$summary$predicted_weight %||% NA
    
    if (!is.na(initial_weight) && !is.na(final_weight)) {
      initial_comp <- calculate_body_composition(initial_weight, composition_params)
      final_comp <- calculate_body_composition(final_weight, composition_params)
      
      analysis$body_composition <- list(
        initial = initial_comp,
        final = final_comp,
        changes = list(
          energy_density_change = final_comp$energy_density - initial_comp$energy_density,
          fat_fraction_change = final_comp$fat_fraction - initial_comp$fat_fraction,
          protein_fraction_change = final_comp$protein_fraction - initial_comp$protein_fraction
        )
      )
    }
    
    # Time series composition analysis (if daily output available)
    if (!is.null(result$daily_output)) {
      analysis$composition_timeseries <- analyze_composition_changes(result, composition_params)
    }
  }
  
  # Diet quality analysis (if diet data available)
  if (!is.null(diet_quality_data)) {
    analysis$diet_quality <- assess_diet_quality(
      diet_quality_data$diet_data,
      diet_quality_data$prey_energies,
      diet_quality_data$prey_digestibility
    )
  }
  
  # Integration and summary
  analysis$summary <- list(
    primary_limitation = if (!is.null(analysis$stoichiometric_balance)) {
      analysis$stoichiometric_balance$nutrient_limitation
    } else {
      "Energy-based (no nutrient data)"
    },
    
    growth_efficiency = analysis$energy_budget$summary_metrics$gross_growth_efficiency$estimate %||% NA,
    
    metabolic_scope = analysis$energy_budget$summary_metrics$metabolic_scope$estimate %||% NA,
    
    energy_balance_error = analysis$energy_budget$balance_check$relative_error %||% NA
  )
  
  return(analysis)
}