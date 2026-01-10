#' Sensitivity and Comparative Analysis Functions for FB4 Results
#'
#' @description
#' Functions for sensitivity analysis, comparative studies, and population-level
#' analysis of FB4 simulation results. Includes individual comparisons,
#' parameter sensitivity assessment, and scenario analysis.
#'
#' @name analysis-sensitivity
#' @aliases analysis-sensitivity
NULL

# ============================================================================
# INDIVIDUAL COMPARISON FUNCTIONS
# ============================================================================

#' Compare individuals from hierarchical models
#'
#' @description
#' Compares performance metrics between individuals in hierarchical models.
#' Provides statistical summaries and identifies outliers.
#'
#' @param result FB4 result object from hierarchical method
#' @param metrics Vector of metrics to compare ("consumption", "growth", "efficiency", "all")
#' @param confidence_level Confidence level for comparisons (default 0.95)
#' @return List with individual comparisons and statistics
#' @export
#'
#' @examples
#' \dontrun{
#' # Compare all individuals across all metrics
#' comparison <- compare_individuals(hierarchical_result)
#' 
#' # Compare only specific metrics
#' comparison <- compare_individuals(hierarchical_result, metrics = c("consumption", "growth"))
#' }
compare_individuals <- function(result, metrics = "all", confidence_level = 0.95) {
  
  result_type <- detect_result_type(result)
  
  if (result_type$method != "hierarchical") {
    stop("Individual comparisons are only available for hierarchical models")
  }
  
  # Get individual results
  individual_data <- get_individual_results(result, confidence_level)
  n_individuals <- nrow(individual_data)
  
  # Define available metrics
  available_metrics <- c("consumption", "growth", "efficiency", "p_value")
  
  if ("all" %in% metrics) {
    metrics <- available_metrics
  } else {
    metrics <- intersect(metrics, available_metrics)
    if (length(metrics) == 0) {
      stop("No valid metrics specified. Available: ", paste(available_metrics, collapse = ", "))
    }
  }
  
  comparison_results <- list(
    n_individuals = n_individuals,
    metrics_compared = metrics,
    confidence_level = confidence_level
  )
  
  # Consumption comparison
  if ("consumption" %in% metrics) {
    consumption_data <- list(
      estimates = individual_data$consumption_est,
      se = individual_data$consumption_se,
      ci_lower = individual_data$consumption_ci_lower,
      ci_upper = individual_data$consumption_ci_upper
    )
    
    comparison_results$consumption <- create_individual_summary(consumption_data, "Total Consumption (g)")
  }
  
  # Growth comparison
  if ("growth" %in% metrics) {
    # Total growth
    total_growth_data <- list(
      estimates = individual_data$total_growth_est,
      se = individual_data$total_growth_se,
      ci_lower = individual_data$total_growth_ci_lower,
      ci_upper = individual_data$total_growth_ci_upper
    )
    
    # Relative growth
    relative_growth_data <- list(
      estimates = individual_data$relative_growth_est,
      se = individual_data$relative_growth_se,
      ci_lower = individual_data$relative_growth_ci_lower,
      ci_upper = individual_data$relative_growth_ci_upper
    )
    
    comparison_results$growth <- list(
      total_growth = create_individual_summary(total_growth_data, "Total Growth (g)"),
      relative_growth = create_individual_summary(relative_growth_data, "Relative Growth (%)")
    )
  }
  
  # Efficiency comparison
  if ("efficiency" %in% metrics) {
    efficiency_data <- list(
      estimates = individual_data$gross_efficiency_est,
      se = individual_data$gross_efficiency_se,
      ci_lower = individual_data$gross_efficiency_ci_lower,
      ci_upper = individual_data$gross_efficiency_ci_upper
    )
    
    comparison_results$efficiency <- create_individual_summary(efficiency_data, "Gross Growth Efficiency")
  }
  
  # p_value comparison
  if ("p_value" %in% metrics) {
    p_value_data <- list(
      estimates = individual_data$p_estimate,
      se = individual_data$p_se,
      ci_lower = NA,  # Will be calculated if SE available
      ci_upper = NA
    )
    
    comparison_results$p_value <- create_individual_summary(p_value_data, "p_value (Feeding Rate)")
  }
  
  # Overall ranking
  comparison_results$rankings <- create_individual_rankings(individual_data, metrics)
  
  return(comparison_results)
}

#' Create summary statistics for individual metric
#'
#' @description
#' Internal function to create summary statistics for a single metric
#' across all individuals.
#'
#' @param data List with estimates, se, ci_lower, ci_upper
#' @param metric_name Name of the metric for reporting
#' @return List with summary statistics
#' @keywords internal
create_individual_summary <- function(data, metric_name) {
  
  estimates <- data$estimates
  valid_estimates <- estimates[!is.na(estimates)]
  
  if (length(valid_estimates) == 0) {
    return(list(
      metric_name = metric_name,
      summary = "No valid data available",
      n_valid = 0
    ))
  }
  
  summary_stats <- list(
    metric_name = metric_name,
    n_valid = length(valid_estimates),
    mean = mean(valid_estimates),
    sd = sd(valid_estimates),
    min = min(valid_estimates),
    max = max(valid_estimates),
    median = median(valid_estimates),
    cv = sd(valid_estimates) / mean(valid_estimates) * 100,  # Coefficient of variation
    range = max(valid_estimates) - min(valid_estimates)
  )
  
  # Identify outliers (beyond 2 SD)
  outlier_threshold <- 2
  mean_val <- summary_stats$mean
  sd_val <- summary_stats$sd
  
  outliers <- which(abs(estimates - mean_val) > outlier_threshold * sd_val)
  
  summary_stats$outliers <- list(
    individual_ids = outliers,
    n_outliers = length(outliers),
    outlier_values = estimates[outliers]
  )
  
  # Best and worst performers
  if (length(valid_estimates) > 0) {
    best_id <- which.max(estimates)
    worst_id <- which.min(estimates)
    
    summary_stats$performance <- list(
      best_individual = best_id,
      best_value = estimates[best_id],
      worst_individual = worst_id,
      worst_value = estimates[worst_id],
      performance_ratio = estimates[best_id] / estimates[worst_id]
    )
  }
  
  return(summary_stats)
}

#' Create individual rankings across metrics
#'
#' @description
#' Ranks individuals across multiple metrics and creates composite scores.
#'
#' @param individual_data Data frame with individual results
#' @param metrics Vector of metrics to include in ranking
#' @return Data frame with rankings
#' @keywords internal
create_individual_rankings <- function(individual_data, metrics) {
  
  n_individuals <- nrow(individual_data)
  ranking_data <- data.frame(
    individual_id = 1:n_individuals,
    stringsAsFactors = FALSE
  )
  
  # Rank each metric (higher values = better ranks)
  if ("consumption" %in% metrics && "consumption_est" %in% names(individual_data)) {
    ranking_data$consumption_rank <- rank(-individual_data$consumption_est, na.last = "keep")
  }
  
  if ("growth" %in% metrics && "relative_growth_est" %in% names(individual_data)) {
    ranking_data$growth_rank <- rank(-individual_data$relative_growth_est, na.last = "keep")
  }
  
  if ("efficiency" %in% metrics && "gross_efficiency_est" %in% names(individual_data)) {
    ranking_data$efficiency_rank <- rank(-individual_data$gross_efficiency_est, na.last = "keep")
  }
  
  if ("p_value" %in% metrics && "p_estimate" %in% names(individual_data)) {
    ranking_data$p_value_rank <- rank(-individual_data$p_estimate, na.last = "keep")
  }
  
  # Calculate composite rank (average of available ranks)
  rank_columns <- grep("_rank$", names(ranking_data), value = TRUE)
  
  if (length(rank_columns) > 0) {
    ranking_data$composite_rank <- apply(ranking_data[, rank_columns, drop = FALSE], 1, function(x) {
      valid_ranks <- x[!is.na(x)]
      if (length(valid_ranks) > 0) mean(valid_ranks) else NA
    })
    
    ranking_data$overall_rank <- rank(ranking_data$composite_rank, na.last = "keep")
  }
  
  return(ranking_data)
}

# ============================================================================
# POPULATION VARIATION ANALYSIS
# ============================================================================

#' Analyze population variation in hierarchical models
#'
#' @description
#' Analyzes the magnitude and sources of variation in hierarchical models.
#' Decomposes total variation into between-individual and within-individual components.
#'
#' @param result FB4 result object from hierarchical method
#' @param include_covariates Include covariate effects in analysis
#' @return List with population variation analysis
#' @export
analyze_population_variation <- function(result, include_covariates = TRUE) {
  
  result_type <- detect_result_type(result)
  
  if (result_type$method != "hierarchical") {
    stop("Population variation analysis is only available for hierarchical models")
  }
  
  # Get population and individual results
  pop_results <- get_population_results(result)
  ind_results <- get_individual_results(result)
  
  variation_analysis <- list(
    n_individuals = pop_results$n_individuals,
    population_parameters = list(
      mu_p = list(
        estimate = pop_results$mu_p_estimate,
        se = pop_results$mu_p_se,
        ci_lower = pop_results$mu_p_ci_lower,
        ci_upper = pop_results$mu_p_ci_upper
      ),
      sigma_p = list(
        estimate = pop_results$sigma_p_estimate,
        se = pop_results$sigma_p_se,
        ci_lower = pop_results$sigma_p_ci_lower,
        ci_upper = pop_results$sigma_p_ci_upper
      ),
      sigma_obs = list(
        estimate = pop_results$sigma_obs_estimate,
        se = pop_results$sigma_obs_se,
        ci_lower = pop_results$sigma_obs_ci_lower,
        ci_upper = pop_results$sigma_obs_ci_upper
      )
    )
  )
  
  # Variance decomposition
  sigma_p <- pop_results$sigma_p_estimate %||% 0
  sigma_obs <- pop_results$sigma_obs_estimate %||% 0
  
  total_variance <- sigma_p^2 + sigma_obs^2
  
  if (total_variance > 0) {
    variation_analysis$variance_decomposition <- list(
      between_individual_variance = sigma_p^2,
      within_individual_variance = sigma_obs^2,
      total_variance = total_variance,
      between_individual_prop = sigma_p^2 / total_variance,
      within_individual_prop = sigma_obs^2 / total_variance,
      intraclass_correlation = sigma_p^2 / total_variance
    )
  }
  
  # Individual variation in outcomes
  if (!is.null(ind_results$consumption_est)) {
    consumption_var <- var(ind_results$consumption_est, na.rm = TRUE)
    consumption_mean <- mean(ind_results$consumption_est, na.rm = TRUE)
    
    variation_analysis$outcome_variation <- list(
      consumption = list(
        variance = consumption_var,
        cv = sqrt(consumption_var) / consumption_mean * 100,
        range = range(ind_results$consumption_est, na.rm = TRUE)
      )
    )
    
    # Add growth variation if available
    if (!is.null(ind_results$relative_growth_est)) {
      growth_var <- var(ind_results$relative_growth_est, na.rm = TRUE)
      growth_mean <- mean(ind_results$relative_growth_est, na.rm = TRUE)
      
      variation_analysis$outcome_variation$growth <- list(
        variance = growth_var,
        cv = sqrt(growth_var) / growth_mean * 100,
        range = range(ind_results$relative_growth_est, na.rm = TRUE)
      )
    }
  }
  
  # Covariate effects (if available)
  if (include_covariates && !is.null(result$method_data$population_results$betas)) {
    betas <- result$method_data$population_results$betas %||% numeric(0)
    
    if (length(betas) > 0) {
      variation_analysis$covariate_effects <- list(
        n_covariates = length(betas),
        beta_estimates = betas,
        # Add more covariate analysis here if needed
        significant_effects = abs(betas) > 0.1  # Simple threshold
      )
    }
  }
  
  return(variation_analysis)
}

# ============================================================================
# SENSITIVITY ANALYSIS FUNCTIONS
# ============================================================================

#' Analyze growth rate sensitivity to temperature and feeding levels
#'
#' @description
#' Analyzes how growth rates respond to different temperature and feeding level combinations.
#' Uses p-values (proportion of maximum consumption capacity) to simulate feeding scenarios
#' from survival levels (p ~0.2) to maximum capacity (p = 1.0). Parallelized for efficiency.
#'
#' @param bio_obj Bioenergetic object containing species parameters and environmental data
#' @param temperatures Vector of temperatures to test in °C (default: 8-18°C by 2°C steps)
#' @param p_values Vector of p-values representing feeding levels as proportion of Cmax (default: 0.3-1.0)
#' @param simulation_days Number of days to simulate (default: 365)
#' @param oxycal Oxycalorific coefficient in J/g O2 (default: 13560)
#' @param parallel Use parallel processing (default: FALSE)
#' @param n_cores Number of cores for parallel processing (default: detectCores() - 1)
#' @param verbose Show progress information (default: TRUE)
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic sensitivity analysis
#' results <- analyze_growth_temperature_sensitivity(bio_obj)
#' 
#' # Custom parameter ranges for cold water species
#' results <- analyze_growth_temperature_sensitivity(
#'   bio_obj,
#'   temperatures = seq(4, 14, by = 2),
#'   p_values = seq(0.2, 0.9, by = 0.1)
#' )
#' }
analyze_growth_temperature_sensitivity <- function(bio_obj,
                                     temperatures = seq(8, 18, by = 2),
                                     p_values = seq(0.3, 1.0, by = 0.1),
                                     simulation_days = 365,
                                     oxycal = 13560,
                                     parallel = FALSE,
                                     n_cores = NULL,
                                     verbose = TRUE) {
  
  # Setup parallel processing if requested
  if (parallel) {
    library(future)
    library(furrr)
    
    if (is.null(n_cores)) {
      n_cores <- availableCores() - 1
    }
    
    # Setup future plan
    plan(multisession, workers = min(n_cores, length(temperatures)))
    on.exit(plan(sequential), add = TRUE)  # Cleanup on exit
  }
  
  # Input validation
  if (!is.Bioenergetic(bio_obj)) {
    stop("Input must be a Bioenergetic object")
  }
  
  if (any(p_values <= 0) || any(p_values > 1.0)) {
    stop("p_values must be between 0 and 1.0 (proportion of Cmax)")
  }
  
  # Prepare simulation data once
  processed_data <- prepare_simulation_data(
    bio_obj = bio_obj, 
    strategy = "direct", 
    first_day = 1, 
    last_day = simulation_days,
    validate_inputs = FALSE
  )
  
  # Progress information
  if (verbose) {
    cat("=== GROWTH SENSITIVITY ANALYSIS ===\n")
    if (parallel) {
      cat("Mode: PARALLEL - Multisession (", min(n_cores, length(temperatures)), "cores)\n")
    } else {
      cat("Mode: SEQUENTIAL\n")
    }
    cat("Temperature range:", min(temperatures), "-", max(temperatures), "°C (", length(temperatures), "values)\n")
    cat("P-value range:", min(p_values), "-", max(p_values), "(", length(p_values), "values)\n")
    cat("Total combinations:", length(temperatures) * length(p_values), "\n\n")
  }
  
  # Choose execution method
  if (parallel) {
    # Future-based parallel execution (cross-platform)
    results_list <- future_map(temperatures, ~process_temperature(.x, p_values, processed_data, simulation_days, oxycal, verbose))
  } else {
    # Sequential execution
    results_list <- map(temperatures, ~process_temperature(.x, p_values, processed_data, simulation_days, oxycal, verbose))
  }

  # Combine all results
  results <- do.call(rbind, results_list)
    
  # Add metadata as attributes
  attr(results, "analysis_info") <- list(
    temperature_range = range(temperatures),
    p_value_range = range(p_values),
    simulation_days = simulation_days,
    total_combinations = length(temperatures) * length(p_values),
    success_rate = sum(results$converged, na.rm = TRUE) / (length(temperatures) * length(p_values)),
    parallel_mode = parallel,
    n_cores_used = if (parallel) min(n_cores, length(temperatures)) else 1,
    analysis_date = Sys.time()
  )
  
  # Summary statistics
  if (verbose) {
    successful_runs <- sum(results$converged, na.rm = TRUE)
    total_combinations <- length(temperatures) * length(p_values)
    
    cat("\n=== ANALYSIS COMPLETE ===\n")
    cat("Successful simulations:", successful_runs, "/", total_combinations, 
        "(", round(successful_runs/total_combinations*100, 1), "%)\n")
    
    if (successful_runs > 0) {
      valid_results <- results[results$converged & !is.na(results$specific_growth_rate), ]
      
      if (nrow(valid_results) > 0) {
        max_growth_idx <- which.max(valid_results$specific_growth_rate)
        optimal <- valid_results[max_growth_idx, ]
        
        cat("Maximum growth rate:", sprintf("%.3f", optimal$specific_growth_rate), "%/d\n")
        cat("Optimal conditions:", optimal$temperature, "°C,", 
            optimal$feeding_pct, "% feeding (p =", sprintf("%.2f", optimal$p_value), ")\n")
      }
    }
  }

  return(results)

}

#' Process single temperature with all p-values
#' @param temp Temperature value
#' @param p_values Vector of p-values to test
#' @param processed_data Prepared simulation data
#' @param simulation_days Number of simulation days
#' @param oxycal Oxycalorific coefficient
#' @param verbose Show progress
#' @return Data frame with results for this temperature
#' @keywords internal
process_temperature <- function(temp, p_values, processed_data, simulation_days, oxycal, verbose) {
  
  if (verbose) {
    cat("Processing temperature:", temp, "°C\n")
  }
  
  # Create constant temperature data
  temp_data <- data.frame(
    Day = 1:simulation_days, 
    Temperature = rep(temp, simulation_days)
  )
  
  # Update temperature in processed data
  processed_data_temp <- processed_data
  processed_data_temp$temporal_data$temperature <- temp_data$Temperature
  
  # Initialize results for this temperature
  temp_results <- data.frame()
  
  # Loop through p_values for this temperature
  for (p_val in p_values) {
    
    # Run simulation with current p-value
    tryCatch({
      
      # Execute bioenergetic simulation
      sim_result <- run_fb4_simulation(
        consumption_method = list(type = "p_value", value = p_val),
        processed_simulation_data = processed_data_temp,
        oxycal = oxycal,
        output_daily = FALSE,
        verbose = FALSE
      )
      
      # Extract basic metrics
      initial_weight <- sim_result$initial_weight
      final_weight <- sim_result$final_weight
      total_consumption <- sim_result$total_consumption_g %||% NA
      
      # Calculate growth metrics
      total_growth <- final_weight - initial_weight
      relative_growth <- (final_weight / initial_weight - 1) * 100
      daily_growth_rate <- (final_weight / initial_weight)^(1/simulation_days) - 1
      specific_growth_rate <- log(final_weight / initial_weight) / simulation_days * 100
      
      # Calculate efficiency metrics
      gross_efficiency <- if (!is.na(total_consumption) && total_consumption > 0) {
        total_growth / total_consumption
      } else {
        NA
      }
      
      # Store successful result
      temp_results <- rbind(temp_results, data.frame(
        temperature = temp,
        p_value = p_val,
        feeding_pct = round(p_val * 100, 1),
        initial_weight = initial_weight,
        final_weight = final_weight,
        total_growth = total_growth,
        relative_growth = relative_growth,
        daily_growth_rate = daily_growth_rate,
        specific_growth_rate = specific_growth_rate,
        total_consumption = total_consumption,
        gross_efficiency = gross_efficiency,
        converged = sim_result$converged %||% TRUE,
        stringsAsFactors = FALSE
      ))
      
    }, error = function(e) {
      
      # Store failed result
      temp_results <<- rbind(temp_results, data.frame(
        temperature = temp,
        p_value = p_val,
        feeding_pct = round(p_val * 100, 1),
        initial_weight = processed_data$simulation_settings$initial_weight,
        final_weight = NA,
        total_growth = NA,
        relative_growth = NA,
        daily_growth_rate = NA,
        specific_growth_rate = NA,
        total_consumption = NA,
        gross_efficiency = NA,
        converged = FALSE,
        stringsAsFactors = FALSE
      ))
    })
  }
  
  return(temp_results)
}
   

# ============================================================================
# SCENARIO COMPARISON FUNCTIONS
# ============================================================================

#' Compare multiple FB4 results
#'
#' @description
#' Compares multiple FB4 simulation results across different scenarios,
#' parameters, or methods. Useful for comparing alternative models or
#' experimental conditions.
#'
#' @param result_list Named list of FB4 result objects
#' @param metrics Vector of metrics to compare
#' @param confidence_level Confidence level for comparisons
#' @return List with comparative analysis
#' @export
#'
#' @examples
#' \dontrun{
#' # Compare different fitting methods
#' results <- list(
#'   mle = mle_result,
#'   hierarchical = hierarchical_result,
#'   binary_search = binary_result
#' )
#' comparison <- compare_scenarios(results)
#' }
compare_scenarios <- function(result_list, 
                             metrics = c("consumption", "growth", "efficiency"),
                             confidence_level = 0.95) {
  
  if (!is.list(result_list) || length(result_list) == 0) {
    stop("result_list must be a non-empty list of FB4 result objects")
  }
  
  # Validate all inputs are FB4 results
  valid_results <- sapply(result_list, is.fb4_result)
  if (!all(valid_results)) {
    invalid_names <- names(result_list)[!valid_results]
    stop("Invalid FB4 result objects: ", paste(invalid_names, collapse = ", "))
  }
  
  scenario_names <- names(result_list)
  if (is.null(scenario_names)) {
    scenario_names <- paste0("Scenario_", 1:length(result_list))
    names(result_list) <- scenario_names
  }
  
  comparison <- list(
    n_scenarios = length(result_list),
    scenario_names = scenario_names,
    metrics_compared = metrics,
    confidence_level = confidence_level
  )
  
  # Extract comparable metrics from each result
  scenario_data <- data.frame(
    scenario = scenario_names,
    method = sapply(result_list, function(r) r$summary$method),
    backend = sapply(result_list, function(r) r$model_info$backend),
    converged = sapply(result_list, function(r) r$summary$converged %||% TRUE),
    stringsAsFactors = FALSE
  )
  
  # Add consumption metrics
  if ("consumption" %in% metrics) {
    consumption_data <- lapply(result_list, function(r) {
      get_consumption_uncertainty(r, confidence_level = confidence_level)
    })
    
    scenario_data$consumption_est <- sapply(consumption_data, function(x) x$estimate %||% NA)
    scenario_data$consumption_se <- sapply(consumption_data, function(x) x$se %||% NA)
    scenario_data$consumption_has_uncertainty <- sapply(consumption_data, function(x) x$has_uncertainty)
  }
  
  # Add growth metrics
  if ("growth" %in% metrics) {
    growth_data <- lapply(result_list, function(r) {
      analyze_growth_patterns(r, confidence_level = confidence_level)
    })
    
    scenario_data$initial_weight <- sapply(growth_data, function(x) x$initial_weight %||% NA)
    scenario_data$final_weight_est <- sapply(growth_data, function(x) x$final_weight$estimate %||% NA)
    scenario_data$final_weight_se <- sapply(growth_data, function(x) x$final_weight$se %||% NA)
    scenario_data$relative_growth_est <- sapply(growth_data, function(x) x$relative_growth$estimate %||% NA)
    scenario_data$relative_growth_se <- sapply(growth_data, function(x) x$relative_growth$se %||% NA)
  }
  
  # Add efficiency metrics
  if ("efficiency" %in% metrics) {
    efficiency_data <- lapply(result_list, function(r) {
      get_efficiency_uncertainty(r, confidence_level = confidence_level)
    })
    
    scenario_data$gross_efficiency_est <- sapply(efficiency_data, function(x) x$gross_growth_efficiency$estimate %||% NA)
    scenario_data$gross_efficiency_se <- sapply(efficiency_data, function(x) x$gross_growth_efficiency$se %||% NA)
    scenario_data$metabolic_scope_est <- sapply(efficiency_data, function(x) x$metabolic_scope$estimate %||% NA)
    scenario_data$metabolic_scope_se <- sapply(efficiency_data, function(x) x$metabolic_scope$se %||% NA)
  }

  # Add p_value metrics
  if ("p_value" %in% metrics || "p" %in% metrics) {
    p_value_data <- lapply(result_list, function(r) {
      result_type <- detect_result_type(r)
      
      if (result_type$method == "hierarchical") {
        # Population mean p_value
        pop_results <- get_population_results(r, confidence_level = confidence_level)
        list(
          estimate = pop_results$mu_p_estimate %||% NA,
          se = pop_results$mu_p_se %||% NA,
          has_uncertainty = TRUE
        )
      } else if (result_type$method %in% c("mle", "binary_search", "optim", "bootstrap")) {
        # Single p_value estimate  
        list(
          estimate = r$summary$p_estimate %||% r$summary$p_value %||% r$summary$p_mean %||% NA,
          se = r$method_data$sigma_se %||% r$summary$p_sd %||% NA,
          has_uncertainty = !is.na(r$method_data$sigma_se %||% NA)
        )
      } else {
        list(estimate = NA, se = NA, has_uncertainty = FALSE)
      }
    })
    
    scenario_data$p_value_est <- sapply(p_value_data, function(x) x$estimate %||% NA)
    scenario_data$p_value_se <- sapply(p_value_data, function(x) x$se %||% NA)
    scenario_data$p_value_has_uncertainty <- sapply(p_value_data, function(x) x$has_uncertainty)
  }
  
  comparison$scenario_data <- scenario_data
  
  # Statistical comparisons (if multiple scenarios with uncertainty)
  scenarios_with_uncertainty <- sum(scenario_data$consumption_has_uncertainty %||% FALSE) +
                               sum(!is.na(scenario_data$final_weight_se %||% rep(NA, nrow(scenario_data))))
  
  if (scenarios_with_uncertainty >= 2) {
    comparison$statistical_tests <- perform_scenario_tests(scenario_data, metrics)
  }
  
  # Best performing scenario by metric
  comparison$best_performers <- identify_best_scenarios(scenario_data, metrics)
  
  return(comparison)
}

#' Perform statistical tests between scenarios
#'
#' @description
#' Performs statistical comparisons between scenarios when uncertainty
#' estimates are available.
#'
#' @param scenario_data Data frame with scenario results
#' @param metrics Vector of metrics to test
#' @return List with test results
#' @keywords internal
perform_scenario_tests <- function(scenario_data, metrics) {
  
  tests <- list()
  
  # Simple pairwise comparisons using overlapping confidence intervals
  # (This is a conservative approach)
  
  if ("consumption" %in% metrics && sum(!is.na(scenario_data$consumption_se)) >= 2) {
    tests$consumption <- test_metric_differences(
      scenario_data$consumption_est, 
      scenario_data$consumption_se,
      scenario_data$scenario
    )
  }
  
  if ("growth" %in% metrics && sum(!is.na(scenario_data$relative_growth_se)) >= 2) {
    tests$relative_growth <- test_metric_differences(
      scenario_data$relative_growth_est,
      scenario_data$relative_growth_se,
      scenario_data$scenario
    )
  }
  
  if ("efficiency" %in% metrics && sum(!is.na(scenario_data$gross_efficiency_se)) >= 2) {
    tests$gross_efficiency <- test_metric_differences(
      scenario_data$gross_efficiency_est,
      scenario_data$gross_efficiency_se,
      scenario_data$scenario
    )
  }
  
  return(tests)
}

#' Test differences between metric estimates
#'
#' @description
#' Tests for significant differences between metric estimates using
#' confidence interval overlap method.
#'
#' @param estimates Vector of estimates
#' @param se_values Vector of standard errors
#' @param scenario_names Vector of scenario names
#' @return List with test results
#' @keywords internal
test_metric_differences <- function(estimates, se_values, scenario_names) {
  
  valid_indices <- !is.na(estimates) & !is.na(se_values)
  
  if (sum(valid_indices) < 2) {
    return(list(test = "insufficient_data"))
  }
  
  valid_est <- estimates[valid_indices]
  valid_se <- se_values[valid_indices]
  valid_names <- scenario_names[valid_indices]
  
  n_valid <- length(valid_est)
  
  # Calculate 95% confidence intervals
  z_score <- qnorm(0.975)
  ci_lower <- valid_est - z_score * valid_se
  ci_upper <- valid_est + z_score * valid_se
  
  # Pairwise comparisons
  comparisons <- expand.grid(i = 1:n_valid, j = 1:n_valid)
  comparisons <- comparisons[comparisons$i < comparisons$j, ]
  
  if (nrow(comparisons) > 0) {
    comparison_results <- data.frame(
      scenario_1 = valid_names[comparisons$i],
      scenario_2 = valid_names[comparisons$j],
      estimate_1 = valid_est[comparisons$i],
      estimate_2 = valid_est[comparisons$j],
      difference = valid_est[comparisons$i] - valid_est[comparisons$j],
      ci_overlap = mapply(function(i, j) {
        # Check if confidence intervals overlap
        max(ci_lower[i], ci_lower[j]) <= min(ci_upper[i], ci_upper[j])
      }, comparisons$i, comparisons$j),
      significant_difference = !mapply(function(i, j) {
        max(ci_lower[i], ci_lower[j]) <= min(ci_upper[i], ci_upper[j])
      }, comparisons$i, comparisons$j),
      stringsAsFactors = FALSE
    )
    
    return(list(
      test = "ci_overlap",
      comparisons = comparison_results,
      n_significant = sum(comparison_results$significant_difference)
    ))
  } else {
    return(list(test = "single_scenario"))
  }
}

#' Identify best performing scenarios
#'
#' @description
#' Identifies the best performing scenario for each metric.
#'
#' @param scenario_data Data frame with scenario results
#' @param metrics Vector of metrics to evaluate
#' @return List with best performers
#' @keywords internal
identify_best_scenarios <- function(scenario_data, metrics) {
  
  best_performers <- list()
  
  if ("consumption" %in% metrics && any(!is.na(scenario_data$consumption_est))) {
    best_idx <- which.max(scenario_data$consumption_est)
    best_performers$consumption <- list(
      scenario = scenario_data$scenario[best_idx],
      value = scenario_data$consumption_est[best_idx]
    )
  }
  
  if ("growth" %in% metrics && any(!is.na(scenario_data$relative_growth_est))) {
    best_idx <- which.max(scenario_data$relative_growth_est)
    best_performers$relative_growth <- list(
      scenario = scenario_data$scenario[best_idx],
      value = scenario_data$relative_growth_est[best_idx]
    )
  }
  
  if ("efficiency" %in% metrics && any(!is.na(scenario_data$gross_efficiency_est))) {
    best_idx <- which.max(scenario_data$gross_efficiency_est)
    best_performers$gross_efficiency <- list(
      scenario = scenario_data$scenario[best_idx],
      value = scenario_data$gross_efficiency_est[best_idx]
    )
  }
  
  return(best_performers)
}