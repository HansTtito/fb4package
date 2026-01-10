#' Result Builders for FB4 Model (UNIFIED)
#'
#' @name result-builders-unified
#' @aliases result-builders-unified
NULL

# ============================================================================
# UNIFIED RESULT BUILDER SYSTEM
# ============================================================================

#' Build FB4 result object (UNIFIED APPROACH)
#'
#' @description
#' Unified result builder that creates a single fb4_result class with method-specific
#' data embedded. Eliminates the complexity of multiple result classes while 
#' maintaining all functionality.
#'
#' @param raw_results Raw results from strategy execution
#' @param execution_plan Original execution plan
#' @param elapsed_time Execution time in seconds
#'
#' @return fb4_result object with method-specific data
#' @keywords internal
build_fb4_result_unified <- function(raw_results, execution_plan, elapsed_time) {
  
  # Detect method from raw results or execution plan
  method <- detect_method(raw_results, execution_plan)
  
  result <- structure(list(
    # Core results (standardized across all methods)
    daily_output = raw_results$daily_output,
    summary = create_unified_summary(raw_results, execution_plan, method),
    
    # Method-specific data (contains all method-specific information)
    method_data = create_method_specific_data(raw_results, execution_plan, method),
    
    # Fitting information (standardized)
    fit_info = create_unified_fit_info(raw_results, execution_plan, method),
    
    # Metadata (standardized)
    model_info = list(
      version = "2.0.0",
      execution_time = elapsed_time,
      timestamp = Sys.time(),
      oxycal = execution_plan$oxycal %||% 13560,
      backend = execution_plan$backend %||% "r",
      method = method
    ),
    
    # Original object reference
    bioenergetic_object = execution_plan$bio_obj
    
  ), class = "fb4_result")
  
  # Update bio_obj status if successful
  if (should_mark_as_fitted(result)) {
    execution_plan$bio_obj$fitted <- TRUE
  }
  
  return(result)
}

# ============================================================================
# COMPONENT BUILDERS
# ============================================================================

#' Detect method from raw results or execution plan
#' @keywords internal
detect_method <- function(raw_results, execution_plan) {
  
  # Try to get method from various sources
  method <- raw_results$fit_info$method %||% 
           raw_results$strategy_info$strategy_type %||%
           execution_plan$strategy %||%
           "unknown"
  
  # Standardize method names
  method <- switch(method,
    "binary_search" = "binary_search",
    "optim" = "optim", 
    "optim_Brent" = "optim",
    "mle" = "mle",
    "bootstrap" = "bootstrap",
    "hierarchical" = "hierarchical",
    "direct_p_value" = "direct",
    "direct_ration_percent" = "direct",
    "direct_ration_grams" = "direct",
    method  # Keep original if not in switch
  )
  
  return(method)
}

#' Create unified summary section
#' @keywords internal
create_unified_summary <- function(raw_results, execution_plan, method) {
  
  # Common fields across all methods
  summary <- list(
    method = method,
    initial_weight = execution_plan$initial_weight %||% 
                    raw_results$observed_data$initial_weight %||%
                    NA,
    simulation_days = execution_plan$simulation_days %||% 
                     (execution_plan$last_day - execution_plan$first_day + 1) %||%
                     NA
  )
  
  # Method-specific summary fields
  if (method %in% c("binary_search", "optim", "direct")) {
    # Traditional methods
    summary$fit_to <- execution_plan$fit_to
    summary$fit_value <- execution_plan$fit_value
    summary$final_weight <- raw_results$final_weight
    summary$total_consumption_g <- raw_results$total_consumption_g
    summary$p_value <- raw_results$p_value %||% raw_results$effective_p_value
    summary$converged <- raw_results$converged %||% TRUE
    
  } else if (method == "mle") {
    # MLE method
    summary$fit_to <- "Weight"
    summary$predicted_weight <- raw_results$predicted_weight %||% raw_results$adreport_values$final_weight
    summary$total_consumption_g <- raw_results$total_consumption_g  %||% raw_results$total_consumption_g_est
    summary$p_estimate <- raw_results$p_estimate
    summary$p_se <- raw_results$p_se
    summary$sigma_estimate <- raw_results$sigma_estimate
    summary$converged <- raw_results$converged %||% FALSE
    
  } else if (method == "bootstrap") {
    # Bootstrap method
    summary$fit_to <- "Weight"
    summary$predicted_weight <- raw_results$predicted_weight
    summary$total_consumption_g <- raw_results$consumption_mean
    summary$p_mean <- raw_results$p_mean
    summary$p_sd <- raw_results$p_sd
    summary$consumption_mean <- raw_results$consumption_mean
    summary$consumption_sd <- raw_results$consumption_sd
    summary$converged <- TRUE  # Bootstrap always "converges"
    
  } else if (method == "hierarchical") {
    # Hierarchical method
    summary$fit_to <- "Weight"
    summary$n_individuals <- raw_results$n_individuals
    summary$mu_p_estimate <- raw_results$mu_p_estimate
    summary$sigma_p_estimate <- raw_results$sigma_p_estimate
    summary$converged <- raw_results$converged %||% FALSE
    
  } else {
    # Unknown method - use available fields
    summary$final_weight <- raw_results$final_weight %||% raw_results$predicted_weight
    summary$p_value <- raw_results$p_value %||% raw_results$p_estimate %||% raw_results$p_mean
    summary$converged <- raw_results$converged %||% FALSE
  }
  
  return(summary)
}

#' Create method-specific data section
#' @keywords internal
create_method_specific_data <- function(raw_results, execution_plan, method) {
  
  method_data <- list(method = method)
  
  if (method %in% c("mle", "bootstrap")) {
    # Statistical methods with observed weights
    method_data$observed_weights <- execution_plan$observed_weights
    method_data$n_observations <- length(execution_plan$observed_weights %||% c())
    
    if (!is.null(execution_plan$observed_weights)) {
      method_data$weight_stats <- list(
        mean = mean(execution_plan$observed_weights),
        sd = sd(execution_plan$observed_weights),
        min = min(execution_plan$observed_weights),
        max = max(execution_plan$observed_weights),
        range = range(execution_plan$observed_weights)
      )
    }
    
if (method == "mle") {
  # MLE-specific data
  method_data$confidence_intervals <- list(
    p_ci_lower = raw_results$p_ci_lower,
    p_ci_upper = raw_results$p_ci_upper
  )
  method_data$sigma_estimate <- raw_results$sigma_estimate
  method_data$sigma_se <- raw_results$sigma_se
  method_data$log_likelihood <- raw_results$log_likelihood
  method_data$aic <- raw_results$aic
  method_data$profile_likelihood <- raw_results$profile_likelihood
  method_data$confidence_level <- raw_results$confidence_level %||% 0.95
  
  # ADD: TMB uncertainty data for basic model
  if (execution_plan$backend == "tmb") {
    method_data$tmb_uncertainty <- list(
      # Core variables with uncertainty
      final_weight_est = raw_results$final_weight_est,
      final_weight_se = raw_results$final_weight_se,
      total_consumption_g_est = raw_results$total_consumption_g_est,
      total_consumption_g_se = raw_results$total_consumption_g_se,
      gross_growth_efficiency_est = raw_results$gross_growth_efficiency_est,
      gross_growth_efficiency_se = raw_results$gross_growth_efficiency_se,
      total_growth_est = raw_results$total_growth_est,
      total_growth_se = raw_results$total_growth_se,
      relative_growth_est = raw_results$relative_growth_est,
      relative_growth_se = raw_results$relative_growth_se,
      
      # Energy budget variables with uncertainty
      total_consumption_energy_est = raw_results$total_consumption_energy_est,
      total_consumption_energy_se = raw_results$total_consumption_energy_se,
      total_respiration_energy_est = raw_results$total_respiration_energy_est,
      total_respiration_energy_se = raw_results$total_respiration_energy_se,
      total_egestion_energy_est = raw_results$total_egestion_energy_est,
      total_egestion_energy_se = raw_results$total_egestion_energy_se,
      total_excretion_energy_est = raw_results$total_excretion_energy_est,
      total_excretion_energy_se = raw_results$total_excretion_energy_se,
      total_sda_energy_est = raw_results$total_sda_energy_est,
      total_sda_energy_se = raw_results$total_sda_energy_se,
      total_net_energy_est = raw_results$total_net_energy_est,
      total_net_energy_se = raw_results$total_net_energy_se,
      total_spawn_energy_est = raw_results$total_spawn_energy_est,
      total_spawn_energy_se = raw_results$total_spawn_energy_se,
      
      # Efficiency and consumption metrics with uncertainty
      mean_daily_consumption_est = raw_results$mean_daily_consumption_est,
      mean_daily_consumption_se = raw_results$mean_daily_consumption_se,
      mean_specific_consumption_est = raw_results$mean_specific_consumption_est,
      mean_specific_consumption_se = raw_results$mean_specific_consumption_se,
      specific_growth_rate_est = raw_results$specific_growth_rate_est,
      specific_growth_rate_se = raw_results$specific_growth_rate_se,
      metabolic_scope_est = raw_results$metabolic_scope_est,
      metabolic_scope_se = raw_results$metabolic_scope_se,
      
      # Energy budget proportions with uncertainty
      prop_respiration_est = raw_results$prop_respiration_est,
      prop_respiration_se = raw_results$prop_respiration_se,
      prop_egestion_est = raw_results$prop_egestion_est,
      prop_egestion_se = raw_results$prop_egestion_se,
      prop_excretion_est = raw_results$prop_excretion_est,
      prop_excretion_se = raw_results$prop_excretion_se,
      prop_sda_est = raw_results$prop_sda_est,
      prop_sda_se = raw_results$prop_sda_se,
      prop_growth_est = raw_results$prop_growth_est,
      prop_growth_se = raw_results$prop_growth_se,
      
      # Final energy density with uncertainty
      final_energy_density_est = raw_results$final_energy_density_est,
      final_energy_density_se = raw_results$final_energy_density_se
    )
  }   
} else if (method == "bootstrap") {
      # Bootstrap-specific data
      method_data$bootstrap_results <- list(
        p_values = raw_results$bootstrap_p_values,
        consumption_values = raw_results$bootstrap_consumption_values,
        predicted_weights = raw_results$bootstrap_predicted_weights
      )
      method_data$confidence_intervals <- list(
        p_ci_lower = raw_results$p_ci_lower,
        p_ci_upper = raw_results$p_ci_upper,
        consumption_ci_lower = raw_results$consumption_ci_lower,
        consumption_ci_upper = raw_results$consumption_ci_upper
      )
      method_data$bootstrap_info <- list(
        n_bootstrap = raw_results$n_bootstrap,
        successful_iterations = raw_results$successful_iterations,
        success_rate = raw_results$success_rate,
        parallel_used = raw_results$parallel_used %||% FALSE,
        n_cores_used = raw_results$n_cores_used
      )
      method_data$model_diagnostics <- raw_results$model_diagnostics
      method_data$percentiles <- raw_results$p_percentiles
    }
    
  } else if (method == "hierarchical") {
  # Hierarchical method data
  method_data$n_individuals <- raw_results$n_individuals
  method_data$individual_results <- list(
    p_estimates = raw_results$individual_p_estimates,
    p_se = raw_results$individual_p_se
  )
  method_data$population_results <- list(
    mu_p_estimate = raw_results$mu_p_estimate,
    mu_p_se = raw_results$mu_p_se,
    sigma_p_estimate = raw_results$sigma_p_estimate,
    sigma_p_se = raw_results$sigma_p_se,
    sigma_obs_estimate = raw_results$sigma_obs_estimate,
    sigma_obs_se = raw_results$sigma_obs_se
  )
  method_data$model_fit <- list(
    log_likelihood = raw_results$log_likelihood,
    aic = raw_results$aic,
    bic = raw_results$bic
  )
  method_data$confidence_level <- raw_results$confidence_level %||% 0.95
  
  # ADD: Individual-level uncertainty data
  method_data$individual_uncertainty <- list(
    final_weights_est = raw_results$individual_final_weights_est,
    final_weights_se = raw_results$individual_final_weights_se,
    total_consumption_est = raw_results$individual_total_consumption_est,
    total_consumption_se = raw_results$individual_total_consumption_se,
    total_growth_est = raw_results$individual_total_growth_est,
    total_growth_se = raw_results$individual_total_growth_se,
    relative_growth_est = raw_results$individual_relative_growth_est,
    relative_growth_se = raw_results$individual_relative_growth_se,
    gross_efficiency_est = raw_results$individual_gross_efficiency_est,
    gross_efficiency_se = raw_results$individual_gross_efficiency_se,
    metabolic_scope_est = raw_results$individual_metabolic_scope_est,
    metabolic_scope_se = raw_results$individual_metabolic_scope_se,
    final_energy_density_est = raw_results$individual_final_energy_density_est,
    final_energy_density_se = raw_results$individual_final_energy_density_se,
    respiration_energy_est = raw_results$individual_respiration_energy_est,
    respiration_energy_se = raw_results$individual_respiration_energy_se,
    egestion_energy_est = raw_results$individual_egestion_energy_est,
    egestion_energy_se = raw_results$individual_egestion_energy_se,
    excretion_energy_est = raw_results$individual_excretion_energy_est,
    excretion_energy_se = raw_results$individual_excretion_energy_se,
    sda_energy_est = raw_results$individual_sda_energy_est,
    sda_energy_se = raw_results$individual_sda_energy_se,
    net_energy_est = raw_results$individual_net_energy_est,
    net_energy_se = raw_results$individual_net_energy_se,
    spawn_energy_est = raw_results$individual_spawn_energy_est,
    spawn_energy_se = raw_results$individual_spawn_energy_se
  )
  
  # ADD: Population-level uncertainty data
  method_data$population_uncertainty <- list(
    mean_final_weight_est = raw_results$mean_final_weight_est,
    mean_final_weight_se = raw_results$mean_final_weight_se,
    mean_total_consumption_est = raw_results$mean_total_consumption_est,
    mean_total_consumption_se = raw_results$mean_total_consumption_se,
    mean_total_growth_est = raw_results$mean_total_growth_est,
    mean_total_growth_se = raw_results$mean_total_growth_se,
    mean_relative_growth_est = raw_results$mean_relative_growth_est,
    mean_relative_growth_se = raw_results$mean_relative_growth_se,
    mean_gross_efficiency_est = raw_results$mean_gross_efficiency_est,
    mean_gross_efficiency_se = raw_results$mean_gross_efficiency_se,
    mean_metabolic_scope_est = raw_results$mean_metabolic_scope_est,
    mean_metabolic_scope_se = raw_results$mean_metabolic_scope_se,
    mean_final_energy_density_est = raw_results$mean_final_energy_density_est,
    mean_final_energy_density_se = raw_results$mean_final_energy_density_se,
    mean_respiration_energy_est = raw_results$mean_respiration_energy_est,
    mean_respiration_energy_se = raw_results$mean_respiration_energy_se,
    mean_egestion_energy_est = raw_results$mean_egestion_energy_est,
    mean_egestion_energy_se = raw_results$mean_egestion_energy_se,
    mean_excretion_energy_est = raw_results$mean_excretion_energy_est,
    mean_excretion_energy_se = raw_results$mean_excretion_energy_se,
    mean_sda_energy_est = raw_results$mean_sda_energy_est,
    mean_sda_energy_se = raw_results$mean_sda_energy_se,
    mean_net_energy_est = raw_results$mean_net_energy_est,
    mean_net_energy_se = raw_results$mean_net_energy_se,
    mean_spawn_energy_est = raw_results$mean_spawn_energy_est,
    mean_spawn_energy_se = raw_results$mean_spawn_energy_se
  )
  }  else {
    # Traditional methods (binary_search, optim, direct)
    method_data$target_info <- list(
      fit_to = execution_plan$fit_to,
      fit_value = execution_plan$fit_value,
      target_achieved = check_target_achievement(raw_results, execution_plan)
    )
    
    if (method %in% c("binary_search", "optim")) {
      method_data$optimization_info <- list(
        iterations = raw_results$iterations,
        final_error = raw_results$final_error,
        tolerance = execution_plan$tolerance
      )
    }
  }
  
  return(method_data)
  }

#' Create unified fit info section
#' @keywords internal
create_unified_fit_info <- function(raw_results, execution_plan, method) {
  
  fit_info <- list(
    method = method,
    converged = raw_results$converged %||% (method %in% c("bootstrap", "direct")),
    backend = execution_plan$backend %||% "r"
  )
  
  # Add method-specific fit information
  if (method %in% c("binary_search", "optim")) {
    fit_info$iterations <- raw_results$iterations %||% 1
    fit_info$final_error <- raw_results$final_error %||% 0
    fit_info$tolerance <- execution_plan$tolerance
    
  } else if (method == "mle") {
    fit_info$approach <- "maximum_likelihood"
    fit_info$distribution <- "log_normal"
    
  } else if (method == "bootstrap") {
    fit_info$approach <- "bootstrap_estimation"
    fit_info$parallel_used <- raw_results$parallel_used %||% FALSE
    fit_info$execution_time <- raw_results$execution_time
    
  } else if (method == "hierarchical") {
    fit_info$approach <- "hierarchical_mixed_effects"
    fit_info$backend <- "tmb"  # Hierarchical always uses TMB
    
  } else if (method == "direct") {
    fit_info$approach <- "direct_execution"
  }
  
  return(fit_info)
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Check if target was achieved for traditional methods
#' @keywords internal
check_target_achievement <- function(raw_results, execution_plan) {
  
  if (is.null(execution_plan$fit_value) || is.null(raw_results$final_weight)) {
    return(NA)
  }
  
  target <- execution_plan$fit_value
  achieved <- raw_results$final_weight
  
  # Allow 1% tolerance for "achieved"
  tolerance <- max(0.1, target * 0.01)
  
  return(abs(achieved - target) <= tolerance)
}

#' Determine if bio_obj should be marked as fitted
#' @keywords internal
should_mark_as_fitted <- function(result) {
  
  # Check convergence based on method
  method <- result$model_info$method
  
  if (method == "bootstrap") {
    return(TRUE)  # Bootstrap always "succeeds"
  } else if (method == "direct") {
    return(TRUE)  # Direct execution always "succeeds"
  } else {
    return(result$fit_info$converged %||% FALSE)
  }
}

# ============================================================================
# RESULT VALIDATION
# ============================================================================

#' Validate unified result object structure
#'
#' @description
#' Validates that the unified result object has all required components
#' and follows the expected structure.
#'
#' @param result FB4 result object to validate
#' @return TRUE if valid, otherwise stops with error
#' @keywords internal
validate_fb4_result <- function(result) {
  
  # Basic structure validation
  required_components <- c("daily_output", "summary", "method_data", "fit_info", 
                          "model_info", "bioenergetic_object")
  
  missing_components <- setdiff(required_components, names(result))
  if (length(missing_components) > 0) {
    stop("Result object missing required components: ", paste(missing_components, collapse = ", "))
  }
  
  # Check that summary has essential fields
  if (is.null(result$summary$method)) {
    stop("Result summary must contain method field")
  }
  
  # Check that method_data is consistent with method
  if (result$summary$method != result$method_data$method) {
    warning("Method mismatch between summary and method_data")
  }
  
  return(TRUE)
}

# ============================================================================
# EXECUTION SUMMARY FOR VERBOSE OUTPUT
# ============================================================================

#' Create execution summary for verbose output (UNIFIED)
#'
#' @description
#' Creates a summary of the execution for logging purposes.
#' Works with the unified result structure.
#'
#' @param result Unified FB4 result object
#' @param execution_plan Original execution plan
#' @param elapsed_time Execution time in seconds
#'
#' @return Character vector with summary lines
#' @keywords internal
create_execution_summary <- function(result, execution_plan, elapsed_time) {
  
  method <- result$summary$method
  summary_lines <- c()
  summary_lines <- c(summary_lines, paste("Simulation completed in", round(elapsed_time, 2), "seconds"))
  
  if (method == "hierarchical") {
    # Hierarchical results
    pop_results <- result$method_data$population_results
    summary_lines <- c(summary_lines,
                       "Hierarchical estimation completed",
                       paste("Population mean p_value:", round(pop_results$mu_p_estimate, 4),
                             "±", round(pop_results$mu_p_se, 4)),
                       paste("Population SD p_value:", round(pop_results$sigma_p_estimate, 4),
                             "±", round(pop_results$sigma_p_se, 4)),
                       paste("Individuals:", result$summary$n_individuals),
                       paste("Model converged:", result$summary$converged)
    )
    
  } else if (method == "mle") {
    # MLE results
    ci <- result$method_data$confidence_intervals
    summary_lines <- c(summary_lines, 
                       paste("Estimated p_value:", round(result$summary$p_estimate, 4), 
                             "(95% CI:", round(ci$p_ci_lower, 4), "-", 
                             round(ci$p_ci_upper, 4), ")"),
                       paste("Predicted weight:", round(result$summary$predicted_weight, 2), "g"),
                       paste("MLE converged:", result$summary$converged)
    )
    
  } else if (method == "bootstrap") {
    # Bootstrap results
    ci <- result$method_data$confidence_intervals
    bootstrap_info <- result$method_data$bootstrap_info
    
    summary_lines <- c(summary_lines,
                       "Bootstrap estimation completed",
                       paste("Estimated p_value:", round(result$summary$p_mean, 4), 
                             "(95% CI:", round(ci$p_ci_lower, 4), "-", 
                             round(ci$p_ci_upper, 4), ")"),
                       paste("Estimated consumption:", round(result$summary$consumption_mean, 2), 
                             "±", round(result$summary$consumption_sd, 2), "g"),
                       paste("Bootstrap success rate:", round(bootstrap_info$success_rate * 100, 1), "%")
    )
    
    if (bootstrap_info$parallel_used) {
      summary_lines <- c(summary_lines,
                         paste("Parallel speedup achieved with", bootstrap_info$n_cores_used, "cores")
      )
    }
    
  } else {
    # Traditional results (binary_search, optim, direct)
    summary_lines <- c(summary_lines,
                       paste("Final weight:", round(result$summary$final_weight, 2), "g"),
                       paste("p_value:", round(result$summary$p_value, 4))
    )
    
    if (result$summary$converged) {
      summary_lines <- c(summary_lines, "Fitting successful")
    } else {
      summary_lines <- c(summary_lines, "Fitting failed - using best approximation")
    }
  }
  
  return(summary_lines)
}