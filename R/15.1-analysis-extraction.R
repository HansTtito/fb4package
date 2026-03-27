#' Basic Analysis and Extraction Functions for FB4 Results
#'
#' @description
#' Functions for basic analysis and extraction of FB4 simulation results.
#' These functions build on the core extraction functions to provide
#' meaningful biological interpretations and statistical summaries.
#'
#' @name analysis-extraction
#' @aliases analysis-extraction
NULL

# ============================================================================
# GROWTH ANALYSIS FUNCTIONS
# ============================================================================

#' Analyze growth patterns from FB4 results
#'
#' @description
#' Extracts and analyzes growth patterns from FB4 simulation results.
#' Calculates growth rates, efficiency metrics, and provides uncertainty
#' estimates when available.
#'
#' @param result FB4 result object
#' @param individual_id Individual ID for hierarchical models (NULL for population/single individual)
#' @param confidence_level Confidence level for intervals (default 0.95)
#' @return List with comprehensive growth analysis
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic growth analysis
#' growth <- analyze_growth_patterns(result)
#' 
#' # For specific individual in hierarchical model
#' growth_ind1 <- analyze_growth_patterns(result, individual_id = 1)
#' }
analyze_growth_patterns <- function(result, individual_id = NULL, confidence_level = 0.95) {
  
  if (!is.fb4_result(result)) {
    stop("Input must be an fb4_result object")
  }
  
  result_type <- detect_result_type(result)
  method <- result_type$method
  has_uncertainty <- result_type$has_uncertainty
  
  # Get basic growth information
  growth_info <- list(
    method = method,
    has_uncertainty = has_uncertainty,
    individual_id = individual_id
  )
  
  # Extract growth metrics based on method
  if (method == "hierarchical") {
    
    if (is.null(individual_id)) {
      # Population mean
      pop_results <- get_population_results(result, confidence_level)
      
      growth_info$initial_weight <- result$summary$initial_weight %||% NA
      growth_info$final_weight <- list(
        estimate = pop_results$mean_final_weight_est %||% NA,
        se = pop_results$mean_final_weight_se %||% NA,
        ci_lower = pop_results$mean_final_weight_ci_lower %||% NA,
        ci_upper = pop_results$mean_final_weight_ci_upper %||% NA
      )
      
      growth_info$total_growth <- list(
        estimate = pop_results$mean_total_growth_est %||% NA,
        se = pop_results$mean_total_growth_se %||% NA,
        ci_lower = pop_results$mean_total_growth_ci_lower %||% NA,
        ci_upper = pop_results$mean_total_growth_ci_upper %||% NA
      )
      
      growth_info$relative_growth <- list(
        estimate = pop_results$mean_relative_growth_est %||% NA,
        se = pop_results$mean_relative_growth_se %||% NA,
        ci_lower = pop_results$mean_relative_growth_ci_lower %||% NA,
        ci_upper = pop_results$mean_relative_growth_ci_upper %||% NA
      )
      
      growth_info$n_individuals <- pop_results$n_individuals
      
    } else {
      # Individual result
      ind_results <- get_individual_results(result, confidence_level)
      
      if (individual_id > nrow(ind_results)) {
        stop("Individual ID ", individual_id, " not found. Available: 1-", nrow(ind_results))
      }
      
      ind_data <- ind_results[individual_id, ]
      
      growth_info$initial_weight <- result$summary$initial_weight %||% NA  # Same for all individuals
      growth_info$final_weight <- list(
        estimate = ind_data$final_weight_est %||% NA,
        se = ind_data$final_weight_se %||% NA,
        ci_lower = ind_data$final_weight_ci_lower %||% NA,
        ci_upper = ind_data$final_weight_ci_upper %||% NA
      )
      
      growth_info$total_growth <- list(
        estimate = ind_data$total_growth_est %||% NA,
        se = ind_data$total_growth_se %||% NA,
        ci_lower = ind_data$total_growth_ci_lower %||% NA,
        ci_upper = ind_data$total_growth_ci_upper %||% NA
      )
      
      growth_info$relative_growth <- list(
        estimate = ind_data$relative_growth_est %||% NA,
        se = ind_data$relative_growth_se %||% NA,
        ci_lower = ind_data$relative_growth_ci_lower %||% NA,
        ci_upper = ind_data$relative_growth_ci_upper %||% NA
      )
      
      growth_info$p_value <- list(
        estimate = ind_data$p_estimate %||% NA,
        se = ind_data$p_se %||% NA
      )
    }
    
  } else {
    # Non-hierarchical methods
    growth_info$initial_weight <- result$summary$initial_weight %||% NA
    
    if (method == "mle" && result$model_info$backend == "tmb") {
      # MLE with TMB - has uncertainty
      tmb_unc <- result$method_data$tmb_uncertainty
      
      growth_info$final_weight <- list(
        estimate = tmb_unc$final_weight_est %||% result$summary$predicted_weight %||% NA,
        se = tmb_unc$final_weight_se %||% NA,
        ci_lower = NA,
        ci_upper = NA
      )
      
      growth_info$total_growth <- list(
        estimate = tmb_unc$total_growth_est %||% NA,
        se = tmb_unc$total_growth_se %||% NA,
        ci_lower = NA,
        ci_upper = NA
      )
      
      growth_info$relative_growth <- list(
        estimate = tmb_unc$relative_growth_est %||% NA,
        se = tmb_unc$relative_growth_se %||% NA,
        ci_lower = NA,
        ci_upper = NA
      )
      
      # Calculate CIs if we have estimates and SEs
      z <- z_score(confidence_level)
      for (metric in c("final_weight", "total_growth", "relative_growth")) {
        est <- growth_info[[metric]]$estimate
        se <- growth_info[[metric]]$se
        if (!is.na(est) && !is.na(se)) {
          growth_info[[metric]]$ci_lower <- est - z * se
          growth_info[[metric]]$ci_upper <- est + z * se
        }
      }
      
    } else {
      # Traditional methods or MLE without full uncertainty
      final_weight <- result$summary$final_weight %||% result$summary$predicted_weight %||% NA
      
      growth_info$final_weight <- list(
        estimate = final_weight,
        se = NA,
        ci_lower = NA,
        ci_upper = NA
      )
      
      if (!is.na(final_weight) && !is.na(growth_info$initial_weight)) {
        total_growth <- final_weight - growth_info$initial_weight
        relative_growth <- (final_weight / growth_info$initial_weight - 1) * 100
        
        growth_info$total_growth <- list(
          estimate = total_growth,
          se = NA,
          ci_lower = NA,
          ci_upper = NA
        )
        
        growth_info$relative_growth <- list(
          estimate = relative_growth,
          se = NA,
          ci_lower = NA,
          ci_upper = NA
        )
      }
    }
    
    # Add p_value information for fitted methods
    if (method %in% c("mle", "binary_search", "optim")) {
      growth_info$p_value <- list(
        estimate = result$summary$p_estimate %||% result$summary$p_value %||% NA,
        se = result$method_data$sigma_se %||% NA
      )
    }
  }
  
  # Calculate derived metrics
  duration <- result$summary$simulation_days %||% NA
  if (!is.na(duration) && !is.na(growth_info$total_growth$estimate)) {
    # Daily growth rate (absolute)
    growth_info$daily_growth_rate <- list(
      estimate = growth_info$total_growth$estimate / duration,
      se = if (!is.na(growth_info$total_growth$se)) growth_info$total_growth$se / duration else NA,
      ci_lower = if (!is.na(growth_info$total_growth$ci_lower)) growth_info$total_growth$ci_lower / duration else NA,
      ci_upper = if (!is.na(growth_info$total_growth$ci_upper)) growth_info$total_growth$ci_upper / duration else NA
    )
    
    # Specific growth rate (% per day)
    if (!is.na(growth_info$initial_weight) && !is.na(growth_info$final_weight$estimate)) {
      sgr <- log(growth_info$final_weight$estimate / growth_info$initial_weight) / duration * 100
      growth_info$specific_growth_rate <- list(
        estimate = sgr,
        se = NA,  # Would need delta method for proper SE
        ci_lower = NA,
        ci_upper = NA
      )
    }
  }
  
  return(growth_info)
}

# ============================================================================
# ENERGY BUDGET ANALYSIS FUNCTIONS
# ============================================================================

#' Analyze energy budget from FB4 results
#'
#' @description
#' Analyzes energy budget components from FB4 simulation results.
#' Calculates proportional allocation to different processes with
#' uncertainty propagation when available.
#'
#' @param result FB4 result object
#' @param individual_id Individual ID for hierarchical models (NULL for population/single individual)
#' @param confidence_level Confidence level for intervals (default 0.95)
#' @return List with comprehensive energy budget analysis
#' @export
analyze_energy_budget <- function(result, individual_id = NULL, confidence_level = 0.95) {
  
  if (!is.fb4_result(result)) {
    stop("Input must be an fb4_result object")
  }
  
  result_type <- detect_result_type(result)
  method <- result_type$method
  has_uncertainty <- result_type$has_uncertainty
  
  # Get energy budget components
  budget <- get_energy_budget_uncertainty(result, individual_id, confidence_level)
  
  # Initialize analysis result
  budget_analysis <- list(
    method = method,
    has_uncertainty = has_uncertainty,
    individual_id = individual_id,
    energy_components = budget
  )
  
  # Calculate energy proportions if consumption energy is available
  consumption_est <- budget$consumption_energy$estimate
  
  if (!is.na(consumption_est) && consumption_est > 0) {
    
    # Calculate proportions
    components <- c("respiration_energy", "egestion_energy", "excretion_energy", "sda_energy", "net_energy")
    
    budget_analysis$proportions <- list()
    
    for (component in components) {
      comp_est <- budget[[component]]$estimate
      comp_se <- budget[[component]]$se
      
      if (!is.na(comp_est)) {
        prop_est <- comp_est / consumption_est
        
        # Approximate SE for proportion using delta method
        prop_se <- NA
        if (!is.na(comp_se) && !is.na(budget$consumption_energy$se)) {
          # Delta method for ratio: Var(Y/X) ≈ (Y/X)² * [Var(Y)/Y² + Var(X)/X² - 2*Cov(X,Y)/(X*Y)]
          # Assuming independence: Cov(X,Y) = 0
          cv_comp <- comp_se / comp_est
          cv_cons <- budget$consumption_energy$se / consumption_est
          prop_se <- prop_est * sqrt(cv_comp^2 + cv_cons^2)
        }
        
        prop_name <- gsub("_energy", "", component)
        budget_analysis$proportions[[paste0("prop_", prop_name)]] <- list(
          estimate = prop_est,
          se = prop_se,
          ci_lower = if (!is.na(prop_se)) prop_est - z_score(confidence_level) * prop_se else NA,
          ci_upper = if (!is.na(prop_se)) prop_est + z_score(confidence_level) * prop_se else NA
        )
      }
    }
    
    # Calculate summary metrics
    prop_respiration <- budget_analysis$proportions$prop_respiration$estimate %||% 0
    prop_sda <- budget_analysis$proportions$prop_sda$estimate %||% 0
    prop_net <- budget_analysis$proportions$prop_net$estimate %||% 0
    
    budget_analysis$summary_metrics <- list(
      gross_growth_efficiency = list(
        estimate = prop_net,
        se = budget_analysis$proportions$prop_net$se %||% NA
      ),
      metabolic_scope = list(
        estimate = prop_respiration + prop_sda,
        se = NA  # Would need covariance for proper SE
      ),
      assimilation_efficiency = list(
        estimate = 1 - (budget_analysis$proportions$prop_egestion$estimate %||% 0),
        se = budget_analysis$proportions$prop_egestion$se %||% NA
      )
    )
    
    # Energy balance check
    total_allocated <- sum(sapply(components, function(x) budget[[x]]$estimate %||% 0))
    budget_analysis$balance_check <- list(
      consumption_energy = consumption_est,
      total_allocated = total_allocated,
      balance_error = abs(total_allocated - consumption_est),
      relative_error = abs(total_allocated - consumption_est) / consumption_est * 100
    )
    
  } else {
    budget_analysis$proportions <- NULL
    budget_analysis$summary_metrics <- NULL
    budget_analysis$balance_check <- list(
      consumption_energy = consumption_est,
      error = "Consumption energy not available or zero"
    )
  }
  
  return(budget_analysis)
}

# ============================================================================
# FEEDING ANALYSIS FUNCTIONS
# ============================================================================

#' Analyze feeding performance from FB4 results
#'
#' @description
#' Analyzes feeding-related metrics including consumption rates,
#' feeding efficiency, and p_value estimates with uncertainty.
#'
#' @param result FB4 result object
#' @param individual_id Individual ID for hierarchical models (NULL for population/single individual)
#' @param confidence_level Confidence level for intervals (default 0.95)
#' @return List with feeding analysis
#' @export
analyze_feeding_performance <- function(result, individual_id = NULL, confidence_level = 0.95) {
  
  if (!is.fb4_result(result)) {
    stop("Input must be an fb4_result object")
  }
  
  result_type <- detect_result_type(result)
  method <- result_type$method
  
  # Get consumption data
  consumption <- get_consumption_uncertainty(result, individual_id, confidence_level)
  
  # Initialize feeding analysis
  feeding_analysis <- list(
    method = method,
    has_uncertainty = consumption$has_uncertainty,
    individual_id = individual_id
  )
  
  # Basic consumption metrics
  feeding_analysis$total_consumption <- consumption
  
  # Calculate consumption rates
  initial_weight <- result$summary$initial_weight %||% NA
  simulation_days <- result$summary$simulation_days %||% NA
  
  if (!is.na(consumption$estimate) && !is.na(initial_weight) && !is.na(simulation_days)) {
    
    # Daily consumption rate (g/day)
    daily_consumption <- consumption$estimate / simulation_days
    daily_consumption_se <- if (!is.na(consumption$se)) consumption$se / simulation_days else NA
    
    feeding_analysis$daily_consumption <- list(
      estimate = daily_consumption,
      se = daily_consumption_se,
      ci_lower = if (!is.na(daily_consumption_se)) daily_consumption - z_score(confidence_level) * daily_consumption_se else NA,
      ci_upper = if (!is.na(daily_consumption_se)) daily_consumption + z_score(confidence_level) * daily_consumption_se else NA
    )
    
    # Specific consumption rate (g consumption / g fish / day)
    specific_consumption <- daily_consumption / initial_weight
    specific_consumption_se <- if (!is.na(daily_consumption_se)) daily_consumption_se / initial_weight else NA
    
    feeding_analysis$specific_consumption <- list(
      estimate = specific_consumption,
      se = specific_consumption_se,
      ci_lower = if (!is.na(specific_consumption_se)) specific_consumption - z_score(confidence_level) * specific_consumption_se else NA,
      ci_upper = if (!is.na(specific_consumption_se)) specific_consumption + z_score(confidence_level) * specific_consumption_se else NA
    )
  }
  
  # p_value analysis (feeding rate)
  if (method == "hierarchical") {
    if (is.null(individual_id)) {
      # Population parameters
      pop_results <- get_population_results(result, confidence_level)
      feeding_analysis$p_value <- list(
        population_mean = pop_results$mu_p_estimate %||% NA,
        population_se = pop_results$mu_p_se %||% NA,
        population_sd = pop_results$sigma_p_estimate %||% NA,
        n_individuals = pop_results$n_individuals
      )
    } else {
      # Individual p_value
      ind_results <- get_individual_results(result, confidence_level)
      if (individual_id <= nrow(ind_results)) {
        feeding_analysis$p_value <- list(
          estimate = ind_results$p_estimate[individual_id] %||% NA,
          se = ind_results$p_se[individual_id] %||% NA
        )
      }
    }
  } else if (method %in% c("mle", "binary_search", "optim")) {
    # Single p_value estimate
    feeding_analysis$p_value <- list(
      estimate = result$summary$p_estimate %||% result$summary$p_value %||% NA,
      se = result$method_data$sigma_se %||% NA,
      ci_lower = result$method_data$confidence_intervals$p_ci_lower %||% NA,
      ci_upper = result$method_data$confidence_intervals$p_ci_upper %||% NA
    )
  }
  
  # Feeding efficiency (if growth data available)
  growth <- analyze_growth_patterns(result, individual_id, confidence_level)
  
  if (!is.na(growth$total_growth$estimate) && !is.na(consumption$estimate)) {
    feeding_efficiency <- growth$total_growth$estimate / consumption$estimate
    
    # Approximate SE using delta method
    feeding_efficiency_se <- NA
    if (!is.na(growth$total_growth$se) && !is.na(consumption$se)) {
      cv_growth <- growth$total_growth$se / growth$total_growth$estimate
      cv_consumption <- consumption$se / consumption$estimate
      feeding_efficiency_se <- feeding_efficiency * sqrt(cv_growth^2 + cv_consumption^2)
    }
    
    feeding_analysis$feeding_efficiency <- list(
      estimate = feeding_efficiency,
      se = feeding_efficiency_se,
      ci_lower = if (!is.na(feeding_efficiency_se)) feeding_efficiency - z_score(confidence_level) * feeding_efficiency_se else NA,
      ci_upper = if (!is.na(feeding_efficiency_se)) feeding_efficiency + z_score(confidence_level) * feeding_efficiency_se else NA
    )
  }
  
  return(feeding_analysis)
}

# ============================================================================
# SUMMARY STATISTICS FUNCTIONS
# ============================================================================

#' Create comprehensive result summary
#'
#' @description
#' Creates a comprehensive summary of FB4 results including all key
#' metrics with uncertainty estimates when available.
#'
#' @param result FB4 result object
#' @param individual_id Individual ID for hierarchical models (NULL for population summary)
#' @param confidence_level Confidence level for intervals (default 0.95)
#' @return List with comprehensive summary
#' @export
create_result_summary <- function(result, individual_id = NULL, confidence_level = 0.95) {
  
  if (!is.fb4_result(result)) {
    stop("Input must be an fb4_result object")
  }
  
  # Get all major analyses
  growth <- analyze_growth_patterns(result, individual_id, confidence_level)
  feeding <- analyze_feeding_performance(result, individual_id, confidence_level)
  budget <- analyze_energy_budget(result, individual_id, confidence_level)
  
  summary_result <- list(
    # Model information
    model_info = list(
      method = result$summary$method,
      backend = result$model_info$backend,
      has_uncertainty = growth$has_uncertainty,
      individual_id = individual_id,
      confidence_level = confidence_level
    ),
    
    # Growth metrics
    growth = list(
      initial_weight = growth$initial_weight,
      final_weight = growth$final_weight,
      total_growth = growth$total_growth,
      relative_growth = growth$relative_growth,
      daily_growth_rate = growth$daily_growth_rate,
      specific_growth_rate = growth$specific_growth_rate
    ),
    
    # Feeding metrics
    feeding = list(
      total_consumption = feeding$total_consumption,
      daily_consumption = feeding$daily_consumption,
      specific_consumption = feeding$specific_consumption,
      p_value = feeding$p_value,
      feeding_efficiency = feeding$feeding_efficiency
    ),
    
    # Energy budget
    energy_budget = list(
      components = budget$energy_components,
      proportions = budget$proportions,
      summary_metrics = budget$summary_metrics,
      balance_check = budget$balance_check
    ),
    
    # Model fit (for statistical methods)
    model_fit = if (result$summary$method %in% c("mle", "hierarchical")) {
      list(
        converged = result$summary$converged,
        log_likelihood = result$method_data$model_fit$log_likelihood %||% result$method_data$log_likelihood,
        aic = result$method_data$model_fit$aic %||% result$method_data$aic,
        bic = result$method_data$model_fit$bic %||% result$method_data$bic
      )
    } else {
      list(
        converged = result$summary$converged %||% TRUE
      )
    }
  )
  
  return(summary_result)
}