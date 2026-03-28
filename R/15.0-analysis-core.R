#' Core Analysis Functions for FB4 Results
#'
#' @description
#' Core functions for extracting and accessing results from FB4 simulations.
#' These functions provide a unified interface to access results regardless
#' of the fitting method used (basic, optim, MLE, hierarchical).
#'
#' @name analysis-core
#' @aliases analysis-core
NULL

# ============================================================================
# CORE UTILITY FUNCTIONS
# ============================================================================

#' Test if Object is fb4_result
#'
#' @description
#' Tests whether an object inherits from the fb4_result class.
#'
#' @param x Object to test
#' @return Logical indicating whether object is of class fb4_result
#' @export
is.fb4_result <- function(x) {
  inherits(x, "fb4_result")
}

#' Detect method and backend from fb4_result
#'
#' @description
#' Internal function to detect the method and backend used in a simulation.
#'
#' @param result FB4 result object
#' @return List with method, backend, and has_uncertainty flags
#' @keywords internal
detect_result_type <- function(result) {
  
  if (!is.fb4_result(result)) {
    stop("Input must be an fb4_result object")
  }
  
  method <- result$summary$method %||% "unknown"
  backend <- result$model_info$backend %||% "r"
  
  # Determine if uncertainty is available
  has_uncertainty <- FALSE
  if (method == "hierarchical") {
    has_uncertainty <- TRUE
  } else if (method == "mle" && backend == "tmb") {
    has_uncertainty <- TRUE
  }
  
  return(list(
    method = method,
    backend = backend,
    has_uncertainty = has_uncertainty
  ))
}

# ============================================================================
# CONSUMPTION EXTRACTION FUNCTIONS
# ============================================================================

#' Get consumption results with uncertainty
#'
#' @description
#' Extracts consumption results from FB4 simulations with uncertainty 
#' propagation when available. Works with all fitting methods.
#'
#' @param result FB4 result object
#' @param individual_id Individual ID for hierarchical models (NULL for population mean)
#' @param confidence_level Confidence level for intervals (default 0.95)
#' @return List with consumption estimates, standard errors, and confidence intervals
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic consumption extraction
#' consumption <- get_consumption_uncertainty(result)
#' 
#' # For hierarchical models - specific individual
#' consumption_ind1 <- get_consumption_uncertainty(result, individual_id = 1)
#' 
#' # Population mean for hierarchical
#' consumption_pop <- get_consumption_uncertainty(result, individual_id = NULL)
#' }
get_consumption_uncertainty <- function(result, individual_id = NULL, confidence_level = 0.95) {
  
  result_type <- detect_result_type(result)
  method <- result_type$method
  backend <- result_type$backend
  has_uncertainty <- result_type$has_uncertainty
  
  # Initialize return structure
  consumption_result <- list(
    estimate = NA,
    se = NA,
    ci_lower = NA,
    ci_upper = NA,
    method = method,
    backend = backend,
    has_uncertainty = has_uncertainty,
    individual_id = individual_id
  )
  
  # Calculate z-score for confidence intervals
  z <- z_score(confidence_level)
  
  if (method == "hierarchical") {
    
    if (is.null(individual_id)) {
      # Population mean
      consumption_result$estimate <- result$method_data$population_uncertainty$mean_total_consumption_est %||% NA
      consumption_result$se <- result$method_data$population_uncertainty$mean_total_consumption_se %||% NA
    } else {
      # Individual result
      if (individual_id > 0 && individual_id <= length(result$method_data$individual_uncertainty$total_consumption_est %||% c())) {
        consumption_result$estimate <- result$method_data$individual_uncertainty$total_consumption_est[individual_id] %||% NA
        consumption_result$se <- result$method_data$individual_uncertainty$total_consumption_se[individual_id] %||% NA
      } else {
        warning("Invalid individual_id: ", individual_id)
        return(consumption_result)
      }
    }
    
  } else if (method == "mle" && backend == "tmb") {
    # MLE with TMB uncertainty
    consumption_result$estimate <- result$method_data$tmb_uncertainty$total_consumption_g_est %||% NA
    consumption_result$se <- result$method_data$tmb_uncertainty$total_consumption_g_se %||% NA
    
  } else if (method == "mle") {
    # Basic MLE without full uncertainty
    consumption_result$estimate <- result$summary$total_consumption_g %||% NA
    consumption_result$se <- NA
    
  } else {
    # Traditional methods (binary_search, optim, direct)
    consumption_result$estimate <- result$summary$total_consumption_g %||% 
                                   result$summary$final_weight %||% NA
    consumption_result$se <- NA
  }
  
  # Calculate confidence intervals if uncertainty is available
  if (!is.na(consumption_result$estimate) && !is.na(consumption_result$se)) {
    consumption_result$ci_lower <- consumption_result$estimate - z * consumption_result$se
    consumption_result$ci_upper <- consumption_result$estimate + z * consumption_result$se
  }
  
  return(consumption_result)
}

# ============================================================================
# EFFICIENCY EXTRACTION FUNCTIONS  
# ============================================================================

#' Get efficiency results with uncertainty
#'
#' @description
#' Extracts growth efficiency results from FB4 simulations with uncertainty
#' propagation when available.
#'
#' @param result FB4 result object
#' @param individual_id Individual ID for hierarchical models (NULL for population mean)
#' @param confidence_level Confidence level for intervals (default 0.95)
#' @return List with efficiency estimates, standard errors, and confidence intervals
#' @export
get_efficiency_uncertainty <- function(result, individual_id = NULL, confidence_level = 0.95) {
  
  result_type <- detect_result_type(result)
  method <- result_type$method
  backend <- result_type$backend
  has_uncertainty <- result_type$has_uncertainty
  
  # Initialize return structure
  efficiency_result <- list(
    gross_growth_efficiency = list(estimate = NA, se = NA, ci_lower = NA, ci_upper = NA),
    metabolic_scope = list(estimate = NA, se = NA, ci_lower = NA, ci_upper = NA),
    method = method,
    backend = backend,
    has_uncertainty = has_uncertainty,
    individual_id = individual_id
  )
  
  # Calculate z-score for confidence intervals
  z <- z_score(confidence_level)
  
  if (method == "hierarchical") {
    
    if (is.null(individual_id)) {
      # Population mean
      efficiency_result$gross_growth_efficiency$estimate <- result$method_data$population_uncertainty$mean_gross_efficiency_est %||% NA
      efficiency_result$gross_growth_efficiency$se <- result$method_data$population_uncertainty$mean_gross_efficiency_se %||% NA
      
      efficiency_result$metabolic_scope$estimate <- result$method_data$population_uncertainty$mean_metabolic_scope_est %||% NA
      efficiency_result$metabolic_scope$se <- result$method_data$population_uncertainty$mean_metabolic_scope_se %||% NA
    } else {
      # Individual result
      if (individual_id > 0 && individual_id <= length(result$method_data$individual_uncertainty$gross_efficiency_est %||% c())) {
        efficiency_result$gross_growth_efficiency$estimate <- result$method_data$individual_uncertainty$gross_efficiency_est[individual_id] %||% NA
        efficiency_result$gross_growth_efficiency$se <- result$method_data$individual_uncertainty$gross_efficiency_se[individual_id] %||% NA
        
        efficiency_result$metabolic_scope$estimate <- result$method_data$individual_uncertainty$metabolic_scope_est[individual_id] %||% NA
        efficiency_result$metabolic_scope$se <- result$method_data$individual_uncertainty$metabolic_scope_se[individual_id] %||% NA
      } else {
        warning("Invalid individual_id: ", individual_id)
        return(efficiency_result)
      }
    }
    
  } else if (method == "mle" && backend == "tmb") {
    # MLE with TMB uncertainty
    efficiency_result$gross_growth_efficiency$estimate <- result$method_data$tmb_uncertainty$gross_growth_efficiency_est %||% NA
    efficiency_result$gross_growth_efficiency$se <- result$method_data$tmb_uncertainty$gross_growth_efficiency_se %||% NA
    
    efficiency_result$metabolic_scope$estimate <- result$method_data$tmb_uncertainty$metabolic_scope_est %||% NA
    efficiency_result$metabolic_scope$se <- result$method_data$tmb_uncertainty$metabolic_scope_se %||% NA
    
  } else {
    # Traditional methods - no uncertainty available
    # Try to get basic efficiency from summary or calculate from available data
    efficiency_result$gross_growth_efficiency$estimate <- result$summary$gross_growth_efficiency %||% NA
    efficiency_result$metabolic_scope$estimate <- result$summary$metabolic_scope %||% NA
  }
  
  # Calculate confidence intervals for both metrics
  for (metric in c("gross_growth_efficiency", "metabolic_scope")) {
    estimate <- efficiency_result[[metric]]$estimate
    se <- efficiency_result[[metric]]$se
    
    if (!is.na(estimate) && !is.na(se)) {
      efficiency_result[[metric]]$ci_lower <- estimate - z * se
      efficiency_result[[metric]]$ci_upper <- estimate + z * se
    }
  }
  
  return(efficiency_result)
}

# ============================================================================
# INDIVIDUAL RESULTS EXTRACTION
# ============================================================================

#' Get individual results from hierarchical models
#'
#' @description
#' Extracts all individual-level results from hierarchical FB4 models.
#' Returns a comprehensive summary for each individual.
#'
#' @param result FB4 result object from hierarchical method
#' @param confidence_level Confidence level for intervals (default 0.95)
#' @return Data frame with individual results and uncertainties
#' @export
get_individual_results <- function(result, confidence_level = 0.95) {
  
  result_type <- detect_result_type(result)
  
  if (result_type$method != "hierarchical") {
    stop("Individual results are only available for hierarchical models")
  }
  
  n_individuals <- result$method_data$n_individuals
  z <- z_score(confidence_level)
  
  # Extract individual p_values
  p_estimates <- result$method_data$individual_results$p_estimates %||% rep(NA, n_individuals)
  p_se <- result$method_data$individual_results$p_se %||% rep(NA, n_individuals)
  
  # Create individual results data frame
  individual_df <- data.frame(
    individual_id = 1:n_individuals,
    p_estimate = p_estimates,
    p_se = p_se,
    stringsAsFactors = FALSE
  )
  
  # Add uncertainty data if available
  if (!is.null(result$method_data$individual_uncertainty)) {
    uncertainty_data <- result$method_data$individual_uncertainty
    
    # Final weights
    individual_df$final_weight_est <- uncertainty_data$final_weights_est %||% rep(NA, n_individuals)
    individual_df$final_weight_se <- uncertainty_data$final_weights_se %||% rep(NA, n_individuals)
    
    # Consumption
    individual_df$consumption_est <- uncertainty_data$total_consumption_est %||% rep(NA, n_individuals)
    individual_df$consumption_se <- uncertainty_data$total_consumption_se %||% rep(NA, n_individuals)
    
    # Growth
    individual_df$total_growth_est <- uncertainty_data$total_growth_est %||% rep(NA, n_individuals)
    individual_df$total_growth_se <- uncertainty_data$total_growth_se %||% rep(NA, n_individuals)
    
    individual_df$relative_growth_est <- uncertainty_data$relative_growth_est %||% rep(NA, n_individuals)
    individual_df$relative_growth_se <- uncertainty_data$relative_growth_se %||% rep(NA, n_individuals)
    
    # Efficiency
    individual_df$gross_efficiency_est <- uncertainty_data$gross_efficiency_est %||% rep(NA, n_individuals)
    individual_df$gross_efficiency_se <- uncertainty_data$gross_efficiency_se %||% rep(NA, n_individuals)
    
    individual_df$metabolic_scope_est <- uncertainty_data$metabolic_scope_est %||% rep(NA, n_individuals)
    individual_df$metabolic_scope_se <- uncertainty_data$metabolic_scope_se %||% rep(NA, n_individuals)
    
    # Calculate confidence intervals for key metrics
    metrics_with_ci <- c("final_weight", "consumption", "total_growth", "relative_growth", 
                        "gross_efficiency", "metabolic_scope")
    
    for (metric in metrics_with_ci) {
      est_col <- paste0(metric, "_est")
      se_col <- paste0(metric, "_se")
      
      if (est_col %in% names(individual_df) && se_col %in% names(individual_df)) {
        individual_df[[paste0(metric, "_ci_lower")]] <- individual_df[[est_col]] - z * individual_df[[se_col]]
        individual_df[[paste0(metric, "_ci_upper")]] <- individual_df[[est_col]] + z * individual_df[[se_col]]
      }
    }
  }
  
  return(individual_df)
}

# ============================================================================
# POPULATION RESULTS EXTRACTION
# ============================================================================

#' Get population results from hierarchical models
#'
#' @description
#' Extracts population-level results from hierarchical FB4 models.
#' Returns means, standard errors, and population parameters.
#'
#' @param result FB4 result object from hierarchical method
#' @param confidence_level Confidence level for intervals (default 0.95)
#' @return List with population results and uncertainties
#' @export
get_population_results <- function(result, confidence_level = 0.95) {
  
  result_type <- detect_result_type(result)
  
  if (result_type$method != "hierarchical") {
    stop("Population results are only available for hierarchical models")
  }
  
  z <- z_score(confidence_level)
  
  # Population parameters
  pop_params <- result$method_data$population_results
  
  population_results <- list(
    # Population parameters
    mu_p_estimate = pop_params$mu_p_estimate %||% NA,
    mu_p_se = pop_params$mu_p_se %||% NA,
    sigma_p_estimate = pop_params$sigma_p_estimate %||% NA,
    sigma_p_se = pop_params$sigma_p_se %||% NA,
    sigma_obs_estimate = pop_params$sigma_obs_estimate %||% NA,
    sigma_obs_se = pop_params$sigma_obs_se %||% NA,
    
    # Sample size
    n_individuals = result$method_data$n_individuals,
    
    # Model fit
    log_likelihood = result$method_data$model_fit$log_likelihood %||% NA,
    aic = result$method_data$model_fit$aic %||% NA,
    bic = result$method_data$model_fit$bic %||% NA
  )
  
  # Add population means with uncertainty if available
  if (!is.null(result$method_data$population_uncertainty)) {
    pop_uncertainty <- result$method_data$population_uncertainty
    
    # Final weights
    population_results$mean_final_weight_est <- pop_uncertainty$mean_final_weight_est %||% NA
    population_results$mean_final_weight_se <- pop_uncertainty$mean_final_weight_se %||% NA
    
    # Consumption
    population_results$mean_consumption_est <- pop_uncertainty$mean_total_consumption_est %||% NA
    population_results$mean_consumption_se <- pop_uncertainty$mean_total_consumption_se %||% NA
    
    # Growth
    population_results$mean_total_growth_est <- pop_uncertainty$mean_total_growth_est %||% NA
    population_results$mean_total_growth_se <- pop_uncertainty$mean_total_growth_se %||% NA
    
    population_results$mean_relative_growth_est <- pop_uncertainty$mean_relative_growth_est %||% NA
    population_results$mean_relative_growth_se <- pop_uncertainty$mean_relative_growth_se %||% NA
    
    # Efficiency
    population_results$mean_gross_efficiency_est <- pop_uncertainty$mean_gross_efficiency_est %||% NA
    population_results$mean_gross_efficiency_se <- pop_uncertainty$mean_gross_efficiency_se %||% NA
    
    population_results$mean_metabolic_scope_est <- pop_uncertainty$mean_metabolic_scope_est %||% NA
    population_results$mean_metabolic_scope_se <- pop_uncertainty$mean_metabolic_scope_se %||% NA
    
    # Calculate confidence intervals for population means
    pop_metrics <- c("mean_final_weight", "mean_consumption", "mean_total_growth", 
                    "mean_relative_growth", "mean_gross_efficiency", "mean_metabolic_scope")
    
    for (metric in pop_metrics) {
      est_name <- paste0(metric, "_est")
      se_name <- paste0(metric, "_se")
      
      if (!is.na(population_results[[est_name]]) && !is.na(population_results[[se_name]])) {
        population_results[[paste0(metric, "_ci_lower")]] <- population_results[[est_name]] - z * population_results[[se_name]]
        population_results[[paste0(metric, "_ci_upper")]] <- population_results[[est_name]] + z * population_results[[se_name]]
      }
    }
  }
  
  # Calculate confidence intervals for population parameters
  param_metrics <- c("mu_p", "sigma_p", "sigma_obs")
  for (param in param_metrics) {
    est_name <- paste0(param, "_estimate")
    se_name <- paste0(param, "_se")
    
    if (!is.na(population_results[[est_name]]) && !is.na(population_results[[se_name]])) {
      population_results[[paste0(param, "_ci_lower")]] <- population_results[[est_name]] - z * population_results[[se_name]]
      population_results[[paste0(param, "_ci_upper")]] <- population_results[[est_name]] + z * population_results[[se_name]]
    }
  }
  
  return(population_results)
}

# ============================================================================
# ENERGY BUDGET EXTRACTION
# ============================================================================

#' Fill energy budget components from a named source list
#'
#' @description
#' Internal helper that populates the \code{estimate} and \code{se} slots of
#' several energy components inside a budget result list.  Avoids repeating the
#' same assignment pattern across population, individual, and TMB branches of
#' \code{get_energy_budget_uncertainty()}.
#'
#' @param budget  Named list with one sub-list per energy component, each
#'   containing at least \code{estimate} and \code{se} slots.
#' @param src     Named list (or data frame column) that holds the raw
#'   \code{<prefix>_est} / \code{<prefix>_se} values.
#' @param mapping Named character vector mapping component names (keys) to
#'   source field prefixes (values), e.g.
#'   \code{c(consumption_energy = "mean_total_consumption", ...)}.
#' @param idx     Integer index used for vector extraction (individual branch).
#'   \code{NULL} for scalar extraction (population / TMB branches).
#'
#' @return The updated \code{budget} list.
#' @keywords internal
assign_energy_components <- function(budget, src, mapping, idx = NULL) {
  for (comp in names(mapping)) {
    prefix    <- mapping[[comp]]
    est_field <- paste0(prefix, "_est")
    se_field  <- paste0(prefix, "_se")
    if (!is.null(idx)) {
      budget[[comp]]$estimate <- src[[est_field]][idx] %||% NA
      budget[[comp]]$se       <- src[[se_field]][idx]  %||% NA
    } else {
      budget[[comp]]$estimate <- src[[est_field]] %||% NA
      budget[[comp]]$se       <- src[[se_field]]  %||% NA
    }
  }
  budget
}

#' Get energy budget components with uncertainty
#'
#' @description
#' Extracts energy budget components from FB4 simulations with uncertainty
#' propagation when available.
#'
#' @param result FB4 result object
#' @param individual_id Individual ID for hierarchical models (NULL for population mean)
#' @param confidence_level Confidence level for intervals (default 0.95)
#' @return List with energy budget components and uncertainties
#' @export
get_energy_budget_uncertainty <- function(result, individual_id = NULL, confidence_level = 0.95) {
  
  result_type <- detect_result_type(result)
  method <- result_type$method
  backend <- result_type$backend
  has_uncertainty <- result_type$has_uncertainty
  
  # Initialize return structure
  budget_result <- list(
    consumption_energy = list(estimate = NA, se = NA, ci_lower = NA, ci_upper = NA),
    respiration_energy = list(estimate = NA, se = NA, ci_lower = NA, ci_upper = NA),
    egestion_energy = list(estimate = NA, se = NA, ci_lower = NA, ci_upper = NA),
    excretion_energy = list(estimate = NA, se = NA, ci_lower = NA, ci_upper = NA),
    sda_energy = list(estimate = NA, se = NA, ci_lower = NA, ci_upper = NA),
    net_energy = list(estimate = NA, se = NA, ci_lower = NA, ci_upper = NA),
    method = method,
    backend = backend,
    has_uncertainty = has_uncertainty,
    individual_id = individual_id
  )
  
  # Calculate z-score for confidence intervals
  z <- z_score(confidence_level)

  if (method %in% c("binary_search", "direct", "bootstrap", "optim") &&
      !is.null(result$daily_output) && nrow(result$daily_output) > 0) {
    # For R-backend methods the daily simulation output contains all energy
    # flows per day.  Sum across days to get the seasonal energy budget.
    do <- result$daily_output

    budget_result$consumption_energy$estimate <- sum(do$Consumption_energy, na.rm = TRUE)
    budget_result$respiration_energy$estimate <- sum(do$Respiration,         na.rm = TRUE)
    budget_result$egestion_energy$estimate    <- sum(do$Egestion,            na.rm = TRUE)
    budget_result$excretion_energy$estimate   <- sum(do$Excretion,           na.rm = TRUE)
    budget_result$sda_energy$estimate         <- sum(do$SDA,                 na.rm = TRUE)
    budget_result$net_energy$estimate         <- sum(do$Net_energy,          na.rm = TRUE)
    # SE / CI are not available for deterministic methods — leave as NA

  } else if (method == "hierarchical") {

    if (is.null(individual_id)) {
      # Population mean
      if (!is.null(result$method_data$population_uncertainty)) {
        budget_result <- assign_energy_components(
          budget_result,
          src     = result$method_data$population_uncertainty,
          mapping = c(
            consumption_energy = "mean_total_consumption",
            respiration_energy = "mean_respiration_energy",
            egestion_energy    = "mean_egestion_energy",
            excretion_energy   = "mean_excretion_energy",
            sda_energy         = "mean_sda_energy",
            net_energy         = "mean_net_energy"
          )
        )
      }
    } else {
      # Individual result
      if (individual_id > 0 && individual_id <= result$method_data$n_individuals) {
        if (!is.null(result$method_data$individual_uncertainty)) {
          # Note: individual consumption uses grams (total_consumption) as proxy
          budget_result <- assign_energy_components(
            budget_result,
            src     = result$method_data$individual_uncertainty,
            mapping = c(
              consumption_energy = "total_consumption",
              respiration_energy = "respiration_energy",
              egestion_energy    = "egestion_energy",
              excretion_energy   = "excretion_energy",
              sda_energy         = "sda_energy",
              net_energy         = "net_energy"
            ),
            idx = individual_id
          )
        }
      } else {
        warning("Invalid individual_id: ", individual_id)
        return(budget_result)
      }
    }
    
  } else if (method == "mle" && backend == "tmb") {
    # MLE with TMB uncertainty
    if (!is.null(result$method_data$tmb_uncertainty)) {
      budget_result <- assign_energy_components(
        budget_result,
        src     = result$method_data$tmb_uncertainty,
        mapping = c(
          consumption_energy = "total_consumption_energy",
          respiration_energy = "total_respiration_energy",
          egestion_energy    = "total_egestion_energy",
          excretion_energy   = "total_excretion_energy",
          sda_energy         = "total_sda_energy",
          net_energy         = "total_net_energy"
        )
      )
    }
  }
  
  # Calculate confidence intervals for all energy components
  energy_components <- c("consumption_energy", "respiration_energy", "egestion_energy", 
                        "excretion_energy", "sda_energy", "net_energy")
  
  for (component in energy_components) {
    estimate <- budget_result[[component]]$estimate
    se <- budget_result[[component]]$se
    
    if (!is.na(estimate) && !is.na(se)) {
      budget_result[[component]]$ci_lower <- estimate - z * se
      budget_result[[component]]$ci_upper <- estimate + z * se
    }
  }
  
  return(budget_result)
}