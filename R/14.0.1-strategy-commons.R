#' Strategy Commons for FB4 Model
#'
#' @description
#' Shared functions used across multiple FB4 strategies to eliminate code duplication.
#' Consolidates execution, data preparation, and parameter extraction logic.
#'
#' @name strategy-commons
#' @aliases strategy-commons
NULL

# ============================================================================
# SHARED SIMULATION EXECUTION
# ============================================================================

#' Execute simulation with any method
#'
#' @description
#' Unified simulation execution function used by all strategies.
#' Consolidates the execute_simulation_with_method() that was duplicated 
#' across multiple strategy files.
#'
#' @param method_type Method type ("p_value", "ration_percent", "ration_grams")
#' @param method_value Method-specific value
#' @param processed_simulation_data Complete processed simulation data
#' @param oxycal Oxycalorific coefficient, default 13560
#' @param extract_metric What to extract: "weight", "consumption", "full" (default "full")
#' @param output_daily Whether to include daily output, default FALSE for fitting
#' @param verbose Whether to show progress messages, default FALSE
#' @return Extracted metric (if extract_metric specified) or full simulation result
#' @keywords internal
execute_simulation_with_method <- function(method_type, method_value, processed_simulation_data,
                                          oxycal = 13560, extract_metric = "full", 
                                          output_daily = FALSE, verbose = FALSE) {
  
  if (verbose) {
    message("Running simulation with method: ", method_type, " = ", method_value)
  }
  
  consumption_method <- list(type = method_type, value = method_value)
  
  tryCatch({
    sim_result <- run_fb4_simulation(
      consumption_method = consumption_method,
      processed_simulation_data = processed_simulation_data,
      oxycal = oxycal,
      output_daily = output_daily,
      verbose = verbose
    )
    
    # Extract specific metric if requested
    if (extract_metric == "weight") {
      return(sim_result$final_weight)
    } else if (extract_metric == "consumption") {
      return(sim_result$total_consumption_g)
    } else {
      return(sim_result)  # "full" - return complete result
    }
    
  }, error = function(e) {
    if (verbose) {
      warning("Simulation failed: ", e$message)
    }
    return(NULL)
  })
}

# ============================================================================
# SHARED PARAMETER EXTRACTION
# ============================================================================

#' Extract common strategy parameters from execution plan
#'
#' @description
#' Extracts and validates common parameters used across multiple strategies.
#' Eliminates parameter extraction duplication.
#'
#' @param execution_plan Execution plan with parameters
#' @param required_params Vector of required parameter names
#' @param default_values Named list of default values
#' @return List with extracted and validated parameters
#' @keywords internal
extract_strategy_parameters <- function(execution_plan, required_params = NULL, 
                                       default_values = list()) {
  
  # Standard parameters used across strategies
  standard_params <- list(
    oxycal = execution_plan$oxycal %||% default_values$oxycal %||% 13560,
    tolerance = execution_plan$tolerance %||% default_values$tolerance %||% 0.001,
    max_iterations = execution_plan$max_iterations %||% default_values$max_iterations %||% 25,
    verbose = execution_plan$verbose %||% default_values$verbose %||% FALSE,
    first_day = execution_plan$first_day %||% default_values$first_day %||% 1,
    last_day = execution_plan$last_day %||% default_values$last_day,
    backend = execution_plan$backend %||% default_values$backend %||% "r"
  )
  
  # Extract additional parameters from execution plan
  additional_params <- execution_plan$additional_params %||% list()
  
  # Add strategy-specific parameters with defaults
  strategy_params <- list(
    # Traditional strategies
    lower = additional_params$lower %||% default_values$lower %||% 0.01,
    upper = additional_params$upper %||% default_values$upper %||% 5.0,
    optim_method = additional_params$optim_method %||% default_values$optim_method %||% "Brent",
    hessian = additional_params$hessian %||% default_values$hessian %||% FALSE,
    
    # Statistical strategies  
    confidence_level = additional_params$confidence_level %||% default_values$confidence_level %||% 0.95,
    estimate_sigma = additional_params$estimate_sigma %||% default_values$estimate_sigma %||% TRUE,
    compute_profile = additional_params$compute_profile %||% default_values$compute_profile %||% FALSE,
    profile_grid_size = additional_params$profile_grid_size %||% default_values$profile_grid_size %||% 50,
    
    # Bootstrap-specific
    n_bootstrap = additional_params$n_bootstrap %||% default_values$n_bootstrap %||% 1000,
    parallel = additional_params$parallel %||% default_values$parallel %||% FALSE,
    n_cores = additional_params$n_cores %||% default_values$n_cores,
    sample_size = additional_params$sample_size %||% default_values$sample_size,
    compute_percentiles = additional_params$compute_percentiles %||% default_values$compute_percentiles %||% TRUE,
    store_predicted_weights_boot = additional_params$store_predicted_weights_boot %||% default_values$store_predicted_weights_boot %||% TRUE
  )
  
  # Combine all parameters
  all_params <- c(standard_params, strategy_params)
  
  # Validate required parameters if specified
  if (!is.null(required_params)) {
    missing_required <- setdiff(required_params, names(all_params))
    if (length(missing_required) > 0) {
      stop("Missing required parameters: ", paste(missing_required, collapse = ", "))
    }
  }
  
  return(all_params)
}

# ============================================================================
# SHARED STRATEGY METADATA
# ============================================================================

#' Add standard strategy metadata to results
#'
#' @description
#' Adds consistent metadata structure to strategy results.
#' Eliminates metadata creation duplication.
#'
#' @param result_list Current result list  
#' @param strategy_info Strategy-specific information
#' @param execution_plan Original execution plan
#' @return Updated result list with strategy metadata
#' @keywords internal
add_strategy_metadata <- function(result_list, strategy_info, execution_plan) {
  
  result_list$strategy_info <- list(
    strategy_type = strategy_info$strategy_type,
    backend = execution_plan$backend,
    execution_plan = execution_plan,
    timestamp = Sys.time()
  )
  
  # Add strategy-specific metadata
  if (!is.null(strategy_info$additional_metadata)) {
    result_list$strategy_info <- c(result_list$strategy_info, strategy_info$additional_metadata)
  }
  
  return(result_list)
}

# ============================================================================
# SHARED FINAL SIMULATION
# ============================================================================

#' Run final simulation with optimal parameter
#'
#' @description
#' Runs final detailed simulation using optimal parameter found by strategy.
#' Used by binary search, optim, and MLE strategies.
#'
#' @param optimal_p_value Optimal p_value found by strategy
#' @param processed_simulation_data Processed simulation data
#' @param oxycal Oxycalorific coefficient
#' @param verbose Show simulation progress
#' @return Complete simulation result with daily output
#' @keywords internal
run_final_simulation <- function(optimal_p_value, processed_simulation_data, 
                                oxycal = 13560, verbose = FALSE) {
  
  if (is.na(optimal_p_value) || optimal_p_value <= 0) {
    warning("Invalid optimal p_value: ", optimal_p_value)
    return(NULL)
  }
  
  if (verbose) {
    message("Running final simulation with p_value: ", round(optimal_p_value, 6))
  }
  
  tryCatch({
    final_simulation <- run_fb4_simulation(
      consumption_method = list(type = "p_value", value = optimal_p_value),
      processed_simulation_data = processed_simulation_data,
      oxycal = oxycal,
      output_daily = TRUE,
      verbose = FALSE
    )
    
    return(final_simulation)
    
  }, error = function(e) {
    warning("Final simulation failed: ", e$message)
    return(NULL)
  })
}

# ============================================================================
# SHARED VALIDATION HELPERS
# ============================================================================

#' Validate common strategy inputs
#'
#' @description
#' Common input validation used across strategies.
#' Consolidates basic validation logic.
#'
#' @param fit_to Fitting target (for traditional strategies)
#' @param fit_value Fitting value (for traditional strategies)  
#' @param observed_weights Observed weights (for statistical strategies)
#' @param strategy_type Strategy type for context
#' @return TRUE if valid, stops with error otherwise
#' @keywords internal
validate_common_strategy_inputs <- function(fit_to = NULL, fit_value = NULL,
                                          observed_weights = NULL, strategy_type = NULL) {
  
  # Traditional strategies need fit_to/fit_value
  if (!is.null(fit_to) || !is.null(fit_value)) {
    if (is.null(fit_to) || is.null(fit_value)) {
      stop("fit_to and fit_value must both be provided or both be NULL")
    }
    
    if (!is.numeric(fit_value) || length(fit_value) != 1 || fit_value <= 0) {
      stop("fit_value must be a single positive number")
    }
    
    valid_fit_options <- c("Weight", "Consumption", "Ration", "Ration_prey", "p_value")
    if (!fit_to %in% valid_fit_options) {
      stop("fit_to must be one of: ", paste(valid_fit_options, collapse = ", "))
    }
  }
  
  # Statistical strategies need observed_weights
  if (!is.null(observed_weights)) {
    if (!is.numeric(observed_weights) && !is.data.frame(observed_weights)) {
      stop("observed_weights must be numeric vector or data.frame")
    }
    
    if (is.numeric(observed_weights)) {
      if (any(observed_weights <= 0, na.rm = TRUE)) {
        stop("All observed_weights must be positive")
      }
      
      if (length(observed_weights) < 3) {
        stop("At least 3 observations required for statistical strategies")
      }
    }
  }
  
  return(TRUE)
}

# ============================================================================
# SHARED ERROR HANDLING
# ============================================================================

#' Create standardized error result
#'
#' @description
#' Creates consistent error result structure across strategies.
#'
#' @param error_message Error description
#' @param strategy_type Strategy that failed
#' @param execution_plan Original execution plan
#' @return Standardized error result
#' @keywords internal
create_error_result <- function(error_message, strategy_type, execution_plan) {
  
  list(
    # Basic failure indicators
    converged = FALSE,
    p_value = NA,
    final_weight = NA,
    total_consumption_g = NA,
    daily_output = NULL,
    
    # Error information
    error_message = error_message,
    method = paste0(strategy_type, "_failed"),
    
    # Strategy metadata
    strategy_info = list(
      strategy_type = strategy_type,
      backend = execution_plan$backend %||% "r",
      execution_plan = execution_plan,
      error_timestamp = Sys.time()
    )
  )
}