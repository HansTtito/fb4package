#' Strategy-Binary-search for FB4 Model
#'
#' @name strategy-binary-search
#' @aliases strategy-binary-search
NULL

# ============================================================================
# BINARY SEARCH STRATEGY (USING SHARED COMMONS)
# ============================================================================

#' Create Binary Search Strategy
#' @param execution_plan Execution plan with parameters
#' @return Strategy object implementing FB4Strategy interface
#' @keywords internal
create_binary_search_strategy <- function(execution_plan) {
  
  strategy <- list(
    
    execute = function(plan) {
      
      if (plan$verbose) {
        message("Executing binary search strategy")
      }
      
      # Use shared data preparation instead of duplicated code
      processed_data <- prepare_simulation_data(
        bio_obj = plan$bio_obj,
        strategy = "binary-search",
        fit_to = plan$fit_to,
        fit_value = plan$fit_value,
        first_day = plan$first_day,
        last_day = plan$last_day,
        output_format = "simulation"
      )
      
      # Extract common parameters using shared function
      params <- extract_strategy_parameters(
        execution_plan = plan,
        required_params = c("tolerance", "max_iterations"),
        default_values = list(
          tolerance      = 0.001,
          max_iterations = 25,
          lower          = 0.01,
          upper          = 5.0
        )
      )

      # Execute binary search fitting
      result <- fit_fb4_binary_search(
        target_value              = plan$fit_value,
        fit_type                  = tolower(plan$fit_to),
        processed_simulation_data = processed_data,
        oxycal                    = params$oxycal,
        tolerance                 = params$tolerance,
        max_iterations            = params$max_iterations,
        lower_bound               = params$lower,
        upper_bound               = params$upper,
        verbose                   = params$verbose
      )

      # Add strategy metadata using shared function
      strategy_info <- list(
        strategy_type = "binary_search",
        additional_metadata = list(
          tolerance      = params$tolerance,
          max_iterations = params$max_iterations,
          lower_bound    = params$lower,
          upper_bound    = params$upper
        )
      )
      
      result <- add_strategy_metadata(result, strategy_info, plan)
      
      return(result)
    },
    
    validate_plan = function(plan) {
      # Use shared validation
      validate_common_strategy_inputs(
        fit_to = plan$fit_to,
        fit_value = plan$fit_value,
        strategy_type = "binary_search"
      )
      
      # Binary search specific validation
      if (!plan$fit_to %in% c("Weight", "Consumption")) {
        stop("Binary search only supports Weight or Consumption fitting")
      }
      
      return(TRUE)
    },
    
    get_strategy_info = function() {
      return(list(
        name = "Binary Search",
        type = "traditional_fitting",
        supports_backends = "r",
        supports_fit_types = c("Weight", "Consumption"),
        description = "Iterative binary search for optimal p_value"
      ))
    }
  )
  
  class(strategy) <- c("FB4BinarySearchStrategy", "FB4Strategy")
  return(strategy)
}

# ============================================================================
# BINARY SEARCH ALGORITHMS
# ============================================================================

#' Binary search for optimal p_value
#'
#' @description
#' Pure binary search algorithm for finding p_value that achieves target metric.
#' Now uses shared simulation execution function.
#'
#' @param target_value Target value (final weight or total consumption)
#' @param fit_type Type of fitting ("weight" or "consumption")
#' @param lower_bound Lower bound for p_value search
#' @param upper_bound Upper bound for p_value search
#' @param simulation_function Function that runs simulation and returns metric
#' @param tolerance Tolerance for convergence
#' @param max_iterations Maximum number of iterations
#' @return List with fitted p_value and convergence information
#' @keywords internal
binary_search_p_value <- function(target_value, fit_type, lower_bound, upper_bound,
                                  simulation_function, tolerance = 0.001, max_iterations = 25) {
  
  iteration <- 0
  best_p <- NA
  best_error <- Inf
  
  while (iteration < max_iterations) {
    iteration <- iteration + 1
    mid_p <- (lower_bound + upper_bound) / 2
    
    # Execute simulation with candidate p_value
    sim_result <- simulation_function(mid_p)
    
    # Handle failed simulation
    if (is.null(sim_result) || is.na(sim_result)) {
      # Adjust range to avoid problematic p_value
      if (mid_p > 1) {
        upper_bound <- mid_p
      } else {
        lower_bound <- mid_p
      }
      next
    }
    
    # Calculate error
    error <- abs(sim_result - target_value)
    
    # Update best result
    if (error < best_error) {
      best_error <- error
      best_p <- mid_p
    }
    
    # Check convergence
    if (error <= tolerance) {
      return(list(
        p_value = mid_p,
        converged = TRUE,
        iterations = iteration,
        final_error = error
      ))
    }
    
    # Adjust range for next iteration
    if (sim_result < target_value) {
      lower_bound <- mid_p
    } else {
      upper_bound <- mid_p
    }
    
    # Check if range is too narrow
    if ((upper_bound - lower_bound) < 1e-10) {
      break
    }
  }
  
  # Return best result if no convergence
  return(list(
    p_value = best_p,
    converged = FALSE,
    iterations = iteration,
    final_error = best_error
  ))
}

#' Fit FB4 model using binary search
#'
#' @description
#' Coordinates binary search fitting process for weight or consumption targets.
#' Now uses shared commons functions to eliminate code duplication.
#'
#' @param target_value Target value to fit
#' @param fit_type Type of fitting ("weight" or "consumption")
#' @param processed_simulation_data Complete processed simulation data
#' @param oxycal Oxycalorific coefficient (J/g O2), default 13560
#' @param tolerance Tolerance for convergence, default 0.001
#' @param max_iterations Maximum number of iterations, default 25
#' @param verbose Whether to show progress messages, default FALSE
#' @return List with fitting results and final simulation
#' @keywords internal
fit_fb4_binary_search <- function(target_value, fit_type, processed_simulation_data,
                                  oxycal = 13560, tolerance = 0.001, max_iterations = 25,
                                  lower_bound = 0.01, upper_bound = 5.0,
                                  verbose = FALSE) {
  
  # Initial progress message
  if (verbose) {
    message("Starting binary search for ", fit_type, " fitting")
    message("Target value: ", target_value)
    message("Tolerance: ", tolerance)
  }
  
  # Create simulation function using shared execution wrapper
  simulation_function <- function(p_val) {
    execute_simulation_with_method(
      method_type = "p_value",
      method_value = p_val,
      processed_simulation_data = processed_simulation_data,
      oxycal = oxycal,
      extract_metric = fit_type,
      output_daily = FALSE,
      verbose = FALSE
    )
  }
  
  # Execute binary search
  search_result <- binary_search_p_value(
    target_value        = target_value,
    fit_type            = fit_type,
    lower_bound         = lower_bound,
    upper_bound         = upper_bound,
    simulation_function = simulation_function,
    tolerance           = tolerance,
    max_iterations      = max_iterations
  )
  
  # Progress reporting
  if (verbose) {
    if (search_result$converged) {
      message("Fitting converged in ", search_result$iterations, " iterations")
      message("Final p_value: ", round(search_result$p_value, 6))
      message("Final error: ", round(search_result$final_error, 6))
    } else {
      message("Fitting did not converge after ", search_result$iterations, " iterations")
      message("Best p_value found: ", round(search_result$p_value, 6))
      message("Best error achieved: ", round(search_result$final_error, 6))
    }
  }
  
  # Execute final simulation with best p_value using shared function
  if (!is.na(search_result$p_value)) {
    final_simulation <- run_final_simulation(
      optimal_p_value = search_result$p_value,
      processed_simulation_data = processed_simulation_data,
      oxycal = oxycal,
      verbose = verbose
    )
    
    if (!is.null(final_simulation)) {
      # Combine results
      return(list(
        # Fitting information
        p_value = search_result$p_value,
        converged = search_result$converged,
        iterations = search_result$iterations,
        final_error = search_result$final_error,
        target_value = target_value,
        fit_type = fit_type,
        
        # Simulation results
        final_weight = final_simulation$final_weight,
        total_consumption_g = final_simulation$total_consumption_g,
        daily_output = final_simulation$daily_output,
        
        # Metadata
        method = "binary_search",
        tolerance = tolerance
      ))
    }
  }
  
  # Return error result using shared function
  return(create_error_result(
    error_message = "Binary search failed to find valid p_value",
    strategy_type = "binary_search",
    execution_plan = list(tolerance = tolerance, max_iterations = max_iterations)
  ))
}