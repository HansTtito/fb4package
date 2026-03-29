#' Strategy-Optim for FB4 Model
#'
#' @name strategy-optim
#' @aliases strategy-optim
NULL

# ============================================================================
# OPTIM STRATEGY (USING SHARED COMMONS)
# ============================================================================

#' Create Optim Strategy
#' @param execution_plan Execution plan with parameters
#' @return Strategy object implementing FB4Strategy interface
#' @keywords internal
create_optim_strategy <- function(execution_plan) {
  
  strategy <- list(
    
    execute = function(plan) {
      
      if (plan$verbose) {
        message("Executing optim strategy")
      }
      
      # Use shared data preparation
      processed_data <- prepare_simulation_data(
        bio_obj = plan$bio_obj,
        strategy = "optim",
        fit_to = plan$fit_to,
        fit_value = plan$fit_value,
        first_day = plan$first_day,
        last_day = plan$last_day,
        output_format = "simulation"
      )
      
      # Extract parameters using shared function
      params <- extract_strategy_parameters(
        execution_plan = plan,
        required_params = c("optim_method", "lower", "upper"),
        default_values = list(
          optim_method = "Brent",
          lower = 0.01,
          upper = 2.00,
          hessian = FALSE
        )
      )
      
      # Execute optim fitting
      result <- fit_fb4_optim(
        target_value = plan$fit_value,
        fit_type = tolower(plan$fit_to),
        processed_simulation_data = processed_data,
        method = params$optim_method,
        oxycal = params$oxycal,
        lower = params$lower,
        upper = params$upper,
        hessian = params$hessian,
        verbose = params$verbose
      )
      
      # Add strategy metadata using shared function
      strategy_info <- list(
        strategy_type = "optim",
        additional_metadata = list(
          optim_method = params$optim_method,
          lower = params$lower,
          upper = params$upper,
          hessian = params$hessian
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
        strategy_type = "optim"
      )
      
      # Optim specific validation
      if (!plan$fit_to %in% c("Weight", "Consumption")) {
        stop("Optim strategy only supports Weight or Consumption fitting")
      }
      
      return(TRUE)
    },
    
    get_strategy_info = function() {
      return(list(
        name = "Optimization (optim)",
        type = "traditional_fitting",
        supports_backends = "r",
        supports_fit_types = c("Weight", "Consumption"),
        description = "R's optim() function for p_value optimization"
      ))
    }
  )
  
  class(strategy) <- c("FB4OptimStrategy", "FB4Strategy")
  return(strategy)
}

# ============================================================================
# OPTIM ALGORITHMS
# ============================================================================

#' Optimization using optim() for optimal p_value
#'
#' @description
#' Uses R's optim() function for finding p_value that achieves target metric.
#' Now uses shared simulation execution function.
#'
#' @param target_value Target value (final weight or total consumption)
#' @param fit_type Type of fitting ("weight" or "consumption")
#' @param simulation_function Function that runs simulation and returns metric
#' @param method Optimization method ("Brent", "L-BFGS-B", etc.)
#' @param lower Lower bound for p_value search, default 0.01
#' @param upper Upper bound for p_value search, default 5.0
#' @param hessian Whether to compute Hessian for standard errors, default FALSE
#' 
#' @return List with fitted p_value and convergence information
#' @keywords internal
optim_search_p_value <- function(target_value, fit_type, simulation_function,
                                 method = "Brent", lower = 0.01, upper = 5.0, hessian = FALSE) {
  
  # Objective function: minimize |result - target|
  counter <- 0
  objective_function <- function(p_value) {
    counter <<- counter + 1
    sim_result <- simulation_function(p_value)
    
    # Handle failed simulation
    if (is.null(sim_result) || is.na(sim_result)) {
      return(1e6)  # Large penalty
    }
    
    # Return absolute error
    return(abs(sim_result - target_value))
  }
  
  # Run optimization — all supported methods (Brent, L-BFGS-B, …) use the
  # same optim() call; the previous if/else was dead code.
  optim_result <- optim(
    par     = (lower + upper) / 2,
    fn      = objective_function,
    method  = method,
    lower   = lower,
    upper   = upper,
    hessian = hessian
  )
  
  return(list(
    p_value = optim_result$par,
    converged = (optim_result$convergence == 0),
    iterations = counter,
    final_error = optim_result$value,
    method_used = method,
    hessian = optim_result$hessian %||% NA
  ))
}

#' Fit FB4 model using optim()
#'
#' @description
#' Coordinates optim() fitting process for weight or consumption targets.
#' Now uses shared commons functions to eliminate code duplication.
#'
#' @param target_value Target value to fit
#' @param fit_type Type of fitting ("weight" or "consumption")
#' @param processed_simulation_data Complete processed simulation data
#' @param method Optimization method, default "Brent"
#' @param oxycal Oxycalorific coefficient (J/g O2), default 13560
#' @param lower Lower bound for p_value search, default 0.01
#' @param upper Upper bound for p_value search, default 5.0
#' @param hessian Whether to compute Hessian for standard errors, default FALSE
#' @param verbose Whether to show progress messages, default FALSE
#' @return List with fitting results and final simulation
#' @keywords internal
fit_fb4_optim <- function(target_value, fit_type, processed_simulation_data,
                          method = "Brent", oxycal = 13560, lower = 0.01, upper = 5.0,
                          hessian = FALSE, verbose = FALSE) {
  
  if (verbose) {
    message("Starting optim() search for ", fit_type, " fitting")
    message("Target value: ", target_value)
    message("Method: ", method)
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
  
  # Execute optim search
  search_result <- optim_search_p_value(
    target_value = target_value,
    fit_type = fit_type,
    simulation_function = simulation_function,
    method = method,
    lower = lower,
    upper = upper,
    hessian = hessian
  )
  
  # Progress reporting
  if (verbose) {
    if (search_result$converged) {
      message("Fitting converged in ", search_result$iterations, " iterations")
      message("Final p_value: ", round(search_result$p_value, 6))
      message("Final error: ", round(search_result$final_error, 6))
    } else {
      message("Fitting did not converge")
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
        hessian = search_result$hessian,
        
        # Simulation results
        final_weight = final_simulation$final_weight,
        total_consumption_g = final_simulation$total_consumption_g,
        daily_output = final_simulation$daily_output,
        
        # Metadata
        method = paste0("optim_", method),
        method_details = search_result$method_used
      ))
    }
  }
  
  # Return error result using shared function
  return(create_error_result(
    error_message = "Optim search failed",
    strategy_type = "optim",
    execution_plan = list(method = method, lower = lower, upper = upper)
  ))
}