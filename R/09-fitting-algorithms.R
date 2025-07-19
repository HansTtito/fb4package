#' Fitting Algorithms for FB4 Model - Integrated Version
#'
#' @name fitting-algorithms-fb4
#' @aliases fitting-algorithms-fb4
NULL

# ============================================================================
# MAIN INTEGRATED FITTING FUNCTIONS
# ============================================================================

#' Binary Search Algorithm for FB4 Fitting
#'
#' Robust implementation of binary search algorithm based on critical functions
#'
#' @param species_params Extracted species parameters
#' @param initial_weight Initial weight (g)
#' @param fit_to Type of fitting ("weight" or "consumption")
#' @param fit_value Target value
#' @param processed_data Processed environmental data
#' @param model_options Model options
#' @param oxycal Oxycalorific coefficient
#' @param output_daily Return daily output
#' @param tolerance Tolerance for convergence
#' @param max_iterations Maximum number of iterations
#' @return List with fitting result and final simulation
fit_fb4_binary_search <- function(species_params, initial_weight, fit_to, fit_value,
                                  processed_data, model_options, oxycal = 13560,
                                  output_daily = TRUE, tolerance = 0.001, max_iterations = 25) {
  
  # Validations
  if (!fit_to %in% c("weight", "consumption")) {
    stop("fit_to must be 'weight' or 'consumption'")
  }
  
  fit_value <- check_numeric_value(fit_value, "fit_value", min_val = 0.001)
  
  # Execute binary search using improved critical function
  fit_result <- binary_search_p_value_robust(
    target_value = fit_value,
    fit_type = fit_to,
    species_params = species_params,
    initial_weight = initial_weight,
    processed_data = processed_data,
    model_options = model_options,
    oxycal = oxycal,
    tolerance = tolerance,
    max_iterations = max_iterations
  )
  
  # If fitting was successful, execute final complete simulation
  if (fit_result$fit_successful) {
    final_simulation <- run_fb4_simulation_unified(
      initial_weight = initial_weight,
      consumption_params = list(type = "p_value", value = fit_result$p_value),
      species_params = species_params,
      processed_data = processed_data,
      model_options = model_options,
      oxycal = oxycal,
      output_daily = output_daily
    )
    
    # Combine results
    result <- list(
      # Fitting information
      fit_info = fit_result,
      # Simulation results
      final_weight = final_simulation$final_weight,
      total_consumption = final_simulation$total_consumption,
      p_value = fit_result$p_value,
      fit_successful = fit_result$fit_successful,
      fit_iterations = fit_result$iterations,
      fit_error = fit_result$final_error
    )
    
    # Add daily output if requested
    if (output_daily) {
      result$daily_output <- final_simulation$daily_output
    }
    
  } else {
    # If fitting failed, return error information
    result <- list(
      fit_info = fit_result,
      final_weight = NA,
      total_consumption = NA,
      p_value = fit_result$p_value,
      fit_successful = FALSE,
      fit_iterations = fit_result$iterations,
      fit_error = fit_result$final_error,
      error_message = "Fitting did not converge within allowed iterations"
    )
  }
  
  return(result)
}

#' Robust Binary Search for P-value (Improved Version)
#'
#' Improved version based on critical function from first script
#'
#' @param target_value Target value (final weight or total consumption)
#' @param fit_type Type of fitting ("weight" or "consumption")
#' @param species_params Species parameters
#' @param initial_weight Initial weight
#' @param processed_data Processed data
#' @param model_options Model options
#' @param oxycal Oxycalorific coefficient
#' @param tolerance Tolerance for convergence
#' @param max_iterations Maximum number of iterations
#' @return List with fitted p-value and fitting information
binary_search_p_value_robust <- function(target_value, fit_type,
                                         species_params, initial_weight,
                                         processed_data, model_options,
                                         oxycal = 13560,
                                         tolerance = 0.001,
                                         max_iterations = 25) {
  
  # Initial range for p_value
  lower <- 0.01
  upper <- 5
  
  iteration <- 0
  fit_successful <- FALSE
  best_p <- NA
  best_error <- Inf
  
  # INITIAL PRINT
  cat("\n=== BINARY SEARCH START ===\n")
  cat("Target value:", target_value, "\n")
  cat("Fit type:", fit_type, "\n")
  cat("Initial weight:", initial_weight, "\n")
  cat("Initial range: [", lower, ",", upper, "]\n")
  cat("Tolerance:", tolerance, "\n\n")
  
  while (iteration < max_iterations) {
    iteration <- iteration + 1
    mid_p <- (lower + upper) / 2
    
    # ITERATION PRINT
    cat("Iteration", iteration, "p-value =", mid_p)
    
    # Execute simulation with candidate p_value
    sim <- run_fb4_simulation_unified(
      initial_weight = initial_weight,
      consumption_params = list(type = "p_value", value = mid_p),
      species_params = species_params,
      processed_data = processed_data,
      model_options = model_options,
      oxycal = oxycal,
      output_daily = FALSE
    )
    
    # Select metric to evaluate according to fit_type
    metric <- if (fit_type == "weight") sim$final_weight else sim$total_consumption
    
    error <- abs(metric - target_value)
    
    # PRINT ITERATION RESULTS
    cat(" Result =", round(metric, 6), "Target =", target_value)
    cat(" Error =", round(error, 6))
    
    # Save best result
    if (error < best_error) {
      best_error <- error
      best_p <- mid_p
      cat(" [BEST]")
    }
    
    # Check convergence
    if (error <= tolerance) {
      fit_successful <- TRUE
      cat(" [CONVERGED]\n")
      break
    }
    
    # Adjust range for next iteration
    if (metric < target_value) {
      lower <- mid_p
      cat(" [INCREASE p]")
    } else {
      upper <- mid_p
      cat(" [DECREASE p]")
    }
    
    cat(" New range: [", round(lower, 6), ",", round(upper, 6), "]\n")
  }
  
  # FINAL PRINT
  cat("\n=== FINAL RESULT ===\n")
  cat("Converged:", fit_successful, "\n")
  cat("Iterations:", iteration, "\n")
  cat("Best p-value:", best_p, "\n")
  cat("Final error:", best_error, "\n")
  cat("==================\n\n")
  
  return(list(
    p_value = best_p,
    fit_successful = fit_successful,
    iterations = iteration,
    final_error = best_error
  ))
}