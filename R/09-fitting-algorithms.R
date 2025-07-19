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
#' @param verbose Logical, whether to show progress messages (default FALSE)
#' @return List with fitting result and final simulation
#' @keywords internal
fit_fb4_binary_search <- function(species_params, initial_weight, fit_to, fit_value,
                                  processed_data, model_options, oxycal = 13560,
                                  output_daily = TRUE, tolerance = 0.001, 
                                  max_iterations = 25, verbose = FALSE) {
  
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
    max_iterations = max_iterations,
    verbose = verbose
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
    warning("Model fitting did not converge within ", max_iterations, 
            " iterations. Final error: ", round(fit_result$final_error, 6))
    
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
#' @param verbose Logical, whether to show progress messages (default FALSE)
#' @return List with fitted p-value and fitting information
#' @keywords internal
binary_search_p_value_robust <- function(target_value, fit_type,
                                         species_params, initial_weight,
                                         processed_data, model_options,
                                         oxycal = 13560,
                                         tolerance = 0.001,
                                         max_iterations = 25,
                                         verbose = FALSE) {
  
  # Initial range for p_value
  lower <- 0.01
  upper <- 5
  
  iteration <- 0
  fit_successful <- FALSE
  best_p <- NA
  best_error <- Inf
  
  # Initial progress message
  if (verbose) {
    message("Starting binary search for ", fit_type, " fitting")
    message("Target value: ", target_value)
    message("Initial weight: ", initial_weight, " g")
    message("Tolerance: ", tolerance)
  }
  
  while (iteration < max_iterations) {
    iteration <- iteration + 1
    mid_p <- (lower + upper) / 2
    
    # Execute simulation with candidate p_value
    sim <- tryCatch({
      run_fb4_simulation_unified(
        initial_weight = initial_weight,
        consumption_params = list(type = "p_value", value = mid_p),
        species_params = species_params,
        processed_data = processed_data,
        model_options = model_options,
        oxycal = oxycal,
        output_daily = FALSE
      )
    }, error = function(e) {
      if (verbose) {
        warning("Simulation failed at iteration ", iteration, 
                " with p-value ", round(mid_p, 4), ": ", e$message)
      }
      return(NULL)
    })
    
    # Check if simulation failed
    if (is.null(sim)) {
      # Adjust range to avoid problematic p-value
      if (mid_p > 1) {
        upper <- mid_p
      } else {
        lower <- mid_p
      }
      next
    }
    
    # Select metric to evaluate according to fit_type
    metric <- if (fit_type == "weight") sim$final_weight else sim$total_consumption
    
    # Check for invalid results
    if (is.na(metric) || !is.finite(metric) || metric <= 0) {
      if (verbose) {
        warning("Invalid simulation result at iteration ", iteration, 
                " with p-value ", round(mid_p, 4))
      }
      # Adjust range to avoid problematic p-value
      if (mid_p > 1) {
        upper <- mid_p
      } else {
        lower <- mid_p
      }
      next
    }
    
    error <- abs(metric - target_value)
    
    # Progress message for verbose mode
    if (verbose) {
      message("Iteration ", iteration, ": p-value = ", round(mid_p, 4), 
              ", result = ", round(metric, 2), 
              ", error = ", round(error, 4))
    }
    
    # Save best result
    if (error < best_error) {
      best_error <- error
      best_p <- mid_p
    }
    
    # Check convergence
    if (error <= tolerance) {
      fit_successful <- TRUE
      if (verbose) {
        message("Convergence achieved at iteration ", iteration)
      }
      break
    }
    
    # Adjust range for next iteration
    if (metric < target_value) {
      lower <- mid_p
    } else {
      upper <- mid_p
    }
    
    # Check if range is too narrow (potential infinite loop)
    if ((upper - lower) < 1e-10) {
      if (verbose) {
        warning("Search range became too narrow. Stopping search.")
      }
      break
    }
  }
  
  # Final status message
  if (verbose) {
    if (fit_successful) {
      message("Fitting completed successfully in ", iteration, " iterations")
      message("Final p-value: ", round(best_p, 6))
      message("Final error: ", round(best_error, 6))
    } else {
      message("Fitting did not converge after ", iteration, " iterations")
      message("Best p-value found: ", round(best_p, 6))
      message("Best error achieved: ", round(best_error, 6))
    }
  }
  
  # Issue warning if convergence failed
  if (!fit_successful && best_error > tolerance * 10) {
    warning("Binary search did not achieve good convergence. ",
            "Consider adjusting tolerance or checking model parameters.")
  }
  
  return(list(
    p_value = best_p,
    fit_successful = fit_successful,
    iterations = iteration,
    final_error = best_error,
    convergence_tolerance = tolerance
  ))
}