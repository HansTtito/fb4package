#' Direct Strategies for FB4 Model (UPDATED)
#'
#' @name strategy-direct
#' @aliases strategy-direct
NULL

# ============================================================================
# DIRECT STRATEGY FUNCTIONS (USING SHARED COMMONS)
# ============================================================================

#' Create Direct Strategy
#' @param execution_plan Execution plan with parameters
#' @param direct_type Type of direct method ("p_value", "ration_percent", "ration_grams")
#' @return Strategy object implementing FB4Strategy interface
#' @keywords internal
create_direct_strategy <- function(execution_plan, direct_type) {
  
  strategy <- list(
    
    execute = function(plan) {

      if (plan$verbose) {
        message("Executing direct strategy: ", direct_type)
      }

      # Resolve method value: create_fb4_strategy normalises fit_value in its
      # LOCAL copy of execution_plan, but R passes lists by value so the outer
      # execution_plan still has fit_value = NULL when execute() is called.
      # Fall back to additional_params$p_value / fit_value as a safety net.
      method_value <- plan$fit_value %||%
                      plan$additional_params$p_value %||%
                      plan$additional_params$fit_value

      # Use shared data preparation
      processed_data <- prepare_simulation_data(
        bio_obj = plan$bio_obj,
        strategy = "direct",
        fit_to = plan$fit_to,
        fit_value = method_value,
        first_day = plan$first_day,
        last_day = plan$last_day,
        output_format = "simulation"
      )

      # Extract common parameters using shared function
      params <- extract_strategy_parameters(
        execution_plan = plan,
        default_values = list(
          oxycal = plan$oxycal
        )
      )

      # Execute direct method using shared commons
      result <- run_fb4_direct_method(
        method_type = direct_type,
        method_value = method_value,
        processed_simulation_data = processed_data,
        oxycal = params$oxycal,
        verbose = params$verbose
      )
      
      # Add strategy metadata using shared function
      strategy_info <- list(
        strategy_type = "direct",
        additional_metadata = list(
          direct_type = direct_type
        )
      )
      
      result <- add_strategy_metadata(result, strategy_info, plan)
      
      return(result)
    },
    
    validate_plan = function(plan) {
      # Resolve using same fallback chain as execute()
      val <- plan$fit_value %||%
             plan$additional_params$p_value %||%
             plan$additional_params$fit_value

      # Direct strategies don't use fit_to — only validate val directly
      if (is.null(val) || !is.numeric(val) || length(val) != 1) {
        stop("Direct strategy requires a single numeric fit_value (e.g. p_value = 0.5)")
      }

      # Method-specific validation
      if (direct_type == "p_value" && (val <= 0 || val > 5)) {
        stop("p_value must be between 0 and 5")
      }

      if (direct_type == "ration_percent" && (val < 0 || val > 100)) {
        stop("ration_percent must be between 0 and 100")
      }
      
      if (direct_type == "ration_grams" && val <= 0) {
        stop("ration_grams must be positive")
      }
      
      return(TRUE)
    },
    
    get_strategy_info = function() {
      return(list(
        name = paste("Direct", toupper(direct_type)),
        type = "direct_execution",
        supports_backends = "r",
        supports_fit_types = direct_type,
        description = paste("Direct execution with", direct_type, "method")
      ))
    }
  )
  
  class(strategy) <- c("FB4DirectStrategy", "FB4Strategy")
  return(strategy)
}

# ============================================================================
# DIRECT METHOD EXECUTION
# ============================================================================

#' Run FB4 with direct method
#' 
#' @description
#' Execute FB4 simulation directly with specified method and value.
#' Now uses shared commons functions for consistency.
#'
#' @param method_type Method type ("p_value", "ration_percent", "ration_grams")
#' @param method_value Method-specific value
#' @param processed_simulation_data Complete processed simulation data
#' @param oxycal Oxycalorific coefficient, default 13560
#' @param verbose Whether to show progress messages, default FALSE
#' @return Complete simulation result
#' @keywords internal
run_fb4_direct_method <- function(method_type, method_value, processed_simulation_data,
                                  oxycal = 13560, verbose = FALSE) {
  
  if (verbose) {
    message("Running direct simulation with ", method_type, " = ", method_value)
  }
  
  # Execute simulation using shared function with full output
  sim_result <- execute_simulation_with_method(
    method_type = method_type,
    method_value = method_value,
    processed_simulation_data = processed_simulation_data,
    oxycal = oxycal,
    extract_metric = "full",  # Get complete simulation result
    output_daily = TRUE,      # Include daily output for direct execution
    verbose = verbose
  )
  
  if (is.null(sim_result)) {
    # Return error result using shared function
    return(create_error_result(
      error_message = paste("Direct simulation failed with", method_type, "=", method_value),
      strategy_type = "direct",
      execution_plan = list(method_type = method_type, method_value = method_value)
    ))
  }
  
  # Add direct method information
  sim_result$method <- paste0("direct_", method_type)
  sim_result$converged <- TRUE  # Direct execution always "converges"
  sim_result$fit_type <- method_type
  sim_result$fit_value <- method_value
  
  # Calculate effective p_value if not p_value method
  if (method_type != "p_value") {
    # For ration methods, we can calculate equivalent p_value from the result
    if (!is.null(sim_result$daily_output) && "P_value" %in% names(sim_result$daily_output)) {
      sim_result$effective_p_value <- mean(sim_result$daily_output$P_value, na.rm = TRUE)
    } else {
      sim_result$effective_p_value <- NA
    }
  } else {
    sim_result$effective_p_value <- method_value
  }
  
  if (verbose) {
    message("Direct simulation completed successfully")
    message("Final weight: ", round(sim_result$final_weight, 2), "g")
    message("Total consumption: ", round(sim_result$total_consumption_g, 2), "g")
  }
  
  return(sim_result)
}