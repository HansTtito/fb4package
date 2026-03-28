#' Strategy Interface and Factory for FB4 Model
#'
#' @name strategy-interface
#' @aliases strategy-interface
NULL

# ============================================================================
# STRATEGY INTERFACE DEFINITION
# ============================================================================

#' FB4 Strategy Interface
#'
#' @description
#' Defines the common interface that all FB4 execution strategies must implement.
#' This ensures consistent behavior across different fitting methods while allowing
#' for method-specific optimizations.
#'
#' @details
#' All strategies must implement:
#' - execute(execution_plan): Main execution logic
#' - validate_plan(execution_plan): Strategy-specific validation
#' - get_strategy_info(): Metadata about the strategy
#'
#' @keywords internal
FB4Strategy <- list(
  # execute(execution_plan): Main execution logic — must be implemented by subclass
  execute = function(execution_plan) {
    stop("execute() must be implemented by concrete strategy")
  },
  # validate_plan(execution_plan): Strategy-specific validation
  validate_plan = function(execution_plan) {
    stop("validate_plan() must be implemented by concrete strategy")
  },
  # get_strategy_info(): Returns metadata about the strategy
  get_strategy_info = function() {
    stop("get_strategy_info() must be implemented by concrete strategy")
  }
)

# ============================================================================
# EXECUTION PLAN CREATION
# ============================================================================


#' Create execution plan for FB4 strategies
#'
#' @description
#' Consolidates all parameters needed for FB4 execution into a single plan object.
#' Handles method auto-detection, backend selection, and parameter validation.
#'
#' @param bio_obj Bioenergetic object
#' @param fit_to Target type for fitting
#' @param fit_value Target value for fitting
#' @param observed_weights Vector of observed weights (for MLE/bootstrap)
#' @param strategy Execution method ("binary_search", "optim", "mle", "bootstrap", "hierarchical")
#' @param backend Backend selection ("r", "tmb")
#' @param first_day First simulation day
#' @param last_day Last simulation day
#' @param ... Additional parameters
#'
#' @return List with complete execution plan
#' @keywords internal
create_execution_plan <- function(bio_obj, fit_to = NULL, fit_value = NULL, 
                                  observed_weights = NULL, covariates = NULL, strategy, 
                                  backend, first_day = 1, last_day = NULL,
                                  oxycal = 13560, tolerance = 0.001, 
                                  max_iterations = 25, verbose = FALSE, ...) {
  
  # ============================================================================
  # CREATE EXECUTION PLAN
  # ============================================================================
  
  execution_plan <- list(
    # Core parameters
    bio_obj = bio_obj,
    strategy = strategy,
    backend = backend,
    
    # Fitting parameters
    fit_to = fit_to,
    fit_value = fit_value,
    observed_weights = observed_weights,
    covariates = covariates,
    
    # Simulation parameters
    first_day = first_day,
    last_day = last_day,
    initial_weight = bio_obj$simulation_settings$initial_weight,
    simulation_days = last_day - first_day + 1,
    
    # Algorithm parameters
    oxycal = oxycal,
    tolerance = tolerance,
    max_iterations = max_iterations,
    
    # Execution parameters
    verbose = verbose,
    timestamp = Sys.time(),
    
    # Additional parameters (from ...)
    additional_params = list(...)
  )
  
  if (verbose) {
    log_execution_plan(execution_plan)
  }
  
  return(execution_plan)
}


# ============================================================================
# STRATEGY FACTORY
# ============================================================================

#' Create FB4 strategy based on method
#'
#' @description
#' Factory function that creates the appropriate strategy instance based on
#' the specified method. Handles strategy instantiation and initial setup.
#' Validates concordance between fit_to and strategy parameters.
#'
#' @param execution_plan Complete execution plan with method specified
#'
#' @return Strategy object implementing FB4Strategy interface
#' @keywords internal
create_fb4_strategy <- function(execution_plan) {
  
  method <- execution_plan$strategy
  fit_to <- execution_plan$fit_to

  # Normalise "direct" shorthand: pull p_value / fit_value from additional_params
  # so that create_direct_strategy can find it at plan$fit_value
  if (method == "direct") {
    p_val <- execution_plan$additional_params$p_value %||%
             execution_plan$additional_params$fit_value %||%
             execution_plan$fit_value
    execution_plan$fit_value <- p_val
    method <- "direct_p_value"
    execution_plan$strategy <- method
  }

  # Validate concordance between fit_to and strategy
  validate_fit_to_strategy_concordance(fit_to, method)

  strategy <- switch(method,
                    "binary_search" = create_binary_search_strategy(execution_plan),
                    "optim" = create_optim_strategy(execution_plan),
                    "mle" = create_mle_strategy(execution_plan),
                    "bootstrap" = create_bootstrap_strategy(execution_plan),
                    "hierarchical" = create_hierarchical_strategy(execution_plan),
                    "direct_p_value" = create_direct_strategy(execution_plan, "p_value"),
                    "direct_ration_percent" = create_direct_strategy(execution_plan, "ration_percent"),
                    "direct_ration_grams" = create_direct_strategy(execution_plan, "ration_grams"),
                    stop("Unknown method: ", method)
  )
  
  # Validate the created strategy
  strategy$validate_plan(execution_plan)
  
  return(strategy)
}

#' Validate concordance between fit_to and strategy parameters
#'
#' @description
#' Ensures that the fit_to parameter matches the expected strategy method.
#' Throws an error if there's a mismatch.
#'
#' @param fit_to The target fitting parameter
#' @param strategy The strategy method to be used
#'
#' @keywords internal
validate_fit_to_strategy_concordance <- function(fit_to, strategy) {

  # NULL fit_to is valid for direct/optim strategies — nothing to validate
  if (is.null(fit_to)) return(invisible(NULL))

  # Define expected concordances
  expected_concordances <- list(
    "p_value" = "direct_p_value",
    "Ration" = "direct_ration_percent",
    "Ration_prey" = "direct_ration_grams"
  )

  # Check if fit_to requires specific strategy
  if (isTRUE(fit_to %in% names(expected_concordances))) {
    expected_strategy <- expected_concordances[[fit_to]]
    
    if (strategy != expected_strategy) {
      stop("Strategy mismatch: fit_to '", fit_to, "' requires strategy '", 
           expected_strategy, "', but '", strategy, "' was provided.")
    }
  }
  
  # Additional check: ensure direct strategies have correct fit_to
  direct_strategies <- c("direct_p_value", "direct_ration_percent", "direct_ration_grams")
  
  if (strategy %in% direct_strategies) {
    valid_fit_to <- switch(strategy,
                          "direct_p_value" = "p_value",
                          "direct_ration_percent" = "Ration",
                          "direct_ration_grams" = "Ration_prey"
    )
    
    if (fit_to != valid_fit_to) {
      stop("Strategy '", strategy, "' requires fit_to '", valid_fit_to, 
           "', but '", fit_to, "' was provided.")
    }
  }
}


# ============================================================================
# HELPER FUNCTIONS: Method Detection and Validation
# ============================================================================

#' Log execution plan details
#' @keywords internal
log_execution_plan <- function(plan) {

 message("=== FB4 Execution Plan ===")
 message("Method: ", plan$strategy)
 message("Backend: ", plan$backend)
 message("Fit to: ", plan$fit_to)

 if (!is.null(plan$observed_weights)) {
   if (is.data.frame(plan$observed_weights)) {
     # Hierarchical data
     n_individuals <- nrow(plan$observed_weights)
     weight_cols <- grep("^observed_weight", names(plan$observed_weights), value = TRUE)
     message("Hierarchical data: ", n_individuals, " individuals, ",
             length(weight_cols), " observation(s) per individual")
   } else if (is.numeric(plan$observed_weights)) {
     # Traditional vector
     message("Observed weights: n=", length(plan$observed_weights),
             ", range=", round(min(plan$observed_weights), 1), "-",
             round(max(plan$observed_weights), 1), "g")
   } else {
     message("Observed weights: ", typeof(plan$observed_weights), " format")
   }
 } else {
   message("Target value: ", plan$fit_value)
 }
 
 # Covariates information
 if (!is.null(plan$covariates)) {
   if (is.character(plan$covariates)) {
     message("Covariates: ", paste(plan$covariates, collapse = ", "))
   } else {
     message("Covariates: ", ncol(plan$covariates), " variable(s)")
   }
 }

 message("Initial weight: ", plan$initial_weight, "g")
 message("Simulation days: ", plan$simulation_days, " (", plan$first_day, "-", plan$last_day, ")")
 message("========================")
}
