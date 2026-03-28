#' FB4 Main Orchestrator
#'
#' @name run-fb4-orchestrator
#' @aliases run-fb4-orchestrator
NULL

# ============================================================================
# MAIN ORCHESTRATOR FUNCTION
# ============================================================================


#' Run FB4 simulation on Bioenergetic object (REFACTORED)
#'
#' @description
#' S3 method with automatic backend selection and bootstrap estimation.
#' Supports traditional optimization methods, MLE approaches, and new bootstrap
#' estimation for final weight data. This is the main entry point that coordinates
#' all FB4 execution strategies.
#'
#' @param x Bioenergetic object with all model components
#' @param fit_to Target type: "Weight", "Consumption", "p_value", "Ration", "Ration_prey"
#' @param fit_value Target value for deterministic approach
#' @param observed_weights Vector of observed final weights for MLE or bootstrap approaches (optional)
#' @param covariates Optional covariate matrix or data frame
#' @param first_day First simulation day, default 1
#' @param last_day Last simulation day (auto-detected if NULL)
#' @param backend Backend selection: "r" (pure R) or "tmb" (C++ via TMB, faster MLE)
#' @param strategy Fitting strategy: "binary_search" (default), "direct", "optim",
#'   "mle" (maximum likelihood), or "bootstrap" (bootstrap estimation)
#' @param oxycal Oxycalorific coefficient (J/g O2), default 13560
#' @param tolerance Convergence tolerance for iterative fitting, default 0.001
#' @param max_iterations Maximum iterations for binary search, default 25
#' @param lower Lower bound for p_value search (proportion of Cmax), default 0.01
#' @param upper Upper bound for p_value search (proportion of Cmax). Biologically,
#'   p = 1.0 is maximum ration; values > 1.0 are super-maximal. Default 1.0 for
#'   bootstrap, 5.0 for binary_search.
#' @param verbose Whether to show progress messages, default FALSE
#' @param optim_method If using optim, which method: "Brent", "L-BFGS-B", etc.
#' @param confidence_level Confidence level for MLE/bootstrap intervals, default 0.95
#' @param estimate_sigma Whether to estimate measurement error in MLE, default TRUE
#' @param compute_profile Whether to compute likelihood profile for MLE, default FALSE
#' @param profile_grid_size Number of points in profile grid for MLE, default 50
#' @param hessian Whether to compute Hessian for standard errors, default FALSE
#' @param n_bootstrap Number of bootstrap iterations, default 1000
#' @param parallel Whether to use parallel processing for bootstrap, default FALSE
#' @param n_cores Number of cores for parallel processing (NULL = auto-detect)
#' @param sample_size Sample size for each bootstrap iteration (NULL = same as original)
#' @param compute_percentiles Whether to compute additional percentiles for bootstrap, default TRUE
#' @param ... Additional arguments passed to strategy-specific functions
#'   (e.g., \code{store_predicted_weights_boot} for bootstrap)
#'
#' @return An object of class \code{fb4_result} containing:
#'   \describe{
#'     \item{\code{summary}}{Named list with \code{method}, \code{p_estimate},
#'       \code{final_weight}, \code{total_consumption}, and method-specific fields
#'       (\code{p_mean}, \code{p_sd}, confidence intervals, etc.)}
#'     \item{\code{daily_output}}{Data frame with one row per simulation day:
#'       \code{Day}, \code{Weight}, \code{Consumption_energy}, \code{Respiration},
#'       \code{Egestion}, \code{Excretion}, \code{SDA}, \code{Net_energy}, etc.}
#'     \item{\code{method_data}}{Method-specific data (bootstrap distributions,
#'       MLE likelihood profile, etc.)}
#'   }
#' @export
run_fb4.Bioenergetic <- function(x, 
                                 fit_to = NULL, 
                                 fit_value = NULL, 
                                 observed_weights = NULL,
                                 covariates = NULL,
                                 first_day = 1, 
                                 last_day = NULL,
                                 backend = "r",        
                                 strategy = "binary_search",   
                                 oxycal = 13560, 
                                 tolerance = 0.001, 
                                 max_iterations = 25, 
                                 optim_method = "Brent", 
                                 lower = 0.01, 
                                 upper = 5, 
                                 hessian = FALSE,
                                 verbose = FALSE,
                                 confidence_level = 0.95, 
                                 estimate_sigma = TRUE,
                                 compute_profile = FALSE,
                                 profile_grid_size = 50,
                                 n_bootstrap = 1000,
                                 parallel = FALSE,
                                 n_cores = NULL,
                                 sample_size = NULL,
                                 compute_percentiles = TRUE,
                                 ...) {
  
  # ============================================================================
  # BASIC VALIDATION
  # ============================================================================

  if (!is.Bioenergetic(x)) {
    stop("x must be an object of class 'Bioenergetic'")
  }

  # ============================================================================
  # BASIC VALIDATION
  # ============================================================================
  
  if (is.null(last_day)) {
    # Infer last_day from the bio object: use simulation duration if set,
    # otherwise use the maximum day in the temperature data, fallback to 365.
    if (!is.null(x$simulation_settings$duration)) {
      last_day <- x$simulation_settings$duration
    } else if (!is.null(x$environmental_data$temperature)) {
      last_day <- max(x$environmental_data$temperature$Day)
    } else {
      last_day <- 365
    }
  }

  validate_fb4_inputs(x, strategy, fit_to, fit_value, first_day, last_day, observed_weights, covariates)

  backend <- validate_backend_compatibility(strategy, backend, verbose)

  # ============================================================================
  # EXECUTION PLANNING
  # ============================================================================
  
  start_time <- proc.time()
  
  if (verbose) {
    message("=== Starting FB4 Execution ===")
  }
  
  # Create comprehensive execution plan
  execution_plan <- create_execution_plan(
    bio_obj = x,
    fit_to = fit_to,
    fit_value = fit_value,
    observed_weights = observed_weights,
    covariates = covariates,
    strategy = strategy,
    backend = backend,
    first_day = first_day,
    last_day = last_day,
    oxycal = oxycal,
    tolerance = tolerance,
    max_iterations = max_iterations,
    verbose = verbose,
    # Pass all additional parameters
    optim_method = optim_method,
    lower = lower,
    upper = upper,
    hessian = hessian,
    confidence_level = confidence_level,
    estimate_sigma = estimate_sigma,
    compute_profile = compute_profile,
    profile_grid_size = profile_grid_size,
    n_bootstrap = n_bootstrap,
    parallel = parallel,
    n_cores = n_cores,
    sample_size = sample_size,
    compute_percentiles = compute_percentiles,
    ...
  )
  
  # ============================================================================
  # STRATEGY EXECUTION
  # ============================================================================
  
  if (verbose) {
    message("Creating execution strategy...")
  }
  
  # Create appropriate strategy
  strategy_instance <- create_fb4_strategy(execution_plan)
  
  if (verbose) {
    strategy_info <- strategy_instance$get_strategy_info()
    message("Using strategy: ", strategy_info$name)
  }
  
  # Execute the strategy
  raw_results <- strategy_instance$execute(execution_plan)
  
  # ============================================================================
  # RESULT BUILDING
  # ============================================================================
  
  elapsed_time <- proc.time() - start_time
  
  if (verbose) {
    message("Building results...")
  }
  
  # Build final result object
  final_result <- build_fb4_result_unified(raw_results, execution_plan, elapsed_time[3])
  
  # Validate result structure
  validate_fb4_result(final_result)
  
  # ============================================================================
  # FINAL REPORTING
  # ============================================================================
  
  if (verbose) {
    message("=== Execution Summary ===")
    summary_lines <- create_execution_summary(final_result, execution_plan, elapsed_time[3])
    for (line in summary_lines) {
      message(line)
    }
    message("========================")
  }
  
  return(final_result)
}


# ============================================================================
# GENERIC FUNCTIONS
# ============================================================================


#' Default run_fb4 method
#'
#' @description
#' Generic function that dispatches to appropriate S3 method.
#'
#' @param x Object to run FB4 simulation on
#' @param ... Arguments passed to methods
#' @export
run_fb4 <- function(x, ...) {
  UseMethod("run_fb4")
}

#' Default method for run_fb4 (error)
#'
#' @description
#' Default method that provides helpful error message.
#'
#' @param x Object of unsupported class
#' @param ... Additional arguments
#' @export
run_fb4.default <- function(x, ...) {
  stop("run_fb4() is only supported for objects of class 'Bioenergetic'. ",
       "Use Bioenergetic() to create a proper Bioenergetic object.")
}


