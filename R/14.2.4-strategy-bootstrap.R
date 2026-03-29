# ============================================================================
# BOOTSTRAP STRATEGY (USING SHARED COMMONS)
# ============================================================================

#' Create Bootstrap Strategy
#' @param execution_plan Execution plan with parameters
#' @return Strategy object implementing FB4Strategy interface
#' @keywords internal
create_bootstrap_strategy <- function(execution_plan) {
  
  strategy <- list(
    
    execute = function(plan) {
      
      if (plan$verbose) {
        message("Executing bootstrap strategy")
      }
      
      # Use shared data preparation
      processed_data <- prepare_simulation_data(
        bio_obj = plan$bio_obj,
        strategy = "bootstrap",
        first_day = plan$first_day,
        last_day = plan$last_day,
        observed_weights = plan$observed_weights,
        output_format = "simulation"
      )
      
      # Extract bootstrap-specific parameters using shared function
      params <- extract_strategy_parameters(
        execution_plan = plan,
        required_params = c("n_bootstrap", "confidence_level"),
        default_values = list(
          n_bootstrap                  = 1000,
          confidence_level             = 0.95,
          parallel                     = FALSE,
          n_cores                      = NULL,
          sample_size                  = NULL,
          compute_percentiles          = TRUE,
          store_predicted_weights_boot = TRUE,
          lower                        = 0.01,
          upper                        = 1.0   # biologically: p > 1 is already super-maximal
        )
      )

      # Execute bootstrap fitting
      result <- fit_fb4_bootstrap(
        final_weights             = plan$observed_weights,
        processed_simulation_data = processed_data,
        n_bootstrap               = params$n_bootstrap,
        confidence_level          = params$confidence_level,
        oxycal                    = params$oxycal,
        sample_size               = params$sample_size,
        compute_percentiles       = params$compute_percentiles,
        parallel                  = params$parallel,
        n_cores                   = params$n_cores,
        upper_p                   = params$upper,
        verbose                   = params$verbose,
        store_predicted_weights   = params$store_predicted_weights_boot
      )
      
      # Add strategy metadata using shared function
      strategy_info <- list(
        strategy_type = "bootstrap",
        additional_metadata = list(
          n_bootstrap = params$n_bootstrap,
          parallel = params$parallel,
          n_cores = params$n_cores,
          confidence_level = params$confidence_level
        )
      )
      
      result <- add_strategy_metadata(result, strategy_info, plan)
      
      return(result)
    },
    
    validate_plan = function(plan) {
      # Use shared validation
      validate_common_strategy_inputs(
        observed_weights = plan$observed_weights,
        strategy_type = "bootstrap"
      )
      
      # Bootstrap specific validation
      if (length(plan$observed_weights) < 5) {
        stop("Bootstrap strategy requires at least 5 observations")
      }
      
      if (plan$fit_to != "Weight") {
        stop("Bootstrap strategy currently only supports Weight fitting")
      }
      
      return(TRUE)
    },
    
    get_strategy_info = function() {
      return(list(
        name = "Bootstrap Estimation",
        type = "mle_fitting",
        supports_backends = "r",
        supports_fit_types = "Weight",
        description = "Bootstrap resampling for parameter uncertainty estimation"
      ))
    }
  )
  
  class(strategy) <- c("FB4BootstrapStrategy", "FB4Strategy")
  return(strategy)
}

# ============================================================================
# BOOTSTRAP ALGORITHMS
# ============================================================================

#' Execute a single bootstrap iteration
#'
#' @description
#' Internal helper: resamples weights, finds the p_value that achieves the
#' resampled mean weight via \code{optim_search_p_value}, then extracts
#' consumption and (optionally) the predicted final weight.
#'
#' Extracted to eliminate code duplication between the sequential and parallel
#' execution paths of \code{\link{bootstrap_p_values}}.
#'
#' @param final_weights           Observed final weights vector
#' @param n_sample                Sample size for bootstrap resampling
#' @param simulation_function     Closure accepting a p_value and returning the
#'   predicted final weight; used by \code{optim_search_p_value}
#' @param processed_simulation_data Processed simulation data list
#' @param oxycal                  Oxycalorific coefficient (J/g O2)
#' @param store_predicted_weights Logical; retrieve the predicted final weight?
#' @param upper_p                 Upper bound for p_value search, default 5.0
#'
#' @return Named list with fields \code{p_estimate}, \code{consumption_estimate},
#'   \code{predicted_weight}, \code{mean_fin}, and \code{success} (logical).
#' @keywords internal
bootstrap_single_iteration <- function(final_weights, n_sample, simulation_function,
                                       processed_simulation_data, oxycal,
                                       store_predicted_weights, upper_p = 1.0) {

  sample_final_w <- sample(final_weights, size = n_sample, replace = TRUE)
  mean_fin_w     <- mean(sample_final_w)

  optim_result <- optim_search_p_value(
    target_value        = mean_fin_w,
    fit_type            = "weight",
    simulation_function = simulation_function,
    method              = "Brent",
    lower               = 0.01,
    upper               = upper_p
  )

  # Return early if optim did not converge to a valid p_value
  if (!optim_result$converged || is.na(optim_result$p_value) ||
      optim_result$p_value <= 0 || optim_result$p_value > upper_p) {
    return(list(
      p_estimate           = NA_real_,
      consumption_estimate = NA_real_,
      predicted_weight     = NA_real_,
      mean_fin             = mean_fin_w,
      success              = FALSE
    ))
  }

  p_val <- optim_result$p_value

  consumption_estimate <- execute_simulation_with_method(
    method_type               = "p_value",
    method_value              = p_val,
    processed_simulation_data = processed_simulation_data,
    oxycal                    = oxycal,
    extract_metric            = "consumption",
    output_daily              = FALSE,
    verbose                   = FALSE
  )

  predicted_weight <- if (store_predicted_weights) {
    execute_simulation_with_method(
      method_type               = "p_value",
      method_value              = p_val,
      processed_simulation_data = processed_simulation_data,
      oxycal                    = oxycal,
      extract_metric            = "weight",
      output_daily              = FALSE,
      verbose                   = FALSE
    )
  } else {
    NA_real_
  }

  list(
    p_estimate           = p_val,
    consumption_estimate = consumption_estimate,
    predicted_weight     = predicted_weight,
    mean_fin             = mean_fin_w,
    success              = TRUE
  )
}


#' Bootstrap estimation of p_values with optional parallel processing
#'
#' @description
#' Estimates population p_value and consumption using bootstrap resampling of
#' final weights. Parallel execution is supported via the \pkg{future} /
#' \pkg{furrr} ecosystem when \code{parallel = TRUE}.
#'
#' Each iteration delegates to \code{\link{bootstrap_single_iteration}} so that
#' the sequential and parallel code paths share a single implementation.
#'
#' @param processed_simulation_data Complete processed simulation data (must
#'   contain \code{simulation_settings$initial_weight} and
#'   \code{simulation_settings$observed_weights})
#' @param n_bootstrap   Number of bootstrap iterations, default 1000
#' @param oxycal        Oxycalorific coefficient (J/g O2), default 13560
#' @param sample_size   Sample size per iteration; \code{NULL} = same as data
#' @param parallel      Logical; use parallel processing? default \code{FALSE}
#' @param n_cores       Number of cores for parallel processing (\code{NULL} =
#'   auto-detect via \code{future::availableCores()})
#' @param verbose       Logical; show progress messages? default \code{FALSE}
#' @param store_predicted_weights Logical; store predicted final weights?
#'   default \code{TRUE}
#'
#' @return Named list with:
#'   \describe{
#'     \item{p_values}{Valid p_value estimates (NAs removed)}
#'     \item{consumption_estimates}{Valid consumption estimates}
#'     \item{predicted_weights}{Valid predicted weights, or \code{NULL}}
#'     \item{success_rate, n_bootstrap, successful_iterations}{Diagnostics}
#'     \item{parallel_used, n_cores_used}{Execution metadata}
#'   }
#' @seealso \code{\link{bootstrap_single_iteration}}, \code{\link{fit_fb4_bootstrap}}
#' @keywords internal
bootstrap_p_values <- function(processed_simulation_data,
                               n_bootstrap = 1000, oxycal = 13560,
                               sample_size = NULL, parallel = FALSE,
                               n_cores = NULL, upper_p = 1.0, verbose = FALSE,
                               store_predicted_weights = TRUE) {

  # ---- Extract data --------------------------------------------------------
  initial_weight <- processed_simulation_data$simulation_settings$initial_weight
  final_weights  <- processed_simulation_data$simulation_settings$observed_weights

  # ---- Validate inputs -----------------------------------------------------
  if (length(final_weights) < 5) {
    stop("At least 5 observations required for final weights")
  }
  if (any(final_weights <= 0) || initial_weight <= 0) {
    stop("All weights must be positive")
  }
  if (initial_weight >= min(final_weights)) {
    warning(
      "Initial weight (", round(initial_weight, 1), "g) is not smaller than ",
      "minimum final weight (", round(min(final_weights), 1), "g). Check your data."
    )
  }

  n_sample <- ifelse(is.null(sample_size), length(final_weights), sample_size)

  # ---- Shared simulation closure (used inside optim) -----------------------
  # Captures processed_simulation_data and oxycal from this scope; serialized
  # correctly by future when parallel = TRUE.
  simulation_function <- function(p_val) {
    execute_simulation_with_method(
      method_type               = "p_value",
      method_value              = p_val,
      processed_simulation_data = processed_simulation_data,
      oxycal                    = oxycal,
      extract_metric            = "weight",
      output_daily              = FALSE,
      verbose                   = FALSE
    )
  }

  # ---- Parallel fallback check ---------------------------------------------
  if (parallel &&
      (!requireNamespace("future", quietly = TRUE) ||
       !requireNamespace("furrr",  quietly = TRUE))) {
    warning("future/furrr packages not available, falling back to sequential processing")
    parallel <- FALSE
  }

  # ---- Shared safe-wrapper (used in both paths) ----------------------------
  safe_iter <- function(b) {
    if (!parallel && verbose && b %% 100 == 0) {
      message("Bootstrap iteration ", b, "/", n_bootstrap,
              " (", round(100 * b / n_bootstrap, 1), "% complete)")
    }
    tryCatch(
      bootstrap_single_iteration(
        final_weights, n_sample, simulation_function,
        processed_simulation_data, oxycal, store_predicted_weights,
        upper_p = upper_p
      ),
      error = function(e) list(
        p_estimate           = NA_real_,
        consumption_estimate = NA_real_,
        predicted_weight     = NA_real_,
        mean_fin             = NA_real_,
        success              = FALSE
      )
    )
  }

  # ---- Execute bootstrap ---------------------------------------------------
  if (parallel) {
    if (is.null(n_cores)) {
      n_cores <- future::availableCores() - 1L
    }
    n_cores <- max(1L, min(as.integer(n_cores), future::availableCores() - 1L))

    if (verbose) {
      message("Starting parallel bootstrap (", n_cores, " cores, ",
              n_bootstrap, " iterations)")
    }

    future::plan(future::multisession, workers = n_cores)
    on.exit(future::plan(future::sequential), add = TRUE)

    # Export internal package functions explicitly so multisession workers can
    # resolve them. On Windows (and other non-forking backends), workers start
    # as fresh R processes: they load the package but only see exported symbols.
    # Non-exported helpers (bootstrap_single_iteration, execute_simulation_with_method,
    # optim_search_p_value, run_fb4_simulation) live in the package namespace and
    # are accessible via `:::`, but closures captured in this session resolve them
    # through the *current* environment chain, which the worker cannot reconstruct.
    # Passing them as explicit globals guarantees availability on every backend.
    parallel_globals <- list(
      bootstrap_single_iteration     = bootstrap_single_iteration,
      execute_simulation_with_method = execute_simulation_with_method,
      optim_search_p_value           = optim_search_p_value,
      run_fb4_simulation             = run_fb4_simulation,
      simulation_function            = simulation_function,
      final_weights                  = final_weights,
      n_sample                       = n_sample,
      processed_simulation_data      = processed_simulation_data,
      oxycal                         = oxycal,
      store_predicted_weights        = store_predicted_weights,
      upper_p                        = upper_p,
      n_bootstrap                    = n_bootstrap,
      parallel                       = parallel,
      verbose                        = verbose
    )

    raw_list <- tryCatch(
      furrr::future_map(
        seq_len(n_bootstrap), safe_iter,
        .options = furrr::furrr_options(
          seed    = TRUE,
          globals = parallel_globals
        )
      ),
      error = function(e) stop("Parallel bootstrap failed: ", e$message)
    )

  } else {
    if (verbose) {
      message("Starting sequential bootstrap (", n_bootstrap, " iterations)")
    }
    raw_list <- lapply(seq_len(n_bootstrap), safe_iter)
  }

  # ---- Aggregate results ---------------------------------------------------
  p_estimates           <- vapply(raw_list, `[[`, numeric(1), "p_estimate")
  consumption_estimates <- vapply(raw_list, `[[`, numeric(1), "consumption_estimate")
  mean_fin_w_boot       <- vapply(raw_list, `[[`, numeric(1), "mean_fin")
  predicted_weights_raw <- vapply(raw_list, `[[`, numeric(1), "predicted_weight")
  successful_iterations <- sum(vapply(raw_list, function(x) isTRUE(x$success), logical(1)))
  success_rate          <- successful_iterations / n_bootstrap

  valid_p           <- p_estimates[!is.na(p_estimates)]
  valid_consumption <- consumption_estimates[!is.na(consumption_estimates)]
  valid_predicted   <- if (store_predicted_weights) {
    predicted_weights_raw[!is.na(predicted_weights_raw)]
  } else {
    NULL
  }

  # ---- Verbose summary -----------------------------------------------------
  if (verbose) {
    message("Bootstrap completed \u2014 successful: ", successful_iterations, "/",
            n_bootstrap, " (", round(success_rate * 100, 1), "%)")
    if (success_rate < 0.5) {
      warning("Low success rate (", round(success_rate * 100, 1),
              "%). Consider checking FB4 parameters or weight ranges.")
    }
    if (store_predicted_weights && length(valid_predicted) > 0) {
      message("Predicted weights: ", round(min(valid_predicted), 1),
              " - ", round(max(valid_predicted), 1), "g",
              "  (mean: ", round(mean(valid_predicted), 1), "g |",
              " observed mean: ", round(mean(final_weights), 1), "g)")
    }
  }

  list(
    p_values                = valid_p,
    consumption_estimates   = valid_consumption,
    predicted_weights       = valid_predicted,
    success_rate            = success_rate,
    n_bootstrap             = n_bootstrap,
    successful_iterations   = successful_iterations,
    initial_weight          = initial_weight,
    mean_fin_w_range        = range(mean_fin_w_boot, na.rm = TRUE),
    parallel_used           = parallel,
    n_cores_used            = if (parallel) n_cores else NULL,
    store_predicted_weights = store_predicted_weights
  )
}


#' Fit FB4 model using bootstrap estimation with parallel option
#'
#' @description
#' Coordinates bootstrap fitting process for final weight data.
#' Now uses shared commons functions and run_final_simulation for consistency.
#'
#' @param final_weights Vector of final observed weights
#' @param processed_simulation_data Complete processed simulation data (contains initial_weight)
#' @param n_bootstrap Number of bootstrap iterations, default 1000
#' @param oxycal Oxycalorific coefficient (J/g O2), default 13560
#' @param confidence_level Confidence level for intervals, default 0.95
#' @param sample_size Sample size for each bootstrap iteration (NULL = same as original)
#' @param compute_percentiles Whether to compute additional percentiles, default TRUE
#' @param parallel Whether to use parallel processing, default FALSE
#' @param n_cores Number of cores for parallel processing (NULL = auto-detect)
#' @param verbose Whether to show progress messages, default FALSE
#' @param store_predicted_weights Whether to store predicted final weights, default TRUE
#'
#' @return List with bootstrap fitting results including consumption and predicted weights
#' @keywords internal
fit_fb4_bootstrap <- function(final_weights, processed_simulation_data,
                              n_bootstrap = 1000, oxycal = 13560,
                              confidence_level = 0.95, sample_size = NULL,
                              compute_percentiles = TRUE, parallel = FALSE,
                              n_cores = NULL, upper_p = 1.0, verbose = FALSE,
                              store_predicted_weights = TRUE) {
  
  # Extract initial weight from processed data
  initial_weight <- processed_simulation_data$simulation_settings$initial_weight
  final_weights <- processed_simulation_data$simulation_settings$observed_weights
  
  if (verbose) {
    message("Starting FB4 bootstrap estimation")
    message("Initial weight (from model): ", round(initial_weight, 1), "g")
    if (parallel) {
      message("Parallel processing enabled")
    }
    if (store_predicted_weights) {
      message("Storing predicted weights for diagnostics")
    }
  }
  
  # Record start time for performance measurement
  start_time <- proc.time()
  
  # Run bootstrap estimation
  bootstrap_p <- bootstrap_p_values(
    processed_simulation_data = processed_simulation_data,
    n_bootstrap             = n_bootstrap,
    oxycal                  = oxycal,
    sample_size             = sample_size,
    parallel                = parallel,
    n_cores                 = n_cores,
    upper_p                 = upper_p,
    verbose                 = verbose,
    store_predicted_weights = store_predicted_weights
  )
  
  # Calculate elapsed time
  elapsed_time <- proc.time() - start_time
  
  # Extract results from bootstrap list
  bootstrap_p_values <- bootstrap_p$p_values
  success_rate <- bootstrap_p$success_rate
  successful_iterations <- bootstrap_p$successful_iterations
  model_initial_weight <- bootstrap_p$initial_weight
  mean_fin_w_range <- bootstrap_p$mean_fin_w_range
  bootstrap_consumption <- bootstrap_p$consumption_estimates
  bootstrap_predicted_weights <- bootstrap_p$predicted_weights
  parallel_used <- bootstrap_p$parallel_used
  n_cores_used <- bootstrap_p$n_cores_used
  predicted_weights_stored <- bootstrap_p$store_predicted_weights
  
  # Calculate summary statistics for p_values
  if (length(bootstrap_p_values) > 0) {
    p_mean <- mean(bootstrap_p_values)
    p_sd <- sd(bootstrap_p_values)
    p_median <- median(bootstrap_p_values)
    
    # Confidence intervals for p_values
    alpha <- 1 - confidence_level
    ci_probs <- c(alpha/2, 1 - alpha/2)
    p_ci <- quantile(bootstrap_p_values, ci_probs)
    
    # Additional percentiles for p_values if requested
    if (compute_percentiles) {
      p_percentiles <- quantile(bootstrap_p_values, c(0.05, 0.10, 0.25, 0.75, 0.90, 0.95))
    } else {
      p_percentiles <- NULL
    }
    
  } else {
    stop("No successful bootstrap iterations. Check your data and FB4 parameters.")
  }
  
  # Calculate summary statistics for consumption
  if (length(bootstrap_consumption) > 0) {
    consumption_mean <- mean(bootstrap_consumption)
    consumption_sd <- sd(bootstrap_consumption)
    consumption_median <- median(bootstrap_consumption)
    
    # Confidence intervals for consumption
    consumption_ci <- quantile(bootstrap_consumption, ci_probs)
    
    # Additional percentiles for consumption if requested
    if (compute_percentiles) {
      consumption_percentiles <- quantile(bootstrap_consumption, c(0.05, 0.10, 0.25, 0.75, 0.90, 0.95))
    } else {
      consumption_percentiles <- NULL
    }
    
  } else {
    consumption_mean <- consumption_sd <- consumption_median <- NA
    consumption_ci <- c(NA, NA)
    consumption_percentiles <- NULL
  }
  
  # Calculate summary statistics for predicted weights if available
  if (store_predicted_weights && length(bootstrap_predicted_weights) > 0) {
    predicted_weights_mean <- mean(bootstrap_predicted_weights)
    predicted_weights_sd <- sd(bootstrap_predicted_weights)
    predicted_weights_median <- median(bootstrap_predicted_weights)
    
    # Confidence intervals for predicted weights
    predicted_weights_ci <- quantile(bootstrap_predicted_weights, ci_probs)
    
    # Additional percentiles for predicted weights if requested
    if (compute_percentiles) {
      predicted_weights_percentiles <- quantile(bootstrap_predicted_weights, c(0.05, 0.10, 0.25, 0.75, 0.90, 0.95))
    } else {
      predicted_weights_percentiles <- NULL
    }
    
    # Model diagnostics: compare predicted vs observed
    observed_mean_fin_w <- mean(final_weights)
    prediction_bias <- predicted_weights_mean - observed_mean_fin_w
    prediction_bias_pct <- (prediction_bias / observed_mean_fin_w) * 100
    
  } else {
    predicted_weights_mean <- predicted_weights_sd <- predicted_weights_median <- NA
    predicted_weights_ci <- c(NA, NA)
    predicted_weights_percentiles <- NULL
    prediction_bias <- prediction_bias_pct <- NA
  }
  
  # Run final simulation with mean p_value using shared function
  final_simulation <- run_final_simulation(
    optimal_p_value = p_mean,
    processed_simulation_data = processed_simulation_data,
    oxycal = oxycal,
    verbose = verbose
  )
  
  # Calculate observed statistics
  observed_mean_fin_w <- mean(final_weights)
  observed_growth <- observed_mean_fin_w - initial_weight
  observed_growth_rate <- (observed_mean_fin_w / initial_weight) - 1
  
  if (verbose) {
    message("Bootstrap estimation completed in ", round(elapsed_time[3], 2), " seconds")
    message("Mean p_value: ", round(p_mean, 4), " \u00b1 ", round(p_sd, 4))
    message(confidence_level*100, "% CI: [", round(p_ci[1], 4), ", ", round(p_ci[2], 4), "]")
    message("Mean consumption: ", round(consumption_mean, 2), " \u00b1 ", round(consumption_sd, 2), "g")
    message("Consumption ", confidence_level*100, "% CI: [", round(consumption_ci[1], 2), ", ", round(consumption_ci[2], 2), "]g")
    
    if (store_predicted_weights && !is.na(predicted_weights_mean)) {
      message("Mean predicted weight: ", round(predicted_weights_mean, 1), " \u00b1 ", round(predicted_weights_sd, 1), "g")
      message("Predicted weights ", confidence_level*100, "% CI: [", round(predicted_weights_ci[1], 1), ", ", round(predicted_weights_ci[2], 1), "]g")
      message("Model bias: ", round(prediction_bias, 2), "g (", round(prediction_bias_pct, 1), "%)")
    }
    
    message("Observed mean growth: ", round(observed_growth, 1), "g (", 
            round(observed_growth_rate * 100, 1), "% increase)")
    
    if (parallel_used) {
      message("Parallel processing used ", n_cores_used, " cores")
      estimated_sequential_time <- elapsed_time[3] * n_cores_used
      speedup <- estimated_sequential_time / elapsed_time[3]
      message("Estimated speedup: ", round(speedup, 1), "x")
    }
  }
  
  # Prepare final result
  if (!is.null(final_simulation)) {
    return(list(
      # Bootstrap results for p_values
      p_mean = p_mean,
      p_sd = p_sd,
      p_median = p_median,
      p_ci_lower = p_ci[1],
      p_ci_upper = p_ci[2],
      bootstrap_p_values = bootstrap_p_values,
      p_percentiles = p_percentiles,
      
      # Bootstrap results for consumption
      consumption_mean = consumption_mean,
      consumption_sd = consumption_sd,
      consumption_median = consumption_median,
      consumption_ci_lower = consumption_ci[1],
      consumption_ci_upper = consumption_ci[2],
      bootstrap_consumption_values = bootstrap_consumption,
      consumption_percentiles = consumption_percentiles,
      
      # Bootstrap results for predicted weights
      predicted_weights_mean = predicted_weights_mean,
      predicted_weights_sd = predicted_weights_sd,
      predicted_weights_median = predicted_weights_median,
      predicted_weights_ci_lower = predicted_weights_ci[1],
      predicted_weights_ci_upper = predicted_weights_ci[2],
      bootstrap_predicted_weights = bootstrap_predicted_weights,
      predicted_weights_percentiles = predicted_weights_percentiles,
      
      # Model diagnostics
      model_diagnostics = list(
        prediction_bias = prediction_bias,
        prediction_bias_pct = prediction_bias_pct,
        predicted_weights_stored = predicted_weights_stored
      ),
      
      # Bootstrap diagnostics
      n_bootstrap = n_bootstrap,
      successful_iterations = successful_iterations,
      success_rate = success_rate,
      initial_weight = model_initial_weight,
      mean_fin_w_range = mean_fin_w_range,
      
      # Performance information
      execution_time = elapsed_time[3],
      parallel_used = parallel_used,
      n_cores_used = n_cores_used,
      
      # Observed data summary
      observed_data = list(
        initial_weight = initial_weight,
        fin_w_stats = list(
          mean = observed_mean_fin_w,
          sd = sd(final_weights),
          min = min(final_weights),
          max = max(final_weights),
          n = length(final_weights)
        ),
        final_weights = final_weights,
        growth_stats = list(
          mean_growth = observed_growth,
          growth_rate = observed_growth_rate
        )
      ),
      
      # Simulation results using mean p
      predicted_weight = final_simulation$final_weight,
      daily_output = final_simulation$daily_output,
      
      # Fitting information
      fit_info = list(
        method = "bootstrap",
        converged = TRUE,
        approach = "bootstrap_estimation"
      ),
      
      # Metadata
      model_info = list(
        version = "2.0.0",
        timestamp = Sys.time(),
        oxycal = oxycal,
        confidence_level = confidence_level,
        method = "bootstrap"
      )
    ))
  }
  
  # Return error result using shared function
  return(create_error_result(
    error_message = "Bootstrap estimation failed",
    strategy_type = "bootstrap",
    execution_plan = list(n_bootstrap = n_bootstrap, parallel = parallel)
  ))
}