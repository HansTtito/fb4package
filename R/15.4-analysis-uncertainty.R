#' FB4 Uncertainty Propagation Functions
#'
#' @description
#' Functions for propagating parameter uncertainty to consumption predictions
#' using both delta method (analytical) and bootstrap (simulation-based) approaches.
#'
#' @name uncertainty-prediction
#' @aliases uncertainty-prediction
NULL

# ============================================================================
# DELTA METHOD FOR UNCERTAINTY PROPAGATION
# ============================================================================

#' Delta method for consumption uncertainty propagation
#'
#' @description
#' Propagates p-value uncertainty to consumption predictions using the delta method.
#' Computes numerical derivatives and applies first-order approximation for 
#' uncertainty propagation. Suitable when the relationship between p and consumption
#' is approximately linear.
#'
#' @param p_est Estimated p-value (feeding level parameter)
#' @param p_se Standard error of p-value estimate
#' @param bio_obj Bioenergetic object with simulation settings and environmental data
#' @param delta_size Small increment for numerical derivative computation, default 0.001
#' @param first_day First simulation day, default 1
#' @param last_day Last simulation day, default 365
#' @param verbose Show progress messages, default FALSE
#'
#' @details
#' The delta method uses first-order Taylor series approximation:
#' Var(f(X)) ≈ [f'(μ)]² × Var(X)
#' 
#' The linearity check verifies that the derivative times delta_size is small
#' relative to the consumption estimate, indicating local linearity.
#'
#' @examples
#' \dontrun{
#' # Propagate uncertainty from MLE estimate
#' mle_result <- run_fb4(bio_obj, strategy = "mle", observed_weights = weights)
#' p_est <- mle_result$summary$p_estimate
#' p_se <- mle_result$method_data$confidence_intervals$p_se
#' 
#' uncertainty_result <- predict_consumption_delta(
#'   p_est = p_est,
#'   p_se = p_se,
#'   bio_obj = bio_obj      
#' )
#' }
#'
#' @export
predict_consumption_delta <- function(p_est, p_se, bio_obj, delta_size = 0.001,
                                      first_day = 1, last_day = 365, 
                                      verbose = FALSE) {
  
  if (verbose) {
    message("Computing consumption uncertainty using delta method")
    message("p_value estimate: ", round(p_est, 4), " ± ", round(p_se, 4))
  }
  
  # Validation
  if (p_est <= 0 || p_est > 5) {
    stop("p_est must be between 0 and 5")
  }
  
  if (p_se <= 0) {
    stop("p_se must be positive")
  }
  
  if (delta_size <= 0 || delta_size >= 0.1) {
    stop("delta_size must be between 0 and 0.1")
  }
  
  # Function to compute consumption given p
  consumption_fn <- function(p) {
    if (p <= 0 || p > 5) {
      return(NA)  # Return NA for invalid p values
    }
    
    tryCatch({
      result <- run_fb4(
        x = bio_obj,
        first_day = first_day,
        last_day = last_day,
        fit_to = "p_value",
        fit_value = p,
        strategy = "direct_p_value",
        verbose = FALSE
      )
      return(result$summary$total_consumption_g)
    }, error = function(e) {
      if (verbose) {
        warning("Simulation failed for p = ", p, ": ", e$message)
      }
      return(NA)
    })
  }
  
  # Compute central consumption
  consumption_est <- consumption_fn(p_est)
  
  if (is.na(consumption_est)) {
    stop("Failed to compute consumption at p_est = ", p_est)
  }
  
  # Compute numerical derivative: ∂consumption/∂p
  consumption_plus <- consumption_fn(p_est + delta_size)
  consumption_minus <- consumption_fn(p_est - delta_size)
  
  if (is.na(consumption_plus) || is.na(consumption_minus)) {
    stop("Failed to compute numerical derivative. Try smaller delta_size.")
  }
  
  derivative <- (consumption_plus - consumption_minus) / (2 * delta_size)
  
  # Delta method: Var(consumption) ≈ (∂consumption/∂p)² × Var(p)
  consumption_var <- derivative^2 * p_se^2
  consumption_se <- sqrt(consumption_var)
  
  # Assuming normality for confidence intervals
  z_score <- qnorm(0.975)  # 95% CI
  
  # Linearity check: derivative * delta should be small relative to consumption
  linearity_check <- abs(derivative * delta_size) < 0.01 * consumption_est
  
  if (verbose) {
    message("Delta method completed")
    message("Consumption estimate: ", round(consumption_est, 2), " ± ", round(consumption_se, 2), " g")
    message("95% CI: [", round(consumption_est - z_score * consumption_se, 2), ", ",
            round(consumption_est + z_score * consumption_se, 2), "] g")
    message("Derivative: ", round(derivative, 4))
    message("Linearity assumption valid: ", linearity_check)
  }
  
  if (!linearity_check && verbose) {
    warning("Linearity assumption may be violated. Consider bootstrap method for more accurate results.")
  }
  
  return(list(
    method = "delta",
    consumption_est = consumption_est,
    consumption_se = consumption_se,
    consumption_ci = c(
      consumption_est - z_score * consumption_se,
      consumption_est + z_score * consumption_se
    ),
    derivative = derivative,
    linearity_check = linearity_check,
    p_est = p_est,
    p_se = p_se
  ))
}

# ============================================================================
# BOOTSTRAP METHOD FOR UNCERTAINTY PROPAGATION
# ============================================================================

#' Bootstrap method for consumption uncertainty propagation
#'
#' @description
#' Propagates p-value uncertainty to consumption predictions using parametric bootstrap.
#' Generates multiple samples from the p-value distribution and runs FB4 simulations
#' for each sample. Provides full uncertainty distribution without linearity assumptions.
#' Supports parallel processing for improved performance.
#'
#' @param p_mean Mean of p-value distribution
#' @param p_sd Standard deviation of p-value distribution  
#' @param bio_obj Bioenergetic object with simulation settings and environmental data
#' @param n_sims Number of bootstrap simulations, default 1000
#' @param first_day First simulation day, default 1
#' @param last_day Last simulation day, default 365
#' @param parallel Use parallel processing, default FALSE
#' @param n_cores Number of cores for parallel processing (NULL = auto-detect), default NULL
#' @param confidence_level Confidence level for intervals, default 0.95
#' @param verbose Show progress messages, default FALSE
#' 
#' @details
#' The bootstrap method:
#' 1. Samples p-values from Normal(p_mean, p_sd)
#' 2. Constrains samples to valid range [0.01, 5.0]
#' 3. Runs FB4 simulation for each p-value sample
#' 4. Summarizes consumption distribution
#' 
#' Parallel processing can significantly reduce computation time for large n_sims.
#' The method handles simulation failures gracefully and reports success rates.
#'
#' @examples
#' \dontrun{
#' # Bootstrap uncertainty propagation
#' uncertainty_result <- predict_consumption_bootstrap(
#'   p_mean = 0.8,
#'   p_sd = 0.15,
#'   bio_obj = bio_obj,
#'   n_sims = 1000,
#'   parallel = TRUE
#' )
#' 
#' # Compare with delta method
#' delta_result <- predict_consumption_delta(0.8, 0.15, bio_obj)
#' }
#'
#' @export
predict_consumption_bootstrap <- function(p_mean, p_sd, bio_obj, n_sims = 1000,
                                         first_day = 1, last_day = 365,
                                         parallel = FALSE, n_cores = NULL, 
                                         confidence_level = 0.95, verbose = FALSE) {
  
  if (verbose) {
    message("Starting bootstrap uncertainty propagation")
    message("p_value: ", round(p_mean, 4), " ± ", round(p_sd, 4))
    message("Bootstrap simulations: ", n_sims)
    if (parallel) {
      message("Parallel processing enabled")
    }
  }
  
  # Validation
  if (p_mean <= 0 || p_mean > 5) {
    stop("p_mean must be between 0 and 5")
  }
  
  if (p_sd <= 0) {
    stop("p_sd must be positive")
  }
  
  if (n_sims < 10) {
    stop("n_sims must be at least 10")
  }
  
  if (confidence_level <= 0 || confidence_level >= 1) {
    stop("confidence_level must be between 0 and 1")
  }
  
  start_time <- proc.time()
  
  # Generate p-value samples
  p_samples <- rnorm(n_sims, p_mean, p_sd)
  p_samples <- pmax(0.01, pmin(5.0, p_samples))  # Constrain to valid range
  
  if (verbose) {
    message("p_value sample range: ", round(min(p_samples), 3), " - ", round(max(p_samples), 3))
  }
  
  # Setup parallel processing if requested
  if (parallel) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      warning("parallel package not available, falling back to sequential processing")
      parallel <- FALSE
    } else {
      if (is.null(n_cores)) {
        n_cores <- parallel::detectCores() - 1
      }
      n_cores <- min(n_cores, n_sims, parallel::detectCores() - 1)
      
      if (verbose) {
        message("Using ", n_cores, " cores for parallel processing")
      }
    }
  }
  
  # Define simulation function for parallel processing
  simulate_consumption_chunk <- function(p_vals) {
    results <- vector("numeric", length(p_vals))
    
    for (i in seq_along(p_vals)) {
      tryCatch({
        result <- run_fb4(
          x = bio_obj,
          fit_to = "p_value",
          fit_value = p_vals[i],
          first_day = first_day,
          last_day = last_day,
          strategy = "direct_p_value",
          verbose = FALSE
        )
        results[i] <- result$summary$total_consumption_g
      }, error = function(e) {
        results[i] <- NA
      })
    }
    
    return(results)
  }
  
  # Run simulations (parallel or sequential)
  if (parallel) {
    # Split work across cores
    chunk_size <- ceiling(n_sims / n_cores)
    p_chunks <- split(p_samples, ceiling(seq_along(p_samples) / chunk_size))
    
    # Setup cluster
    cl <- parallel::makeCluster(n_cores)
    
    # Export necessary objects
    parallel::clusterEvalQ(cl, library(fb4package))
    parallel::clusterExport(cl, c("bio_obj", "first_day", "last_day"), 
                           envir = environment())
    
    tryCatch({
      # Run parallel simulations
      chunk_results <- parallel::parLapply(cl, p_chunks, simulate_consumption_chunk)
      consumption_samples <- unlist(chunk_results)
    }, finally = {
      parallel::stopCluster(cl)
    })
    
  } else {
    # Sequential processing with progress
    consumption_samples <- numeric(n_sims)
    
    for (i in 1:n_sims) {
      if (verbose && i %% 100 == 0) {
        message("Progress: ", i, "/", n_sims, " (", round(100 * i / n_sims, 1), "%)")
      }
      
      tryCatch({
        result <- run_fb4(
          x = bio_obj,
          fit_to = "p_value",
          fit_value = p_samples[i],
          first_day = first_day,
          last_day = last_day,
          strategy = "direct_p_value",
          verbose = FALSE
        )
        consumption_samples[i] <- result$summary$total_consumption_g
      }, error = function(e) {
        consumption_samples[i] <- NA
        if (verbose) {
          warning("Simulation ", i, " failed for p = ", round(p_samples[i], 4), ": ", e$message)
        }
      })
    }
  }
  
  # Remove failed simulations
  successful_idx <- !is.na(consumption_samples)
  consumption_successful <- consumption_samples[successful_idx]
  p_successful <- p_samples[successful_idx]
  
  n_successful <- length(consumption_successful)
  success_rate <- n_successful / n_sims
  
  if (n_successful < 10) {
    stop("Too few successful simulations (", n_successful, "). Check input parameters and bio_obj.")
  }
  
  if (success_rate < 0.8 && verbose) {
    warning("Low success rate (", round(success_rate * 100, 1), "%). Consider checking p_value range.")
  }
  
  # Calculate summary statistics
  consumption_mean <- mean(consumption_successful)
  consumption_sd <- sd(consumption_successful)
  consumption_median <- median(consumption_successful)
  
  # Calculate confidence intervals
  alpha <- 1 - confidence_level
  ci_probs <- c(alpha/2, 1 - alpha/2)
  consumption_ci <- quantile(consumption_successful, ci_probs)
  
  elapsed_time <- proc.time() - start_time
  
  if (verbose) {
    message("Bootstrap completed in ", round(elapsed_time[3], 2), " seconds")
    message("Successful simulations: ", n_successful, "/", n_sims, " (", round(success_rate * 100, 1), "%)")
    message("Consumption: ", round(consumption_mean, 2), " ± ", round(consumption_sd, 2), " g")
    message(confidence_level * 100, "% CI: [", round(consumption_ci[1], 2), ", ", 
            round(consumption_ci[2], 2), "] g")
    
    if (parallel) {
      estimated_sequential_time <- elapsed_time[3] * n_cores
      speedup <- estimated_sequential_time / elapsed_time[3]
      message("Estimated speedup: ", round(speedup, 1), "x")
    }
  }
  
  return(list(
    method = "bootstrap",
    consumption_mean = consumption_mean,
    consumption_sd = consumption_sd,
    consumption_median = consumption_median,
    consumption_ci = consumption_ci,
    consumption_samples = as.numeric(consumption_successful),
    p_samples = p_successful,
    n_successful = n_successful,
    success_rate = success_rate,
    parallel_used = parallel,
    n_cores_used = if (parallel) n_cores else NULL,
    execution_time = elapsed_time[3],
    confidence_level = confidence_level,
    p_mean = p_mean,
    p_sd = p_sd
  ))
}
