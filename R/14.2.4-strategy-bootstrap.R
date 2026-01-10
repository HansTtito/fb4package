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
          n_bootstrap = 1000,
          confidence_level = 0.95,
          parallel = FALSE,
          n_cores = NULL,
          sample_size = NULL,
          compute_percentiles = TRUE,
          store_predicted_weights_boot = TRUE
        )
      )
      
      # Execute bootstrap fitting
      result <- fit_fb4_bootstrap(
        final_weights = plan$observed_weights,
        processed_simulation_data = processed_data,
        n_bootstrap = params$n_bootstrap,
        confidence_level = params$confidence_level,
        oxycal = params$oxycal,
        sample_size = params$sample_size,
        compute_percentiles = params$compute_percentiles,
        parallel = params$parallel,
        n_cores = params$n_cores,
        verbose = params$verbose,
        store_predicted_weights = params$store_predicted_weights_boot
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

#' Bootstrap estimation for p_values with parallel option (Low-level - OPTIMIZED)
#'
#' @description
#' Estimates population p_value and consumption using bootstrap resampling of final weights.
#' Now uses shared simulation execution functions for consistency and reduced duplication.
#'
#' @param processed_simulation_data Complete processed simulation data (contains initial_weight)
#' @param n_bootstrap Number of bootstrap iterations, default 1000
#' @param oxycal Oxycalorific coefficient (J/g O2), default 13560
#' @param sample_size Sample size for each bootstrap iteration (NULL = same as original)
#' @param parallel Whether to use parallel processing, default FALSE
#' @param n_cores Number of cores for parallel processing (NULL = auto-detect)
#' @param verbose Whether to show progress messages, default FALSE
#' @param store_predicted_weights Whether to store predicted final weights, default TRUE
#'
#' @return List with bootstrap results including p_values, consumption, and predicted weights
#' @keywords internal
bootstrap_p_values <- function(processed_simulation_data,
                               n_bootstrap = 1000, oxycal = 13560, 
                               sample_size = NULL, parallel = FALSE, 
                               n_cores = NULL, verbose = FALSE,
                               store_predicted_weights = TRUE) {
  
  # Extract initial weight from processed data
  initial_weight <- processed_simulation_data$simulation_settings$initial_weight
  final_weights <- processed_simulation_data$simulation_settings$observed_weights

  # Validate inputs
  if (length(final_weights) < 5) {
    stop("At least 5 observations required for final weights")
  }
  
  if (any(final_weights <= 0) || initial_weight <= 0) {
    stop("All weights must be positive")
  }
  
  if (initial_weight >= min(final_weights)) {
    warning("Initial weight (", round(initial_weight, 1), "g) is not smaller than minimum final weight (", 
            round(min(final_weights), 1), "g). Check your data.")
  }
  
  # Set sample size
  n_sample <- ifelse(is.null(sample_size), length(final_weights), sample_size)
  
  if (verbose) {
    message("Starting bootstrap estimation for p_values and consumption")
    message("Initial weight (from model): ", round(initial_weight, 1), "g")
    message("Final weights (n=", length(final_weights), "): ", 
            round(min(final_weights), 1), " - ", round(max(final_weights), 1), "g")
    message("Bootstrap iterations: ", n_bootstrap)
    message("Storing predicted weights: ", store_predicted_weights)
    
    if (parallel) {
      if (is.null(n_cores)) {
        n_cores <- future::availableCores() - 1
      }
      message("Using parallel processing with ", n_cores, " cores")
    }
  }
  
  # Create shared simulation function that will be used in bootstrap
  simulation_function <- function(p_val) {
    execute_simulation_with_method(
      method_type = "p_value",
      method_value = p_val,
      processed_simulation_data = processed_simulation_data,
      oxycal = oxycal,
      extract_metric = "weight",
      output_daily = FALSE,
      verbose = FALSE
    )
  }
  
  # Execute bootstrap: parallel or sequential
  if (parallel) {
    
    # Check if future package is available
    if (!requireNamespace("future", quietly = TRUE) || !requireNamespace("furrr", quietly = TRUE)) {
      warning("future/furrr packages not available, falling back to sequential processing")
      parallel <- FALSE
    } else {
      
      library(future)
      library(furrr)
      
      if (verbose) {
        message("Initiating parallel bootstrap processing...")
      }
      
      # Set up parallel processing
      if (is.null(n_cores)) {
        n_cores <- availableCores() - 1
      }
      
      # Ensure reasonable number of cores
      n_cores <- min(n_cores, n_bootstrap, availableCores() - 1)
      
      # Setup future plan
      plan(multisession, workers = n_cores)
      on.exit(plan(sequential), add = TRUE)
      
      # Define bootstrap worker function
      bootstrap_worker <- function(n_iter) {
        
        # Storage for this worker's results
        worker_p_estimates <- numeric(n_iter)
        worker_consumption_estimates <- numeric(n_iter)
        worker_predicted_weights <- if(store_predicted_weights) numeric(n_iter) else NULL
        worker_mean_fin <- numeric(n_iter)
        worker_success <- 0
        
        for (i in 1:n_iter) {
          tryCatch({
            # Resample final weights only
            sample_final_w <- sample(final_weights, size = n_sample, replace = TRUE)
            mean_fin_w <- mean(sample_final_w)
            
            worker_mean_fin[i] <- mean_fin_w
            
            # Solve for p_value that achieves growth from initial_weight to mean_fin_w
            optim_result <- optim_search_p_value(
              target_value = mean_fin_w,
              fit_type = "weight",
              simulation_function = simulation_function,
              method = "Brent",
              lower = 0.01,
              upper = 2
            )
            
            if (optim_result$converged && !is.na(optim_result$p_value) && 
                optim_result$p_value > 0 && optim_result$p_value <= 2) {
              
              worker_p_estimates[i] <- optim_result$p_value
              
              # Calculate consumption for this p_value using shared function
              consumption_result <- execute_simulation_with_method(
                method_type = "p_value",
                method_value = optim_result$p_value,
                processed_simulation_data = processed_simulation_data,
                oxycal = oxycal,
                extract_metric = "consumption",
                output_daily = FALSE,
                verbose = FALSE
              )
              
              worker_consumption_estimates[i] <- consumption_result
              
              # Store predicted final weight if requested
              if (store_predicted_weights) {
                predicted_weight <- execute_simulation_with_method(
                  method_type = "p_value",
                  method_value = optim_result$p_value,
                  processed_simulation_data = processed_simulation_data,
                  oxycal = oxycal,
                  extract_metric = "weight",
                  output_daily = FALSE,
                  verbose = FALSE
                )
                worker_predicted_weights[i] <- predicted_weight
              }
              
              worker_success <- worker_success + 1
              
            } else {
              worker_p_estimates[i] <- NA
              worker_consumption_estimates[i] <- NA
              if (store_predicted_weights) {
                worker_predicted_weights[i] <- NA
              }
            }
            
          }, error = function(e) {
            worker_p_estimates[i] <- NA
            worker_consumption_estimates[i] <- NA
            if (store_predicted_weights) {
              worker_predicted_weights[i] <- NA
            }
            worker_mean_fin[i] <- NA
          })
        }
        
        return(list(
          p_estimates = worker_p_estimates,
          consumption_estimates = worker_consumption_estimates,
          predicted_weights = worker_predicted_weights,
          mean_fin = worker_mean_fin,
          success_count = worker_success
        ))
      }
      
      # Distribute work across cores
      iterations_per_core <- rep(floor(n_bootstrap / n_cores), n_cores)
      remainder <- n_bootstrap %% n_cores
      if (remainder > 0) {
        iterations_per_core[1:remainder] <- iterations_per_core[1:remainder] + 1
      }
      
      # Execute parallel bootstrap using future
      tryCatch({
        worker_results <- future_map(iterations_per_core, bootstrap_worker)
        
        # Combine results from all workers
        p_estimates <- unlist(lapply(worker_results, function(x) x$p_estimates))
        consumption_estimates <- unlist(lapply(worker_results, function(x) x$consumption_estimates))
        predicted_weights <- if(store_predicted_weights) {
          unlist(lapply(worker_results, function(x) x$predicted_weights))
        } else {
          NULL
        }
        mean_fin_w_boot <- unlist(lapply(worker_results, function(x) x$mean_fin))
        successful_iterations <- sum(sapply(worker_results, function(x) x$success_count))
        
      }, error = function(e) {
        stop("Parallel bootstrap failed: ", e$message)
      })
      
      if (verbose) {
        message("Parallel bootstrap completed")
      }
    }
  }
  
  # Sequential processing (default or fallback)
  if (!parallel) {
    
    if (verbose) {
      message("Using sequential processing")
    }
    
    # Storage for results
    p_estimates <- numeric(n_bootstrap)
    consumption_estimates <- numeric(n_bootstrap)
    predicted_weights <- if(store_predicted_weights) numeric(n_bootstrap) else NULL
    mean_fin_w_boot <- numeric(n_bootstrap)
    
    # Progress tracking
    successful_iterations <- 0
    
    # Bootstrap loop
    for (b in 1:n_bootstrap) {
      
      # Progress reporting
      if (verbose && b %% 100 == 0) {
        message("Bootstrap iteration ", b, "/", n_bootstrap, 
                " (", round(100*b/n_bootstrap, 1), "% complete)")
      }
      
      # Resample final weights only
      sample_final_w <- sample(final_weights, size = n_sample, replace = TRUE)
      mean_fin_w <- mean(sample_final_w)
      
      # Store mean for diagnostics
      mean_fin_w_boot[b] <- mean_fin_w
      
      # Solve for p_value explaining growth from initial_weight to mean_fin_w
      tryCatch({
        
        # Solve for p_value using optim function from MLE strategy
        optim_result <- optim_search_p_value(
          target_value = mean_fin_w,
          fit_type = "weight",
          simulation_function = simulation_function,
          method = "Brent",
          lower = 0.01,
          upper = 5.0
        )
        
        if (optim_result$converged && !is.na(optim_result$p_value) && 
            optim_result$p_value > 0 && optim_result$p_value <= 5.0) {
          
          p_estimates[b] <- optim_result$p_value
          
          # Calculate consumption for this p_value using shared function
          consumption_result <- execute_simulation_with_method(
            method_type = "p_value",
            method_value = optim_result$p_value,
            processed_simulation_data = processed_simulation_data,
            oxycal = oxycal,
            extract_metric = "consumption",
            output_daily = FALSE,
            verbose = FALSE
          )
          
          consumption_estimates[b] <- consumption_result
          
          # Store predicted final weight if requested
          if (store_predicted_weights) {
            predicted_weight <- execute_simulation_with_method(
              method_type = "p_value",
              method_value = optim_result$p_value,
              processed_simulation_data = processed_simulation_data,
              oxycal = oxycal,
              extract_metric = "weight",
              output_daily = FALSE,
              verbose = FALSE
            )
            predicted_weights[b] <- predicted_weight
          }
          
          successful_iterations <- successful_iterations + 1
          
        } else {
          p_estimates[b] <- NA
          consumption_estimates[b] <- NA
          if (store_predicted_weights) {
            predicted_weights[b] <- NA
          }
        }
        
      }, error = function(e) {
        p_estimates[b] <- NA
        consumption_estimates[b] <- NA
        if (store_predicted_weights) {
          predicted_weights[b] <- NA
        }
      })
    }
  }
  
  # Calculate success rate
  success_rate <- successful_iterations / n_bootstrap
  
  if (verbose) {
    message("Bootstrap completed")
    message("Successful iterations: ", successful_iterations, "/", n_bootstrap, 
            " (", round(success_rate * 100, 1), "%)")
    
    if (success_rate < 0.5) {
      warning("Low success rate (", round(success_rate * 100, 1), 
              "%). Consider checking FB4 parameters or weight ranges.")
    }
    
    if (store_predicted_weights) {
      valid_predicted <- predicted_weights[!is.na(predicted_weights)]
      if (length(valid_predicted) > 0) {
        message("Predicted weights range: ", round(min(valid_predicted), 1), 
                " - ", round(max(valid_predicted), 1), "g")
        message("Mean predicted weight: ", round(mean(valid_predicted), 1), "g")
        message("Observed mean weight: ", round(mean(final_weights), 1), "g")
      }
    }
  }
  
  # Return results as structured list
  valid_p <- p_estimates[!is.na(p_estimates)]
  valid_consumption <- consumption_estimates[!is.na(consumption_estimates)]
  valid_predicted_weights <- if(store_predicted_weights) {
    predicted_weights[!is.na(predicted_weights)]
  } else {
    NULL
  }
  
  return(list(
    # Primary results
    p_values = valid_p,
    consumption_estimates = valid_consumption,
    predicted_weights = valid_predicted_weights,
    
    # Bootstrap diagnostics
    success_rate = success_rate,
    n_bootstrap = n_bootstrap,
    successful_iterations = successful_iterations,
    initial_weight = initial_weight,
    mean_fin_w_range = range(mean_fin_w_boot, na.rm = TRUE),
    
    # Processing information
    parallel_used = parallel,
    n_cores_used = if(parallel) n_cores else NULL,
    store_predicted_weights = store_predicted_weights
  ))
}


#' Fit FB4 model using bootstrap estimation with parallel option (Mid-level - OPTIMIZED)
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
                              n_cores = NULL, verbose = FALSE,
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
    n_bootstrap = n_bootstrap,
    oxycal = oxycal,
    sample_size = sample_size,
    parallel = parallel,
    n_cores = n_cores,
    verbose = verbose,
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
    message("Mean p_value: ", round(p_mean, 4), " ± ", round(p_sd, 4))
    message(confidence_level*100, "% CI: [", round(p_ci[1], 4), ", ", round(p_ci[2], 4), "]")
    message("Mean consumption: ", round(consumption_mean, 2), " ± ", round(consumption_sd, 2), "g")
    message("Consumption ", confidence_level*100, "% CI: [", round(consumption_ci[1], 2), ", ", round(consumption_ci[2], 2), "]g")
    
    if (store_predicted_weights && !is.na(predicted_weights_mean)) {
      message("Mean predicted weight: ", round(predicted_weights_mean, 1), " ± ", round(predicted_weights_sd, 1), "g")
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