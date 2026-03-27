#' Strategy-MLE for FB4 Model (UPDATED)
#'
#' @name strategy-mle
#' @aliases strategy-mle
NULL

# ============================================================================
# MLE STRATEGY (USING SHARED COMMONS)
# ============================================================================

#' Create MLE Strategy
#' @param execution_plan Execution plan with parameters
#' @return Strategy object implementing FB4Strategy interface
#' @keywords internal
create_mle_strategy <- function(execution_plan) {
  
  strategy <- list(
    
    execute = function(plan) {
      
      if (plan$verbose) {
        message("Executing MLE strategy")
      }
      
      # Route to TMB backend if specified
      if (plan$backend == "tmb") {
        return(execute_mle_tmb(plan))
      }
      
      # individual_data <- convert_to_individual_data(plan$observed_weights, plan$verbose)
      # R backend execution using shared data preparation
      processed_data <- prepare_simulation_data(
        bio_obj = plan$bio_obj,
        strategy = "mle",
        first_day = plan$first_day,
        last_day = plan$last_day,
        output_format = "simulation",
        observed_weights = plan$observed_weights
      )
      
      # Extract MLE-specific parameters using shared function
      params <- extract_strategy_parameters(
        execution_plan = plan,
        required_params = c("estimate_sigma", "confidence_level"),
        default_values = list(
          estimate_sigma = TRUE,
          confidence_level = 0.95,
          compute_profile = FALSE,
          profile_grid_size = 50
        )
      )
      
      # Execute MLE fitting
      result <- fit_fb4_mle(
        observed_weights = plan$observed_weights,
        processed_simulation_data = processed_data,
        estimate_sigma = params$estimate_sigma,
        oxycal = params$oxycal,
        confidence_level = params$confidence_level,
        compute_profile = params$compute_profile,
        profile_grid_size = params$profile_grid_size,
        verbose = params$verbose
      )
      
      # Add strategy metadata using shared function
      strategy_info <- list(
        strategy_type = "mle",
        additional_metadata = list(
          estimate_sigma = params$estimate_sigma,
          confidence_level = params$confidence_level,
          compute_profile = params$compute_profile
        )
      )
      
      result <- add_strategy_metadata(result, strategy_info, plan)
      
      return(result)
    },
    
    validate_plan = function(plan) {
      # Use shared validation
      validate_common_strategy_inputs(
        observed_weights = plan$observed_weights,
        strategy_type = "mle"
      )
      
      # MLE specific validation
      if (plan$fit_to != "Weight") {
        stop("MLE strategy currently only supports Weight fitting")
      }
      
      return(TRUE)
    },
    
    get_strategy_info = function() {
      return(list(
        name = "Maximum Likelihood Estimation",
        type = "mle_fitting",
        supports_backends = c("r", "tmb"),
        supports_fit_types = "Weight",
        description = "Statistical estimation using log-normal likelihood"
      ))
    }
  )
  
  class(strategy) <- c("FB4MLEStrategy", "FB4Strategy")
  return(strategy)
}

#' Execute MLE TMB using unified architecture
#' @keywords internal
execute_mle_tmb <- function(plan) {
 
 if (plan$verbose) {
   message("Using TMB backend for high-performance MLE estimation")
 }
 
 # Use shared data preparation for TMB format
 tmb_data <- prepare_simulation_data(
   bio_obj = plan$bio_obj,
   strategy = "mle",
   first_day = plan$first_day,
   last_day = plan$last_day,
   output_format = "tmb_basic",
   observed_weights = plan$observed_weights,
   oxycal = plan$oxycal,
   validate_inputs = FALSE
 )
 
 # Extract TMB-specific parameters using shared function
 params <- extract_strategy_parameters(
   execution_plan = plan,
   default_values = list(
     confidence_level = 0.95,
     compute_profile = FALSE,
     profile_grid_size = 50
   )
 )
 
 # Create TMB objective function
 initial_params <- list(
   log_p_value = log(1.0),
   log_sigma = log(0.1)
 )
 
 obj <- TMB::MakeADFun(
   data = tmb_data,
   parameters = initial_params,
   DLL = "fb4package",
   silent = !plan$verbose
 )
 
 # Optimize using 18-tmb-shared.R functions
 bounds <- list(
   lower = c(log(0.01), log(0.001)),
   upper = c(log(10.0), log(2.0))
 )
 
 opt_result <- run_robust_optimization(
   obj = obj,
   method = "nlminb",
   lower = bounds$lower,
   upper = bounds$upper,
   verbose = plan$verbose
 )
 
 # Extract results
 result <- extract_tmb_results(
   obj = obj,
   opt_result = opt_result,
   model_type = "basic",
   confidence_level = params$confidence_level,
   verbose = plan$verbose
 )
 
 # Generate daily output using standard simulation
 if (result$converged && !is.na(result$p_estimate)) {
   # Recreate standard format data from original bio_obj
    final_simulation <- run_fb4(
        x = plan$bio_obj,
        fit_to = "p_value",
        fit_value = result$p_estimate,
        strategy = "direct_p_value",
        first_day = plan$first_day,
        last_day = plan$last_day
    )
   
   if (!is.null(final_simulation)) {
     result$daily_output <- final_simulation$daily_output
   }
 }
 
 # Add strategy metadata using shared function
 strategy_info <- list(
   strategy_type = "mle",
   additional_metadata = list(
     backend = "tmb",
     confidence_level = params$confidence_level
   )
 )
 
 result <- add_strategy_metadata(result, strategy_info, plan)
 
 return(result)
}


# ============================================================================
# MLE ALGORITHMS (OPTIMIZED)
# ============================================================================

#' Calculate negative log-likelihood for log-normal weight observations (CORRECTED)
#'
#' @description
#' FIXED VERSION: Calculates the NEGATIVE log-likelihood for observed final weights
#' assuming they follow a log-normal distribution around the predicted weight.
#' This version is consistent with TMB backend.
#'
#' @param params Vector of parameters to estimate:
#'   - params[1]: p_value (feeding level, 0.01-5.0)
#'   - params[2]: log(sigma) (log of standard deviation of log-weights)
#' @param observed_weights Numeric vector of observed final weights (g)
#' @param simulation_function Function that takes p_value and returns predicted weight
#'
#' @return Negative log-likelihood value (scalar)
#' @keywords internal
neg_log_likelihood_lognormal <- function(params, observed_weights, simulation_function) {
  # Extract parameters
  p_value <- params[1]
  sigma <- exp(params[2])  # Use exp() to ensure sigma > 0
  
  # Boundary check for p_value
  if (p_value < 0.01 || p_value > 5.0) {
    return(1e6)  # Large penalty for out-of-bounds values
  }
  
  # Run simulation to get predicted weight using shared function
  sim_weight <- simulation_function(p_value)
  
  # Handle simulation failures
  if (is.null(sim_weight) || is.na(sim_weight) || sim_weight <= 0) {
    return(1e6)  # Large penalty for invalid predictions
  }
  
  # Calculate log-likelihood using log-normal distribution
  log_lik <- sum(dlnorm(observed_weights, 
                        meanlog = log(sim_weight), 
                        sdlog = sigma, 
                        log = TRUE))
  
  # This is consistent with TMB backend expectation
  return(-log_lik)  # Still negative for optim() minimization
}

#' Maximum Likelihood Estimation for p_value using log-normal distribution (CORRECTED)
#'
#' @description
#' FIXED VERSION: Estimates p_value and uncertainty using observed final weights
#' with frequentist likelihood approach using log-normal distribution.
#' Now handles log-likelihood signs correctly and uses shared simulation function.
#'
#' @param observed_weights Vector of observed final weights
#' @param simulation_function Function that runs simulation and returns weight
#' @param estimate_sigma Whether to estimate sigma or use fixed value
#' @param fixed_sigma Fixed sigma value (if estimate_sigma = FALSE)
#' @return List with MLE results
#' @keywords internal
mle_estimate_p_value_lognormal <- function(observed_weights, simulation_function, 
                                           estimate_sigma = TRUE, fixed_sigma = 0.1) {
  
  # Objective function for optimization (MINIMIZATION)
  objective_function <- function(params) {
    return(neg_log_likelihood_lognormal(params, observed_weights, simulation_function))
  }
  
  # Initial parameters
  if (estimate_sigma) {
    # Initial guess: p=1.0, sigma based on observed CV
    obs_cv <- sd(observed_weights) / mean(observed_weights)
    initial_params <- c(1.0, log(max(obs_cv, 0.01)))  # p, log(sigma)
    lower_bounds <- c(0.01, log(0.001))
    upper_bounds <- c(5.0, log(2.0))
  } else {
    initial_params <- c(1.0)  # just p
    lower_bounds <- c(0.01)
    upper_bounds <- c(5.0)
  }
  
  # Optimize (MINIMIZATION)
  mle_result <- optim(
    par = initial_params,
    fn = objective_function,
    method = "L-BFGS-B",
    lower = lower_bounds,
    upper = upper_bounds,
    hessian = TRUE  # For confidence intervals
  )
  
  # Extract results
  p_estimate <- mle_result$par[1]
  
  if (estimate_sigma) {
    sigma_estimate <- exp(mle_result$par[2])
  } else {
    sigma_estimate <- fixed_sigma
  }
  
  # Calculate standard errors from Hessian
  if (mle_result$convergence == 0 && all(is.finite(mle_result$hessian))) {
    tryCatch({
      std_errors <- sqrt(diag(solve(mle_result$hessian)))
      p_se <- std_errors[1]
      
      if (estimate_sigma) {
        # Delta method for log-transformed parameter
        sigma_se <- exp(mle_result$par[2]) * std_errors[2]
      } else {
        sigma_se <- NA
      }
    }, error = function(e) {
      p_se <- sigma_se <- NA
    })
  } else {
    p_se <- sigma_se <- NA
  }
  
  # Since optim() minimized the NEGATIVE log-likelihood
  final_log_likelihood <- -mle_result$value  # Convert back to positive
  
  return(list(
    p_estimate = p_estimate,
    p_se = p_se,
    sigma_estimate = sigma_estimate,
    sigma_se = sigma_se,
    log_likelihood = final_log_likelihood,  # \u2190 POSITIVE
    aic = 2 * length(mle_result$par) - 2 * final_log_likelihood,
    converged = (mle_result$convergence == 0),
    hessian = mle_result$hessian,
    observed_weights = observed_weights,
    n_observations = length(observed_weights)
  ))
}

#' Compute likelihood profile for p_value (OPTIMIZED)
#'
#' @description
#' FIXED VERSION: Computes likelihood profile by evaluating the likelihood function
#' across a grid of p_values while keeping other parameters optimal.
#' Now uses shared simulation function and returns POSITIVE log-likelihoods.
#'
#' @param p_estimate Central p_value (MLE estimate)
#' @param p_se Standard error of p_value 
#' @param simulation_function Function that runs simulation and returns target metric
#' @param observed_weights Vector of observed weights
#' @param sigma_estimate Estimated sigma parameter
#' @param grid_size Number of points in profile grid, default 50
#' @param grid_range_factor Range factor around estimate (±factor*SE), default 3
#' @return Data frame with p_values and corresponding log_likelihoods
#' @keywords internal
compute_likelihood_profile <- function(p_estimate, p_se, simulation_function, 
                                       observed_weights, sigma_estimate,
                                       grid_size = 50, grid_range_factor = 3) {
  
  # Define grid range
  if (is.na(p_se) || p_se <= 0) {
    # Fallback range if no standard error
    p_range <- c(max(0.01, p_estimate * 0.5), min(5.0, p_estimate * 2.0))
  } else {
    # Grid based on standard error
    p_range <- c(max(0.01, p_estimate - grid_range_factor * p_se),
                 min(5.0, p_estimate + grid_range_factor * p_se))
  }
  
  # Create grid
  p_grid <- seq(p_range[1], p_range[2], length.out = grid_size)
  
  # Evaluate likelihood at each point
  log_likelihoods <- sapply(p_grid, function(p_val) {
    tryCatch({
      # Run simulation for this p_value using shared function
      sim_weight <- simulation_function(p_val)
      
      # Handle simulation failure
      if (is.null(sim_weight) || is.na(sim_weight) || sim_weight <= 0) {
        return(NA)
      }
      
      # Calculate positive log-likelihood
      log_lik <- sum(dlnorm(observed_weights, 
                            meanlog = log(sim_weight), 
                            sdlog = sigma_estimate, 
                            log = TRUE))
      return(log_lik) 
      
    }, error = function(e) {
      return(NA)
    })
  })
  
  # Return profile data
  profile_data <- data.frame(
    p_value = p_grid,
    log_likelihood = log_likelihoods,
    relative_likelihood = exp(log_likelihoods - max(log_likelihoods, na.rm = TRUE))
  )
  
  # Remove failed evaluations
  profile_data <- profile_data[!is.na(profile_data$log_likelihood), ]
  
  return(profile_data)
}

#' Fit FB4 model using Maximum Likelihood Estimation (Mid-level - OPTIMIZED)
#'
#' @description
#' Coordinates MLE fitting process using log-normal distribution for weights.
#' Now uses shared commons functions to eliminate code duplication.
#'
#' @param observed_weights Vector of observed final weights
#' @param processed_simulation_data Complete processed simulation data
#' @param estimate_sigma Whether to estimate measurement error
#' @param oxycal Oxycalorific coefficient (J/g O2), default 13560
#' @param confidence_level Confidence level for intervals, default 0.95
#' @param compute_profile Whether to compute likelihood profile, default FALSE
#' @param profile_grid_size Number of points in profile grid, default 50
#' @param verbose Whether to show progress messages, default FALSE
#' @return List with MLE fitting results
#' @keywords internal
fit_fb4_mle <- function(observed_weights, processed_simulation_data,
                        estimate_sigma = TRUE, oxycal = 13560, confidence_level = 0.95,
                        compute_profile = FALSE, profile_grid_size = 50,
                        verbose = FALSE) {
  
  if (verbose) {
    message("Starting MLE estimation for p_value")
    message("Observations: ", length(observed_weights))
    message("Weight range: ", round(min(observed_weights), 2), " - ", 
            round(max(observed_weights), 2), "g")
    if (compute_profile) {
      message("Computing likelihood profile with ", profile_grid_size, " points...")
    }
  }
  
  # Create simulation function using shared execution wrapper
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
  
  # Run MLE
  mle_result <- mle_estimate_p_value_lognormal(
    observed_weights = observed_weights,
    simulation_function = simulation_function,
    estimate_sigma = estimate_sigma
  )
  
  # Calculate confidence intervals
  alpha <- 1 - confidence_level
  z <- z_score(1 - alpha)
  
  if (!is.na(mle_result$p_se)) {
    p_ci_lower <- mle_result$p_estimate - z * mle_result$p_se
    p_ci_upper <- mle_result$p_estimate + z * mle_result$p_se
  } else {
    p_ci_lower <- p_ci_upper <- NA
  }
  
  # Compute likelihood profile if requested
  profile_data <- NULL
  if (compute_profile) {
    if (verbose) message("Computing likelihood profile...")
    
    profile_data <- compute_likelihood_profile(
      p_estimate = mle_result$p_estimate,
      p_se = mle_result$p_se,
      simulation_function = simulation_function,
      observed_weights = observed_weights,
      sigma_estimate = mle_result$sigma_estimate,
      grid_size = profile_grid_size
    )
    
    if (verbose) {
      message("Profile computed with ", nrow(profile_data), " successful evaluations")
    }
  }
  
  # Run final simulation with estimated p using shared function
  final_simulation <- run_final_simulation(
    optimal_p_value = mle_result$p_estimate,
    processed_simulation_data = processed_simulation_data,
    oxycal = oxycal,
    verbose = verbose
  )
  
  if (verbose) {
    message("MLE converged: ", mle_result$converged)
    message("Estimated p_value: ", round(mle_result$p_estimate, 4), 
            " \u00b1 ", round(mle_result$p_se, 4))
    message("95% CI: [", round(p_ci_lower, 4), ", ", round(p_ci_upper, 4), "]")
  }
  
  # Prepare final result
  if (!is.null(final_simulation)) {
    return(list(
      # MLE results
      p_estimate = mle_result$p_estimate,
      p_se = mle_result$p_se,
      p_ci_lower = p_ci_lower,
      p_ci_upper = p_ci_upper,
      sigma_estimate = mle_result$sigma_estimate,
      sigma_se = mle_result$sigma_se,

      total_consumption_g = final_simulation$total_consumption_g,
      
      # Model fit statistics
      log_likelihood = mle_result$log_likelihood,
      aic = mle_result$aic,
      n_observations = mle_result$n_observations,
      
      # Likelihood profile
      profile_likelihood = profile_data,
      
      # Simulation results
      predicted_weight = final_simulation$final_weight,
      daily_output = final_simulation$daily_output,
      
      # Metadata
      method = "mle",
      confidence_level = confidence_level,
      converged = mle_result$converged
    ))
  }
  
  # Return error result using shared function
  return(create_error_result(
    error_message = "MLE estimation failed",
    strategy_type = "mle", 
    execution_plan = list(estimate_sigma = estimate_sigma, confidence_level = confidence_level)
  ))
}