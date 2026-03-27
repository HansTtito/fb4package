#' FB4 TMB Shared Functions
#'
#' @description
#' Core TMB functions shared between basic and hierarchical models.
#' Includes optimization, result extraction, profiling, and utility functions.
#'
#' @name FB4-TMB-Shared
NULL

# ============================================================================
# SDR EXTRACTION HELPERS
# ============================================================================
# Low-level helpers used by extract_basic_parameters and
# extract_hierarchical_parameters.  Accepting sdr_summary as an explicit
# parameter (rather than capturing it from an enclosing scope) keeps these
# functions pure and eliminates the need to re-define closures inside two
# separate functions.

#' Pull a scalar estimate from an sdreport summary
#' @param sdr_summary Matrix returned by \code{summary(TMB::sdreport(obj))}
#' @param name        Row name to look up
#' @return Numeric scalar estimate, or \code{NA_real_} if not found
#' @keywords internal
sdr_pull_est <- function(sdr_summary, name) {
  idx <- which(rownames(sdr_summary) == name)
  if (length(idx) > 0L) sdr_summary[idx[[1L]], "Estimate"] else NA_real_
}

#' Pull a scalar standard error from an sdreport summary
#' @param sdr_summary Matrix returned by \code{summary(TMB::sdreport(obj))}
#' @param name        Row name to look up
#' @return Numeric scalar SE, or \code{NA_real_} if not found
#' @keywords internal
sdr_pull_se <- function(sdr_summary, name) {
  idx <- which(rownames(sdr_summary) == name)
  if (length(idx) > 0L) sdr_summary[idx[[1L]], "Std. Error"] else NA_real_
}

#' Pull a vector of estimates and SEs from an sdreport summary
#'
#' @description
#' Extracts the first \code{n} rows whose name matches \code{name} from
#' \code{sdr_summary}, returning both estimates and standard errors.
#' Used for per-individual variables in the hierarchical model.
#'
#' @param sdr_summary Matrix returned by \code{summary(TMB::sdreport(obj))}
#' @param name        Row name to look up (may appear multiple times)
#' @param n           Number of elements expected
#' @return Named list with \code{estimates} and \code{ses} (length-\code{n}
#'   numeric vectors; filled with \code{NA_real_} when rows are missing)
#' @keywords internal
sdr_pull_vec <- function(sdr_summary, name, n) {
  idx <- which(rownames(sdr_summary) == name)
  if (length(idx) >= n) {
    list(
      estimates = sdr_summary[idx[seq_len(n)], "Estimate"],
      ses       = sdr_summary[idx[seq_len(n)], "Std. Error"]
    )
  } else {
    list(estimates = rep(NA_real_, n), ses = rep(NA_real_, n))
  }
}

#' Batch-assign scalar estimate/SE pairs to a results list
#'
#' @description
#' For each name in \code{fields}, sets \code{results[[name_est]]} and
#' \code{results[[name_se]]} by calling \code{\link{sdr_pull_est}} /
#' \code{\link{sdr_pull_se}}.  Replaces the repetitive
#' \code{results$field_est <- sdr_pull_est(sdr_summary, "field")} pattern.
#'
#' @param results     Named list to update
#' @param sdr_summary SDR summary matrix (from \code{summary(sdreport(obj))})
#' @param fields      Character vector of ADREPORT variable names
#' @return Updated \code{results} list
#' @keywords internal
sdr_assign_scalars <- function(results, sdr_summary, fields) {
  for (nm in fields) {
    results[[paste0(nm, "_est")]] <- sdr_pull_est(sdr_summary, nm)
    results[[paste0(nm, "_se")]]  <- sdr_pull_se(sdr_summary, nm)
  }
  results
}

# ============================================================================
# TMB COMPILATION AND VALIDATION
# ============================================================================

#' Validate TMB objective function
#'
#' @description
#' Test TMB objective function evaluation and gradient computation
#' to ensure proper setup before optimization.
#'
#' @param obj TMB objective function object
#' @param verbose Show diagnostic information
#' @return TRUE if validation passes, stops with error otherwise
#' @keywords internal
validate_tmb_objective <- function(obj, verbose = FALSE) {
  
  if (verbose) {
    message("Validating TMB objective function...")
  }
  
  # Test objective function evaluation
  test_value <- tryCatch({
    obj$fn(obj$par)
  }, error = function(e) {
    stop("TMB objective function evaluation failed: ", e$message)
  })
  
  if (!is.finite(test_value)) {
    stop("TMB objective function returned non-finite value: ", test_value)
  }
  
  # Test gradient computation
  test_gradient <- tryCatch({
    obj$gr(obj$par)
  }, error = function(e) {
    stop("TMB gradient computation failed: ", e$message)
  })
  
  if (any(!is.finite(test_gradient))) {
    stop("TMB gradient contains non-finite values")
  }
  
  if (length(test_gradient) != length(obj$par)) {
    stop("TMB gradient length (", length(test_gradient), 
         ") does not match parameter length (", length(obj$par), ")")
  }
  
  if (verbose) {
    message("TMB validation successful:")
    message("  Objective value: ", round(test_value, 4))
    message("  Gradient norm: ", round(sqrt(sum(test_gradient^2)), 6))
    message("  Parameters: ", length(obj$par))
  }
  
  return(TRUE)
}

#' Check TMB model availability (simplified for installed package)
#'
#' @description
#' Verify that TMB model is available in installed package.
#'
#' @param dll_name Name of DLL (default: "fb4package")
#' @param verbose Show diagnostic information
#' @return TRUE if model is available, FALSE otherwise
#' @keywords internal
check_tmb_compilation <- function(dll_name = "fb4package", verbose = FALSE) {
  
  # Check if DLL is loaded
  dll_loaded <- dll_name %in% names(getLoadedDLLs())
  
  if (!dll_loaded) {
    if (verbose) {
      message("TMB DLL '", dll_name, "' not loaded")
    }
    return(FALSE)
  }
  
  if (verbose) {
    message("TMB model available and loaded")
  }
  
  return(TRUE)
}


# ============================================================================
# ROBUST OPTIMIZATION ENGINE
# ============================================================================

#' Run robust optimization with multiple fallback strategies
#'
#' @description
#' Advanced optimization routine with automatic fallback strategies,
#' multiple starting points, and comprehensive error handling.
#' Works for both basic and hierarchical models.
#'
#' @param obj TMB objective function
#' @param method Primary optimization method ("nlminb" or "optim")
#' @param lower Lower parameter bounds (log scale)
#' @param upper Upper parameter bounds (log scale)
#' @param max_iter Maximum iterations
#' @param tolerance Convergence tolerance
#' @param n_restarts Number of random restarts if primary fails
#' @param verbose Show optimization progress
#' @param ... Additional arguments passed to optimization function
#' @return Optimization result list
#' @keywords internal
run_robust_optimization <- function(obj, 
                                    method = "nlminb", 
                                    lower = NULL, 
                                    upper = NULL,
                                    max_iter = 1000, 
                                    tolerance = 1e-6,
                                    n_restarts = 3,
                                    verbose = FALSE, 
                                    ...) {
  
  if (verbose) {
    message("Starting robust optimization...")
    message("Method: ", method)
    message("Parameters: ", length(obj$par))
  }
  
  # Validate TMB object first
  validate_tmb_objective(obj, verbose = verbose)
  
  # Set default bounds if not provided
  n_params <- length(obj$par)
  if (is.null(lower)) {
    lower <- rep(-10, n_params)  # Very permissive defaults
  }
  if (is.null(upper)) {
    upper <- rep(10, n_params)
  }
  
  # Create wrapper functions for TMB
  objective_wrapper <- function(par) {
    result <- obj$fn(par)
    return(result)
  }
  
  gradient_wrapper <- function(par) {
    result <- obj$gr(par)
    return(result)
  }
  
  # Store original parameters
  original_par <- obj$par
  
  # Primary optimization attempt
  opt_result <- run_single_optimization(
    start_par = obj$par,
    obj_fn = objective_wrapper,
    grad_fn = gradient_wrapper,
    method = method,
    lower = lower,
    upper = upper,
    max_iter = max_iter,
    tolerance = tolerance,
    verbose = verbose,
    ...
  )
  
  # Check if primary optimization succeeded
  if (!is.null(opt_result) && opt_result$convergence == 0) {
    if (verbose) {
      message("Primary optimization succeeded")
    }
    obj$par <- opt_result$par
    return(opt_result)
  }
  
  # Fallback strategies if primary failed
  if (verbose) {
    message("Primary optimization failed, trying fallback strategies...")
  }
  
  # Strategy 1: Try alternative method
  alt_method <- if (method == "nlminb") "optim" else "nlminb"
  
  if (verbose) {
    message("Trying alternative method: ", alt_method)
  }
  
  obj$par <- original_par  # Reset parameters
  
  opt_result <- run_single_optimization(
    start_par = obj$par,
    obj_fn = objective_wrapper,
    grad_fn = gradient_wrapper,
    method = alt_method,
    lower = lower,
    upper = upper,
    max_iter = max_iter,
    tolerance = tolerance,
    verbose = FALSE,
    ...
  )
  
  if (!is.null(opt_result) && opt_result$convergence == 0) {
    if (verbose) {
      message("Alternative method succeeded")
    }
    obj$par <- opt_result$par
    return(opt_result)
  }
  
  # Strategy 2: Random restarts with different starting points
  if (verbose) {
    message("Trying random restarts (", n_restarts, " attempts)...")
  }
  
  for (restart in 1:n_restarts) {
    
    # Generate random starting point within bounds
    random_start <- runif(n_params, 
                          pmax(lower, original_par - 2), 
                          pmin(upper, original_par + 2))
    
    if (verbose) {
      message("Random restart ", restart, "/", n_restarts)
    }
    
    opt_result <- run_single_optimization(
      start_par = random_start,
      obj_fn = objective_wrapper,
      grad_fn = gradient_wrapper,
      method = method,
      lower = lower,
      upper = upper,
      max_iter = max_iter,
      tolerance = tolerance,
      verbose = FALSE,
      ...
    )
    
    if (!is.null(opt_result) && opt_result$convergence == 0) {
      if (verbose) {
        message("Random restart ", restart, " succeeded")
      }
      obj$par <- opt_result$par
      return(opt_result)
    }
  }
  
  # Strategy 3: Relaxed tolerance
  if (verbose) {
    message("Trying relaxed convergence criteria...")
  }
  
  obj$par <- original_par  # Reset parameters
  
  opt_result <- run_single_optimization(
    start_par = obj$par,
    obj_fn = objective_wrapper,
    grad_fn = gradient_wrapper,
    method = method,
    lower = lower,
    upper = upper,
    max_iter = max_iter * 2,
    tolerance = tolerance * 100,  # Much more relaxed
    verbose = FALSE,
    ...
  )
  
  if (!is.null(opt_result)) {
    if (verbose) {
      message("Relaxed optimization completed with convergence code: ", opt_result$convergence)
    }
    obj$par <- opt_result$par
    return(opt_result)
  }
  
  # All strategies failed
  stop("All optimization strategies failed. Check model specification and data.")
}

#' Run single optimization attempt
#'
#' @description
#' Execute a single optimization run with specified method and parameters.
#'
#' @param start_par Starting parameter values
#' @param obj_fn Objective function
#' @param grad_fn Gradient function
#' @param method Optimization method
#' @param lower Lower bounds
#' @param upper Upper bounds
#' @param max_iter Maximum iterations
#' @param tolerance Convergence tolerance
#' @param verbose Show progress
#' @param ... Additional arguments
#' @return Optimization result or NULL if failed
#' @keywords internal
run_single_optimization <- function(start_par, obj_fn, grad_fn, method,
                                    lower, upper, max_iter, tolerance, 
                                    verbose = FALSE, ...) {
  
  tryCatch({
    if (method == "nlminb") {
      nlminb(
        start = start_par,
        objective = obj_fn,
        gradient = grad_fn,
        lower = lower,
        upper = upper,
        control = list(
          eval.max = max_iter,
          iter.max = max_iter,
          abs.tol = tolerance,
          rel.tol = tolerance,
          trace = if (verbose) 1 else 0,
          ...
        )
      )
    } else if (method == "optim") {
      optim(
        par = start_par,
        fn = obj_fn,
        gr = grad_fn,
        method = "L-BFGS-B",
        lower = lower,
        upper = upper,
        control = list(
          maxit = max_iter,
          factr = tolerance / .Machine$double.eps,
          trace = if (verbose) 1 else 0,
          ...
        )
      )
    } else {
      stop("Unsupported optimization method: ", method)
    }
  }, error = function(e) {
    NULL
  })
}

# ============================================================================
# RESULTS EXTRACTION
# ============================================================================

#' Extract comprehensive results from TMB optimization
#'
#' @description
#' Extract and process optimization results for both basic and hierarchical models.
#' Handles standard errors, confidence intervals, and model diagnostics.
#'
#' @param obj TMB objective function
#' @param opt_result Optimization result
#' @param model_type Type of model ("basic" or "hierarchical")
#' @param confidence_level Confidence level for intervals
#' @param verbose Show extraction progress
#' @return List with comprehensive results
#' @keywords internal
extract_tmb_results <- function(obj, opt_result, model_type = "basic", 
                                confidence_level = 0.95, verbose = FALSE) {
  
  if (verbose) {
    message("Extracting TMB results for ", model_type, " model...")
  }
  
  # Basic convergence information
  converged <- (opt_result$convergence == 0)
  neg_log_likelihood <- opt_result$objective
  log_likelihood <- -neg_log_likelihood
  
  # Extract optimized parameters
  optimized_params <- opt_result$par
  n_params <- length(optimized_params)
  
  # Get final report from TMB
  final_report <- obj$report()
  
  # Initialize results structure
  results <- list(
    converged = converged,
    log_likelihood = log_likelihood,
    aic = 2 * n_params - 2 * log_likelihood,
    bic = log(final_report$n_observations %||% length(obj$env$data$observed_weights)) * n_params - 2 * log_likelihood,
    optimization_result = opt_result,
    model_type = model_type
  )
  
  # Model-specific parameter extraction
  if (model_type == "basic") {
    results <- extract_basic_parameters(results, optimized_params, obj, confidence_level, verbose)
  } else if (model_type == "hierarchical") {
    results <- extract_hierarchical_parameters(results, optimized_params, obj, confidence_level, verbose)
  }
  
  # Extract ADREPORT values if available
  results$adreport_values <- extract_adreport_values(final_report, model_type)
  
  if (verbose) {
    message("Results extraction completed")
    message("  Converged: ", converged)
    message("  Log-likelihood: ", round(log_likelihood, 2))
    message("  AIC: ", round(results$aic, 2))
  }
  
  return(results)
}

#' Extract parameters for basic model
#'
#' @param results Current results list
#' @param params Optimized parameters (log scale)
#' @param obj TMB objective function
#' @param confidence_level Confidence level
#' @param verbose Show progress
#' @return Updated results list
#' @keywords internal
extract_basic_parameters <- function(results, params, obj, confidence_level, verbose) {
  
  # Transform from log scale
  p_estimate <- exp(params[1])
  sigma_estimate <- exp(params[2])
  
  # Initialize SE and CI
  p_se <- sigma_se <- p_ci_lower <- p_ci_upper <- NA
  
  # Compute standard errors using TMB::sdreport
  sdr <- NULL
  tryCatch({
    if (verbose) message("Computing standard errors...")
    
    sdr <- TMB::sdreport(obj)
    
    # Standard errors on log scale for fixed parameters
    if (length(sdr$cov.fixed) >= 4 && nrow(sdr$cov.fixed) >= 2) {
      log_se <- sqrt(diag(sdr$cov.fixed))
      
      if (length(log_se) >= 2) {
        # Transform to original scale using delta method
        p_se <- p_estimate * log_se[1]
        sigma_se <- sigma_estimate * log_se[2]
        
        # Confidence intervals
        z <- z_score(confidence_level)
        p_ci_lower <- exp(params[1] - z * log_se[1])
        p_ci_upper <- exp(params[1] + z * log_se[1])
      }
    }
    
  }, error = function(e) {
    if (verbose) {
      message("Warning: Standard error computation failed: ", e$message)
    }
  })

  # Extract ADREPORT values for basic model
  adreport_vals <- extract_adreport_values(obj$report(), "basic")

  # Extract ADREPORT values with uncertainty propagation
  tryCatch({
    if (!is.null(sdr) && !is.null(sdr$value)) {
      
      if (verbose) message("Extracting ADREPORT values with uncertainty...")
      
      # Get summary with estimates and standard errors
      sdr_summary <- summary(sdr)
      
      # Bulk-assign all ADREPORT scalar variables using the package-level helpers
      # (sdr_pull_est / sdr_pull_se / sdr_assign_scalars defined at top of this file)
      results <- sdr_assign_scalars(results, sdr_summary, c(
        # Core growth and consumption
        "final_weight", "total_consumption_g", "gross_growth_efficiency",
        "total_growth", "relative_growth",
        # Energy budget components
        "total_consumption_energy", "total_respiration_energy",
        "total_egestion_energy", "total_excretion_energy",
        "total_sda_energy", "total_net_energy", "total_spawn_energy",
        # Efficiency and consumption metrics
        "mean_daily_consumption", "mean_specific_consumption",
        "specific_growth_rate", "metabolic_scope",
        # Energy budget proportions
        "prop_respiration", "prop_egestion", "prop_excretion",
        "prop_sda", "prop_growth",
        # Final energy density
        "final_energy_density"
      ))

      if (verbose) {
        n_extracted <- sum(!is.na(c(results$final_weight_est,
                                    results$total_consumption_g_est,
                                    results$gross_growth_efficiency_est)))
        message("Successfully extracted ", n_extracted, " ADREPORT variables with uncertainty")
      }
      
    }
  }, error = function(e) {
    if (verbose) {
      message("Warning: ADREPORT extraction failed: ", e$message)
    }
  })

  # Add basic model results
  results$p_estimate <- as.numeric(p_estimate)
  results$p_se <- as.numeric(p_se)
  results$p_ci_lower <- as.numeric(p_ci_lower)
  results$p_ci_upper <- as.numeric(p_ci_upper)
  results$sigma_estimate <- as.numeric(sigma_estimate)
  results$sigma_se <- as.numeric(sigma_se)
  results$confidence_level <- as.numeric(confidence_level)
  
  return(results)
}

#' Extract parameters for hierarchical model
#'
#' @param results Current results list
#' @param params Optimized parameters (log scale)
#' @param obj TMB objective function
#' @param confidence_level Confidence level
#' @param verbose Show progress
#' @return Updated results list
#' @keywords internal
extract_hierarchical_parameters <- function(results, params, obj, confidence_level, verbose) {
  
  n_individuals <- obj$env$data$n_individuals
  n_covariates <- obj$env$data$n_covariates

  if (length(params) != (n_covariates + 3)) {
    stop("Parameter vector length mismatch: expected ", (n_covariates + 3), 
        ", got ", length(params))
  }

  # Extract betas (first n_covariates parameters, WITHOUT exp())
  if (n_covariates > 0) {
    betas <- params[1:n_covariates] 
  } else {
    betas <- numeric(0)
  }

  # Population parameters (WITH exp() because they are on log scale)
  mu_p_estimate <- exp(params[n_covariates + 1])
  sigma_p_estimate <- exp(params[n_covariates + 2]) 
  sigma_obs_estimate <- exp(params[n_covariates + 3])
  
  # Individual parameters - get from report, not from params
  final_report <- obj$report()
  individual_p_estimates <- final_report$individual_p_values
  
  # If report failed, reconstruct from random effects
  if (is.null(individual_p_estimates) || length(individual_p_estimates) == 0) {
    if (verbose) message("Report failed, reconstructing individual p_values...")
    # This is a fallback - may not work perfectly with random effects
    individual_p_estimates <- rep(mu_p_estimate, n_individuals)
  }
  
  # Initialize SE structures
  mu_p_se <- sigma_p_se <- sigma_obs_se <- NA
  individual_p_se <- rep(NA, n_individuals)  # Not available with random effects
  
  # Compute standard errors and extract ADREPORT values
  sdr <- NULL
  tryCatch({
    if (verbose) message("Computing hierarchical standard errors...")
    
    sdr <- TMB::sdreport(obj)
    
    # Standard errors for betas
    if (n_covariates > 0) {
      beta_indices <- (1:n_covariates)
      results$betas_se <- sqrt(diag(sdr$cov.fixed))[beta_indices]
    }
    
    # Standard errors for population parameters
    if (length(sdr$cov.fixed) > 0 && nrow(sdr$cov.fixed) >= (n_covariates + 3)) {
      log_se <- sqrt(diag(sdr$cov.fixed))
      
      # CORRECTION: Indices must consider that betas come first
      if (length(log_se) >= (n_covariates + 3)) {
        mu_p_se <- mu_p_estimate * log_se[n_covariates + 1]       # ← CORRECT
        sigma_p_se <- sigma_p_estimate * log_se[n_covariates + 2] # ← CORRECT  
        sigma_obs_se <- sigma_obs_estimate * log_se[n_covariates + 3] # ← CORRECT
      }
    }
    
  }, error = function(e) {
    if (verbose) {
      message("Warning: Hierarchical standard error computation failed: ", e$message)
    }
  })
  
  # Extract ADREPORT values for hierarchical model
  adreport_vals <- extract_adreport_values(final_report, "hierarchical")
  
  # Extract ADREPORT values with uncertainty propagation for hierarchical model
  tryCatch({
    if (!is.null(sdr) && !is.null(sdr$value)) {
      
      if (verbose) message("Extracting hierarchical ADREPORT values with uncertainty...")
      
      # Get summary with estimates and standard errors
      sdr_summary <- summary(sdr)
      
      # ---- Individual-level variables (vector, length = n_individuals) --------
      # p_values: only SE needed (estimates come from obj$report())
      results$individual_p_values_se <-
        sdr_pull_vec(sdr_summary, "individual_p_values", n_individuals)$ses

      # Remaining individual variables: extract both est and se
      # Each entry: c(sdr_name, results_prefix)
      individual_vector_fields <- list(
        c("final_weights",                   "individual_final_weights"),
        c("individual_total_consumption",    "individual_total_consumption"),
        c("individual_total_growth",         "individual_total_growth"),
        c("individual_relative_growth",      "individual_relative_growth"),
        c("individual_gross_efficiency",     "individual_gross_efficiency"),
        c("individual_metabolic_scope",      "individual_metabolic_scope"),
        c("individual_final_energy_density", "individual_final_energy_density"),
        c("individual_respiration_energy",   "individual_respiration_energy"),
        c("individual_egestion_energy",      "individual_egestion_energy"),
        c("individual_excretion_energy",     "individual_excretion_energy"),
        c("individual_sda_energy",           "individual_sda_energy"),
        c("individual_net_energy",           "individual_net_energy"),
        c("individual_spawn_energy",         "individual_spawn_energy")
      )
      for (pair in individual_vector_fields) {
        vec <- sdr_pull_vec(sdr_summary, pair[[1L]], n_individuals)
        results[[paste0(pair[[2L]], "_est")]] <- vec$estimates
        results[[paste0(pair[[2L]], "_se")]]  <- vec$ses
      }

      # ---- Population-level scalar variables ----------------------------------
      results <- sdr_assign_scalars(results, sdr_summary, c(
        "mean_final_weight", "mean_total_consumption",
        "mean_total_growth", "mean_relative_growth",
        "mean_gross_efficiency", "mean_metabolic_scope",
        "mean_final_energy_density",
        "mean_respiration_energy", "mean_egestion_energy",
        "mean_excretion_energy", "mean_sda_energy",
        "mean_net_energy", "mean_spawn_energy"
      ))

      if (verbose) {
        n_individual_vars <- sum(!is.na(results$individual_final_weights_est))
        n_population_vars <- sum(!is.na(c(results$mean_final_weight_est,
                                          results$mean_total_consumption_est)))
        message("Successfully extracted uncertainty for ", n_individual_vars,
                " individual and ", n_population_vars, " population variables")
      }
      
    }
  }, error = function(e) {
    if (verbose) {
      message("Warning: Hierarchical ADREPORT extraction failed: ", e$message)
    }
  })

  # Add hierarchical model results
  results$mu_p_estimate <- as.numeric(mu_p_estimate)
  results$mu_p_se <- as.numeric(mu_p_se)
  results$sigma_p_estimate <- as.numeric(sigma_p_estimate)
  results$sigma_p_se <- as.numeric(sigma_p_se)
  results$sigma_obs_estimate <- as.numeric(sigma_obs_estimate)
  results$sigma_obs_se <- as.numeric(sigma_obs_se)
  results$individual_p_estimates <- as.numeric(individual_p_estimates)
  results$individual_p_se <- as.numeric(individual_p_se)
  results$n_individuals <- as.numeric(n_individuals)
  results$confidence_level <- as.numeric(confidence_level)
  results$betas <- as.numeric(betas)
  results$betas_se <- rep(NA, length(betas))
  
  return(results)
}


#' Extract ADREPORT values from TMB report
#'
#' @param report TMB report object
#' @param model_type Model type
#' @return List of ADREPORT values
#' @keywords internal
extract_adreport_values <- function(report, model_type) {
  
  if (model_type == "basic") {
    return(list(
      final_weight = report$final_weight %||% NA,
      total_consumption_g = report$total_consumption_g %||% NA,
      gross_growth_efficiency = report$gross_growth_efficiency %||% NA,
      total_growth = report$total_growth %||% NA,
      relative_growth = report$relative_growth %||% NA,
      # Energy budget variables
      total_consumption_energy = report$total_consumption_energy %||% NA,
      total_respiration_energy = report$total_respiration_energy %||% NA,
      total_egestion_energy = report$total_egestion_energy %||% NA,
      total_excretion_energy = report$total_excretion_energy %||% NA,
      total_sda_energy = report$total_sda_energy %||% NA,
      total_net_energy = report$total_net_energy %||% NA,
      total_spawn_energy = report$total_spawn_energy %||% NA,

      # Efficiency and consumption metrics
      mean_daily_consumption = report$mean_daily_consumption %||% NA,
      mean_specific_consumption = report$mean_specific_consumption %||% NA,
      specific_growth_rate = report$specific_growth_rate %||% NA,
      metabolic_scope = report$metabolic_scope %||% NA,

      # Energy budget proportions
      prop_respiration = report$prop_respiration %||% NA,
      prop_egestion = report$prop_egestion %||% NA,
      prop_excretion = report$prop_excretion %||% NA,
      prop_sda = report$prop_sda %||% NA,
      prop_growth = report$prop_growth %||% NA
    
    ))
  } else if (model_type == "hierarchical") {
    n_individuals <- report$n_individuals %||% 1
    return(list(
      final_weights = report$final_weights %||% rep(NA, report$n_individuals %||% 1),
      mean_final_weight = report$mean_final_weight %||% NA,
      individual_p_values = report$individual_p_values %||% rep(NA, length(results$individual_p_estimates)),

      # Variables individuales (vectores con n_individuals elementos cada uno):
      individual_p_values = report$individual_p_values,
      final_weights = report$final_weights,
      individual_total_consumption = report$individual_total_consumption,
      individual_total_growth = report$individual_total_growth,
      individual_relative_growth = report$individual_relative_growth,
      individual_gross_efficiency = report$individual_gross_efficiency,
      individual_metabolic_scope = report$individual_metabolic_scope,
      individual_final_energy_density = report$individual_final_energy_density,

      # Componentes del presupuesto energético por individuo:
      individual_respiration_energy = report$individual_respiration_energy,
      individual_egestion_energy = report$individual_egestion_energy,
      individual_excretion_energy = report$individual_excretion_energy,
      individual_sda_energy = report$individual_sda_energy,
      individual_net_energy = report$individual_net_energy,
      individual_spawn_energy = report$individual_spawn_energy,

      # Variables poblacionales (promedios con incertidumbre propagada):
      mean_final_weight = report$mean_final_weight,
      mean_total_consumption = report$mean_total_consumption,
      mean_total_growth = report$mean_total_growth,
      mean_relative_growth = report$mean_relative_growth,
      mean_gross_efficiency = report$mean_gross_efficiency,
      mean_metabolic_scope = report$mean_metabolic_scope,
      mean_final_energy_density = report$mean_final_energy_density,
      mean_respiration_energy = report$mean_respiration_energy,
      mean_egestion_energy = report$mean_egestion_energy,
      mean_excretion_energy = report$mean_excretion_energy,
      mean_sda_energy = report$mean_sda_energy,
      mean_net_energy = report$mean_net_energy,
      mean_spawn_energy = report$mean_spawn_energy
    ))
  }
  
  return(list())
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Safe parameter extraction with defaults
#'
#' @description
#' Safely extract parameters from nested lists with validation and defaults.
#'
#' @param param_list Parameter list
#' @param param_name Parameter name
#' @param default Default value if missing
#' @param required Whether parameter is required
#' @return Parameter value
#' @keywords internal
safe_extract_param <- function(param_list, param_name, default = NA, required = TRUE) {
  
  if (is.null(param_list)) {
    if (required) {
      stop("Missing parameter category: ", deparse(substitute(param_list)))
    }
    return(default)
  }
  
  value <- param_list[[param_name]]
  
  if (is.null(value)) {
    if (required) {
      stop("Missing required parameter: ", param_name, " in ", deparse(substitute(param_list)))
    }
    return(default)
  }
  
  # Handle list values
  if (is.list(value)) {
    if (length(value) == 1) {
      value <- value[[1]]
    } else if (length(value) == 0) {
      return(default)
    } else {
      stop("Parameter ", param_name, " is a list with multiple elements")
    }
  }
  
  # Convert to numeric
  value <- as.numeric(value)
  
  if (is.na(value) && required) {
    stop("Parameter ", param_name, " is NA or cannot be converted to numeric")
  }
  
  return(value)
}
