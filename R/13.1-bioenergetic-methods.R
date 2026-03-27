
#' Methods for FB4 Bioenergetic Model
#'
#' @description
#' S3 class system for the Fish Bioenergetics 4.0 model, providing structured
#' data containers and methods for bioenergetic simulations and results.
#'
#' @name bioenergetic-methods
#' @aliases bioenergetic-methods
NULL

# ============================================================================
# BIOENERGETIC CLASS METHODS (Improved)
# ============================================================================

#' Check Bioenergetic Object Readiness
#'
#' @description
#' Returns a named logical list indicating which of the four required
#' components (species parameters, temperature, diet, initial weight) are
#' present in a \code{Bioenergetic} object.  Centralises the readiness
#' check that is otherwise duplicated across \code{print.Bioenergetic} and
#' \code{summary.Bioenergetic}.
#'
#' @param x A \code{Bioenergetic} object.
#'
#' @return Named logical list with elements \code{has_params},
#'   \code{has_temp}, \code{has_diet}, and \code{has_initial}.
#'
#' @keywords internal
check_bioenergetic_readiness <- function(x) {
  list(
    has_params  = !is.null(x$species_params) && length(x$species_params) > 0,
    has_temp    = !is.null(x$environmental_data$temperature),
    has_diet    = !is.null(x$diet_data$proportions),
    has_initial = !is.null(x$simulation_settings$initial_weight)
  )
}

#' Print Method for Bioenergetic Objects
#'
#' @description
#' Displays a concise one-page overview of a \code{Bioenergetic} object,
#' including species identity, initial weight, simulation duration, and the
#' status of each required component (parameters, temperature, diet).
#' Readiness for fitting is reported in the final status line.
#'
#' @param x Bioenergetic object
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#'
#' @examples
#' \dontrun{
#' bio <- Bioenergetic("Oncorhynchus mykiss")
#' print(bio)
#' }
#'
#' @export
print.Bioenergetic <- function(x, ...) {
  cat("FB4 Bioenergetic Model\n")
  cat(rep("=", 25), "\n", sep = "")
  
  # Species information
  info <- x$species_info
  species_name <- info$scientific_name %||% info$common_name %||% "Unknown species"
  if (!is.null(info$scientific_name) && !is.null(info$common_name)) {
    cat("Species: ", info$scientific_name, " (", info$common_name, ")\n", sep = "")
  } else {
    cat("Species: ", species_name, "\n", sep = "")
  }
  
  # Core setup
  initial_weight <- x$simulation_settings$initial_weight
  duration <- x$simulation_settings$duration %||% 
             x$environmental_data$duration %||% "Auto-detect"
  
  cat("Setup: ")
  if (!is.null(initial_weight)) {
    cat(initial_weight, "g")
  } else {
    cat("No initial weight")
  }
  cat(" \u2192 ", duration, " days\n", sep = "")
  
  # Component status with details
  cat("\nComponents:\n")
  ready <- check_bioenergetic_readiness(x)

  # Parameters
  if (ready$has_params) {
    param_count <- length(unlist(x$species_params))
    categories <- paste(names(x$species_params), collapse = ", ")
    cat("  \u2713 Parameters: ", param_count, " params (", categories, ")\n", sep = "")
  } else {
    cat("  \u2717 Parameters: Missing\n")
  }

  # Temperature
  if (ready$has_temp) {
    temp_data <- x$environmental_data$temperature
    temp_range <- range(temp_data$Temperature, na.rm = TRUE)
    cat("  \u2713 Temperature: ", nrow(temp_data), " days (",
        round(temp_range[1], 1), "-", round(temp_range[2], 1), "\u00b0C)\n", sep = "")
  } else {
    cat("  \u2717 Temperature: Missing\n")
  }

  # Diet
  if (ready$has_diet) {
    prey_count <- length(x$diet_data$prey_names)
    diet_days <- nrow(x$diet_data$proportions)
    cat("  \u2713 Diet: ", prey_count, " prey species, ", diet_days, " days\n", sep = "")
  } else {
    cat("  \u2717 Diet: Missing\n")
  }

  # Simulation readiness
  is_ready <- ready$has_params && ready$has_temp && ready$has_diet && ready$has_initial
  cat("\nStatus: ")
  if (x$fitted) {
    cat("\u2713 Fitted and ready\n")
  } else if (is_ready) {
    cat("Ready for fitting\n")
  } else {
    missing_count <- sum(!ready$has_params, !ready$has_temp, !ready$has_diet, !ready$has_initial)
    cat("Incomplete (", missing_count, " components missing)\n", sep = "")
  }
  
  invisible(x)
}

#' Summary Method for Bioenergetic Objects
#'
#' @description
#' Prints a detailed multi-section summary of a \code{Bioenergetic} object,
#' covering species identity, parameter categories, environmental data
#' statistics, diet composition, simulation settings, and overall readiness
#' status.  Complements \code{print.Bioenergetic}, which shows the compact
#' single-page view.
#'
#' @param object Bioenergetic object
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#'
#' @examples
#' \dontrun{
#' bio <- Bioenergetic("Oncorhynchus mykiss")
#' summary(bio)
#' }
#'
#' @export
summary.Bioenergetic <- function(object, ...) {
  cat("FB4 Bioenergetic Model - Detailed Summary\n")
  cat(rep("=", 45), "\n", sep = "")
  
  # Species Information
  info <- object$species_info
  if (!is.null(info) && length(info) > 0) {
    cat("SPECIES:\n")
    if (!is.null(info$scientific_name)) cat("  Scientific: ", info$scientific_name, "\n", sep = "")
    if (!is.null(info$common_name)) cat("  Common: ", info$common_name, "\n", sep = "")
    if (!is.null(info$life_stage)) cat("  Life stage: ", info$life_stage, "\n", sep = "")
    cat("\n")
  }
  
  # Parameters Summary
  if (!is.null(object$species_params)) {
    cat("PARAMETERS:\n")
    for (cat_name in names(object$species_params)) {
      params <- object$species_params[[cat_name]]
      if (length(params) > 0) {
        cat("  ", tools::toTitleCase(cat_name), " (", length(params), "):\n", sep = "")
        
        # Show key parameters based on category
        key_params <- switch(cat_name,
          "consumption" = c("CA", "CB", "CQ", "CTO", "CTM"),
          "respiration" = c("RA", "RB", "RQ", "RTO", "RTM"),
          "egestion" = c("FA", "FB", "FG"),
          "excretion" = c("UA", "UB", "UG"),
          names(params)[1:min(3, length(params))]
        )
        
        for (param in key_params) {
          if (param %in% names(params)) {
            cat("    ", param, " = ", params[[param]], "\n", sep = "")
          }
        }
        if (length(params) > length(key_params)) {
          cat("    ... (", length(params) - length(key_params), " more)\n", sep = "")
        }
      }
    }
    cat("\n")
  }
  
  # Environmental Data
  if (!is.null(object$environmental_data$temperature)) {
    cat("ENVIRONMENT:\n")
    temp_data <- object$environmental_data$temperature
    temp_stats <- summary(temp_data$Temperature)
    day_range <- range(temp_data$Day)
    
    cat("  Temperature: ", round(temp_stats[4], 1), "\u00b0C (mean)\n", sep = "")
    cat("  Range: ", round(temp_stats[1], 1), " - ", round(temp_stats[6], 1), "\u00b0C\n", sep = "")
    cat("  Duration: ", nrow(temp_data), " days (", day_range[1], " to ", day_range[2], ")\n", sep = "")
    cat("\n")
  }
  
  # Diet Information
  if (!is.null(object$diet_data)) {
    cat("DIET:\n")
    if (!is.null(object$diet_data$proportions)) {
      diet_props <- object$diet_data$proportions
      prey_names <- object$diet_data$prey_names
      
      cat("  Prey species: ", length(prey_names), "\n", sep = "")
      cat("  Coverage: ", nrow(diet_props), " days\n", sep = "")
      
      # Show top 3 prey by average proportion
      if (length(prey_names) > 0) {
        avg_props <- sapply(prey_names, function(p) mean(diet_props[[p]], na.rm = TRUE))
        top_prey <- sort(avg_props, decreasing = TRUE)[1:min(3, length(avg_props))]
        
        cat("  Main prey:\n")
        for (i in seq_along(top_prey)) {
          prey_name <- names(top_prey)[i]
          prop_pct <- round(top_prey[i] * 100, 1)
          cat("    ", i, ". ", prey_name, ": ", prop_pct, "%\n", sep = "")
        }
      }
    }
    
    if (!is.null(object$diet_data$energies)) {
      energy_data <- object$diet_data$energies
      energy_range <- range(unlist(energy_data[prey_names]), na.rm = TRUE)
      cat("  Energy density: ", round(energy_range[1]), " - ", 
          round(energy_range[2]), " J/g\n", sep = "")
    }
    cat("\n")
  }
  
  # Simulation Settings
  if (!is.null(object$simulation_settings)) {
    cat("SIMULATION:\n")
    settings <- object$simulation_settings
    for (setting in names(settings)) {
      if (!is.null(settings[[setting]])) {
        unit <- switch(setting,
          "initial_weight" = " g",
          "duration" = " days",
          ""
        )
        cat("  ", tools::toTitleCase(gsub("_", " ", setting)), ": ", 
            settings[[setting]], unit, "\n", sep = "")
      }
    }
    cat("\n")
  }
  
  # Final Status
  cat("STATUS: ")
  if (object$fitted) {
    cat("\u2713 Model fitted and results available\n")
  } else {
    # Check readiness
    ready <- check_bioenergetic_readiness(object)
    ready_count <- sum(ready$has_params, ready$has_temp, ready$has_diet, ready$has_initial)
    cat("Ready: ", ready_count, "/4 components")

    if (ready_count == 4) {
      cat(" - Ready for fitting!")
    } else {
      missing <- c(
        if (!ready$has_params)  "parameters",
        if (!ready$has_temp)    "temperature",
        if (!ready$has_diet)    "diet",
        if (!ready$has_initial) "initial_weight"
      )
      cat(" (missing: ", paste(missing, collapse = ", "), ")", sep = "")
    }
    cat("\n")
  }
  
  invisible(object)
}

# ============================================================================
# FB4_RESULT CLASS METHODS (Unified and Improved)
# ============================================================================

#' Print Method for fb4_result Objects
#'
#' @description
#' Displays a concise summary of an \code{fb4_result} object.  The output
#' adapts to the fitting method used: traditional methods (binary search,
#' optim, direct) show weight, growth, consumption, and convergence;
#' \code{"mle"} shows parameter estimates with confidence intervals and
#' AIC; \code{"bootstrap"} shows mean/SD estimates and CI; and
#' \code{"hierarchical"} shows population-level parameters with model fit
#' statistics.
#'
#' @param x fb4_result object
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#'
#' @examples
#' \dontrun{
#' result <- run_fb4(bio, fit_to = "Weight", fit_value = 150)
#' print(result)
#' }
#'
#' @export
print.fb4_result <- function(x, ...) {
  method <- x$summary$method
  
  cat("FB4 Simulation Results\n")
  cat(rep("=", 25), "\n", sep = "")
  
  # Species and basic info
  species_name <- x$bioenergetic_object$species_info$scientific_name %||% 
                  x$bioenergetic_object$species_info$common_name %||% "Unknown"
  cat("Species: ", species_name, "\n", sep = "")
  cat("Method: ", switch(method,
    "binary_search" = "Binary Search",
    "optim" = "Optimization",
    "direct" = "Direct Execution", 
    "mle" = "Maximum Likelihood",
    "bootstrap" = "Bootstrap",
    "hierarchical" = "Hierarchical",
    tools::toTitleCase(method)
  ), "\n", sep = "")
  
  # Duration
  cat("Duration: ", x$summary$simulation_days, " days\n", sep = "")
  
  cat("\n")
  
  # Method-specific results
  if (method %in% c("binary_search", "optim", "direct")) {
    # Traditional methods
    cat("RESULTS:\n")
    cat("  Initial weight: ", round(x$summary$initial_weight, 2), " g\n", sep = "")
    cat("  Final weight: ", round(x$summary$final_weight, 2), " g\n", sep = "")
    
    growth_pct <- (x$summary$final_weight / x$summary$initial_weight - 1) * 100
    cat("  Growth: ", round(growth_pct, 1), "%\n", sep = "")
    cat("  Total consumption: ", round(x$summary$total_consumption_g, 2), " g\n", sep = "")
    cat("  P_value: ", round(x$summary$p_value, 4), "\n", sep = "")
    
    # Target achievement (only show if a target was actually set)
    target_info <- x$method_data$target_info
    if (!is.null(target_info) && !is.null(target_info$fit_value)) {
      cat("  Target (", target_info$fit_to, " = ", target_info$fit_value, "): ",
          if (isTRUE(target_info$target_achieved)) "Achieved" else "Not achieved", "\n", sep = "")
    }
    
    # Convergence
    cat("\nFITTING: ")
    if (x$summary$converged) {
      cat("\u2713 Successful")
      if (!is.null(x$method_data$optimization_info$iterations)) {
        cat(" (", x$method_data$optimization_info$iterations, " iterations)", sep = "")
      }
    } else {
      cat("\u2717 Failed - using best approximation")
    }
    cat("\n")
    
  } else if (method == "mle") {
    # MLE method
    cat("MAXIMUM LIKELIHOOD ESTIMATION:\n")
    cat("  Observations: ", x$method_data$n_observations, " weights\n", sep = "")
    cat("  Initial weight: ", round(x$summary$initial_weight, 2), " g\n", sep = "")
    cat("  Predicted weight: ", round(x$summary$predicted_weight, 2), " g\n", sep = "")
    cat("\n")
    
    cat("PARAMETER ESTIMATES:\n")
    cat("  P_value: ", round(x$summary$p_estimate, 4), 
        " \u00b1 ", round(x$summary$p_se, 4), "\n", sep = "")
    
    ci <- x$method_data$confidence_intervals
    if (!is.null(ci$p_ci_lower)) {
      cat("  95% CI: [", round(ci$p_ci_lower, 4), ", ", 
          round(ci$p_ci_upper, 4), "]\n", sep = "")
    }
    
    cat("  Measurement error (\u03c3): ", round(x$method_data$sigma_estimate, 3), "\n", sep = "")
    
    cat("\nMODEL FIT:\n")
    cat("  Log-likelihood: ", round(x$method_data$log_likelihood, 2), "\n", sep = "")
    cat("  AIC: ", round(x$method_data$aic, 2), "\n", sep = "")
    cat("  Converged: ", if (x$summary$converged) "\u2713 Yes" else "\u2717 No", "\n", sep = "")
    
  } else if (method == "bootstrap") {
    # Bootstrap method
    bootstrap_info <- x$method_data$bootstrap_info
    cat("BOOTSTRAP ESTIMATION:\n")
    cat("  Bootstrap samples: ", bootstrap_info$n_bootstrap, 
        " (", round(bootstrap_info$success_rate * 100, 1), "% successful)\n", sep = "")
    cat("  Initial weight: ", round(x$summary$initial_weight, 2), " g\n", sep = "")
    cat("  Predicted weight: ", round(x$summary$predicted_weight, 2), " g\n", sep = "")
    
    if (bootstrap_info$parallel_used) {
      cat("  Parallel processing: \u2713 (", bootstrap_info$n_cores_used, " cores)\n", sep = "")
    }
    cat("\n")
    
    cat("ESTIMATES:\n")
    cat("  P_value: ", round(x$summary$p_mean, 4), 
        " \u00b1 ", round(x$summary$p_sd, 4), "\n", sep = "")
    
    ci <- x$method_data$confidence_intervals
    if (!is.null(ci$p_ci_lower)) {
      cat("  95% CI: [", round(ci$p_ci_lower, 4), ", ", 
          round(ci$p_ci_upper, 4), "]\n", sep = "")
    }
    
    cat("  Consumption: ", round(x$summary$consumption_mean, 2), 
        " \u00b1 ", round(x$summary$consumption_sd, 2), " g\n", sep = "")
    
  } else if (method == "hierarchical") {
    # Hierarchical method
    pop_results <- x$method_data$population_results
    cat("HIERARCHICAL ESTIMATION:\n")
    cat("  Individuals: ", x$summary$n_individuals, "\n", sep = "")
    cat("\n")
    
    cat("POPULATION PARAMETERS:\n")
    cat("  Mean P_value: ", round(pop_results$mu_p_estimate, 4), 
        " \u00b1 ", round(pop_results$mu_p_se, 4), "\n", sep = "")
    cat("  SD P_value: ", round(pop_results$sigma_p_estimate, 4), 
        " \u00b1 ", round(pop_results$sigma_p_se, 4), "\n", sep = "")
    
    cat("\nMODEL FIT:\n")
    model_fit <- x$method_data$model_fit
    cat("  Log-likelihood: ", round(model_fit$log_likelihood, 2), "\n", sep = "")
    cat("  AIC: ", round(model_fit$aic, 2), "\n", sep = "")
    cat("  Converged: ", if (x$summary$converged) "\u2713 Yes" else "\u2717 No", "\n", sep = "")
  }
  
  invisible(x)
}

#' Summary Method for fb4_result Objects
#'
#' @description
#' Prints the compact \code{print.fb4_result} output followed by a detailed
#' section with execution metadata (backend, version, timing) and
#' method-specific diagnostics: optimisation tolerances for traditional
#' methods, statistical details and profile-likelihood availability for MLE,
#' success rate and percentiles for bootstrap, and individual/population
#' parameter distributions for hierarchical fits.  Daily output column
#' availability is reported at the end.
#'
#' @param object fb4_result object
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object
#'
#' @examples
#' \dontrun{
#' result <- run_fb4(bio, fit_to = "Weight", fit_value = 150)
#' summary(result)
#' }
#'
#' @export
summary.fb4_result <- function(object, ...) {
  # Print basic information first
  print(object)
  
  method <- object$summary$method
  
  cat("\n")
  cat(rep("-", 40), "\n", sep = "")
  cat("DETAILED INFORMATION\n")
  cat(rep("-", 40), "\n", sep = "")
  
  # Execution details
  cat("EXECUTION:\n")
  cat("  Backend: ", object$model_info$backend, "\n", sep = "")
  cat("  Version: ", object$model_info$version, "\n", sep = "")
  cat("  Execution time: ", round(object$model_info$execution_time, 2), " seconds\n", sep = "")
  cat("  Timestamp: ", format(object$model_info$timestamp, "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
  cat("\n")
  
  # Method-specific detailed information
  if (method %in% c("binary_search", "optim")) {
    # Optimization details
    opt_info <- object$method_data$optimization_info
    cat("OPTIMIZATION DETAILS:\n")
    cat("  Final error: ", format(opt_info$final_error, scientific = TRUE), "\n", sep = "")
    cat("  Tolerance: ", opt_info$tolerance, "\n", sep = "")
    cat("  Iterations: ", opt_info$iterations, "\n", sep = "")
    cat("\n")
    
  } else if (method == "mle") {
    # MLE detailed information
    cat("STATISTICAL DETAILS:\n")
    cat("  Distribution: Log-Normal\n")
    cat("  Confidence level: ", object$method_data$confidence_level * 100, "%\n", sep = "")
    
    # Observed data statistics
    if (!is.null(object$method_data$weight_stats)) {
      weight_stats <- object$method_data$weight_stats
      cat("\nOBSERVED DATA:\n")
      cat("  Mean weight: ", round(weight_stats$mean, 2), " g\n", sep = "")
      cat("  SD weight: ", round(weight_stats$sd, 2), " g\n", sep = "")
      cat("  Range: ", round(weight_stats$min, 2), " - ", 
          round(weight_stats$max, 2), " g\n", sep = "")
      cat("  CV: ", round(weight_stats$sd / weight_stats$mean * 100, 1), "%\n", sep = "")
    }
    
    # Profile likelihood info
    if (!is.null(object$method_data$profile_likelihood)) {
      cat("\nPROFILE LIKELIHOOD:\n")
      cat("  Grid points computed: Available\n")
      cat("  Profile-based CI: Available\n")
    }
    cat("\n")
    
  } else if (method == "bootstrap") {
    # Bootstrap detailed information
    bootstrap_info <- object$method_data$bootstrap_info
    cat("BOOTSTRAP DETAILS:\n")
    cat("  Successful iterations: ", bootstrap_info$successful_iterations, 
        "/", bootstrap_info$n_bootstrap, "\n", sep = "")
    cat("  Success rate: ", round(bootstrap_info$success_rate * 100, 1), "%\n", sep = "")
    
    if (bootstrap_info$parallel_used) {
      cat("  Parallel execution: \u2713 (", bootstrap_info$n_cores_used, 
          " cores used)\n", sep = "")
    }
    
    # Model diagnostics
    if (!is.null(object$method_data$model_diagnostics)) {
      diagnostics <- object$method_data$model_diagnostics
      cat("\nMODEL DIAGNOSTICS:\n")
      for (metric in names(diagnostics)) {
        cat("  ", tools::toTitleCase(gsub("_", " ", metric)), ": ", 
            round(diagnostics[[metric]], 4), "\n", sep = "")
      }
    }
    
    # Percentiles
    if (!is.null(object$method_data$percentiles)) {
      percentiles <- object$method_data$percentiles
      cat("\nP_VALUE PERCENTILES:\n")
      for (pct in names(percentiles)) {
        cat("  ", pct, ": ", round(percentiles[[pct]], 4), "\n", sep = "")
      }
    }
    cat("\n")
    
  } else if (method == "hierarchical") {
    # Hierarchical detailed information
    cat("HIERARCHICAL MODEL DETAILS:\n")
    cat("  Backend: TMB (Template Model Builder)\n")
    cat("  Confidence level: ", object$method_data$confidence_level * 100, "%\n", sep = "")
    
    # Individual results summary
    ind_results <- object$method_data$individual_results
    if (!is.null(ind_results$p_estimates)) {
      p_est_stats <- summary(ind_results$p_estimates)
      cat("\nINDIVIDUAL P_VALUES:\n")
      cat("  Range: ", round(p_est_stats[1], 4), " - ", 
          round(p_est_stats[6], 4), "\n", sep = "")
      cat("  Median: ", round(p_est_stats[3], 4), "\n", sep = "")
      cat("  IQR: ", round(p_est_stats[2], 4), " - ", 
          round(p_est_stats[5], 4), "\n", sep = "")
    }
    
    # Model comparison
    model_fit <- object$method_data$model_fit
    cat("\nMODEL SELECTION:\n")
    cat("  AIC: ", round(model_fit$aic, 2), "\n", sep = "")
    cat("  BIC: ", round(model_fit$bic, 2), "\n", sep = "")
    cat("\n")
  }
  
  # Daily output information
  if (!is.null(object$daily_output)) {
    cat("OUTPUT DATA:\n")
    cat("  Daily values: ", nrow(object$daily_output), " days\n", sep = "")
    
    # Show key columns available
    key_cols <- intersect(names(object$daily_output), 
                         c("Weight", "Consumption", "Growth", "Temperature"))
    if (length(key_cols) > 0) {
      cat("  Variables: ", paste(key_cols, collapse = ", "), "\n", sep = "")
    }
  }
  
  invisible(object)
}