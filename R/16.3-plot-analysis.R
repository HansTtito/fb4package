#' Analysis Plots for FB4 Results (Uncertainty and Sensitivity)
#'
#' @description
#' Plotting functions for uncertainty analysis and sensitivity analysis.
#' Includes functions for MLE, bootstrap, hierarchical methods, and sensitivity analysis.
#'
#' @name fb4-analysis-plots
#' @importFrom graphics plot hist lines abline text legend grid points barplot
#' @importFrom stats density
NULL

# ============================================================================
# UNCERTAINTY ANALYSIS PLOTS
# ============================================================================

#' Plot parameter uncertainty for probabilistic methods
#'
#' @description
#' Creates plots showing parameter estimates with confidence intervals.
#' Adapts automatically to the method used (MLE, bootstrap, hierarchical).
#'
#' @param fb4_result FB4 result object with uncertainty estimates
#' @param parameters Parameters to plot: "p_value", "consumption", "all", default "all"
#' @param color_scheme Color scheme to use, default "blue"
#' @param add_ci_text Add confidence interval text, default TRUE
#'
#' @return NULL (creates plot)
#' @export
#'
#' @examples
#' \dontrun{
#' plot_uncertainty.fb4_result(mle_result)
#' plot_uncertainty.fb4_result(bootstrap_result, parameters = "p_value")
#' }
plot_uncertainty.fb4_result <- function(fb4_result, parameters = "all", 
                                        color_scheme = "blue", add_ci_text = TRUE) {
  
  # Validate input
  if (!is.fb4_result(fb4_result)) {
    stop("Input must be an fb4_result object")
  }
  
  if (!has_uncertainty(fb4_result)) {
    stop("Uncertainty plots require MLE, bootstrap, or hierarchical methods")
  }
  
  method <- fb4_result$summary$method
  colors <- get_color_scheme(color_scheme)
  
  # Route to method-specific function
  switch(method,
    "mle" = plot_mle_uncertainty(fb4_result, parameters, colors, add_ci_text),
    "bootstrap" = plot_bootstrap_uncertainty(fb4_result, parameters, colors, add_ci_text),
    "hierarchical" = plot_hierarchical_uncertainty(fb4_result, parameters, colors, add_ci_text),
    stop("Unknown method with uncertainty: ", method)
  )
}

#' Plot parameter distributions for bootstrap and hierarchical methods
#'
#' @description
#' Shows distributions of parameters from bootstrap samples or 
#' hierarchical individual estimates.
#'
#' @param fb4_result FB4 result object with distribution data
#' @param color_scheme Color scheme to use, default "green"
#' @param show_individuals For hierarchical: show individual estimates, default TRUE
#'
#' @return NULL (creates plot)
#' @export
#'
#' @examples
#' \dontrun{
#' plot_distributions.fb4_result(bootstrap_result)
#' plot_distributions.fb4_result(hierarchical_result)
#' }
plot_distributions.fb4_result <- function(fb4_result, color_scheme = "green", 
                                          show_individuals = TRUE) {
  
  # Validate input
  if (!is.fb4_result(fb4_result)) {
    stop("Input must be an fb4_result object")
  }
  
  method <- fb4_result$summary$method
  
  if (!method %in% c("bootstrap", "hierarchical")) {
    stop("Distributions only available for bootstrap and hierarchical methods")
  }
  
  colors <- get_color_scheme(color_scheme)
  
  # Route to method-specific function
  switch(method,
    "bootstrap" = plot_bootstrap_distributions(fb4_result, colors),
    "hierarchical" = plot_hierarchical_distributions(fb4_result, colors, show_individuals),
    stop("Unknown method: ", method)
  )
}

# ============================================================================
# SENSITIVITY ANALYSIS PLOTS
# ============================================================================

#' Plot sensitivity analysis for FB4 results
#'
#' @description
#' Runs sensitivity analysis internally and creates plots. Called by plot.fb4_result().
#' Does analysis + plotting in one step.
#'
#' @param fb4_result FB4 result object
#' @param temperatures Vector of temperatures to test, default seq(5, 20, 2)
#' @param feeding_levels Vector of feeding levels, default c(0.5, 0.75, 1.0)
#' @param feeding_type Type of feeding levels: "proportion_cmax" or "p_value", default "proportion_cmax"
#' @param color_scheme Color scheme: "grayscale", "color", or color name, default "grayscale"
#' @param add_annotations Add optimal temperature annotations, default TRUE
#' @param verbose Show analysis progress, default FALSE
#' @param ... Additional arguments passed to plot()
#'
#' @return NULL (creates plot)
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple usage
#' plot(fb4_result, type = "sensitivity")
#' 
#' # Custom parameters
#' plot(fb4_result, type = "sensitivity", 
#'      temperatures = seq(8, 18, 1),
#'      feeding_type = "p_value", 
#'      feeding_levels = c(-0.2, 0, 0.2))
#' }
plot_sensitivity.fb4_result <- function(fb4_result, 
                                        temperatures = seq(2, 20, by = 1),
                                        feeding_levels = c(0.5, 0.75, 1.0),
                                        feeding_type = "proportion_cmax",
                                        color_scheme = "grayscale",
                                        add_annotations = TRUE,
                                        verbose = FALSE, ...) {
  
  # Validate input
  if (!is.fb4_result(fb4_result)) {
    stop("Input must be an fb4_result object")
  }
  
  # Step 1: Run sensitivity analysis
  if (verbose) {
    message("Running sensitivity analysis...")
  }
  
  sensitivity_data <- analyze_growth_temperature_sensitivity(
    fb4_result = fb4_result,
    temperatures = temperatures,
    feeding_levels = feeding_levels,
    feeding_type = feeding_type,
    verbose = verbose
  )
  
  # Step 2: Create plot
  plot_growth_vs_temperature(
    sensitivity_data = sensitivity_data,
    fb4_result = fb4_result,
    color_scheme = color_scheme,
    add_annotations = add_annotations,
    ...
  )
  
  if (verbose) {
    message("Sensitivity plot completed")
  }
}

#' Plot growth rate vs temperature
#'
#' @description
#' Creates growth vs temperature plots for sensitivity analysis.
#'
#' @param sensitivity_data Data frame from analyze_growth_temperature_sensitivity
#' @param fb4_result FB4 result object (for title context)
#' @param color_scheme Color scheme to use
#' @param add_annotations Add optimal temperature annotations
#' @param ... Additional arguments
#'
#' @return NULL (creates plot)
#' @keywords internal
plot_growth_vs_temperature <- function(sensitivity_data, fb4_result, 
                                       color_scheme = "grayscale", 
                                       add_annotations = TRUE, ...) {
  
  # Setup plot
  old_par <- setup_plot_layout("single", "default")
  on.exit(graphics::par(old_par))
  
  # Get colors
  colors <- get_color_scheme(color_scheme)
  
  # Create plot
  plot(sensitivity_data$temperature, sensitivity_data$growth_rate,
       type = "l", col = colors$primary, lwd = 2,
       xlab = "Temperature (°C)", ylab = "Growth Rate (g/day)",
       main = "Growth Rate vs Temperature",
       ...)
  
  # Add grid
  graphics::grid(col = "lightgray", lty = "dotted")
  
  # Add annotations if requested
  if (add_annotations) {
    # Find optimal temperature
    opt_temp <- sensitivity_data$temperature[which.max(sensitivity_data$growth_rate)]
    graphics::abline(v = opt_temp, col = colors$accent, lty = 2)
    graphics::text(opt_temp, max(sensitivity_data$growth_rate), 
                   paste("Optimal:", round(opt_temp, 1), "°C"),
                   pos = 4, col = colors$accent)
  }
}

# ============================================================================
# METHOD-SPECIFIC UNCERTAINTY FUNCTIONS
# ============================================================================

#' Plot MLE uncertainty
#'
#' @description
#' Internal function for MLE uncertainty plotting.
#'
#' @param fb4_result MLE result object
#' @param parameters Parameters to plot
#' @param colors Color scheme
#' @param add_ci_text Add confidence interval text
#'
#' @return NULL
#' @keywords internal
plot_mle_uncertainty <- function(fb4_result, parameters, colors, add_ci_text) {
  
  # Setup plot
  old_par <- setup_plot_layout(c(2, 2), "compact")
  on.exit(graphics::par(old_par))
  
  # Extract MLE results
  mle_results <- fb4_result$mle_results
  
  # Plot each parameter
  param_names <- if (parameters == "all") names(mle_results$estimates) else parameters
  
  for (i in seq_along(param_names)) {
    param <- param_names[i]
    
    if (param %in% names(mle_results$estimates)) {
      estimate <- mle_results$estimates[param]
      se <- mle_results$std_errors[param]
      
      # Create confidence interval
      ci_lower <- estimate - 1.96 * se
      ci_upper <- estimate + 1.96 * se
      
      # Plot
      graphics::plot(1, estimate, ylim = c(ci_lower * 0.9, ci_upper * 1.1),
                     xlab = "", ylab = param, main = paste("MLE:", param),
                     pch = 19, col = colors$primary)
      
      # Add confidence interval
      graphics::arrows(1, ci_lower, 1, ci_upper, angle = 90, code = 3, 
                       col = colors$accent, lwd = 2)
      
      if (add_ci_text) {
        graphics::text(1, ci_upper, paste("95% CI:", round(ci_lower, 3), "-", round(ci_upper, 3)),
                       pos = 3, cex = 0.8)
      }
    }
  }
}

#' Plot bootstrap uncertainty
#'
#' @description
#' Internal function for bootstrap uncertainty plotting.
#'
#' @param fb4_result Bootstrap result object
#' @param parameters Parameters to plot
#' @param colors Color scheme
#' @param add_ci_text Add confidence interval text
#'
#' @return NULL
#' @keywords internal
plot_bootstrap_uncertainty <- function(fb4_result, parameters, colors, add_ci_text) {
  
  # Setup plot
  old_par <- setup_plot_layout(c(2, 2), "compact")
  on.exit(graphics::par(old_par))
  
  # Extract bootstrap results
  bootstrap_results <- fb4_result$bootstrap_results
  
  # Plot each parameter
  param_names <- if (parameters == "all") names(bootstrap_results$estimates) else parameters
  
  for (i in seq_along(param_names)) {
    param <- param_names[i]
    
    if (param %in% names(bootstrap_results$estimates)) {
      estimate <- bootstrap_results$estimates[param]
      ci_lower <- bootstrap_results$ci_lower[param]
      ci_upper <- bootstrap_results$ci_upper[param]
      
      # Plot
      graphics::plot(1, estimate, ylim = c(ci_lower * 0.9, ci_upper * 1.1),
                     xlab = "", ylab = param, main = paste("Bootstrap:", param),
                     pch = 19, col = colors$primary)
      
      # Add confidence interval
      graphics::arrows(1, ci_lower, 1, ci_upper, angle = 90, code = 3, 
                       col = colors$accent, lwd = 2)
      
      if (add_ci_text) {
        graphics::text(1, ci_upper, paste("95% CI:", round(ci_lower, 3), "-", round(ci_upper, 3)),
                       pos = 3, cex = 0.8)
      }
    }
  }
}

#' Plot hierarchical uncertainty
#'
#' @description
#' Internal function for hierarchical uncertainty plotting.
#'
#' @param fb4_result Hierarchical result object
#' @param parameters Parameters to plot
#' @param colors Color scheme
#' @param add_ci_text Add confidence interval text
#'
#' @return NULL
#' @keywords internal
plot_hierarchical_uncertainty <- function(fb4_result, parameters, colors, add_ci_text) {
  
  # Setup plot
  old_par <- setup_plot_layout(c(2, 2), "compact")
  on.exit(graphics::par(old_par))
  
  # Extract hierarchical results
  hierarchical_results <- fb4_result$hierarchical_results
  
  # Plot each parameter
  param_names <- if (parameters == "all") names(hierarchical_results$estimates) else parameters
  
  for (i in seq_along(param_names)) {
    param <- param_names[i]
    
    if (param %in% names(hierarchical_results$estimates)) {
      estimate <- hierarchical_results$estimates[param]
      ci_lower <- hierarchical_results$ci_lower[param]
      ci_upper <- hierarchical_results$ci_upper[param]
      
      # Plot
      graphics::plot(1, estimate, ylim = c(ci_lower * 0.9, ci_upper * 1.1),
                     xlab = "", ylab = param, main = paste("Hierarchical:", param),
                     pch = 19, col = colors$primary)
      
      # Add confidence interval
      graphics::arrows(1, ci_lower, 1, ci_upper, angle = 90, code = 3, 
                       col = colors$accent, lwd = 2)
      
      if (add_ci_text) {
        graphics::text(1, ci_upper, paste("95% CI:", round(ci_lower, 3), "-", round(ci_upper, 3)),
                       pos = 3, cex = 0.8)
      }
    }
  }
}

# ============================================================================
# DISTRIBUTION PLOTTING FUNCTIONS
# ============================================================================

#' Plot bootstrap distributions
#'
#' @description
#' Internal function for bootstrap distribution plotting.
#'
#' @param fb4_result Bootstrap result object
#' @param colors Color scheme
#'
#' @return NULL
#' @keywords internal
plot_bootstrap_distributions <- function(fb4_result, colors) {
  
  # Setup plot
  old_par <- setup_plot_layout(c(2, 2), "compact")
  on.exit(graphics::par(old_par))
  
  # Extract bootstrap samples
  bootstrap_samples <- fb4_result$bootstrap_results$samples
  
  # Plot histograms for each parameter
  param_names <- names(bootstrap_samples)
  
  for (i in seq_along(param_names)) {
    param <- param_names[i]
    samples <- bootstrap_samples[[param]]
    
    # Create histogram
    graphics::hist(samples, main = paste("Bootstrap:", param),
                   xlab = param, col = colors$main, border = colors$primary,
                   probability = TRUE)
    
    # Add density line
    density_obj <- stats::density(samples, na.rm = TRUE)
    graphics::lines(density_obj, col = colors$accent, lwd = 2)
  }
}

#' Plot hierarchical distributions
#'
#' @description
#' Internal function for hierarchical distribution plotting.
#'
#' @param fb4_result Hierarchical result object
#' @param colors Color scheme
#' @param show_individuals Show individual estimates
#'
#' @return NULL
#' @keywords internal
plot_hierarchical_distributions <- function(fb4_result, colors, show_individuals) {
  
  # Setup plot
  old_par <- setup_plot_layout(c(2, 2), "compact")
  on.exit(graphics::par(old_par))
  
  # Extract hierarchical results
  hierarchical_results <- fb4_result$hierarchical_results
  
  # Plot each parameter
  param_names <- names(hierarchical_results$individual_estimates)
  
  for (i in seq_along(param_names)) {
    param <- param_names[i]
    individual_ests <- hierarchical_results$individual_estimates[[param]]
    population_est <- hierarchical_results$estimates[param]
    
    # Create histogram
    graphics::hist(individual_ests, main = paste("Hierarchical:", param),
                   xlab = param, col = colors$main, border = colors$primary,
                   probability = TRUE)
    
    # Add population estimate line
    graphics::abline(v = population_est, col = colors$accent, lwd = 2, lty = 2)
    
    # Add individual points if requested
    if (show_individuals) {
      graphics::rug(individual_ests, col = colors$secondary)
    }
  }
}