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
#' \donttest{
#' data(fish4_parameters)
#' sp   <- fish4_parameters[["Oncorhynchus tshawytscha"]]$life_stages$adult
#' info <- fish4_parameters[["Oncorhynchus tshawytscha"]]$species_info
#' bio  <- Bioenergetic(
#'   species_params     = sp,
#'   species_info       = info,
#'   environmental_data = list(
#'     temperature = data.frame(Day = 1:30, Temperature = rep(12, 30))
#'   ),
#'   diet_data = list(
#'     proportions = data.frame(Day = 1:30, Prey1 = 1.0),
#'     energies    = data.frame(Day = 1:30, Prey1 = 5000),
#'     prey_names  = "Prey1"
#'   ),
#'   simulation_settings = list(initial_weight = 100, duration = 30)
#' )
#' bio$species_params$predator$ED_ini <- 5000
#' bio$species_params$predator$ED_end <- 5500
#' set.seed(42)
#' obs_weights <- rnorm(10, mean = 90, sd = 5)
#' result_mle <- run_fb4(bio, strategy = "mle", fit_to = "Weight",
#'                       observed_weights = obs_weights, verbose = FALSE)
#' plot_uncertainty.fb4_result(result_mle)
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
#' \donttest{
#' data(fish4_parameters)
#' sp   <- fish4_parameters[["Oncorhynchus tshawytscha"]]$life_stages$adult
#' info <- fish4_parameters[["Oncorhynchus tshawytscha"]]$species_info
#' bio  <- Bioenergetic(
#'   species_params     = sp,
#'   species_info       = info,
#'   environmental_data = list(
#'     temperature = data.frame(Day = 1:30, Temperature = rep(12, 30))
#'   ),
#'   diet_data = list(
#'     proportions = data.frame(Day = 1:30, Prey1 = 1.0),
#'     energies    = data.frame(Day = 1:30, Prey1 = 5000),
#'     prey_names  = "Prey1"
#'   ),
#'   simulation_settings = list(initial_weight = 100, duration = 30)
#' )
#' bio$species_params$predator$ED_ini <- 5000
#' bio$species_params$predator$ED_end <- 5500
#' set.seed(42)
#' obs_weights <- rnorm(10, mean = 90, sd = 5)
#' result_boot <- run_fb4(bio, strategy = "bootstrap", fit_to = "Weight",
#'                        observed_weights = obs_weights, n_bootstrap = 20,
#'                        verbose = FALSE)
#' plot_distributions.fb4_result(result_boot)
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

#' Plot temperature sensitivity analysis for a Bioenergetic object
#'
#' @description
#' Runs \code{analyze_growth_temperature_sensitivity} and plots the result.
#' Sensitivity analysis requires a \code{Bioenergetic} object (not an
#' \code{fb4_result}), because it re-runs the model across a grid of
#' temperatures and p_values. Use \code{plot(bio_obj, type = "sensitivity")}
#' as the primary interface; this function is the underlying implementation.
#'
#' @param bio_obj Bioenergetic object with species parameters, temperature
#'   profile, diet, and simulation settings.
#' @param temperatures Numeric vector of absolute temperatures (°C) to test.
#'   Default \code{seq(4, 20, by = 2)}.
#' @param p_values Numeric vector of p_values (proportion of Cmax) to
#'   evaluate. Must be in (0, 1]. Default \code{seq(0.3, 1.0, by = 0.1)}.
#' @param simulation_days Number of simulation days. Default 365.
#' @param color_scheme Color scheme for the plot. Default \code{"grayscale"}.
#' @param add_annotations Add optimal temperature annotations. Default TRUE.
#' @param verbose Show analysis progress. Default FALSE.
#' @param ... Additional arguments passed to \code{plot_growth_temperature_sensitivity()}.
#'
#' @return NULL (creates plot)
#' @export
#'
#' @examples
#' \donttest{
#' data(fish4_parameters)
#' sp   <- fish4_parameters[["Oncorhynchus tshawytscha"]]$life_stages$adult
#' info <- fish4_parameters[["Oncorhynchus tshawytscha"]]$species_info
#' bio  <- Bioenergetic(
#'   species_params     = sp,
#'   species_info       = info,
#'   environmental_data = list(
#'     temperature = data.frame(Day = 1:30, Temperature = rep(12, 30))
#'   ),
#'   diet_data = list(
#'     proportions = data.frame(Day = 1:30, Prey1 = 1.0),
#'     energies    = data.frame(Day = 1:30, Prey1 = 5000),
#'     prey_names  = "Prey1"
#'   ),
#'   simulation_settings = list(initial_weight = 100, duration = 30)
#' )
#' bio$species_params$predator$ED_ini <- 5000
#' bio$species_params$predator$ED_end <- 5500
#' plot_sensitivity.fb4_result(
#'   bio_obj         = bio,
#'   temperatures    = c(10, 14),
#'   p_values        = c(0.4, 0.7),
#'   simulation_days = 30,
#'   verbose         = FALSE
#' )
#' }
plot_sensitivity.fb4_result <- function(bio_obj,
                                        temperatures    = seq(4, 20, by = 2),
                                        p_values        = seq(0.3, 1.0, by = 0.1),
                                        simulation_days = 365,
                                        color_scheme    = "grayscale",
                                        add_annotations = TRUE,
                                        verbose         = FALSE, ...) {

  if (!is.Bioenergetic(bio_obj)) {
    stop("bio_obj must be a Bioenergetic object. ",
         "Use plot(bio_obj, type = 'sensitivity') as the primary interface.")
  }

  if (verbose) message("Running temperature sensitivity analysis...")

  sensitivity_data <- analyze_growth_temperature_sensitivity(
    bio_obj         = bio_obj,
    temperatures    = temperatures,
    p_values        = p_values,
    simulation_days = simulation_days,
    verbose         = verbose
  )

  plot_growth_temperature_sensitivity(
    sensitivity_data = sensitivity_data,
    ...
  )

  if (verbose) message("Sensitivity plot completed.")
  invisible(NULL)
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
       xlab = "Temperature (\u00b0C)", ylab = "Growth Rate (g/day)",
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
                   paste("Optimal:", round(opt_temp, 1), "\u00b0C"),
                   pos = 4, col = colors$accent)
  }
}

# ============================================================================
# METHOD-SPECIFIC UNCERTAINTY FUNCTIONS
# ============================================================================

#' Draw a single parameter estimate panel with confidence interval
#'
#' @description
#' Internal helper shared by \code{plot_mle_uncertainty},
#' \code{plot_bootstrap_uncertainty}, and
#' \code{plot_hierarchical_uncertainty}.  Draws a dot-and-arrow panel for one
#' parameter — estimate point, CI arrow, and optional CI text — so the
#' identical plotting code is not triplicated.
#'
#' @param estimate  Point estimate for the parameter.
#' @param ci_lower  Lower confidence bound.
#' @param ci_upper  Upper confidence bound.
#' @param param_name  Character: parameter name, used as axis label and title.
#' @param method_label  Character prefix for the panel title (e.g. \code{"MLE"}).
#' @param colors  Color scheme list from \code{get_color_scheme()}.
#' @param add_ci_text  Logical; add CI text above the arrow.
#'
#' @return \code{NULL} (modifies current graphics device).
#' @keywords internal
plot_estimate_panel <- function(estimate, ci_lower, ci_upper,
                                param_name, method_label, colors, add_ci_text) {
  graphics::plot(1, estimate,
                 ylim = c(ci_lower * 0.9, ci_upper * 1.1),
                 xlab = "", ylab = param_name,
                 main = paste0(method_label, ": ", param_name),
                 pch  = 19, col = colors$primary)
  graphics::arrows(1, ci_lower, 1, ci_upper,
                   angle = 90, code = 3, col = colors$accent, lwd = 2)
  if (add_ci_text) {
    graphics::text(1, ci_upper,
                   paste("95% CI:", round(ci_lower, 3), "-", round(ci_upper, 3)),
                   pos = 3, cex = 0.8)
  }
}

#' Plot MLE uncertainty
#'
#' @description Internal function for MLE uncertainty plotting.
#' @param fb4_result MLE result object
#' @param parameters Parameters to plot
#' @param colors Color scheme
#' @param add_ci_text Add confidence interval text
#' @return NULL
#' @keywords internal
plot_mle_uncertainty <- function(fb4_result, parameters, colors, add_ci_text) {
  old_par <- setup_plot_layout(c(2, 2), "compact")
  on.exit(graphics::par(old_par))

  # MLE data lives in result$method_data (built by create_method_specific_data)
  md  <- fb4_result$method_data
  ci  <- md$confidence_intervals %||% list()

  p_est      <- fb4_result$summary$p_estimate
  p_ci_lower <- ci$p_ci_lower %||% NA_real_
  p_ci_upper <- ci$p_ci_upper %||% NA_real_

  # Panel 1: p_value estimate with confidence interval
  plot_estimate_panel(p_est, p_ci_lower, p_ci_upper,
                      "p_value", "MLE", colors, add_ci_text)

  # Panel 2: sigma estimate (measurement-error parameter)
  sigma_est <- md$sigma_estimate %||% NA_real_
  sigma_se  <- md$sigma_se      %||% NA_real_
  if (!is.na(sigma_est)) {
    sigma_lo <- if (!is.na(sigma_se)) sigma_est - 1.96 * sigma_se else NA_real_
    sigma_hi <- if (!is.na(sigma_se)) sigma_est + 1.96 * sigma_se else NA_real_
    plot_estimate_panel(sigma_est, sigma_lo, sigma_hi,
                        "sigma", "MLE", colors, add_ci_text)
  }

  # Panel 3: likelihood profile (optional — only when compute_profile = TRUE)
  pl <- md$profile_likelihood
  if (!is.null(pl) && is.data.frame(pl) && nrow(pl) > 0 &&
      all(c("p_value", "log_likelihood") %in% names(pl))) {

    graphics::plot(pl$p_value, pl$log_likelihood,
                   type = "l", lwd = 2, col = colors$primary,
                   xlab = "p_value", ylab = "Log-likelihood",
                   main = "Likelihood Profile")
    graphics::abline(v = p_est,
                     col = colors$accent, lty = 2, lwd = 1.5)
    if (!is.na(p_ci_lower) && !is.na(p_ci_upper)) {
      graphics::abline(v = c(p_ci_lower, p_ci_upper),
                       col = colors$secondary, lty = 3)
    }
  }

  # Panel 4: AIC / log-likelihood summary text
  aic <- md$aic %||% NA_real_
  ll  <- md$log_likelihood %||% NA_real_
  if (!is.na(aic) || !is.na(ll)) {
    graphics::plot.new()
    lines_txt <- c(
      "MLE Fit Statistics",
      "",
      if (!is.na(p_est))   sprintf("p_value : %.4f",  p_est)  else NULL,
      if (!is.na(sigma_est)) sprintf("sigma   : %.4f", sigma_est) else NULL,
      if (!is.na(ll))      sprintf("Log-lik : %.2f",  ll)     else NULL,
      if (!is.na(aic))     sprintf("AIC     : %.2f",  aic)    else NULL,
      if (!is.na(md$confidence_level)) {
        sprintf("CI level: %.0f%%", md$confidence_level * 100)
      } else NULL
    )
    graphics::text(0.5, 0.5, paste(lines_txt, collapse = "\n"),
                   cex = 1.0, family = "mono", adj = c(0.5, 0.5))
  }
}

#' Plot bootstrap uncertainty
#'
#' @description Internal function for bootstrap uncertainty plotting.
#' @param fb4_result Bootstrap result object
#' @param parameters Parameters to plot
#' @param colors Color scheme
#' @param add_ci_text Add confidence interval text
#' @return NULL
#' @keywords internal
plot_bootstrap_uncertainty <- function(fb4_result, parameters, colors, add_ci_text) {
  old_par <- setup_plot_layout(c(2, 2), "compact")
  on.exit(graphics::par(old_par))

  # Bootstrap data lives in result$method_data (built by create_method_specific_data)
  md   <- fb4_result$method_data
  bs   <- md$bootstrap_results   %||% list()   # p_values, consumption_values, predicted_weights
  ci   <- md$confidence_intervals %||% list()  # p_ci_lower/upper, consumption_ci_lower/upper
  info <- md$bootstrap_info       %||% list()  # n_bootstrap, successful_iterations, success_rate

  p_mean <- fb4_result$summary$p_mean %||% NA_real_
  n_ok   <- info$successful_iterations %||% length(bs$p_values %||% c())
  n_tot  <- info$n_bootstrap           %||% n_ok
  sr     <- info$success_rate          %||% if (n_tot > 0) n_ok / n_tot else NA_real_

  # Panel 1: histogram of bootstrap p_values
  p_vals <- bs$p_values
  if (!is.null(p_vals) && length(p_vals) > 0) {
    main_txt <- sprintf("Bootstrap p_value  (n=%d/%d | %.0f%% success)",
                        n_ok, n_tot, (sr %||% 1) * 100)
    graphics::hist(p_vals, breaks = 30,
                   col = colors$fill %||% "lightblue",
                   border = colors$secondary %||% "steelblue",
                   xlab = "p_value", ylab = "Frequency", main = main_txt)
    graphics::abline(v = p_mean, col = colors$primary, lwd = 2)
    if (!is.null(ci$p_ci_lower) && !is.na(ci$p_ci_lower)) {
      graphics::abline(v = c(ci$p_ci_lower, ci$p_ci_upper),
                       col = colors$accent %||% "red", lty = 2, lwd = 1.5)
    }
  }

  # Panel 2: p_value point estimate with CI bar
  plot_estimate_panel(p_mean, ci$p_ci_lower %||% NA_real_,
                      ci$p_ci_upper %||% NA_real_,
                      "p_value", "Bootstrap", colors, add_ci_text)

  # Panel 3: histogram of bootstrap consumption values
  cons_vals <- bs$consumption_values
  if (!is.null(cons_vals) && length(cons_vals) > 0) {
    graphics::hist(cons_vals, breaks = 30,
                   col = colors$fill %||% "lightblue",
                   border = colors$secondary %||% "steelblue",
                   xlab = "Total Consumption (g)", ylab = "Frequency",
                   main = "Bootstrap Consumption Distribution")
    c_mean <- mean(cons_vals, na.rm = TRUE)
    graphics::abline(v = c_mean, col = colors$primary, lwd = 2)
    if (!is.null(ci$consumption_ci_lower) && !is.na(ci$consumption_ci_lower)) {
      graphics::abline(v = c(ci$consumption_ci_lower, ci$consumption_ci_upper),
                       col = colors$accent %||% "red", lty = 2, lwd = 1.5)
    }
  }

  # Panel 4: histogram of predicted final weights (if stored)
  pred_w <- bs$predicted_weights
  if (!is.null(pred_w) && length(pred_w) > 0) {
    graphics::hist(pred_w, breaks = 30,
                   col = colors$fill %||% "lightblue",
                   border = colors$secondary %||% "steelblue",
                   xlab = "Predicted Final Weight (g)", ylab = "Frequency",
                   main = "Bootstrap Predicted Weights")
    graphics::abline(v = mean(pred_w, na.rm = TRUE),
                     col = colors$primary, lwd = 2)
  }
}

#' Plot hierarchical uncertainty
#'
#' @description Internal function for hierarchical uncertainty plotting.
#' @param fb4_result Hierarchical result object
#' @param parameters Parameters to plot
#' @param colors Color scheme
#' @param add_ci_text Add confidence interval text
#' @return NULL
#' @keywords internal
plot_hierarchical_uncertainty <- function(fb4_result, parameters, colors, add_ci_text) {
  old_par <- setup_plot_layout(c(2, 2), "compact")
  on.exit(graphics::par(old_par))

  hr     <- fb4_result$hierarchical_results
  params <- if (parameters == "all") names(hr$estimates) else parameters

  for (param in params) {
    if (param %in% names(hr$estimates)) {
      plot_estimate_panel(hr$estimates[param], hr$ci_lower[param], hr$ci_upper[param],
                          param, "Hierarchical", colors, add_ci_text)
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

# ============================================================================
# SENSITIVITY PLOT
# ============================================================================

#' Plot sensitivity analysis
#'
#' @description
#' Creates sensitivity analysis plots for temperature and feeding effects.
#'
#' @param sensitivity_data Data frame from \code{analyze_growth_temperature_sensitivity}
#' @param temperatures Temperature values to test
#' @param feeding_levels Feeding levels to test
#' @param species Optional species name for plot title
#' @param ylim Optional y-axis limits
#' @param xlim Optional x-axis limits
#' @param colors Color scheme
#' @param verbose Show progress messages
#' @param ... Additional arguments
#'
#' @return NULL (creates plot)
#' @export
plot_growth_temperature_sensitivity <- function(sensitivity_data,
                                                temperatures = seq(5, 20, by = 2),
                                                feeding_levels = c(0.5, 0.75, 1.0),
                                                species = NULL,
                                                ylim = NULL,
                                                xlim = NULL,
                                                colors = "grayscale",
                                                verbose = FALSE, ...) {
  
  if (verbose) message("Running sensitivity analysis...")
  
  # Remove failed runs
  valid_results <- sensitivity_data[!is.na(sensitivity_data$daily_growth_rate), ]
  
  if (nrow(valid_results) == 0) {
    stop("No valid sensitivity results to plot")
  }
  
  # Get unique feeding levels
  feeding_levels <- sort(unique(valid_results$feeding_pct), decreasing = TRUE)
  n_levels <- length(feeding_levels)
  
  # Setup colors
  if (colors == "grayscale") {
    plot_colors <- gray.colors(n = n_levels, start = 0.1, end = 0.9)
  } else if (colors == "color") {
    plot_colors <- rainbow(n_levels)
  } else {
    cols <- get_color_scheme(colors)
    plot_colors <- rep(c(cols$primary, cols$secondary, cols$accent), 
                      length.out = n_levels)
  }
  
  # Line and point types
  line_types <- rep(1:5, length.out = n_levels)
  point_types <- rep(c(19, 1, 2, 0, 17), length.out = n_levels)
  
  # Setup plot
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))
  
  # Create title
  main_title <- if (!is.null(species)) {
    paste("Temperature & Feeding Effects -", species)
  } else {
    "Temperature & Feeding Effects on Growth"
  }
  
  # Create plot
  plot(valid_results$temperature, valid_results$daily_growth_rate,
       type = "n",
       xlab = "Temperature (\u00b0C)",
       ylab = "Daily Growth Rate (g/g/day)",
       main = main_title,
       ylim = ylim %||% range(valid_results$daily_growth_rate, na.rm = TRUE),
       xlim = xlim %||% range(valid_results$temperature, na.rm = TRUE))
  
  abline(h = 0, col = "black", lty = 2, lwd = 1)
  grid(col = "lightgray", lty = "dotted")
  
  # Add lines for each feeding level
  for (i in seq_along(feeding_levels)) {
    feeding_pct <- feeding_levels[i]
    feeding_data <- valid_results[valid_results$feeding_pct == feeding_pct, ]
    feeding_data <- feeding_data[order(feeding_data$temperature), ]
    
    lines(feeding_data$temperature, feeding_data$daily_growth_rate,
          col = plot_colors[i], lwd = 2, lty = line_types[i])
    
    points(feeding_data$temperature, feeding_data$daily_growth_rate,
           col = plot_colors[i], pch = point_types[i], cex = 1.2)
  }
  
  # Legend
  legend_labels <- paste("P =", round(feeding_levels, 3))
  
  legend("topright",
         legend = legend_labels,
         col = plot_colors,
         lwd = 2,
         pch = point_types,
         lty = line_types,
         cex = 0.9,
         bg = "white")
  
  # Add optimal temperature annotation
  if (length(feeding_levels) > 0) {
    top_feeding <- feeding_levels[1]
    top_data <- valid_results[valid_results$feeding_pct == top_feeding, ]
    
    if (nrow(top_data) > 0) {
      max_growth_idx <- which.max(top_data$daily_growth_rate)
      optimal_temp <- top_data$temperature[max_growth_idx]
      max_growth <- top_data$daily_growth_rate[max_growth_idx]
      
      abline(v = optimal_temp, col = "red", lty = 2, lwd = 1)
      text(optimal_temp + 1, max_growth * 0.8,
           paste("Optimal\n", optimal_temp, "\u00b0C"),
           col = "red", cex = 0.8, adj = 0)
    }
  }
  
  if (verbose) message("Sensitivity plot completed")
}

