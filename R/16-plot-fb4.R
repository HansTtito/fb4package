#' FB4 Plotting Functions
#'
#' @description
#' Main plotting functions for FB4 bioenergetic model results.
#' Provides S3 methods and plot types for fb4_result and Bioenergetic objects.
#'
#' @name fb4-plots
# #' @importFrom graphics plot par mfrow mar oma lines points abline text legend grid hist barplot polygon arrows rug
#' @importFrom grDevices png pdf dev.off rgb col2rgb rainbow gray.colors
#' @importFrom stats density lowess cor sd
#' @importFrom utils tail
#' @importFrom tools file_ext
NULL

# ============================================================================
# S3 PLOT METHOD FOR FB4_RESULT
# ============================================================================

#' Plot FB4 simulation results
#'
#' @description
#' Main plotting method for fb4_result objects. Automatically detects
#' available data and provides appropriate visualizations.
#'
#' @param x Object of class fb4_result
#' @param type Type of plot: "dashboard", "growth", "consumption", "temperature",
#'   "energy", "uncertainty", "sensitivity"
#' @param save_plot Optional path to save plot (.png or .pdf)
#' @param ... Additional arguments passed to specific plot functions
#'
#' @return Invisibly returns the input object
#' @export
#'
#' @examples
#' \dontrun{
#' plot(result)                          # Dashboard
#' plot(result, type = "growth")         # Growth trajectory
#' plot(result, type = "uncertainty")    # If MLE/bootstrap/hierarchical
#' plot(result, save_plot = "output.png")
#' }
plot.fb4_result <- function(x, type = "dashboard", save_plot = NULL, ...) {
  
  # Validate input
  if (!is.fb4_result(x)) {
    stop("Input must be an fb4_result object")
  }
  
  # Check data availability
  if (is.null(x$daily_output) && type != "uncertainty") {
    stop("No daily output data available for plotting")
  }
  
  # Setup save device if requested
  if (!is.null(save_plot)) {
    setup_save_device(save_plot)
    on.exit(close_save_device(save_plot))
  }
  
  # Route to appropriate plot
  switch(type,
    "dashboard" = plot_dashboard(x, ...),
    "growth" = plot_growth(x, ...),
    "consumption" = plot_consumption(x, ...),
    "temperature" = plot_temperature(x, ...),
    "energy" = plot_energy(x, ...),
    "uncertainty" = plot_uncertainty(x, ...),
    stop("Unknown plot type: '", type, "'. Available: dashboard, growth, ",
         "consumption, temperature, energy, uncertainty, sensitivity")
  )
  
  invisible(x)
}

# ============================================================================
# S3 PLOT METHOD FOR BIOENERGETIC
# ============================================================================

#' Plot Bioenergetic object setup
#'
#' @description
#' Plotting method for Bioenergetic objects to validate setup before simulation.
#'
#' @param x Object of class Bioenergetic
#' @param type Type of plot: "dashboard", "temperature", "diet", "energy"
#' @param save_plot Optional path to save plot
#' @param ... Additional arguments
#'
#' @return Invisibly returns the input object
#' @export
plot.Bioenergetic <- function(x, type = "dashboard", save_plot = NULL, ...) {
  
  if (!is.Bioenergetic(x)) {
    stop("Input must be a Bioenergetic object")
  }
  
  if (!is.null(save_plot)) {
    setup_save_device(save_plot)
    on.exit(close_save_device(save_plot))
  }
  
  if (type == "sensitivity") {
    # Capturar argumentos
    dots <- list(...)
    
    # Argumentos válidos para el análisis
    analysis_args <- dots[names(dots) %in% names(formals(analyze_growth_temperature_sensitivity))]
    
    # Argumentos restantes (para el plot)
    plot_args <- dots[!names(dots) %in% names(formals(analyze_growth_temperature_sensitivity))]
    
    # Ejecutar análisis
    sensitivity_data <- do.call(analyze_growth_temperature_sensitivity,
                                 c(list(bio_obj = x), analysis_args))
    
    # Ejecutar plot
    do.call(plot_growth_temperature_sensitivity,
            c(list(sensitivity_data = sensitivity_data), plot_args))
    
  } else {
    switch(type,
           "dashboard"  = plot_bio_dashboard(x, ...),
           "temperature"= plot_bio_temperature(x, ...),
           "diet"       = plot_bio_diet(x, ...),
           "energy"     = plot_bio_energy(x, ...),
           stop("Unknown plot type: ", type)
    )
  }
  
  invisible(x)
}



# ============================================================================
# DASHBOARD PLOT
# ============================================================================

#' Create dashboard overview
#'
#' @description
#' Creates a 4-panel dashboard showing key simulation results.
#'
#' @param fb4_result FB4 result object
#' @param colors Color scheme name or custom colors
#' @param title Main title for dashboard
#' @param ... Additional arguments (unused)
#'
#' @return NULL (creates plot)
#' @keywords internal
plot_dashboard <- function(fb4_result, colors = "blue", title = NULL, ...) {
  
  daily <- fb4_result$daily_output
  if (is.null(daily)) stop("No daily output data available")
  
  # Setup 2x2 layout
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2), oma = c(0, 0, 2, 0))
  
  cols <- get_color_scheme(colors)
  
  # Panel 1: Growth
  plot(daily$Day, daily$Weight, type = "l", lwd = 2, col = cols$primary,
       xlab = "Day", ylab = "Weight (g)", main = "Growth", las = 1)
  grid(col = "lightgray", lty = "dotted")
  
  # Add growth stats
  initial_w <- daily$Weight[1]
  final_w <- tail(daily$Weight, 1)
  growth_pct <- round((final_w/initial_w - 1) * 100, 1)
  legend("topleft", legend = paste("Growth:", growth_pct, "%"),
         bty = "n", text.col = cols$primary, cex = 0.9)
  
  # Panel 2: Consumption
  if ("Consumption_gg" %in% names(daily)) {
    plot(daily$Day, daily$Consumption_gg, type = "l", lwd = 2, col = cols$secondary,
         xlab = "Day", ylab = "Consumption (g/g/day)", main = "Consumption Rate", las = 1)
    grid(col = "lightgray", lty = "dotted")
    mean_cons <- mean(daily$Consumption_gg, na.rm = TRUE)
    abline(h = mean_cons, col = cols$accent, lty = 2)
    legend("topright", legend = paste("Mean:", round(mean_cons, 4)),
           bty = "n", text.col = cols$secondary, cex = 0.9)
  } else {
    plot.new()
    text(0.5, 0.5, "No consumption data", cex = 1.2, col = "gray")
  }
  
  # Panel 3: Temperature
  if ("Temperature" %in% names(daily)) {
    plot(daily$Day, daily$Temperature, type = "l", lwd = 2, col = cols$accent,
         xlab = "Day", ylab = "Temperature (°C)", main = "Temperature", las = 1)
    grid(col = "lightgray", lty = "dotted")
    mean_temp <- mean(daily$Temperature, na.rm = TRUE)
    legend("topright", legend = paste("Mean:", round(mean_temp, 1), "°C"),
           bty = "n", text.col = cols$accent, cex = 0.9)
  } else {
    plot.new()
    text(0.5, 0.5, "No temperature data", cex = 1.2, col = "gray")
  }
  
  # Panel 4: Summary or Energy
  if (all(c("Consumption_energy", "Net_energy") %in% names(daily))) {
    efficiency <- daily$Net_energy / daily$Consumption_energy * 100
    efficiency[!is.finite(efficiency)] <- 0
    plot(daily$Day, efficiency, type = "l", lwd = 2, col = "purple",
         xlab = "Day", ylab = "Efficiency (%)", main = "Growth Efficiency", las = 1)
    grid(col = "lightgray", lty = "dotted")
    mean_eff <- mean(efficiency[efficiency > 0], na.rm = TRUE)
    if (is.finite(mean_eff)) {
      legend("topright", legend = paste("Mean:", round(mean_eff, 1), "%"),
             bty = "n", text.col = "purple", cex = 0.9)
    }
  } else {
    # Summary panel
    plot.new()
    summary_text <- c(
      paste("Method:", fb4_result$summary$method),
      paste("Duration:", max(daily$Day), "days"),
      paste("Initial:", round(initial_w, 1), "g"),
      paste("Final:", round(final_w, 1), "g")
    )
    if (!is.null(fb4_result$summary$p_value)) {
      summary_text <- c(summary_text, paste("p-value:", round(fb4_result$summary$p_value, 4)))
    }
    text(0.1, seq(0.8, 0.2, length.out = length(summary_text)),
         summary_text, adj = 0, cex = 0.9)
  }
  
  # Main title
  if (is.null(title)) {
    species <- extract_species_name(fb4_result)
    title <- if (!is.null(species)) {
      paste("FB4 Results:", species)
    } else {
      "FB4 Simulation Results"
    }
  }
  mtext(title, outer = TRUE, cex = 1.2, font = 2)
}

# ============================================================================
# GROWTH PLOT
# ============================================================================

#' Plot growth trajectory
#'
#' @description
#' Creates growth trajectory plots with optional growth rate subplot.
#'
#' @param fb4_result FB4 result object
#' @param show_rate Show growth rate subplot
#' @param colors Color scheme
#' @param smooth Add smoothed trend
#' @param ... Additional arguments
#'
#' @return NULL (creates plot)
#' @keywords internal
plot_growth <- function(fb4_result, show_rate = TRUE, colors = "blue", smooth = FALSE, ...) {
  
  daily <- fb4_result$daily_output
  if (is.null(daily) || !"Weight" %in% names(daily)) {
    stop("No weight data available")
  }
  
  cols <- get_color_scheme(colors)
  
  # Setup layout
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  if (show_rate) {
    par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))
  } else {
    par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))
  }
  
  # Weight trajectory
  plot(daily$Day, daily$Weight, type = "l", lwd = 2, col = cols$primary,
       xlab = "Day", ylab = "Weight (g)", main = "Weight Trajectory", las = 1)
  grid(col = "lightgray", lty = "dotted")
  
  # Add smooth trend if requested
  if (smooth && nrow(daily) > 10) {
    smooth_w <- lowess(daily$Day, daily$Weight, f = 0.2)
    lines(smooth_w$x, smooth_w$y, col = cols$secondary, lwd = 2, lty = 2)
  }
  
  # Statistics
  initial_w <- daily$Weight[1]
  final_w <- tail(daily$Weight, 1)
  growth_total <- final_w - initial_w
  growth_pct <- (final_w/initial_w - 1) * 100
  
  stats_text <- c(
    paste("Initial:", round(initial_w, 2), "g"),
    paste("Final:", round(final_w, 2), "g"),
    paste("Growth:", round(growth_total, 2), "g"),
    paste("Relative:", round(growth_pct, 1), "%")
  )
  legend("topleft", legend = stats_text, bty = "o", bg = "white", cex = 0.8)
  
  # Growth rate subplot
  if (show_rate && nrow(daily) > 1) {
    growth_rate <- diff(daily$Weight)
    days_rate <- daily$Day[-1]
    
    plot(days_rate, growth_rate, type = "l", lwd = 2, col = cols$secondary,
         xlab = "Day", ylab = "Daily Growth Rate (g/day)", 
         main = "Daily Growth Rate", las = 1)
    grid(col = "lightgray", lty = "dotted")
    abline(h = 0, col = "red", lty = 2)
    
    mean_rate <- mean(growth_rate, na.rm = TRUE)
    legend("topright", 
           legend = c(paste("Mean:", round(mean_rate, 3), "g/day"),
                     paste("Max:", round(max(growth_rate), 3), "g/day"),
                     paste("Min:", round(min(growth_rate), 3), "g/day")),
           bty = "o", bg = "white", cex = 0.8)
  }
}

# ============================================================================
# CONSUMPTION PLOT
# ============================================================================

#' Plot consumption patterns
#'
#' @description
#' Creates consumption rate plots with optional cumulative consumption.
#'
#' @param fb4_result FB4 result object
#' @param show_cumulative Show cumulative consumption subplot
#' @param colors Color scheme
#' @param ... Additional arguments
#'
#' @return NULL (creates plot)
#' @keywords internal
plot_consumption <- function(fb4_result, show_cumulative = TRUE, colors = "green", ...) {
  
  daily <- fb4_result$daily_output
  if (is.null(daily) || !"Consumption_gg" %in% names(daily)) {
    stop("No consumption data available")
  }
  
  cols <- get_color_scheme(colors)
  
  # Setup layout
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  if (show_cumulative) {
    par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))
  } else {
    par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))
  }
  
  # Specific consumption rate
  plot(daily$Day, daily$Consumption_gg, type = "l", lwd = 2, col = cols$primary,
       xlab = "Day", ylab = "Consumption Rate (g/g/day)", 
       main = "Specific Consumption Rate", las = 1)
  grid(col = "lightgray", lty = "dotted")
  
  mean_cons <- mean(daily$Consumption_gg, na.rm = TRUE)
  abline(h = mean_cons, col = cols$accent, lty = 2, lwd = 1)
  
  # Statistics
  stats_text <- c(
    paste("Mean:", round(mean_cons, 4)),
    paste("Max:", round(max(daily$Consumption_gg, na.rm = TRUE), 4)),
    paste("Min:", round(min(daily$Consumption_gg, na.rm = TRUE), 4)),
    paste("CV:", round(sd(daily$Consumption_gg, na.rm = TRUE) / mean_cons * 100, 1), "%")
  )
  legend("topright", legend = stats_text, bty = "o", bg = "white", cex = 0.8)
  
  # Cumulative consumption
  if (show_cumulative && "Weight" %in% names(daily)) {
    total_daily <- daily$Consumption_gg * daily$Weight
    cumulative <- cumsum(total_daily)
    
    plot(daily$Day, cumulative, type = "l", lwd = 2, col = cols$secondary,
         xlab = "Day", ylab = "Cumulative Consumption (g)", 
         main = "Cumulative Food Consumption", las = 1)
    grid(col = "lightgray", lty = "dotted")
    
    total <- tail(cumulative, 1)
    daily_avg <- total / length(daily$Day)
    
    legend("bottomright", 
           legend = c(paste("Total:", round(total, 1), "g"),
                     paste("Daily avg:", round(daily_avg, 2), "g/day")),
           bty = "o", bg = "white", cex = 0.8)
  }
}

# ============================================================================
# TEMPERATURE PLOT
# ============================================================================

#' Plot temperature profile
#'
#' @description
#' Creates temperature plots with optional consumption correlation.
#'
#' @param fb4_result FB4 result object
#' @param show_correlation Show temperature-consumption relationship
#' @param colors Color scheme
#' @param smooth Add smoothed trend
#' @param ... Additional arguments
#'
#' @return NULL (creates plot)
#' @keywords internal
plot_temperature <- function(fb4_result, show_correlation = TRUE, colors = "red", 
                           smooth = TRUE, ...) {
  
  daily <- fb4_result$daily_output
  if (is.null(daily) || !"Temperature" %in% names(daily)) {
    stop("No temperature data available")
  }
  
  cols <- get_color_scheme(colors)
  
  # Setup layout
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  show_corr <- show_correlation && "Consumption_gg" %in% names(daily)
  if (show_corr) {
    par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))
  } else {
    par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))
  }
  
  # Temperature over time
  plot(daily$Day, daily$Temperature, type = "l", lwd = 2, col = cols$primary,
       xlab = "Day", ylab = "Temperature (°C)", main = "Temperature Profile", las = 1)
  grid(col = "lightgray", lty = "dotted")
  
  # Smooth trend
  if (smooth && nrow(daily) > 10) {
    smooth_temp <- lowess(daily$Day, daily$Temperature, f = 0.2)
    lines(smooth_temp$x, smooth_temp$y, col = cols$secondary, lwd = 2, lty = 2)
  }
  
  # Statistics
  temp_stats <- c(
    paste("Mean:", round(mean(daily$Temperature, na.rm = TRUE), 1), "°C"),
    paste("Range:", round(diff(range(daily$Temperature, na.rm = TRUE)), 1), "°C"),
    paste("Min:", round(min(daily$Temperature, na.rm = TRUE), 1), "°C"),
    paste("Max:", round(max(daily$Temperature, na.rm = TRUE), 1), "°C")
  )
  legend("topleft", legend = temp_stats, bty = "o", bg = "white", cex = 0.8)
  
  # Temperature-consumption correlation
  if (show_corr) {
    plot(daily$Temperature, daily$Consumption_gg,
         xlab = "Temperature (°C)", ylab = "Consumption Rate (g/g/day)",
         main = "Temperature vs Consumption", pch = 16, col = cols$primary, cex = 0.8)
    grid(col = "lightgray", lty = "dotted")
    
    if (nrow(daily) > 5) {
      trend <- lowess(daily$Temperature, daily$Consumption_gg, f = 0.5)
      lines(trend$x, trend$y, col = cols$accent, lwd = 2)
      
      correlation <- cor(daily$Temperature, daily$Consumption_gg, use = "complete.obs")
      legend("topright", legend = paste("r =", round(correlation, 3)),
             bty = "n", text.col = cols$accent, cex = 1.1)
    }
  }
}

# ============================================================================
# ENERGY PLOT
# ============================================================================

#' Plot energy budget components
#'
#' @description
#' Creates energy component plots with optional efficiency subplot.
#'
#' @param fb4_result FB4 result object
#' @param components Energy components to plot (auto-detected if NULL)
#' @param show_efficiency Show growth efficiency subplot
#' @param colors Color scheme
#' @param ... Additional arguments
#'
#' @return NULL (creates plot)
#' @keywords internal
plot_energy <- function(fb4_result, components = NULL, show_efficiency = TRUE, 
                       colors = "purple", ...) {
  
  daily <- fb4_result$daily_output
  if (is.null(daily)) stop("No daily output data available")
  
  # Auto-detect energy components
  if (is.null(components)) {
    energy_cols <- grep("energy|Energy", names(daily), value = TRUE)
    if (length(energy_cols) == 0) {
      stop("No energy data available")
    }
    components <- energy_cols[1:min(4, length(energy_cols))]
  }
  
  # Validate components exist
  components <- intersect(components, names(daily))
  if (length(components) == 0) {
    stop("No valid energy components found")
  }
  
  cols <- get_color_scheme(colors)
  
  # Setup layout
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  show_eff <- show_efficiency && all(c("Consumption_energy", "Net_energy") %in% names(daily))
  if (show_eff) {
    par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))
  } else {
    par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))
  }
  
  # Energy components
  comp_colors <- rainbow(length(components))
  y_range <- range(daily[components], na.rm = TRUE)
  
  plot(daily$Day, daily[[components[1]]], type = "l", lwd = 2, col = comp_colors[1],
       ylim = y_range, xlab = "Day", ylab = "Energy (J/g/day)",
       main = "Energy Budget Components", las = 1)
  grid(col = "lightgray", lty = "dotted")
  
  if (length(components) > 1) {
    for (i in 2:length(components)) {
      lines(daily$Day, daily[[components[i]]], lwd = 2, col = comp_colors[i])
    }
  }
  
  legend("topright", legend = components, col = comp_colors, lwd = 2, 
         cex = 0.8, bg = "white")
  
  # Growth efficiency
  if (show_eff) {
    efficiency <- daily$Net_energy / daily$Consumption_energy * 100
    efficiency[!is.finite(efficiency)] <- 0
    
    plot(daily$Day, efficiency, type = "l", lwd = 2, col = cols$accent,
         xlab = "Day", ylab = "Growth Efficiency (%)", 
         main = "Growth Efficiency Over Time", las = 1)
    grid(col = "lightgray", lty = "dotted")
    
    valid_eff <- efficiency[efficiency > 0 & is.finite(efficiency)]
    if (length(valid_eff) > 0) {
      stats_text <- c(
        paste("Mean:", round(mean(valid_eff), 1), "%"),
        paste("Max:", round(max(valid_eff), 1), "%"),
        paste("Min:", round(min(valid_eff), 1), "%")
      )
      legend("topright", legend = stats_text, bty = "o", bg = "white", cex = 0.8)
    }
  }
}

# ============================================================================
# UNCERTAINTY PLOT
# ============================================================================

#' Plot parameter uncertainty
#'
#' @description
#' Creates uncertainty plots for MLE, bootstrap, and hierarchical methods.
#'
#' @param fb4_result FB4 result object with uncertainty estimates
#' @param type "estimates" for point estimates with CI, "distributions" for histograms
#' @param colors Color scheme
#' @param ... Additional arguments
#'
#' @return NULL (creates plot)
#' @keywords internal
plot_uncertainty <- function(fb4_result, type = "both", colors = "blue", ...) {
  
  method <- fb4_result$summary$method
  if (!method %in% c("mle", "bootstrap", "hierarchical")) {
    stop("Uncertainty plots require MLE, bootstrap, or hierarchical methods")
  }
  
  cols <- get_color_scheme(colors)
  
  # Setup layout
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  # Determine what to plot based on method and type
  if (method == "mle") {
    plot_mle_uncertainty(fb4_result, cols)
  } else if (method == "bootstrap") {
    plot_bootstrap_uncertainty(fb4_result, type, cols)
  } else if (method == "hierarchical") {
    plot_hierarchical_uncertainty(fb4_result, type, cols)
  }
}

#' Plot MLE uncertainty
#' @keywords internal
plot_mle_uncertainty <- function(fb4_result, cols) {
  
  p_est <- fb4_result$summary$p_estimate %||% NA
  p_se <- fb4_result$method_data$sigma_se %||% NA
  ci_lower <- fb4_result$method_data$confidence_intervals$p_ci_lower %||% NA
  ci_upper <- fb4_result$method_data$confidence_intervals$p_ci_upper %||% NA
  
  if (is.na(p_est)) {
    stop("No MLE estimates available")
  }
  
  par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))
  
  # Bar plot with error bars
  bp <- barplot(p_est, names.arg = "p_value", main = "MLE Parameter Estimate",
                ylab = "p_value", col = cols$main, border = cols$primary,
                ylim = c(0, max(p_est + 2*p_se, ci_upper, na.rm = TRUE) * 1.1))
  
  # Error bars
  if (!is.na(p_se)) {
    arrows(bp, p_est - 1.96*p_se, bp, p_est + 1.96*p_se,
           angle = 90, code = 3, length = 0.1, col = cols$accent, lwd = 2)
  }
  
  # CI lines
  if (!is.na(ci_lower) && !is.na(ci_upper)) {
    abline(h = ci_lower, col = cols$accent, lty = 2)
    abline(h = ci_upper, col = cols$accent, lty = 2)
  }
  
  grid(col = "lightgray", lty = "dotted")
  
  # Add text
  text_info <- paste0("Estimate: ", round(p_est, 4))
  if (!is.na(p_se)) text_info <- paste0(text_info, "\nSE: ", round(p_se, 4))
  if (!is.na(ci_lower) && !is.na(ci_upper)) {
    text_info <- paste0(text_info, "\n95% CI: [", round(ci_lower, 4), 
                       ", ", round(ci_upper, 4), "]")
  }
  
  text(bp, p_est + (ci_upper - p_est) * 0.5, text_info, cex = 0.9)
}

#' Plot bootstrap uncertainty
#' @keywords internal
plot_bootstrap_uncertainty <- function(fb4_result, type, cols) {
  
  bootstrap_data <- fb4_result$method_data$bootstrap_results %||% list()
  p_values <- bootstrap_data$p_values %||% NULL
  
  if (is.null(p_values) || length(p_values) == 0) {
    stop("No bootstrap results available")
  }
  
  if (type == "estimates" || type == "both") {
    par(mfrow = if(type == "both") c(2, 1) else c(1, 1), mar = c(4, 4, 3, 2))
    
    # Point estimate with CI
    p_mean <- mean(p_values)
    p_ci <- quantile(p_values, c(0.025, 0.975))
    
    bp <- barplot(p_mean, names.arg = "p_value", main = "Bootstrap Estimate",
                  ylab = "p_value", col = cols$main, border = cols$primary,
                  ylim = c(0, max(p_ci) * 1.1))
    
    arrows(bp, p_ci[1], bp, p_ci[2], angle = 90, code = 3, 
           length = 0.1, col = cols$accent, lwd = 2)
    
    grid(col = "lightgray", lty = "dotted")
    
    text(bp, p_mean + diff(p_ci) * 0.3,
         paste("Mean:", round(p_mean, 4), "\n95% CI: [", 
               round(p_ci[1], 4), ", ", round(p_ci[2], 4), "]"),
         cex = 0.9)
  }
  
  if (type == "distributions" || type == "both") {
    if (type != "both") par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))
    
    # Histogram with density
    hist(p_values, breaks = 30, freq = FALSE, col = cols$main, 
         border = cols$primary, main = "Bootstrap Distribution",
         xlab = "p_value", ylab = "Density")
    
    # Add density curve
    dens <- density(p_values)
    lines(dens, col = cols$primary, lwd = 2)
    
    # Add mean line
    abline(v = mean(p_values), col = cols$accent, lwd = 2, lty = 2)
    
    grid(col = "lightgray", lty = "dotted")
    
    legend("topright", 
           legend = c(paste("Mean:", round(mean(p_values), 4)),
                     paste("SD:", round(sd(p_values), 4)),
                     paste("N:", length(p_values))),
           bty = "o", bg = "white", cex = 0.8)
  }
}

#' Plot hierarchical uncertainty
#' @keywords internal
plot_hierarchical_uncertainty <- function(fb4_result, type, cols) {
  
  pop_results <- fb4_result$method_data$population_results %||% list()
  ind_results <- fb4_result$method_data$individual_results %||% list()
  
  mu_p <- pop_results$mu_p_estimate %||% NA
  mu_p_se <- pop_results$mu_p_se %||% NA
  sigma_p <- pop_results$sigma_p_estimate %||% NA
  p_estimates <- ind_results$p_estimates %||% NULL
  
  if (is.na(mu_p) && is.null(p_estimates)) {
    stop("No hierarchical results available")
  }
  
  par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))
  
  # Population mean estimate
  if (!is.na(mu_p)) {
    bp <- barplot(mu_p, names.arg = "Population μ", 
                  main = "Population Mean p_value",
                  ylab = "p_value", col = cols$main, border = cols$primary,
                  ylim = c(0, max(mu_p + 2*mu_p_se, na.rm = TRUE) * 1.1))
    
    if (!is.na(mu_p_se)) {
      arrows(bp, mu_p - 1.96*mu_p_se, bp, mu_p + 1.96*mu_p_se,
             angle = 90, code = 3, length = 0.1, col = cols$accent, lwd = 2)
    }
    
    grid(col = "lightgray", lty = "dotted")
    
    text(bp, mu_p + mu_p_se,
         paste("μ:", round(mu_p, 4), "\nSE:", round(mu_p_se, 4)),
         cex = 0.9)
  }
  
  # Individual estimates distribution
  if (!is.null(p_estimates) && length(p_estimates) > 0) {
    hist(p_estimates, breaks = min(20, length(p_estimates)/2), freq = FALSE,
         col = cols$main, border = cols$primary,
         main = "Individual p_value Distribution",
         xlab = "p_value", ylab = "Density")
    
    # Add rug plot for individuals
    rug(p_estimates, col = cols$primary)
    
    # Add population mean if available
    if (!is.na(mu_p)) {
      abline(v = mu_p, col = cols$accent, lwd = 2, lty = 2)
    }
    
    grid(col = "lightgray", lty = "dotted")
    
    legend("topright",
           legend = c(paste("Mean:", round(mean(p_estimates), 4)),
                     paste("SD:", round(sd(p_estimates), 4)),
                     paste("N:", length(p_estimates))),
           bty = "o", bg = "white", cex = 0.8)
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
#' @param bio_obj Bioenergetic object
#' @param temperatures Temperature values to test
#' @param feeding_levels Feeding levels to test
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
       xlab = "Temperature (°C)",
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
           paste("Optimal\n", optimal_temp, "°C"),
           col = "red", cex = 0.8, adj = 0)
    }
  }
  
  if (verbose) message("Sensitivity plot completed")
}

# ============================================================================
# BIOENERGETIC PLOTS
# ============================================================================

#' Plot Bioenergetic dashboard
#' @keywords internal
plot_bio_dashboard <- function(bio_obj, colors = "blue", title = NULL, ...) {
  
  cols <- get_color_scheme(colors)
  
  # Setup 2x2 layout
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2), oma = c(0, 0, 2, 0))
  
  # Panel 1: Component status
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  title("Component Status")
  
  has_params <- !is.null(bio_obj$species_params) && length(bio_obj$species_params) > 0
  has_temp <- !is.null(bio_obj$environmental_data$temperature)
  has_diet <- !is.null(bio_obj$diet_data$proportions)
  has_initial <- !is.null(bio_obj$simulation_settings$initial_weight)
  
  components <- list(
    "Parameters" = has_params,
    "Temperature" = has_temp,
    "Diet data" = has_diet,
    "Initial weight" = has_initial
  )
  
  y_pos <- seq(0.8, 0.2, length.out = length(components))
  for (i in seq_along(components)) {
    comp_name <- names(components)[i]
    status <- components[[i]]
    symbol <- if (status) "✓" else "✗"
    color <- if (status) "darkgreen" else "red"
    text(0.1, y_pos[i], paste(symbol, comp_name), adj = 0, col = color, font = 2)
  }
  
  # Panel 2: Data coverage
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  title("Data Coverage")
  
  data_info <- character()
  if (!is.null(bio_obj$environmental_data$temperature)) {
    temp_days <- nrow(bio_obj$environmental_data$temperature)
    data_info <- c(data_info, paste("Temperature:", temp_days, "days"))
  }
  if (!is.null(bio_obj$diet_data$proportions)) {
    diet_days <- nrow(bio_obj$diet_data$proportions)
    prey_count <- length(bio_obj$diet_data$prey_names %||% c())
    data_info <- c(data_info, paste("Diet:", diet_days, "days"))
    data_info <- c(data_info, paste("Prey species:", prey_count))
  }
  
  if (length(data_info) > 0) {
    y_pos <- seq(0.8, 0.2, length.out = length(data_info))
    for (i in seq_along(data_info)) {
      text(0.1, y_pos[i], data_info[i], adj = 0, cex = 0.9)
    }
  } else {
    text(0.5, 0.5, "No data available", cex = 1.0, adj = 0.5, col = "gray")
  }
  
  # Panel 3: Temperature preview (if available)
  if (!is.null(bio_obj$environmental_data$temperature)) {
    temp_data <- bio_obj$environmental_data$temperature
    plot(temp_data$Day, temp_data$Temperature, type = "l", lwd = 2, col = cols$accent,
         xlab = "Day", ylab = "°C", main = "Temperature Preview", las = 1)
    grid(col = "lightgray", lty = "dotted")
  } else {
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1))
    text(0.5, 0.5, "No temperature data", cex = 1.0, adj = 0.5, col = "gray")
  }
  
  # Panel 4: Simulation readiness
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  title("Simulation Ready")
  
  ready_count <- sum(has_params, has_temp, has_diet, has_initial)
  total_required <- 4
  
  if (ready_count == total_required) {
    text(0.5, 0.6, "✓ READY", cex = 2.0, adj = 0.5, col = "darkgreen", font = 2)
    text(0.5, 0.3, "All components available", cex = 1.0, adj = 0.5)
  } else {
    text(0.5, 0.6, "✗ NOT READY", cex = 1.5, adj = 0.5, col = "red", font = 2)
    text(0.5, 0.3, paste(ready_count, "/", total_required, "components"),
         cex = 1.0, adj = 0.5)
  }
  
  # Main title
  if (is.null(title)) {
    species <- bio_obj$species_info$scientific_name %||% 
               bio_obj$species_info$common_name %||% 
               "Unknown Species"
    title <- paste("FB4 Model Setup:", species)
  }
  mtext(title, outer = TRUE, cex = 1.2, font = 2)
}

#' Plot Bioenergetic temperature
#' @keywords internal
plot_bio_temperature <- function(bio_obj, colors = "red", ...) {
  
  temp_data <- bio_obj$environmental_data$temperature
  if (is.null(temp_data)) {
    stop("No temperature data available")
  }
  
  cols <- get_color_scheme(colors)
  
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))
  
  plot(temp_data$Day, temp_data$Temperature, type = "l", lwd = 2, col = cols$primary,
       xlab = "Day", ylab = "Temperature (°C)", main = "Temperature Profile", las = 1)
  grid(col = "lightgray", lty = "dotted")
  
  mean_temp <- mean(temp_data$Temperature, na.rm = TRUE)
  abline(h = mean_temp, col = cols$accent, lty = 2)
  
  temp_range <- range(temp_data$Temperature, na.rm = TRUE)
  abline(h = temp_range, col = cols$secondary, lty = 3, lwd = 1)
  
  stats_text <- c(
    paste("Mean:", round(mean_temp, 1), "°C"),
    paste("Range:", round(diff(temp_range), 1), "°C"),
    paste("Days:", nrow(temp_data))
  )
  legend("topleft", legend = stats_text, bty = "o", bg = "white", cex = 0.8)
}

#' Plot Bioenergetic diet
#' @keywords internal
plot_bio_diet <- function(bio_obj, colors = "green", max_prey = 5, ...) {
  
  diet_data <- bio_obj$diet_data
  if (is.null(diet_data) || is.null(diet_data$proportions)) {
    stop("No diet data available")
  }
  
  prey_names <- diet_data$prey_names
  if (length(prey_names) > max_prey) {
    prey_names <- prey_names[1:max_prey]
    warning("Displaying only first ", max_prey, " prey species")
  }
  
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))
  
  # Stacked area plot for diet composition
  diet_props <- diet_data$proportions
  diet_matrix <- as.matrix(diet_props[, prey_names, drop = FALSE])
  
  prey_colors <- rainbow(length(prey_names), alpha = 0.7)
  
  plot(diet_props$Day, rep(0, nrow(diet_props)), type = "n", ylim = c(0, 1),
       xlab = "Day", ylab = "Diet Proportion", 
       main = "Diet Composition Over Time", las = 1)
  
  cumulative <- rep(0, nrow(diet_props))
  for (i in 1:length(prey_names)) {
    cumulative_new <- cumulative + diet_matrix[, i]
    x_coords <- c(diet_props$Day, rev(diet_props$Day))
    y_coords <- c(cumulative, rev(cumulative_new))
    polygon(x_coords, y_coords, col = prey_colors[i], border = NA)
    cumulative <- cumulative_new
  }
  
  grid(col = "lightgray", lty = "dotted")
  legend("topright", legend = prey_names, fill = prey_colors, cex = 0.8, bg = "white")
}

#' Plot Bioenergetic energy
#' @keywords internal
plot_bio_energy <- function(bio_obj, colors = "purple", ...) {
  
  predator_params <- bio_obj$species_params$predator
  if (is.null(predator_params)) {
    stop("No predator parameters available")
  }
  
  cols <- get_color_scheme(colors)
  
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))
  
  PREDEDEQ <- predator_params$PREDEDEQ %||% 1
  
  if (PREDEDEQ == 1) {
    # Time-series energy data
    duration <- bio_obj$simulation_settings$duration %||% 365
    days <- 1:duration
    
    if (!is.null(predator_params$ED_data)) {
      energy_values <- rep(predator_params$ED_data, length.out = duration)
    } else if (!is.null(predator_params$ED_ini) && !is.null(predator_params$ED_end)) {
      energy_values <- seq(from = predator_params$ED_ini, 
                          to = predator_params$ED_end, 
                          length.out = duration)
    } else {
      stop("PREDEDEQ=1 requires ED_data or ED_ini/ED_end")
    }
    
    plot(days, energy_values, type = "l", lwd = 2, col = cols$primary,
         xlab = "Day", ylab = "Energy Density (J/g)", 
         main = "Predator Energy Density", las = 1)
    grid(col = "lightgray", lty = "dotted")
    
    legend("topright",
           legend = c(paste("Mean:", round(mean(energy_values), 0), "J/g"),
                     paste("Range:", round(diff(range(energy_values)), 0), "J/g")),
           bty = "o", bg = "white", cex = 0.8)
           
  } else if (PREDEDEQ == 2) {
    # Weight-based equation
    weights <- seq(1, 1000, length.out = 100)
    
    Alpha1 <- predator_params$Alpha1 %||% 0
    Beta1 <- predator_params$Beta1 %||% 0
    Alpha2 <- predator_params$Alpha2 %||% 0
    Beta2 <- predator_params$Beta2 %||% 0
    Cutoff <- predator_params$Cutoff %||% 100
    
    energy_values <- ifelse(weights <= Cutoff,
                           Alpha1 + Beta1 * weights,
                           Alpha2 + Beta2 * weights)
    
    plot(weights, energy_values, type = "l", lwd = 2, col = cols$primary,
         xlab = "Weight (g)", ylab = "Energy Density (J/g)", 
         main = "Predator Energy Density vs Weight", las = 1)
    
    if (Cutoff > 0 && Cutoff < max(weights)) {
      abline(v = Cutoff, col = cols$accent, lty = 2, lwd = 1)
      text(Cutoff + 50, max(energy_values) * 0.9, 
           paste("Cutoff:", Cutoff, "g"), col = cols$accent)
    }
    
    grid(col = "lightgray", lty = "dotted")
  }
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Extract species name from fb4_result or Bioenergetic object
#' @keywords internal
extract_species_name <- function(x) {
  species_info <- x$bioenergetic_object$species_info %||% x$species_info
  if (is.null(species_info)) return(NULL)
  
  species_info$scientific_name %||% species_info$common_name %||% NULL
}
