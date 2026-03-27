#' Daily Simulation Plots for FB4 Results
#'
#' @description
#' Plotting functions for daily simulation output. These functions work with
#' the daily_output data produced by run_fb4() and visualise growth,
#' consumption, temperature, and energy patterns over time.
#'
#' @name fb4-daily-plots
#' @importFrom graphics plot lines abline text legend grid par points barplot
#' @importFrom stats lowess cor
#' @importFrom utils tail
NULL

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
         xlab = "Day", ylab = "Temperature (\u00b0C)", main = "Temperature", las = 1)
    grid(col = "lightgray", lty = "dotted")
    mean_temp <- mean(daily$Temperature, na.rm = TRUE)
    legend("topright", legend = paste("Mean:", round(mean_temp, 1), "\u00b0C"),
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
       xlab = "Day", ylab = "Temperature (\u00b0C)", main = "Temperature Profile", las = 1)
  grid(col = "lightgray", lty = "dotted")
  
  # Smooth trend
  if (smooth && nrow(daily) > 10) {
    smooth_temp <- lowess(daily$Day, daily$Temperature, f = 0.2)
    lines(smooth_temp$x, smooth_temp$y, col = cols$secondary, lwd = 2, lty = 2)
  }
  
  # Statistics
  temp_stats <- c(
    paste("Mean:", round(mean(daily$Temperature, na.rm = TRUE), 1), "\u00b0C"),
    paste("Range:", round(diff(range(daily$Temperature, na.rm = TRUE)), 1), "\u00b0C"),
    paste("Min:", round(min(daily$Temperature, na.rm = TRUE), 1), "\u00b0C"),
    paste("Max:", round(max(daily$Temperature, na.rm = TRUE), 1), "\u00b0C")
  )
  legend("topleft", legend = temp_stats, bty = "o", bg = "white", cex = 0.8)
  
  # Temperature-consumption correlation
  if (show_corr) {
    plot(daily$Temperature, daily$Consumption_gg,
         xlab = "Temperature (\u00b0C)", ylab = "Consumption Rate (g/g/day)",
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

