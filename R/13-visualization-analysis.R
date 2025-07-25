#' Simplified Visualization Functions for FB4 Model
#'
#' Essential plotting functions for FB4 simulation results
#' @name fb4-visualization
#' @importFrom graphics plot lines abline text legend grid par pie mtext
#' @importFrom grDevices png pdf dev.off
#' @importFrom stats lowess cor
#' @importFrom utils tail write.csv
#' @importFrom tools file_ext
NULL

# ============================================================================
# CORE PLOTTING FUNCTIONS
# ============================================================================

#' Main plotting function for FB4 results
#'
#' @param x Object of class fb4_result
#' @param plot_type Type of plot: "growth", "consumption", "temperature", "energy", "dashboard"
#' @param save_plot Path to save plot (optional)
#' @param ... Additional arguments
#' @export
plot.fb4_result <- function(x, plot_type = "growth", save_plot = NULL, ...) {
  
  if (!inherits(x, "fb4_result")) {
    stop("Input must be an fb4_result object")
  }
  
  if (is.null(x$daily_output)) {
    stop("x must contain daily_output for plotting")
  }
  
  # Setup device if saving
  if (!is.null(save_plot)) {
    file_ext <- tools::file_ext(save_plot)
    if (file_ext == "png") {
      grDevices::png(save_plot, width = 10, height = 8, units = "in", res = 300)
    } else if (file_ext == "pdf") {
      grDevices::pdf(save_plot, width = 10, height = 8)
    }
  }
  
  # Route to specific plot function
  switch(plot_type,
         "growth" = plot_growth_components(x, ...),
         "consumption" = plot_consumption_components(x, ...),
         "temperature" = plot_temperature_profile(x, ...),
         "energy" = plot_energy_components(x, ...),
         "dashboard" = plot_dashboard(x, ...),
         stop("Unknown plot_type: ", plot_type)
  )
  
  # Close device if saving
  if (!is.null(save_plot)) {
    grDevices::dev.off()
    message("Plot saved to: ", save_plot)
  }
}

#' Plot growth components (weight gain and cumulative)
#'
#' @param fb4_result FB4 result object
#' @param show_cumulative Show cumulative growth
#' @export
plot_growth_components <- function(fb4_result, show_cumulative = TRUE) {
  
  daily_data <- fb4_result$daily_output
  
  if (show_cumulative) {
    graphics::par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))
  }
  
  # 1. Weight trajectory
  graphics::plot(daily_data$Day, daily_data$Weight, type = "l", lwd = 2, col = "blue",
                 xlab = "Day", ylab = "Weight (g)", main = "Weight Trajectory", las = 1)
  graphics::grid(col = "lightgray", lty = "dotted")
  
  # Add statistics
  initial_weight <- daily_data$Weight[1]
  final_weight <- utils::tail(daily_data$Weight, 1)
  total_growth <- final_weight - initial_weight
  
  graphics::legend("topleft", 
                   legend = c(paste("Initial:", round(initial_weight, 2), "g"),
                              paste("Final:", round(final_weight, 2), "g"),
                              paste("Growth:", round(total_growth, 2), "g")),
                   bg = "white", cex = 0.9)
  
  # 2. Cumulative growth (if requested)
  if (show_cumulative && nrow(daily_data) > 1) {
    cumulative_growth <- daily_data$Weight - daily_data$Weight[1]
    graphics::plot(daily_data$Day, cumulative_growth, type = "l", lwd = 2, col = "darkgreen",
                   xlab = "Day", ylab = "Cumulative Growth (g)", 
                   main = "Cumulative Weight Gain", las = 1)
    graphics::grid(col = "lightgray", lty = "dotted")
  }
  
  if (show_cumulative) graphics::par(mfrow = c(1, 1))
}

#' Plot consumption components (rate and total by diet items)
#'
#' @param fb4_result FB4 result object
#' @param show_diet_breakdown Show consumption by diet items
#' @export
plot_consumption_components <- function(fb4_result, show_diet_breakdown = TRUE) {
  
  daily_data <- fb4_result$daily_output
  
  if (!"Consumption_gg" %in% names(daily_data)) {
    stop("Consumption data not available")
  }
  
  if (show_diet_breakdown) {
    graphics::par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  } else {
    graphics::par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))
  }
  
  # 1. Consumption rate
  graphics::plot(daily_data$Day, daily_data$Consumption_gg, type = "l", lwd = 2, col = "darkgreen",
                 xlab = "Day", ylab = "Consumption Rate (g/g/day)", 
                 main = "Daily Consumption Rate", las = 1)
  
  # Add mean line
  mean_consumption <- mean(daily_data$Consumption_gg, na.rm = TRUE)
  graphics::abline(h = mean_consumption, col = "red", lty = 2)
  graphics::text(max(daily_data$Day) * 0.1, mean_consumption * 1.1, 
                 paste("Mean:", round(mean_consumption, 4)), col = "red", cex = 0.9)
  graphics::grid(col = "lightgray", lty = "dotted")
  
  # 2. Total daily consumption (g/day)
  total_consumption <- daily_data$Consumption_gg * daily_data$Weight
  graphics::plot(daily_data$Day, total_consumption, type = "l", lwd = 2, col = "orange",
                 xlab = "Day", ylab = "Total Consumption (g/day)", 
                 main = "Total Daily Food Consumption", las = 1)
  graphics::grid(col = "lightgray", lty = "dotted")
  
  # 3. Cumulative consumption
  cumulative_consumption <- cumsum(total_consumption)
  graphics::plot(daily_data$Day, cumulative_consumption, type = "l", lwd = 2, col = "purple",
                 xlab = "Day", ylab = "Cumulative Consumption (g)", 
                 main = "Cumulative Food Consumption", las = 1)
  graphics::grid(col = "lightgray", lty = "dotted")
  
  # 4. Diet breakdown (if bioenergetic object has diet info)
  if (show_diet_breakdown && !is.null(fb4_result$bioenergetic_object$diet)) {
    plot_diet_consumption(fb4_result)
  } else if (show_diet_breakdown) {
    # P-value as feeding level
    if ("P_value" %in% names(daily_data)) {
      graphics::plot(daily_data$Day, daily_data$P_value, type = "l", lwd = 2, col = "brown",
                     xlab = "Day", ylab = "P-value (Feeding Level)", 
                     main = "Feeding Level Over Time", las = 1)
      graphics::grid(col = "lightgray", lty = "dotted")
    }
  }
  
  graphics::par(mfrow = c(1, 1))
}

#' Plot diet consumption breakdown
#'
#' @param fb4_result FB4 result object
#' @keywords internal
plot_diet_consumption <- function(fb4_result) {
  
  daily_data <- fb4_result$daily_output
  diet_info <- fb4_result$bioenergetic_object$diet_data
  
  if (is.null(diet_info$proportions) || is.null(diet_info$prey_names)) {
    # Fallback to P-value plot
    if ("P_value" %in% names(daily_data)) {
      graphics::plot(daily_data$Day, daily_data$P_value, type = "l", lwd = 2, col = "brown",
                     xlab = "Day", ylab = "P-value", main = "Feeding Level", las = 1)
      graphics::grid(col = "lightgray", lty = "dotted")
    }
    return()
  }
  
  # Calculate consumption by diet items
  total_consumption <- daily_data$Consumption_gg * daily_data$Weight
  n_items <- length(diet_info$prey_names) + 1
  
  if (n_items <= 5) {
    colors <- c("red", "blue", "green", "orange")[1:n_items]
    
    # Stacked area plot or individual lines
    graphics::plot(daily_data$Day, total_consumption * diet_info$proportions[[1]], 
                   type = "n", lwd = 2, col = colors[1],
                   xlab = "Day", ylab = "Consumption by Item (g/day)", 
                   main = "Diet Item Consumption", las = 1,
                   ylim = c(0, max(total_consumption) * 1.1))
    
    if (n_items > 1) {
      for (i in 2:n_items) {
        graphics::lines(daily_data$Day, total_consumption * diet_info$proportions[[i]], 
                        lwd = 2, col = colors[i - 1])
      }
    }
    
    graphics::legend("topright", legend = diet_info$prey_names, col = colors, 
                     lwd = 2, cex = 0.8, bg = "white")
    graphics::grid(col = "lightgray", lty = "dotted")
  } else {
    # Too many items, show pie chart of proportions
    graphics::pie(diet_info$proportions, labels = diet_info$prey_names, 
                  main = "Diet Composition", cex = 0.8)
  }
}

#' Plot temperature profile
#'
#' @param fb4_result FB4 result object
#' @param add_smooth Add smoothed trend line
#' @export
plot_temperature_profile <- function(fb4_result, add_smooth = TRUE) {
  
  daily_data <- fb4_result$daily_output
  
  if (!"Temperature" %in% names(daily_data)) {
    stop("Temperature data not available")
  }
  
  graphics::par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))
  
  # 1. Temperature over time
  graphics::plot(daily_data$Day, daily_data$Temperature, type = "l", lwd = 2, col = "red",
                 xlab = "Day", ylab = "Temperature (deg C)", 
                 main = "Temperature Profile", las = 1)
  
  if (add_smooth && nrow(daily_data) > 10) {
    smooth_data <- stats::lowess(daily_data$Day, daily_data$Temperature, f = 0.2)
    graphics::lines(smooth_data$x, smooth_data$y, col = "darkred", lwd = 2, lty = 2)
  }
  
  # Add statistics
  temp_stats <- c(paste("Mean:", round(mean(daily_data$Temperature, na.rm = TRUE), 1), "deg C"),
                  paste("Range:", round(diff(range(daily_data$Temperature, na.rm = TRUE)), 1), "deg C"))
  graphics::legend("topright", legend = temp_stats, bg = "white", cex = 0.9)
  graphics::grid(col = "lightgray", lty = "dotted")
  
  # 2. Temperature vs consumption relationship
  if ("Consumption_gg" %in% names(daily_data)) {
    graphics::plot(daily_data$Temperature, daily_data$Consumption_gg,
                   xlab = "Temperature (deg C)", ylab = "Consumption Rate (g/g/day)",
                   main = "Temperature vs Consumption", pch = 16, col = "darkblue", cex = 0.8)
    
    if (nrow(daily_data) > 5) {
      trend <- stats::lowess(daily_data$Temperature, daily_data$Consumption_gg, f = 0.5)
      graphics::lines(trend$x, trend$y, col = "red", lwd = 2)
      
      # Add correlation
      correlation <- stats::cor(daily_data$Temperature, daily_data$Consumption_gg, use = "complete.obs")
      graphics::text(min(daily_data$Temperature, na.rm = TRUE), 
                     max(daily_data$Consumption_gg, na.rm = TRUE) * 0.9,
                     paste("r =", round(correlation, 3)), col = "red", font = 2)
    }
    graphics::grid(col = "lightgray", lty = "dotted")
  }
  
  graphics::par(mfrow = c(1, 1))
}

#' Plot energy components
#'
#' @param fb4_result FB4 result object
#' @param components Energy components to plot
#' @export
plot_energy_components <- function(fb4_result, 
                                   components = c("Consumption_energy", "Respiration", "Net_energy")) {
  
  daily_data <- fb4_result$daily_output
  
  # Check available components
  available_components <- intersect(components, names(daily_data))
  
  if (length(available_components) == 0) {
    stop("No energy components available in data")
  }
  
  # Colors for components
  colors <- c("blue", "red", "green", "orange", "purple")[1:length(available_components)]
  
  # Find y-axis range
  y_range <- range(daily_data[available_components], na.rm = TRUE)
  
  # Create base plot
  graphics::plot(daily_data$Day, daily_data[[available_components[1]]], 
                 type = "l", lwd = 2, col = colors[1],
                 ylim = y_range, xlab = "Day", ylab = "Energy (J/g/day)",
                 main = "Energy Components", las = 1)
  
  # Add other components
  if (length(available_components) > 1) {
    for (i in 2:length(available_components)) {
      graphics::lines(daily_data$Day, daily_data[[available_components[i]]], 
                      lwd = 2, col = colors[i])
    }
  }
  
  # Add legend
  graphics::legend("topright", legend = available_components, 
                   col = colors, lwd = 2, cex = 0.9, bg = "white")
  
  graphics::grid(col = "lightgray", lty = "dotted")
}

#' Create simple dashboard with key plots
#'
#' @param fb4_result FB4 result object
#' @export
plot_dashboard <- function(fb4_result) {
  
  daily_data <- fb4_result$daily_output
  
  # Setup 2x2 layout
  graphics::par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
  
  # 1. Weight trajectory
  graphics::plot(daily_data$Day, daily_data$Weight, type = "l", lwd = 2, col = "blue",
                 xlab = "Day", ylab = "Weight (g)", main = "Growth", las = 1)
  graphics::grid(col = "lightgray", lty = "dotted")
  
  # 2. Consumption rate
  if ("Consumption_gg" %in% names(daily_data)) {
    graphics::plot(daily_data$Day, daily_data$Consumption_gg, type = "l", lwd = 2, col = "green",
                   xlab = "Day", ylab = "Consumption (g/g/day)", main = "Consumption Rate", las = 1)
    graphics::grid(col = "lightgray", lty = "dotted")
  }
  
  # 3. Temperature
  if ("Temperature" %in% names(daily_data)) {
    graphics::plot(daily_data$Day, daily_data$Temperature, type = "l", lwd = 2, col = "red",
                   xlab = "Day", ylab = "Temperature (deg C)", main = "Temperature", las = 1)
    graphics::grid(col = "lightgray", lty = "dotted")
  }
  
  # 4. Energy efficiency or P-value
  if (all(c("Consumption_energy", "Net_energy") %in% names(daily_data))) {
    efficiency <- daily_data$Net_energy / daily_data$Consumption_energy * 100
    efficiency[!is.finite(efficiency)] <- 0
    graphics::plot(daily_data$Day, efficiency, type = "l", lwd = 2, col = "purple",
                   xlab = "Day", ylab = "Efficiency (%)", main = "Growth Efficiency", las = 1)
    graphics::grid(col = "lightgray", lty = "dotted")
  } else if ("P_value" %in% names(daily_data)) {
    graphics::plot(daily_data$Day, daily_data$P_value, type = "l", lwd = 2, col = "orange",
                   xlab = "Day", ylab = "P-value", main = "Feeding Level", las = 1)
    graphics::grid(col = "lightgray", lty = "dotted")
  }
  
  # Add main title
  species_name <- fb4_result$bioenergetic_object$species_info$scientific_name %||% 
    "FB4 Simulation"
  graphics::mtext(species_name, outer = TRUE, cex = 1.2, font = 2, line = -2)
  
  graphics::par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Export results to CSV
#'
#' @param fb4_result FB4 result object
#' @param file_path Path for CSV file
#' @export
export_fb4_results <- function(fb4_result, file_path) {
  
  if (!inherits(fb4_result, "fb4_result")) {
    stop("Input must be an fb4_result object")
  }
  
  if (is.null(fb4_result$daily_output)) {
    stop("No daily output data available for export")
  }
  
  utils::write.csv(fb4_result$daily_output, file_path, row.names = FALSE)
  message("Results exported to: ", file_path)
}

#' Summary method for fb4_result objects
#'
#' @param object FB4 result object
#' @param ... Additional arguments
#' @export
#' @method summary fb4_result
summary.fb4_result <- function(object, ...) {
  
  message("FB4 Simulation Summary")
  message("")
  
  if (!is.null(object$summary)) {
    message("Growth Results:")
    message("  Initial Weight: ", object$summary$initial_weight, " g")
    message("  Final Weight: ", object$summary$final_weight, " g")
    message("  Total Growth: ", round(object$summary$final_weight - object$summary$initial_weight, 2), " g")
    message("  Simulation Days: ", object$summary$simulation_days)
    if (!is.null(object$summary$p_value)) {
      message("  P-value: ", round(object$summary$p_value, 4))
    }
    message("")
  }
  
  if (!is.null(object$fit_info)) {
    message("Fitting Status: ", if(object$fit_info$fit_successful) "Successful" else "Failed")
    if (!is.null(object$fit_info$iterations)) {
      message("Iterations: ", object$fit_info$iterations)
    }
    message("")
  }
  
  message("Available Data: ", if(!is.null(object$daily_output)) "Daily output available" else "No daily output")
  
  invisible(object)
}