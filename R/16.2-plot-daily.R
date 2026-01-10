# #' Daily Components Plots for FB4 Results
# #'
# #' @description
# #' Plotting functions for daily simulation components that are available
# #' for all FB4 result types. These functions work with the daily_output
# #' data and provide visualizations of growth, consumption, temperature,
# #' and energy patterns over time.
# #'
# #' @name fb4-daily-plots
# #' @importFrom graphics plot lines abline text legend grid par points
# #' @importFrom stats lowess cor
# #' @importFrom utils tail
# NULL

# # ============================================================================
# # DASHBOARD PLOT
# # ============================================================================

# #' Create dashboard overview plot
# #'
# #' @description
# #' Creates a multi-panel dashboard showing the most important simulation results.
# #' Automatically adapts content based on available data in daily_output.
# #'
# #' @param fb4_result FB4 result object
# #' @param color_scheme Color scheme: "blue", "green", "red", or custom color
# #' @param add_title Add main title with method information, default TRUE
# #' @param title_override Custom title (overrides automatic title)
# #' @param compact_layout Use compact layout with minimal text, default FALSE
# #'
# #' @return NULL (creates plot)
# #' @export
# #'
# #' @examples
# #' \dontrun{
# #' plot_dashboard.fb4_result(result)
# #' plot_dashboard.fb4_result(result, color_scheme = "green")
# #' plot_dashboard.fb4_result(result, title_override = "My Analysis")
# #' }
# plot_dashboard.fb4_result <- function(fb4_result, color_scheme = "blue", 
#                                       add_title = TRUE, title_override = NULL, 
#                                       compact_layout = FALSE) {
  
#   # Validate input
#   if (!is.fb4_result(fb4_result)) {
#     stop("Input must be an fb4_result object")
#   }
  
#   daily_data <- fb4_result$daily_output
#   if (is.null(daily_data)) {
#     stop("No daily output data available for dashboard")
#   }
  
#   capabilities <- detect_plot_capabilities(fb4_result)
#   colors <- get_color_scheme(color_scheme)
  
#   # Setup 2x2 layout with space for title
#   old_par <- setup_plot_layout(layout = c(2, 2), margins = "compact")
#   if (add_title) {
#     graphics::par(oma = c(0, 0, 3, 0))  # Add outer margin for title
#   }
  
#   tryCatch({
    
#     # Panel 1: Growth trajectory (always available)
#     plot_dashboard_growth_panel(daily_data, colors, compact_layout)
    
#     # Panel 2: Consumption (if available)
#     if (capabilities$has_consumption_data) {
#       plot_dashboard_consumption_panel(daily_data, colors, compact_layout)
#     } else {
#       plot_dashboard_placeholder("Consumption data\nnot available", colors)
#     }
    
#     # Panel 3: Temperature (if available)
#     if (capabilities$has_temperature_data) {
#       plot_dashboard_temperature_panel(daily_data, colors, compact_layout)
#     } else {
#       plot_dashboard_method_info_panel(fb4_result, capabilities, colors)
#     }
    
#     # Panel 4: Energy or summary statistics
#     if (capabilities$has_energy_data) {
#       plot_dashboard_energy_panel(daily_data, colors, compact_layout)
#     } else {
#       plot_dashboard_summary_panel(fb4_result, colors)
#     }
    
#     # Add main title
#     if (add_title) {
#       main_title <- create_dashboard_title(fb4_result, capabilities, title_override)
#       graphics::mtext(main_title, outer = TRUE, cex = 1.2, font = 2, line = 1)
#     }
    
#   }, finally = {
#     graphics::par(old_par)
#   })
# }

# # ============================================================================
# # GROWTH TRAJECTORY PLOT
# # ============================================================================

# #' Plot growth trajectory over time
# #'
# #' @description
# #' Creates detailed plots of weight trajectory and growth patterns.
# #' Shows both absolute weight and growth rates with statistics.
# #'
# #' @param fb4_result FB4 result object
# #' @param show_growth_rate Show growth rate subplot, default TRUE
# #' @param color_scheme Color scheme to use, default "blue"
# #' @param add_stats Add statistics annotations, default TRUE
# #' @param smooth_growth Add smoothed growth trend, default FALSE
# #'
# #' @return NULL (creates plot)
# #' @export
# #'
# #' @examples
# #' \dontrun{
# #' plot_growth.fb4_result(result)
# #' plot_growth.fb4_result(result, show_growth_rate = FALSE)
# #' plot_growth.fb4_result(result, smooth_growth = TRUE)
# #' }
# plot_growth.fb4_result <- function(fb4_result, show_growth_rate = TRUE, 
#                                    color_scheme = "blue", add_stats = TRUE,
#                                    smooth_growth = FALSE) {
  
#   # Validate input
#   if (!is.fb4_result(fb4_result)) {
#     stop("Input must be an fb4_result object")
#   }
  
#   daily_data <- fb4_result$daily_output
#   if (is.null(daily_data) || !"Weight" %in% names(daily_data)) {
#     stop("No weight data available in daily output")
#   }
  
#   colors <- get_color_scheme(color_scheme)
  
#   # Setup layout
#   layout_spec <- if (show_growth_rate) c(2, 1) else "single"
#   old_par <- setup_plot_layout(layout = layout_spec, margins = "default")
  
#   tryCatch({
    
#     # 1. Weight trajectory
#     graphics::plot(daily_data$Day, daily_data$Weight, 
#                    type = "l", lwd = 2, col = colors$primary,
#                    xlab = "Day", ylab = "Weight (g)", 
#                    main = "Weight Trajectory", las = 1)
    
#     # Add smoothed trend if requested
#     if (smooth_growth && nrow(daily_data) > 10) {
#       smooth_data <- stats::lowess(daily_data$Day, daily_data$Weight, f = 0.2)
#       graphics::lines(smooth_data$x, smooth_data$y, 
#                       col = colors$secondary, lwd = 2, lty = 2)
      
#       graphics::legend("bottomright", 
#                        legend = c("Weight", "Trend"), 
#                        col = c(colors$primary, colors$secondary), 
#                        lty = c(1, 2), lwd = 2, cex = 0.8)
#     }
    
#     add_plot_annotations(add_grid = TRUE)
    
#     # Add growth statistics
#     if (add_stats) {
#       initial_weight <- daily_data$Weight[1]
#       final_weight <- utils::tail(daily_data$Weight, 1)
#       total_growth <- final_weight - initial_weight
#       relative_growth <- (final_weight / initial_weight - 1) * 100
#       duration <- max(daily_data$Day) - min(daily_data$Day) + 1
      
#       growth_stats <- list(
#         "Initial" = paste(round(initial_weight, 2), "g"),
#         "Final" = paste(round(final_weight, 2), "g"),
#         "Growth" = paste(round(total_growth, 2), "g"),
#         "Relative" = paste(round(relative_growth, 1), "%"),
#         "Days" = duration
#       )
      
#       stats_text <- format_statistics_text(growth_stats)
#       add_plot_annotations(stats_text = stats_text, stats_position = "topleft")
#     }
    
#     # 2. Growth rate (if requested)
#     if (show_growth_rate && nrow(daily_data) > 1) {
#       # Calculate daily growth rate
#       growth_rate <- diff(daily_data$Weight)
#       days_for_rate <- daily_data$Day[-1]  # Remove first day
      
#       graphics::plot(days_for_rate, growth_rate, 
#                      type = "l", lwd = 2, col = colors$secondary,
#                      xlab = "Day", ylab = "Daily Growth Rate (g/day)", 
#                      main = "Daily Growth Rate", las = 1)
      
#       # Add zero reference line
#       add_plot_annotations(add_grid = TRUE, reference_lines = 0)
      
#       # Add growth rate statistics
#       if (add_stats) {
#         mean_growth_rate <- mean(growth_rate, na.rm = TRUE)
        
#         rate_stats <- list(
#           "Mean rate" = paste(round(mean_growth_rate, 3), "g/day"),
#           "Max rate" = paste(round(max(growth_rate, na.rm = TRUE), 3), "g/day"),
#           "Min rate" = paste(round(min(growth_rate, na.rm = TRUE), 3), "g/day")
#         )
        
#         rate_text <- format_statistics_text(rate_stats)
#         add_plot_annotations(stats_text = rate_text, stats_position = "topright")
#       }
#     }
    
#   }, finally = {
#     graphics::par(old_par)
#   })
# }

# # ============================================================================
# # CONSUMPTION PATTERNS PLOT
# # ============================================================================

# #' Plot consumption patterns over time
# #'
# #' @description
# #' Creates plots showing consumption rates and patterns over the simulation period.
# #' Displays both specific consumption (g/g/day) and total daily consumption.
# #'
# #' @param fb4_result FB4 result object
# #' @param show_cumulative Show cumulative consumption, default TRUE
# #' @param color_scheme Color scheme to use, default "green"
# #' @param add_stats Add statistics annotations, default TRUE
# #'
# #' @return NULL (creates plot)
# #' @export
# #'
# #' @examples
# #' \dontrun{
# #' plot_consumption.fb4_result(result)
# #' plot_consumption.fb4_result(result, show_cumulative = FALSE)
# #' }
# plot_consumption.fb4_result <- function(fb4_result, show_cumulative = TRUE, 
#                                         color_scheme = "green", add_stats = TRUE) {
  
#   # Validate input
#   if (!is.fb4_result(fb4_result)) {
#     stop("Input must be an fb4_result object")
#   }
  
#   daily_data <- fb4_result$daily_output
#   if (is.null(daily_data) || !"Consumption_gg" %in% names(daily_data)) {
#     stop("No consumption data available in daily output")
#   }
  
#   colors <- get_color_scheme(color_scheme)
  
#   # Setup layout
#   layout_spec <- if (show_cumulative) c(2, 1) else "single"
#   old_par <- setup_plot_layout(layout = layout_spec, margins = "default")
  
#   tryCatch({
    
#     # 1. Specific consumption rate
#     graphics::plot(daily_data$Day, daily_data$Consumption_gg, 
#                    type = "l", lwd = 2, col = colors$primary,
#                    xlab = "Day", ylab = "Consumption Rate (g/g/day)", 
#                    main = "Specific Consumption Rate", las = 1)
    
#     # Add mean consumption line
#     mean_consumption <- mean(daily_data$Consumption_gg, na.rm = TRUE)
#     add_plot_annotations(add_grid = TRUE, reference_lines = mean_consumption)
    
#     # Add consumption statistics
#     if (add_stats) {
#       cons_stats <- list(
#         "Mean" = round(mean_consumption, 4),
#         "Max" = round(max(daily_data$Consumption_gg, na.rm = TRUE), 4),
#         "Min" = round(min(daily_data$Consumption_gg, na.rm = TRUE), 4),
#         "CV" = round(sd(daily_data$Consumption_gg, na.rm = TRUE) / mean_consumption * 100, 1)
#       )
      
#       stats_text <- format_statistics_text(cons_stats)
#       add_plot_annotations(stats_text = stats_text, stats_position = "topright")
#     }
    
#     # 2. Cumulative consumption (if requested)
#     if (show_cumulative) {
#       # Calculate total daily consumption (g/day)
#       total_daily_consumption <- daily_data$Consumption_gg * daily_data$Weight
#       cumulative_consumption <- cumsum(total_daily_consumption)
      
#       graphics::plot(daily_data$Day, cumulative_consumption, 
#                      type = "l", lwd = 2, col = colors$secondary,
#                      xlab = "Day", ylab = "Cumulative Consumption (g)", 
#                      main = "Cumulative Food Consumption", las = 1)
      
#       add_plot_annotations(add_grid = TRUE)
      
#       # Add cumulative statistics
#       if (add_stats) {
#         total_consumed <- utils::tail(cumulative_consumption, 1)
#         daily_avg <- total_consumed / length(daily_data$Day)
        
#         cum_stats <- list(
#           "Total" = paste(round(total_consumed, 1), "g"),
#           "Daily avg" = paste(round(daily_avg, 2), "g/day")
#         )
        
#         cum_text <- format_statistics_text(cum_stats)
#         add_plot_annotations(stats_text = cum_text, stats_position = "bottomright")
#       }
#     }
    
#   }, finally = {
#     graphics::par(old_par)
#   })
# }

# # ============================================================================
# # TEMPERATURE PROFILE PLOT
# # ============================================================================

# #' Plot temperature profile and effects
# #'
# #' @description
# #' Creates plots showing temperature over time and its relationship with
# #' biological processes. Includes temperature statistics and correlations.
# #'
# #' @param fb4_result FB4 result object
# #' @param show_correlation Show temperature-consumption correlation, default TRUE
# #' @param add_smooth Add smoothed temperature trend, default TRUE
# #' @param color_scheme Color scheme to use, default "red"
# #' @param add_stats Add statistics annotations, default TRUE
# #'
# #' @return NULL (creates plot)
# #' @export
# #'
# #' @examples
# #' \dontrun{
# #' plot_temperature.fb4_result(result)
# #' plot_temperature.fb4_result(result, show_correlation = FALSE)
# #' }
# plot_temperature.fb4_result <- function(fb4_result, show_correlation = TRUE, 
#                                         add_smooth = TRUE, color_scheme = "red", 
#                                         add_stats = TRUE) {
  
#   # Validate input
#   if (!is.fb4_result(fb4_result)) {
#     stop("Input must be an fb4_result object")
#   }
  
#   daily_data <- fb4_result$daily_output
#   if (is.null(daily_data) || !"Temperature" %in% names(daily_data)) {
#     stop("No temperature data available in daily output")
#   }
  
#   colors <- get_color_scheme(color_scheme)
  
#   # Setup layout
#   layout_spec <- if (show_correlation && "Consumption_gg" %in% names(daily_data)) c(2, 1) else "single"
#   old_par <- setup_plot_layout(layout = layout_spec, margins = "default")
  
#   tryCatch({
    
#     # 1. Temperature over time
#     graphics::plot(daily_data$Day, daily_data$Temperature, 
#                    type = "l", lwd = 2, col = colors$primary,
#                    xlab = "Day", ylab = "Temperature (°C)", 
#                    main = "Temperature Profile", las = 1)
    
#     # Add smooth trend if requested
#     if (add_smooth && nrow(daily_data) > 10) {
#       smooth_temp <- stats::lowess(daily_data$Day, daily_data$Temperature, f = 0.2)
#       graphics::lines(smooth_temp$x, smooth_temp$y, 
#                       col = colors$secondary, lwd = 2, lty = 2)
      
#       graphics::legend("topright", 
#                        legend = c("Temperature", "Trend"), 
#                        col = c(colors$primary, colors$secondary), 
#                        lty = c(1, 2), lwd = 2, cex = 0.8)
#     }
    
#     add_plot_annotations(add_grid = TRUE)
    
#     # Add temperature statistics
#     if (add_stats) {
#       temp_stats <- list(
#         "Mean" = paste(round(mean(daily_data$Temperature, na.rm = TRUE), 1), "°C"),
#         "Range" = paste(round(diff(range(daily_data$Temperature, na.rm = TRUE)), 1), "°C"),
#         "Min" = paste(round(min(daily_data$Temperature, na.rm = TRUE), 1), "°C"),
#         "Max" = paste(round(max(daily_data$Temperature, na.rm = TRUE), 1), "°C")
#       )
      
#       stats_text <- format_statistics_text(temp_stats)
#       add_plot_annotations(stats_text = stats_text, stats_position = "topleft")
#     }
    
#     # 2. Temperature-consumption relationship (if requested and data available)
#     if (show_correlation && "Consumption_gg" %in% names(daily_data)) {
#       graphics::plot(daily_data$Temperature, daily_data$Consumption_gg,
#                      xlab = "Temperature (°C)", ylab = "Consumption Rate (g/g/day)",
#                      main = "Temperature vs Consumption Relationship", 
#                      pch = 16, col = colors$primary, cex = 0.8)
      
#       # Add trend line and correlation
#       if (nrow(daily_data) > 5) {
#         trend <- stats::lowess(daily_data$Temperature, daily_data$Consumption_gg, f = 0.5)
#         graphics::lines(trend$x, trend$y, col = colors$accent, lwd = 2)
        
#         if (add_stats) {
#           correlation <- stats::cor(daily_data$Temperature, daily_data$Consumption_gg, 
#                                     use = "complete.obs")
#           graphics::text(min(daily_data$Temperature, na.rm = TRUE), 
#                          max(daily_data$Consumption_gg, na.rm = TRUE) * 0.9,
#                          paste("r =", round(correlation, 3)), 
#                          col = colors$accent, font = 2, cex = 1.1)
#         }
#       }
      
#       add_plot_annotations(add_grid = TRUE)
#     }
    
#   }, finally = {
#     graphics::par(old_par)
#   })
# }

# # ============================================================================
# # ENERGY COMPONENTS PLOT
# # ============================================================================

# #' Plot energy budget components over time
# #'
# #' @description
# #' Creates plots showing energy flows and budget allocation over the simulation.
# #' Displays energy components and calculates efficiency metrics.
# #'
# #' @param fb4_result FB4 result object
# #' @param components Energy components to plot, default auto-detect available
# #' @param show_efficiency Show growth efficiency subplot, default TRUE
# #' @param color_scheme Color scheme to use, default "purple"
# #' @param add_stats Add statistics annotations, default TRUE
# #'
# #' @return NULL (creates plot)
# #' @export
# #'
# #' @examples
# #' \dontrun{
# #' plot_energy.fb4_result(result)
# #' plot_energy.fb4_result(result, show_efficiency = FALSE)
# #' }
# plot_energy.fb4_result <- function(fb4_result, components = NULL, 
#                                    show_efficiency = TRUE, color_scheme = "purple", 
#                                    add_stats = TRUE) {
  
#   # Validate input
#   if (!is.fb4_result(fb4_result)) {
#     stop("Input must be an fb4_result object")
#   }
  
#   daily_data <- fb4_result$daily_output
#   if (is.null(daily_data)) {
#     stop("No daily output data available")
#   }
  
#   # Auto-detect available energy components
#   if (is.null(components)) {
#     energy_columns <- grep("energy|Energy", names(daily_data), value = TRUE)
#     if (length(energy_columns) == 0) {
#       stop("No energy data available in daily output")
#     }
#     components <- energy_columns[1:min(4, length(energy_columns))]  # Limit to 4 for readability
#   }
  
#   # Validate components exist
#   available_components <- intersect(components, names(daily_data))
#   if (length(available_components) == 0) {
#     stop("No specified energy components found in data. Available: ", 
#          paste(names(daily_data), collapse = ", "))
#   }
  
#   colors <- get_color_scheme(color_scheme)
  
#   # Setup layout
#   layout_spec <- if (show_efficiency && all(c("Consumption_energy", "Net_energy") %in% names(daily_data))) {
#     c(2, 1)
#   } else {
#     "single"
#   }
#   old_par <- setup_plot_layout(layout = layout_spec, margins = "default")
  
#   tryCatch({
    
#     # 1. Energy components over time
#     # Define colors for different components
#     component_colors <- c("blue", "red", "green", "orange", "purple")[1:length(available_components)]
#     names(component_colors) <- available_components
    
#     # Find y-axis range
#     y_range <- range(daily_data[available_components], na.rm = TRUE)
    
#     # Create base plot
#     graphics::plot(daily_data$Day, daily_data[[available_components[1]]], 
#                    type = "l", lwd = 2, col = component_colors[1],
#                    ylim = y_range, xlab = "Day", ylab = "Energy (J/g/day)",
#                    main = "Energy Budget Components", las = 1)
    
#     # Add other components
#     if (length(available_components) > 1) {
#       for (i in 2:length(available_components)) {
#         graphics::lines(daily_data$Day, daily_data[[available_components[i]]], 
#                         lwd = 2, col = component_colors[i])
#       }
#     }
    
#     # Add legend
#     graphics::legend("topright", legend = available_components, 
#                      col = component_colors, lwd = 2, cex = 0.9, bg = "white")
    
#     add_plot_annotations(add_grid = TRUE)
    
#     # Add energy statistics
#     if (add_stats) {
#       # Calculate mean values for each component
#       energy_means <- sapply(available_components, function(comp) {
#         mean(daily_data[[comp]], na.rm = TRUE)
#       })
      
#       # Show only the first 3 components to avoid clutter
#       display_components <- available_components[1:min(3, length(available_components))]
#       energy_stats <- as.list(round(energy_means[display_components], 0))
#       names(energy_stats) <- paste("Mean", display_components)
      
#       stats_text <- format_statistics_text(energy_stats)
#       add_plot_annotations(stats_text = stats_text, stats_position = "topleft")
#     }
    
#     # 2. Growth efficiency over time (if requested and data available)
#     if (show_efficiency && all(c("Consumption_energy", "Net_energy") %in% names(daily_data))) {
#       efficiency <- daily_data$Net_energy / daily_data$Consumption_energy * 100
#       efficiency[!is.finite(efficiency)] <- 0
      
#       graphics::plot(daily_data$Day, efficiency, 
#                      type = "l", lwd = 2, col = colors$accent,
#                      xlab = "Day", ylab = "Growth Efficiency (%)", 
#                      main = "Growth Efficiency Over Time", las = 1)
      
#       add_plot_annotations(add_grid = TRUE)
      
#       # Add efficiency statistics
#       if (add_stats) {
#         valid_efficiency <- efficiency[efficiency > 0 & is.finite(efficiency)]
        
#         if (length(valid_efficiency) > 0) {
#           eff_stats <- list(
#             "Mean" = paste(round(mean(valid_efficiency), 1), "%"),
#             "Max" = paste(round(max(valid_efficiency), 1), "%"),
#             "Min" = paste(round(min(valid_efficiency), 1), "%")
#           )
          
#           eff_text <- format_statistics_text(eff_stats)
#           add_plot_annotations(stats_text = eff_text, stats_position = "topright")
#         }
#       }
#     }
    
#   }, finally = {
#     graphics::par(old_par)
#   })
# }

# # ============================================================================
# # DASHBOARD PANEL HELPER FUNCTIONS
# # ============================================================================

# #' Dashboard Growth Panel
# #' @keywords internal
# plot_dashboard_growth_panel <- function(daily_data, colors, compact_layout) {
  
#   graphics::plot(daily_data$Day, daily_data$Weight, 
#                  type = "l", lwd = 2, col = colors$primary,
#                  xlab = "Day", ylab = "Weight (g)", 
#                  main = "Growth", las = 1)
  
#   add_plot_annotations(add_grid = TRUE)
  
#   # Add growth percentage
#   initial_weight <- daily_data$Weight[1]
#   final_weight <- utils::tail(daily_data$Weight, 1)
#   growth_pct <- round((final_weight/initial_weight - 1) * 100, 1)
  
#   if (compact_layout) {
#     graphics::text(max(daily_data$Day) * 0.7, max(daily_data$Weight) * 0.9,
#                    paste(growth_pct, "%"), cex = 1.2, font = 2, col = colors$primary)
#   } else {
#     graphics::text(max(daily_data$Day) * 0.7, max(daily_data$Weight) * 0.85,
#                    paste("Growth:", growth_pct, "%\nFinal:", round(final_weight, 1), "g"),
#                    cex = 0.9, font = 2, col = colors$primary, bg = "white")
#   }
# }

# #' Dashboard Consumption Panel
# #' @keywords internal
# plot_dashboard_consumption_panel <- function(daily_data, colors, compact_layout) {
  
#   graphics::plot(daily_data$Day, daily_data$Consumption_gg, 
#                  type = "l", lwd = 2, col = colors$secondary,
#                  xlab = "Day", ylab = "Consumption (g/g/day)", 
#                  main = "Consumption Rate", las = 1)
  
#   add_plot_annotations(add_grid = TRUE)
  
#   mean_cons <- mean(daily_data$Consumption_gg, na.rm = TRUE)
  
#   if (compact_layout) {
#     graphics::text(max(daily_data$Day) * 0.7, max(daily_data$Consumption_gg) * 0.9,
#                    round(mean_cons, 4), cex = 1.0, font = 2, col = colors$secondary)
#   } else {
#     graphics::text(max(daily_data$Day) * 0.7, max(daily_data$Consumption_gg) * 0.85,
#                    paste("Mean:", round(mean_cons, 4)),
#                    cex = 0.9, font = 2, col = colors$secondary, bg = "white")
#   }
# }

# #' Dashboard Temperature Panel
# #' @keywords internal
# plot_dashboard_temperature_panel <- function(daily_data, colors, compact_layout) {
  
#   graphics::plot(daily_data$Day, daily_data$Temperature, 
#                  type = "l", lwd = 2, col = colors$accent,
#                  xlab = "Day", ylab = "Temperature (°C)", 
#                  main = "Temperature", las = 1)
  
#   add_plot_annotations(add_grid = TRUE)
  
#   mean_temp <- mean(daily_data$Temperature, na.rm = TRUE)
  
#   if (compact_layout) {
#     graphics::text(max(daily_data$Day) * 0.7, max(daily_data$Temperature) * 0.9,
#                    paste(round(mean_temp, 1), "°C"), cex = 1.1, font = 2, col = colors$accent)
#   } else {
#     temp_range <- diff(range(daily_data$Temperature, na.rm = TRUE))
#     graphics::text(max(daily_data$Day) * 0.7, max(daily_data$Temperature) * 0.85,
#                    paste("Mean:", round(mean_temp, 1), "°C\nRange:", round(temp_range, 1), "°C"),
#                    cex = 0.9, font = 2, col = colors$accent, bg = "white")
#   }
# }

# #' Dashboard Energy Panel
# #' @keywords internal
# plot_dashboard_energy_panel <- function(daily_data, colors, compact_layout) {
  
#   # Try to find efficiency data
#   if (all(c("Consumption_energy", "Net_energy") %in% names(daily_data))) {
#     efficiency <- daily_data$Net_energy / daily_data$Consumption_energy * 100
#     efficiency[!is.finite(efficiency)] <- 0
    
#     graphics::plot(daily_data$Day, efficiency, 
#                    type = "l", lwd = 2, col = "purple",
#                    xlab = "Day", ylab = "Efficiency (%)", 
#                    main = "Growth Efficiency", las = 1)
    
#     add_plot_annotations(add_grid = TRUE)
    
#     mean_eff <- mean(efficiency[efficiency > 0], na.rm = TRUE)
    
#     if (is.finite(mean_eff)) {
#       if (compact_layout) {
#         graphics::text(max(daily_data$Day) * 0.7, max(efficiency) * 0.9,
#                        paste(round(mean_eff, 1), "%"), cex = 1.1, font = 2, col = "purple")
#       } else {
#         graphics::text(max(daily_data$Day) * 0.7, max(efficiency) * 0.85,
#                        paste("Mean:", round(mean_eff, 1), "%"),
#                        cex = 0.9, font = 2, col = "purple", bg = "white")
#       }
#     }
#   } else {
#     # Show first available energy component
#     energy_cols <- grep("energy|Energy", names(daily_data), value = TRUE)
#     if (length(energy_cols) > 0) {
#       energy_col <- energy_cols[1]
#       graphics::plot(daily_data$Day, daily_data[[energy_col]], 
#                      type = "l", lwd = 2, col = "purple",
#                      xlab = "Day", ylab = "Energy (J/g/day)", 
#                      main = "Energy", las = 1)
      
#       add_plot_annotations(add_grid = TRUE)
      
#       mean_energy <- mean(daily_data[[energy_col]], na.rm = TRUE)
#       graphics::text(max(daily_data$Day) * 0.7, max(daily_data[[energy_col]]) * 0.9,
#                      round(mean_energy, 0), cex = 1.0, font = 2, col = "purple")
#     } else {
#       plot_dashboard_placeholder("Energy data\nnot available", colors)
#     }
#   }
# }

# #' Dashboard Method Info Panel
# #' @keywords internal
# plot_dashboard_method_info_panel <- function(fb4_result, capabilities, colors) {
  
#   graphics::plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
#                  xlab = "", ylab = "", main = "Method Info", axes = FALSE)
  
#   # Create info text
#   info_lines <- c(
#     paste("Method:", capabilities$method),
#     "",
#     paste("Backend:", capabilities$backend),
#     paste("Has uncertainty:", ifelse(capabilities$has_uncertainty, "Yes", "No"))
#   )
  
#   # Add convergence info if available
#   if (!is.null(fb4_result$summary$converged)) {
#     converged_text <- ifelse(fb4_result$summary$converged, "Yes", "No")
#     info_lines <- c(info_lines, paste("Converged:", converged_text))
#   }
  
#   # Display text
#   for (i in seq_along(info_lines)) {
#     graphics::text(0.1, 0.8 - (i-1) * 0.15, info_lines[i], 
#                    adj = 0, cex = 0.9, font = if(info_lines[i] == "") 1 else 2)
#   }
# }

# #' Dashboard Summary Panel
# #' @keywords internal
# plot_dashboard_summary_panel <- function(fb4_result, colors) {
  
#   graphics::plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
#                  xlab = "", ylab = "", main = "Summary", axes = FALSE)
  
#   summary_data <- fb4_result$summary
  
#   if (is.null(summary_data)) {
#     graphics::text(0.5, 0.5, "No summary\ndata available", 
#                    cex = 1.0, adj = 0.5, col = "gray")
#     return()
#   }
  
#   # Extract key summary statistics
#   summary_lines <- c()
  
#   if (!is.null(summary_data$initial_weight)) {
#     summary_lines <- c(summary_lines, 
#                        paste("Initial:", round(summary_data$initial_weight, 1), "g"))
#   }
  
#   if (!is.null(summary_data$final_weight)) {
#     summary_lines <- c(summary_lines, 
#                        paste("Final:", round(summary_data$final_weight, 1), "g"))
#   } else if (!is.null(summary_data$predicted_weight)) {
#     summary_lines <- c(summary_lines, 
#                        paste("Predicted:", round(summary_data$predicted_weight, 1), "g"))
#   }
  
#   if (!is.null(summary_data$total_consumption_g)) {
#     summary_lines <- c(summary_lines, 
#                        paste("Total cons:", round(summary_data$total_consumption_g, 1), "g"))
#   }
  
#   if (!is.null(summary_data$p_value) || !is.null(summary_data$p_estimate)) {
#     p_val <- summary_data$p_value %||% summary_data$p_estimate
#     summary_lines <- c(summary_lines, 
#                        paste("p_value:", round(p_val, 4)))
#   }
  
#   if (!is.null(summary_data$simulation_days)) {
#     summary_lines <- c(summary_lines, 
#                        paste("Days:", summary_data$simulation_days))
#   }
  
#   # Display summary
#   if (length(summary_lines) > 0) {
#     for (i in seq_along(summary_lines)) {
#       graphics::text(0.1, 0.8 - (i-1) * 0.12, summary_lines[i], 
#                      adj = 0, cex = 0.9, font = 1)
#     }
#   } else {
#     graphics::text(0.5, 0.5, "No summary\nstatistics", 
#                    cex = 1.0, adj = 0.5, col = "gray")
#   }
# }

# #' Dashboard Placeholder Panel
# #' @keywords internal
# plot_dashboard_placeholder <- function(message, colors) {
#   graphics::plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
#                  xlab = "", ylab = "", main = "", axes = FALSE)
#   graphics::text(0.5, 0.5, message, cex = 1.0, adj = 0.5, col = "gray")
# }

# # ============================================================================
# # HELPER FUNCTIONS
# # ============================================================================

# #' Create Dashboard Title
# #' @keywords internal
# create_dashboard_title <- function(fb4_result, capabilities, title_override) {
  
#   if (!is.null(title_override)) {
#     return(title_override)
#   }
  
#   # Try to get species name
#   species_name <- NULL
#   if (!is.null(fb4_result$bioenergetic_object$species_info$scientific_name)) {
#     species_name <- fb4_result$bioenergetic_object$species_info$scientific_name
#   } else if (!is.null(fb4_result$bioenergetic_object$species_info$common_name)) {
#     species_name <- fb4_result$bioenergetic_object$species_info$common_name
#   }
  
#   # Create title
#   if (!is.null(species_name)) {
#     title <- paste("FB4 Results:", species_name)
#   } else {
#     title <- "FB4 Simulation Results"
#   }
  
#   # Add method info if not traditional
#   if (capabilities$method != "binary_search") {
#     title <- paste(title, "-", toupper(capabilities$method))
#   }
  
#   return(title)
# }