# #' Bioenergetic Object Plots for Setup Validation
# #'
# #' @description
# #' Plotting functions for Bioenergetic objects (before simulation).
# #' Focuses on data validation, setup verification, and input data analysis.
# #' These plots help users validate their model setup before running simulations.
# #'
# #' @name fb4-bioenergetic-plots
# #' @importFrom graphics plot lines abline text legend grid points barplot
# #' @importFrom grDevices gray.colors
# #' @importFrom utils tail
# NULL

# # ============================================================================
# # MAIN S3 PLOT METHOD
# # ============================================================================

# #' Plot Bioenergetic object setup and input data
# #'
# #' @description
# #' Main plotting method for Bioenergetic objects. Provides visualization
# #' of input data and model setup before running simulations.
# #'
# #' @param x Object of class Bioenergetic
# #' @param type Type of plot to create:
# #'   \itemize{
# #'     \item{\code{"dashboard"} - Multi-panel overview (default)}
# #'     \item{\code{"temperature"} - Temperature profile over time}
# #'     \item{\code{"diet"} - Diet composition and prey energies}
# #'     \item{\code{"energy"} - Energy densities (predator and prey)}
# #'   }
# #' @param save_plot Optional path to save plot (supports .png and .pdf)
# #' @param ... Additional arguments passed to specific plot functions
# #'
# #' @return Invisibly returns the input object
# #'
# #' @details
# #' These plots are designed to validate model setup before simulation:
# #' \itemize{
# #'   \item{Dashboard: Overview of all components and readiness status}
# #'   \item{Temperature: Temporal profile with gaps and interpolations}
# #'   \item{Diet: Prey composition and energy densities over time}
# #'   \item{Energy: Predator and prey energy densities comparison}
# #' }
# #'
# #' @export
# #'
# #' @examples
# #' \dontrun{
# #' # Basic plots
# #' plot(bio_obj)                          # Dashboard overview
# #' plot(bio_obj, type = "temperature")    # Temperature profile
# #' plot(bio_obj, type = "diet")           # Diet composition
# #' plot(bio_obj, type = "energy")         # Energy densities
# #' 
# #' # Save plot
# #' plot(bio_obj, save_plot = "setup.png")
# #' }
# plot.Bioenergetic <- function(x, type = "dashboard", save_plot = NULL, ...) {
  
#   # Validate input
#   if (!is.Bioenergetic(x)) {
#     stop("Input must be a Bioenergetic object")
#   }
  
#   # Check available plot types
#   available_types <- get_available_bioenergetic_plot_types(x)
#   if (!type %in% available_types) {
#     stop("Plot type '", type, "' not available. Available types: ", 
#          paste(available_types, collapse = ", "))
#   }
  
#   # Setup device for saving if requested
#   old_par <- NULL
#   if (!is.null(save_plot)) {
#     old_par <- setup_save_device(save_plot)
#   }
  
#   # Route to appropriate plot function
#   tryCatch({
#     switch(type,
#            "dashboard" = plot_setup_dashboard.Bioenergetic(x, ...),
#            "temperature" = plot_temperature_profile.Bioenergetic(x, ...),
#            "diet" = plot_diet_composition.Bioenergetic(x, ...),
#            "energy" = plot_energy_densities.Bioenergetic(x, ...),
#            stop("Unknown plot type: '", type, "'")
#     )
#   }, finally = {
#     # Clean up device and restore settings
#     if (!is.null(save_plot)) {
#       cleanup_save_device(save_plot, old_par)
#     }
#   })
  
#   invisible(x)
# }

# # ============================================================================
# # SETUP DASHBOARD PLOT
# # ============================================================================

# #' Create setup dashboard overview
# #'
# #' @description
# #' Creates a 2x2 dashboard showing model setup status, key parameters,
# #' data availability, and simulation readiness.
# #'
# #' @param bio_obj Bioenergetic object
# #' @param color_scheme Color scheme: "blue", "green", "red", or custom color
# #' @param add_title Add main title, default TRUE
# #' @param title_override Custom title (overrides automatic title)
# #'
# #' @return NULL (creates plot)
# #' @export
# #'
# #' @examples
# #' \dontrun{
# #' plot_setup_dashboard.Bioenergetic(bio_obj)
# #' plot_setup_dashboard.Bioenergetic(bio_obj, color_scheme = "green")
# #' }
# plot_setup_dashboard.Bioenergetic <- function(bio_obj, color_scheme = "blue", 
#                                               add_title = TRUE, title_override = NULL) {
  
#   # Validate input
#   if (!is.Bioenergetic(bio_obj)) {
#     stop("Input must be a Bioenergetic object")
#   }
  
#   colors <- get_color_scheme(color_scheme)
  
#   # Setup 2x2 layout
#   old_par <- setup_plot_layout(layout = c(2, 2), margins = "compact")
#   if (add_title) {
#     graphics::par(oma = c(0, 0, 3, 0))  # Add outer margin for title
#   }
  
#   tryCatch({
    
#     # Panel 1: Component status
#     plot_component_status_panel(bio_obj, colors)
    
#     # Panel 2: Key parameters summary
#     plot_parameters_summary_panel(bio_obj, colors)
    
#     # Panel 3: Data availability
#     plot_data_availability_panel(bio_obj, colors)
    
#     # Panel 4: Simulation readiness
#     plot_readiness_panel(bio_obj, colors)
    
#     # Add main title
#     if (add_title) {
#       main_title <- create_bioenergetic_title(bio_obj, title_override)
#       graphics::mtext(main_title, outer = TRUE, cex = 1.2, font = 2, line = 1)
#     }
    
#   }, finally = {
#     graphics::par(old_par)
#   })
# }

# # ============================================================================
# # TEMPERATURE PROFILE PLOT
# # ============================================================================

# #' Plot temperature profile over time
# #'
# #' @description
# #' Creates plots showing temperature temporal series with statistics
# #' and data quality information.
# #'
# #' @param bio_obj Bioenergetic object
# #' @param color_scheme Color scheme to use, default "red"
# #' @param add_stats Add statistics annotations, default TRUE
# #' @param show_range Add temperature range information, default TRUE
# #'
# #' @return NULL (creates plot)
# #' @export
# #'
# #' @examples
# #' \dontrun{
# #' plot_temperature_profile.Bioenergetic(bio_obj)
# #' plot_temperature_profile.Bioenergetic(bio_obj, show_range = FALSE)
# #' }
# plot_temperature_profile.Bioenergetic <- function(bio_obj, color_scheme = "red", 
#                                                   add_stats = TRUE, show_range = TRUE) {
  
#   # Validate input
#   if (!is.Bioenergetic(bio_obj)) {
#     stop("Input must be a Bioenergetic object")
#   }
  
#   temp_data <- bio_obj$environmental_data$temperature
#   if (is.null(temp_data)) {
#     stop("No temperature data available in Bioenergetic object")
#   }
  
#   colors <- get_color_scheme(color_scheme)
  
#   old_par <- setup_plot_layout(layout = "single", margins = "default")
  
#   tryCatch({
    
#     # Main temperature plot
#     graphics::plot(temp_data$Day, temp_data$Temperature, 
#                    type = "l", lwd = 2, col = colors$primary,
#                    xlab = "Day", ylab = "Temperature (°C)", 
#                    main = "Temperature Profile", las = 1)
    
#     # Add mean temperature line
#     mean_temp <- mean(temp_data$Temperature, na.rm = TRUE)
#     add_plot_annotations(add_grid = TRUE, reference_lines = mean_temp)
    
#     # Add temperature range if requested
#     if (show_range) {
#       temp_range <- range(temp_data$Temperature, na.rm = TRUE)
#       graphics::abline(h = temp_range, col = colors$secondary, lty = 3, lwd = 1)
#     }
    
#     # Add temperature statistics
#     if (add_stats) {
#       temp_stats <- list(
#         "Mean" = paste(round(mean_temp, 1), "°C"),
#         "Range" = paste(round(diff(range(temp_data$Temperature, na.rm = TRUE)), 1), "°C"),
#         "Min" = paste(round(min(temp_data$Temperature, na.rm = TRUE), 1), "°C"),
#         "Max" = paste(round(max(temp_data$Temperature, na.rm = TRUE), 1), "°C"),
#         "Days" = nrow(temp_data)
#       )
      
#       stats_text <- format_statistics_text(temp_stats)
#       add_plot_annotations(stats_text = stats_text, stats_position = "topleft")
#     }
    
#   }, finally = {
#     graphics::par(old_par)
#   })
# }

# # ============================================================================
# # DIET COMPOSITION PLOT
# # ============================================================================

# #' Plot diet composition and prey energies over time
# #'
# #' @description
# #' Creates plots showing diet composition as stacked areas and
# #' prey energy densities over the simulation period.
# #'
# #' @param bio_obj Bioenergetic object
# #' @param show_energies Show prey energy densities subplot, default TRUE
# #' @param color_scheme Color scheme to use, default "green"
# #' @param max_prey_display Maximum number of prey to display, default 5
# #'
# #' @return NULL (creates plot)
# #' @export
# #'
# #' @examples
# #' \dontrun{
# #' plot_diet_composition.Bioenergetic(bio_obj)
# #' plot_diet_composition.Bioenergetic(bio_obj, show_energies = FALSE)
# #' }
# plot_diet_composition.Bioenergetic <- function(bio_obj, show_energies = TRUE, 
#                                                color_scheme = "green", max_prey_display = 5) {
  
#   # Validate input
#   if (!is.Bioenergetic(bio_obj)) {
#     stop("Input must be a Bioenergetic object")
#   }
  
#   diet_data <- bio_obj$diet_data
#   if (is.null(diet_data) || is.null(diet_data$proportions)) {
#     stop("No diet data available in Bioenergetic object")
#   }
  
#   colors <- get_color_scheme(color_scheme)
#   prey_names <- diet_data$prey_names
  
#   # Limit number of prey displayed
#   if (length(prey_names) > max_prey_display) {
#     prey_names <- prey_names[1:max_prey_display]
#     warning("Displaying only first ", max_prey_display, " prey species")
#   }
  
#   # Setup layout
#   layout_spec <- if (show_energies && !is.null(diet_data$energies)) c(2, 1) else "single"
#   old_par <- setup_plot_layout(layout = layout_spec, margins = "default")
  
#   tryCatch({
    
#     # 1. Diet composition (stacked area plot)
#     diet_props <- diet_data$proportions
    
#     # Create stacked area plot
#     plot_stacked_diet_composition(diet_props, prey_names, colors)
    
#     # 2. Prey energy densities (if requested and available)
#     if (show_energies && !is.null(diet_data$energies)) {
#       plot_prey_energy_densities(diet_data$energies, prey_names, colors)
#     }
    
#   }, finally = {
#     graphics::par(old_par)
#   })
# }

# # ============================================================================
# # ENERGY DENSITIES PLOT
# # ============================================================================

# #' Plot energy densities for predator and prey
# #'
# #' @description
# #' Creates plots showing predator energy density over time and
# #' comparison with prey energy densities.
# #'
# #' @param bio_obj Bioenergetic object
# #' @param color_scheme Color scheme to use, default "purple"
# #' @param show_prey_comparison Show prey energy comparison, default TRUE
# #'
# #' @return NULL (creates plot)
# #' @export
# #'
# #' @examples
# #' \dontrun{
# #' plot_energy_densities.Bioenergetic(bio_obj)
# #' plot_energy_densities.Bioenergetic(bio_obj, show_prey_comparison = FALSE)
# #' }
# plot_energy_densities.Bioenergetic <- function(bio_obj, color_scheme = "purple", 
#                                                show_prey_comparison = TRUE) {
  
#   # Validate input
#   if (!is.Bioenergetic(bio_obj)) {
#     stop("Input must be a Bioenergetic object")
#   }
  
#   # Check for predator energy data
#   predator_params <- bio_obj$species_params$predator
#   if (is.null(predator_params)) {
#     stop("No predator parameters available in Bioenergetic object")
#   }
  
#   colors <- get_color_scheme(color_scheme)
  
#   # Setup layout based on available data
#   has_prey_energies <- !is.null(bio_obj$diet_data$energies)
#   layout_spec <- if (show_prey_comparison && has_prey_energies) c(2, 1) else "single"
#   old_par <- setup_plot_layout(layout = layout_spec, margins = "default")
  
#   tryCatch({
    
#     # 1. Predator energy density
#     plot_predator_energy_density(bio_obj, predator_params, colors)
    
#     # 2. Prey energy comparison (if requested and available)
#     if (show_prey_comparison && has_prey_energies) {
#       plot_predator_vs_prey_energies(bio_obj, colors)
#     }
    
#   }, finally = {
#     graphics::par(old_par)
#   })
# }

# # ============================================================================
# # HELPER FUNCTIONS FOR DASHBOARD PANELS
# # ============================================================================

# #' Component Status Panel
# #' @keywords internal
# plot_component_status_panel <- function(bio_obj, colors) {
  
#   graphics::plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
#                  xlab = "", ylab = "", main = "Component Status", axes = FALSE)
  
#   # Check component availability
#   has_params <- !is.null(bio_obj$species_params) && length(bio_obj$species_params) > 0
#   has_temp <- !is.null(bio_obj$environmental_data$temperature)
#   has_diet <- !is.null(bio_obj$diet_data$proportions)
#   has_initial <- !is.null(bio_obj$simulation_settings$initial_weight)
  
#   components <- list(
#     "Parameters" = has_params,
#     "Temperature" = has_temp,
#     "Diet data" = has_diet,
#     "Initial weight" = has_initial
#   )
  
#   # Display status
#   y_positions <- seq(0.8, 0.2, length.out = length(components))
#   for (i in seq_along(components)) {
#     comp_name <- names(components)[i]
#     status <- components[[i]]
#     symbol <- if (status) "✓" else "✗"
#     color <- if (status) "darkgreen" else "red"
    
#     graphics::text(0.1, y_positions[i], paste(symbol, comp_name), 
#                    adj = 0, cex = 1.0, col = color, font = 2)
#   }
# }

# #' Parameters Summary Panel
# #' @keywords internal
# plot_parameters_summary_panel <- function(bio_obj, colors) {
  
#   graphics::plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
#                  xlab = "", ylab = "", main = "Key Parameters", axes = FALSE)
  
#   params <- bio_obj$species_params
#   if (is.null(params)) {
#     graphics::text(0.5, 0.5, "No parameters\navailable", cex = 1.0, adj = 0.5)
#     return()
#   }
  
#   # Extract key parameters
#   param_info <- c()
  
#   if (!is.null(params$consumption)) {
#     ceq <- params$consumption$CEQ %||% "?"
#     param_info <- c(param_info, paste("Consumption eq:", ceq))
#   }
  
#   if (!is.null(params$respiration)) {
#     req <- params$respiration$REQ %||% "?"
#     param_info <- c(param_info, paste("Respiration eq:", req))
#   }
  
#   if (!is.null(params$egestion)) {
#     egeq <- params$egestion$EGEQ %||% "?"
#     param_info <- c(param_info, paste("Egestion eq:", egeq))
#   }
  
#   if (!is.null(params$excretion)) {
#     exeq <- params$excretion$EXEQ %||% "?"
#     param_info <- c(param_info, paste("Excretion eq:", exeq))
#   }
  
#   # Display parameter info
#   if (length(param_info) > 0) {
#     y_positions <- seq(0.8, 0.2, length.out = length(param_info))
#     for (i in seq_along(param_info)) {
#       graphics::text(0.1, y_positions[i], param_info[i], 
#                      adj = 0, cex = 0.9, font = 1)
#     }
#   } else {
#     graphics::text(0.5, 0.5, "No equation\ninformation", cex = 1.0, adj = 0.5)
#   }
# }

# #' Data Availability Panel
# #' @keywords internal
# plot_data_availability_panel <- function(bio_obj, colors) {
  
#   graphics::plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
#                  xlab = "", ylab = "", main = "Data Coverage", axes = FALSE)
  
#   data_info <- c()
  
#   # Temperature data
#   if (!is.null(bio_obj$environmental_data$temperature)) {
#     temp_days <- nrow(bio_obj$environmental_data$temperature)
#     temp_range <- range(bio_obj$environmental_data$temperature$Day)
#     data_info <- c(data_info, paste("Temperature:", temp_days, "days"))
#     data_info <- c(data_info, paste("Range:", temp_range[1], "-", temp_range[2]))
#   }
  
#   # Diet data
#   if (!is.null(bio_obj$diet_data$proportions)) {
#     diet_days <- nrow(bio_obj$diet_data$proportions)
#     prey_count <- length(bio_obj$diet_data$prey_names %||% c())
#     data_info <- c(data_info, paste("Diet:", diet_days, "days"))
#     data_info <- c(data_info, paste("Prey species:", prey_count))
#   }
  
#   # Display data info
#   if (length(data_info) > 0) {
#     y_positions <- seq(0.8, 0.2, length.out = length(data_info))
#     for (i in seq_along(data_info)) {
#       graphics::text(0.1, y_positions[i], data_info[i], 
#                      adj = 0, cex = 0.9, font = 1)
#     }
#   } else {
#     graphics::text(0.5, 0.5, "No temporal\ndata available", cex = 1.0, adj = 0.5)
#   }
# }

# #' Readiness Panel
# #' @keywords internal
# plot_readiness_panel <- function(bio_obj, colors) {
  
#   graphics::plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
#                  xlab = "", ylab = "", main = "Simulation Ready", axes = FALSE)
  
#   # Check readiness
#   has_params <- !is.null(bio_obj$species_params) && length(bio_obj$species_params) > 0
#   has_temp <- !is.null(bio_obj$environmental_data$temperature)
#   has_diet <- !is.null(bio_obj$diet_data$proportions)
#   has_initial <- !is.null(bio_obj$simulation_settings$initial_weight)
  
#   ready_count <- sum(has_params, has_temp, has_diet, has_initial)
#   total_required <- 4
  
#   # Display readiness status
#   if (ready_count == total_required) {
#     graphics::text(0.5, 0.6, "✓ READY", cex = 2.0, adj = 0.5, 
#                    col = "darkgreen", font = 2)
#     graphics::text(0.5, 0.3, "All components\navailable", cex = 1.0, adj = 0.5)
#   } else {
#     graphics::text(0.5, 0.6, "✗ NOT READY", cex = 1.5, adj = 0.5, 
#                    col = "red", font = 2)
#     graphics::text(0.5, 0.3, paste(ready_count, "/", total_required, 
#                                    "components\navailable"), cex = 1.0, adj = 0.5)
#   }
# }

# # ============================================================================
# # HELPER FUNCTIONS FOR DIET PLOTS
# # ============================================================================

# #' Plot Stacked Diet Composition
# #' @keywords internal
# plot_stacked_diet_composition <- function(diet_props, prey_names, colors) {
  
#   # Create stacked area plot
#   diet_matrix <- as.matrix(diet_props[, prey_names, drop = FALSE])
  
#   # Generate colors for prey species
#   n_prey <- length(prey_names)
#   prey_colors <- rainbow(n_prey, alpha = 0.7)
  
#   # Plot base
#   graphics::plot(diet_props$Day, rep(0, nrow(diet_props)), 
#                  type = "n", ylim = c(0, 1),
#                  xlab = "Day", ylab = "Diet Proportion", 
#                  main = "Diet Composition Over Time", las = 1)
  
#   # Add stacked areas
#   cumulative <- rep(0, nrow(diet_props))
#   for (i in 1:n_prey) {
#     cumulative_new <- cumulative + diet_matrix[, i]
    
#     # Create polygon for this prey
#     x_coords <- c(diet_props$Day, rev(diet_props$Day))
#     y_coords <- c(cumulative, rev(cumulative_new))
    
#     graphics::polygon(x_coords, y_coords, col = prey_colors[i], border = NA)
    
#     cumulative <- cumulative_new
#   }
  
#   add_plot_annotations(add_grid = TRUE)
  
#   # Add legend
#   graphics::legend("topright", legend = prey_names, 
#                    fill = prey_colors, cex = 0.8, bg = "white")
# }

# #' Plot Prey Energy Densities
# #' @keywords internal
# plot_prey_energy_densities <- function(energy_data, prey_names, colors) {
  
#   # Find y-axis range
#   energy_matrix <- as.matrix(energy_data[, prey_names, drop = FALSE])
#   y_range <- range(energy_matrix, na.rm = TRUE)
  
#   # Generate colors for prey species
#   n_prey <- length(prey_names)
#   prey_colors <- rainbow(n_prey)
  
#   # Create base plot
#   graphics::plot(energy_data$Day, energy_matrix[, 1], 
#                  type = "l", lwd = 2, col = prey_colors[1],
#                  ylim = y_range, xlab = "Day", ylab = "Energy Density (J/g)",
#                  main = "Prey Energy Densities", las = 1)
  
#   # Add other prey species
#   if (n_prey > 1) {
#     for (i in 2:n_prey) {
#       graphics::lines(energy_data$Day, energy_matrix[, i], 
#                       lwd = 2, col = prey_colors[i])
#     }
#   }
  
#   add_plot_annotations(add_grid = TRUE)
  
#   # Add legend
#   graphics::legend("topright", legend = prey_names, 
#                    col = prey_colors, lwd = 2, cex = 0.8, bg = "white")
# }

# # ============================================================================
# # HELPER FUNCTIONS FOR ENERGY PLOTS
# # ============================================================================

# #' Plot Predator Energy Density
# #' @keywords internal
# plot_predator_energy_density <- function(bio_obj, predator_params, colors) {
  
#   PREDEDEQ <- predator_params$PREDEDEQ %||% 1
  
#   if (PREDEDEQ == 1) {
#     # Time-series energy data
#     plot_predator_energy_timeseries(bio_obj, predator_params, colors)
#   } else if (PREDEDEQ == 2) {
#     # Weight-based equation
#     plot_predator_energy_equation(predator_params, colors)
#   } else {
#     graphics::plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
#                    main = "Predator Energy Density", xlab = "", ylab = "", axes = FALSE)
#     graphics::text(0.5, 0.5, paste("PREDEDEQ", PREDEDEQ, "\nnot supported"), 
#                    cex = 1.2, adj = 0.5)
#   }
# }

# #' Plot Predator Energy Time Series
# #' @keywords internal
# plot_predator_energy_timeseries <- function(bio_obj, predator_params, colors) {
  
#   # Get simulation duration
#   duration <- bio_obj$simulation_settings$duration %||% 
#              bio_obj$environmental_data$duration %||% 365
  
#   days <- 1:duration
  
#   # Check for ED_data
#   if (!is.null(predator_params$ED_data) & !is.na(predator_params$ED_data)) {
#     energy_values <- rep(predator_params$ED_data, length.out = duration)
#   } else if (!is.null(predator_params$ED_ini) && !is.null(predator_params$ED_end) | 
#              !is.na(predator_params$ED_ini) && !is.na(predator_params$ED_end)) {
#     # Linear interpolation
#     energy_values <- seq(from = predator_params$ED_ini, 
#                          to = predator_params$ED_end, 
#                          length.out = duration)
#   } else {
#     stop("PREDEDEQ=1 requires either ED_data or both ED_ini and ED_end")
#   }
  
#   # Plot
#   graphics::plot(days, energy_values, 
#                  type = "l", lwd = 2, col = colors$primary,
#                  xlab = "Day", ylab = "Energy Density (J/g)", 
#                  main = "Predator Energy Density", las = 1)
  
#   add_plot_annotations(add_grid = TRUE)
  
#   # Add statistics
#   stats_text <- format_statistics_text(list(
#     "Mean" = paste(round(mean(energy_values), 0), "J/g"),
#     "Range" = paste(round(diff(range(energy_values)), 0), "J/g")
#   ))
#   add_plot_annotations(stats_text = stats_text, stats_position = "topright")
# }

# #' Plot Predator Energy Equation
# #' @keywords internal
# plot_predator_energy_equation <- function(predator_params, colors) {
  
#   # Extract equation parameters
#   Alpha1 <- predator_params$Alpha1 %||% 0
#   Beta1 <- predator_params$Beta1 %||% 0
#   Alpha2 <- predator_params$Alpha2 %||% 0
#   Beta2 <- predator_params$Beta2 %||% 0
#   Cutoff <- predator_params$Cutoff %||% 100
  
#   # Create weight range for plotting
#   weights <- seq(1, 1000, length.out = 100)
  
#   # Calculate energy densities using FB4 equation
#   energy_values <- ifelse(weights <= Cutoff,
#                           Alpha1 + Beta1 * weights,
#                           Alpha2 + Beta2 * weights)
  
#   # Plot
#   graphics::plot(weights, energy_values, 
#                  type = "l", lwd = 2, col = colors$primary,
#                  xlab = "Weight (g)", ylab = "Energy Density (J/g)", 
#                  main = "Predator Energy Density vs Weight", las = 1)
  
#   # Add cutoff line
#   if (Cutoff > 0 && Cutoff < max(weights)) {
#     graphics::abline(v = Cutoff, col = colors$accent, lty = 2, lwd = 1)
#     graphics::text(Cutoff + 50, max(energy_values) * 0.9, 
#                    paste("Cutoff:", Cutoff, "g"), col = colors$accent)
#   }
  
#   add_plot_annotations(add_grid = TRUE)
# }

# #' Plot Predator vs Prey Energies
# #' @keywords internal
# plot_predator_vs_prey_energies <- function(bio_obj, colors) {
  
#   # Get prey energies
#   prey_energies <- bio_obj$diet_data$energies
#   prey_names <- bio_obj$diet_data$prey_names
  
#   if (is.null(prey_energies) || length(prey_names) == 0) {
#     graphics::plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
#                    main = "Energy Comparison", xlab = "", ylab = "", axes = FALSE)
#     graphics::text(0.5, 0.5, "No prey energy\ndata available", cex = 1.2, adj = 0.5)
#     return()
#   }
  
#   # Calculate mean prey energies
#   prey_matrix <- as.matrix(prey_energies[, prey_names, drop = FALSE])
#   mean_prey_energies <- colMeans(prey_matrix, na.rm = TRUE)
  
#   # Get predator energy (simplified - use mean or constant)
#   predator_params <- bio_obj$species_params$predator
#   if (!is.null(predator_params$ED_data) & !is.na(predator_params$ED_data)) {
#     mean_predator_energy <- mean(predator_params$ED_data)
#   } else if (!is.null(predator_params$ED_ini) && !is.null(predator_params$ED_end)) {
#     mean_predator_energy <- mean(c(predator_params$ED_ini, predator_params$ED_end))
#   } else {
#     mean_predator_energy <- 7000  # Default value
#   }
  
#   # Create comparison plot
#   all_energies <- c(mean_prey_energies, mean_predator_energy)
#   all_names <- c(prey_names, "Predator")
#   all_colors <- c(rainbow(length(prey_names)), colors$primary)
  
#   bp <- graphics::barplot(all_energies,
#                           names.arg = all_names,
#                           main = "Energy Density Comparison",
#                           ylab = "Energy Density (J/g)",
#                           col = all_colors,
#                           border = colors$primary,
#                           las = 2)  # Rotate labels
  
#   add_plot_annotations(add_grid = TRUE)
  
#   # Add mean line
#   mean_all <- mean(all_energies, na.rm = TRUE)
#   graphics::abline(h = mean_all, col = colors$accent, lty = 2, lwd = 1)
#   graphics::text(max(bp), mean_all + max(all_energies) * 0.05, 
#                  paste("Mean:", round(mean_all, 0)), col = colors$accent)
# }

# # ============================================================================
# # UTILITY FUNCTIONS
# # ============================================================================

# #' Get available plot types for Bioenergetic object
# #'
# #' @description
# #' Returns vector of plot types available for a given Bioenergetic object
# #' based on data availability.
# #'
# #' @param bio_obj Bioenergetic object
# #' @return Character vector of available plot types
# #' @export
# #'
# #' @examples
# #' \dontrun{
# #' available_plots <- get_available_bioenergetic_plot_types(bio_obj)
# #' print(available_plots)
# #' }
# get_available_bioenergetic_plot_types <- function(bio_obj) {
  
#   # Base plots always available
#   base_plots <- c("dashboard")
  
#   # Temperature plot
#   if (!is.null(bio_obj$environmental_data$temperature)) {
#     base_plots <- c(base_plots, "temperature")
#   }
  
#   # Diet plot
#   if (!is.null(bio_obj$diet_data$proportions)) {
#     base_plots <- c(base_plots, "diet")
#   }
  
#   # Energy plot
#   if (!is.null(bio_obj$species_params$predator)) {
#     base_plots <- c(base_plots, "energy")
#   }
  
#   return(sort(base_plots))
# }

# #' Create title for Bioenergetic plots
# #' @keywords internal
# create_bioenergetic_title <- function(bio_obj, title_override) {
  
#   if (!is.null(title_override)) {
#     return(title_override)
#   }
  
#   # Try to get species name
#   species_info <- bio_obj$species_info
#   species_name <- species_info$scientific_name %||% 
#                   species_info$common_name %||% 
#                   "Unknown Species"
  
#   # Create title
#   title <- paste("FB4 Model Setup:", species_name)
  
#   return(title)
# }

