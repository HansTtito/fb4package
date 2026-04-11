#' Bioenergetic Object Plots for Setup Validation
#'
#' @description
#' Plotting functions for \code{Bioenergetic} objects (before running a
#' simulation). These plots help validate model setup by displaying temperature
#' profiles (\code{plot_bio_temperature}), diet composition over time
#' (\code{plot_bio_diet}), predator energy density
#' (\code{plot_bio_energy}), and an integrated readiness dashboard
#' (\code{plot_bio_dashboard}).
#'
#' @references
#' Deslauriers, D., Chipps, S.R., Breck, J.E., Rice, J.A. and Madenjian, C.P.
#' (2017). Fish Bioenergetics 4.0: An R-based modeling application.
#' \emph{Fisheries}, 42(11), 586–596. \doi{10.1080/03632415.2017.1377558}
#'
#' @return No return value, called for side effects (plots). See individual function documentation for details.
#' @name fb4-bioenergetic-plots
#' @aliases fb4-bioenergetic-plots
#' @importFrom graphics plot lines abline text legend grid points barplot
#' @importFrom grDevices gray.colors
#' @importFrom utils tail
NULL

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
  
  ready <- check_bioenergetic_readiness(bio_obj)

  components <- list(
    "Parameters"    = ready$has_params,
    "Temperature"   = ready$has_temp,
    "Diet data"     = ready$has_diet,
    "Initial weight"= ready$has_initial
  )
  
  y_pos <- seq(0.8, 0.2, length.out = length(components))
  for (i in seq_along(components)) {
    comp_name <- names(components)[i]
    status <- components[[i]]
    symbol <- if (status) "[OK]" else "[X]"
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
         xlab = "Day", ylab = "\u00b0C", main = "Temperature Preview", las = 1)
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
  
  ready_count <- sum(ready$has_params, ready$has_temp, ready$has_diet, ready$has_initial)
  total_required <- 4
  
  if (ready_count == total_required) {
    text(0.5, 0.6, "[OK] READY", cex = 2.0, adj = 0.5, col = "darkgreen", font = 2)
    text(0.5, 0.3, "All components available", cex = 1.0, adj = 0.5)
  } else {
    text(0.5, 0.6, "[X] NOT READY", cex = 1.5, adj = 0.5, col = "red", font = 2)
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
       xlab = "Day", ylab = "Temperature (\u00b0C)", main = "Temperature Profile", las = 1)
  grid(col = "lightgray", lty = "dotted")
  
  mean_temp <- mean(temp_data$Temperature, na.rm = TRUE)
  abline(h = mean_temp, col = cols$accent, lty = 2)
  
  temp_range <- range(temp_data$Temperature, na.rm = TRUE)
  abline(h = temp_range, col = cols$secondary, lty = 3, lwd = 1)
  
  stats_text <- c(
    paste("Mean:", round(mean_temp, 1), "\u00b0C"),
    paste("Range:", round(diff(temp_range), 1), "\u00b0C"),
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
  for (i in seq_along(prey_names)) {
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
    days <- seq_len(duration)
    
    if (!is.null(predator_params$ED_data) && !all(is.na(predator_params$ED_data))) {
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

    legend("topright",
           legend = c(paste("Alpha1:", round(Alpha1, 2)),
                     paste("Beta1:", round(Beta1, 4)),
                     paste("Cutoff:", Cutoff, "g")),
           bty = "o", bg = "white", cex = 0.8)

  } else {
    stop("PREDEDEQ must be 1 or 2")
  }

  invisible(NULL)
}