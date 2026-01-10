#' Core Plotting Functions for FB4 Results
#'
#' @description
#' Core utilities and helper functions for FB4 visualization system.
#' Provides consistent styling, validation, and common plotting operations
#' shared across all plotting functions.
#'
#' @name fb4-plot-core
#' @importFrom graphics par plot hist lines abline text legend grid points polygon
#' @importFrom grDevices rgb col2rgb png pdf dev.off
#' @importFrom stats density
#' @importFrom tools file_ext
NULL

# ============================================================================
# COLOR SCHEMES AND VISUAL STYLING
# ============================================================================

#' Get Color Scheme for Plots
#'
#' @description
#' Returns standardized color schemes for consistent plot styling.
#'
#' @param scheme Color scheme name: "blue", "green", "red", "purple", or custom color
#' @return Named list of colors for different plot elements
#' @keywords internal
get_color_scheme <- function(scheme = "blue") {
  
  schemes <- list(
    blue = list(
      main = "lightblue", 
      primary = "darkblue", 
      secondary = "steelblue",
      accent = "red", 
      ci = "red",
      background = "lightgray"
    ),
    green = list(
      main = "lightgreen", 
      primary = "darkgreen", 
      secondary = "forestgreen",
      accent = "red", 
      ci = "red",
      background = "lightgray"
    ),
    red = list(
      main = "lightcoral", 
      primary = "darkred", 
      secondary = "brown",
      accent = "blue", 
      ci = "blue",
      background = "lightgray"
    ),
    purple = list(
      main = "plum", 
      primary = "purple", 
      secondary = "darkviolet",
      accent = "orange", 
      ci = "orange",
      background = "lightgray"
    )
  )
  
  # Return predefined scheme or create custom
  if (scheme %in% names(schemes)) {
    return(schemes[[scheme]])
  } else if (is.character(scheme) && length(scheme) == 1) {
    # Create scheme from single color
    return(list(
      main = scheme, 
      primary = scheme, 
      secondary = scheme,
      accent = "red", 
      ci = "red",
      background = "lightgray"
    ))
  } else {
    return(schemes$blue)  # Default fallback
  }
}

#' Setup Plot Layout
#'
#' @description
#' Configures plot layout and margins for consistent appearance.
#'
#' @param layout Layout specification: c(nrows, ncols) or "single"
#' @param margins Margin specification: "default", "compact", or numeric vector
#' @return Previous par() settings (for restoration)
#' @keywords internal
setup_plot_layout <- function(layout = "single", margins = "default") {
  
  # Store previous settings
  old_par <- graphics::par(no.readonly = TRUE)
  
  # Set layout
  if (identical(layout, "single")) {
    graphics::par(mfrow = c(1, 1))
  } else if (is.numeric(layout) && length(layout) == 2) {
    graphics::par(mfrow = layout)
  }
  
  # Set margins
  if (identical(margins, "default")) {
    graphics::par(mar = c(4, 4, 3, 2))
  } else if (identical(margins, "compact")) {
    graphics::par(mar = c(3, 3, 2, 1))
  } else if (is.numeric(margins) && length(margins) == 4) {
    graphics::par(mar = margins)
  }
  
  return(old_par)
}

# ============================================================================
# DEVICE MANAGEMENT
# ============================================================================

#' Setup graphics device for saving
#'
#' @description
#' Opens appropriate graphics device based on file extension.
#'
#' @param save_path Path to save file (.png or .pdf)
#' @return NULL
#' @keywords internal
setup_save_device <- function(save_path) {
  
  ext <- tolower(tools::file_ext(save_path))
  
  if (ext == "png") {
    grDevices::png(save_path, width = 12, height = 10, units = "in", res = 300)
  } else if (ext == "pdf") {
    grDevices::pdf(save_path, width = 12, height = 10)
  } else {
    warning("Unknown file extension '", ext, "'. Supported: .png, .pdf")
  }
}

#' Close graphics device after saving
#'
#' @description
#' Closes graphics device and shows confirmation message.
#'
#' @param save_path Path where file was saved
#' @return NULL
#' @keywords internal
close_save_device <- function(save_path) {
  
  ext <- tolower(tools::file_ext(save_path))
  
  if (ext %in% c("png", "pdf")) {
    grDevices::dev.off()
    message("Plot saved to: ", save_path)
  }
}

# ============================================================================
# PLOT ANNOTATIONS AND FORMATTING
# ============================================================================

#' Add Confidence Bands to Plot
#'
#' @description
#' Adds shaded confidence intervals to existing plots.
#'
#' @param x X-coordinates
#' @param lower Lower confidence bound
#' @param upper Upper confidence bound
#' @param color Color for shading, default "red"
#' @param alpha Transparency level (0-1), default 0.2
#' @return NULL (modifies current plot)
#' @keywords internal
add_confidence_bands <- function(x, lower, upper, color = "red", alpha = 0.2) {
  
  if (length(x) != length(lower) || length(x) != length(upper)) {
    warning("add_confidence_bands: x, lower, and upper must have same length")
    return(invisible())
  }
  
  # Remove non-finite values
  valid_idx <- is.finite(x) & is.finite(lower) & is.finite(upper)
  if (!any(valid_idx)) {
    warning("add_confidence_bands: no valid data points")
    return(invisible())
  }
  
  x_valid <- x[valid_idx]
  lower_valid <- lower[valid_idx]
  upper_valid <- upper[valid_idx]
  
  # Create polygon for shading
  x_poly <- c(x_valid, rev(x_valid))
  y_poly <- c(lower_valid, rev(upper_valid))
  
  # Add shaded area
  graphics::polygon(x_poly, y_poly, 
                    col = grDevices::rgb(grDevices::col2rgb(color)[1,1]/255, 
                                         grDevices::col2rgb(color)[2,1]/255, 
                                         grDevices::col2rgb(color)[3,1]/255, 
                                         alpha), 
                    border = NA)
}

#' Format Statistics Text for Plots
#'
#' @description
#' Creates formatted text blocks with key statistics for plot annotations.
#'
#' @param stats Named list or vector of statistics
#' @param digits Number of digits for rounding, default 3
#' @param prefix Optional prefix for each line
#' @return Character vector of formatted text lines
#' @keywords internal
format_statistics_text <- function(stats, digits = 3, prefix = "") {
  
  if (is.null(stats) || length(stats) == 0) {
    return(character(0))
  }
  
  # Handle different input types
  if (is.list(stats)) {
    stat_names <- names(stats)
    stat_values <- unlist(stats)
  } else {
    stat_names <- names(stats)
    stat_values <- as.numeric(stats)
  }
  
  # Format each statistic
  formatted_lines <- character(length(stat_values))
  for (i in seq_along(stat_values)) {
    name <- stat_names[i] %||% paste("Value", i)
    value <- stat_values[i]
    
    if (is.numeric(value) && is.finite(value)) {
      formatted_value <- round(value, digits)
    } else {
      formatted_value <- as.character(value)
    }
    
    formatted_lines[i] <- paste0(prefix, name, ": ", formatted_value)
  }
  
  return(formatted_lines)
}

#' Add Standard Plot Annotations
#'
#' @description
#' Adds common plot elements like grid, reference lines, and statistics text.
#'
#' @param add_grid Logical, add grid lines, default TRUE
#' @param reference_lines Numeric vector of reference line positions
#' @param stats_text Character vector of statistics to display
#' @param stats_position Position for statistics: "topright", "topleft", etc.
#' @return NULL (modifies current plot)
#' @keywords internal
add_plot_annotations <- function(add_grid = TRUE, 
                                 reference_lines = NULL,
                                 stats_text = NULL, 
                                 stats_position = "topright") {
  
  # Add grid
  if (add_grid) {
    graphics::grid(col = "lightgray", lty = "dotted")
  }
  
  # Add reference lines
  if (!is.null(reference_lines) && is.numeric(reference_lines)) {
    for (ref_line in reference_lines) {
      if (is.finite(ref_line)) {
        graphics::abline(h = ref_line, col = "red", lty = 2, lwd = 1)
      }
    }
  }
  
  # Add statistics text
  if (!is.null(stats_text) && length(stats_text) > 0) {
    stats_combined <- paste(stats_text, collapse = "\n")
    graphics::legend(stats_position, legend = stats_combined, 
                     bty = "o", bg = "white", cex = 0.8)
  }
}

# ============================================================================
# DATA VALIDATION AND EXTRACTION
# ============================================================================

#' Validate Plot Data Availability
#'
#' @description
#' Checks if required data columns are available for plotting.
#'
#' @param daily_data Daily output data frame
#' @param required_columns Character vector of required column names
#' @param plot_type Name of plot type for error messages
#' @return NULL (throws error if data not available)
#' @keywords internal
validate_plot_data <- function(daily_data, required_columns, plot_type = "plot") {
  
  if (is.null(daily_data)) {
    stop("No daily output data available for ", plot_type)
  }
  
  missing_columns <- setdiff(required_columns, names(daily_data))
  if (length(missing_columns) > 0) {
    stop("Missing required columns for ", plot_type, ": ", 
         paste(missing_columns, collapse = ", "), 
         ". Available columns: ", paste(names(daily_data), collapse = ", "))
  }
}

#' Extract Species Information for Titles
#'
#' @description
#' Extracts species name from fb4_result object for plot titles.
#'
#' @param fb4_result FB4 result object
#' @return Character string with species name or NULL if not available
#' @keywords internal
extract_species_name <- function(fb4_result) {
  
  if (is.null(fb4_result$bioenergetic_object)) {
    return(NULL)
  }
  
  species_info <- fb4_result$bioenergetic_object$species_info
  
  if (!is.null(species_info$scientific_name)) {
    return(species_info$scientific_name)
  } else if (!is.null(species_info$common_name)) {
    return(species_info$common_name)
  } else {
    return(NULL)
  }
}

#' Calculate Basic Growth Metrics
#'
#' @description
#' Calculates basic growth metrics from daily weight data.
#'
#' @param daily_data Daily output data frame with Weight column
#' @return List with growth metrics
#' @keywords internal
calculate_growth_metrics <- function(daily_data) {
  
  if (is.null(daily_data$Weight)) {
    return(NULL)
  }
  
  weights <- daily_data$Weight
  initial_weight <- weights[1]
  final_weight <- weights[length(weights)]
  
  # Calculate metrics
  total_growth <- final_weight - initial_weight
  growth_rate <- total_growth / length(weights)
  relative_growth <- (final_weight / initial_weight - 1) * 100
  
  return(list(
    initial_weight = initial_weight,
    final_weight = final_weight,
    total_growth = total_growth,
    growth_rate = growth_rate,
    relative_growth = relative_growth
  ))
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Get Available Plot Types
#'
#' @description
#' Returns list of available plot types for fb4_result objects.
#'
#' @param x fb4_result object
#' @return Character vector of available plot types
#' @keywords internal
get_available_plot_types <- function(x) {
  
  base_types <- c("dashboard", "growth", "consumption", "temperature", "energy")
  
  # Add uncertainty plots if available
  if (has_uncertainty(x)) {
    base_types <- c(base_types, "uncertainty")
  }
  
  # Add sensitivity plots if available
  if (!is.null(x$sensitivity_analysis)) {
    base_types <- c(base_types, "sensitivity")
  }
  
  return(base_types)
}

#' Check if result has uncertainty information
#'
#' @description
#' Checks if fb4_result contains uncertainty information.
#'
#' @param x fb4_result object
#' @return Logical indicating if uncertainty data is available
#' @keywords internal
has_uncertainty <- function(x) {
  
  has_bootstrap <- !is.null(x$bootstrap_results)
  has_mle <- !is.null(x$mle_results)
  has_hierarchical <- !is.null(x$hierarchical_results)
  
  return(has_bootstrap || has_mle || has_hierarchical)
}
