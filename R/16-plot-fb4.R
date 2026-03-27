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
    "uncertainty" = plot_uncertainty.fb4_result(x, ...),
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



