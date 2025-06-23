#' Extract Daily Data from fb4_result Object
#'
#' @description
#' Extracts daily simulation data from fb4_result objects with optional
#' variable filtering for focused analysis.
#'
#' @param x Object of class fb4_result
#' @param variables Vector of variable names to extract (NULL for all)
#'
#' @return Data frame with daily simulation results
#'
#' @details
#' Available daily variables typically include:
#' \itemize{
#'   \item{Day, Temperature.C: }{Basic temporal and environmental data}
#'   \item{Weight.g, Weight.Change.g: }{Growth metrics}
#'   \item{Consumption.g.g.d, Consumption.J.g.d: }{Feeding rates}
#'   \item{Respiration.g.O2.g.d, Respiration.J.g.d: }{Metabolic rates}
#'   \item{Egestion.J.g.d, Excretion.J.g.d, SDA.J.g.d: }{Energy losses}
#'   \item{Net.Energy.J.g.d: }{Available energy for growth}
#'   \item{P.Value: }{Daily proportion of maximum consumption}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- run_fb4(bio_obj, fit_to = "Weight", fit_value = 50)
#' 
#' # Get all daily data
#' daily_data <- get_daily_data(result)
#' 
#' # Get specific variables
#' growth_data <- get_daily_data(result, c("Weight.g", "Temperature.C"))
#' }
get_daily_data <- function(x, variables = NULL) {
  if (!inherits(x, "fb4_result")) {
    stop("x must be an object of class 'fb4_result'")
  }
  
  if (is.null(x$daily_output)) {
    stop("This object does not contain daily data. Run with output_daily = TRUE")
  }
  
  daily_data <- x$daily_output
  
  if (!is.null(variables)) {
    missing_vars <- setdiff(variables, names(daily_data))
    if (length(missing_vars) > 0) {
      warning("Variables not found: ", paste(missing_vars, collapse = ", "))
    }
    available_vars <- intersect(variables, names(daily_data))
    if (length(available_vars) == 0) {
      stop("None of the specified variables are available")
    }
    daily_data <- daily_data[, c("Day", available_vars), drop = FALSE]
  }
  
  return(daily_data)
}

#' Extract Summary from fb4_result Object
#'
#' @description
#' Extracts summary statistics from fb4_result objects with additional
#' metadata for comprehensive result interpretation.
#'
#' @param x Object of class fb4_result
#'
#' @return List with summary results and metadata
#'
#' @details
#' The summary includes:
#' \itemize{
#'   \item{Growth metrics: }{Final weight, total growth, growth rates}
#'   \item{Simulation metadata: }{Species, methods, execution details}
#'   \item{Model performance: }{Fit success, convergence information}
#'   \item{Temporal scope: }{Simulation period and duration}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- run_fb4(bio_obj, fit_to = "Weight", fit_value = 50)
#' summary_info <- get_summary(result)
#' print(summary_info$final_weight)
#' }
get_summary <- function(x) {
  if (!inherits(x, "fb4_result")) {
    stop("x must be an object of class 'fb4_result'")
  }
  
  summary_data <- x$summary
  
  # Add additional information
  summary_data$simulation_period <- paste(
    x$parameters_used$initial_conditions$first_day, "-",
    x$parameters_used$initial_conditions$last_day
  )
  summary_data$species <- x$parameters_used$species
  summary_data$fit_method <- x$parameters_used$initial_conditions$fit_to
  summary_data$fit_successful <- x$model_info$fit_successful
  summary_data$execution_time <- x$model_info$execution_time
  
  return(summary_data)
}

#' Extract Energy Balance from fb4_result Object
#'
#' @description
#' Extracts detailed energy balance information from fb4_result objects
#' for energetic analysis and model validation.
#'
#' @param x Object of class fb4_result
#'
#' @return List with energy balance components
#'
#' @details
#' Energy balance components include:
#' \itemize{
#'   \item{Consumption: }{Total energy intake}
#'   \item{Losses: }{Respiration, egestion, excretion, SDA}
#'   \item{Net energy: }{Available for growth and reproduction}
#'   \item{Efficiency: }{Conversion and assimilation rates}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- run_fb4(bio_obj, fit_to = "Weight", fit_value = 50)
#' energy_balance <- get_energy_balance(result)
#' print(energy_balance$efficiency)
#' }
get_energy_balance <- function(x) {
  if (!inherits(x, "fb4_result")) {
    stop("x must be an object of class 'fb4_result'")
  }
  
  if (is.null(x$energy_balance)) {
    stop("This object does not contain energy balance information")
  }
  
  return(x$energy_balance)
}

#' Get Fitting Information
#'
#' @description
#' Extracts detailed information about the fitting process including
#' convergence statistics and final parameter values.
#'
#' @param x Object of class fb4_result
#'
#' @return List with fitting process information
#'
#' @details
#' Fitting information includes:
#' \itemize{
#'   \item{Method details: }{Fitting approach and target values}
#'   \item{Convergence: }{Success status and iteration count}
#'   \item{Final parameters: }{Achieved p-values and error metrics}
#'   \item{Performance: }{Execution time and numerical stability}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- run_fb4(bio_obj, fit_to = "Weight", fit_value = 50)
#' fit_info <- get_fit_info(result)
#' print(fit_info$successful)
#' }
get_fit_info <- function(x) {
  if (!inherits(x, "fb4_result")) {
    stop("x must be an object of class 'fb4_result'")
  }
  
  fit_info <- list(
    method = x$parameters_used$initial_conditions$fit_to,
    target_value = x$parameters_used$initial_conditions$fit_value,
    successful = x$model_info$fit_successful,
    p_value = x$model_info$p_value,
    execution_time = x$model_info$execution_time
  )
  
  # Add method-specific information if available
  if (!is.null(x$fit_info)) {
    fit_info <- modifyList(fit_info, x$fit_info)
  }
  
  return(fit_info)
}

#' Compare Multiple fb4_result Objects
#'
#' @description
#' Creates a comparative analysis of multiple fb4_result objects for
#' scenario evaluation and sensitivity analysis.
#'
#' @param ... fb4_result objects or list of fb4_result objects
#' @param names Vector of names for the objects (optional)
#'
#' @return Data frame with comparative results
#'
#' @details
#' Comparison includes key metrics across simulations:
#' \itemize{
#'   \item{Growth outcomes: }{Final weights and growth rates}
#'   \item{Model parameters: }{P-values and fitting targets}
#'   \item{Performance: }{Execution times and convergence}
#'   \item{Methods: }{Fitting approaches and species}
#' }
#'
#' Useful for parameter sensitivity analysis, method comparison,
#' and scenario evaluation studies.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Compare different temperature scenarios
#' result1 <- run_fb4(bio_obj1, fit_to = "Weight", fit_value = 50)
#' result2 <- run_fb4(bio_obj2, fit_to = "Weight", fit_value = 50)
#' 
#' comparison <- compare_results(result1, result2, names = c("Warm", "Cold"))
#' print(comparison)
#' }
compare_results <- function(..., names = NULL) {
  # Get objects
  objects <- list(...)
  if (length(objects) == 1 && is.list(objects[[1]]) && !inherits(objects[[1]], "fb4_result")) {
    objects <- objects[[1]]  # If a list was passed
  }
  
  # Validate that all are fb4_result
  are_fb4 <- sapply(objects, function(x) inherits(x, "fb4_result"))
  if (!all(are_fb4)) {
    stop("All objects must be of class 'fb4_result'")
  }
  
  # Names for objects
  if (is.null(names)) {
    names <- paste0("Result_", seq_along(objects))
  } else if (length(names) != length(objects)) {
    stop("Names vector must have same length as number of objects")
  }
  
  # Extract information from each object
  comparison_data <- data.frame(
    Name = names,
    Species = character(length(objects)),
    Method = character(length(objects)),
    Target_Value = numeric(length(objects)),
    Initial_Weight = numeric(length(objects)),
    Final_Weight = numeric(length(objects)),
    Total_Growth = numeric(length(objects)),
    P_Value = numeric(length(objects)),
    Fit_Successful = logical(length(objects)),
    Execution_Time = numeric(length(objects)),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(objects)) {
    obj <- objects[[i]]
    
    comparison_data$Species[i] <- obj$parameters_used$species %||% NA
    comparison_data$Method[i] <- obj$parameters_used$initial_conditions$fit_to %||% NA
    comparison_data$Target_Value[i] <- obj$parameters_used$initial_conditions$fit_value %||% NA
    comparison_data$Initial_Weight[i] <- obj$parameters_used$initial_conditions$initial_weight %||% NA
    comparison_data$Final_Weight[i] <- obj$summary$final_weight %||% NA
    comparison_data$Total_Growth[i] <- obj$summary$total_growth %||% NA
    comparison_data$P_Value[i] <- obj$model_info$p_value %||% NA
    comparison_data$Fit_Successful[i] <- obj$model_info$fit_successful %||% FALSE
    comparison_data$Execution_Time[i] <- obj$model_info$execution_time %||% NA
  }
  
  class(comparison_data) <- c("fb4_comparison", "data.frame")
  return(comparison_data)
}

#' Print fb4 Results Comparison
#'
#' @description
#' Prints a formatted comparison table of multiple fb4_result objects
#' with summary statistics.
#'
#' @param x Object of class fb4_comparison
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @details
#' Displays a tabular comparison with key metrics and provides
#' summary statistics including success rates and performance metrics.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' comparison <- compare_results(result1, result2, result3)
#' print(comparison)
#' }
print.fb4_comparison <- function(x, ...) {
  cat("=== FB4 Results Comparison ===\n\n")
  cat("Number of simulations:", nrow(x), "\n\n")
  
  # Show summary table
  display_cols <- c("Name", "Method", "Target_Value", "Final_Weight", 
                    "Total_Growth", "Fit_Successful")
  display_data <- x[, display_cols, drop = FALSE]
  
  # Round numeric values
  numeric_cols <- sapply(display_data, is.numeric)
  display_data[numeric_cols] <- lapply(display_data[numeric_cols], round, 2)
  
  print(display_data, row.names = FALSE)
  
  cat("\n")
  successful_runs <- sum(x$Fit_Successful)
  cat("Successful fits:", successful_runs, "of", nrow(x), "\n")
  
  if (successful_runs > 0) {
    successful_data <- x[x$Fit_Successful, ]
    cat("Final weight - Min:", round(min(successful_data$Final_Weight, na.rm = TRUE), 2),
        "| Max:", round(max(successful_data$Final_Weight, na.rm = TRUE), 2), "g\n")
    cat("Average execution time:", round(mean(x$Execution_Time, na.rm = TRUE), 2), "seconds\n")
  }
  
  invisible(x)
}

#' Check if Fit was Successful
#'
#' @description
#' Tests whether a bioenergetic model fit converged successfully.
#'
#' @param x fb4_result object
#'
#' @return Logical indicating fit success
#'
#' @details
#' Returns TRUE if the fitting algorithm converged within tolerance
#' and iteration limits, FALSE otherwise. Essential for validating
#' results before interpretation.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- run_fb4(bio_obj, fit_to = "Weight", fit_value = 50)
#' if (is_fit_successful(result)) {
#'   print("Model fit successfully!")
#' }
#' }
is_fit_successful <- function(x) {
  if (!inherits(x, "fb4_result")) {
    stop("x must be an object of class 'fb4_result'")
  }
  
  return(x$model_info$fit_successful %||% FALSE)
}

#' Get Quick Growth Statistics
#'
#' @description
#' Calculates comprehensive growth statistics from fb4_result objects
#' for rapid assessment of model outcomes.
#'
#' @param x fb4_result object
#'
#' @return List with growth statistics
#'
#' @details
#' Provides both summary and detailed growth metrics:
#' 
#' \strong{Summary metrics (always available):}
#' \itemize{
#'   \item{Total and relative growth}
#'   \item{Initial and final weights}
#' }
#'
#' \strong{Detailed metrics (if daily data available):}
#' \itemize{
#'   \item{Daily growth statistics (max, min, mean)}
#'   \item{Growth pattern analysis (positive/negative days)}
#'   \item{Growth variability measures}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- run_fb4(bio_obj, fit_to = "Weight", fit_value = 50)
#' growth_stats <- growth_stats(result)
#' print(growth_stats$relative_growth)
#' }
growth_stats <- function(x) {
  if (!inherits(x, "fb4_result")) {
    stop("x must be an object of class 'fb4_result'")
  }
  
  if (is.null(x$daily_output)) {
    # Summary data only
    return(list(
      initial_weight = x$summary$initial_weight %||% NA,
      final_weight = x$summary$final_weight %||% NA,
      total_growth = x$summary$total_growth %||% NA,
      relative_growth = (x$summary$final_weight / x$summary$initial_weight - 1) * 100
    ))
  }
  
  # Calculate detailed statistics from daily data
  weights <- x$daily_output$Weight.g
  daily_growth <- c(0, diff(weights))
  
  return(list(
    initial_weight = weights[1],
    final_weight = weights[length(weights)],
    total_growth = weights[length(weights)] - weights[1],
    relative_growth = (weights[length(weights)] / weights[1] - 1) * 100,
    max_daily_growth = max(daily_growth, na.rm = TRUE),
    min_daily_growth = min(daily_growth, na.rm = TRUE),
    mean_daily_growth = mean(daily_growth, na.rm = TRUE),
    growth_days = sum(daily_growth > 0, na.rm = TRUE),
    loss_days = sum(daily_growth < 0, na.rm = TRUE)
  ))
}

#' Validate fb4_result Object
#'
#' @description
#' Validates the structure and completeness of fb4_result objects
#' to ensure reliable downstream analysis.
#'
#' @param x Object to validate
#'
#' @return Logical indicating whether object is a valid fb4_result
#'
#' @details
#' Validates essential components including parameter storage,
#' model information, and result structure. Provides warnings
#' for missing optional components that might affect analysis.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- run_fb4(bio_obj, fit_to = "Weight", fit_value = 50)
#' if (is.fb4_result(result)) {
#'   print("Valid fb4_result object")
#' }
#' }
is.fb4_result <- function(x) {
  if (!inherits(x, "fb4_result")) return(FALSE)
  
  required_components <- c("parameters_used", "model_info")
  missing <- setdiff(required_components, names(x))
  
  if (length(missing) > 0) {
    warning("fb4_result missing components: ", paste(missing, collapse = ", "))
    return(FALSE)
  }
  
  return(TRUE)
}

#' Export Results to Different Formats
#'
#' @description
#' Exports fb4_result objects to various file formats for external
#' analysis, reporting, and data sharing.
#'
#' @param x fb4_result object
#' @param format Export format: "csv", "xlsx", "json"
#' @param file File name (without extension)
#' @param include_daily Include daily data in export (logical)
#'
#' @return Invisibly returns TRUE on successful export
#'
#' @details
#' Export formats and their characteristics:
#' 
#' \strong{CSV format:}
#' \itemize{
#'   \item{Exports daily data only (most commonly needed)}
#'   \item{Compatible with all statistical software}
#'   \item{Efficient for large datasets}
#' }
#'
#' \strong{Excel format:}
#' \itemize{
#'   \item{Multiple sheets: Summary, Daily Data, Energy Balance}
#'   \item{Formatted for easy viewing and analysis}
#'   \item{Requires openxlsx package}
#' }
#'
#' \strong{JSON format:}
#' \itemize{
#'   \item{Complete object structure preserved}
#'   \item{Machine-readable for web applications}
#'   \item{Requires jsonlite package}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- run_fb4(bio_obj, fit_to = "Weight", fit_value = 50)
#' 
#' # Export to CSV
#' export_results(result, "csv", "salmon_growth")
#' 
#' # Export to Excel with all data
#' export_results(result, "xlsx", "complete_results", include_daily = TRUE)
#' }
export_results <- function(x, format = "csv", file = "fb4_results", include_daily = TRUE) {
  if (!inherits(x, "fb4_result")) {
    stop("x must be an object of class 'fb4_result'")
  }
  
  format <- match.arg(format, c("csv", "xlsx", "json"))
  
  # Prepare export data
  export_data <- list()
  
  # Summary
  export_data$summary <- get_summary(x)
  
  # Daily data if available and requested
  if (include_daily && !is.null(x$daily_output)) {
    export_data$daily_data <- x$daily_output
  }
  
  # Energy balance if available
  if (!is.null(x$energy_balance)) {
    export_data$energy_balance <- x$energy_balance
  }
  
  # Fit information
  export_data$fit_info <- get_fit_info(x)
  
  # Export by format
  if (format == "csv") {
    # CSV for daily data only (most important)
    if (!is.null(export_data$daily_data)) {
      write.csv(export_data$daily_data, paste0(file, ".csv"), row.names = FALSE)
      message("Daily data exported to ", file, ".csv")
    } else {
      stop("No daily data available for CSV export")
    }
    
  } else if (format == "xlsx") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' is required for Excel export")
    }
    
    wb <- openxlsx::createWorkbook()
    
    # Summary sheet
    openxlsx::addWorksheet(wb, "Summary")
    summary_df <- data.frame(
      Parameter = names(export_data$summary),
      Value = unlist(export_data$summary),
      stringsAsFactors = FALSE
    )
    openxlsx::writeData(wb, "Summary", summary_df)
    
    # Daily data sheet
    if (!is.null(export_data$daily_data)) {
      openxlsx::addWorksheet(wb, "Daily_Data")
      openxlsx::writeData(wb, "Daily_Data", export_data$daily_data)
    }
    
    openxlsx::saveWorkbook(wb, paste0(file, ".xlsx"), overwrite = TRUE)
    message("Results exported to ", file, ".xlsx")
    
  } else if (format == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package 'jsonlite' is required for JSON export")
    }
    
    jsonlite::write_json(export_data, paste0(file, ".json"), pretty = TRUE)
    message("Results exported to ", file, ".json")
  }
  
  invisible(TRUE)
}#' S3 Classes for FB4 Bioenergetic Model
#'
#' @description
#' S3 class system for the Fish Bioenergetics 4.0 model, providing structured
#' data containers and methods for bioenergetic simulations and results.
#'
#' @name bioenergetic-classes
#' @aliases bioenergetic-classes
NULL

# ============================================================================
# MAIN CLASS: Bioenergetic
# ============================================================================

#' Constructor for Bioenergetic Objects
#'
#' @description
#' Creates a Bioenergetic class object that encapsulates all components
#' of the fish bioenergetic model for streamlined simulation management.
#'
#' @param species_params List with species parameters organized by categories
#' @param environmental_data List with environmental data (temperature, etc.)
#' @param diet_data List with diet and prey energy data
#' @param model_options List with model configuration options
#' @param simulation_settings List with simulation configuration
#'
#' @return Object of class "Bioenergetic"
#'
#' @details
#' The Bioenergetic object serves as a comprehensive container for all
#' bioenergetic model components:
#'
#' \strong{Required Components:}
#' \itemize{
#'   \item{species_params: }{Parameter sets for consumption, respiration, etc.}
#' }
#'
#' \strong{Optional Components:}
#' \itemize{
#'   \item{environmental_data: }{Temperature and other environmental variables}
#'   \item{diet_data: }{Diet composition and prey energy densities}
#'   \item{model_options: }{Sub-model toggles and advanced settings}
#'   \item{simulation_settings: }{Initial conditions and duration}
#' }
#'
#' The object maintains state information including fit status and results,
#' allowing for iterative model development and parameter refinement.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create species parameters
#' params <- list(
#'   consumption = list(CA = 0.303, CB = -0.275, CQ = 3, CTO = 15, CTM = 25, CTL = 28),
#'   respiration = list(RA = 0.0548, RB = -0.299, RQ = 2, RTO = 5, RTM = 25, RTL = 35)
#' )
#'
#' # Create bioenergetic object
#' bio_obj <- Bioenergetic(
#'   species_params = params,
#'   simulation_settings = list(initial_weight = 10, duration = 365)
#' )
#' }
#'
#' @seealso \code{\link{new_bioenergetic}}, \code{\link{run_fb4.Bioenergetic}}
Bioenergetic <- function(species_params,
                         environmental_data = NULL,
                         diet_data = NULL,
                         model_options = list(),
                         simulation_settings = list()) {
  stopifnot(!is.null(species_params))
  
  structure(
    list(
      species_params = species_params,
      environmental_data = environmental_data,
      diet_data = diet_data,
      model_options = model_options,
      simulation_settings = simulation_settings,
      results = NULL,
      fitted = FALSE
    ),
    class = c("Bioenergetic", "list")
  )
}

# ============================================================================
# SIMPLIFIED CONSTRUCTOR
# ============================================================================

#' Simplified Constructor for Bioenergetic Models
#'
#' @description
#' Creates a Bioenergetic object with sensible defaults and automatic
#' data generation for quick model setup and prototyping.
#'
#' @param species Scientific name or species index
#' @param initial_weight Initial weight in grams
#' @param life_stage Life stage identifier
#' @param duration Simulation duration in days
#' @param temperature_avg Average temperature in °C
#' @param temperature_variation Temperature variation in °C
#' @param diet_composition Diet composition as named list
#' @param ... Additional arguments
#'
#' @return Bioenergetic object with generated data
#'
#' @details
#' This function provides a user-friendly interface for creating bioenergetic
#' models with minimal input requirements. It automatically:
#'
#' \strong{Temperature Generation:}
#' \itemize{
#'   \item{Creates sinusoidal temperature patterns}
#'   \item{Simulates seasonal variation}
#'   \item{Uses biologically realistic ranges}
#' }
#'
#' \strong{Diet Processing:}
#' \itemize{
#'   \item{Normalizes diet proportions to sum to 1.0}
#'   \item{Assigns typical energy densities for common prey types}
#'   \item{Supports custom prey categories}
#' }
#'
#' \strong{Parameter Selection:}
#' \itemize{
#'   \item{Retrieves species-specific parameters from database}
#'   \item{Selects appropriate life stage parameters}
#'   \item{Validates parameter completeness}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple salmon model
#' salmon_model <- new_bioenergetic(
#'   species = "Oncorhynchus tshawytscha",
#'   initial_weight = 15,
#'   duration = 365,
#'   temperature_avg = 12,
#'   diet_composition = list(Fish = 0.7, Zooplankton = 0.3)
#' )
#'
#' # Run simulation
#' result <- run_fb4(salmon_model, fit_to = "Weight", fit_value = 100)
#' }
#'
#' @seealso \code{\link{Bioenergetic}}, \code{\link{list_species}}, \code{\link{species_info}}
new_bioenergetic <- function(species,
                             initial_weight,
                             life_stage = NULL,
                             duration = 365,
                             temperature_avg = 20,
                             temperature_variation = 5,
                             diet_composition = list(Zooplankton = 1.0),
                             ...) {
  if (!exists("fish4_parameters")) {
    data("fish4_parameters", package = "fb4package", envir = environment())
  }
  
  # Select species
  species_data <- if (is.character(species)) {
    match <- fish4_parameters[[species]]
    if (is.null(match)) {
      stop("Species not found in fish4_parameters")
    }
    match
  } else if (is.numeric(species) && species >= 1 && species <= length(fish4_parameters)) {
    fish4_parameters[[species]]
  } else {
    stop("Species must be valid name or index")
  }
  
  # Select life stage
  stages <- names(species_data$life_stages)
  if (is.null(life_stage)) life_stage <- stages[1]
  if (!life_stage %in% stages) stop("Invalid life stage")
  params <- species_data$life_stages[[life_stage]]
  params$species_info <- species_data$species_info
  
  # Generate temperature data
  days <- 1:duration
  temperature <- temperature_avg + temperature_variation * sin(2 * pi * days / 365)
  env <- list(
    temperature = data.frame(Day = days, Temperature = temperature),
    days = days,
    duration = duration
  )
  
  # Process diet
  diet_names <- names(diet_composition)
  diet_props <- as.numeric(diet_composition)
  diet_props <- diet_props / sum(diet_props)
  
  typical_energies <- list(
    Zooplankton = 3500, Invertebrates = 4200, Fish = 5500,
    Detritus = 2000, Benthos = 4000, Insects = 4500,
    Phytoplankton = 2500, Crustaceans = 4000, Mollusks = 3800
  )
  
  diet_df <- energy_df <- data.frame(Day = days)
  for (i in seq_along(diet_names)) {
    diet_df[[diet_names[i]]] <- diet_props[i]
    energy_df[[diet_names[i]]] <- typical_energies[[diet_names[i]]] %||% 3500
  }
  
  sim <- list(initial_weight = initial_weight, duration = duration)
  opts <- list(calc_mortality = FALSE, calc_reproduction = FALSE, 
               calc_contaminant = FALSE, calc_nutrient = FALSE, output_daily = TRUE)
  
  Bioenergetic(species_params = params, 
               environmental_data = env, 
               diet_data = list(proportions = diet_df, energies = energy_df, prey_names = diet_names),
               model_options =  opts, 
               simulation_settings = sim)
}

# ============================================================================
# S3 METHODS FOR BIOENERGETIC CLASS
# ============================================================================

#' Print Method for Bioenergetic Objects
#'
#' @description
#' Provides a concise summary of a Bioenergetic object's contents and status.
#'
#' @param x Bioenergetic object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @details
#' Displays key information including species identification, simulation
#' settings, fit status, and enabled sub-models. Provides a quick overview
#' for model verification and debugging.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' bio_obj <- new_bioenergetic("Salmo salar", 10, duration = 200)
#' print(bio_obj)  # or simply: bio_obj
#' }
print.Bioenergetic <- function(x, ...) {
  cat("=== FB4 Bioenergetic Model ===\n\n")
  info <- x$species_params$species_info
  if (!is.null(info)) {
    cat("Species:", info$scientific_name %||% "", "\n")
    cat("Common name:", info$common_name %||% "", "\n")
    cat("Family:", info$family %||% "", "\n")
  }
  cat("Initial weight:", x$simulation_settings$initial_weight, "g\n")
  cat("Duration:", x$simulation_settings$duration, "days\n")
  cat("Status:", if (x$fitted) "Fitted" else "Not fitted", "\n")
  mods <- names(Filter(isTRUE, x$model_options))
  if (length(mods) > 0) cat("Enabled sub-models:", paste(mods, collapse = ", "), "\n")
  invisible(x)
}

#' Summary Method for Bioenergetic Objects
#'
#' @description
#' Provides detailed information about a Bioenergetic object including
#' parameters, environmental conditions, and diet composition.
#'
#' @param object Bioenergetic object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @details
#' Extends the print method with additional technical details useful
#' for model validation and parameter verification.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' bio_obj <- new_bioenergetic("Salmo salar", 10)
#' summary(bio_obj)
#' }
summary.Bioenergetic <- function(object, ...) {
  print(object)
  cat("\n=== Parameters ===\n")
  cat("Consumption equation:", get_parameter_value(object$species_params, "CEQ") %||% "NA", "\n")
  cat("Respiration equation:", get_parameter_value(object$species_params, "REQ") %||% "NA", "\n")
  cat("\n=== Temperature ===\n")
  temps <- object$environmental_data$temperature$Temperature
  cat("Min:", min(temps), "| Max:", max(temps), "| Mean:", mean(temps), "°C\n")
  cat("\n=== Diet ===\n")
  cat("Prey items:", paste(object$diet_data$prey_names, collapse = ", "), "\n")
  invisible(object)
}

# ============================================================================
# CONFIGURATION METHODS
# ============================================================================

#' Set Environmental Data
#'
#' @description
#' Generic function for setting environmental data in model objects.
#'
#' @param x Model object
#' @param temperature_data Temperature data frame
#'
#' @return Updated model object
#'
#' @export
set_environment <- function(x, temperature_data) UseMethod("set_environment")

#' Set Environmental Data for Bioenergetic Objects
#'
#' @description
#' Updates the environmental data component of a Bioenergetic object
#' with new temperature information.
#'
#' @param x Bioenergetic object
#' @param temperature_data Data frame with Day and Temperature columns
#'
#' @return Updated Bioenergetic object with new environmental data
#'
#' @details
#' The temperature data frame must contain:
#' \itemize{
#'   \item{Day: }{Numeric sequence of simulation days}
#'   \item{Temperature: }{Temperature values in °C}
#' }
#'
#' Setting new environmental data resets the fitted status and clears
#' any existing results, as the model needs to be re-run.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' bio_obj <- new_bioenergetic("Salmo salar", 10)
#' temp_data <- data.frame(Day = 1:100, Temperature = 15 + 5*sin(1:100/50))
#' bio_obj <- set_environment(bio_obj, temp_data)
#' }
set_environment.Bioenergetic <- function(x, temperature_data) {
  stopifnot(is.data.frame(temperature_data), all(c("Day", "Temperature") %in% names(temperature_data)))
  x$environmental_data$temperature <- temperature_data
  x$environmental_data$duration <- max(temperature_data$Day)
  x$fitted <- FALSE; x$results <- NULL
  x
}

#' Set Diet Data
#'
#' @description
#' Generic function for setting diet data in model objects.
#'
#' @param x Model object
#' @param diet_proportions Diet composition data frame
#' @param prey_energies Prey energy density data frame
#'
#' @return Updated model object
#'
#' @export
set_diet <- function(x, diet_proportions, prey_energies) UseMethod("set_diet")

#' Set Diet Data for Bioenergetic Objects
#'
#' @description
#' Updates the diet data component of a Bioenergetic object with
#' new diet composition and prey energy information.
#'
#' @param x Bioenergetic object
#' @param diet_proportions Data frame with daily diet proportions
#' @param prey_energies Data frame with daily prey energy densities
#'
#' @return Updated Bioenergetic object with new diet data
#'
#' @details
#' Both data frames must:
#' \itemize{
#'   \item{Contain a "Day" column with matching values}
#'   \item{Have identical prey species columns}
#'   \item{Use consistent prey naming between datasets}
#' }
#'
#' Diet proportions should sum to approximately 1.0 for each day.
#' Prey energies should be positive values in J/g.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' bio_obj <- new_bioenergetic("Salmo salar", 10)
#' diet_props <- data.frame(Day = 1:365, fish = 0.6, zooplankton = 0.4)
#' prey_energy <- data.frame(Day = 1:365, fish = 5000, zooplankton = 3000)
#' bio_obj <- set_diet(bio_obj, diet_props, prey_energy)
#' }
set_diet.Bioenergetic <- function(x, diet_proportions, prey_energies, indigestible_prey = NULL) {
  stopifnot(all(c("Day") %in% names(diet_proportions)),
            all(c("Day") %in% names(prey_energies)))
  
  prey_cols <- setdiff(names(diet_proportions), "Day")
  stopifnot(identical(sort(prey_cols), sort(setdiff(names(prey_energies), "Day"))))
  
  # Validar indigestible_prey si se proporciona
  if (!is.null(indigestible_prey)) {
    stopifnot(all(c("Day") %in% names(indigestible_prey)))
    indigestible_cols <- setdiff(names(indigestible_prey), "Day")
    stopifnot(identical(sort(prey_cols), sort(indigestible_cols)))
  } else {
    # Crear datos por defecto si no se proporcionan
    indigestible_prey <- diet_proportions
    indigestible_prey[prey_cols] <- 0  # Default: 0% indigerible
  }
  
  x$diet_data <- list(
    proportions = diet_proportions, 
    energies = prey_energies, 
    indigestible = indigestible_prey,  # ¡NUEVO!
    prey_names = prey_cols
  )
  
  x$fitted <- FALSE
  x$results <- NULL
  x
}
# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Test if Object is Bioenergetic
#'
#' @description
#' Tests whether an object inherits from the Bioenergetic class.
#'
#' @param x Object to test
#'
#' @return Logical indicating whether object is of class Bioenergetic
#'
#' @export
#'
#' @examples
#' \dontrun{
#' bio_obj <- new_bioenergetic("Salmo salar", 10)
#' is.Bioenergetic(bio_obj)  # TRUE
#' is.Bioenergetic(list())   # FALSE
#' }
is.Bioenergetic <- function(x) inherits(x, "Bioenergetic")

#' Get Results from Fitted Model
#'
#' @description
#' Generic function for extracting results from fitted bioenergetic models.
#'
#' @param x Fitted model object
#' @param component Component to extract (default "all")
#'
#' @return Model results or specified component
#'
#' @export
get_results <- function(x, component = "all") UseMethod("get_results")

#' Get Results from Bioenergetic Objects
#'
#' @description
#' Extracts simulation results from a fitted Bioenergetic object.
#'
#' @param x Fitted Bioenergetic object
#' @param component Component to extract: "all", "daily", "summary", etc.
#'
#' @return Complete results or specified component
#'
#' @details
#' The object must have been fitted using \code{\link{run_fb4}} before
#' results can be extracted. Available components depend on simulation
#' options and may include daily output, summary statistics, and
#' energy balance information.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' bio_obj <- new_bioenergetic("Salmo salar", 10)
#' result <- run_fb4(bio_obj, fit_to = "Weight", fit_value = 50)
#' 
#' # Extract all results
#' all_results <- get_results(bio_obj)
#' 
#' # Extract specific component
#' daily_data <- get_results(bio_obj, "daily")
#' }
get_results.Bioenergetic <- function(x, component = "all") {
  stopifnot(x$fitted)
  if (component == "all") return(x$results)
  if (!component %in% names(x$results)) stop("Component not found")
  x$results[[component]]
}

#' List Available Species
#'
#' @description
#' Lists available species in the fish bioenergetics parameter database,
#' with optional filtering by taxonomic family.
#'
#' @param fish4_db Fish bioenergetics database (optional)
#' @param family Taxonomic family filter (optional)
#'
#' @return Character vector of available species names
#'
#' @details
#' If no database is provided, the function searches for the default
#' fish4_parameters database in the environment or loads it from file.
#' Family filtering allows users to focus on related species with
#' similar bioenergetic characteristics.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # List all species
#' all_species <- list_species()
#' 
#' # List only salmon species
#' salmon_species <- list_species(family = "Salmonidae")
#' }
#'
#' @seealso \code{\link{species_info}}, \code{\link{new_bioenergetic}}
list_species <- function(fish4_db = NULL, family = NULL) {
  if (is.null(fish4_db)) {
    if (exists("fish4_parameters")) fish4_db <- fish4_parameters
    else if (file.exists("fish4_parameters.RData")) {
      load("fish4_parameters.RData"); fish4_db <- fish4_parameters
    } else stop("Database not found")
  }
  sp <- names(fish4_db)
  if (!is.null(family)) {
    fams <- sapply(fish4_db, function(x) x$species_info$family %||% NA)
    sp <- sp[fams == family & !is.na(fams)]
  }
  sp
}

#' Get Species Information
#'
#' @description
#' Retrieves detailed information about a specific species from the
#' fish bioenergetics parameter database.
#'
#' @param species Species name (scientific name)
#' @param fish4_db Fish bioenergetics database (optional)
#'
#' @return List with species information including available life stages
#'
#' @details
#' Returns comprehensive species information including:
#' \itemize{
#'   \item{Taxonomic information (family, common names)}
#'   \item{Available life stages and their parameters}
#'   \item{Literature sources and references}
#'   \item{Parameter derivation methods}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get information about Atlantic salmon
#' salmon_info <- species_info("Salmo salar")
#' print(salmon_info$available_life_stages)
#' }
#'
#' @seealso \code{\link{list_species}}, \code{\link{new_bioenergetic}}
species_info <- function(species, fish4_db = NULL) {
  if (is.null(fish4_db)) {
    if (exists("fish4_parameters")) fish4_db <- fish4_parameters
    else if (file.exists("fish4_parameters.RData")) {
      load("fish4_parameters.RData"); fish4_db <- fish4_parameters
    } else stop("Database not found")
  }
  stopifnot(species %in% names(fish4_db))
  sp <- fish4_db[[species]]
  list(
    scientific_name = species,
    species_info = sp$species_info,
    available_life_stages = names(sp$life_stages),
    sources = sp$sources
  )
}

#' Get Parameter Value
#'
#' @description
#' Retrieves a specific parameter value from species parameter lists,
#' searching across all parameter categories.
#'
#' @param params Species parameters list
#' @param param Parameter name to retrieve
#'
#' @return Parameter value or NULL if not found
#'
#' @details
#' Searches through all parameter categories (consumption, respiration,
#' predator, etc.) to find the specified parameter. This utility function
#' simplifies parameter access when the category is unknown.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get a consumption parameter
#' ca_value <- get_parameter_value(species_params, "CA")
#' 
#' # Get a respiration parameter
#' ra_value <- get_parameter_value(species_params, "RA")
#' }
get_parameter_value <- function(params, param) {
  for (cat in names(params)) {
    if (param %in% names(params[[cat]])) return(params[[cat]][[param]])
  }
  NULL
}

#' Set Parameter Value
#'
#' @description
#' Sets a specific parameter value in species parameter lists,
#' automatically finding the correct category.
#'
#' @param params Species parameters list
#' @param param Parameter name to set
#' @param value New parameter value
#'
#' @return Updated parameters list
#'
#' @details
#' Modifies parameter values in place, maintaining the original
#' structure of the parameters list. Throws an error if the
#' parameter is not found in any category.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Modify a consumption parameter
#' new_params <- set_parameter_value(params, "CA", 0.35)
#' 
#' # Modify temperature optimum
#' new_params <- set_parameter_value(params, "CTO", 18)
#' }
set_parameter_value <- function(params, param, value) {
  for (cat in names(params)) {
    if (param %in% names(params[[cat]])) {
      params[[cat]][[param]] <- value
      return(params)
    }
  }
  stop("Parameter '", param, "' not found in any category.")
}

# ============================================================================
# S3 METHODS FOR fb4_result CLASS
# ============================================================================

#' Print fb4_result Object
#'
#' @description
#' Provides a comprehensive summary of Fish Bioenergetics 4.0 simulation results.
#'
#' @param x Object of class fb4_result
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @details
#' Displays key simulation information including species, fitting method,
#' growth results, and model performance metrics. Provides a quick overview
#' of simulation success and main outcomes.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' bio_obj <- new_bioenergetic("Salmo salar", 10)
#' result <- run_fb4(bio_obj, fit_to = "Weight", fit_value = 50)
#' print(result)  # or simply: result
#' }
print.fb4_result <- function(x, ...) {
  cat("=== Fish Bioenergetics 4.0 - Results ===\n\n")
  
  # Basic information
  info <- x$parameters_used$initial_conditions
  cat("Species:", x$parameters_used$species, "\n")
  cat("Fitting method:", info$fit_to, "\n")
  cat("Target value:", info$fit_value, "\n")
  cat("Initial weight:", info$initial_weight, "g\n")
  
  # Main results
  if (!is.null(x$summary)) {
    summary_data <- x$summary
    cat("Final weight:", round(summary_data$final_weight %||% NA, 2), "g\n")
    cat("Total growth:", round(summary_data$total_growth %||% NA, 2), "g\n")
    if (!is.null(summary_data$total_consumption)) {
      cat("Total consumption:", round(summary_data$total_consumption, 2), "g\n")
    }
  }
  
  # Fit status
  model_info <- x$model_info
  cat("Fit successful:", if(model_info$fit_successful) "Yes" else "No", "\n")
  if (!is.null(model_info$p_value)) {
    cat("Final p-value:", round(model_info$p_value, 4), "\n")
  }
  cat("Execution time:", round(model_info$execution_time, 2), "seconds\n")
  
  # Data information
  cat("Simulation period:", info$first_day, "-", info$last_day, "(", 
      info$last_day - info$first_day + 1, "days )\n")
  
  # Daily data availability
  daily_available <- !is.null(x$daily_output)
  cat("Daily data available:", if(daily_available) "Yes" else "No", "\n")
  
  if (daily_available) {
    cat("Daily variables:", ncol(x$daily_output), "columns\n")
  }
  
  invisible(x)
}

#' Detailed Summary of fb4_result Object
#'
#' @description
#' Provides an extended summary including energy balance details
#' and environmental conditions.
#'
#' @param object Object of class fb4_result
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @details
#' Extends the print method with detailed energy balance information,
#' environmental statistics, and model configuration details.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- run_fb4(bio_obj, fit_to = "Weight", fit_value = 50)
#' summary(result)
#' }
summary.fb4_result <- function(object, ...) {
  print(object)
  
  cat("\n=== Energy Summary ===\n")
  
  # Energy balance
  if (!is.null(object$energy_balance)) {
    eb <- object$energy_balance
    cat("Energy consumed:", round(eb$total_consumption_energy %||% NA, 0), "J\n")
    cat("Energy respired:", round(eb$total_respiration_energy %||% NA, 0), "J\n")
    cat("Energy egested:", round(eb$total_egestion_energy %||% NA, 0), "J\n")
    cat("Energy excreted:", round(eb$total_excretion_energy %||% NA, 0), "J\n")
    if (!is.null(eb$efficiency)) {
      cat("Conversion efficiency:", round(eb$efficiency * 100, 1), "%\n")
    }
  }
  
  # Temperature statistics if daily data available
  if (!is.null(object$daily_output)) {
    cat("\n=== Environmental Conditions ===\n")
    temps <- object$daily_output$Temperature.C
    cat("Temperature - Min:", round(min(temps, na.rm = TRUE), 1), 
        "| Mean:", round(mean(temps, na.rm = TRUE), 1),
        "| Max:", round(max(temps, na.rm = TRUE), 1), "°C\n")
  }
  
  # Model information
  cat("\n=== Model Configuration ===\n")
  model_opts <- object$parameters_used$model_options
  enabled_opts <- names(Filter(isTRUE, model_opts))
  if (length(enabled_opts) > 0) {
    cat("Enabled sub-models:", paste(enabled_opts, collapse = ", "), "\n")
  } else {
    cat("Enabled sub-models: None\n")
  }
  
  cat("FB4 Version:", object$model_info$version, "\n")
  cat("Executed:", format(object$model_info$timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
  
  invisible(object)
}