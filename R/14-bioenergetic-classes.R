#' S3 Classes for FB4 Bioenergetic Model
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
#' Performs basic validation to ensure object integrity.
#'
#' @param species_params List with species parameters organized by categories
#' @param species_info List with species identification information
#' @param environmental_data List with environmental data (temperature, etc.)
#' @param diet_data List with diet and prey energy data
#' @param reproduction_data List with reproduction parameters (optional)
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
#'   \item{species_info: }{Species identification with scientific_name or common_name}
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
#' \strong{Species Information:}
#' The species_info list should contain at minimum one of:
#' \itemize{
#'   \item{scientific_name: }{Scientific species name (e.g., "Salmo salar")}
#'   \item{common_name: }{Common species name (e.g., "Atlantic salmon")}
#' }
#' Optional fields include life_stage, source, notes, etc.
#'
#' The constructor performs basic validation to ensure object integrity.
#' Complete validation is performed when running simulations.
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
#' # Create species info
#' species_info <- list(
#'   scientific_name = "Salmo salar",
#'   common_name = "Atlantic salmon",
#'   life_stage = "juvenile"
#' )
#'
#' # Create bioenergetic object
#' bio_obj <- Bioenergetic(
#'   species_params = params,
#'   species_info = species_info,
#'   simulation_settings = list(initial_weight = 10, duration = 365)
#' )
#' }
#'
#' @seealso \code{\link{new_bioenergetic}}, \code{\link{run_fb4.Bioenergetic}}
Bioenergetic <- function(species_params,
                         species_info = NULL,
                         environmental_data = NULL,
                         diet_data = NULL,
                         reproduction_data = NULL,
                         model_options = list(),
                         simulation_settings = list()) {
  
  # Basic validation - species_params
  if (is.null(species_params) || !is.list(species_params)) {
    stop("species_params must be a non-null list")
  }
  
  # Basic validation - species_info
  if (is.null(species_info)) {
    warning("species_info is NULL. Consider providing species identification.")
    species_info <- list()
  } else if (!is.list(species_info)) {
    stop("species_info must be a list")
  } else {
    # Check for at least one identification field
    has_scientific <- !is.null(species_info$scientific_name) && 
      nzchar(as.character(species_info$scientific_name))
    has_common <- !is.null(species_info$common_name) && 
      nzchar(as.character(species_info$common_name))
    
    if (!has_scientific && !has_common) {
      warning("species_info should contain at least 'scientific_name' or 'common_name'")
    }
  }
  
  # Basic validation - optional components
  if (!is.null(environmental_data) && !is.list(environmental_data)) {
    stop("environmental_data must be a list")
  }
  
  if (!is.null(diet_data) && !is.list(diet_data)) {
    stop("diet_data must be a list")
  }
  
  if (!is.null(reproduction_data) && !is.list(reproduction_data)) {
    stop("reproduction_data must be a list")
  }
  
  if (!is.list(model_options)) {
    stop("model_options must be a list")
  }
  
  if (!is.list(simulation_settings)) {
    stop("simulation_settings must be a list")
  }
  
  # Basic validation - species_params structure
  expected_param_categories <- c("consumption", "respiration", "egestion", 
                                 "excretion", "predator")
  present_categories <- intersect(names(species_params), expected_param_categories)
  
  if (length(present_categories) == 0) {
    warning("species_params doesn't contain recognized parameter categories. ",
            "Expected at least one of: ", paste(expected_param_categories, collapse = ", "))
  }
  
  # Validate that present categories are lists
  for (category in present_categories) {
    if (!is.list(species_params[[category]])) {
      stop("species_params$", category, " must be a list")
    }
  }
  
  # Set essential defaults for model_options
  if (is.null(model_options$output_daily)) {
    model_options$output_daily <- TRUE
  }
  if (is.null(model_options$calc_mortality)) {
    model_options$calc_mortality <- FALSE
  }
  if (is.null(model_options$calc_reproduction)) {
    model_options$calc_reproduction <- FALSE
  }
  if (is.null(model_options$detailed_output)) {
    model_options$detailed_output <- FALSE
  }
  
  # Create and return object
  bio_obj <- structure(
    list(
      species_info = species_info,
      species_params = species_params,
      environmental_data = environmental_data,
      diet_data = diet_data,
      reproduction_data = reproduction_data,
      model_options = model_options,
      simulation_settings = simulation_settings,
      fitted = FALSE
    ),
    class = c("Bioenergetic", "list")
  )
  
  # Success message
  species_name <- species_info$scientific_name %||% 
    species_info$common_name %||% 
    "Unknown species"
  
  message("Bioenergetic object created for: ", species_name)
  
  return(bio_obj)
}


# ============================================================================
# S3 METHODS FOR BIOENERGETIC CLASS
# ============================================================================

#' Print Method for Bioenergetic Objects
#'
#' @description
#' Provides a concise overview of a Bioenergetic object's status and completeness.
#'
#' @param x Bioenergetic object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @details
#' Shows essential information for quick verification including species
#' identification, basic simulation settings, and component availability.
#'
#' @export
print.Bioenergetic <- function(x, ...) {
  cat("=== FB4 Bioenergetic Model ===\n\n")
  
  # Species information
  info <- x$species_info
  if (!is.null(info)) {
    species_name <- info$scientific_name %||% info$common_name %||% "Unknown species"
    common_part <- if (!is.null(info$common_name) && !is.null(info$scientific_name)) {
      paste0(" (", info$common_name, ")")
    } else ""
    cat("Species:", species_name, common_part, "\n")
  } else {
    cat("Species: Not specified\n")
  }
  
  # Basic simulation info
  initial_weight <- x$simulation_settings$initial_weight %||% "Not set"
  duration <- x$simulation_settings$duration %||% "Not set"
  cat("Setup:", initial_weight, "g →", duration, "days\n")
  
  # Component status
  has_params <- !is.null(x$species_params) && length(x$species_params) > 0
  has_temp <- !is.null(x$environmental_data$temperature)
  has_diet <- !is.null(x$diet_data$proportions)
  has_sim <- !is.null(x$simulation_settings$initial_weight)
  
  components_ready <- sum(has_params, has_temp, has_diet, has_sim)
  cat("Components:", components_ready, "/4", 
      if (components_ready == 4) " Ready" else " Incomplete", "\n")
  
  # Fit status
  cat("Status:", if (x$fitted) "Fitted" else "Not fitted", "\n")
  
  invisible(x)
}

#' Summary Method for Bioenergetic Objects
#'
#' @description
#' Provides detailed information about all components of a Bioenergetic object
#' including parameter values, data ranges, and validation status.
#'
#' @param object Bioenergetic object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @details
#' Shows comprehensive information for model verification and debugging,
#' including statistical summaries of environmental and diet data.
#'
#' @export
summary.Bioenergetic <- function(object, ...) {
  cat("=== FB4 Bioenergetic Model Summary ===\n\n")
  
  # Species Information
  cat("SPECIES INFORMATION:\n")
  info <- object$species_info
  if (!is.null(info) && length(info) > 0) {
    for (field in names(info)) {
      if (!is.null(info[[field]]) && nzchar(as.character(info[[field]]))) {
        cat("  ", tools::toTitleCase(gsub("_", " ", field)), ":", info[[field]], "\n")
      }
    }
  } else {
    cat("  No species information provided\n")
  }
  
  # Species Parameters
  cat("\nSPECIES PARAMETERS:\n")
  if (!is.null(object$species_params)) {
    param_categories <- c("consumption", "respiration", "egestion", "excretion", "predator")
    present_categories <- intersect(names(object$species_params), param_categories)
    
    if (length(present_categories) > 0) {
      cat("  Categories:", paste(present_categories, collapse = ", "), "\n")
      
      # Show key parameters for each category
      for (cat_name in present_categories) {
        params <- object$species_params[[cat_name]]
        if (is.list(params) && length(params) > 0) {
          key_params <- head(names(params), 3)  # Show first 3 parameters
          param_values <- sapply(key_params, function(p) params[[p]])
          cat("  ", tools::toTitleCase(cat_name), ":", 
              paste(paste(key_params, param_values, sep = "="), collapse = ", "))
          if (length(params) > 3) cat(" ...")
          cat("\n")
        }
      }
    } else {
      cat("  No recognized parameter categories found\n")
    }
  } else {
    cat("  No parameters provided\n")
  }
  
  # Environmental Data
  cat("\nENVIRONMENTAL DATA:\n")
  if (!is.null(object$environmental_data$temperature)) {
    temp_data <- object$environmental_data$temperature
    temp_range <- range(temp_data$Temperature, na.rm = TRUE)
    temp_mean <- mean(temp_data$Temperature, na.rm = TRUE)
    day_range <- range(temp_data$Day, na.rm = TRUE)
    
    cat("  Temperature: ", round(temp_mean, 1), "°C (range: ", 
        round(temp_range[1], 1), "-", round(temp_range[2], 1), "°C)\n", sep = "")
    cat("  Duration:", nrow(temp_data), "days (", day_range[1], "-", day_range[2], ")\n")
  } else {
    cat("  No temperature data provided\n")
  }
  
  # Diet Data
  cat("\nDIET DATA:\n")
  if (!is.null(object$diet_data)) {
    if (!is.null(object$diet_data$proportions)) {
      diet_props <- object$diet_data$proportions
      prey_cols <- setdiff(names(diet_props), "Day")
      
      if (length(prey_cols) > 0) {
        cat("  Prey species:", length(prey_cols), "(", paste(prey_cols, collapse = ", "), ")\n")
        
        # Show proportion ranges
        for (prey in prey_cols) {
          prop_range <- range(diet_props[[prey]], na.rm = TRUE)
          prop_mean <- mean(diet_props[[prey]], na.rm = TRUE)
          cat("    ", prey, ": ", round(prop_mean * 100, 1), "% (", 
              round(prop_range[1] * 100, 1), "-", round(prop_range[2] * 100, 1), "%)\n", sep = "")
        }
      }
      
      cat("  Days covered:", nrow(diet_props), "\n")
    }
    
    if (!is.null(object$diet_data$energies)) {
      energy_data <- object$diet_data$energies
      prey_cols <- setdiff(names(energy_data), "Day")
      
      if (length(prey_cols) > 0) {
        energy_ranges <- sapply(prey_cols, function(p) {
          range(energy_data[[p]], na.rm = TRUE)
        })
        cat("  Energy range:", round(min(energy_ranges)), "-", 
            round(max(energy_ranges)), "J/g\n")
      }
    }
  } else {
    cat("  No diet data provided\n")
  }
  
  # Simulation Settings
  cat("\nSIMULATION SETTINGS:\n")
  if (!is.null(object$simulation_settings)) {
    settings <- object$simulation_settings
    for (setting in names(settings)) {
      if (!is.null(settings[[setting]])) {
        cat("  ", tools::toTitleCase(gsub("_", " ", setting)), ":", settings[[setting]])
        if (setting == "initial_weight") cat(" g")
        if (setting == "duration") cat(" days")
        cat("\n")
      }
    }
  } else {
    cat("  No simulation settings provided\n")
  }
  
  # Model Options
  if (!is.null(object$model_options) && length(object$model_options) > 0) {
    cat("\nMODEL OPTIONS:\n")
    enabled_options <- names(Filter(isTRUE, object$model_options))
    disabled_options <- names(Filter(function(x) !isTRUE(x), object$model_options))
    
    if (length(enabled_options) > 0) {
      cat("  Enabled:", paste(enabled_options, collapse = ", "), "\n")
    }
    if (length(disabled_options) > 0) {
      cat("  Disabled:", paste(disabled_options, collapse = ", "), "\n")
    }
  }
  
  # Overall Status
  cat("\nSTATUS:\n")
  cat("  Fitted:", if (object$fitted) "Yes" else "No", "\n")
  if (object$fitted && !is.null(object$results)) {
    cat("  Results available: Yes\n")
  }
  
  invisible(object)
}

#' Structure Method for Bioenergetic Objects
#'
#' @description
#' Shows the complete internal structure of a Bioenergetic object.
#'
#' @param object Bioenergetic object
#' @param ... Additional arguments passed to str()
#'
#' @return Invisibly returns the input object
#'
#' @details
#' Provides technical view of the object structure, useful for debugging
#' and advanced users who need to understand the internal organization.
#'
#' @export
str.Bioenergetic <- function(object, ...) {
  cat("=== FB4 Bioenergetic Object Structure ===\n\n")
  
  # Remove class temporarily to avoid recursion
  obj_copy <- object
  class(obj_copy) <- "list"
  
  # Use base str() function
  str(obj_copy, ...)
  
  invisible(object)
}

# ============================================================================
# CONFIGURATION METHODS
# ============================================================================

#' Set Environmental Data for Bioenergetic Objects
#'
#' @description
#' Updates the environmental data component of a Bioenergetic object
#' with new temperature information. Automatically validates and completes
#' the temperature series if needed.
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
#' The function automatically:
#' \itemize{
#'   \item{Validates temperature data structure and values}
#'   \item{Interpolates missing days to create complete daily series}
#'   \item{Reports any interpolation performed}
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
#' 
#' # With sparse data - will be automatically interpolated
#' sparse_temp <- data.frame(Day = c(1, 50, 100), Temperature = c(10, 15, 20))
#' bio_obj <- set_environment(bio_obj, sparse_temp)
#' }
set_environment.Bioenergetic <- function(x, temperature_data) {
  
  # Basic validation
  stopifnot(is.data.frame(temperature_data), 
            all(c("Day", "Temperature") %in% names(temperature_data)))
  
  # Validate temperature data using validation function
  validate_time_series_data(temperature_data, "temperature_data", 
                            c("Day", "Temperature"))
  
  # Check if interpolation is needed
  day_range <- range(temperature_data$Day)
  expected_days <- day_range[1]:day_range[2]
  existing_days <- sort(unique(temperature_data$Day))
  
  # If missing days, interpolate
  if (length(existing_days) != length(expected_days)) {
    missing_count <- length(expected_days) - length(existing_days)
    message("Interpolating temperature for ", missing_count, " missing days")
    
    temperature_data <- interpolate_time_series(
      data = temperature_data,
      value_columns = "Temperature",
      target_days = expected_days,
      method = "linear"
    )
  }
  
  # Set environmental data
  x$environmental_data$temperature <- temperature_data
  x$environmental_data$duration <- max(temperature_data$Day)
  
  # Reset model state
  x$fitted <- FALSE
  x$results <- NULL
  
  return(x)
}


#' Set Diet Data for Bioenergetic Objects
#'
#' @description
#' Updates the diet data component of a Bioenergetic object with
#' new diet composition and prey energy information. Automatically validates,
#' interpolates missing days, and normalizes diet proportions.
#'
#' @param x Bioenergetic object
#' @param diet_proportions Data frame with daily diet proportions
#' @param prey_energies Data frame with daily prey energy densities
#' @param indigestible_prey Data frame with indigestible proportions (optional)
#' @param normalize_diet Logical, whether to normalize diet proportions to sum to 1 (default TRUE)
#'
#' @return Updated Bioenergetic object with new diet data
#'
#' @details
#' Both data frames must:
#' \itemize{
#'   \item{Contain a "Day" column}
#'   \item{Have identical prey species columns}
#'   \item{Use consistent prey naming between datasets}
#' }
#'
#' The function automatically:
#' \itemize{
#'   \item{Validates diet and energy data structure and values}
#'   \item{Interpolates missing days to create complete daily series}
#'   \item{Normalizes diet proportions to sum to 1.0 each day (if normalize_diet = TRUE)}
#'   \item{Creates default indigestible data with zero percent if not provided}
#'   \item{Reports any processing performed}
#' }
#'
#' Diet proportions should be non-negative values.
#' Prey energies should be positive values in J/g.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' bio_obj <- new_bioenergetic("Salmo salar", 10)
#' 
#' # Complete daily data
#' diet_props <- data.frame(Day = 1:365, fish = 0.6, zooplankton = 0.4)
#' prey_energy <- data.frame(Day = 1:365, fish = 5000, zooplankton = 3000)
#' bio_obj <- set_diet(bio_obj, diet_props, prey_energy)
#' 
#' # Sparse data - will be automatically interpolated and normalized
#' sparse_diet <- data.frame(Day = c(1, 100, 200), fish = c(0.5, 0.8, 0.3), 
#'                          zooplankton = c(0.4, 0.15, 0.6))
#' sparse_energy <- data.frame(Day = c(1, 100, 200), fish = c(5000, 5200, 4800), 
#'                            zooplankton = c(3000, 3100, 2900))
#' bio_obj <- set_diet(bio_obj, sparse_diet, sparse_energy)
#' }
set_diet.Bioenergetic <- function(x, diet_proportions, prey_energies, 
                                  indigestible_prey = NULL, normalize_diet = TRUE) {
  
  # Basic validation
  stopifnot(all(c("Day") %in% names(diet_proportions)),
            all(c("Day") %in% names(prey_energies)))
  
  # Validate individual datasets
  validate_time_series_data(diet_proportions, "diet_proportions", 
                            c("Day"))
  validate_time_series_data(prey_energies, "prey_energies", 
                            c("Day"))
  
  # Cross-validation between datasets
  validate_diet_consistency(diet_proportions, prey_energies, check_temporal = TRUE)
  
  # Get prey columns
  prey_cols <- setdiff(names(diet_proportions), "Day")
  
  # Determine complete day range
  all_days <- sort(unique(c(diet_proportions$Day, prey_energies$Day)))
  day_range <- range(all_days)
  target_days <- day_range[1]:day_range[2]
  
  # Interpolate diet proportions if needed
  if (nrow(diet_proportions) != length(target_days)) {
    missing_count <- length(target_days) - nrow(diet_proportions)
    message("Interpolating diet proportions for ", missing_count, " missing days")
    
    diet_proportions <- interpolate_time_series(
      data = diet_proportions,
      value_columns = prey_cols,
      target_days = target_days,
      method = "linear"
    )
  }
  
  # Interpolate prey energies if needed
  if (nrow(prey_energies) != length(target_days)) {
    missing_count <- length(target_days) - nrow(prey_energies)
    message("Interpolating prey energies for ", missing_count, " missing days")
    
    prey_energies <- interpolate_time_series(
      data = prey_energies,
      value_columns = prey_cols,
      target_days = target_days,
      method = "linear"
    )
  }
  
  # Normalize diet proportions to sum to 1
  if (normalize_diet) {
    row_sums <- rowSums(diet_proportions[prey_cols], na.rm = TRUE)
    
    # Check if normalization is needed
    needs_normalization <- any(abs(row_sums - 1) > 0.01, na.rm = TRUE)
    
    if (needs_normalization) {
      message("Normalizing diet proportions to sum to 1.0")
      
      # Normalize each row
      for (i in 1:nrow(diet_proportions)) {
        if (row_sums[i] > 0) {
          diet_proportions[i, prey_cols] <- diet_proportions[i, prey_cols] / row_sums[i]
        }
      }
    }
  }
  
  # Handle indigestible prey data
  if (!is.null(indigestible_prey)) {
    # Validate indigestible data
    stopifnot(all(c("Day") %in% names(indigestible_prey)))
    indigestible_cols <- setdiff(names(indigestible_prey), "Day")
    stopifnot(identical(sort(prey_cols), sort(indigestible_cols)))
    
    # Interpolate indigestible data if needed
    indigestible_range <- range(indigestible_prey$Day)
    if (indigestible_range[1] > target_days[1] || indigestible_range[2] < tail(target_days, 1)) {
      message("Interpolating indigestible prey data for complete series")
      
      indigestible_prey <- interpolate_time_series(
        data = indigestible_prey,
        value_columns = prey_cols,
        target_days = target_days,
        method = "linear"
      )
    }
  } else {
    # Create default indigestible data (0% indigestible)
    message("Creating default indigestible prey data (0% for all prey)")
    indigestible_prey <- diet_proportions
    indigestible_prey[prey_cols] <- 0
  }
  
  # Set diet data
  x$diet_data <- list(
    proportions = diet_proportions, 
    energies = prey_energies, 
    indigestible = indigestible_prey,
    prey_names = prey_cols
  )
  
  # Reset model state
  x$fitted <- FALSE
  x$results <- NULL
  
  return(x)
}


#' Set Simulation Settings for Bioenergetic Objects
#'
#' @description
#' Updates the simulation configuration of a Bioenergetic object with
#' initial weight and duration settings. Automatically detects duration
#' from existing data when not specified.
#'
#' @param x Bioenergetic object
#' @param initial_weight Initial weight in grams
#' @param duration Simulation duration in days (auto-detected if NULL)
#'
#' @return Updated Bioenergetic object with new simulation settings
#'
#' @details
#' The function follows a lenient input approach:
#' \itemize{
#'   \item{Accepts partial settings without complete validation}
#'   \item{Auto-detects duration from environmental or diet data when available}
#'   \item{Provides warnings for conflicts but allows overrides}
#'   \item{Defers complete validation to simulation execution}
#' }
#'
#' Duration precedence (highest to lowest):
#' \itemize{
#'   \item{Explicitly provided duration parameter}
#'   \item{Maximum day from environmental data}
#'   \item{Maximum day from diet data}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' bio_obj <- Bioenergetic(species_params = params)
#' 
#' # Set both parameters
#' bio_obj <- set_simulation_settings(bio_obj, initial_weight = 15, duration = 365)
#' 
#' # Auto-detect duration from existing data
#' bio_obj <- set_simulation_settings(bio_obj, initial_weight = 15)
#' 
#' # Update only one parameter
#' bio_obj <- set_simulation_settings(bio_obj, duration = 200)
#' }
set_simulation_settings.Bioenergetic <- function(x, initial_weight = NULL, duration = NULL) {
  
  # Initialize simulation_settings if NULL
  if (is.null(x$simulation_settings)) {
    x$simulation_settings <- list()
  }
  
  # Handle initial_weight
  if (!is.null(initial_weight)) {
    # Basic validation
    if (!is.numeric(initial_weight) || length(initial_weight) != 1 || initial_weight <= 0) {
      stop("initial_weight must be a single positive number")
    }
    
    # Check for conflicts
    if (!is.null(x$simulation_settings$initial_weight) && 
        x$simulation_settings$initial_weight != initial_weight) {
      warning("Overriding existing initial_weight: ", 
              x$simulation_settings$initial_weight, "g -> ", initial_weight, "g")
    }
    
    x$simulation_settings$initial_weight <- initial_weight
  }
  
  # Handle duration
  if (!is.null(duration)) {
    # Basic validation
    if (!is.numeric(duration) || length(duration) != 1 || duration <= 0 || duration != round(duration)) {
      stop("duration must be a single positive integer (days)")
    }
    
    # Check for conflicts
    if (!is.null(x$simulation_settings$duration) && 
        x$simulation_settings$duration != duration) {
      warning("Overriding existing duration: ", 
              x$simulation_settings$duration, " -> ", duration, " days")
    }
    
    x$simulation_settings$duration <- duration
    
  } else if (is.null(x$simulation_settings$duration)) {
    # Auto-detect duration from existing data
    detected_duration <- NULL
    
    # Check environmental data
    if (!is.null(x$environmental_data$temperature)) {
      env_max <- max(x$environmental_data$temperature$Day, na.rm = TRUE)
      detected_duration <- max(detected_duration %||% 0, env_max)
    }
    
    # Check diet data
    if (!is.null(x$diet_data$proportions)) {
      diet_max <- max(x$diet_data$proportions$Day, na.rm = TRUE)
      detected_duration <- max(detected_duration %||% 0, diet_max)
    }
    
    if (!is.null(detected_duration) && detected_duration > 0) {
      message("Auto-detected simulation duration: ", detected_duration, " days")
      x$simulation_settings$duration <- detected_duration
    }
  }
  
  # Validate consistency with existing data (warnings only)
  if (!is.null(x$simulation_settings$duration)) {
    current_duration <- x$simulation_settings$duration
    
    # Check against environmental data
    if (!is.null(x$environmental_data$temperature)) {
      env_max <- max(x$environmental_data$temperature$Day, na.rm = TRUE)
      if (current_duration > env_max) {
        warning("Simulation duration (", current_duration, " days) exceeds environmental data range (", 
                env_max, " days). Consider extending environmental data.")
      }
    }
    
    # Check against diet data
    if (!is.null(x$diet_data$proportions)) {
      diet_max <- max(x$diet_data$proportions$Day, na.rm = TRUE)
      if (current_duration > diet_max) {
        warning("Simulation duration (", current_duration, " days) exceeds diet data range (", 
                diet_max, " days). Consider extending diet data.")
      }
    }
  }
  
  # Reset model state
  x$fitted <- FALSE
  x$results <- NULL
  
  return(x)
}


#' Set Model Options for Bioenergetic Objects
#'
#' @description
#' Updates the model configuration options of a Bioenergetic object,
#' controlling which sub-models are enabled and output settings.
#'
#' @param x Bioenergetic object
#' @param calc_mortality Logical, calculate mortality rates (default FALSE)
#' @param calc_reproduction Logical, calculate reproduction costs (default FALSE)
#' @param output_daily Logical, save daily outputs vs final only (default TRUE)
#' @param detailed_output Logical, include intermediate calculations (default FALSE)
#' @param ... Additional model options for future extensions
#'
#' @return Updated Bioenergetic object with new model options
#'
#' @details
#' Model options control the complexity and output detail of simulations:
#'
#' \strong{Sub-model toggles:}
#' \itemize{
#'   \item{calc_mortality: Enables mortality rate calculations}
#'   \item{calc_reproduction: Enables reproductive cost calculations}
#' }
#'
#' \strong{Output control:}
#' \itemize{
#'   \item{output_daily: Save results for each day vs final values only}
#'   \item{detailed_output: Include intermediate values (consumption, respiration, etc.)}
#' }
#'
#' The function follows a lenient approach, allowing partial updates
#' and preserving existing options not specified in the call.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' bio_obj <- Bioenergetic(species_params = params)
#' 
#' # Enable all options
#' bio_obj <- set_model_options(bio_obj, 
#'   calc_mortality = TRUE,
#'   calc_reproduction = TRUE,
#'   detailed_output = TRUE
#' )
#' 
#' # Update only specific options
#' bio_obj <- set_model_options(bio_obj, output_daily = FALSE)
#' 
#' # Disable sub-models
#' bio_obj <- set_model_options(bio_obj, 
#'   calc_mortality = FALSE,
#'   calc_reproduction = FALSE
#' )
#' }
set_model_options.Bioenergetic <- function(x, 
                                           calc_mortality = NULL,
                                           calc_reproduction = NULL,
                                           output_daily = NULL,
                                           detailed_output = NULL,
                                           ...) {
  
  # Initialize model_options if NULL
  if (is.null(x$model_options)) {
    x$model_options <- list()
  }
  
  # Helper function to update option with validation
  update_option <- function(option_name, new_value, current_options) {
    if (!is.null(new_value)) {
      # Validate logical input
      if (!is.logical(new_value) || length(new_value) != 1) {
        stop(option_name, " must be a single logical value (TRUE/FALSE)")
      }
      
      # Check for conflicts and warn
      if (!is.null(current_options[[option_name]]) && 
          current_options[[option_name]] != new_value) {
        old_value <- if (current_options[[option_name]]) "TRUE" else "FALSE"
        new_value_str <- if (new_value) "TRUE" else "FALSE"
        warning("Overriding ", option_name, ": ", old_value, " -> ", new_value_str)
      }
      
      current_options[[option_name]] <- new_value
    }
    return(current_options)
  }
  
  # Update each option
  x$model_options <- update_option("calc_mortality", calc_mortality, x$model_options)
  x$model_options <- update_option("calc_reproduction", calc_reproduction, x$model_options)
  x$model_options <- update_option("output_daily", output_daily, x$model_options)
  x$model_options <- update_option("detailed_output", detailed_output, x$model_options)
  
  # Handle additional options from ...
  additional_options <- list(...)
  if (length(additional_options) > 0) {
    for (option_name in names(additional_options)) {
      option_value <- additional_options[[option_name]]
      
      # Basic validation for additional options
      if (!is.logical(option_value) || length(option_value) != 1) {
        warning("Additional option '", option_name, "' should be a single logical value. Skipping.")
        next
      }
      
      # Check for conflicts
      if (!is.null(x$model_options[[option_name]]) && 
          x$model_options[[option_name]] != option_value) {
        old_value <- if (x$model_options[[option_name]]) "TRUE" else "FALSE"
        new_value_str <- if (option_value) "TRUE" else "FALSE"
        warning("Overriding ", option_name, ": ", old_value, " -> ", new_value_str)
      }
      
      x$model_options[[option_name]] <- option_value
    }
  }
  
  # Set defaults for unspecified core options
  core_defaults <- list(
    calc_mortality = FALSE,
    calc_reproduction = FALSE,
    output_daily = TRUE,
    detailed_output = FALSE
  )
  
  for (option_name in names(core_defaults)) {
    if (is.null(x$model_options[[option_name]])) {
      x$model_options[[option_name]] <- core_defaults[[option_name]]
    }
  }
  
  # Validate option combinations (warnings only)
  if (isTRUE(x$model_options$calc_reproduction) && isTRUE(x$model_options$calc_mortality)) {
    message("Note: Both mortality and reproduction calculations enabled. This may increase computation time.")
  }
  
  if (isTRUE(x$model_options$detailed_output) && !isTRUE(x$model_options$output_daily)) {
    warning("detailed_output = TRUE typically requires output_daily = TRUE for meaningful results.")
  }
  
  # Reset model state
  x$fitted <- FALSE
  x$results <- NULL
  
  return(x)
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

