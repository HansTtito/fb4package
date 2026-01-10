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
# MAIN CLASS: Bioenergetic Constructor and Core Methods
# ============================================================================

#' Constructor for Bioenergetic Objects
#'
#' @description
#' Creates a Bioenergetic class object that encapsulates all components
#' of the fish bioenergetic model for streamlined simulation management.
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
#' bioenergetic model components.
#'
#' \strong{Required Components:}
#' \describe{
#'   \item{species_params}{Parameter sets for consumption, respiration, etc.}
#'   \item{species_info}{Species identification with scientific_name or common_name}
#' }
#'
#' \strong{Optional Components:}
#' \describe{
#'   \item{environmental_data}{Temperature and other environmental variables}
#'   \item{diet_data}{Diet composition and prey energy densities}
#'   \item{model_options}{Sub-model toggles and advanced settings}
#'   \item{simulation_settings}{Initial conditions and duration}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create species parameters
#' params <- list(
#'   consumption = list(CEQ = 2, CA = 0.303, CB = -0.275, CQ = 3, CTO = 15, CTM = 25),
#'   respiration = list(REQ = 1, RA = 0.0548, RB = -0.299, RQ = 2, RTO = 5, RTM = 25)
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
# CONFIGURATION METHODS
# ============================================================================

#' Set Environmental Data for Bioenergetic Objects
#'
#' @description
#' Updates the environmental data component of a Bioenergetic object
#' with new temperature information.
#'
#' @param x Bioenergetic object
#' @param temperature_data Data frame with Day and Temperature columns
#' @return Updated Bioenergetic object with new environmental data
#' @export
set_environment <- function(x, temperature_data) {
  UseMethod("set_environment")
}

#' @export
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
#' new diet composition and prey energy information.
#'
#' @param x Bioenergetic object
#' @param diet_proportions Data frame with daily diet proportions
#' @param prey_energies Data frame with daily prey energy densities
#' @param indigestible_prey Data frame with indigestible proportions (optional)
#' @param normalize_diet Logical, whether to normalize diet proportions to sum to 1 (default TRUE)
#' @return Updated Bioenergetic object with new diet data
#' @export
set_diet <- function(x, diet_proportions, prey_energies, indigestible_prey = NULL, normalize_diet = TRUE) {
  UseMethod("set_diet")
}

#' @export
set_diet.Bioenergetic <- function(x, diet_proportions, prey_energies, 
                                  indigestible_prey = NULL, normalize_diet = TRUE) {
  
  # Basic validation
  stopifnot(all(c("Day") %in% names(diet_proportions)),
            all(c("Day") %in% names(prey_energies)))
  
  # Validate individual datasets
  validate_time_series_data(diet_proportions, "diet_proportions", c("Day"))
  validate_time_series_data(prey_energies, "prey_energies", c("Day"))
  
  # Cross-validation between datasets
  validate_diet_consistency(diet_proportions, prey_energies)
  
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
#' Updates the simulation configuration of a Bioenergetic object.
#'
#' @param x Bioenergetic object
#' @param initial_weight Initial weight in grams
#' @param duration Simulation duration in days (auto-detected if NULL)
#' @return Updated Bioenergetic object with new simulation settings
#' @export
set_simulation_settings <- function(x, initial_weight = NULL, duration = NULL) {
  UseMethod("set_simulation_settings")
}

#' @export
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
#' @return Logical indicating whether object is of class Bioenergetic
#' @export
is.Bioenergetic <- function(x) inherits(x, "Bioenergetic")

#' Get Parameter Value from Species Parameters
#'
#' @description
#' Retrieves a specific parameter value from species parameter lists,
#' searching across all parameter categories.
#'
#' @param params Species parameters list
#' @param param Parameter name to retrieve
#' @return Parameter value or NULL if not found
#' @export
get_parameter_value <- function(params, param) {
  for (cat in names(params)) {
    if (param %in% names(params[[cat]])) return(params[[cat]][[param]])
  }
  NULL
}

#' Set Parameter Value in Species Parameters
#'
#' @description
#' Sets a specific parameter value in species parameter lists,
#' automatically finding the correct category.
#'
#' @param params Species parameters list
#' @param param Parameter name to set
#' @param value New parameter value
#' @return Updated parameters list
#' @export
set_parameter_value <- function(params, param, value) {
  for (cat in names(params)) {
    if (param %in% names(params[[cat]])) {
      params[[cat]][[param]] <- value
      return(params)
    }
  }
  stop("Parameter '", param, "' not found in any category.")
}