#' Data Validation Functions for FB4
#'
#' @description
#' Reorganized and optimized versions of your data validation functions.
#' Maintains original interfaces while using core validators internally
#' to eliminate code duplication.
#'
#' @name data-validators
#' @aliases data-validators
NULL

# ============================================================================
# DIET DATA VALIDATION
# ============================================================================

#' Validate Consistency Between Diet and Energy Data
#'
#' @description
#' Validates consistency between diet composition and prey energy density data,
#' ensuring they have matching prey species and valid values.
#'
#' @param diet_data Data frame with diet proportions
#' @param energy_data Data frame with prey energies
#'
#' @return NULL (throws error if inconsistent)
#'
#' @details
#' Validation includes:
#' \itemize{
#'   \item{Matching prey species columns between datasets}
#'   \item{Diet proportions sum approximately to 1.0}
#'   \item{No negative diet proportions}
#'   \item{All prey energies are positive}
#' }
#'
#' @keywords internal
#'
#' @examples
#' diet <- data.frame(Day = 1:5, fish = 0.6, zooplankton = 0.4)
#' energy <- data.frame(Day = 1:5, fish = 4000, zooplankton = 2500)
#' isTRUE(validate_diet_consistency(diet, energy))
#' @export
validate_diet_consistency <- function(diet_data, energy_data) {
  
  # Basic structure validation using existing function (optimized in 02-basic-validators.R)
  validate_time_series_data(diet_data, "diet_data", "Day")
  validate_time_series_data(energy_data, "energy_data", "Day")
  
  # Extract prey columns
  diet_prey_cols <- setdiff(names(diet_data), "Day")
  energy_prey_cols <- setdiff(names(energy_data), "Day")
  
  if (length(diet_prey_cols) == 0) {
    stop("diet_data must have at least one prey column besides 'Day'")
  }
  
  if (!identical(sort(diet_prey_cols), sort(energy_prey_cols))) {
    stop("Prey columns must be identical in diet_data and energy_data")
  }
  
  # Validate diet proportions using core validators
  diet_matrix <- as.matrix(diet_data[diet_prey_cols])
  
  # Check for negative values
  negative_result <- validate_numeric_core(
    value = as.vector(diet_matrix),
    param_name = "diet_proportions",
    min_val = 0,
    strategy = "strict"
  )
  
  if (!negative_result$valid) {
    stop("Diet proportions cannot be negative")
  }
  
  # Check proportion sums
  diet_sums <- rowSums(diet_matrix, na.rm = TRUE)
  if (any(abs(diet_sums - 1) > 0.1, na.rm = TRUE)) {
    warning("Some diet proportions deviate significantly from 1.0")
  }
  
  # Validate prey energies using core validators
  energy_matrix <- as.matrix(energy_data[energy_prey_cols])
  
  energy_result <- validate_positive(
    value = as.vector(energy_matrix),
    param_name = "prey_energies",
    strategy = "strict"
  )
  
  if (!energy_result$valid) {
    stop("Prey energies must be positive")
  }
  
  # Check realistic energy ranges
  energy_range_result <- validate_numeric_core(
    value = as.vector(energy_matrix),
    param_name = "prey_energies",
    min_val = 500,
    max_val = 25000,
    strategy = "warn"
  )
  
  # Issue warnings for energy ranges
  for (warning_msg in energy_range_result$warnings) {
    warning(warning_msg, call. = FALSE)
  }
  
  return(invisible(TRUE))
}

# ============================================================================
# INDIVIDUAL DATA VALIDATION
# ============================================================================

#' Validate individual data for hierarchical models
#'
#' @description
#' Validates individual fish data for hierarchical mark-recapture models.
#'
#' @param individual_data Data frame with individual observations
#' @param require_positive_growth Whether growth must be positive
#' @param check_outliers Whether to check for outliers
#'
#' @return NULL (throws error if invalid)
#' @export
validate_individual_data <- function(individual_data, require_positive_growth = TRUE,
                                    check_outliers = TRUE) {
  
  # Basic structure validation using core validators
  structure_result <- validate_structure_core(
    data = individual_data,
    data_name = "individual_data",
    required_class = "data.frame",
    required_cols = c("individual_id", "initial_weight", "final_weight"),
    min_rows = 1
  )
  
  if (!structure_result$valid) {
    stop(paste(structure_result$errors, collapse = "; "))
  }
  
  # Check for missing values in required columns
  required_cols <- c("individual_id", "initial_weight", "final_weight")
  for (col in required_cols) {
    if (any(is.na(individual_data[[col]]))) {
      stop(col, " cannot contain missing values")
    }
  }
  
  # Check for duplicate individual IDs
  if (anyDuplicated(individual_data$individual_id)) {
    stop("individual_id must be unique")
  }
  
  # Validate weights using core validators
  initial_weight_result <- validate_positive(
    value = individual_data$initial_weight,
    param_name = "initial_weight",
    strategy = "strict"
  )
  
  if (!initial_weight_result$valid) {
    stop(paste(initial_weight_result$errors, collapse = "; "))
  }
  
  final_weight_result <- validate_positive(
    value = individual_data$final_weight,
    param_name = "final_weight", 
    strategy = "strict"
  )
  
  if (!final_weight_result$valid) {
    stop(paste(final_weight_result$errors, collapse = "; "))
  }
  
  # Check growth requirement
  if (require_positive_growth) {
    negative_growth <- individual_data$final_weight <= individual_data$initial_weight
    if (any(negative_growth)) {
      n_negative <- sum(negative_growth)
      stop(n_negative, " individuals show negative or zero growth")
    }
  }
  
  # Check for realistic growth rates
  growth_ratio <- individual_data$final_weight / individual_data$initial_weight
  extreme_growth <- growth_ratio > 10 | growth_ratio < 0.1
  
  if (any(extreme_growth)) {
    n_extreme <- sum(extreme_growth)
    warning(n_extreme, " individuals show extreme growth ratios (>10x or <0.1x)", call. = FALSE)
  }
  
  # Check for outliers if requested
  if (check_outliers) {
    check_weight_outliers(individual_data)
  }
  
  return(invisible(TRUE))
}

#' Check for outliers in weight data
#'
#' @description
#' Identifies potential outliers in weight measurements using IQR method.
#'
#' @param individual_data Individual data frame
#'
#' @keywords internal
check_weight_outliers <- function(individual_data) {
  
  # Calculate growth ratios
  growth_ratios <- individual_data$final_weight / individual_data$initial_weight
  
  # Use IQR method for outlier detection
  Q1 <- quantile(growth_ratios, 0.25, na.rm = TRUE)
  Q3 <- quantile(growth_ratios, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outliers <- growth_ratios < lower_bound | growth_ratios > upper_bound
  
  if (any(outliers)) {
    n_outliers <- sum(outliers)
    outlier_ids <- individual_data$individual_id[outliers]
    
    warning("Detected ", n_outliers, " potential outliers in growth ratios: ",
            paste(head(outlier_ids, 5), collapse = ", "),
            if (n_outliers > 5) " ... and others" else "",
            call. = FALSE)
  }
  
  return(invisible(TRUE))
}

# ============================================================================
# TEMPORAL DATA VALIDATION (from your data-processing.R, optimized)
# ============================================================================

#' Validate temporal data
#'
#' @description
#' Validates temporal data arrays for simulation readiness.
#'
#' @param temperature Temperature vector
#' @param diet_matrix Diet proportion matrix
#' @param energy_matrix Prey energy matrix  
#' @param indigestible_matrix Indigestible fraction matrix
#' @param reproduction_data Reproduction vector
#' @keywords internal
#' @export
validate_temporal_data <- function(temperature, diet_matrix, energy_matrix, 
                                   indigestible_matrix, reproduction_data) {
  
  # Temperature validation using core validators
  temp_result <- validate_temperature(temperature, "temperature", strategy = "warn")
  if (!temp_result$valid) {
    stop("Temperature data contains invalid values: ", paste(temp_result$errors, collapse = "; "))
  }
  
  # Issue temperature warnings
  for (warning_msg in temp_result$warnings) {
    warning(warning_msg, call. = FALSE)
  }
  
  # Diet data validation using core validators
  diet_result <- validate_numeric_core(
    value = as.vector(diet_matrix),
    param_name = "diet_proportions",
    min_val = 0,
    max_val = 1,
    strategy = "strict"
  )
  
  if (!diet_result$valid) {
    stop("Diet proportions contain invalid values: ", paste(diet_result$errors, collapse = "; "))
  }
  
  # Energy data validation using core validators
  energy_result <- validate_numeric_core(
    value = as.vector(energy_matrix),
    param_name = "prey_energies",
    min_val = 0.1,  # Very small positive value
    strategy = "strict"
  )
  
  if (!energy_result$valid) {
    stop("Prey energies contain invalid values: ", paste(energy_result$errors, collapse = "; "))
  }
  
  # Check realistic energy ranges
  energy_range_result <- validate_numeric_core(
    value = as.vector(energy_matrix),
    param_name = "prey_energies",
    min_val = 500,
    max_val = 25000,
    strategy = "warn"
  )
  
  for (warning_msg in energy_range_result$warnings) {
    warning(warning_msg, call. = FALSE)
  }
  
  # Indigestible fraction validation using core validators
  indigestible_result <- validate_fraction(
    value = as.vector(indigestible_matrix),
    param_name = "indigestible_fractions",
    strategy = "strict"
  )
  
  if (!indigestible_result$valid) {
    stop("Indigestible fractions contain invalid values: ", paste(indigestible_result$errors, collapse = "; "))
  }
  
  # Reproduction validation using core validators
  reproduction_result <- validate_fraction(
    value = reproduction_data,
    param_name = "reproduction_proportions",
    strategy = "strict"
  )
  
  if (!reproduction_result$valid) {
    stop("Reproduction proportions contain invalid values: ", paste(reproduction_result$errors, collapse = "; "))
  }
  
  # Check total annual reproduction
  if (sum(reproduction_data) > 2) {
    warning("Total annual reproduction very high (>200% of weight)", call. = FALSE)
  }
  
  return(invisible(TRUE))
}

# ============================================================================
# COMPLETE SIMULATION DATA VALIDATION
# ============================================================================

#' Validate complete simulation data
#'
#' @description
#' Validates the complete processed simulation data structure before running.
#'
#' @param simulation_data Complete processed simulation data
#'
#' @return Invisibly returns \code{TRUE} if validation passes. Throws an error
#'   with a descriptive message if the simulation data structure is invalid.
#' @keywords internal
#' @export
validate_complete_simulation_data <- function(simulation_data) {
  
  # Check main structure using core validators
  structure_result <- validate_structure_core(
    data = simulation_data,
    data_name = "simulation_data",
    required_class = "list",
    allow_empty = FALSE
  )
  
  if (!structure_result$valid) {
    stop("Invalid simulation data structure: ", paste(structure_result$errors, collapse = "; "))
  }
  
  # Check required components
  required_components <- c("species_params", "temporal_data", "simulation_settings", "metadata")
  missing <- setdiff(required_components, names(simulation_data))
  if (length(missing) > 0) {
    stop("Missing simulation data components: ", paste(missing, collapse = ", "))
  }
  
  # Validate temporal data dimensions match
  n_days <- simulation_data$metadata$duration
  temp_data <- simulation_data$temporal_data
  
  if (length(temp_data$temperature) != n_days) {
    stop("Temperature data length does not match simulation duration")
  }
  
  if (nrow(temp_data$diet_proportions) != n_days) {
    stop("Diet proportion data rows do not match simulation duration")
  }
  
  if (nrow(temp_data$prey_energies) != n_days) {
    stop("Prey energy data rows do not match simulation duration")
  }
  
  # Check consistency between species parameters and temporal data
  n_prey_temporal <- length(temp_data$prey_names)
  
  # Only validate data that's actually present and needed
  validate_data_consistency(simulation_data, n_prey_temporal)
  
  return(invisible(TRUE))
}

#' Validate data consistency between components
#'
#' @description
#' Checks cross-component consistency within processed simulation data.
#'
#' @param simulation_data Complete simulation data
#' @param n_prey_temporal Number of prey species in temporal data
#' @keywords internal
#' @export
validate_data_consistency <- function(simulation_data, n_prey_temporal) {
  
  temp_data <- simulation_data$temporal_data
  species_params <- simulation_data$species_params
  
  # Check contaminant data consistency (only if both are present)
  if (!is.null(species_params$contaminant) && !is.null(temp_data$contaminant)) {
    if (!is.null(temp_data$contaminant$prey_concentrations)) {
      if (ncol(temp_data$contaminant$prey_concentrations) != n_prey_temporal) {
        stop("Contaminant prey concentration data does not match number of prey species")
      }
    }
  }
  
  # Check nutrient data consistency (only if both are present)
  if (!is.null(species_params$nutrient) && !is.null(temp_data$nutrient)) {
    if (!is.null(temp_data$nutrient$nitrogen_concentrations)) {
      if (ncol(temp_data$nutrient$nitrogen_concentrations) != n_prey_temporal) {
        stop("Nutrient concentration data does not match number of prey species")
      }
    }
  }
  
  return(invisible(TRUE))
}

# ============================================================================
# FITTING SETTINGS VALIDATION (from your data-processing.R, optimized)
# ============================================================================

#' Validate fitting settings
#'
#' @description
#' Validates processed simulation settings before execution.
#'
#' @param settings Processed simulation settings
#' @keywords internal
#' @export
validate_fitting_settings <- function(settings) {
  
  valid_fit_options <- c("Weight", "Consumption", "Ration", "Ration_prey", "p_value")
  
  if (!settings$fit_to %in% valid_fit_options) {
    stop("fit_to must be one of: ", paste(valid_fit_options, collapse = ", "))
  }
  
  # Validate fit_value using core validators
  fit_value_result <- validate_positive(settings$fit_value, "fit_value", strategy = "strict")
  if (!fit_value_result$valid) {
    stop("fit_value must be a positive number when fit_to is specified")
  }
  
  # Specific validations by fit type
  if (settings$fit_to == "Weight" && settings$fit_value < 0.01) {
    warning("Very low target weight for fitting", call. = FALSE)
  }
  
  if (settings$fit_to %in% c("p_value") && settings$fit_value > 5) {
    warning("Very high p_value for fitting (>5)", call. = FALSE)
  }
  
  return(invisible(TRUE))
}

# ============================================================================
# PARAMETER PROCESSING VALIDATION
# ============================================================================

#' Validate equation parameters against requirements
#'
#' @description
#' Validates equation parameters against their category requirements.
#'
#' @param category Category name
#' @param equation_num Equation number as character
#' @param params Parameter list
#' @keywords internal
#' @export
validate_equation_params <- function(category, equation_num, params) {
  
  if (!category %in% names(EQUATION_REQUIREMENTS)) {
    stop("Unknown category: ", category)
  }
  
  if (!equation_num %in% names(EQUATION_REQUIREMENTS[[category]])) {
    stop("Invalid equation number for ", category, ": ", equation_num)
  }
  
  requirements <- EQUATION_REQUIREMENTS[[category]][[equation_num]]
  
  # Check required parameters
  if ("required" %in% names(requirements)) {
    missing_required <- setdiff(requirements$required, names(params))
    if (length(missing_required) > 0) {
      stop("Missing required parameters for ", category, " equation ", equation_num, 
           ": ", paste(missing_required, collapse = ", "))
    }
  }
  
  # Validate parameter ranges using optimized function from 03-parameter-validators.R
  if ("validations" %in% names(requirements)) {
    validate_parameter_ranges(params, requirements$validations, category)
  }
  
  return(invisible(TRUE))
}

