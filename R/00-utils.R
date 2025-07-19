#' Utility Functions for FB4 Model
#'
#' @description
#' Collection of utility functions for the Fish Bioenergetics 4.0 model,
#' including mathematical operators, validation functions, and data processing helpers.
#'
#' @name utils
#' @aliases utils
NULL

# ============================================================================
# OPERATORS AND BASIC MATHEMATICAL FUNCTIONS
# ============================================================================

#' Null Coalescing Operator
#'
#' @description
#' Returns the first value if it's not NULL, otherwise returns the second value.
#' Similar to the || operator in other programming languages.
#'
#' @param x First value
#' @param y Default value if x is NULL
#'
#' @return x if not NULL, y otherwise
#'
#' @details
#' This operator is useful for providing default values when working with
#' potentially NULL parameters or list elements.
#'
#' @name null-coalescing
#' @rdname null-coalescing
#' @export
#'
#' @examples
#' # Basic usage
#' NULL %||% "default"        # Returns "default"
#' "value" %||% "default"     # Returns "value"
#' 
#' # Common use case with list elements
#' params <- list(temperature = 15)
#' temp <- params$temperature %||% 10  # Returns 15
#' depth <- params$depth %||% 5        # Returns 5 (default)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


#' Safe Square Root Function
#'
#' @description
#' Computes square root with protection against invalid inputs.
#' Returns NA for non-finite inputs and minimum value for negative inputs.
#'
#' @param x Numeric value
#' @param min_val Minimum allowed value, default 0
#'
#' @return Square root of x, or appropriate fallback value
#'
#' @details
#' This function prevents errors that could occur with standard sqrt()
#' when dealing with potentially problematic numerical data.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' safe_sqrt(4)      # Returns 2
#' safe_sqrt(-1)     # Returns 0 (min_val)
#' safe_sqrt(NA)     # Returns NA
#' }
safe_sqrt <- function(x, min_val = 0) {
  if (is.na(x) || !is.finite(x)) return(NA_real_)
  if (x < min_val) return(min_val)
  return(sqrt(x))
}

#' Safe Exponential Function
#'
#' @description
#' Computes exponential with protection against overflow and invalid inputs.
#' Limits extreme values to prevent numerical overflow.
#'
#' @param x Numeric value
#' @param max_exp Maximum exponent value, default 700
#'
#' @return Exponential of x, or appropriate bounded value
#'
#' @details
#' This function prevents numerical overflow that can occur with exp()
#' for very large input values, which is common in bioenergetic calculations
#' with temperature dependencies.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' safe_exp(1)       # Returns exp(1) ≈ 2.718
#' safe_exp(1000)    # Returns exp(700) (bounded)
#' safe_exp(-1000)   # Returns 0
#' }
safe_exp <- function(x, max_exp = 700) {
  if (is.na(x) || !is.finite(x)) return(NA_real_)
  if (x > max_exp) return(exp(max_exp))
  if (x < -max_exp) return(0)
  return(exp(x))
}

#' Clamp Values Between Minimum and Maximum
#'
#' @description
#' Constrains numeric values to lie within specified bounds.
#' Values below minimum are set to minimum, values above maximum are set to maximum.
#'
#' @param x Numeric value or vector
#' @param min_val Minimum allowed value
#' @param max_val Maximum allowed value
#'
#' @return Clamped value(s) within [min_val, max_val]
#'
#' @details
#' This function is essential for ensuring model parameters stay within
#' biologically realistic ranges and maintaining numerical stability.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' clamp(1.5, 0, 1)    # Returns 1.0
#' clamp(-0.5, 0, 1)   # Returns 0.0
#' clamp(0.3, 0, 1)    # Returns 0.3
#' clamp(c(-1, 0.5, 2), 0, 1)  # Returns c(0, 0.5, 1)
#' }
clamp <- function(x, min_val, max_val) {
  pmax(min_val, pmin(max_val, x))
}

# ============================================================================
# BASIC VALIDATIONS
# ============================================================================

#' Validate Time Series Data
#'
#' @description
#' Validates time series data structure and content for use in FB4 simulations.
#' Checks for required columns, data types, and temporal consistency.
#'
#' @param data Data to validate
#' @param data_name Name of the dataset (for error messages)
#' @param required_cols Required column names
#' @param min_cols Minimum number of columns
#'
#' @return NULL (throws error if invalid)
#'
#' @details
#' Performs comprehensive validation including:
#' \itemize{
#'   \item{Structure validation (data.frame, non-empty)}
#'   \item{Required column presence}
#'   \item{Day column validation (numeric, finite, ascending)}
#'   \item{Duplicate detection}
#' }
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' temp_data <- data.frame(Day = 1:10, Temperature = 15:24)
#' validate_time_series_data(temp_data, "temperature", c("Day", "Temperature"))
#' }
validate_time_series_data <- function(data, data_name, required_cols = NULL, min_cols = NULL) {
  
  if (is.null(data)) {
    stop(data_name, " cannot be NULL")
  }
  
  if (!is.data.frame(data)) {
    stop(data_name, " must be a data.frame")
  }
  
  if (nrow(data) == 0) {
    stop(data_name, " cannot be empty")
  }
  
  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop("Missing columns in ", data_name, ": ", paste(missing_cols, collapse = ", "))
    }
  }
  
  if (!is.null(min_cols) && ncol(data) < min_cols) {
    stop(data_name, " must have at least ", min_cols, " columns")
  }
  
  if ("Day" %in% names(data)) {
    if (!is.numeric(data$Day)) {
      stop("'Day' column in ", data_name, " must be numeric")
    }
    if (any(!is.finite(data$Day))) {
      stop("'Day' column in ", data_name, " contains non-finite values")
    }
    if (anyDuplicated(data$Day)) {
      warning("'Day' column in ", data_name, " contains duplicate values")
    }
    if (any(diff(data$Day) <= 0)) {
      warning("'Day' column in ", data_name, " is not in ascending order")
    }
  }
}

#' Validate Basic Model Parameters
#'
#' @description
#' Validates fundamental model parameters for biological feasibility
#' and computational practicality.
#'
#' @param initial_weight Initial weight in grams
#' @param duration Duration in days
#'
#' @return NULL (throws error if invalid)
#'
#' @details
#' Checks that:
#' \itemize{
#'   \item{Initial weight is positive and numeric}
#'   \item{Duration is positive and numeric}
#'   \item{Duration is not excessively long (performance warning)}
#' }
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' validate_basic_params(10.5, 365)  # Valid
#' validate_basic_params(-5, 100)    # Error: negative weight
#' }
validate_basic_params <- function(initial_weight, duration) {
  if (!is.numeric(initial_weight) || initial_weight <= 0) {
    stop("initial_weight must be a positive number")
  }
  if (!is.numeric(duration) || duration <= 0) {
    stop("duration must be a positive number")
  }
  if (duration > 10000) {
    warning("Very long duration (>10000 days), this may cause performance issues")
  }
}

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
#' \dontrun{
#' diet <- data.frame(Day = 1:5, fish = 0.6, zooplankton = 0.4)
#' energy <- data.frame(Day = 1:5, fish = 4000, zooplankton = 2500)
#' validate_diet_consistency(diet, energy)
#' }
validate_diet_consistency <- function(diet_data, energy_data) {
  diet_prey_cols <- setdiff(names(diet_data), "Day")
  energy_prey_cols <- setdiff(names(energy_data), "Day")
  
  if (length(diet_prey_cols) == 0) {
    stop("diet_data must have at least one prey column besides 'Day'")
  }
  
  if (!identical(sort(diet_prey_cols), sort(energy_prey_cols))) {
    stop("Prey columns must be identical in diet_data and energy_data")
  }
  
  diet_sums <- rowSums(diet_data[diet_prey_cols], na.rm = TRUE)
  if (any(abs(diet_sums - 1) > 0.1, na.rm = TRUE)) {
    warning("Some diet proportions deviate significantly from 1.0")
  }
  
  if (any(as.matrix(diet_data[diet_prey_cols]) < 0, na.rm = TRUE)) {
    stop("Diet proportions cannot be negative")
  }
  
  if (any(as.matrix(energy_data[energy_prey_cols]) <= 0, na.rm = TRUE)) {
    stop("Prey energies must be positive")
  }
}


#' Check Numeric Value
#'
#' @param value Value to validate
#' @param name Parameter name for error messages
#' @param min_val Minimum allowed value (default -Inf)
#' @param max_val Maximum allowed value (default Inf)
#' @param allow_vector Allow vectors or require scalar (default FALSE = scalar only)
#' @param must_be_integer Require integer values (default FALSE)
#' @return Validated numeric value(s)
check_numeric_value <- function(value, name, min_val = -Inf, max_val = Inf, 
                                allow_vector = FALSE, must_be_integer = FALSE) {
  
  # Fast NULL check
  if (is.null(value)) {
    stop(name, " cannot be NULL", call. = FALSE)
  }
  
  # Check for NA first (works for any type)
  if (anyNA(value)) {
    stop(name, " cannot contain NA, NaN, or Inf values", call. = FALSE)
  }
  
  # Fast type check
  if (!is.numeric(value)) {
    stop(name, " must be numeric, got ", class(value)[1], call. = FALSE)
  }
  
  # Fast finite check (más eficiente que individually checking)
  if (!all(is.finite(value))) {
    stop(name, " cannot contain NA, NaN, or Inf values", call. = FALSE)
  }
  
  # Scalar validation
  if (!allow_vector && length(value) != 1) {
    stop(name, " must be a single value, got ", length(value), " values", call. = FALSE)
  }
  
  # Integer validation (if needed)
  if (must_be_integer && !all(value == as.integer(value))) {
    stop(name, " must be integer values", call. = FALSE)
  }
  
  # Range validation (vectorized)
  if (any(value < min_val) || any(value > max_val)) {
    out_of_range <- value[value < min_val | value > max_val]
    stop(name, " must be between ", min_val, " and ", max_val, 
         ". Out of range values: ", paste(out_of_range, collapse = ", "), 
         call. = FALSE)
  }
  
  return(value)  # Return as-is, no unnecessary conversion
}

# ============================================================================
# SPECIES PARAMETER VALIDATION
# ============================================================================


#' Calculate Daily Spawning Energy Loss
#'
#' @description
#' Calculates the energy lost to reproduction (spawning) on a given day
#' based on reproductive schedule and fish condition.
#'
#' @param day Current simulation day
#' @param current_weight Current fish weight (g)
#' @param predator_ed Predator energy density (J/g)
#' @param model_options Model configuration options
#' @param reproduction_data Vector with daily reproduction proportions
#'
#' @return Spawning energy loss (J)
#'
#' @details
#' Spawning energy represents the metabolic cost of reproduction,
#' including gamete production and spawning behavior. The energy
#' loss is calculated as a fraction of total body energy content:
#'
#' spawn_energy = spawn_fraction × weight × energy_density
#'
#' Spawning only occurs when:
#' \itemize{
#'   \item{Reproduction calculations are enabled}
#'   \item{A spawning schedule is provided}
#'   \item{The spawning fraction for the day is > 0}
#' }
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal function used during simulation
#' spawn_loss <- calculate_daily_spawn_energy(
#'   day = 150, 
#'   current_weight = 25, 
#'   predator_ed = 5000,
#'   model_options = my_options,
#'   reproduction_data = my_repro_data
#' )
#' }
calculate_spawn_energy <- function(day, current_weight, predator_ed, model_options, reproduction_data = NULL) {
  
  # Si no está habilitada la reproducción, return 0
  if (!(model_options$calc_reproduction %||% FALSE)) {
    return(0)
  }
  
  # Si no hay datos de reproducción, avisar pero no fallar
  if (is.null(reproduction_data)) {
    warning("Reproduction enabled but no reproduction data provided. Using 0.")
    return(0)
  }
  
  # Obtener proporción de pérdida para este día
  spawn_proportion <- reproduction_data[day] %||% 0
  
  if (spawn_proportion > 0) {
    # Energía perdida = proporción × peso × densidad energética
    spawn_energy <- spawn_proportion * current_weight * predator_ed
    return(spawn_energy)
  }
  
  return(0)
}


