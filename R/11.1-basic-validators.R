#' Basic Validation Functions for FB4
#'
#' @description
#' Optimized versions of basic validation and mathematical functions.
#' These functions maintain their original interfaces while using
#' core validators internally to eliminate code duplication.
#'
#' @name basic-validators
#' @aliases basic-validators
NULL

# ============================================================================
# BASIC PARAMETER VALIDATION
# ============================================================================

#' Check Numeric Value
#'
#' @description
#' Fast validation of numeric values with basic range checking.
#' Simplified utility function for common validation needs.
#'
#' @param value Value to validate
#' @param name Parameter name for error messages
#' @param min_val Minimum allowed value (default -Inf)
#' @param max_val Maximum allowed value (default Inf)
#' @return Validated numeric value(s)
#' 
#' @details
#' Performs essential validations:
#' \itemize{
#'   \item{Not NULL}
#'   \item{Numeric type}
#'   \item{Finite values (no NA, NaN, Inf)}
#'   \item{Within specified range}
#' }
#' 
#' @keywords internal
#' 
#' @examples
#' check_numeric_value(5, "weight")
#' try(check_numeric_value(-1, "weight", min_val = 0))
#' try(check_numeric_value(NA, "weight"))
#' @export
check_numeric_value <- function(value, name, min_val = -Inf, max_val = Inf) {
  
  # Use core numeric validation with strict strategy
  validation_result <- validate_numeric_core(
    value = value,
    param_name = name,
    required = TRUE,
    allow_na = FALSE,
    allow_infinite = FALSE,
    min_val = if (is.finite(min_val)) min_val else NULL,
    max_val = if (is.finite(max_val)) max_val else NULL,
    strategy = "strict"
  )
  
  # Throw errors if validation failed
  if (!validation_result$valid) {
    stop(paste(validation_result$errors, collapse = "; "), call. = FALSE)
  }
  
  # Issue warnings if any
  if (length(validation_result$warnings) > 0) {
    for (warning_msg in validation_result$warnings) {
      warning(warning_msg, call. = FALSE)
    }
  }
  
  return(value)
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
#' @return Invisibly returns \code{TRUE} if validation passes; throws an error otherwise.
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
#' isTRUE(validate_basic_params(10.5, 365))
#' try(validate_basic_params(-5, 100))
#' @export
validate_basic_params <- function(initial_weight, duration) {
  
  # Validate initial weight using core validator
  weight_result <- validate_positive(initial_weight, "initial_weight", strategy = "strict")
  if (!weight_result$valid) {
    stop(paste(weight_result$errors, collapse = "; "), call. = FALSE)
  }
  
  # Validate duration using core validator
  duration_result <- validate_positive(duration, "duration", strategy = "strict")
  if (!duration_result$valid) {
    stop(paste(duration_result$errors, collapse = "; "), call. = FALSE)
  }
  
  # Performance warning for very long durations
  if (duration > 10000) {
    warning("Very long duration (>10000 days), this may cause performance issues", call. = FALSE)
  }
  
  return(invisible(TRUE))
}


#' Create empty composition for invalid inputs (Utility)
#'
#' @return Empty composition list
#' @keywords internal
#' @export
create_empty_composition <- function() {
  return(list(
    total_weight = 0,
    water_g = 0, protein_g = 0, ash_g = 0, fat_g = 0,
    water_fraction = 0, protein_fraction = 0, ash_fraction = 0, fat_fraction = 0,
    energy_density = 0, total_energy = 0,
    total_fraction = 0, balanced = FALSE
  ))
}

# ============================================================================
# SPECIALIZED BASIC VALIDATORS (using your patterns)
# ============================================================================

#' Validate Time Series Data Structure (Basic Level)
#'
#' @description
#' Basic validation of time series data structure and content for use in FB4 simulations.
#'
#' @param data Data to validate
#' @param data_name Name of the dataset (for error messages)
#' @param required_cols Required column names
#' @param min_cols Minimum number of columns
#'
#' @return Invisibly returns \code{TRUE} if validation passes; throws an error otherwise.
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
#' temp_data <- data.frame(Day = 1:10, Temperature = 15:24)
#' isTRUE(validate_time_series_data(temp_data, "temperature", c("Day", "Temperature")))
#' @export
validate_time_series_data <- function(data, data_name, required_cols = NULL, min_cols = NULL) {
  
  # Use core structure validation
  structure_result <- validate_structure_core(
    data = data,
    data_name = data_name,
    required_class = "data.frame",
    required_cols = required_cols,
    min_cols = min_cols,
    allow_empty = FALSE
  )
  
  # Throw errors if structure validation failed
  if (!structure_result$valid) {
    stop(paste(structure_result$errors, collapse = "; "), call. = FALSE)
  }
  
  # Validate Day column specifically
  if ("Day" %in% names(data)) {
    
    # Use core numeric validation for Day column
    day_result <- validate_numeric_core(
      value = data$Day,
      param_name = paste("'Day' column in", data_name),
      required = TRUE,
      allow_na = FALSE,
      integer_only = TRUE,
      min_val = 1,
      strategy = "strict"
    )
    
    if (!day_result$valid) {
      stop(paste(day_result$errors, collapse = "; "), call. = FALSE)
    }
    
    # Check for duplicates
    if (anyDuplicated(data$Day)) {
      warning("'Day' column in ", data_name, " contains duplicate values", call. = FALSE)
    }
    
    # Check ascending order
    if (any(diff(data$Day) <= 0)) {
      warning("'Day' column in ", data_name, " is not in ascending order", call. = FALSE)
    }
  }
  
  # Issue any warnings from structure validation
  if (length(structure_result$warnings) > 0) {
    for (warning_msg in structure_result$warnings) {
      warning(warning_msg, call. = FALSE)
    }
  }
  
  return(invisible(TRUE))
}