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
# MATHEMATICAL OPERATIONS (your original functions, optimized)
# ============================================================================

#' Safe Exponential Function
#'
#' @description
#' Computes exponential with protection against overflow and invalid inputs.
#' Limits extreme values to prevent numerical overflow and issues warnings
#' when input values are modified.
#'
#' @param x Numeric value or vector
#' @param max_exp Maximum exponent value, default 700
#' @param warn Logical, whether to issue warnings for problematic values, default TRUE
#' @param param_name Character string identifying the parameter for warning messages
#'
#' @return Exponential of x, or appropriate bounded value
#'
#' @details
#' This function prevents numerical overflow that can occur with exp()
#' for very large input values, which is common in bioenergetic calculations
#' with temperature dependencies. When warn=TRUE, it alerts users about
#' input modifications that could affect model behavior.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' safe_exp(1)                     # Returns exp(1) ≈ 2.718
#' safe_exp(1000)                  # Returns exp(700) with warning
#' safe_exp(-1000)                 # Returns 0 with warning
#' safe_exp(c(1, 800, -800))      # Warns about extreme values
#' }
#' @export
safe_exp <- function(x, max_exp = 700, warn = TRUE, param_name = "exponent") {
  
  # Use core safe math operation
  math_result <- safe_math_core(
    x = x,
    operation = "exp",
    param_name = param_name,
    max_exp = max_exp,
    strategy = if (warn) "clamp" else "strict"
  )
  
  # Issue warnings if requested and validation has warnings
  if (warn && length(math_result$validation$warnings) > 0) {
    for (warning_msg in math_result$validation$warnings) {
      warning(warning_msg, call. = FALSE)
    }
  }
  
  return(math_result$result)
}


#' Safe Square Root Function
#'
#' @description
#' Computes square root with protection against invalid inputs.
#' Returns NA for non-finite inputs and minimum value for negative inputs.
#' Issues warnings when input values are modified or invalid.
#'
#' @param x Numeric value or vector
#' @param min_val Minimum allowed value, default 0
#' @param warn Logical, whether to issue warnings for problematic values, default TRUE
#' @param param_name Character string identifying the parameter for warning messages
#'
#' @return Square root of x, or appropriate fallback value
#'
#' @details
#' This function prevents errors that could occur with standard sqrt()
#' when dealing with potentially problematic numerical data. When warn=TRUE,
#' it alerts users about input modifications, helping identify data quality issues.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' safe_sqrt(4)                    # Returns 2
#' safe_sqrt(-1)                   # Returns 0 (min_val) with warning
#' safe_sqrt(NA)                   # Returns NA with warning
#' safe_sqrt(c(4, -1, 9), warn=TRUE)  # Warns about negative value
#' }
#' @export
safe_sqrt <- function(x, min_val = 0, warn = TRUE, param_name = "value") {
  
  # Use core safe math operation
  math_result <- safe_math_core(
    x = x,
    operation = "sqrt",
    param_name = param_name,
    min_val = min_val,
    strategy = if (warn) "clamp" else "strict"
  )
  
  # Issue warnings if requested and validation has warnings
  if (warn && length(math_result$validation$warnings) > 0) {
    for (warning_msg in math_result$validation$warnings) {
      warning(warning_msg, call. = FALSE)
    }
  }
  
  return(math_result$result)
}

#' Clamp Values Between Minimum and Maximum
#'
#' @description
#' Constrains numeric values to lie within specified bounds.
#' Values below minimum are set to minimum, values above maximum are set to maximum.
#' Issues warnings when values are modified to alert users about potential data issues.
#'
#' @param x Numeric value or vector
#' @param min_val Minimum allowed value
#' @param max_val Maximum allowed value
#' @param warn Logical, whether to issue warnings for out-of-range values, default TRUE
#' @param param_name Character string identifying the parameter for warning messages
#'
#' @return Clamped value(s) within [min_val, max_val]
#'
#' @details
#' This function is essential for ensuring model parameters stay within
#' biologically realistic ranges and maintaining numerical stability.
#' When warn=TRUE, it provides detailed feedback about which values were
#' modified, helping users identify potential data quality issues or
#' model parameter problems.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' clamp(1.5, 0, 1)                          # Returns 1.0 with warning
#' clamp(-0.5, 0, 1)                         # Returns 0.0 with warning
#' clamp(0.3, 0, 1)                          # Returns 0.3 (no warning)
#' clamp(c(-1, 0.5, 2), 0, 1, param_name="proportion")  # Warns about 2 values
#' clamp(15, 0, 30, warn=FALSE)              # Returns 15, no warning
#' }
#' @export
clamp <- function(x, min_val, max_val, warn = TRUE, param_name = "value") {
  
  if (min_val > max_val) {
    stop(sprintf("clamp: min_val (%.3f) cannot be greater than max_val (%.3f)", 
                 min_val, max_val))
  }
  
  # Use core range validation with clamping strategy
  range_result <- validate_range_core(
    value = x,
    param_name = param_name,
    min_val = min_val,
    max_val = max_val,
    strategy = "clamp"
  )
  
  # Apply clamping
  result <- x
  
  # Handle non-finite values first
  non_finite_mask <- is.na(x) | !is.finite(x)
  if (any(non_finite_mask, na.rm = TRUE) && warn) {
    n_invalid <- sum(non_finite_mask, na.rm = TRUE)
    warning(sprintf(
      "clamp: %d non-finite %s(s) found, preserving as NA",
      n_invalid, param_name
    ), call. = FALSE)
  }
  
  # Clamp to minimum
  below_min_mask <- !non_finite_mask & (x < min_val)
  if (any(below_min_mask, na.rm = TRUE)) {
    if (warn) {
      n_below <- sum(below_min_mask, na.rm = TRUE)
      min_observed <- min(x[below_min_mask], na.rm = TRUE)
      warning(sprintf(
        "clamp: %d %s(s) below minimum (%.3f), smallest was %.3f, clamped to %.3f",
        n_below, param_name, min_val, min_observed, min_val
      ), call. = FALSE)
    }
    result[below_min_mask] <- min_val
  }
  
  # Clamp to maximum
  above_max_mask <- !non_finite_mask & (x > max_val)
  if (any(above_max_mask, na.rm = TRUE)) {
    if (warn) {
      n_above <- sum(above_max_mask, na.rm = TRUE)
      max_observed <- max(x[above_max_mask], na.rm = TRUE)
      warning(sprintf(
        "clamp: %d %s(s) above maximum (%.3f), largest was %.3f, clamped to %.3f",
        n_above, param_name, max_val, max_observed, max_val
      ), call. = FALSE)
    }
    result[above_max_mask] <- max_val
  }
  
  return(result)
}

# ============================================================================
# BASIC PARAMETER VALIDATION (your original functions, optimized)
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
#' \dontrun{
#' check_numeric_value(5, "weight")                    # Valid
#' check_numeric_value(-1, "weight", min_val = 0)     # Error: below minimum
#' check_numeric_value(NA, "weight")                  # Error: not finite
#' }
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
#' This is your original function optimized with core validators.
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