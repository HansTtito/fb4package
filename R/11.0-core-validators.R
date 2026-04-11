#' Core Validation Functions for FB4
#'
#' @description
#' Atomic validation functions that provide the foundation for all other
#' validation operations. These functions handle the most basic validation
#' patterns used throughout the FB4 system: numeric range checks, structural
#' requirements (required columns, minimum rows), and domain-specific
#' validators for fractions, positive quantities, and temperatures. All
#' validators return standardised \code{fb4_validation} objects constructed
#' by \code{\link{validation_result}}, which can be aggregated with
#' \code{\link{accumulate_validations}}.
#'
#' @references
#' Deslauriers, D., Chipps, S.R., Breck, J.E., Rice, J.A. and Madenjian, C.P.
#' (2017). Fish Bioenergetics 4.0: An R-based modeling application.
#' \emph{Fisheries}, 42(11), 586–596. \doi{10.1080/03632415.2017.1377558}
#'
#' @return No return value; this page documents the core validation functions module. See individual function documentation for return values.
#' @name core-validators
#' @aliases core-validators
NULL

# ============================================================================
# VALIDATION RESULT SYSTEM
# ============================================================================

#' Create standardized validation result
#'
#' @description
#' Constructor for standardized validation result objects used throughout
#' the FB4 validation system.
#'
#' @param valid Logical indicating if validation passed
#' @param errors Character vector of error messages
#' @param warnings Character vector of warning messages
#' @param info Character vector of info messages
#' @param level Validation level ("core", "structure", "parameter", etc.)
#' @param category Optional category being validated
#' @param checked_items List of items that were checked
#'
#' @return An object of class \code{fb4_validation}: a named list with eight
#'   elements: \code{valid} (logical), \code{errors} (character vector of
#'   error messages), \code{warnings} (character vector of warning messages),
#'   \code{info} (character vector of informational messages), \code{level}
#'   (character, validation tier), \code{category} (character or \code{NULL}),
#'   \code{checked_items} (list of items examined), and \code{timestamp}
#'   (POSIXct).
#' @examples
#' validation_result(valid = TRUE)
#' validation_result(valid = FALSE, errors = "weight must be positive",
#'                   level = "parameter")
#' @export
validation_result <- function(valid = TRUE, errors = character(), warnings = character(),
                              info = character(), level = "core", category = NULL,
                              checked_items = list()) {
  structure(list(
    valid = valid,
    errors = errors,
    warnings = warnings,
    info = info,
    level = level,
    category = category,
    checked_items = checked_items,
    timestamp = Sys.time()
  ), class = "fb4_validation")
}

#' Accumulate multiple validation results
#'
#' @description
#' Combines multiple validation results into a single result,
#' aggregating errors, warnings, and info messages.
#'
#' @param ... Validation result objects to combine
#' @param level Overall validation level for the combined result
#'
#' @return An object of class \code{fb4_validation} (see
#'   \code{\link{validation_result}}) representing the combined state of all
#'   inputs. \code{valid} is \code{TRUE} only if all supplied results are
#'   valid. \code{errors} and \code{warnings} are the concatenation of those
#'   fields across all inputs.
#' @examples
#' r1 <- validation_result(valid = TRUE)
#' r2 <- validation_result(valid = FALSE, errors = "value out of range")
#' accumulate_validations(r1, r2)
#' @export
accumulate_validations <- function(..., level = "combined") {
  results <- list(...)
  
  # Handle empty input
  if (length(results) == 0) {
    return(validation_result(level = level))
  }
  
  # Filter out NULL results
  results <- results[!vapply(results, is.null, logical(1))]
  
  if (length(results) == 0) {
    return(validation_result(level = level))
  }
  
  # Aggregate results
  all_valid <- all(vapply(results, function(x) x$valid, logical(1)))
  all_errors <- unlist(lapply(results, function(x) x$errors))
  all_warnings <- unlist(lapply(results, function(x) x$warnings))
  all_info <- unlist(lapply(results, function(x) x$info))
  all_checked <- do.call(c, lapply(results, function(x) x$checked_items))
  
  validation_result(
    valid = all_valid,
    errors = all_errors,
    warnings = all_warnings,
    info = all_info,
    level = level,
    checked_items = all_checked
  )
}

# ============================================================================
# CORE NUMERIC VALIDATION
# ============================================================================

#' Core numeric validation with flexible handling strategies
#'
#' @description
#' Atomic validator for numeric values with support for different
#' handling strategies for problematic values. Used as foundation
#' for other numeric validation functions.
#'
#' @param value Value(s) to validate
#' @param param_name Parameter name for error messages
#' @param required Whether the parameter is required (cannot be NULL)
#' @param allow_na Whether NA values are allowed
#' @param allow_infinite Whether infinite values are allowed
#' @param min_val Minimum allowed value (optional)
#' @param max_val Maximum allowed value (optional)
#' @param strategy Handling strategy: "strict", "clamp", "replace", "warn"
#' @param default_value Default value for "replace" strategy
#' @param integer_only Whether value must be integer
#'
#' @return Validation result with potentially modified value
#' @keywords internal
validate_numeric_core <- function(value, param_name, required = TRUE, allow_na = FALSE,
                                  allow_infinite = FALSE, min_val = NULL, max_val = NULL,
                                  strategy = "strict", default_value = NULL, integer_only = FALSE) {
  
  result <- validation_result(level = "core")
  
  # Check if required but missing
  if (required && is.null(value)) {
    result$valid <- FALSE
    result$errors <- c(result$errors, paste(param_name, "cannot be NULL"))
    return(result)
  }
  
  # If NULL and not required, return valid
  if (is.null(value)) {
    return(result)
  }
  
  # Check if numeric
  if (!is.numeric(value)) {
    result$valid <- FALSE
    result$errors <- c(result$errors, paste(param_name, "must be numeric"))
    return(result)
  }
  
  # Handle NA values
  na_mask <- is.na(value)
  if (any(na_mask) && !allow_na) {
    if (strategy == "strict") {
      result$valid <- FALSE
      result$errors <- c(result$errors, paste(param_name, "contains NA values"))
      return(result)
    } else {
      n_na <- sum(na_mask)
      result$warnings <- c(result$warnings, 
                          sprintf("%s contains %d NA value(s)", param_name, n_na))
    }
  }
  
  # Handle infinite values
  inf_mask <- is.infinite(value) & !na_mask
  if (any(inf_mask) && !allow_infinite) {
    if (strategy == "strict") {
      result$valid <- FALSE
      result$errors <- c(result$errors, paste(param_name, "contains infinite values"))
      return(result)
    } else {
      n_inf <- sum(inf_mask)
      result$warnings <- c(result$warnings,
                          sprintf("%s contains %d infinite value(s)", param_name, n_inf))
    }
  }
  
  # Check integer requirement
  if (integer_only) {
    non_integer_mask <- !na_mask & !inf_mask & (value != round(value))
    if (any(non_integer_mask)) {
      if (strategy == "strict") {
        result$valid <- FALSE
        result$errors <- c(result$errors, paste(param_name, "must be integer"))
        return(result)
      } else {
        result$warnings <- c(result$warnings, paste(param_name, "contains non-integer values"))
      }
    }
  }
  
  # Range validation
  if (!is.null(min_val) || !is.null(max_val)) {
    range_result <- validate_range_core(value, param_name, min_val, max_val, 
                                       strategy, default_value, na_mask | inf_mask)
    result <- accumulate_validations(result, range_result)
  }
  
  # Record what was checked
  result$checked_items <- list(
    param_name = param_name,
    type = "numeric",
    length = length(value),
    has_na = any(na_mask),
    has_infinite = any(inf_mask),
    range_checked = !is.null(min_val) || !is.null(max_val)
  )
  
  return(result)
}

#' Core range validation with multiple strategies
#'
#' @description
#' Validates that numeric values fall within specified ranges.
#' Supports different strategies for handling out-of-range values.
#'
#' @param value Numeric value(s) to validate
#' @param param_name Parameter name for messages
#' @param min_val Minimum allowed value
#' @param max_val Maximum allowed value
#' @param strategy Handling strategy for out-of-range values
#' @param default_value Default value for replacement
#' @param skip_mask Logical mask of values to skip validation
#'
#' @return Validation result
#' @keywords internal
validate_range_core <- function(value, param_name, min_val = NULL, max_val = NULL,
                                strategy = "strict", default_value = NULL, skip_mask = NULL) {
  
  result <- validation_result(level = "core")
  
  if (is.null(min_val) && is.null(max_val)) {
    return(result)
  }
  
  # Create skip mask if not provided
  if (is.null(skip_mask)) {
    skip_mask <- rep(FALSE, length(value))
  }
  
  # Check minimum value
  if (!is.null(min_val)) {
    below_min <- !skip_mask & (value < min_val)
    if (any(below_min, na.rm = TRUE)) {
      n_below <- sum(below_min, na.rm = TRUE)
      min_observed <- if (any(below_min)) min(value[below_min], na.rm = TRUE) else NA
      
      if (strategy == "strict") {
        result$valid <- FALSE
        result$errors <- c(result$errors, 
                          sprintf("%s: %d value(s) below minimum %.3f (smallest: %.3f)",
                                  param_name, n_below, min_val, min_observed))
      } else {
        result$warnings <- c(result$warnings,
                            sprintf("%s: %d value(s) below minimum %.3f", 
                                    param_name, n_below, min_val))
      }
    }
  }
  
  # Check maximum value
  if (!is.null(max_val)) {
    above_max <- !skip_mask & (value > max_val)
    if (any(above_max, na.rm = TRUE)) {
      n_above <- sum(above_max, na.rm = TRUE)
      max_observed <- if (any(above_max)) max(value[above_max], na.rm = TRUE) else NA
      
      if (strategy == "strict") {
        result$valid <- FALSE
        result$errors <- c(result$errors,
                          sprintf("%s: %d value(s) above maximum %.3f (largest: %.3f)",
                                  param_name, n_above, max_val, max_observed))
      } else {
        result$warnings <- c(result$warnings,
                            sprintf("%s: %d value(s) above maximum %.3f",
                                    param_name, n_above, max_val))
      }
    }
  }
  
  return(result)
}

# ============================================================================
# CORE STRUCTURE VALIDATION
# ============================================================================

#' Core data structure validation
#'
#' @description
#' Validates basic data structure requirements like data.frame format,
#' required columns, minimum rows, etc.
#'
#' @param data Data to validate
#' @param data_name Name for error messages
#' @param required_class Expected class(es)
#' @param required_cols Required column names (for data.frames)
#' @param min_rows Minimum number of rows
#' @param min_cols Minimum number of columns
#' @param allow_empty Whether empty data is allowed
#'
#' @return Validation result
#' @keywords internal
validate_structure_core <- function(data, data_name, required_class = NULL, 
                                    required_cols = NULL, min_rows = NULL, 
                                    min_cols = NULL, allow_empty = FALSE) {
  
  result <- validation_result(level = "core", category = "structure")
  
  # Check NULL
  if (is.null(data)) {
    result$valid <- FALSE
    result$errors <- c(result$errors, paste(data_name, "cannot be NULL"))
    return(result)
  }
  
  # Check class
  if (!is.null(required_class)) {
    if (!any(vapply(required_class, function(cls) inherits(data, cls), logical(1)))) {
      result$valid <- FALSE
      result$errors <- c(result$errors, 
                        sprintf("%s must be of class: %s", 
                                data_name, paste(required_class, collapse = " or ")))
      return(result)
    }
  }
  
  # For data.frames, check structure
  if (is.data.frame(data)) {
    
    # Check if empty
    if (nrow(data) == 0 && !allow_empty) {
      result$valid <- FALSE
      result$errors <- c(result$errors, paste(data_name, "cannot be empty"))
      return(result)
    }
    
    # Check minimum rows
    if (!is.null(min_rows) && nrow(data) < min_rows) {
      result$valid <- FALSE
      result$errors <- c(result$errors, 
                        sprintf("%s must have at least %d rows", data_name, min_rows))
    }
    
    # Check minimum columns
    if (!is.null(min_cols) && ncol(data) < min_cols) {
      result$valid <- FALSE
      result$errors <- c(result$errors,
                        sprintf("%s must have at least %d columns", data_name, min_cols))
    }
    
    # Check required columns
    if (!is.null(required_cols)) {
      missing_cols <- setdiff(required_cols, names(data))
      if (length(missing_cols) > 0) {
        result$valid <- FALSE
        result$errors <- c(result$errors,
                          sprintf("Missing columns in %s: %s", 
                                  data_name, paste(missing_cols, collapse = ", ")))
      }
    }
  }
  
  # Record what was checked
  result$checked_items <- list(
    data_name = data_name,
    class = class(data),
    structure_type = if (is.data.frame(data)) "data.frame" else "other",
    dimensions = if (is.data.frame(data)) dim(data) else length(data)
  )
  
  return(result)
}

# ============================================================================
# SPECIALIZED CORE VALIDATORS
# ============================================================================

#' Validate fraction values (0-1 range)
#'
#' @description
#' Specialized validator for fraction/proportion values.
#'
#' @param value Value(s) to validate
#' @param param_name Parameter name
#' @param strategy Handling strategy
#' @param allow_zero Whether zero is allowed
#' @param allow_one Whether one is allowed
#'
#' @return An object of class \code{fb4_validation} (see
#'   \code{\link{validation_result}}). \code{valid} is \code{TRUE} when all
#'   values lie within \eqn{[0, 1]} (or the bounds set by \code{allow_zero}
#'   and \code{allow_one}). Out-of-range values are recorded in
#'   \code{errors} (strategy \code{"strict"}) or \code{warnings}
#'   (strategy \code{"warn"}).
#' @examples
#' validate_fraction(0.5, "diet_proportion")
#' validate_fraction(c(0.3, 0.7), "fractions")
#' @export
validate_fraction <- function(value, param_name, strategy = "strict", 
                             allow_zero = TRUE, allow_one = TRUE) {
  
  min_val <- if (allow_zero) 0 else 0.001
  max_val <- if (allow_one) 1 else 0.999
  
  validate_numeric_core(value, param_name, min_val = min_val, max_val = max_val,
                       strategy = strategy)
}

#' Validate positive values
#'
#' @description
#' Specialized validator for positive numeric values.
#'
#' @param value Value(s) to validate
#' @param param_name Parameter name
#' @param strategy Handling strategy
#' @param min_val Minimum positive value (default 0.001)
#'
#' @return An object of class \code{fb4_validation} (see
#'   \code{\link{validation_result}}). \code{valid} is \code{TRUE} when all
#'   values are \eqn{\ge} \code{min_val}. Violations are recorded in
#'   \code{errors} (\code{strategy = "strict"}) or \code{warnings}
#'   (\code{strategy = "warn"}).
#' @examples
#' validate_positive(5, "weight")
#' validate_positive(0, "weight")$valid
#' @export
validate_positive <- function(value, param_name, strategy = "strict", min_val = 0.001) {
  validate_numeric_core(value, param_name, min_val = min_val, strategy = strategy)
}

#' Validate temperature values
#'
#' @description
#' Specialized validator for temperature values with realistic ranges.
#'
#' @param value Temperature value(s) in Celsius
#' @param param_name Parameter name
#' @param strategy Handling strategy
#' @param min_temp Minimum realistic temperature (default -5°C)
#' @param max_temp Maximum realistic temperature (default 45°C)
#'
#' @return An object of class \code{fb4_validation} (see
#'   \code{\link{validation_result}}). \code{valid} is \code{TRUE} when all
#'   values are finite and lie within \code{[min_temp, max_temp]}. Values
#'   outside the range are recorded in \code{warnings} by default
#'   (\code{strategy = "warn"}).
#' @examples
#' validate_temperature(15, "water_temp")
#' validate_temperature(c(5, 12, 18), "temperatures")
#' @export
validate_temperature <- function(value, param_name, strategy = "warn", 
                                 min_temp = -5, max_temp = 45) {
  validate_numeric_core(value, param_name, min_val = min_temp, max_val = max_temp,
                       strategy = strategy)
}