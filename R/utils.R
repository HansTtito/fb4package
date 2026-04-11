#' Utility Functions for fb4package
#'
#' @description
#' Cross-cutting utilities used throughout the package: the null-coalescing
#' operator, safe mathematical operations, and value clamping.
#' These functions have no internal dependencies and are available to all layers.
#'
#' @return No return value; this page documents cross-cutting utility functions used throughout the package. See individual function documentation for return values.
#' @name utils
#' @aliases utils
NULL

# ============================================================================
# NULL-COALESCING OPERATOR
# ============================================================================

#' Null-coalescing operator
#'
#' @description
#' Returns \code{x} if it is not \code{NULL}, otherwise returns \code{y}.
#' Equivalent to \code{rlang::\%||\%} but without requiring that dependency.
#'
#' @param x Value to test
#' @param y Fallback value if \code{x} is \code{NULL}
#'
#' @return \code{x} if not \code{NULL}, otherwise \code{y}
#'
#' @name op-null-default
#' @keywords internal
#'
#' @examples
#' NULL %||% "default"
#' "value" %||% "default"
#' list()$missing %||% 0
`%||%` <- function(x, y) if (!is.null(x)) x else y


# ============================================================================
# SAFE MATHEMATICAL OPERATIONS
# ============================================================================

#' Safe exponential
#'
#' @description
#' Computes \code{exp(x)} with protection against overflow. Values above
#' \code{max_exp} are clamped before exponentiation to avoid \code{Inf}.
#'
#' @param x Numeric value or vector
#' @param max_exp Maximum allowed exponent, default 700
#' @param warn Logical; issue a warning when clamping occurs, default TRUE
#' @param param_name Name used in warning messages
#'
#' @return \code{exp(x)}, or \code{exp(max_exp)} for values that would overflow
#'
#' @keywords internal
#' @export
#'
#' @examples
#' safe_exp(1)
#' suppressWarnings(safe_exp(1000))
#' suppressWarnings(safe_exp(-1000))
safe_exp <- function(x, max_exp = 700, warn = TRUE, param_name = "exponent") {

  result <- x

  overflow  <- is.finite(x) & (x >  max_exp)
  underflow <- is.finite(x) & (x < -max_exp)

  if (any(overflow, na.rm = TRUE)) {
    if (warn) warning(sprintf(
      "safe_exp: %d %s value(s) above %.0f clamped to avoid overflow",
      sum(overflow), param_name, max_exp), call. = FALSE)
    result[overflow] <- max_exp
  }

  if (any(underflow, na.rm = TRUE)) {
    if (warn) warning(sprintf(
      "safe_exp: %d %s value(s) below -%.0f clamped to avoid underflow",
      sum(underflow), param_name, max_exp), call. = FALSE)
    result[underflow] <- -max_exp
  }

  exp(result)
}


#' Safe square root
#'
#' @description
#' Computes \code{sqrt(x)} with protection against negative inputs.
#' Negative values are clamped to \code{min_val} before taking the root.
#'
#' @param x Numeric value or vector
#' @param min_val Minimum value before sqrt, default 0
#' @param warn Logical; issue a warning when clamping occurs, default TRUE
#' @param param_name Name used in warning messages
#'
#' @return \code{sqrt(x)}, or \code{sqrt(min_val)} for negative values
#'
#' @keywords internal
#' @export
#'
#' @examples
#' safe_sqrt(4)
#' suppressWarnings(safe_sqrt(-1))
safe_sqrt <- function(x, min_val = 0, warn = TRUE, param_name = "value") {

  result <- x
  negative <- is.finite(x) & (x < min_val)

  if (any(negative, na.rm = TRUE)) {
    if (warn) warning(sprintf(
      "safe_sqrt: %d negative %s value(s) clamped to %.3f",
      sum(negative), param_name, min_val), call. = FALSE)
    result[negative] <- min_val
  }

  sqrt(result)
}


# ============================================================================
# VALUE CLAMPING
# ============================================================================

#' Clamp values to a range
#'
#' @description
#' Restricts \code{x} to the interval \code{[min_val, max_val]}.
#' Values outside the range are replaced by the nearest bound.
#'
#' @param x Numeric value or vector
#' @param min_val Lower bound
#' @param max_val Upper bound
#' @param warn Logical; issue a warning when clamping occurs, default TRUE
#' @param param_name Name used in warning messages
#'
#' @return \code{x} with values outside \code{[min_val, max_val]} replaced by the bounds
#'
#' @keywords internal
#' @export
#'
#' @examples
#' suppressWarnings(clamp(1.5, 0, 1))
#' suppressWarnings(clamp(-0.5, 0, 1))
#' clamp(0.3, 0, 1)
#' suppressWarnings(clamp(c(-1, 0.5, 2), 0, 1))
clamp <- function(x, min_val, max_val, warn = TRUE, param_name = "value") {

  if (min_val > max_val) stop(sprintf(
    "clamp: min_val (%.3f) cannot exceed max_val (%.3f)", min_val, max_val))

  result <- x
  non_finite <- !is.finite(x)

  if (any(non_finite, na.rm = TRUE) && warn) {
    warning(sprintf("clamp: %d non-finite %s value(s) preserved as-is",
                    sum(non_finite, na.rm = TRUE), param_name), call. = FALSE)
  }

  below <- is.finite(x) & (x < min_val)
  above <- is.finite(x) & (x > max_val)

  if (any(below, na.rm = TRUE)) {
    if (warn) warning(sprintf(
      "clamp: %d %s value(s) below %.3f clamped (min observed: %.3f)",
      sum(below), param_name, min_val, min(x[below], na.rm = TRUE)), call. = FALSE)
    result[below] <- min_val
  }

  if (any(above, na.rm = TRUE)) {
    if (warn) warning(sprintf(
      "clamp: %d %s value(s) above %.3f clamped (max observed: %.3f)",
      sum(above), param_name, max_val, max(x[above], na.rm = TRUE)), call. = FALSE)
    result[above] <- max_val
  }

  result
}


# ============================================================================
# CONFIDENCE INTERVAL UTILITIES
# ============================================================================

#' @noRd
z_score <- function(confidence_level = 0.95) {
  stats::qnorm(1 - (1 - confidence_level) / 2)
}


#' Confidence interval from estimate and standard error
#'
#' @description
#' Computes a symmetric confidence interval given a point estimate and
#' its standard error. Returns \code{NA} bounds when either input is \code{NA}.
#'
#' @param estimate Numeric point estimate (scalar or vector)
#' @param se Standard error of the estimate
#' @param confidence_level Confidence level in (0, 1), default 0.95
#' @param method \code{"normal"} (default) for symmetric normal-theory interval;
#'   \code{"log-normal"} for a delta-method interval on the log scale
#'   (requires \code{estimate > 0})
#'
#' @return Named list with \code{ci_lower}, \code{ci_upper},
#'   \code{method}, and \code{confidence_level}
#'
#' @keywords internal
#' @export
#'
#' @examples
#' calculate_confidence_intervals(0.5, 0.05)
#' calculate_confidence_intervals(0.5, 0.05, confidence_level = 0.99)
#' calculate_confidence_intervals(NA, 0.05)
calculate_confidence_intervals <- function(estimate, se,
                                           confidence_level = 0.95,
                                           method = "normal") {
  if (is.na(estimate) || is.na(se)) {
    return(list(ci_lower = NA, ci_upper = NA,
                method = method, confidence_level = confidence_level))
  }

  z <- z_score(confidence_level)

  if (method == "normal") {
    ci_lower <- estimate - z * se
    ci_upper <- estimate + z * se

  } else if (method == "log-normal") {
    if (estimate <= 0) {
      warning("calculate_confidence_intervals: log-normal method requires estimate > 0",
              call. = FALSE)
      return(list(ci_lower = NA, ci_upper = NA,
                  method = method, confidence_level = confidence_level))
    }
    log_se   <- se / estimate          # delta method
    ci_lower <- exp(log(estimate) - z * log_se)
    ci_upper <- exp(log(estimate) + z * log_se)

  } else {
    stop("calculate_confidence_intervals: method must be 'normal' or 'log-normal'")
  }

  list(ci_lower = ci_lower, ci_upper = ci_upper,
       method = method, confidence_level = confidence_level)
}
