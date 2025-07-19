#' FB4 Model Test Suite
#'
#' @description
#' Comprehensive test suite for Fish Bioenergetics 4.0 model functions
#' using testthat framework. Tests cover utility functions, bioenergetic
#' calculations, data processing, and complete simulations.
#'
#' @name fb4-tests
NULL

# Load required packages
library(testthat)

# ============================================================================
# UTILITY FUNCTIONS TESTS
# ============================================================================

test_that("Null coalescing operator works correctly", {
  # Basic functionality
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(0 %||% 5, 0)
  expect_equal(FALSE %||% TRUE, FALSE)
  
  # With lists and complex objects
  params <- list(temperature = 15)
  expect_equal(params$temperature %||% 10, 15)
  expect_equal(params$depth %||% 5, 5)
  
  # Edge cases
  expect_equal(NA %||% "default", NA)
  expect_equal("" %||% "default", "")
})

test_that("Safe mathematical functions handle edge cases", {
  # Safe square root
  expect_equal(safe_sqrt(4), 2)
  expect_equal(safe_sqrt(-1), 0)
  expect_equal(safe_sqrt(-1, min_val = 1), 1)
  expect_true(is.na(safe_sqrt(NA)))
  expect_true(is.na(safe_sqrt(Inf)))
  
  # Safe exponential
  expect_equal(safe_exp(0), 1)
  expect_equal(safe_exp(1), exp(1))
  expect_equal(safe_exp(1000), exp(700))  # Bounded
  expect_equal(safe_exp(-1000), 0)
  expect_true(is.na(safe_exp(NA)))
  
  # Clamp function
  expect_equal(clamp(1.5, 0, 1), 1.0)
  expect_equal(clamp(-0.5, 0, 1), 0.0)
  expect_equal(clamp(0.3, 0, 1), 0.3)
  expect_equal(clamp(c(-1, 0.5, 2), 0, 1), c(0, 0.5, 1))
})

test_that("Parameter validation functions work correctly", {
  # check_numeric_value basic functionality
  expect_equal(check_numeric_value(5, "test"), 5)
  expect_equal(check_numeric_value(c(1, 2, 3), "test", allow_vector = TRUE), c(1, 2, 3))
  
  # Error cases
  expect_error(check_numeric_value(NULL, "test"), "cannot be NULL")
  expect_error(check_numeric_value("text", "test"), "must be numeric")
  expect_error(check_numeric_value(NA, "test"), "cannot contain NA")
  expect_error(check_numeric_value(c(1, 2), "test", allow_vector = FALSE), "must be a single value")
  expect_error(check_numeric_value(-5, "test", min_val = 0), "must be between")
  expect_error(check_numeric_value(15, "test", max_val = 10), "must be between")
  expect_error(check_numeric_value(1.5, "test", must_be_integer = TRUE), "must be integer")
})


