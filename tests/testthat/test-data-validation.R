# ============================================================================
# DATA VALIDATION TESTS
# ============================================================================

test_that("Time series data validation works correctly", {
  # Valid data
  valid_data <- data.frame(Day = 1:10, Temperature = 15:24)
  expect_silent(validate_time_series_data(valid_data, "temperature", c("Day", "Temperature")))
  
  # Error cases
  expect_error(validate_time_series_data(NULL, "test"), "cannot be NULL")
  expect_error(validate_time_series_data(list(), "test"), "must be a data.frame")
  expect_error(validate_time_series_data(data.frame(), "test"), "cannot be empty")
  
  # Missing columns
  missing_col_data <- data.frame(Day = 1:5)
  expect_error(validate_time_series_data(missing_col_data, "test", c("Day", "Temperature")), 
               "Missing columns")
  
  # Invalid Day column
  invalid_day_data <- data.frame(Day = c("1", "2", "3"), Temperature = c(15, 16, 17))
  expect_error(validate_time_series_data(invalid_day_data, "test"), "must be numeric")
  
  # Warning cases
  duplicate_day_data <- data.frame(Day = c(1, 1, 2), Temperature = c(15, 16, 17))
  expect_warning(validate_time_series_data(duplicate_day_data, "test"), "duplicate values")
  
  unordered_data <- data.frame(Day = c(3, 1, 2), Temperature = c(17, 15, 16))
  expect_warning(validate_time_series_data(unordered_data, "test"), "not in ascending order")
})

test_that("Basic model parameters validation works", {
  # Valid parameters
  expect_silent(validate_basic_params(10.5, 365))
  expect_silent(validate_basic_params(1.0, 100))
  
  # Invalid parameters
  expect_error(validate_basic_params(-5, 100), "must be a positive number")
  expect_error(validate_basic_params(10, -50), "must be a positive number")
  expect_error(validate_basic_params("text", 100), "must be a positive number")
  
  # Warning for very long duration
  expect_warning(validate_basic_params(10, 15000), "Very long duration")
})

test_that("Diet consistency validation works", {
  # Valid data
  diet_data <- data.frame(Day = 1:5, fish = 0.6, zooplankton = 0.4)
  energy_data <- data.frame(Day = 1:5, fish = 4000, zooplankton = 2500)
  expect_silent(validate_diet_consistency(diet_data, energy_data))
  
  # Missing prey columns
  expect_error(validate_diet_consistency(data.frame(Day = 1:5), energy_data), 
               "must have at least one prey column")
  
  # Mismatched prey columns
  mismatched_energy <- data.frame(Day = 1:5, fish = 4000, invertebrates = 3000)
  expect_error(validate_diet_consistency(diet_data, mismatched_energy), 
               "Prey columns must be identical")
  
  # Invalid diet proportions
  negative_diet <- data.frame(Day = 1:5, fish = -0.1, zooplankton = 1.1)
  expect_error(validate_diet_consistency(negative_diet, energy_data), 
               "cannot be negative")
  
  # Invalid prey energies
  zero_energy <- data.frame(Day = 1:5, fish = 0, zooplankton = 2500)
  expect_error(validate_diet_consistency(diet_data, zero_energy), 
               "must be positive")
  
  # Warning for poor diet balance
  unbalanced_diet <- data.frame(Day = 1:5, fish = 0.3, zooplankton = 0.3)
  expect_warning(validate_diet_consistency(unbalanced_diet, energy_data), 
                 "deviate significantly from 1.0")
})

