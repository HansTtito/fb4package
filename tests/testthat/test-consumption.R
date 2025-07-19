# ============================================================================
# CONSUMPTION FUNCTIONS TESTS
# ============================================================================

test_that("Temperature functions for consumption work correctly", {
  # Equation 1 - Simple exponential
  temp_factor_1 <- consumption_temp_eq1(15, 0.1)
  expect_gt(temp_factor_1, 0)
  expect_true(is.finite(temp_factor_1))
  
  # Higher temperature should give higher factor (positive CQ)
  temp_factor_1_high <- consumption_temp_eq1(20, 0.1)
  expect_gt(temp_factor_1_high, temp_factor_1)
  
  # Equation 2 - Kitchell et al.
  temp_factor_2 <- consumption_temp_eq2(15, CTM = 25, CTO = 20, CX = 1.5)
  expect_gte(temp_factor_2, 0)
  expect_true(is.finite(temp_factor_2))
  
  # Temperature at or above CTM should return 0
  expect_equal(consumption_temp_eq2(25, CTM = 25, CTO = 20, CX = 1.5), 0)
  expect_equal(consumption_temp_eq2(30, CTM = 25, CTO = 20, CX = 1.5), 0)
  
  # Equation 3 - Thornton and Lessem
  temp_factor_3 <- consumption_temp_eq3(15, CQ = 20, CG1 = 0.1, CK1 = 0.1, 
                                        CG2 = 0.1, CTL = 30, CK4 = 0.1)
  expect_gte(temp_factor_3, 0)
  expect_true(is.finite(temp_factor_3))
  
  # Equation 4 - Polynomial
  temp_factor_4 <- consumption_temp_eq4(15, CQ = 0.1, CK1 = -0.01, CK4 = 0.001)
  expect_gt(temp_factor_4, 0)
  expect_true(is.finite(temp_factor_4))
})

test_that("Consumption calculation works correctly", {
  # Create test parameters
  consumption_params <- list(
    CEQ = 1,
    CA = 0.303,
    CB = 0.275,
    CQ = 0.1
  )
  
  # Basic calculation
  consumption_rate <- calculate_consumption(
    temperature = 15,
    weight = 100,
    p_value = 0.5,
    consumption_params = consumption_params,
    method = "rate"
  )
  
  expect_gt(consumption_rate, 0)
  expect_true(is.finite(consumption_rate))
  
  # Maximum consumption should be higher than rate consumption
  consumption_max <- calculate_consumption(
    temperature = 15,
    weight = 100,
    p_value = 0.5,
    consumption_params = consumption_params,
    method = "maximum"
  )
  
  expect_gt(consumption_max, consumption_rate)
  
  # Zero p-value should give zero consumption (rate method)
  consumption_zero <- calculate_consumption(
    temperature = 15,
    weight = 100,
    p_value = 0,
    consumption_params = consumption_params,
    method = "rate"
  )
  
  expect_equal(consumption_zero, 0)
  
  # Higher weight should generally give higher consumption
  consumption_heavy <- calculate_consumption(
    temperature = 15,
    weight = 200,
    p_value = 0.5,
    consumption_params = consumption_params,
    method = "rate"
  )
  
  expect_gt(consumption_heavy, consumption_rate)
})

test_that("Consumption parameter calculations work", {
  # Test equation 2 parameter calculation
  params_eq2 <- calculate_consumption_params_eq2(CQ = 2, CTM = 25, CTO = 15)
  
  expect_true(is.list(params_eq2))
  expect_true(all(c("CY", "CZ", "CX") %in% names(params_eq2)))
  expect_true(all(sapply(params_eq2, is.finite)))
  
  # Test invalid inputs
  expect_error(calculate_consumption_params_eq2(CQ = NA, CTM = 25, CTO = 15), 
               "must be valid")
  expect_error(calculate_consumption_params_eq2(CQ = 2, CTM = 15, CTO = 20), 
               "CTM must be greater than CTO")
  expect_error(calculate_consumption_params_eq2(CQ = -1, CTM = 25, CTO = 15), 
               "CQ must be positive")
  
  # Test equation 3 parameter calculation
  params_eq3 <- calculate_consumption_params_eq3(
    CTO = 20, CQ = 15, CTL = 30, CTM = 25, CK1 = 0.1, CK4 = 0.1
  )
  
  expect_true(is.list(params_eq3))
  expect_true(all(c("CG1", "CG2") %in% names(params_eq3)))
  expect_true(all(sapply(params_eq3, is.finite)))
})

