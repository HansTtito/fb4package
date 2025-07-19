# ============================================================================
# RESPIRATION FUNCTIONS TESTS
# ============================================================================

test_that("Respiration temperature functions work correctly", {
  # Equation 1
  resp_temp_1 <- respiration_temp_eq1(15, RQ = 0.1)
  expect_gte(resp_temp_1, 0.001)  # Minimum value
  expect_true(is.finite(resp_temp_1))
  
  # Higher temperature should give higher factor (positive RQ)
  resp_temp_1_high <- respiration_temp_eq1(20, RQ = 0.1)
  expect_gt(resp_temp_1_high, resp_temp_1)
  
  # Equation 2
  resp_temp_2 <- respiration_temp_eq2(15, RTM = 25, RTO = 20, RX = 1.5)
  expect_gte(resp_temp_2, 0.001)
  expect_true(is.finite(resp_temp_2))
  
  # Temperature at or above RTM should return minimum
  expect_equal(respiration_temp_eq2(25, RTM = 25, RTO = 20, RX = 1.5), 0.001)
})

test_that("Activity factor calculation works", {
  activity_factor <- calculate_activity_factor(
    weight = 100,
    temperature = 15,
    RTL = 20,
    ACT = 1.5,
    RK4 = -0.2,
    BACT = 0.05,
    RK1 = 2.0,
    RK5 = 0.08,
    RTO = 0.1
  )
  
  expect_gte(activity_factor, 1.0)  # Minimum of 1.0
  expect_true(is.finite(activity_factor))
})

test_that("Respiration calculation works correctly", {
  # Create test parameters
  respiration_params <- list(
    REQ = 1,
    RA = 0.0548,
    RB = 0.8,
    RQ = 0.1,
    RTL = 20,
    RK4 = -0.2,
    RK1 = 2.0,
    RK5 = 0.08,
    RTO = 0.1
  )
  
  activity_params <- list(
    ACT = 1.5,
    BACT = 0.05
  )
  
  respiration_rate <- calculate_respiration(
    temperature = 15,
    weight = 100,
    respiration_params = respiration_params,
    activity_params = activity_params
  )
  
  expect_gt(respiration_rate, 0)
  expect_true(is.finite(respiration_rate))
  
  # Higher weight should generally give higher respiration
  respiration_heavy <- calculate_respiration(
    temperature = 15,
    weight = 200,
    respiration_params = respiration_params,
    activity_params = activity_params
  )
  
  expect_gt(respiration_heavy, respiration_rate)
})

test_that("SDA calculation works correctly", {
  sda_energy <- calculate_sda(
    consumption_energy = 1000,
    egestion_energy = 200,
    SDA_coeff = 0.1
  )
  
  expect_equal(sda_energy, 80)  # 0.1 * (1000 - 200)
  expect_gte(sda_energy, 0)
  
  # Edge cases
  expect_equal(calculate_sda(0, 0, 0.1), 0)
  expect_equal(calculate_sda(1000, 1000, 0.1), 0)  # No assimilated energy
  
  # Egestion > consumption should be corrected
  sda_corrected <- calculate_sda(1000, 1200, 0.1)
  expect_equal(sda_corrected, 0)
  
  # Invalid inputs
  expect_error(calculate_sda(-100, 0, 0.1), "must be between")
  expect_error(calculate_sda(1000, 200, 1.5), "must be between")
})

test_that("Respiration energy conversion works", {
  resp_energy <- convert_respiration_to_energy(0.1, oxycal = 13560)
  expect_equal(resp_energy, 1356)
  
  # Different oxycal values
  resp_energy_alt <- convert_respiration_to_energy(0.1, oxycal = 14000)
  expect_equal(resp_energy_alt, 1400)
  
  # Invalid inputs
  expect_error(convert_respiration_to_energy(-0.1), "must be between")
  expect_error(convert_respiration_to_energy(0.1, oxycal = 500), "must be between")
})

