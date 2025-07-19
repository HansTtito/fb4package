# ============================================================================
# PREDATOR ENERGY DENSITY TESTS
# ============================================================================

test_that("Predator energy density equations work", {
  # Equation 1 - Interpolated data
  energy_data <- c(4500, 4600, 4700, 4800, 4900)
  ed_1 <- predator_energy_eq1(weight = 100, day = 3, energy_data = energy_data)
  expect_equal(ed_1, 4700)
  
  # Out of bounds should be clamped
  ed_1_high <- predator_energy_eq1(weight = 100, day = 10, energy_data = energy_data)
  expect_equal(ed_1_high, 4900)  # Last value
  
  # Equation 2 - Linear piecewise
  ed_2_low <- predator_energy_eq2(
    weight = 50,
    Alpha1 = 3000,
    Beta1 = 20,
    Alpha2 = 4000,
    Beta2 = 5,
    Cutoff = 100
  )
  expect_equal(ed_2_low, 4000)  # 3000 + 20*50
  expect_gte(ed_2_low, 1000)
  expect_lte(ed_2_low, 15000)
  
  ed_2_high <- predator_energy_eq2(
    weight = 150,
    Alpha1 = 3000,
    Beta1 = 20,
    Alpha2 = 4000,
    Beta2 = 5,
    Cutoff = 100
  )
  expect_equal(ed_2_high, 4750)  # 4000 + 5*150
  
  # Equation 3 - Power function
  ed_3 <- predator_energy_eq3(
    weight = 100,
    Alpha1 = 5000,
    Beta1 = 0.1
  )
  expected_ed3 <- 5000 * (100^0.1)
  expect_equal(ed_3, expected_ed3)
  expect_gte(ed_3, 1000)
  expect_lte(ed_3, 15000)
})

test_that("Predator energy density main function works", {
  # Test equation 1
  predator_params_1 <- list(
    PREDEDEQ = 1,
    ED_ini = 4000,
    ED_end = 5000,
    duration = 100
  )
  
  ed_day1 <- calculate_predator_energy_density(
    weight = 100,
    day = 1,
    predator_params = predator_params_1
  )
  expect_equal(ed_day1, 4000)
  
  ed_day100 <- calculate_predator_energy_density(
    weight = 100,
    day = 100,
    predator_params = predator_params_1
  )
  expect_equal(ed_day100, 5000)
  
  # Test equation 2
  predator_params_2 <- list(
    PREDEDEQ = 2,
    Alpha1 = 3000,
    Beta1 = 20,
    Alpha2 = 4000,
    Beta2 = 5,
    Cutoff = 100
  )
  
  ed_2 <- calculate_predator_energy_density(
    weight = 50,
    day = 1,
    predator_params = predator_params_2
  )
  expect_equal(ed_2, 4000)  # 3000 + 20*50
  
  # Test equation 3
  predator_params_3 <- list(
    PREDEDEQ = 3,
    Alpha1 = 5000,
    Beta1 = 0.1
  )
  
  ed_3 <- calculate_predator_energy_density(
    weight = 100,
    day = 1,
    predator_params = predator_params_3
  )
  expected_ed3 <- 5000 * (100^0.1)
  expect_equal(ed_3, expected_ed3)
})

test_that("Weight solving functions work", {
  # Test power function weight solving
  final_weight <- solve_weight_power_function(
    initial_weight = 100,
    net_energy = 10000,
    Alpha1 = 5000,
    Beta1 = 0.1
  )
  
  expect_gt(final_weight, 100)  # Should grow with positive energy
  expect_true(is.finite(final_weight))
  expect_gt(final_weight, 0.001)  # Above minimum
  
  # Test with negative energy (weight loss)
  final_weight_loss <- solve_weight_power_function(
    initial_weight = 100,
    net_energy = -50000,
    Alpha1 = 5000,
    Beta1 = 0.1
  )
  
  expect_lt(final_weight_loss, 100)  # Should lose weight
  expect_gte(final_weight_loss, 0.001)  # But not below minimum
})

test_that("Final weight calculation works", {
  predator_params <- list(
    PREDEDEQ = 1,
    ED_ini = 4500,
    ED_end = 4500,
    duration = 365
  )
  
  weight_result <- calculate_final_weight_fb4(
    initial_weight = 100,
    net_energy = 10000,
    spawn_energy = 0,
    predator_params = predator_params,
    day = 1
  )
  
  expect_true(is.list(weight_result))
  expect_true(all(c("final_weight", "final_energy_density", "weight_change") %in% names(weight_result)))
  expect_gt(weight_result$final_weight, 100)  # Should grow
  expect_equal(weight_result$weight_change, weight_result$final_weight - 100)
  
  # Test with spawning energy
  weight_result_spawn <- calculate_final_weight_fb4(
    initial_weight = 100,
    net_energy = 10000,
    spawn_energy = 5000,
    predator_params = predator_params,
    day = 1
  )
  
  expect_lt(weight_result_spawn$final_weight, weight_result$final_weight)  # Less growth due to spawning
})

