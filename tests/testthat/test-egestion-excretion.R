# ============================================================================
# EGESTION AND EXCRETION TESTS
# ============================================================================

test_that("Egestion models work correctly", {
  # Model 1 - Basic
  egestion_1 <- egestion_model_1(consumption = 1000, FA = 0.2)
  expect_equal(egestion_1, 200)
  expect_gte(egestion_1, 0)
  expect_lte(egestion_1, 1000)  # Cannot exceed consumption
  
  # Model 2 - Elliott
  egestion_2 <- egestion_model_2(
    consumption = 1000,
    temperature = 15,
    p_value = 0.5,
    FA = 0.158,
    FB = -0.222,
    FG = 0.631
  )
  expect_gt(egestion_2, 0)
  expect_lte(egestion_2, 1000)
  
  # Model 3 - Stewart et al.
  egestion_3 <- egestion_model_3(
    consumption = 1000,
    temperature = 15,
    p_value = 0.5,
    FA = 0.212,
    FB = -0.222,
    FG = 0.631,
    indigestible_fraction = 0.1
  )
  expect_gt(egestion_3, 0)
  expect_lte(egestion_3, 1000)
  
  # Model 4 - Without p-value
  egestion_4 <- egestion_model_4(
    consumption = 1000,
    temperature = 15,
    FA = 0.212,
    FB = -0.222
  )
  expect_gt(egestion_4, 0)
  expect_lte(egestion_4, 1000)
})

test_that("Excretion models work correctly", {
  # Model 1 - Basic
  excretion_1 <- excretion_model_1(
    consumption = 1000,
    egestion = 200,
    UA = 0.1
  )
  expect_equal(excretion_1, 80)  # 0.1 * (1000 - 200)
  expect_gte(excretion_1, 0)
  expect_lte(excretion_1, 800)  # Cannot exceed assimilated
  
  # Model 2 - With temperature and feeding dependence
  excretion_2 <- excretion_model_2(
    consumption = 1000,
    egestion = 200,
    temperature = 15,
    p_value = 0.5,
    UA = 0.0314,
    UB = 0.58,
    UG = -0.299
  )
  expect_gt(excretion_2, 0)
  expect_lte(excretion_2, 800)
  
  # Zero consumption should give zero excretion
  excretion_zero <- excretion_model_2(
    consumption = 0,
    egestion = 0,
    temperature = 15,
    p_value = 0.5,
    UA = 0.0314,
    UB = 0.58,
    UG = -0.299
  )
  expect_equal(excretion_zero, 0)
})

test_that("Main egestion and excretion functions work", {
  # Egestion calculation
  egestion_params <- list(
    EGEQ = 1,
    FA = 0.2
  )
  
  egestion_result <- calculate_egestion(
    consumption = 1000,
    temperature = 15,
    p_value = 0.5,
    egestion_params = egestion_params
  )
  
  expect_gt(egestion_result, 0)
  expect_lte(egestion_result, 1000)
  
  # Excretion calculation
  excretion_params <- list(
    EXEQ = 1,
    UA = 0.1
  )
  
  excretion_result <- calculate_excretion(
    consumption = 1000,
    egestion = egestion_result,
    temperature = 15,
    p_value = 0.5,
    excretion_params = excretion_params
  )
  
  expect_gt(excretion_result, 0)
  expect_lte(excretion_result, 1000 - egestion_result)
  
  # Zero consumption
  expect_equal(calculate_egestion(0, 15, 0.5, egestion_params), 0)
  expect_equal(calculate_excretion(0, 0, 15, 0.5, excretion_params), 0)
})

