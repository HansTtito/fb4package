# ============================================================================
# DATA PROCESSING TESTS
# ============================================================================

test_that("Time series interpolation works correctly", {
  # Create test data with gaps
  test_data <- data.frame(
    Day = c(1, 3, 5, 10),
    Temperature = c(10, 15, 20, 25),
    Oxygen = c(8, 7, 6, 5)
  )
  
  target_days <- 1:10
  interpolated <- interpolate_time_series(
    data = test_data,
    value_columns = c("Temperature", "Oxygen"),
    target_days = target_days,
    method = "linear"
  )
  
  expect_equal(nrow(interpolated), 10)
  expect_equal(interpolated$Temperature[1], 10)
  expect_equal(interpolated$Temperature[10], 25)
  expect_true(all(interpolated$Temperature[2] > 10 & interpolated$Temperature[2] < 15))
  
  # Test constant method
  interpolated_const <- interpolate_time_series(
    data = test_data,
    value_columns = c("Temperature"),
    target_days = target_days,
    method = "constant"
  )
  
  expect_equal(nrow(interpolated_const), 10)
  expect_equal(interpolated_const$Temperature[2], 10)  # Should hold constant
  
  # Test with missing data
  test_data_na <- test_data
  test_data_na$Temperature[2] <- NA
  
  interpolated_na <- interpolate_time_series(
    data = test_data_na,
    value_columns = c("Temperature"),
    target_days = target_days,
    method = "linear"
  )
  
  expect_equal(nrow(interpolated_na), 10)
  expect_false(any(is.na(interpolated_na$Temperature)))
})

test_that("Species equation validation works", {
  # Valid consumption parameters
  # Parámetros completos para test
  valid_consumption <- list(
    CEQ = 1,
    CA = 0.303,
    CB = -0.275,
    CQ = 0.1,
    CTO = 15,    # Temperatura óptima
    CTM = 25,    # Temperatura máxima
    CTL = 28     # Temperatura letal
  )
  
  valid_respiration <- list(
    REQ = 1,
    RA = 0.05,
    RB = 0.5,
    RQ = 0.1,
    RTO = 10,    # Temperatura óptima para respiración
    RTM = 20,    # Temperatura máxima para respiración  
    RTL = 25,    # Temperatura letal para respiración
    RK1 = 1,     # Coeficiente K1
    RK4 = 0.13,  # Coeficiente K4
    RK5 = 0.1    # Coeficiente K5
  )
  
  valid_activity <- list(
    ACT = 1,     # Multiplicador de actividad
    BACT = 0     # Coeficiente de actividad dependiente del peso
  )
  
  valid_sda <- list(
    SDA = 0.172  # Coeficiente de acción dinámica específica
  )
  
  valid_egestion <- list(
    EGEQ = 1,
    FA = 0.2,
    FB = -0.222,  # Parámetro adicional común
    FG = 0.631    # Parámetro adicional común
  )
  
  valid_excretion <- list(
    EXEQ = 1,
    UA = 0.1,
    UB = 0.123,   # Parámetro adicional común
    UG = -0.2     # Parámetro adicional común
  )
  
  valid_predator <- list(
    PREDEDEQ = 1,
    ED_ini = 4500,
    ED_end = 4500,
    Alpha1 = 1,   # Parámetros de densidad energética
    Beta1 = 0,
    Alpha2 = 1,
    Beta2 = 0,
    Cutoff = 999  # Punto de corte para cambio de ecuación
  )
  
  # Parámetros completos
  valid_species_params <- list(
    consumption = valid_consumption,
    respiration = valid_respiration,
    activity = valid_activity,      # ← Categoría faltante
    sda = valid_sda,               # ← Categoría faltante
    egestion = valid_egestion,
    excretion = valid_excretion,
    predator = valid_predator
  )
  
  # Ahora el test debería pasar
  validation_result <- validate_species_equations(valid_species_params)
  expect_true(validation_result$valid)
  expect_equal(length(validation_result$errors), 0)
  
  # Missing required parameters
  invalid_consumption <- list(
    CEQ = 1,
    CA = 0.303
    # Missing CB, CQ
  )
  
  invalid_species_params <- list(
    consumption = invalid_consumption,
    respiration = valid_species_params$respiration,
    egestion = valid_species_params$egestion,
    excretion = valid_species_params$excretion,
    predator = valid_species_params$predator
  )
  
  validation_invalid <- validate_species_equations(invalid_species_params)
  expect_false(validation_invalid$valid)
  expect_gt(length(validation_invalid$errors), 0)
  
  # Missing entire category
  incomplete_params <- list(
    consumption = valid_consumption
    # Missing other categories
  )
  
  validation_incomplete <- validate_species_equations(incomplete_params)
  expect_false(validation_incomplete$valid)
})

