# =============================================================================
# test-01-consumption.R
# Tests para las funciones de consumo del modelo FB4.
# =============================================================================

# -----------------------------------------------------------------------------
# Helpers locales
# -----------------------------------------------------------------------------

# Parámetros de consumo procesados de Chinook (CEQ = 2)
cons_p <- processed_cons_params   # viene de helper-setup.R

# Parámetros mínimos para ecuación 1 (solo CQ)
cons_p_eq1 <- list(CEQ = 1, CA = 0.303, CB = -0.275, CQ = 0.07)

# =============================================================================
# 1. Ecuaciones de temperatura de bajo nivel
# =============================================================================

test_that("consumption_temp_eq1 crece con la temperatura (exponencial)", {
  ft8  <- fb4package:::consumption_temp_eq1(8,  CQ = 0.07)
  ft15 <- fb4package:::consumption_temp_eq1(15, CQ = 0.07)
  ft20 <- fb4package:::consumption_temp_eq1(20, CQ = 0.07)

  expect_gt(ft15, ft8)
  expect_gt(ft20, ft15)
  expect_gt(ft8, 0)
})

test_that("consumption_temp_eq2 retorna 0 cuando temperature >= CTM", {
  # Temperatura justo en CTM
  expect_equal(
    suppressWarnings(fb4package:::consumption_temp_eq2(20, CTM = 20, CTO = 15, CX = 1)),
    0
  )
  # Temperatura por encima de CTM
  expect_equal(
    suppressWarnings(fb4package:::consumption_temp_eq2(22, CTM = 20, CTO = 15, CX = 1)),
    0
  )
})

test_that("consumption_temp_eq2 tiene máximo en CTO", {
  # El factor temperatura debe ser ~1 en CTO y decaer hacia CTM
  ft_opt   <- fb4package:::consumption_temp_eq2(15, CTM = 20, CTO = 15, CX = 1, warn = FALSE)
  ft_below <- fb4package:::consumption_temp_eq2(10, CTM = 20, CTO = 15, CX = 1, warn = FALSE)
  ft_above <- fb4package:::consumption_temp_eq2(18, CTM = 20, CTO = 15, CX = 1, warn = FALSE)

  expect_gte(ft_opt, ft_below)
  expect_gte(ft_opt, ft_above)
  expect_gte(ft_opt, 0)
})

test_that("consumption_temp_eq2 retorna valor positivo en rango biológico", {
  ft <- fb4package:::consumption_temp_eq2(12, CTM = 20, CTO = 15, CX = 1, warn = FALSE)
  expect_gt(ft, 0)
  expect_lte(ft, 2)   # no debería ser un valor absurdo
})

test_that("consumption_temp_eq4 produce resultado no-negativo", {
  ft <- fb4package:::consumption_temp_eq4(10, CQ = 0.06, CK1 = -0.001, CK4 = 0.0)
  expect_gte(ft, 0)
})

# =============================================================================
# 2. calculate_consumption — interfaz principal
# =============================================================================

test_that("calculate_consumption retorna valor positivo con p_value = 0.5", {
  c_rate <- calculate_consumption(
    temperature = 10, weight = 500, p_value = 0.5,
    processed_consumption_params = cons_p
  )
  expect_gt(c_rate, 0)
  expect_lt(c_rate, 1)  # consumo específico g/g/día típicamente < 1
})

test_that("calculate_consumption escala linealmente con p_value (method = 'rate')", {
  c_half <- calculate_consumption(10, 500, p_value = 0.5, processed_consumption_params = cons_p)
  c_full <- calculate_consumption(10, 500, p_value = 1.0, processed_consumption_params = cons_p)

  expect_equal(c_full, 2 * c_half, tolerance = 1e-9)
})

test_that("calculate_consumption aumenta con el peso (alometría negativa → gg decrece, pero consumo total sube)", {
  # Consumo absoluto = c_gg * peso; consumo específico puede bajar con el peso
  # pero el consumo total debe subir
  cgg_small <- calculate_consumption(10, 100,  p_value = 0.5, processed_consumption_params = cons_p)
  cgg_large <- calculate_consumption(10, 1000, p_value = 0.5, processed_consumption_params = cons_p)

  cons_total_small <- cgg_small * 100
  cons_total_large <- cgg_large * 1000

  expect_gt(cons_total_large, cons_total_small)
})

test_that("calculate_consumption retorna 0 cuando p_value = 0", {
  c_zero <- calculate_consumption(10, 500, p_value = 0, processed_consumption_params = cons_p)
  expect_equal(c_zero, 0, tolerance = 1e-12)
})

test_that("calculate_consumption method='maximum' ignora p_value", {
  cmax1 <- calculate_consumption(10, 500, p_value = 0.3,
                                  processed_consumption_params = cons_p, method = "maximum")
  cmax2 <- calculate_consumption(10, 500, p_value = 0.9,
                                  processed_consumption_params = cons_p, method = "maximum")
  expect_equal(cmax1, cmax2)
})

test_that("calculate_consumption es 0 cerca de CTM (temp alta)", {
  # A temperatura = CTM el factor temperatura debe ser 0
  cons_near_ctm <- suppressWarnings(
    calculate_consumption(20, 500, p_value = 0.5, processed_consumption_params = cons_p)
  )
  expect_equal(cons_near_ctm, 0)
})

# =============================================================================
# 3. Cálculo de parámetros derivados (eq 2)
# =============================================================================

test_that("calculate_consumption_params_eq2 produce lista con CY, CZ, CX", {
  params <- fb4package:::calculate_consumption_params_eq2(CQ = 3.0, CTM = 20, CTO = 15)

  expect_named(params, c("CY", "CZ", "CX"))
  expect_gt(params$CX, 0)
})

test_that("calculate_consumption_params_eq2 maneja CY = 0 sin error (CQ = 1)", {
  # log(CQ=1) = 0 → CY = 0 → fallback CX = 1
  params <- suppressWarnings(
    fb4package:::calculate_consumption_params_eq2(CQ = 1.0, CTM = 20, CTO = 15)
  )
  expect_equal(params$CX, 1)
})
