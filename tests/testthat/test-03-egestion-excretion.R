# =============================================================================
# test-03-egestion-excretion.R
# Tests para las funciones de egestion y excreción del modelo FB4.
# =============================================================================

ege_p <- processed_ege_params   # EGEQ = 1 (Chinook demo)
exc_p <- processed_exc_params   # EXEQ = 1 (Chinook demo)

# Consumo de referencia en J/g (valor plausible para 1 día)
C_ref  <- 100   # J/g
T_ref  <- 10    # °C
p_ref  <- 0.5

# =============================================================================
# 1. Modelos de egestion (bajo nivel)
# =============================================================================

test_that("egestion_model_1 es fracción constante del consumo", {
  e <- fb4package:::egestion_model_1(consumption = 1000, FA = 0.212)
  expect_equal(e, 212, tolerance = 1e-9)
})

test_that("egestion_model_1 nunca excede el consumo (cap en consumo)", {
  # FA > 1 no debería ocurrir biológicamente, pero la función lo maneja
  e <- fb4package:::egestion_model_1(consumption = 100, FA = 1.5)
  expect_lte(e, 100)
})

test_that("egestion_model_1 es 0 cuando consumo es 0", {
  expect_equal(fb4package:::egestion_model_1(0, FA = 0.212), 0)
})

test_that("egestion_model_2 escala con el consumo", {
  e1 <- fb4package:::egestion_model_2(100,  T_ref, p_ref, FA = 0.212, FB = -0.222, FG = 0.631)
  e2 <- fb4package:::egestion_model_2(200,  T_ref, p_ref, FA = 0.212, FB = -0.222, FG = 0.631)
  expect_equal(e2 / e1, 2, tolerance = 1e-6)
})

test_that("egestion_model_2 no excede el consumo", {
  e <- fb4package:::egestion_model_2(C_ref, T_ref, p_ref, FA = 0.212, FB = -0.222, FG = 0.631)
  expect_lte(e, C_ref)
  expect_gte(e, 0)
})

test_that("egestion_model_3 retorna fracción entre 0 y consumo", {
  e <- fb4package:::egestion_model_3(C_ref, T_ref, p_ref,
                                      FA = 0.212, FB = -0.222, FG = 0.631,
                                      indigestible_fraction = 0.1)
  expect_gte(e, 0)
  expect_lte(e, C_ref)
})

# =============================================================================
# 2. calculate_egestion — interfaz principal
# =============================================================================

test_that("calculate_egestion retorna 0 cuando consumo es 0", {
  e <- calculate_egestion(0, T_ref, p_ref, ege_p)
  expect_equal(e, 0)
})

test_that("calculate_egestion retorna valor positivo con consumo > 0", {
  e <- calculate_egestion(C_ref, T_ref, p_ref, ege_p)
  expect_gt(e, 0)
  expect_lt(e, C_ref)   # egestion siempre < consumo
})

test_that("calculate_egestion escala con el consumo (EGEQ = 1)", {
  e1 <- calculate_egestion(C_ref,     T_ref, p_ref, ege_p)
  e2 <- calculate_egestion(2 * C_ref, T_ref, p_ref, ege_p)
  expect_equal(e2, 2 * e1, tolerance = 1e-9)
})

# =============================================================================
# 3. Modelos de excreción (bajo nivel)
# =============================================================================

test_that("excretion_model_1 es fracción del asimilado", {
  cons <- 1000; ege <- 200
  u <- fb4package:::excretion_model_1(cons, ege, UA = 0.0314)
  expect_equal(u, 0.0314 * (cons - ege), tolerance = 1e-9)
})

test_that("excretion_model_1 retorna 0 cuando consumo = 0", {
  expect_equal(fb4package:::excretion_model_1(0, 0, UA = 0.0314), 0)
})

test_that("excretion_model_2 es positivo en condiciones normales", {
  cons <- 1000; ege <- 200
  u <- fb4package:::excretion_model_2(cons, ege, T_ref, p_ref,
                                       UA = 0.0314, UB = 0.58, UG = -0.299)
  expect_gt(u, 0)
})

# =============================================================================
# 4. calculate_excretion — interfaz principal
# =============================================================================

test_that("calculate_excretion retorna 0 cuando consumo es 0", {
  u <- calculate_excretion(0, 0, T_ref, p_ref, exc_p)
  expect_equal(u, 0)
})

test_that("calculate_excretion retorna valor positivo con consumo > 0", {
  egestion <- calculate_egestion(C_ref, T_ref, p_ref, ege_p)
  u <- calculate_excretion(C_ref, egestion, T_ref, p_ref, exc_p)
  expect_gt(u, 0)
  expect_lt(u, C_ref)   # excreción < consumo
})

test_that("balance: egestion + excretion < consumo (neto > 0)", {
  e <- calculate_egestion(C_ref, T_ref, p_ref, ege_p)
  u <- calculate_excretion(C_ref, e, T_ref, p_ref, exc_p)

  expect_lt(e + u, C_ref)
})
