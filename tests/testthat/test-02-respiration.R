# =============================================================================
# test-02-respiration.R
# Tests para las funciones de respiración del modelo FB4.
# =============================================================================

resp_p <- processed_resp_params  # viene de helper-setup.R

# =============================================================================
# 1. Ecuaciones de temperatura de bajo nivel
# =============================================================================

test_that("respiration_temp_eq1 crece con la temperatura (exponencial)", {
  ft5  <- fb4package:::respiration_temp_eq1(5,  RQ = 0.07)
  ft15 <- fb4package:::respiration_temp_eq1(15, RQ = 0.07)
  expect_gt(ft15, ft5)
  expect_gt(ft5, 0)
})

test_that("respiration_temp_eq2 retorna valor mínimo cuando temp >= RTM", {
  ft <- suppressWarnings(
    fb4package:::respiration_temp_eq2(25, RTM = 25, RTO = 20, RX = 1)
  )
  expect_equal(ft, 0.000001)
})

test_that("respiration_temp_eq2 tiene máximo alrededor de RTO", {
  ft_opt   <- fb4package:::respiration_temp_eq2(20, RTM = 25, RTO = 20, RX = 2, warn = FALSE)
  ft_low   <- fb4package:::respiration_temp_eq2(10, RTM = 25, RTO = 20, RX = 2, warn = FALSE)
  ft_high  <- fb4package:::respiration_temp_eq2(23, RTM = 25, RTO = 20, RX = 2, warn = FALSE)

  expect_gt(ft_opt, ft_low)
  expect_gt(ft_opt, ft_high)
})

test_that("respiration_temp_eq2 siempre retorna >= 0.000001", {
  temps <- c(5, 10, 15, 20, 24.9)
  for (t in temps) {
    ft <- fb4package:::respiration_temp_eq2(t, RTM = 25, RTO = 20, RX = 2, warn = FALSE)
    expect_gte(ft, 0.000001, label = paste("temp =", t))
  }
})

# =============================================================================
# 2. calculate_respiration — interfaz principal
# =============================================================================

test_that("calculate_respiration retorna valor positivo en condiciones normales", {
  r <- calculate_respiration(10, 500, resp_p)
  expect_gt(r, 0)
  expect_lt(r, 1)   # g O2/g/día típicamente < 0.05 para salmonidos
})

test_that("calculate_respiration aumenta con la temperatura (REQ = 1)", {
  r_cold <- calculate_respiration(5,  500, resp_p)
  r_warm <- calculate_respiration(15, 500, resp_p)
  expect_gt(r_warm, r_cold)
})

test_that("calculate_respiration es mayor en peces más grandes (consumo total)", {
  r_small  <- calculate_respiration(10, 100,  resp_p)
  r_large  <- calculate_respiration(10, 1000, resp_p)

  total_small  <- r_small  * 100
  total_large  <- r_large  * 1000
  expect_gt(total_large, total_small)
})

test_that("calculate_respiration nunca retorna NA ni Inf", {
  temps <- c(2, 8, 15, 24)
  for (t in temps) {
    r <- calculate_respiration(t, 500, resp_p)
    expect_true(is.finite(r), label = paste("temp =", t))
    expect_gt(r, 0, label = paste("temp =", t))
  }
})

# =============================================================================
# 3. SDA (Specific Dynamic Action)
# =============================================================================

test_that("calculate_sda retorna valor positivo con energías válidas", {
  sda <- fb4package:::calculate_sda(
    consumption_energy = 1000,
    egestion_energy    = 200,
    SDA_coeff          = 0.172
  )
  # SDA = 0.172 * (1000 - 200) = 137.6
  expect_equal(sda, 137.6, tolerance = 1e-6)
})

test_that("calculate_sda retorna 0 cuando consumo es 0", {
  sda <- fb4package:::calculate_sda(
    consumption_energy = 0,
    egestion_energy    = 0,
    SDA_coeff          = 0.172
  )
  expect_equal(sda, 0)
})

test_that("calculate_sda no es negativo cuando egestion > consumption", {
  sda <- fb4package:::calculate_sda(
    consumption_energy = 100,
    egestion_energy    = 200,   # egestion > consumption (caso edge)
    SDA_coeff          = 0.172
  )
  expect_gte(sda, 0)
})

# =============================================================================
# 4. convert_respiration_to_energy
# =============================================================================

test_that("convert_respiration_to_energy escala linealmente con oxycal", {
  e1 <- fb4package:::convert_respiration_to_energy(0.01, oxycal = 13560)
  e2 <- fb4package:::convert_respiration_to_energy(0.01, oxycal = 27120)
  expect_equal(e2, 2 * e1, tolerance = 1e-9)
})

test_that("convert_respiration_to_energy retorna 0 cuando respiración es 0", {
  e <- fb4package:::convert_respiration_to_energy(0, oxycal = 13560)
  expect_equal(e, 0)
})
