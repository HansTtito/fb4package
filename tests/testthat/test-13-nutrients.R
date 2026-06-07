# =============================================================================
# test-13-nutrients.R
# Tests para el sub-modelo de nutrientes (N y P) en el loop de simulación.
# Usa fixtures de helper-setup.R (bio_chinook_30, make_bio_chinook).
# =============================================================================

# ---------------------------------------------------------------------------
# Fixture: objeto Bioenergetic con nutrient_data
# ---------------------------------------------------------------------------
make_bio_nutrients <- function(n_days = 30) {
  bio <- make_bio_chinook(n_days)

  bio$nutrient_data <- list(
    prey_n_concentrations = data.frame(
      Day     = c(1, n_days),
      Alewife = c(0.025, 0.025),
      Shrimp  = c(0.018, 0.018)
    ),
    prey_p_concentrations = data.frame(
      Day     = c(1, n_days),
      Alewife = c(0.004, 0.004),
      Shrimp  = c(0.003, 0.003)
    ),
    predator_n_concentration = data.frame(
      Day   = c(1, n_days),
      value = c(0.030, 0.030)
    ),
    predator_p_concentration = data.frame(
      Day   = c(1, n_days),
      value = c(0.004, 0.004)
    ),
    n_assimilation_efficiency = data.frame(
      Day     = c(1, n_days),
      Alewife = c(0.80, 0.80),
      Shrimp  = c(0.75, 0.75)
    ),
    p_assimilation_efficiency = data.frame(
      Day     = c(1, n_days),
      Alewife = c(0.60, 0.60),
      Shrimp  = c(0.55, 0.55)
    )
  )
  bio$calc_nutrients <- TRUE
  bio
}

# =============================================================================
# 1. Columnas en daily_output
# =============================================================================

test_that("daily_output contiene columnas de nitrógeno cuando se pasa nutrient_data", {
  bio <- make_bio_nutrients()
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  n_cols <- c("Nitrogen_consumed_g", "Nitrogen_growth_g",
              "Nitrogen_excretion_g", "Nitrogen_egestion_g")
  expect_true(all(n_cols %in% names(do)),
              info = paste("Columnas faltantes:", paste(setdiff(n_cols, names(do)), collapse = ", ")))
})

test_that("daily_output contiene columnas de fósforo cuando se pasa nutrient_data", {
  bio <- make_bio_nutrients()
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  p_cols <- c("Phosphorus_consumed_g", "Phosphorus_growth_g",
              "Phosphorus_excretion_g", "Phosphorus_egestion_g")
  expect_true(all(p_cols %in% names(do)),
              info = paste("Columnas faltantes:", paste(setdiff(p_cols, names(do)), collapse = ", ")))
})

test_that("daily_output contiene ratios N:P cuando se pasa nutrient_data", {
  bio <- make_bio_nutrients()
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  expect_true("N_to_P_consumption" %in% names(do))
  expect_true("N_to_P_growth"      %in% names(do))
})

# =============================================================================
# 2. Balance de masa
# =============================================================================

test_that("balance de masa N cierra: consumido = crecimiento + excretado + egestado", {
  bio <- make_bio_nutrients()
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  N_err <- abs(do$Nitrogen_consumed_g -
               (do$Nitrogen_growth_g + do$Nitrogen_excretion_g + do$Nitrogen_egestion_g))
  expect_lt(max(N_err, na.rm = TRUE), 1e-8)
})

test_that("balance de masa P cierra: consumido = crecimiento + excretado + egestado", {
  bio <- make_bio_nutrients()
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  P_err <- abs(do$Phosphorus_consumed_g -
               (do$Phosphorus_growth_g + do$Phosphorus_excretion_g + do$Phosphorus_egestion_g))
  expect_lt(max(P_err, na.rm = TRUE), 1e-8)
})

# =============================================================================
# 3. Valores físicamente razonables
# =============================================================================

test_that("N y P consumidos son siempre no-negativos", {
  bio <- make_bio_nutrients()
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  expect_true(all(do$Nitrogen_consumed_g   >= 0, na.rm = TRUE))
  expect_true(all(do$Phosphorus_consumed_g >= 0, na.rm = TRUE))
})

test_that("N y P egestados son siempre no-negativos", {
  bio <- make_bio_nutrients()
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  expect_true(all(do$Nitrogen_egestion_g   >= 0, na.rm = TRUE))
  expect_true(all(do$Phosphorus_egestion_g >= 0, na.rm = TRUE))
})

test_that("ratio N:P consumo está en rango biológico razonable (2–60)", {
  bio <- make_bio_nutrients()
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  np <- do$N_to_P_consumption
  pct_ok <- mean(np > 2 & np < 60, na.rm = TRUE)
  expect_gt(pct_ok, 0.95)
})

# =============================================================================
# 4. Sin nutrient_data no aparecen columnas de N/P
# =============================================================================

test_that("sin nutrient_data no aparecen columnas N/P en daily_output", {
  res <- run_fb4(bio_chinook_30, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  expect_false("Nitrogen_consumed_g"   %in% names(do))
  expect_false("Phosphorus_consumed_g" %in% names(do))
})
