# =============================================================================
# test-14c-contaminants.R
# Tests para el sub-modelo de contaminantes (CONTEQ 1, 2, 3) en el loop de
# simulación. Usa fixtures de helper-setup.R (make_bio_chinook).
# =============================================================================

# ---------------------------------------------------------------------------
# Fixture: construye un objeto Bioenergetic con contaminant_data para un CONTEQ dado
# ---------------------------------------------------------------------------
make_bio_contam <- function(CONTEQ_num, n_days = 30, extra = list()) {
  bio <- make_bio_chinook(n_days)

  base_cd <- list(
    CONTEQ                  = CONTEQ_num,
    initial_concentration   = 0.10,
    prey_concentrations     = data.frame(
      Day     = c(1, n_days),
      Alewife = c(0.05, 0.05),
      Shrimp  = c(0.02, 0.02)
    ),
    transfer_efficiency     = data.frame(
      Day     = c(1, n_days),
      Alewife = c(0.85, 0.85),
      Shrimp  = c(0.75, 0.75)
    ),
    assimilation_efficiency = data.frame(
      Day     = c(1, n_days),
      Alewife = c(0.80, 0.80),
      Shrimp  = c(0.70, 0.70)
    )
  )

  bio$contaminant_data   <- c(base_cd, extra)
  bio$calc_contaminants  <- TRUE
  bio
}

# =============================================================================
# 1. Columnas presentes en daily_output
# =============================================================================

test_that("CONTEQ 1 — daily_output contiene columnas de contaminante", {
  bio <- make_bio_contam(1)
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  cols <- c("Contaminant_clearance_ug_d", "Contaminant_uptake_ug",
            "Contaminant_burden_ug",      "Contaminant_concentration_ug_g")
  expect_true(all(cols %in% names(do)),
              info = paste("Columnas faltantes:", paste(setdiff(cols, names(do)), collapse = ", ")))
})

test_that("CONTEQ 2 — daily_output contiene columnas de contaminante", {
  bio <- make_bio_contam(2)
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  cols <- c("Contaminant_clearance_ug_d", "Contaminant_uptake_ug",
            "Contaminant_burden_ug",      "Contaminant_concentration_ug_g")
  expect_true(all(cols %in% names(do)))
})

test_that("CONTEQ 3 — daily_output contiene columnas de contaminante", {
  bio <- make_bio_contam(3, extra = list(
    gill_efficiency      = 0.5,
    fish_water_partition = 5000,
    water_concentration  = 0.0001,
    dissolved_fraction   = 0.9,
    do_saturation        = 0.9
  ))
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  cols <- c("Contaminant_clearance_ug_d", "Contaminant_uptake_ug",
            "Contaminant_burden_ug",      "Contaminant_concentration_ug_g")
  expect_true(all(cols %in% names(do)))
})

# =============================================================================
# 2. Burden y concentración siempre no-negativos
# =============================================================================

test_that("CONTEQ 1 — burden y concentración son siempre no-negativos", {
  bio <- make_bio_contam(1)
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  expect_true(all(do$Contaminant_burden_ug            >= 0, na.rm = TRUE))
  expect_true(all(do$Contaminant_concentration_ug_g   >= 0, na.rm = TRUE))
})

test_that("CONTEQ 2 — burden y concentración son siempre no-negativos", {
  bio <- make_bio_contam(2)
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  expect_true(all(do$Contaminant_burden_ug            >= 0, na.rm = TRUE))
  expect_true(all(do$Contaminant_concentration_ug_g   >= 0, na.rm = TRUE))
})

test_that("CONTEQ 3 — burden y concentración son siempre no-negativos", {
  bio <- make_bio_contam(3, extra = list(
    gill_efficiency      = 0.5,
    fish_water_partition = 5000,
    water_concentration  = 0.0001,
    dissolved_fraction   = 0.9,
    do_saturation        = 0.9
  ))
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  expect_true(all(do$Contaminant_burden_ug            >= 0, na.rm = TRUE))
  expect_true(all(do$Contaminant_concentration_ug_g   >= 0, na.rm = TRUE))
})

# =============================================================================
# 3. Comportamiento específico por CONTEQ
# =============================================================================

test_that("CONTEQ 1 — clearance es cero (acumulación pura, sin eliminación)", {
  bio <- make_bio_contam(1)
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  expect_true(all(do$Contaminant_clearance_ug_d == 0, na.rm = TRUE))
})

test_that("CONTEQ 1 — burden es monotonamente creciente", {
  bio <- make_bio_contam(1)
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  expect_true(all(diff(do$Contaminant_burden_ug) >= -1e-10, na.rm = TRUE))
})

test_that("CONTEQ 2 — eliminación activa la mayoría de días", {
  bio <- make_bio_contam(2)
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  pct_active <- mean(do$Contaminant_clearance_ug_d > 0, na.rm = TRUE)
  expect_gt(pct_active, 0.5)
})

test_that("CONTEQ 3 — eliminación activa la mayoría de días", {
  bio <- make_bio_contam(3, extra = list(
    gill_efficiency      = 0.5,
    fish_water_partition = 5000,
    water_concentration  = 0.0001,
    dissolved_fraction   = 0.9,
    do_saturation        = 0.9
  ))
  res <- run_fb4(bio, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  pct_active <- mean(do$Contaminant_clearance_ug_d > 0, na.rm = TRUE)
  expect_gt(pct_active, 0.5)
})

# =============================================================================
# 4. Sin contaminant_data no aparecen columnas de contaminante
# =============================================================================

test_that("sin contaminant_data no aparecen columnas de contaminante en daily_output", {
  res <- run_fb4(bio_chinook_30, fit_to = "p_value", fit_value = 0.4,
                 strategy = "direct", verbose = FALSE)
  do  <- res$daily_output

  expect_false("Contaminant_burden_ug"          %in% names(do))
  expect_false("Contaminant_concentration_ug_g" %in% names(do))
})
