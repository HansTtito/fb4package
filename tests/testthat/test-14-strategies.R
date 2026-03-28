# =============================================================================
# test-14-strategies.R
# Tests de integración para run_fb4() con las estrategias principales.
# Se usan 30 días para que los tests sean rápidos.
# =============================================================================

# =============================================================================
# 1. Estrategia "direct" — p_value fijo
# =============================================================================

test_that("run_fb4 direct retorna objeto fb4_result", {
  result <- run_fb4(x = bio_chinook_30, strategy = "direct", p_value = 0.5)

  expect_s3_class(result, "fb4_result")
})

test_that("run_fb4 direct: summary contiene campos esenciales", {
  result <- run_fb4(x = bio_chinook_30, strategy = "direct", p_value = 0.5)

  expect_true("method"       %in% names(result$summary))
  expect_true("final_weight" %in% names(result$summary))
  expect_true("p_estimate"   %in% names(result$summary))
  expect_equal(result$summary$method, "direct")
})

test_that("run_fb4 direct: p_estimate coincide con el p_value dado", {
  result <- run_fb4(x = bio_chinook_30, strategy = "direct", p_value = 0.6)
  expect_equal(result$summary$p_estimate, 0.6, tolerance = 1e-9)
})

test_that("run_fb4 direct: mayor p produce mayor peso final", {
  r_low  <- run_fb4(bio_chinook_30, strategy = "direct", p_value = 0.3)
  r_high <- run_fb4(bio_chinook_30, strategy = "direct", p_value = 0.8)

  expect_gt(r_high$summary$final_weight, r_low$summary$final_weight)
})

test_that("run_fb4 direct tiene daily_output con 30 filas", {
  result <- run_fb4(bio_chinook_30, strategy = "direct", p_value = 0.5)
  expect_equal(nrow(result$daily_output), 30L)
})

# =============================================================================
# 2. Estrategia "binary_search" — ajuste a peso final
# =============================================================================

target_weight <- 900  # g (por encima del inicial de 800 g)

test_that("run_fb4 binary_search retorna objeto fb4_result", {
  result <- run_fb4(
    x      = bio_chinook_30,
    fit_to = "Weight",
    fit_value = target_weight
  )
  expect_s3_class(result, "fb4_result")
})

test_that("run_fb4 binary_search: final_weight converge al objetivo", {
  result <- run_fb4(
    x         = bio_chinook_30,
    fit_to    = "Weight",
    fit_value = target_weight,
    tolerance = 1.0   # 1 g de tolerancia
  )
  expect_equal(result$summary$final_weight, target_weight, tolerance = 2.0)
})

test_that("run_fb4 binary_search: p_estimate en rango (0, 1]", {
  result <- run_fb4(
    x         = bio_chinook_30,
    fit_to    = "Weight",
    fit_value = target_weight
  )
  p <- result$summary$p_estimate
  expect_gt(p, 0)
  expect_lte(p, 1)
})

test_that("run_fb4 binary_search: método reportado es 'binary_search'", {
  result <- run_fb4(
    x         = bio_chinook_30,
    fit_to    = "Weight",
    fit_value = target_weight
  )
  expect_equal(result$summary$method, "binary_search")
})

test_that("run_fb4 binary_search: objetivo mayor requiere p mayor", {
  r_low  <- run_fb4(bio_chinook_30, fit_to = "Weight", fit_value = 820)
  r_high <- run_fb4(bio_chinook_30, fit_to = "Weight", fit_value = 950)

  expect_gt(r_high$summary$p_estimate, r_low$summary$p_estimate)
})

# =============================================================================
# 3. Estrategia "bootstrap"
# =============================================================================

test_that("run_fb4 bootstrap retorna objeto fb4_result con método 'bootstrap'", {
  set.seed(123)
  obs_w <- rnorm(10, mean = target_weight, sd = 20)

  result <- run_fb4(
    x                  = bio_chinook_30,
    fit_to             = "Weight",
    strategy           = "bootstrap",
    observed_weights   = obs_w,
    n_bootstrap        = 50,    # pocas iter para que sea rápido
    upper              = 1.0,
    parallel           = FALSE
  )
  expect_s3_class(result, "fb4_result")
  expect_equal(result$summary$method, "bootstrap")
})

test_that("run_fb4 bootstrap: p_mean en rango (0, 1]", {
  set.seed(123)
  obs_w <- rnorm(10, mean = target_weight, sd = 20)

  result <- run_fb4(
    x                = bio_chinook_30,
    fit_to           = "Weight",
    strategy         = "bootstrap",
    observed_weights = obs_w,
    n_bootstrap      = 50,
    upper            = 1.0,
    parallel         = FALSE
  )
  p <- result$summary$p_mean %||% result$summary$p_estimate
  expect_gt(p, 0)
  expect_lte(p, 1)
})

test_that("run_fb4 bootstrap: method_data contiene bootstrap_results", {
  set.seed(123)
  obs_w <- rnorm(10, mean = target_weight, sd = 20)

  result <- run_fb4(
    x                = bio_chinook_30,
    fit_to           = "Weight",
    strategy         = "bootstrap",
    observed_weights = obs_w,
    n_bootstrap      = 50,
    upper            = 1.0,
    parallel         = FALSE
  )
  expect_true(!is.null(result$method_data$bootstrap_results))
  expect_true(length(result$method_data$bootstrap_results$p_values) > 0)
})

# =============================================================================
# 4. is.fb4_result y print/summary (sanidad general)
# =============================================================================

test_that("is.fb4_result detecta correctamente objetos fb4_result", {
  result <- run_fb4(bio_chinook_30, strategy = "direct", p_value = 0.5)
  expect_true(is.fb4_result(result))
  expect_false(is.fb4_result(list(a = 1)))
  expect_false(is.fb4_result("string"))
})

test_that("print.fb4_result no lanza error", {
  result <- run_fb4(bio_chinook_30, strategy = "direct", p_value = 0.5)
  expect_no_error(print(result))
})

test_that("summary.fb4_result no lanza error", {
  result <- run_fb4(bio_chinook_30, strategy = "direct", p_value = 0.5)
  expect_no_error(summary(result))
})
