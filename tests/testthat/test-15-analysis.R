# =============================================================================
# test-15-analysis.R
# Tests para las funciones de análisis post-simulación.
# Requiere un resultado de binary_search como fixture de entrada.
# =============================================================================

# Fixture compartido: resultado de binary_search a 30 días
result_bs <- run_fb4(
  x         = bio_chinook_30,
  fit_to    = "Weight",
  fit_value = 900
)

result_direct <- run_fb4(
  x         = bio_chinook_30,
  strategy  = "direct",
  p_value   = 0.5
)

# =============================================================================
# 1. analyze_growth_patterns
# =============================================================================

test_that("analyze_growth_patterns retorna lista con campos básicos", {
  g <- analyze_growth_patterns(result_bs)

  expect_type(g, "list")
  expect_true("final_weight"         %in% names(g))
  expect_true("total_growth"         %in% names(g))
  expect_true("relative_growth"      %in% names(g))
  expect_true("daily_growth_rate"    %in% names(g))
  expect_true("specific_growth_rate" %in% names(g))
})

test_that("analyze_growth_patterns: final_weight$estimate > initial_weight", {
  g <- analyze_growth_patterns(result_bs)
  expect_gt(g$final_weight$estimate, bio_chinook_30$simulation_settings$initial_weight)
})

test_that("analyze_growth_patterns: relative_growth ~ 12.5% (800→900 en 30 días)", {
  g <- analyze_growth_patterns(result_bs)
  expect_equal(g$relative_growth$estimate, 12.5, tolerance = 0.5)
})

test_that("analyze_growth_patterns: daily_growth_rate > 0", {
  g <- analyze_growth_patterns(result_bs)
  expect_gt(g$daily_growth_rate$estimate, 0)
})

test_that("analyze_growth_patterns: specific_growth_rate (SGR) es positivo", {
  g <- analyze_growth_patterns(result_bs)
  expect_gt(g$specific_growth_rate$estimate, 0)
})

# =============================================================================
# 2. analyze_feeding_performance
# =============================================================================

test_that("analyze_feeding_performance retorna lista con campos básicos", {
  f <- analyze_feeding_performance(result_bs)

  expect_type(f, "list")
  expect_true("total_consumption"   %in% names(f))
  expect_true("daily_consumption"   %in% names(f))
  expect_true("feeding_efficiency"  %in% names(f))
  expect_true("p_value"             %in% names(f))
})

test_that("analyze_feeding_performance: total_consumption > 0", {
  f <- analyze_feeding_performance(result_bs)
  expect_gt(f$total_consumption$estimate, 0)
})

test_that("analyze_feeding_performance: feeding_efficiency en (0, 1)", {
  f <- analyze_feeding_performance(result_bs)
  fe <- f$feeding_efficiency$estimate
  expect_gt(fe, 0)
  expect_lt(fe, 1)
})

test_that("analyze_feeding_performance: daily_consumption = total / n_days", {
  f <- analyze_feeding_performance(result_bs)
  expect_equal(
    f$daily_consumption$estimate,
    f$total_consumption$estimate / 30,
    tolerance = 0.01
  )
})

# =============================================================================
# 3. analyze_energy_budget
# =============================================================================

test_that("analyze_energy_budget retorna lista con energy_components", {
  e <- analyze_energy_budget(result_bs)
  expect_type(e, "list")
  expect_true("energy_components" %in% names(e))
})

test_that("analyze_energy_budget: consumption_energy > 0 (fix aplicado)", {
  e <- analyze_energy_budget(result_bs)
  expect_gt(e$energy_components$consumption_energy$estimate, 0)
})

test_that("analyze_energy_budget: todos los componentes son no-negativos", {
  e   <- analyze_energy_budget(result_bs)
  ec  <- e$energy_components
  components <- c("consumption_energy", "respiration_energy",
                  "egestion_energy", "excretion_energy", "sda_energy")
  for (comp in components) {
    est <- ec[[comp]]$estimate
    expect_gte(est, 0, label = comp)
  }
})

test_that("analyze_energy_budget: balance  cons = resp + ege + exc + SDA + net (aprox)", {
  e   <- analyze_energy_budget(result_bs)
  ec  <- e$energy_components

  cons <- ec$consumption_energy$estimate
  resp <- ec$respiration_energy$estimate
  ege  <- ec$egestion_energy$estimate
  exc  <- ec$excretion_energy$estimate
  sda  <- ec$sda_energy$estimate
  net  <- ec$net_energy$estimate

  expect_equal(cons, resp + ege + exc + sda + net, tolerance = cons * 0.01)
})

# =============================================================================
# 4. comprehensive_nutritional_analysis
# =============================================================================

test_that("comprehensive_nutritional_analysis retorna lista con model_info y energy_budget", {
  na <- comprehensive_nutritional_analysis(result_bs)
  expect_type(na, "list")
  expect_true("model_info"    %in% names(na))
  expect_true("energy_budget" %in% names(na))
})

test_that("comprehensive_nutritional_analysis: energy budget no es todo NA después del fix", {
  na  <- comprehensive_nutritional_analysis(result_bs)
  ec  <- na$energy_budget$energy_components
  expect_gt(ec$consumption_energy$estimate, 0)
})

# =============================================================================
# 5. Clases y validación (Bioenergetic)
# =============================================================================

test_that("is.Bioenergetic detecta correctamente objetos Bioenergetic", {
  expect_true(is.Bioenergetic(bio_chinook_30))
  expect_false(is.Bioenergetic(list(a = 1)))
  expect_false(is.Bioenergetic(42))
})

test_that("print.Bioenergetic no lanza error", {
  expect_no_error(print(bio_chinook_30))
})

test_that("summary.Bioenergetic no lanza error", {
  expect_no_error(summary(bio_chinook_30))
})
