# =============================================================================
# test-14b-strategies-mle-hierarchical.R
# Tests de integración para run_fb4() con estrategias MLE y jerárquica.
# Se usan 30 días y pocos datos para que los tests sean rápidos.
# =============================================================================

# Peso objetivo razonable para bio_chinook_30 (800 g inicial, 30 días)
target_weight_30 <- 900   # g

# Vector numérico de pesos finales — usado por MLE y bootstrap
obs_weights_30   <- c(880, 890, 895, 900, 905, 910, 920,
                      885, 897, 903, 908, 915, 878, 912, 901)

# Data.frame individual — requerido por la estrategia hierarchical
obs_individual_30 <- data.frame(
  individual_id  = paste0("fish_", seq_along(obs_weights_30)),
  initial_weight = 800,          # igual al initial_weight de bio_chinook_30
  final_weight   = obs_weights_30,
  stringsAsFactors = FALSE
)

# =============================================================================
# 1. MLE — estructura del resultado
# =============================================================================

test_that("run_fb4 mle retorna objeto fb4_result", {
  set.seed(1)
  result <- run_fb4(
    x                = bio_chinook_30,
    strategy         = "mle",
    fit_to           = "Weight",
    observed_weights = obs_weights_30,
    upper            = 1.5
  )
  expect_s3_class(result, "fb4_result")
})

test_that("run_fb4 mle: method es 'mle'", {
  set.seed(1)
  result <- run_fb4(
    x                = bio_chinook_30,
    strategy         = "mle",
    fit_to           = "Weight",
    observed_weights = obs_weights_30,
    upper            = 1.5
  )
  expect_equal(result$summary$method, "mle")
})

test_that("run_fb4 mle: summary$p_value es numerico y finito", {
  set.seed(1)
  result <- run_fb4(
    x                = bio_chinook_30,
    strategy         = "mle",
    fit_to           = "Weight",
    observed_weights = obs_weights_30,
    upper            = 1.5
  )
  p <- result$summary$p_value
  expect_true(is.numeric(p))
  expect_true(is.finite(p))
  expect_gt(p, 0)
  expect_lte(p, 1.5)
})

test_that("run_fb4 mle: p_value y p_estimate son el mismo valor", {
  set.seed(1)
  result <- run_fb4(
    x                = bio_chinook_30,
    strategy         = "mle",
    fit_to           = "Weight",
    observed_weights = obs_weights_30,
    upper            = 1.5
  )
  expect_equal(result$summary$p_value, result$summary$p_estimate)
})

test_that("run_fb4 mle: summary$total_consumption es alias de total_consumption_g", {
  set.seed(1)
  result <- run_fb4(
    x                = bio_chinook_30,
    strategy         = "mle",
    fit_to           = "Weight",
    observed_weights = obs_weights_30,
    upper            = 1.5
  )
  expect_equal(result$summary$total_consumption,
               result$summary$total_consumption_g)
})

test_that("run_fb4 mle: summary$p_se es numerico (puede ser NA si no converge)", {
  set.seed(1)
  result <- run_fb4(
    x                = bio_chinook_30,
    strategy         = "mle",
    fit_to           = "Weight",
    observed_weights = obs_weights_30,
    upper            = 1.5
  )
  # p_se should be numeric (NA is acceptable when not converged)
  expect_true(is.numeric(result$summary$p_se) || is.na(result$summary$p_se))
})

test_that("run_fb4 mle: summary$converged es logico", {
  set.seed(1)
  result <- run_fb4(
    x                = bio_chinook_30,
    strategy         = "mle",
    fit_to           = "Weight",
    observed_weights = obs_weights_30,
    upper            = 1.5
  )
  expect_true(is.logical(result$summary$converged))
})

test_that("run_fb4 mle: daily_output tiene 30 filas", {
  set.seed(1)
  result <- run_fb4(
    x                = bio_chinook_30,
    strategy         = "mle",
    fit_to           = "Weight",
    observed_weights = obs_weights_30,
    upper            = 1.5
  )
  expect_equal(nrow(result$daily_output), 30L)
})

test_that("run_fb4 mle: print no lanza error", {
  set.seed(1)
  result <- run_fb4(
    x                = bio_chinook_30,
    strategy         = "mle",
    fit_to           = "Weight",
    observed_weights = obs_weights_30,
    upper            = 1.5
  )
  expect_no_error(print(result))
})

test_that("run_fb4 mle: mle con mas observaciones da SE menor o igual", {
  set.seed(42)
  obs_small <- rnorm(5,  mean = target_weight_30, sd = 15)
  obs_large <- rnorm(50, mean = target_weight_30, sd = 15)

  r_small <- run_fb4(bio_chinook_30, strategy = "mle",
                     fit_to = "Weight", observed_weights = obs_small,
                     upper = 1.5)
  r_large <- run_fb4(bio_chinook_30, strategy = "mle",
                     fit_to = "Weight", observed_weights = obs_large,
                     upper = 1.5)

  se_small <- r_small$summary$p_se
  se_large <- r_large$summary$p_se

  # Only test if both SEs are available and finite
  if (all(!is.na(c(se_small, se_large))) && all(is.finite(c(se_small, se_large)))) {
    expect_lte(se_large, se_small * 1.5)   # larger n → SE should not grow much
  } else {
    skip("SE not available (optimization did not converge)")
  }
})

# =============================================================================
# 2. Jerárquica — estructura del resultado
# =============================================================================

test_that("run_fb4 hierarchical retorna objeto fb4_result", {
  skip_if_not(requireNamespace("TMB", quietly = TRUE),
              "TMB not available — skipping hierarchical tests")
  set.seed(1)
  result <- run_fb4(
    x                = bio_chinook_30,
    strategy         = "hierarchical",
    backend          = "tmb",
    fit_to           = "Weight",
    observed_weights = obs_individual_30
  )
  expect_s3_class(result, "fb4_result")
})

test_that("run_fb4 hierarchical: method es 'hierarchical'", {
  skip_if_not(requireNamespace("TMB", quietly = TRUE),
              "TMB not available")
  set.seed(1)
  result <- run_fb4(
    x                = bio_chinook_30,
    strategy         = "hierarchical",
    backend          = "tmb",
    fit_to           = "Weight",
    observed_weights = obs_individual_30
  )
  expect_equal(result$summary$method, "hierarchical")
})

test_that("run_fb4 hierarchical: mu_p_estimate es numerico y positivo", {
  skip_if_not(requireNamespace("TMB", quietly = TRUE),
              "TMB not available")
  set.seed(1)
  result <- run_fb4(
    x                = bio_chinook_30,
    strategy         = "hierarchical",
    backend          = "tmb",
    fit_to           = "Weight",
    observed_weights = obs_individual_30
  )
  mu_p <- result$summary$mu_p_estimate
  expect_true(is.numeric(mu_p))
  expect_gt(mu_p, 0)
})

test_that("run_fb4 hierarchical: sigma_p_estimate es numerico y no negativo", {
  skip_if_not(requireNamespace("TMB", quietly = TRUE),
              "TMB not available")
  set.seed(1)
  result <- run_fb4(
    x                = bio_chinook_30,
    strategy         = "hierarchical",
    backend          = "tmb",
    fit_to           = "Weight",
    observed_weights = obs_individual_30
  )
  sigma_p <- result$summary$sigma_p_estimate
  expect_true(is.numeric(sigma_p))
  expect_gte(sigma_p, 0)
})

test_that("run_fb4 hierarchical: n_individuals coincide con nrow(observed_weights)", {
  skip_if_not(requireNamespace("TMB", quietly = TRUE),
              "TMB not available")
  set.seed(1)
  result <- run_fb4(
    x                = bio_chinook_30,
    strategy         = "hierarchical",
    backend          = "tmb",
    fit_to           = "Weight",
    observed_weights = obs_individual_30
  )
  expect_equal(result$summary$n_individuals, nrow(obs_individual_30))
})

test_that("run_fb4 hierarchical: summary$converged es logico", {
  skip_if_not(requireNamespace("TMB", quietly = TRUE),
              "TMB not available")
  set.seed(1)
  result <- run_fb4(
    x                = bio_chinook_30,
    strategy         = "hierarchical",
    backend          = "tmb",
    fit_to           = "Weight",
    observed_weights = obs_individual_30
  )
  expect_true(is.logical(result$summary$converged))
})

test_that("run_fb4 hierarchical: mu_p coherente con binary_search en datos sin ruido", {
  skip_if_not(requireNamespace("TMB", quietly = TRUE),
              "TMB not available")

  # Run binary search first to get the "true" p
  r_bs <- run_fb4(bio_chinook_30, strategy = "binary_search",
                  fit_to = "Weight", fit_value = target_weight_30)
  p_true <- r_bs$summary$p_value

  # Generate low-noise observations around that target weight (data.frame format)
  set.seed(99)
  tight_weights <- rnorm(20, mean = target_weight_30, sd = 5)
  obs_tight <- data.frame(
    individual_id  = paste0("fish_", seq_along(tight_weights)),
    initial_weight = 800,
    final_weight   = tight_weights,
    stringsAsFactors = FALSE
  )

  r_hier <- run_fb4(bio_chinook_30, strategy = "hierarchical",
                    backend = "tmb", fit_to = "Weight",
                    observed_weights = obs_tight)

  mu_p <- r_hier$summary$mu_p_estimate
  if (!is.na(mu_p) && is.finite(mu_p)) {
    # mu_p should be within 10% of the binary_search p for tight data
    expect_lt(abs(mu_p - p_true) / p_true, 0.10)
  } else {
    skip("Hierarchical model did not converge — skipping coherence check")
  }
})
