# =============================================================================
# test-12-simulation.R
# Tests para el motor de simulación (run_fb4_simulation) y prepare_simulation_data.
# Se usan fixtures de helper-setup.R (bio_chinook_30, 30 días).
# =============================================================================

# =============================================================================
# 1. prepare_simulation_data
# =============================================================================

test_that("prepare_simulation_data retorna lista con campos requeridos", {
  psd <- prepare_simulation_data(
    bio_obj  = bio_chinook_30,
    strategy = "direct"
  )

  expect_type(psd, "list")
  expect_true("species_params" %in% names(psd))
  expect_true("n_days"         %in% names(psd))
  expect_true("temperatures"   %in% names(psd))
  expect_true("initial_weight" %in% names(psd))
})

test_that("prepare_simulation_data respeta duración del objeto Bioenergetic", {
  psd <- prepare_simulation_data(bio_obj = bio_chinook_30, strategy = "direct")
  expect_equal(psd$n_days, 30L)
})

test_that("prepare_simulation_data tiene 30 valores de temperatura", {
  psd <- prepare_simulation_data(bio_obj = bio_chinook_30, strategy = "direct")
  expect_length(psd$temperatures, 30)
  expect_true(all(psd$temperatures > 0))
})

# =============================================================================
# 2. run_fb4_simulation — método directo (p_value fijo)
# =============================================================================

test_that("run_fb4_simulation (direct, p = 0.5) retorna lista con campos clave", {
  psd <- prepare_simulation_data(bio_obj = bio_chinook_30, strategy = "direct")
  result <- run_fb4_simulation(
    consumption_method       = list(type = "p_value", value = 0.5),
    processed_simulation_data = psd
  )

  expect_type(result, "list")
  expect_true("final_weight"      %in% names(result))
  expect_true("total_consumption" %in% names(result))
  expect_true("daily_output"      %in% names(result))
})

test_that("run_fb4_simulation produce crecimiento positivo con p = 0.5", {
  psd <- prepare_simulation_data(bio_obj = bio_chinook_30, strategy = "direct")
  result <- run_fb4_simulation(
    consumption_method        = list(type = "p_value", value = 0.5),
    processed_simulation_data = psd
  )

  expect_gt(result$final_weight, psd$initial_weight)
})

test_that("run_fb4_simulation: mayor p produce mayor peso final", {
  psd <- prepare_simulation_data(bio_obj = bio_chinook_30, strategy = "direct")

  r_low  <- run_fb4_simulation(list(type = "p_value", value = 0.3), psd)
  r_high <- run_fb4_simulation(list(type = "p_value", value = 0.8), psd)

  expect_gt(r_high$final_weight, r_low$final_weight)
})

test_that("run_fb4_simulation: p = 0 produce pérdida de peso (catabolismo)", {
  psd <- prepare_simulation_data(bio_obj = bio_chinook_30, strategy = "direct")
  result <- run_fb4_simulation(
    consumption_method        = list(type = "p_value", value = 0.0),
    processed_simulation_data = psd
  )
  expect_lt(result$final_weight, psd$initial_weight)
})

test_that("run_fb4_simulation daily_output tiene columnas requeridas", {
  psd <- prepare_simulation_data(bio_obj = bio_chinook_30, strategy = "direct")
  result <- run_fb4_simulation(
    consumption_method        = list(type = "p_value", value = 0.5),
    processed_simulation_data = psd
  )

  do <- result$daily_output
  expect_true(all(c("Day", "Weight", "Consumption_energy",
                    "Respiration", "Egestion", "Excretion",
                    "SDA", "Net_energy") %in% names(do)))
})

test_that("run_fb4_simulation daily_output tiene 30 filas", {
  psd <- prepare_simulation_data(bio_obj = bio_chinook_30, strategy = "direct")
  result <- run_fb4_simulation(
    consumption_method        = list(type = "p_value", value = 0.5),
    processed_simulation_data = psd
  )
  expect_equal(nrow(result$daily_output), 30L)
})

test_that("run_fb4_simulation: energías diarias son todas no-negativas", {
  psd <- prepare_simulation_data(bio_obj = bio_chinook_30, strategy = "direct")
  result <- run_fb4_simulation(
    consumption_method        = list(type = "p_value", value = 0.5),
    processed_simulation_data = psd
  )
  do <- result$daily_output
  expect_true(all(do$Respiration   >= 0))
  expect_true(all(do$Egestion      >= 0))
  expect_true(all(do$Excretion     >= 0))
  expect_true(all(do$SDA           >= 0))
})

test_that("run_fb4_simulation: consumo total = suma diaria de consumo", {
  psd <- prepare_simulation_data(bio_obj = bio_chinook_30, strategy = "direct")
  result <- run_fb4_simulation(
    consumption_method        = list(type = "p_value", value = 0.5),
    processed_simulation_data = psd
  )
  # Consumo total en gramos presa
  cons_sum <- sum(result$daily_output$Consumption_gg * result$daily_output$Weight[-31],
                  na.rm = TRUE)
  expect_equal(result$total_consumption, cons_sum, tolerance = 1)  # tolerancia 1 g
})
