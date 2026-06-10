# =============================================================================
# data-raw/precompute_vignettes.R
#
# Pre-computes all heavy results for vignettes that have bootstrap,
# hierarchical, MLE, or multi-run loops.  Each vignette gets its own .rds
# in inst/extdata/ so they load independently.
#
# Run from the package root:
#   source("data-raw/precompute_vignettes.R")
#
# Re-run whenever model code, species parameters, or vignette inputs change.
# =============================================================================

library(fb4package)

# =============================================================================
# 1. fb4-statistical-estimation
# =============================================================================
message("\n=== fb4-statistical-estimation ===")

data(fish4_parameters)
chinook_db <- fish4_parameters[["Oncorhynchus tshawytscha"]]
stage      <- names(chinook_db$life_stages)[1]
sp_params  <- chinook_db$life_stages[[stage]]
sp_info    <- chinook_db$species_info

days <- 1:100
temp_stat <- data.frame(
  Day         = days,
  Temperature = round(10 + 5 * sin(2 * pi * (days - 60) / 365), 2)
)
diet_stat <- list(
  proportions = data.frame(Day = days, Alewife = 0.7, Shrimp = 0.3),
  prey_names  = c("Alewife", "Shrimp"),
  energies    = data.frame(Day = days, Alewife = 4900, Shrimp = 3200)
)
bio_stat <- Bioenergetic(
  species_params      = sp_params,
  species_info        = sp_info,
  environmental_data  = list(temperature = temp_stat),
  diet_data           = diet_stat,
  simulation_settings = list(initial_weight = 1800, duration = 100)
)
bio_stat$species_params$predator$ED_ini <- 4500
bio_stat$species_params$predator$ED_end <- 5500

set.seed(42)
obs_weights_stat <- rnorm(30, mean = 3000, sd = 80)

message("  binary_search...")
res_bs_stat <- run_fb4(bio_stat, strategy = "binary_search",
                       fit_to = "Weight", fit_value = 3000)

message("  optim...")
res_optim_stat <- run_fb4(bio_stat, strategy = "optim",
                           fit_to = "Weight", fit_value = 3000)

message("  MLE...")
res_mle_stat <- run_fb4(bio_stat, strategy = "mle",
                        fit_to = "Weight", observed_weights = obs_weights_stat)

message("  bootstrap (100 iter)...")
res_boot_stat <- run_fb4(bio_stat,
                         strategy         = "bootstrap",
                         fit_to           = "Weight",
                         observed_weights = obs_weights_stat,
                         n_bootstrap      = 100,
                         upper            = 1,
                         parallel         = FALSE)

message("  hierarchical setup...")
res_ref_stat <- run_fb4(bio_stat, strategy = "binary_search",
                        fit_to = "Weight", fit_value = 2500)
ref_p_stat   <- res_ref_stat$summary$p_value

set.seed(123)
n_fish_stat     <- 20
init_w_stat     <- round(runif(n_fish_stat, 1800 * 0.85, 1800 * 1.15))
true_p_stat     <- pmax(0.3, pmin(1.5, rnorm(n_fish_stat, ref_p_stat, 0.06)))
final_w_stat    <- vapply(seq_len(n_fish_stat), function(i) {
  b <- bio_stat
  b$simulation_settings$initial_weight <- init_w_stat[i]
  r <- run_fb4(b, strategy = "direct", p_value = true_p_stat[i])
  round(r$summary$final_weight * rnorm(1, 1, 0.02), 1)
}, numeric(1))

mr_data_stat <- data.frame(
  individual_id  = sprintf("fish_%02d", seq_len(n_fish_stat)),
  initial_weight = init_w_stat,
  final_weight   = final_w_stat
)

message("  hierarchical model (TMB)...")
res_hier_stat <- run_fb4(
  x                = bio_stat,
  strategy         = "hierarchical",
  backend          = "tmb",
  fit_to           = "Weight",
  observed_weights = mr_data_stat
)

saveRDS(
  list(res_bs    = res_bs_stat,
       res_optim = res_optim_stat,
       res_mle   = res_mle_stat,
       res_boot  = res_boot_stat,
       res_ref   = res_ref_stat,
       ref_p     = ref_p_stat,
       mr_data   = mr_data_stat,
       res_hier  = res_hier_stat,
       n_fish    = n_fish_stat,
       true_p    = true_p_stat),
  file = "inst/extdata/stat_estimation_precomputed.rds"
)
message("  Saved stat_estimation_precomputed.rds")

# =============================================================================
# 2. fb4-introduction
# =============================================================================
message("\n=== fb4-introduction ===")

# Reuses same sp_params / sp_info / days from above
temp_intro <- data.frame(
  Day         = days,
  Temperature = round(10 + 5 * sin(2 * pi * (days - 60) / 365), 2)
)
diet_intro <- list(
  proportions = data.frame(Day = days, Alewife = 0.7, Shrimp = 0.3),
  prey_names  = c("Alewife", "Shrimp"),
  energies    = data.frame(Day = days, Alewife = 4900, Shrimp = 3200)
)
bio_intro <- Bioenergetic(
  species_params      = sp_params,
  species_info        = sp_info,
  environmental_data  = list(temperature = temp_intro),
  diet_data           = diet_intro,
  simulation_settings = list(initial_weight = 1800, duration = 100)
)
bio_intro$species_params$predator$ED_ini <- 4500
bio_intro$species_params$predator$ED_end <- 5500

set.seed(42)
obs_weights_intro <- rnorm(30, mean = 3000, sd = 80)

message("  direct...")
res_direct_intro <- run_fb4(bio_intro, strategy = "direct", p_value = 0.8)

message("  binary_search...")
res_bs_intro <- run_fb4(bio_intro, strategy = "binary_search",
                        fit_to = "Weight", fit_value = 3000)

message("  MLE...")
res_mle_intro <- run_fb4(bio_intro, strategy = "mle",
                         fit_to = "Weight", observed_weights = obs_weights_intro)

message("  bootstrap (100 iter)...")
res_boot_intro <- run_fb4(bio_intro,
                          strategy         = "bootstrap",
                          fit_to           = "Weight",
                          observed_weights = obs_weights_intro,
                          upper            = 1,
                          n_bootstrap      = 100,
                          parallel         = FALSE)

saveRDS(
  list(res_direct = res_direct_intro,
       res_bs     = res_bs_intro,
       res_mle    = res_mle_intro,
       res_boot   = res_boot_intro),
  file = "inst/extdata/intro_precomputed.rds"
)
message("  Saved intro_precomputed.rds")

# =============================================================================
# 3. fb4-case-study-chinook
# =============================================================================
message("\n=== fb4-case-study-chinook ===")

chinook_db2 <- fish4_parameters[["Oncorhynchus tshawytscha"]]
stage2      <- if ("juvenile" %in% names(chinook_db2$life_stages)) "juvenile" else
                 names(chinook_db2$life_stages)[1]
sp_params2  <- chinook_db2$life_stages[[stage2]]
sp_info2    <- chinook_db2$species_info
sp_info2$life_stage <- stage2

days2 <- 1:180
set.seed(42)
base_temp   <- 7 + 7 * sin(pi * (days2 - 20) / 180)
temp_vec2   <- pmax(2, base_temp + rnorm(180, 0, 0.4))
temp_data2  <- data.frame(Day = days2, Temperature = round(temp_vec2, 2))

alewife <- pmax(0, 0.55 + 0.25 * sin(pi * (days2 - 30) / 180))
shrimp  <- pmax(0, 0.28 - 0.10 * sin(pi * (days2 - 30) / 180))
inverts <- pmax(0, 1 - alewife - shrimp)
total   <- alewife + shrimp + inverts

diet_props2 <- data.frame(
  Day     = days2,
  Alewife = round(alewife / total, 4),
  Shrimp  = round(shrimp  / total, 4),
  Inverts = round(inverts / total, 4)
)
prey_energy2 <- data.frame(Day = days2, Alewife = 4900, Shrimp = 3200, Inverts = 2600)

bio_chinook2 <- Bioenergetic(
  species_params     = sp_params2,
  species_info       = sp_info2,
  environmental_data = list(temperature = temp_data2),
  diet_data          = list(
    proportions = diet_props2,
    prey_names  = c("Alewife", "Shrimp", "Inverts"),
    energies    = prey_energy2
  ),
  simulation_settings = list(initial_weight = 5, duration = 180)
)
bio_chinook2$species_params$predator$ED_ini <- 4200
bio_chinook2$species_params$predator$ED_end <- 5000

message("  binary_search...")
res_bs2 <- run_fb4(bio_chinook2, fit_to = "Weight", fit_value = 40,
                   strategy = "binary_search", verbose = FALSE)

message("  direct...")
res_direct2 <- run_fb4(bio_chinook2, fit_to = "p_value", fit_value = 0.75,
                       strategy = "direct_p_value", verbose = FALSE)

set.seed(123)
n_obs2       <- 25
final_wt2    <- res_bs2$summary$final_weight
obs_weights2 <- rnorm(n_obs2, mean = final_wt2, sd = final_wt2 * 0.08)

message("  bootstrap (100 iter, 180 days)...")
res_boot2 <- run_fb4(bio_chinook2,
                     fit_to           = "Weight",
                     observed_weights = obs_weights2,
                     strategy         = "bootstrap",
                     n_bootstrap      = 100,
                     upper            = 1,
                     parallel         = FALSE,
                     confidence_level = 0.95,
                     verbose          = FALSE)

saveRDS(
  list(res_bs      = res_bs2,
       res_direct  = res_direct2,
       obs_weights = obs_weights2,
       res_boot    = res_boot2),
  file = "inst/extdata/case_study_chinook_precomputed.rds"
)
message("  Saved case_study_chinook_precomputed.rds")

# =============================================================================
# 4. fb4-temperature-climate
# =============================================================================
message("\n=== fb4-temperature-climate ===")

sp_params_ch <- list(
  consumption = list(CEQ = 2, CA = 0.303, CB = -0.275,
                     CQ = 3.0, CTO = 15.0, CTM = 20.0,
                     CTL = 24.0, CK1 = 0.1, CK4 = 0.13),
  respiration = list(REQ = 1, RA = 0.00264, RB = -0.217,
                     RQ = 0.06818, RTO = 0.0234, RTM = 0.0,
                     RTL = 25.0, RK1 = 1.0, RK4 = 0.13, RK5 = 0.0),
  activity    = list(ACT = 1.0, BACT = 0.0),
  sda         = list(SDA = 0.172),
  egestion    = list(EGEQ = 1, FA = 0.212, FB = -0.222, FG = 0.631),
  excretion   = list(EXEQ = 1, UA = 0.0314, UB = 0.58, UG = -0.299),
  predator    = list(PREDEDEQ = 2, Alpha1 = 4500, Beta1 = 6.0,
                     Alpha2 = 5500, Beta2 = 2.0, Cutoff = 250)
)

days3      <- 1:120
temp_base3 <- data.frame(Day = days3,
                         Temperature = round(7 + 6 * sin(pi * (days3 - 20) / 120), 2))
diet_props3  <- data.frame(Day = days3, Alewife = 0.65, Shrimp = 0.35)
prey_energy3 <- data.frame(Day = days3, Alewife = 4900, Shrimp = 3200)

make_bio3 <- function(temp_df, initial_weight = 5) {
  Bioenergetic(
    species_params     = sp_params_ch,
    species_info       = list(scientific_name = "Oncorhynchus tshawytscha",
                               common_name = "Chinook salmon",
                               life_stage  = "juvenile"),
    environmental_data = list(temperature = temp_df),
    diet_data          = list(proportions = diet_props3,
                              prey_names  = c("Alewife", "Shrimp"),
                              energies    = prey_energy3),
    simulation_settings = list(initial_weight = initial_weight, duration = 120)
  )
}

target_wt3 <- 40
offsets3   <- c(-3, -2, -1, 0, 1, 2, 3, 4)

message("  manual sensitivity (8 runs)...")
results_sens3 <- lapply(offsets3, function(delta) {
  temp_s <- temp_base3
  temp_s$Temperature <- pmax(1, temp_base3$Temperature + delta)
  bio_s  <- make_bio3(temp_s)
  res    <- tryCatch(
    run_fb4(bio_s, fit_to = "Weight", fit_value = target_wt3,
            strategy = "binary_search", verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(res)) return(data.frame(offset = delta, p_value = NA, final_weight = NA))
  data.frame(offset       = delta,
             p_value      = round(res$summary$p_value,     4),
             final_weight = round(res$summary$final_weight, 1))
})
sens_df3 <- do.call(rbind, results_sens3)

message("  climate scenarios (4 runs)...")
p_fixed3   <- 0.65
scenarios3 <- c("Baseline" = 0, "+1 °C" = 1, "+2 °C" = 2, "+4 °C" = 4)

scenario_results3 <- lapply(names(scenarios3), function(sc) {
  delta  <- scenarios3[[sc]]
  temp_s <- temp_base3
  temp_s$Temperature <- pmax(1, temp_base3$Temperature + delta)
  bio_s  <- make_bio3(temp_s)
  res    <- run_fb4(bio_s, fit_to = "p_value", fit_value = p_fixed3,
                    strategy = "direct_p_value", verbose = FALSE)
  if (is.null(res)) return(NULL)
  data.frame(Scenario    = sc, Delta_T = delta,
             Final_wt_g  = round(res$summary$final_weight,       1),
             Consumption = round(res$summary$total_consumption_g, 1))
})
scenario_df3 <- do.call(rbind, scenario_results3)

message("  sensitivity plot PNG (8 runs)...")
bio_base3 <- make_bio3(temp_base3)
png("inst/extdata/temperature_sensitivity_plot.png", width = 700, height = 500, res = 96)
plot(bio_base3,
     type         = "sensitivity",
     temp_offsets = c(-3, -2, -1, 0, 1, 2, 3, 4),
     fit_to       = "Weight",
     fit_value    = 40,
     colors       = "blue")
dev.off()

message("  thermal window (23 runs)...")
temp_grid3   <- seq(2, 24, by = 1)
p_at_target3 <- sapply(temp_grid3, function(t_const) {
  temp_c <- data.frame(Day = days3, Temperature = t_const)
  bio_t  <- make_bio3(temp_c)
  tryCatch({
    res <- run_fb4(bio_t, fit_to = "Weight", fit_value = target_wt3,
                   strategy = "binary_search", verbose = FALSE)
    res$summary$p_value
  }, error = function(e) NA_real_)
})
perf_df3 <- data.frame(Temperature = temp_grid3, p_required = p_at_target3)

saveRDS(
  list(sens_df     = sens_df3,
       scenario_df = scenario_df3,
       perf_df     = perf_df3,
       p_fixed     = p_fixed3,
       target_wt   = target_wt3,
       temp_base   = temp_base3),
  file = "inst/extdata/temperature_climate_precomputed.rds"
)
message("  Saved temperature_climate_precomputed.rds")

message("\n=== Todos los .rds generados en inst/extdata/ ===")
