# =============================================================================
# helper-setup.R
# Fixtures compartidos para todos los tests de fb4package.
# Testthat carga automáticamente todos los archivos helper-*.R antes de correr
# cualquier test, por lo que estos objetos están disponibles en todos los tests.
# =============================================================================

# ---------------------------------------------------------------------------
# 1. Parámetros crudos de Chinook salmon (Stewart & Ibarra 1991)
# ---------------------------------------------------------------------------
chinook_species_params <- list(
  consumption = list(
    CEQ = 2, CA = 0.303, CB = -0.275,
    CQ = 3.0, CTO = 15.0, CTM = 20.0, CTL = 24.0,
    CK1 = 0.1, CK4 = 0.13
  ),
  respiration = list(
    REQ = 1, RA = 0.00264, RB = -0.217, RQ = 0.06818,
    RTO = 0.0234, RTM = 0.0, RTL = 25.0,
    RK1 = 1.0, RK4 = 0.13, RK5 = 0.0
  ),
  activity = list(ACT = 1.0, BACT = 0.0),
  sda      = list(SDA = 0.172),
  egestion = list(EGEQ = 1, FA = 0.212, FB = -0.222, FG = 0.631),
  excretion = list(EXEQ = 1, UA = 0.0314, UB = 0.58, UG = -0.299),
  predator = list(
    PREDEDEQ = 2, Alpha1 = 4500, Beta1 = 6.0,
    Alpha2 = 5500, Beta2 = 2.0, Cutoff = 250
  )
)

chinook_species_info <- list(
  scientific_name = "Oncorhynchus tshawytscha",
  common_name     = "Chinook salmon",
  family          = "Salmonidae",
  life_stage      = "adult"
)

# ---------------------------------------------------------------------------
# 2. Datos ambientales: temperatura anual simplificada (30 días)
# ---------------------------------------------------------------------------
make_temp_data <- function(n = 30, seed = 42) {
  set.seed(seed)
  days <- seq_len(n)
  data.frame(
    Day         = days,
    Temperature = round(8 + 4 * sin(2 * pi * (days - 60) / 365) + rnorm(n, 0, 0.3), 2)
  )
}

chinook_temp_data <- make_temp_data(30)

# ---------------------------------------------------------------------------
# 3. Dieta uniforme simplificada (30 días, 2 presas)
# ---------------------------------------------------------------------------
make_diet_data <- function(n = 30) {
  days <- seq_len(n)
  list(
    proportions = data.frame(
      Day     = days,
      Alewife = 0.7,
      Shrimp  = 0.3
    ),
    prey_names = c("Alewife", "Shrimp"),
    energies   = data.frame(
      Day     = days,
      Alewife = 4900,
      Shrimp  = 3200
    )
  )
}

chinook_diet_data <- make_diet_data(30)

# ---------------------------------------------------------------------------
# 4. Objeto Bioenergetic listo para simulaciones cortas (30 días)
# ---------------------------------------------------------------------------
make_bio_chinook <- function(n_days = 30, initial_weight = 800) {
  bio <- Bioenergetic(
    species_params      = chinook_species_params,
    species_info        = chinook_species_info,
    environmental_data  = list(temperature = make_temp_data(n_days)),
    diet_data           = make_diet_data(n_days),
    simulation_settings = list(initial_weight = initial_weight, duration = n_days)
  )
  bio$species_params$predator$ED_ini <- 4000
  bio$species_params$predator$ED_end <- 4500
  bio
}

bio_chinook_30 <- make_bio_chinook(30, 800)

# ---------------------------------------------------------------------------
# 5. Parámetros de consumo procesados (para tests unitarios de bajo nivel)
# ---------------------------------------------------------------------------
processed_cons_params <- fb4package:::process_consumption_params(chinook_species_params$consumption)
processed_resp_params <- fb4package:::process_respiration_params(
  chinook_species_params$respiration,
  activity_params = chinook_species_params$activity,
  sda_params      = chinook_species_params$sda
)
processed_ege_params  <- fb4package:::process_egestion_params(chinook_species_params$egestion)
processed_exc_params  <- fb4package:::process_excretion_params(chinook_species_params$excretion)
