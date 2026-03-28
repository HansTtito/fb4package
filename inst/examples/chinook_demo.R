# ==============================================================================
#  FB4 PACKAGE — Chinook Salmon Demo Script
#  Oncorhynchus tshawytscha · Pacific Northwest adult
#
#  Objetivo: ejercitar las principales funciones del paquete fb4package
#  usando parámetros y condiciones realistas para salmón Chinook.
#
#  Secciones:
#    0. Setup y carga del paquete
#    1. Explorar la base de datos de parámetros
#    2. Parámetros de la especie
#    3. Datos ambientales y de dieta
#    4. Construir y validar el objeto Bioenergetic
#    5. Simulación determinística (binary search → peso final 1200 g)
#    6. Simulación por proporción directa (p = 0.6)
#    7. Bootstrap sobre pesos observados
#    8. MLE (máxima verosimilitud)
#    9. Análisis de resultados
#   10. Análisis de sensibilidad a la temperatura
#   11. Guardar gráficos a disco
# ==============================================================================


# ==============================================================================
# 0. SETUP
# ==============================================================================

# Instalar desde el directorio del paquete si es necesario:
# devtools::install_local(".")

library(fb4package)

output_dir <- tempdir()
cat("Gráficos se guardarán en:", output_dir, "\n\n")


# ==============================================================================
# 1. EXPLORAR BASE DE DATOS DE PARÁMETROS
# ==============================================================================

data(fish4_parameters)

cat("=== Base de datos ===\n")
cat("Total de entradas:", length(fish4_parameters), "\n")
cat("Primeras 10 especies:\n")
print(head(names(fish4_parameters), 10))

# Buscar Chinook / Oncorhynchus tshawytscha
chinook_key <- grep("tshawytscha", names(fish4_parameters),
                    value = TRUE, ignore.case = TRUE)
cat("\nEncontrado Chinook:", length(chinook_key) > 0, "\n")

if (length(chinook_key) > 0) {
  cat("Clave en DB:", chinook_key[1], "\n")
  chinook_db <- fish4_parameters[[chinook_key[1]]]
  cat("Etapas de vida disponibles:", names(chinook_db$life_stages), "\n")
  cat("Fuentes:\n"); print(chinook_db$sources)
} else {
  cat("Chinook no encontrado en DB; usando parámetros de Stewart & Ibarra (1991)\n")
}


# ==============================================================================
# 2. PARÁMETROS DE LA ESPECIE
# ==============================================================================

if (length(chinook_key) > 0) {
  stage_names    <- names(chinook_db$life_stages)
  stage          <- if ("juvenile" %in% stage_names) "juvenile" else stage_names[1]
  species_params <- chinook_db$life_stages[[stage]]
  species_info   <- chinook_db$species_info
  species_info$life_stage <- stage
  cat("\nUsando parámetros de la DB → etapa:", stage, "\n")

} else {
  # Parámetros manuales (Stewart & Ibarra 1991; Hanson et al. 1997)
  species_params <- list(
    consumption = list(
      CEQ = 2,  CA = 0.303,  CB = -0.275,
      CQ = 3.0, CTO = 15.0,  CTM = 20.0,  CTL = 24.0,
      CK1 = 0.1, CK4 = 0.13
    ),
    respiration = list(
      REQ = 1,  RA = 0.00264, RB = -0.217, RQ = 0.06818,
      RTO = 0.0234, RTM = 0.0, RTL = 25.0,
      RK1 = 1.0, RK4 = 0.13, RK5 = 0.0
    ),
    activity    = list(ACT = 1.0, BACT = 0.0),
    sda         = list(SDA = 0.172),
    egestion    = list(EGEQ = 1, FA = 0.212, FB = -0.222, FG = 0.631),
    excretion   = list(EXEQ = 1, UA = 0.0314, UB = 0.58, UG = -0.299),
    predator    = list(
      PREDEDEQ = 2, Alpha1 = 4500, Beta1 = 6.0,
      Alpha2 = 5500, Beta2 = 2.0, Cutoff = 250
    )
  )
  species_info <- list(
    scientific_name = "Oncorhynchus tshawytscha",
    common_name     = "Chinook salmon",
    family          = "Salmonidae",
    life_stage      = "adult"
  )
}


# ==============================================================================
# 3. DATOS AMBIENTALES Y DE DIETA
# ==============================================================================

# --- 3a. Temperatura (perfil anual tipo Pacífico NW, °C) --------------------
set.seed(42)
days      <- 1:365
base_temp <- 8 + 6 * sin(2 * pi * (days - 60) / 365)  # onda estacional ~8–14 °C
noise     <- rnorm(365, 0, 0.5)
temp_vec  <- pmax(1, base_temp + noise)

temp_data <- data.frame(
  Day         = days,
  Temperature = round(temp_vec, 2)
)

cat("\n=== Temperatura ===\n")
cat(sprintf("Rango: %.1f – %.1f °C | Media: %.1f °C\n",
            min(temp_data$Temperature),
            max(temp_data$Temperature),
            mean(temp_data$Temperature)))

# --- 3b. Dieta estacional (Alewife, Shrimp, Inverts) -----------------------
alewife  <- pmax(0, 0.60 + 0.20 * sin(2 * pi * (days - 100) / 365))
shrimp   <- pmax(0, 0.25 - 0.10 * sin(2 * pi * (days - 100) / 365))
inverts  <- pmax(0, 1 - alewife - shrimp)
tot      <- alewife + shrimp + inverts

diet_props <- data.frame(
  Day     = days,
  Alewife = round(alewife / tot, 4),
  Shrimp  = round(shrimp  / tot, 4),
  Inverts = round(inverts / tot, 4)
)

# Densidades energéticas diarias (J/g peso húmedo, constantes)
prey_energy <- data.frame(
  Day     = days,
  Alewife = 4900,
  Shrimp  = 3200,
  Inverts = 2600
)

cat("\n=== Dieta (primeras 3 filas) ===\n")
print(head(diet_props, 3))

# --- 3c. Configuración de simulación ----------------------------------------
sim_settings <- list(
  initial_weight = 800.0,   # g
  duration       = 365
)


# ==============================================================================
# 4. CONSTRUIR Y VALIDAR EL OBJETO BIOENERGETIC
# ==============================================================================

bio_chinook <- Bioenergetic(
  species_params     = species_params,
  species_info       = species_info,
  environmental_data = list(temperature = temp_data),
  diet_data = list(
    proportions = diet_props,
    prey_names  = c("Alewife", "Shrimp", "Inverts"),
    energies    = prey_energy
  ),
  simulation_settings = sim_settings
)

# Densidad energética inicial / final del predador (necesaria con PREDEDEQ = 1)
bio_chinook$species_params$predator$ED_ini <- 4000
bio_chinook$species_params$predator$ED_end <- 4500

cat("\n=== Objeto Bioenergetic ===\n")
print(bio_chinook)

cat("\n=== Summary ===\n")
summary(bio_chinook)

# Gráficos de setup
cat("\n>> Plot dashboard de setup\n");       plot(bio_chinook, type = "dashboard")
cat("\n>> Plot temperatura\n");              plot(bio_chinook, type = "temperature")
cat("\n>> Plot dieta\n");                    plot(bio_chinook, type = "diet")
cat("\n>> Plot densidad energética\n");      plot(bio_chinook, type = "energy")


# ==============================================================================
# 5. SIMULACIÓN DETERMINÍSTICA — AJUSTE A PESO FINAL
# ==============================================================================

cat("\n\n=== Simulación: ajuste a peso final (binary search) ===\n")
result_bsearch <- run_fb4(
  x         = bio_chinook,
  fit_to    = "Weight",
  fit_value = 1200,           # peso final objetivo (g)
  strategy  = "binary_search",
  verbose   = FALSE
)

cat("Clase del resultado:", class(result_bsearch), "\n")
print(result_bsearch)

cat("\n--- Daily output (primeras filas) ---\n")
print(head(result_bsearch$daily_output, 5))

# Gráficos del resultado
cat("\n>> Plot dashboard\n");    plot(result_bsearch, type = "dashboard")
cat("\n>> Plot crecimiento\n");  plot(result_bsearch, type = "growth")
cat("\n>> Plot consumo\n");      plot(result_bsearch, type = "consumption")
cat("\n>> Plot temperatura\n");  plot(result_bsearch, type = "temperature")
cat("\n>> Plot energía\n");      plot(result_bsearch, type = "energy")


# ==============================================================================
# 6. SIMULACIÓN CON p_VALUE DIRECTO
# ==============================================================================

cat("\n\n=== Simulación: p_value directo (p = 0.6) ===\n")
result_direct <- run_fb4(
  x         = bio_chinook,
  fit_to    = "p_value",
  fit_value = 0.6,
  strategy  = "direct_p_value",
  verbose   = FALSE
)

print(result_direct)

cat("\nSummary:\n")
summary(result_direct)


# ==============================================================================
# 7. BOOTSTRAP SOBRE PESOS OBSERVADOS
# ==============================================================================

cat("\n\n=== Bootstrap: estimación de p con pesos observados ===\n")

# Peso final del resultado determinístico + ruido para simular datos de campo
p_true            <- result_bsearch$summary$p_value
final_weight_true <- result_bsearch$summary$final_weight
cat(sprintf("p_value verdadero (simulado): %.4f\n", p_true))

set.seed(123)
n_obs            <- 20
observed_weights <- rnorm(n_obs, mean = final_weight_true,
                          sd = final_weight_true * 0.05)  # CV = 5 %

cat(sprintf("Pesos observados (n=%d): media=%.1f g, sd=%.1f g\n",
            n_obs, mean(observed_weights), sd(observed_weights)))

result_boot <- run_fb4(
  x                = bio_chinook,
  fit_to           = "Weight",
  observed_weights = observed_weights,
  strategy         = "bootstrap",
  n_bootstrap      = 500,
  upper            = 2,
  parallel         = TRUE,   # ← TRUE solo si future+furrr están instalados
  confidence_level = 0.95,
  verbose          = FALSE
)

# NOTA parallel = TRUE:
#   En Windows (multisession) los workers necesitan acceso a funciones internas
#   del paquete. Se exportan via furrr_options(globals=...) desde la versión
#   actual, pero requiere future y furrr instalados. Usar FALSE en caso de duda.

cat("Clase del resultado:", class(result_boot), "\n")
print(result_boot)

cat("\n>> Plot incertidumbre (bootstrap)\n")
plot(result_boot, type = "uncertainty")


# ==============================================================================
# 8. MLE — MÁXIMA VEROSIMILITUD SOBRE PESOS OBSERVADOS
# ==============================================================================

cat("\n\n=== MLE: estimación de p con pesos observados ===\n")

result_mle <- run_fb4(
  x                 = bio_chinook,
  fit_to            = "Weight",
  observed_weights  = observed_weights,
  strategy          = "mle",
  estimate_sigma    = TRUE,
  confidence_level  = 0.95,
  compute_profile   = TRUE,
  profile_grid_size = 60,
  verbose           = FALSE
)

cat("Clase del resultado:", class(result_mle), "\n")
print(result_mle)

cat(sprintf("\np_value MLE    : %.4f\n",   result_mle$p_estimate))
cat(sprintf("SE             : %.4f\n",   result_mle$p_se))
cat(sprintf("IC 95%%         : [%.4f, %.4f]\n",
            result_mle$p_ci_lower, result_mle$p_ci_upper))
cat(sprintf("sigma estimado : %.4f\n",   result_mle$sigma_estimate))
cat(sprintf("Log-likelihood : %.2f\n",   result_mle$log_likelihood))
cat(sprintf("AIC            : %.2f\n",   result_mle$aic))
cat(sprintf("Convergió      : %s\n",     result_mle$converged))

cat("\n>> Plot incertidumbre (MLE)\n")
plot(result_mle, type = "uncertainty")


# ==============================================================================
# 9. ANÁLISIS DE RESULTADOS
# ==============================================================================

cat("\n\n=== Análisis de patrones de crecimiento ===\n")
growth_analysis <- analyze_growth_patterns(result_bsearch)
print(growth_analysis)

cat("\n=== Análisis de rendimiento de alimentación ===\n")
feeding_analysis <- analyze_feeding_performance(result_bsearch)
print(feeding_analysis)

cat("\n=== Análisis de presupuesto energético ===\n")
energy_analysis <- analyze_energy_budget(result_bsearch)
print(energy_analysis)

cat("\n=== Análisis nutricional completo ===\n")
# comprehensive_nutritional_analysis combina energy budget, N:P ratios
# y balance estequiométrico si hay datos disponibles
nutritional_analysis <- comprehensive_nutritional_analysis(result_bsearch)
print(nutritional_analysis)


# ==============================================================================
# 10. ANÁLISIS DE SENSIBILIDAD A LA TEMPERATURA
# ==============================================================================

cat("\n\n=== Sensibilidad a la temperatura ===\n")
# analyze_growth_temperature_sensitivity toma temperaturas ABSOLUTAS (°C)
# y una grilla de p_values — no offsets relativos.
# Usamos el rango real del dataset (1.3–14.6 °C) para que las simulaciones
# no salgan del espacio térmico conocido.
plot(bio_chinook,
     type             = "sensitivity",
     temperatures     = seq(2, 20, by = 2),   # °C absolutos
     p_values         = seq(0.3, 0.9, by = 0.1),
     simulation_days  = 365,
     verbose          = FALSE)


# ==============================================================================
# 11. GUARDAR GRÁFICOS A DISCO
# ==============================================================================

cat("\n\n=== Guardando gráficos ===\n")

# Dashboard simulación → PNG
png_path <- file.path(output_dir, "chinook_dashboard.png")
plot(result_bsearch, type = "dashboard", save_plot = png_path)
cat("Guardado:", png_path, "\n")

# Crecimiento → PDF
pdf_path <- file.path(output_dir, "chinook_growth.pdf")
plot(result_bsearch, type = "growth", save_plot = pdf_path)
cat("Guardado:", pdf_path, "\n")

# Setup (objeto Bioenergetic) → PNG
png_setup <- file.path(output_dir, "chinook_setup.png")
plot(bio_chinook, type = "dashboard", save_plot = png_setup)
cat("Guardado:", png_setup, "\n")

# Bootstrap uncertainty → PNG
if (!is.null(result_boot$bootstrap_p_values)) {
  png_boot <- file.path(output_dir, "chinook_bootstrap.png")
  plot(result_boot, type = "uncertainty", save_plot = png_boot)
  cat("Guardado:", png_boot, "\n")
}

# MLE uncertainty → PNG
if (!is.null(result_mle$p_estimate) && !is.na(result_mle$p_estimate)) {
  png_mle <- file.path(output_dir, "chinook_mle.png")
  plot(result_mle, type = "uncertainty", save_plot = png_mle)
  cat("Guardado:", png_mle, "\n")
}

cat("\n=== Demo completado ===\n")
cat("Archivos generados en:", output_dir, "\n")

