#' Funciones de Regeneración de Nutrientes para el Modelo FB4
#'
#' @name nutrient-regeneration
#' @aliases nutrient-regeneration
NULL

# ============================================================================
# FUNCIONES PRINCIPALES DE ASIGNACIÓN DE NUTRIENTES
# ============================================================================

#' Asignación de fósforo en el modelo bioenergético
#'
#' Calcula el balance de fósforo en consumo, crecimiento, excreción y egestion
#' basado en concentraciones en presas y depredador, y eficiencias de asimilación
#'
#' @param C Vector de consumo por tipo de presa (g/día)
#' @param p_conc_prey Vector de concentraciones de P en presas (g P/g peso húmedo)
#' @param AEp Vector de eficiencias de asimilación de P por presa (fracción 0-1)
#' @param weightgain Ganancia de peso del depredador (g/día)
#' @param p_conc_pred Concentración de P en depredador (g P/g peso húmedo)
#' @return Vector con c(P_consumido, P_crecimiento, P_excreción, P_egestion)
#' @export
#' @examples
#' # Ejemplo para pez que consume zooplancton e invertebrados
#' phosphorus_allocation(
#'   C = c(0.8, 0.5),                    # g/día de cada presa
#'   p_conc_prey = c(0.012, 0.008),      # g P/g en cada presa
#'   AEp = c(0.85, 0.90),                # eficiencia asimilación
#'   weightgain = 0.2,                   # g/día ganancia peso
#'   p_conc_pred = 0.015                 # g P/g en depredador
#' )
phosphorous_allocation <- function(C, p_conc_prey, AEp, weightgain, p_conc_pred) {

  # Validar entrada
  if (length(C) != length(p_conc_prey) || length(C) != length(AEp)) {
    stop("C, p_conc_prey y AEp deben tener la misma longitud")
  }

  if (any(AEp < 0 | AEp > 1)) {
    stop("Eficiencias de asimilación deben estar entre 0 y 1")
  }

  if (any(p_conc_prey < 0) || p_conc_pred < 0) {
    stop("Concentraciones de fósforo no pueden ser negativas")
  }

  # 1. Fósforo consumido por tipo de presa (g P/día)
  Cp_by_prey <- C * p_conc_prey

  # 2. Fósforo total consumido (g P/día)
  Cpsum <- sum(Cp_by_prey)

  # 3. Fósforo incorporado en crecimiento (g P/día)
  Gp <- weightgain * p_conc_pred

  # 4. Fósforo asimilado total (g P/día)
  P_assimilated <- sum(AEp * Cp_by_prey)

  # 5. Fósforo excretado (g P/día)
  # P excretado = P asimilado - P crecimiento
  Up <- P_assimilated - Gp

  # Verificar que excreción no sea negativa
  if (Up < 0) {
    warning("Excreción de fósforo negativa calculada, estableciendo a 0")
    Up <- 0
  }

  # 6. Fósforo en egestion (g P/día)
  # P egestion = P consumido - P asimilado
  Fp <- Cpsum - P_assimilated

  # Verificar balance: Consumido = Crecimiento + Excretado + Egestion
  balance_check <- abs(Cpsum - (Gp + Up + Fp))
  if (balance_check > 1e-10) {
    warning("Desbalance en fósforo detectado: ", round(balance_check, 10))
  }

  return(c(Cpsum, Gp, Up, Fp))
}

#' Asignación de nitrógeno en el modelo bioenergético
#'
#' Calcula el balance de nitrógeno en consumo, crecimiento, excreción y egestion
#' basado en concentraciones en presas y depredador, y eficiencias de asimilación
#'
#' @param C Vector de consumo por tipo de presa (g/día)
#' @param n_conc_prey Vector de concentraciones de N en presas (g N/g peso húmedo)
#' @param AEn Vector de eficiencias de asimilación de N por presa (fracción 0-1)
#' @param weightgain Ganancia de peso del depredador (g/día)
#' @param n_conc_pred Concentración de N en depredador (g N/g peso húmedo)
#' @return Vector con c(N_consumido, N_crecimiento, N_excreción, N_egestion)
#' @export
#' @examples
#' # Ejemplo para pez que consume zooplancton e invertebrados
#' nitrogen_allocation(
#'   C = c(0.8, 0.5),                    # g/día de cada presa
#'   n_conc_prey = c(0.095, 0.072),      # g N/g en cada presa
#'   AEn = c(0.88, 0.92),                # eficiencia asimilación
#'   weightgain = 0.2,                   # g/día ganancia peso
#'   n_conc_pred = 0.098                 # g N/g en depredador
#' )
nitrogen_allocation <- function(C, n_conc_prey, AEn, weightgain, n_conc_pred) {

  # Validar entrada
  if (length(C) != length(n_conc_prey) || length(C) != length(AEn)) {
    stop("C, n_conc_prey y AEn deben tener la misma longitud")
  }

  if (any(AEn < 0 | AEn > 1)) {
    stop("Eficiencias de asimilación deben estar entre 0 y 1")
  }

  if (any(n_conc_prey < 0) || n_conc_pred < 0) {
    stop("Concentraciones de nitrógeno no pueden ser negativas")
  }

  # 1. Nitrógeno consumido por tipo de presa (g N/día)
  Cn_by_prey <- C * n_conc_prey

  # 2. Nitrógeno total consumido (g N/día)
  Cnsum <- sum(Cn_by_prey)

  # 3. Nitrógeno incorporado en crecimiento (g N/día)
  Gn <- weightgain * n_conc_pred

  # 4. Nitrógeno asimilado total (g N/día)
  N_assimilated <- sum(AEn * Cn_by_prey)

  # 5. Nitrógeno excretado (g N/día)
  # N excretado = N asimilado - N crecimiento
  Un <- N_assimilated - Gn

  # Verificar que excreción no sea negativa
  if (Un < 0) {
    warning("Excreción de nitrógeno negativa calculada, estableciendo a 0")
    Un <- 0
  }

  # 6. Nitrógeno en egestion (g N/día)
  # N egestion = N consumido - N asimilado
  Fn <- Cnsum - N_assimilated

  # Verificar balance: Consumido = Crecimiento + Excretado + Egestion
  balance_check <- abs(Cnsum - (Gn + Un + Fn))
  if (balance_check > 1e-10) {
    warning("Desbalance en nitrógeno detectado: ", round(balance_check, 10))
  }

  return(c(Cnsum, Gn, Un, Fn))
}

# ============================================================================
# FUNCIONES DE ANÁLISIS DE RATIOS N:P
# ============================================================================

#' Calcular ratios N:P para todos los procesos
#'
#' Calcula ratios molares y de masa N:P para consumo, crecimiento, excreción y egestion
#'
#' @param nitrogen_fluxes Vector resultado de nitrogen_allocation
#' @param phosphorus_fluxes Vector resultado de phosphorous_allocation
#' @param ratio_type Tipo de ratio ("mass" o "molar")
#' @return Lista con ratios N:P para cada proceso
#' @export
#' @examples
#' # Calcular flujos de nutrientes
#' N_fluxes <- nitrogen_allocation(c(0.8, 0.5), c(0.095, 0.072),
#'                                c(0.88, 0.92), 0.2, 0.098)
#' P_fluxes <- phosphorous_allocation(c(0.8, 0.5), c(0.012, 0.008),
#'                                   c(0.85, 0.90), 0.2, 0.015)
#'
#' # Calcular ratios
#' ratios <- calculate_np_ratios(N_fluxes, P_fluxes, ratio_type = "molar")
calculate_np_ratios <- function(nitrogen_fluxes, phosphorus_fluxes, ratio_type = "mass") {

  # Validar entrada
  if (length(nitrogen_fluxes) != 4 || length(phosphorus_fluxes) != 4) {
    stop("Los vectores de flujos deben tener 4 elementos cada uno")
  }

  if (!ratio_type %in% c("mass", "molar")) {
    stop("ratio_type debe ser 'mass' o 'molar'")
  }

  # Nombres de los procesos
  process_names <- c("Consumption", "Growth", "Excretion", "Egestion")

  # Factores de conversión para ratios molares
  # Peso atómico: N = 14.007, P = 30.974
  atomic_weight_N <- 14.007
  atomic_weight_P <- 30.974

  # Calcular ratios
  ratios <- numeric(4)

  for (i in 1:4) {
    N_flux <- nitrogen_fluxes[i]
    P_flux <- phosphorus_fluxes[i]

    if (P_flux == 0) {
      ratios[i] <- Inf  # Ratio infinito cuando P = 0
      warning(paste("P flux es 0 para", process_names[i], "- ratio = Inf"))
    } else {
      if (ratio_type == "mass") {
        # Ratio de masa (g N / g P)
        ratios[i] <- N_flux / P_flux
      } else {
        # Ratio molar (mol N / mol P)
        mol_N <- N_flux / atomic_weight_N
        mol_P <- P_flux / atomic_weight_P
        ratios[i] <- mol_N / mol_P
      }
    }
  }

  # Crear lista con nombres
  names(ratios) <- process_names

  # Agregar información adicional
  result <- list(
    ratios = ratios,
    ratio_type = ratio_type,
    nitrogen_fluxes = nitrogen_fluxes,
    phosphorus_fluxes = phosphorus_fluxes,
    process_names = process_names
  )

  return(result)
}

#' Comparar ratios N:P con valores de referencia (Redfield)
#'
#' Compara ratios calculados con el ratio de Redfield y otros valores de referencia
#'
#' @param np_ratios Output de calculate_np_ratios
#' @param redfield_ratio Ratio de Redfield de referencia (16:1 molar, 7.2:1 masa)
#' @return Data frame con comparación
#' @export
compare_with_redfield <- function(np_ratios, redfield_ratio = NULL) {

  # Valores de Redfield por defecto
  if (is.null(redfield_ratio)) {
    if (np_ratios$ratio_type == "molar") {
      redfield_ratio <- 16  # 16:1 molar
    } else {
      redfield_ratio <- 7.2  # aproximadamente 7.2:1 en masa
    }
  }

  # Crear data frame de comparación
  comparison <- data.frame(
    Process = np_ratios$process_names,
    NP_Ratio = np_ratios$ratios,
    Redfield_Ratio = redfield_ratio,
    Difference = np_ratios$ratios - redfield_ratio,
    Percent_Difference = ((np_ratios$ratios - redfield_ratio) / redfield_ratio) * 100,
    stringsAsFactors = FALSE
  )

  # Agregar interpretación
  comparison$Interpretation <- ifelse(
    comparison$NP_Ratio > redfield_ratio,
    "Rico en N relativo a P",
    "Pobre en N relativo a P"
  )

  return(comparison)
}

# ============================================================================
# FUNCIONES DE VALIDACIÓN Y ANÁLISIS
# ============================================================================

#' Validar concentraciones de nutrientes
#'
#' Verifica que las concentraciones estén en rangos biológicamente realistas
#'
#' @param nutrient_concentrations Lista con concentraciones de N y P
#' @param organism_type Tipo de organismo ("predator", "zooplankton", "invertebrates", "fish")
#' @return Lista con validación
#' @export
validate_nutrient_concentrations <- function(nutrient_concentrations, organism_type = "predator") {

  warnings <- character()
  errors <- character()
  valid <- TRUE

  # Rangos típicos por tipo de organismo (g/g peso húmedo)
  typical_ranges <- list(
    predator = list(
      nitrogen = c(0.080, 0.120),
      phosphorus = c(0.012, 0.020),
      np_ratio_mass = c(4, 10)
    ),
    zooplankton = list(
      nitrogen = c(0.070, 0.110),
      phosphorus = c(0.008, 0.015),
      np_ratio_mass = c(6, 12)
    ),
    invertebrates = list(
      nitrogen = c(0.060, 0.100),
      phosphorus = c(0.006, 0.012),
      np_ratio_mass = c(7, 15)
    ),
    fish = list(
      nitrogen = c(0.085, 0.115),
      phosphorus = c(0.013, 0.018),
      np_ratio_mass = c(5, 9)
    )
  )

  if (!organism_type %in% names(typical_ranges)) {
    warnings <- c(warnings, paste("Tipo de organismo no reconocido:", organism_type))
    organism_type <- "predator"  # Usar como defecto
  }

  ranges <- typical_ranges[[organism_type]]

  # Validar nitrógeno
  if ("nitrogen" %in% names(nutrient_concentrations)) {
    n_conc <- nutrient_concentrations$nitrogen
    if (any(n_conc < 0)) {
      errors <- c(errors, "Concentraciones de nitrógeno negativas")
      valid <- FALSE
    }
    if (any(n_conc < ranges$nitrogen[1] | n_conc > ranges$nitrogen[2])) {
      warnings <- c(warnings,
                    paste("Concentraciones de N fuera de rango típico para", organism_type))
    }
  }

  # Validar fósforo
  if ("phosphorus" %in% names(nutrient_concentrations)) {
    p_conc <- nutrient_concentrations$phosphorus
    if (any(p_conc < 0)) {
      errors <- c(errors, "Concentraciones de fósforo negativas")
      valid <- FALSE
    }
    if (any(p_conc < ranges$phosphorus[1] | p_conc > ranges$phosphorus[2])) {
      warnings <- c(warnings,
                    paste("Concentraciones de P fuera de rango típico para", organism_type))
    }
  }

  # Validar ratio N:P si ambos están presentes
  if ("nitrogen" %in% names(nutrient_concentrations) &&
      "phosphorus" %in% names(nutrient_concentrations)) {
    n_conc <- nutrient_concentrations$nitrogen
    p_conc <- nutrient_concentrations$phosphorus

    np_ratios <- n_conc / p_conc
    if (any(np_ratios < ranges$np_ratio_mass[1] | np_ratios > ranges$np_ratio_mass[2])) {
      warnings <- c(warnings,
                    paste("Ratios N:P fuera de rango típico para", organism_type))
    }
  }

  return(list(
    valid = valid,
    warnings = warnings,
    errors = errors,
    n_warnings = length(warnings),
    n_errors = length(errors),
    typical_ranges = ranges
  ))
}

#' Calcular eficiencias de retención de nutrientes
#'
#' Calcula eficiencias de asimilación y retención neta para N y P
#'
#' @param nitrogen_fluxes Vector resultado de nitrogen_allocation
#' @param phosphorus_fluxes Vector resultado de phosphorous_allocation
#' @return Lista con eficiencias calculadas
#' @export
calculate_nutrient_efficiencies <- function(nitrogen_fluxes, phosphorus_fluxes) {

  # Extraer componentes
  N_consumed <- nitrogen_fluxes[1]
  N_growth <- nitrogen_fluxes[2]
  N_excretion <- nitrogen_fluxes[3]
  N_egestion <- nitrogen_fluxes[4]

  P_consumed <- phosphorus_fluxes[1]
  P_growth <- phosphorus_fluxes[2]
  P_excretion <- phosphorus_fluxes[3]
  P_egestion <- phosphorus_fluxes[4]

  # Calcular eficiencias para nitrógeno
  N_assimilation_efficiency <- if (N_consumed > 0) {
    (N_consumed - N_egestion) / N_consumed
  } else { 0 }

  N_retention_efficiency <- if (N_consumed > 0) {
    N_growth / N_consumed
  } else { 0 }

  N_excretion_rate <- if (N_consumed > 0) {
    N_excretion / N_consumed
  } else { 0 }

  # Calcular eficiencias para fósforo
  P_assimilation_efficiency <- if (P_consumed > 0) {
    (P_consumed - P_egestion) / P_consumed
  } else { 0 }

  P_retention_efficiency <- if (P_consumed > 0) {
    P_growth / P_consumed
  } else { 0 }

  P_excretion_rate <- if (P_consumed > 0) {
    P_excretion / P_consumed
  } else { 0 }

  return(list(
    nitrogen = list(
      assimilation_efficiency = N_assimilation_efficiency,
      retention_efficiency = N_retention_efficiency,
      excretion_rate = N_excretion_rate
    ),
    phosphorus = list(
      assimilation_efficiency = P_assimilation_efficiency,
      retention_efficiency = P_retention_efficiency,
      excretion_rate = P_excretion_rate
    ),
    # Eficiencias relativas
    relative_N_retention = N_retention_efficiency / P_retention_efficiency,
    relative_N_excretion = N_excretion_rate / P_excretion_rate
  ))
}

# ============================================================================
# SIMULACIÓN TEMPORAL DE NUTRIENTES
# ============================================================================

#' Simular dinámica de nutrientes en el tiempo
#'
#' Simula los flujos de N y P durante una serie temporal
#'
#' @param days Vector de días
#' @param consumption_data Matriz de consumo por día y presa
#' @param weight_gain Vector de ganancia de peso por día
#' @param nutrient_data Lista con datos de nutrientes por día
#' @return Data frame con dinámica temporal de nutrientes
#' @export
simulate_nutrient_dynamics <- function(days, consumption_data, weight_gain, nutrient_data) {

  n_days <- length(days)
  n_prey <- ncol(consumption_data)

  # Inicializar resultados
  results <- data.frame(
    Day = days,
    Weight_Gain = weight_gain,
    N_Consumed = numeric(n_days),
    N_Growth = numeric(n_days),
    N_Excretion = numeric(n_days),
    N_Egestion = numeric(n_days),
    P_Consumed = numeric(n_days),
    P_Growth = numeric(n_days),
    P_Excretion = numeric(n_days),
    P_Egestion = numeric(n_days),
    NP_Ratio_Consumption = numeric(n_days),
    NP_Ratio_Excretion = numeric(n_days),
    NP_Ratio_Egestion = numeric(n_days),
    Cumulative_N_Excretion = numeric(n_days),
    Cumulative_P_Excretion = numeric(n_days)
  )

  # Variables acumulativas
  cum_N_excretion <- 0
  cum_P_excretion <- 0

  # Simulación diaria
  for (i in 1:n_days) {
    # Datos del día
    daily_consumption <- consumption_data[i, ]
    daily_weight_gain <- weight_gain[i]

    # Concentraciones en presas para este día
    n_conc_prey <- nutrient_data$nitrogen_prey_concentration[i, ]
    p_conc_prey <- nutrient_data$phosphorus_prey_concentration[i, ]

    # Eficiencias de asimilación para este día
    n_ae <- nutrient_data$nitrogen_ae[i, ]
    p_ae <- nutrient_data$phosphorus_ae[i, ]

    # Concentraciones en depredador para este día
    n_conc_pred <- nutrient_data$nitrogen_predator_concentration[i]
    p_conc_pred <- nutrient_data$phosphorus_predator_concentration[i]

    # Calcular flujos de nitrógeno
    N_fluxes <- nitrogen_allocation(
      C = daily_consumption,
      n_conc_prey = n_conc_prey,
      AEn = n_ae,
      weightgain = daily_weight_gain,
      n_conc_pred = n_conc_pred
    )

    # Calcular flujos de fósforo
    P_fluxes <- phosphorous_allocation(
      C = daily_consumption,
      p_conc_prey = p_conc_prey,
      AEp = p_ae,
      weightgain = daily_weight_gain,
      p_conc_pred = p_conc_pred
    )

    # Guardar resultados diarios
    results$N_Consumed[i] <- N_fluxes[1]
    results$N_Growth[i] <- N_fluxes[2]
    results$N_Excretion[i] <- N_fluxes[3]
    results$N_Egestion[i] <- N_fluxes[4]

    results$P_Consumed[i] <- P_fluxes[1]
    results$P_Growth[i] <- P_fluxes[2]
    results$P_Excretion[i] <- P_fluxes[3]
    results$P_Egestion[i] <- P_fluxes[4]

    # Calcular ratios N:P
    results$NP_Ratio_Consumption[i] <- if (P_fluxes[1] > 0) N_fluxes[1] / P_fluxes[1] else Inf
    results$NP_Ratio_Excretion[i] <- if (P_fluxes[3] > 0) N_fluxes[3] / P_fluxes[3] else Inf
    results$NP_Ratio_Egestion[i] <- if (P_fluxes[4] > 0) N_fluxes[4] / P_fluxes[4] else Inf

    # Actualizar acumulados
    cum_N_excretion <- cum_N_excretion + N_fluxes[3]
    cum_P_excretion <- cum_P_excretion + P_fluxes[3]

    results$Cumulative_N_Excretion[i] <- cum_N_excretion
    results$Cumulative_P_Excretion[i] <- cum_P_excretion
  }

  return(results)
}

# ============================================================================
# FUNCIONES DE VISUALIZACIÓN
# ============================================================================

#' Graficar dinámica de nutrientes
#'
#' Crea gráficos de los flujos de nutrientes en el tiempo
#'
#' @param simulation_results Output de simulate_nutrient_dynamics
#' @param plot_type Tipo de gráfico ("fluxes", "ratios", "cumulative")
#' @return Gráfico de dinámica de nutrientes
#' @export
plot_nutrient_dynamics <- function(simulation_results, plot_type = "fluxes") {

  if (plot_type == "fluxes") {
    # Gráfico de flujos de N y P
    par(mfrow = c(2, 2))

    # Nitrógeno
    plot(simulation_results$Day, simulation_results$N_Consumed,
         type = "l", lwd = 2, col = "blue",
         xlab = "Día", ylab = "N (g/día)",
         main = "Flujos de Nitrógeno")
    lines(simulation_results$Day, simulation_results$N_Growth, lwd = 2, col = "green")
    lines(simulation_results$Day, simulation_results$N_Excretion, lwd = 2, col = "red")
    lines(simulation_results$Day, simulation_results$N_Egestion, lwd = 2, col = "orange")
    legend("topright", legend = c("Consumido", "Crecimiento", "Excreción", "Egestion"),
           col = c("blue", "green", "red", "orange"), lwd = 2, cex = 0.8)
    grid()

    # Fósforo
    plot(simulation_results$Day, simulation_results$P_Consumed,
         type = "l", lwd = 2, col = "blue",
         xlab = "Día", ylab = "P (g/día)",
         main = "Flujos de Fósforo")
    lines(simulation_results$Day, simulation_results$P_Growth, lwd = 2, col = "green")
    lines(simulation_results$Day, simulation_results$P_Excretion, lwd = 2, col = "red")
    lines(simulation_results$Day, simulation_results$P_Egestion, lwd = 2, col = "orange")
    legend("topright", legend = c("Consumido", "Crecimiento", "Excreción", "Egestion"),
           col = c("blue", "green", "red", "orange"), lwd = 2, cex = 0.8)
    grid()

    # Excreción comparativa
    plot(simulation_results$Day, simulation_results$N_Excretion,
         type = "l", lwd = 2, col = "blue",
         xlab = "Día", ylab = "Excreción (g/día)",
         main = "Excreción de Nutrientes")
    lines(simulation_results$Day, simulation_results$P_Excretion * 10, lwd = 2, col = "red")
    legend("topright", legend = c("N", "P × 10"), col = c("blue", "red"), lwd = 2)
    grid()

    # Ganancia de peso
    plot(simulation_results$Day, simulation_results$Weight_Gain,
         type = "l", lwd = 2, col = "darkgreen",
         xlab = "Día", ylab = "Ganancia Peso (g/día)",
         main = "Ganancia de Peso")
    grid()

    par(mfrow = c(1, 1))

  } else if (plot_type == "ratios") {
    # Gráfico de ratios N:P
    par(mfrow = c(1, 3))

    plot(simulation_results$Day, simulation_results$NP_Ratio_Consumption,
         type = "l", lwd = 2, col = "blue",
         xlab = "Día", ylab = "Ratio N:P (masa)",
         main = "N:P en Consumo")
    abline(h = 7.2, lty = 2, col = "red")  # Ratio Redfield
    text(max(simulation_results$Day) * 0.7, 7.5, "Redfield", col = "red")
    grid()

    plot(simulation_results$Day, simulation_results$NP_Ratio_Excretion,
         type = "l", lwd = 2, col = "red",
         xlab = "Día", ylab = "Ratio N:P (masa)",
         main = "N:P en Excreción")
    abline(h = 7.2, lty = 2, col = "gray")
    grid()

    plot(simulation_results$Day, simulation_results$NP_Ratio_Egestion,
         type = "l", lwd = 2, col = "orange",
         xlab = "Día", ylab = "Ratio N:P (masa)",
         main = "N:P en Egestion")
    abline(h = 7.2, lty = 2, col = "gray")
    grid()

    par(mfrow = c(1, 1))

  } else if (plot_type == "cumulative") {
    # Gráfico de excreción acumulativa
    par(mfrow = c(1, 2))

    plot(simulation_results$Day, simulation_results$Cumulative_N_Excretion,
         type = "l", lwd = 2, col = "blue",
         xlab = "Día", ylab = "N Excretado Acumulativo (g)",
         main = "Excreción Acumulativa de Nitrógeno")
    grid()

    plot(simulation_results$Day, simulation_results$Cumulative_P_Excretion,
         type = "l", lwd = 2, col = "red",
         xlab = "Día", ylab = "P Excretado Acumulativo (g)",
         main = "Excreción Acumulativa de Fósforo")
    grid()

    par(mfrow = c(1, 1))
  }
}

#' Crear gráfico de barras de balance de nutrientes
#'
#' Gráfico de barras mostrando el destino de N y P consumidos
#'
#' @param nitrogen_fluxes Vector resultado de nitrogen_allocation
#' @param phosphorus_fluxes Vector resultado de phosphorous_allocation
#' @return Gráfico de barras del balance de nutrientes
#' @export
plot_nutrient_balance_barplot <- function(nitrogen_fluxes, phosphorus_fluxes) {

  # Preparar datos (excluyendo consumo total)
  N_data <- nitrogen_fluxes[2:4]  # Crecimiento, Excreción, Egestion
  P_data <- phosphorus_fluxes[2:4]

  # Convertir a porcentajes del consumo
  N_percentages <- (N_data / nitrogen_fluxes[1]) * 100
  P_percentages <- (P_data / phosphorus_fluxes[1]) * 100

  # Crear matriz para barplot
  balance_matrix <- rbind(N_percentages, P_percentages)
  rownames(balance_matrix) <- c("Nitrógeno", "Fósforo")
  colnames(balance_matrix) <- c("Crecimiento", "Excreción", "Egestion")

  # Crear gráfico
  barplot(balance_matrix, beside = TRUE,
          col = c("lightblue", "lightcoral"),
          main = "Balance de Nutrientes (%)",
          ylab = "Porcentaje del Consumo",
          legend.text = TRUE,
          args.legend = list(x = "topright"))

  grid()

  invisible(balance_matrix)
}

# ============================================================================
# FUNCIONES DE ANÁLISIS ECOSISTÉMICO
# ============================================================================

#' Calcular impacto ecosistémico de excreción de nutrientes
#'
#' Estima el impacto de la excreción de nutrientes por peces en el ecosistema
#'
#' @param daily_excretion Lista con excreción diaria de N y P
#' @param fish_biomass Biomasa total de peces (g)
#' @param water_volume Volumen de agua del sistema (L)
#' @param days Número de días para el cálculo
#' @return Lista con concentraciones e impactos estimados
#' @export
calculate_ecosystem_impact <- function(daily_excretion, fish_biomass, water_volume, days) {

  # Excreción total por día para toda la población
  total_N_excretion_per_day <- daily_excretion$nitrogen * fish_biomass  # g N/día
  total_P_excretion_per_day <- daily_excretion$phosphorus * fish_biomass  # g P/día

  # Excreción total durante el período
  total_N_excretion <- total_N_excretion_per_day * days  # g N
  total_P_excretion <- total_P_excretion_per_day * days  # g P

  # Concentraciones en el agua (asumiendo mezcla completa)
  N_concentration_mgL <- (total_N_excretion / water_volume) * 1000  # mg N/L
  P_concentration_mgL <- (total_P_excretion / water_volume) * 1000  # mg P/L

  # Concentraciones diarias
  daily_N_concentration <- (total_N_excretion_per_day / water_volume) * 1000  # mg N/L/día
  daily_P_concentration <- (total_P_excretion_per_day / water_volume) * 1000  # mg P/L/día

  # Ratio N:P en excreción
  excretion_NP_ratio <- total_N_excretion / total_P_excretion

  # Evaluación del estado trófico (criterios simplificados)
  trophic_status <- "Unknown"
  if (P_concentration_mgL < 0.01) {
    trophic_status <- "Oligotrófico"
  } else if (P_concentration_mgL < 0.05) {
    trophic_status <- "Mesotrófico"
  } else {
    trophic_status <- "Eutrófico"
  }

  return(list(
    # Excreción total
    total_N_excretion_g = total_N_excretion,
    total_P_excretion_g = total_P_excretion,

    # Excreción diaria
    daily_N_excretion_g = total_N_excretion_per_day,
    daily_P_excretion_g = total_P_excretion_per_day,

    # Concentraciones en agua
    N_concentration_mgL = N_concentration_mgL,
    P_concentration_mgL = P_concentration_mgL,
    daily_N_addition_mgL = daily_N_concentration,
    daily_P_addition_mgL = daily_P_concentration,

    # Ratios y evaluación
    excretion_NP_ratio = excretion_NP_ratio,
    trophic_status = trophic_status,

    # Información del sistema
    fish_biomass_g = fish_biomass,
    water_volume_L = water_volume,
    simulation_days = days
  ))
}

#' Comparar excreción de nutrientes entre especies
#'
#' Compara tasas de excreción específicas entre diferentes especies o condiciones
#'
#' @param species_data Lista con datos de excreción por especie
#' @param normalize_by Normalizar por ("biomass", "individual", "consumption")
#' @return Data frame con comparación entre especies
#' @export
compare_species_nutrient_excretion <- function(species_data, normalize_by = "biomass") {

  species_names <- names(species_data)

  results <- data.frame(
    Species = species_names,
    N_Excretion_Rate = numeric(length(species_names)),
    P_Excretion_Rate = numeric(length(species_names)),
    NP_Excretion_Ratio = numeric(length(species_names)),
    N_Efficiency = numeric(length(species_names)),
    P_Efficiency = numeric(length(species_names)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(species_names)) {
    species <- species_names[i]
    data <- species_data[[species]]

    # Calcular tasas según normalización
    if (normalize_by == "biomass") {
      # Por unidad de biomasa (g nutriente/g pez/día)
      normalizer <- data$average_biomass
      units <- "g/g/día"
    } else if (normalize_by == "individual") {
      # Por individuo (g nutriente/individuo/día)
      normalizer <- 1
      units <- "g/ind/día"
    } else if (normalize_by == "consumption") {
      # Por unidad de consumo (g nutriente/g alimento consumido)
      normalizer <- data$total_consumption
      units <- "g/g alimento"
    }

    results$N_Excretion_Rate[i] <- data$total_N_excretion / normalizer
    results$P_Excretion_Rate[i] <- data$total_P_excretion / normalizer
    results$NP_Excretion_Ratio[i] <- data$total_N_excretion / data$total_P_excretion

    # Eficiencias de retención
    results$N_Efficiency[i] <- data$N_growth / data$N_consumed
    results$P_Efficiency[i] <- data$P_growth / data$P_consumed
  }

  # Agregar metadatos
  attr(results, "normalization") <- normalize_by
  attr(results, "units") <- units

  return(results)
}

# ============================================================================
# FUNCIONES DE CALIBRACIÓN Y OPTIMIZACIÓN
# ============================================================================

#' Calibrar eficiencias de asimilación de nutrientes
#'
#' Ajusta eficiencias de asimilación para que coincidan con excreción observada
#'
#' @param consumption Vector de consumo por presa
#' @param prey_concentrations Concentraciones en presas
#' @param predator_concentration Concentración en depredador
#' @param weight_gain Ganancia de peso
#' @param observed_excretion Excreción observada
#' @param initial_efficiency Eficiencias iniciales
#' @param nutrient Tipo de nutriente ("nitrogen" o "phosphorus")
#' @return Lista con eficiencias calibradas
#' @export
calibrate_nutrient_assimilation <- function(consumption, prey_concentrations,
                                            predator_concentration, weight_gain,
                                            observed_excretion, initial_efficiency,
                                            nutrient = "nitrogen") {

  # Función objetivo para minimizar
  objective_function <- function(efficiencies) {

    # Asegurar que eficiencias estén en rango válido
    efficiencies <- pmax(0.1, pmin(0.99, efficiencies))

    # Calcular excreción predicha
    if (nutrient == "nitrogen") {
      fluxes <- nitrogen_allocation(consumption, prey_concentrations,
                                    efficiencies, weight_gain, predator_concentration)
    } else {
      fluxes <- phosphorous_allocation(consumption, prey_concentrations,
                                       efficiencies, weight_gain, predator_concentration)
    }

    predicted_excretion <- fluxes[3]  # Excreción es el 3er elemento

    # Error cuadrático
    error <- (predicted_excretion - observed_excretion)^2

    return(error)
  }

  # Optimización
  if (requireNamespace("stats", quietly = TRUE)) {
    optimization <- stats::optim(
      par = initial_efficiency,
      fn = objective_function,
      method = "L-BFGS-B",
      lower = rep(0.1, length(initial_efficiency)),
      upper = rep(0.99, length(initial_efficiency))
    )

    calibrated_efficiencies <- optimization$par
    final_error <- optimization$value
    converged <- optimization$convergence == 0

  } else {
    # Método simple si optim no está disponible
    warning("Paquete 'stats' no disponible, usando eficiencias iniciales")
    calibrated_efficiencies <- initial_efficiency
    final_error <- objective_function(initial_efficiency)
    converged <- FALSE
  }

  # Calcular flujos finales
  if (nutrient == "nitrogen") {
    final_fluxes <- nitrogen_allocation(consumption, prey_concentrations,
                                        calibrated_efficiencies, weight_gain,
                                        predator_concentration)
  } else {
    final_fluxes <- phosphorous_allocation(consumption, prey_concentrations,
                                           calibrated_efficiencies, weight_gain,
                                           predator_concentration)
  }

  return(list(
    calibrated_efficiencies = calibrated_efficiencies,
    initial_efficiencies = initial_efficiency,
    observed_excretion = observed_excretion,
    predicted_excretion = final_fluxes[3],
    final_error = final_error,
    converged = converged,
    final_fluxes = final_fluxes,
    nutrient_type = nutrient
  ))
}

#' Función de ejemplo integrado de nutrientes
#'
#' Ejemplo completo que muestra el uso de las funciones de nutrientes
#'
#' @param consumption_data Datos de consumo por presa
#' @param weight_gain Ganancia de peso diaria
#' @param prey_N_concentrations Concentraciones de N en presas
#' @param prey_P_concentrations Concentraciones de P en presas
#' @param predator_N_concentration Concentración de N en depredador
#' @param predator_P_concentration Concentración de P en depredador
#' @param N_assimilation_efficiencies Eficiencias de asimilación de N
#' @param P_assimilation_efficiencies Eficiencias de asimilación de P
#' @return Lista con análisis completo de nutrientes
#' @export
#' @examples
#' \dontrun{
#' # Ejemplo de uso completo
#' example_result <- nutrient_analysis_example(
#'   consumption_data = c(0.8, 0.5),
#'   weight_gain = 0.15,
#'   prey_N_concentrations = c(0.095, 0.072),
#'   prey_P_concentrations = c(0.012, 0.008),
#'   predator_N_concentration = 0.098,
#'   predator_P_concentration = 0.015,
#'   N_assimilation_efficiencies = c(0.88, 0.92),
#'   P_assimilation_efficiencies = c(0.85, 0.90)
#' )
#' print(example_result)
#' }
nutrient_analysis_example <- function(consumption_data, weight_gain,
                                      prey_N_concentrations, prey_P_concentrations,
                                      predator_N_concentration, predator_P_concentration,
                                      N_assimilation_efficiencies, P_assimilation_efficiencies) {

  # 1. Calcular flujos de nutrientes
  N_fluxes <- nitrogen_allocation(
    C = consumption_data,
    n_conc_prey = prey_N_concentrations,
    AEn = N_assimilation_efficiencies,
    weightgain = weight_gain,
    n_conc_pred = predator_N_concentration
  )

  P_fluxes <- phosphorous_allocation(
    C = consumption_data,
    p_conc_prey = prey_P_concentrations,
    AEp = P_assimilation_efficiencies,
    weightgain = weight_gain,
    p_conc_pred = predator_P_concentration
  )

  # 2. Calcular ratios N:P
  np_ratios <- calculate_np_ratios(N_fluxes, P_fluxes, ratio_type = "mass")

  # 3. Comparar con Redfield
  redfield_comparison <- compare_with_redfield(np_ratios)

  # 4. Calcular eficiencias
  efficiencies <- calculate_nutrient_efficiencies(N_fluxes, P_fluxes)

  # 5. Validar concentraciones
  validation <- validate_nutrient_concentrations(
    list(nitrogen = predator_N_concentration, phosphorus = predator_P_concentration),
    organism_type = "predator"
  )

  # 6. Resumir resultados
  summary_table <- data.frame(
    Process = c("Consumption", "Growth", "Excretion", "Egestion"),
    Nitrogen_g = N_fluxes,
    Phosphorus_g = P_fluxes,
    NP_Ratio = np_ratios$ratios,
    stringsAsFactors = FALSE
  )

  return(list(
    summary_table = summary_table,
    nitrogen_fluxes = N_fluxes,
    phosphorus_fluxes = P_fluxes,
    np_ratios = np_ratios,
    redfield_comparison = redfield_comparison,
    efficiencies = efficiencies,
    validation = validation,
    input_parameters = list(
      consumption = consumption_data,
      weight_gain = weight_gain,
      prey_N = prey_N_concentrations,
      prey_P = prey_P_concentrations,
      predator_N = predator_N_concentration,
      predator_P = predator_P_concentration,
      N_ae = N_assimilation_efficiencies,
      P_ae = P_assimilation_efficiencies
    )
  ))
}
