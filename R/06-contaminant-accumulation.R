#' Funciones de Acumulación de Contaminantes para el Modelo FB4
#'
#' @name contaminant-accumulation
#' @aliases contaminant-accumulation
NULL

# ============================================================================
# MODELOS DE ACUMULACIÓN DE CONTAMINANTES
# ============================================================================

#' Modelo de concentración de contaminantes en depredador
#'
#' Función principal que calcula la dinámica de contaminantes usando diferentes
#' ecuaciones según la configuración del modelo
#'
#' @param R_O2 Respiración en g O2/g/día
#' @param C Vector de consumo por tipo de presa (g/día)
#' @param W Peso del pez (g)
#' @param Temperature Temperatura del agua (°C)
#' @param X_Prey Vector de concentraciones en presas (μg/g)
#' @param X_Pred Concentración actual en depredador (μg/g)
#' @param TEx Vector de eficiencias de transferencia por presa
#' @param X_ae Vector de eficiencias de asimilación por presa
#' @param Ew Eficiencia de captación branquial (para CONTEQ = 3)
#' @param Kbw Coeficiente de partición pez:agua (para CONTEQ = 3)
#' @param Cw_tot Concentración total en agua (mg/L, para CONTEQ = 3)
#' @param phi_DT Fracción disuelta del contaminante (para CONTEQ = 3)
#' @param DO_Sat_fr Saturación de oxígeno disuelto (fracción, para CONTEQ = 3)
#' @param CONTEQ Ecuación de contaminantes (1, 2, o 3)
#' @return Vector con c(Clearance, Uptake, Burden, X_Pred)
#' @export
#' @examples
#' # Modelo 1: Solo captación de alimento, sin eliminación
#' result1 <- pred_cont_conc(
#'   R_O2 = 0.01, C = c(0.5, 0.3), W = 100, Temperature = 20,
#'   X_Prey = c(2.0, 3.5), X_Pred = 1.5,
#'   TEx = c(0.95, 0.98), X_ae = c(0.8, 0.9),
#'   CONTEQ = 1
#' )
pred_cont_conc <- function(R_O2, C, W, Temperature, X_Prey, X_Pred, TEx, X_ae,
                           Ew = NULL, Kbw = NULL, Cw_tot = NULL, phi_DT = NULL,
                           DO_Sat_fr = NULL, CONTEQ) {

  # Validar entrada
  if (!CONTEQ %in% 1:3) {
    stop("CONTEQ debe ser 1, 2, o 3")
  }

  if (length(C) != length(X_Prey) || length(C) != length(TEx) || length(C) != length(X_ae)) {
    stop("C, X_Prey, TEx y X_ae deben tener la misma longitud")
  }

  # Calcular carga corporal inicial (μg)
  Burden <- X_Pred * W

  # Seleccionar modelo según CONTEQ
  if (CONTEQ == 1) {
    # Modelo 1: Solo captación de alimento, sin eliminación
    result <- contaminant_model_1(C, X_Prey, TEx, Burden)

  } else if (CONTEQ == 2) {
    # Modelo 2: Captación de alimento + eliminación dependiente de T y W
    result <- contaminant_model_2(C, W, Temperature, X_Prey, X_ae, Burden)

  } else if (CONTEQ == 3) {
    # Modelo 3: Arnot & Gobas (2004) - captación de agua y alimento + eliminación
    result <- contaminant_model_3(R_O2, C, W, Temperature, X_Prey, X_ae, Burden,
                                  Ew, Kbw, Cw_tot, phi_DT, DO_Sat_fr)
  }

  # Actualizar concentración en depredador
  result[4] <- result[3] / W  # Burden / W = concentración

  return(result)
}

# ============================================================================
# MODELO 1: SOLO CAPTACIÓN DE ALIMENTO
# ============================================================================

#' Modelo de contaminantes 1 - Solo captación de alimento
#'
#' Modelo simple sin eliminación, solo acumulación desde el alimento
#'
#' @param C Vector de consumo por presa (g/día)
#' @param X_Prey Vector de concentraciones en presas (μg/g)
#' @param TEx Vector de eficiencias de transferencia
#' @param Burden Carga corporal actual (μg)
#' @return Vector con c(Clearance, Uptake, Burden, NA)
#' @export
contaminant_model_1 <- function(C, X_Prey, TEx, Burden) {

  # Captación desde alimento (μg/día)
  Uptake <- sum(C * X_Prey * TEx)

  # Sin eliminación
  Clearance <- 0

  # Acumulación neta
  Accumulation <- Uptake - Clearance

  # Nueva carga corporal
  New_Burden <- Burden + Accumulation

  return(c(Clearance, Uptake, New_Burden, NA))
}

# ============================================================================
# MODELO 2: CAPTACIÓN + ELIMINACIÓN (TEMPERATURA Y PESO)
# ============================================================================

#' Modelo de contaminantes 2 - Con eliminación
#'
#' Modelo con captación de alimento y eliminación dependiente de temperatura y peso
#' Basado en Trudel & Rasmussen (1997) para MeHg
#'
#' @param C Vector de consumo por presa (g/día)
#' @param W Peso del pez (g)
#' @param Temperature Temperatura del agua (°C)
#' @param X_Prey Vector de concentraciones en presas (μg/g)
#' @param X_ae Vector de eficiencias de asimilación
#' @param Burden Carga corporal actual (μg)
#' @return Vector con c(Clearance, Uptake, Burden, NA)
#' @export
contaminant_model_2 <- function(C, W, Temperature, X_Prey, X_ae, Burden) {

  # Captación desde alimento (μg/día)
  Uptake <- sum(C * X_Prey * X_ae)

  # Coeficiente de eliminación de MeHg (Trudel & Rasmussen 1997)
  # Kx = exp(0.066*T - 0.2*log(W) - 6.56)
  Kx <- exp(0.066 * Temperature - 0.2 * log(W) - 6.56)

  # Eliminación (μg/día)
  Clearance <- Kx * Burden

  # Acumulación neta
  Accumulation <- Uptake - Clearance

  # Nueva carga corporal
  New_Burden <- Burden + Accumulation

  return(c(Clearance, Uptake, New_Burden, NA))
}

# ============================================================================
# MODELO 3: ARNOT & GOBAS (2004)
# ============================================================================

#' Modelo de contaminantes 3 - Arnot & Gobas (2004)
#'
#' Modelo completo con captación desde agua y alimento, eliminación proporcional
#' a la respiración
#'
#' @param R_O2 Respiración (g O2/g/día)
#' @param C Vector de consumo por presa (g/día)
#' @param W Peso del pez (g)
#' @param Temperature Temperatura del agua (°C)
#' @param X_Prey Vector de concentraciones en presas (μg/g)
#' @param X_ae Vector de eficiencias de asimilación
#' @param Burden Carga corporal actual (μg)
#' @param Ew Eficiencia de captación branquial
#' @param Kbw Coeficiente de partición pez:agua
#' @param Cw_tot Concentración total en agua (mg/L)
#' @param phi_DT Fracción disuelta
#' @param DO_Sat_fr Saturación de oxígeno (fracción)
#' @return Vector con c(Clearance, Uptake, Burden, NA)
#' @export
contaminant_model_3 <- function(R_O2, C, W, Temperature, X_Prey, X_ae, Burden,
                                Ew, Kbw, Cw_tot, phi_DT, DO_Sat_fr) {

  # Validar parámetros requeridos
  required_params <- c("Ew", "Kbw", "Cw_tot", "phi_DT", "DO_Sat_fr")
  for (param in required_params) {
    if (is.null(get(param))) {
      stop(paste(param, "es requerido para CONTEQ = 3"))
    }
  }

  # Convertir respiración a mg O2/g/día
  VOx <- 1000 * R_O2

  # Concentración de oxígeno disuelto (mg O2/L)
  # Ecuación 9 de Arnot & Gobas (2004)
  COx <- (-0.24 * Temperature + 14.04) * DO_Sat_fr

  # Tasa de eliminación de agua (L/g/día)
  K1 <- Ew * VOx / COx

  # Captación desde agua (μg/día)
  Uptake_water <- W * K1 * phi_DT * Cw_tot * 1000  # (L/día)*(mg/L)*(1000 μg/mg)

  # Captación desde alimento (μg/día)
  Uptake_food <- sum(C * X_Prey * X_ae)

  # Captación total
  Uptake <- Uptake_water + Uptake_food

  # Coeficiente de eliminación
  Kx <- K1 / Kbw

  # Eliminación (μg/día)
  Clearance <- Kx * Burden

  # Acumulación neta
  Accumulation <- Uptake - Clearance

  # Nueva carga corporal
  New_Burden <- Burden + Accumulation

  return(c(Clearance, Uptake, New_Burden, NA))
}

# ============================================================================
# FUNCIONES DE PARÁMETROS PARA MODELO 3
# ============================================================================

#' Calcular eficiencia de captación branquial
#'
#' Calcula Ew basado en el coeficiente de partición octanol:agua (Kow)
#' según Arnot & Gobas (2004)
#'
#' @param Kow Coeficiente de partición octanol:agua
#' @return Eficiencia de captación branquial
#' @export
#' @examples
#' # Para pentaclorobenceno (log Kow = 5.17)
#' Kow <- 10^5.17
#' ew <- calculate_gill_efficiency(Kow)
calculate_gill_efficiency <- function(Kow) {

  if (Kow <= 0) {
    stop("Kow debe ser positivo")
  }

  # Ecuación 6 de Arnot & Gobas (2004)
  Ew <- 1 / (1.85 + (155 / Kow))

  # Validar rango
  if (Ew < 0 || Ew > 1) {
    warning("Eficiencia de captación branquial fuera de rango (0-1): ", Ew)
  }

  return(Ew)
}

#' Calcular coeficiente de partición pez:agua
#'
#' Calcula Kbw basado en composición corporal y Kow según Arnot & Gobas (2004)
#'
#' @param Fat_fr Fracción de grasa en el pez
#' @param ProAsh_fr Fracción de proteína + ceniza
#' @param H2O_fr Fracción de agua
#' @param Kow Coeficiente de partición octanol:agua
#' @return Coeficiente de partición pez:agua
#' @export
#' @examples
#' # Composición típica de pez
#' Kbw <- calculate_fish_water_partition(
#'   Fat_fr = 0.08, ProAsh_fr = 0.20, H2O_fr = 0.72,
#'   Kow = 10^5.17
#' )
calculate_fish_water_partition <- function(Fat_fr, ProAsh_fr, H2O_fr, Kow) {

  # Validar fracciones
  total_fraction <- Fat_fr + ProAsh_fr + H2O_fr
  if (abs(total_fraction - 1) > 0.01) {
    warning("Las fracciones no suman 1.0: ", round(total_fraction, 3))
  }

  # Ecuación 3 de Arnot & Gobas (2004)
  Kbw <- Fat_fr * Kow + ProAsh_fr * 0.035 * Kow + H2O_fr

  return(Kbw)
}

#' Calcular fracción disuelta del contaminante
#'
#' Calcula phi_DT basado en concentraciones de carbono orgánico según
#' Arnot & Gobas (2004)
#'
#' @param XPOC Concentración de carbono orgánico particulado (kg/L)
#' @param XDOC Concentración de carbono orgánico disuelto (kg/L)
#' @param Kow Coeficiente de partición octanol:agua
#' @param DPOC Factor de desequilibrio para POC (por defecto 1)
#' @param DDOC Factor de desequilibrio para DOC (por defecto 1)
#' @param aPOC Constante de proporcionalidad para POC (por defecto 0.35)
#' @param aDOC Constante de proporcionalidad para DOC (por defecto 0.08)
#' @return Fracción disuelta
#' @export
calculate_dissolved_fraction <- function(XPOC, XDOC, Kow,
                                         DPOC = 1, DDOC = 1,
                                         aPOC = 0.35, aDOC = 0.08) {

  # Ecuación 4 de Arnot & Gobas (2004)
  phi_DT <- 1 / (1 + XPOC * DPOC * aPOC * Kow + XDOC * DDOC * aDOC * Kow)

  # Validar rango
  if (phi_DT < 0 || phi_DT > 1) {
    warning("Fracción disuelta fuera de rango (0-1): ", phi_DT)
  }

  return(phi_DT)
}

# ============================================================================
# FUNCIONES DE ANÁLISIS Y VALIDACIÓN
# ============================================================================

#' Validar parámetros de contaminantes
#'
#' Verifica que los parámetros estén en rangos realistas
#'
#' @param params Lista con parámetros de contaminantes
#' @param CONTEQ Ecuación de contaminantes
#' @return Lista con validación
#' @export
validate_contaminant_params <- function(params, CONTEQ) {

  warnings <- character()
  errors <- character()
  valid <- TRUE

  # Validaciones comunes
  if ("X_Prey" %in% names(params)) {
    if (any(params$X_Prey < 0)) {
      errors <- c(errors, "Concentraciones en presas no pueden ser negativas")
      valid <- FALSE
    }
    if (any(params$X_Prey > 1000)) {
      warnings <- c(warnings, "Concentraciones en presas muy altas (>1000 μg/g)")
    }
  }

  if ("X_ae" %in% names(params)) {
    if (any(params$X_ae < 0 | params$X_ae > 1)) {
      errors <- c(errors, "Eficiencias de asimilación deben estar entre 0 y 1")
      valid <- FALSE
    }
  }

  if ("TEx" %in% names(params)) {
    if (any(params$TEx < 0 | params$TEx > 1)) {
      errors <- c(errors, "Eficiencias de transferencia deben estar entre 0 y 1")
      valid <- FALSE
    }
  }

  # Validaciones específicas por modelo
  if (CONTEQ == 3) {
    if ("Ew" %in% names(params)) {
      if (params$Ew < 0 || params$Ew > 1) {
        errors <- c(errors, "Eficiencia branquial debe estar entre 0 y 1")
        valid <- FALSE
      }
    }

    if ("Kbw" %in% names(params)) {
      if (params$Kbw <= 0) {
        errors <- c(errors, "Coeficiente de partición pez:agua debe ser positivo")
        valid <- FALSE
      }
      if (params$Kbw > 1e6) {
        warnings <- c(warnings, "Coeficiente de partición muy alto")
      }
    }

    if ("Cw_tot" %in% names(params)) {
      if (params$Cw_tot < 0) {
        errors <- c(errors, "Concentración en agua no puede ser negativa")
        valid <- FALSE
      }
    }

    if ("phi_DT" %in% names(params)) {
      if (params$phi_DT < 0 || params$phi_DT > 1) {
        errors <- c(errors, "Fracción disuelta debe estar entre 0 y 1")
        valid <- FALSE
      }
    }
  }

  return(list(
    valid = valid,
    warnings = warnings,
    errors = errors,
    n_warnings = length(warnings),
    n_errors = length(errors)
  ))
}

#' Simular acumulación de contaminantes en el tiempo
#'
#' Simula la dinámica de contaminantes durante una serie temporal
#'
#' @param days Vector de días
#' @param consumption_data Matriz de consumo por día y presa
#' @param contaminant_data Lista con datos de contaminantes por día
#' @param initial_concentration Concentración inicial en depredador
#' @param fish_weight Vector de pesos del pez por día
#' @param temperature Vector de temperaturas por día
#' @param CONTEQ Ecuación de contaminantes
#' @param ... Parámetros adicionales según el modelo
#' @return Data frame con dinámica temporal
#' @export
simulate_contaminant_dynamics <- function(days, consumption_data, contaminant_data,
                                          initial_concentration, fish_weight,
                                          temperature, CONTEQ, ...) {

  n_days <- length(days)
  n_prey <- ncol(consumption_data)

  # Inicializar resultados
  results <- data.frame(
    Day = days,
    Weight = fish_weight,
    Temperature = temperature,
    Concentration = numeric(n_days),
    Burden = numeric(n_days),
    Daily_Uptake = numeric(n_days),
    Daily_Clearance = numeric(n_days),
    Cumulative_Uptake = numeric(n_days),
    Cumulative_Clearance = numeric(n_days)
  )

  # Concentración y carga inicial
  current_concentration <- initial_concentration
  cumulative_uptake <- 0
  cumulative_clearance <- 0

  # Parámetros adicionales
  extra_params <- list(...)

  # Simulación diaria
  for (i in 1:n_days) {
    # Datos del día
    daily_consumption <- consumption_data[i, ]
    daily_weight <- fish_weight[i]
    daily_temp <- temperature[i]

    # Concentraciones en presas para este día
    X_Prey <- contaminant_data$prey_concentrations[i, ]

    # Eficiencias para este día
    if ("assimilation_efficiency" %in% names(contaminant_data)) {
      X_ae <- contaminant_data$assimilation_efficiency[i, ]
    } else {
      X_ae <- rep(0.8, n_prey)  # Valor por defecto
    }

    if ("transfer_efficiency" %in% names(contaminant_data)) {
      TEx <- contaminant_data$transfer_efficiency[i, ]
    } else {
      TEx <- rep(0.95, n_prey)  # Valor por defecto
    }

    # Preparar parámetros para el modelo
    model_params <- list(
      R_O2 = extra_params$R_O2[i] %||% 0.01,
      C = daily_consumption,
      W = daily_weight,
      Temperature = daily_temp,
      X_Prey = X_Prey,
      X_Pred = current_concentration,
      TEx = TEx,
      X_ae = X_ae,
      CONTEQ = CONTEQ
    )

    # Agregar parámetros específicos del modelo 3
    if (CONTEQ == 3) {
      model_params$Ew <- extra_params$Ew %||% 0.5
      model_params$Kbw <- extra_params$Kbw %||% 1000
      model_params$Cw_tot <- extra_params$Cw_tot %||% 0.001
      model_params$phi_DT <- extra_params$phi_DT %||% 0.8
      model_params$DO_Sat_fr <- extra_params$DO_Sat_fr %||% 0.9
    }

    # Ejecutar modelo
    daily_result <- do.call(pred_cont_conc, model_params)

    # Extraer resultados
    daily_clearance <- daily_result[1]
    daily_uptake <- daily_result[2]
    new_burden <- daily_result[3]
    new_concentration <- daily_result[4]

    # Actualizar acumulados
    cumulative_uptake <- cumulative_uptake + daily_uptake
    cumulative_clearance <- cumulative_clearance + daily_clearance

    # Guardar resultados
    results$Concentration[i] <- new_concentration
    results$Burden[i] <- new_burden
    results$Daily_Uptake[i] <- daily_uptake
    results$Daily_Clearance[i] <- daily_clearance
    results$Cumulative_Uptake[i] <- cumulative_uptake
    results$Cumulative_Clearance[i] <- cumulative_clearance

    # Actualizar concentración para siguiente día
    current_concentration <- new_concentration
  }

  return(results)
}

# ============================================================================
# FUNCIONES DE VISUALIZACIÓN
# ============================================================================

#' Graficar dinámica de contaminantes
#'
#' Crea gráficos de la acumulación de contaminantes en el tiempo
#'
#' @param simulation_results Output de simulate_contaminant_dynamics
#' @param plot_type Tipo de gráfico ("concentration", "uptake", "balance")
#' @return Gráfico de dinámica de contaminantes
#' @export
plot_contaminant_dynamics <- function(simulation_results, plot_type = "concentration") {

  if (plot_type == "concentration") {
    # Gráfico de concentración y carga
    par(mfrow = c(2, 1))

    plot(simulation_results$Day, simulation_results$Concentration,
         type = "l", lwd = 2, col = "red",
         xlab = "Día", ylab = "Concentración (μg/g)",
         main = "Concentración de Contaminantes")
    grid()

    plot(simulation_results$Day, simulation_results$Burden,
         type = "l", lwd = 2, col = "blue",
         xlab = "Día", ylab = "Carga Corporal (μg)",
         main = "Carga Corporal de Contaminantes")
    grid()

    par(mfrow = c(1, 1))

  } else if (plot_type == "uptake") {
    # Gráfico de captación y eliminación
    par(mfrow = c(2, 1))

    plot(simulation_results$Day, simulation_results$Daily_Uptake,
         type = "l", lwd = 2, col = "green",
         xlab = "Día", ylab = "Captación Diaria (μg/d)",
         main = "Captación Diaria de Contaminantes")
    grid()

    plot(simulation_results$Day, simulation_results$Daily_Clearance,
         type = "l", lwd = 2, col = "orange",
         xlab = "Día", ylab = "Eliminación Diaria (μg/d)",
         main = "Eliminación Diaria de Contaminantes")
    grid()

    par(mfrow = c(1, 1))

  } else if (plot_type == "balance") {
    # Gráfico de balance acumulativo
    plot(simulation_results$Day, simulation_results$Cumulative_Uptake,
         type = "l", lwd = 2, col = "green",
         xlab = "Día", ylab = "Contaminantes (μg)",
         main = "Balance Acumulativo de Contaminantes",
         ylim = range(c(simulation_results$Cumulative_Uptake,
                        simulation_results$Cumulative_Clearance)))

    lines(simulation_results$Day, simulation_results$Cumulative_Clearance,
          lwd = 2, col = "red")

    legend("topleft", legend = c("Captación", "Eliminación"),
           col = c("green", "red"), lwd = 2)
    grid()
  }
}

#' Comparar modelos de contaminantes
#'
#' Compara resultados de diferentes modelos de contaminantes
#'
#' @param scenarios Lista con escenarios para comparar
#' @param metric Métrica a comparar ("concentration", "burden")
#' @return Gráfico comparativo
#' @export
compare_contaminant_models <- function(scenarios, metric = "concentration") {

  colors <- rainbow(length(scenarios))
  labels <- names(scenarios)

  if (is.null(labels)) {
    labels <- paste("Modelo", 1:length(scenarios))
  }

  # Configurar gráfico
  y_range <- range(sapply(scenarios, function(s) range(s[[metric]])))

  plot(NULL, xlim = range(scenarios[[1]]$Day), ylim = y_range,
       xlab = "Día",
       ylab = if(metric == "concentration") "Concentración (μg/g)" else "Carga (μg)",
       main = paste("Comparación de Modelos -",
                    if(metric == "concentration") "Concentración" else "Carga"))

  # Graficar cada escenario
  for (i in seq_along(scenarios)) {
    lines(scenarios[[i]]$Day, scenarios[[i]][[metric]],
          lwd = 2, col = colors[i])
  }

  # Leyenda
  legend("topright", legend = labels, col = colors, lwd = 2, cex = 0.8)
  grid()
}

# Operador %||% para valores por defecto
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
