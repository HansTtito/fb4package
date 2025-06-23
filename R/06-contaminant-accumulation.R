#' Funciones de Acumulación de Contaminantes para el Modelo FB4
#'
#' @name contaminant-accumulation
#' @aliases contaminant-accumulation
NULL

# ============================================================================
# FUNCIONES CORE DE MODELOS DE CONTAMINANTES
# ============================================================================

#' Modelo de contaminantes 1 - Solo captación de alimento
#'
#' Modelo simple sin eliminación, solo acumulación desde el alimento
#'
#' @param consumption Vector de consumo por presa (g/día)
#' @param prey_concentrations Vector de concentraciones en presas (μg/g)
#' @param transfer_efficiency Vector de eficiencias de transferencia
#' @param current_burden Carga corporal actual (μg)
#' @return Lista con clearance, uptake, y nueva carga
#' @keywords internal
contaminant_model_1 <- function(consumption, prey_concentrations, transfer_efficiency, current_burden) {
  
  # Validar entradas
  if (length(consumption) != length(prey_concentrations) || 
      length(consumption) != length(transfer_efficiency)) {
    stop("Vectores de entrada deben tener la misma longitud")
  }
  
  # Captación desde alimento (μg/día)
  uptake <- sum(consumption * prey_concentrations * transfer_efficiency, na.rm = TRUE)
  
  # Sin eliminación en este modelo
  clearance <- 0
  
  # Nueva carga corporal
  new_burden <- current_burden + uptake
  
  return(list(
    clearance = clearance,
    uptake = uptake,
    new_burden = pmax(0, new_burden)
  ))
  
}

#' Modelo de contaminantes 2 - Con eliminación dependiente de T y peso
#'
#' Modelo con captación de alimento y eliminación dependiente de temperatura y peso
#' Basado en Trudel & Rasmussen (1997) para MeHg
#'
#' @param consumption Vector de consumo por presa (g/día)
#' @param weight Peso del pez (g)
#' @param temperature Temperatura del agua (°C)
#' @param prey_concentrations Vector de concentraciones en presas (μg/g)
#' @param assimilation_efficiency Vector de eficiencias de asimilación
#' @param current_burden Carga corporal actual (μg)
#' @return Lista con clearance, uptake, y nueva carga
#' @keywords internal
contaminant_model_2 <- function(consumption, weight, temperature, prey_concentrations, 
                                assimilation_efficiency, current_burden) {
  
  # Validar entradas
  if (length(consumption) != length(prey_concentrations) || 
      length(consumption) != length(assimilation_efficiency)) {
    stop("Vectores de entrada deben tener la misma longitud")
  }
  
  # Captación desde alimento (μg/día)
  uptake <- sum(consumption * prey_concentrations * assimilation_efficiency, na.rm = TRUE)
  
  # Coeficiente de eliminación de MeHg (Trudel & Rasmussen 1997)
  # Kx = exp(0.066*T - 0.2*log(W) - 6.56)
  safe_temp <- clamp(temperature, 0, 40)
  safe_weight <- pmax(0.1, weight)
  
  Kx <- safe_exp(0.066 * safe_temp - 0.2 * log(safe_weight) - 6.56)
  
  # Eliminación (μg/día)
  clearance <- Kx * current_burden
  
  # Nueva carga corporal
  new_burden <- current_burden + uptake - clearance
  
  return(list(
    clearance = clearance,
    uptake = uptake,
    new_burden = pmax(0, new_burden)
  ))
  
}

#' Modelo de contaminantes 3 - Arnot & Gobas (2004)
#'
#' Modelo completo con captación desde agua y alimento, eliminación proporcional a respiración
#'
#' @param respiration_o2 Respiración (g O2/g/día)
#' @param consumption Vector de consumo por presa (g/día)
#' @param weight Peso del pez (g)
#' @param temperature Temperatura del agua (°C)
#' @param prey_concentrations Vector de concentraciones en presas (μg/g)
#' @param assimilation_efficiency Vector de eficiencias de asimilación
#' @param current_burden Carga corporal actual (μg)
#' @param gill_efficiency Eficiencia de captación branquial
#' @param fish_water_partition Coeficiente de partición pez:agua
#' @param water_concentration Concentración total en agua (mg/L)
#' @param dissolved_fraction Fracción disuelta
#' @param do_saturation Saturación de oxígeno disuelto (fracción)
#' @return Lista con clearance, uptake, y nueva carga
#' @keywords internal
contaminant_model_3 <- function(respiration_o2, consumption, weight, temperature,
                                prey_concentrations, assimilation_efficiency, current_burden,
                                gill_efficiency, fish_water_partition, water_concentration,
                                dissolved_fraction, do_saturation) {
  
  # Validar parámetros requeridos
  required_params <- c(gill_efficiency, fish_water_partition, water_concentration, 
                       dissolved_fraction, do_saturation)
  
  if (any(is.na(required_params))) {
    warning("Parámetros faltantes para modelo 3, usando modelo 2")
    return(contaminant_model_2(consumption, weight, temperature, prey_concentrations,
                               assimilation_efficiency, current_burden))
  }
  
  # Convertir respiración a mg O2/g/día
  VOx <- 1000 * respiration_o2
  
  # Concentración de oxígeno disuelto (mg O2/L)
  # Ecuación de Arnot & Gobas (2004)
  safe_temp <- clamp(temperature, 0, 40)
  COx <- (-0.24 * safe_temp + 14.04) * do_saturation
  COx <- pmax(1, COx)  # Evitar división por cero
  
  # Tasa de eliminación de agua (L/g/día)
  K1 <- gill_efficiency * VOx / COx
  
  # Captación desde agua (μg/día)
  uptake_water <- weight * K1 * dissolved_fraction * water_concentration * 1000
  
  # Captación desde alimento (μg/día)
  uptake_food <- sum(consumption * prey_concentrations * assimilation_efficiency, na.rm = TRUE)
  
  # Captación total
  uptake <- uptake_water + uptake_food
  
  # Coeficiente de eliminación
  Kx <- K1 / fish_water_partition
  
  # Eliminación (μg/día)
  clearance <- Kx * current_burden
  
  # Nueva carga corporal
  new_burden <- current_burden + uptake - clearance
  
  return(list(
    clearance = clearance,
    uptake = uptake,
    uptake_water = uptake_water,
    uptake_food = uptake_food,
    new_burden = pmax(0, new_burden)
  ))
  
}

# ============================================================================
# FUNCIÓN PRINCIPAL DE CONTAMINANTES
# ============================================================================

#' Calcular acumulación de contaminantes
#'
#' Función principal para calcular la dinámica de contaminantes
#'
#' @param respiration_o2 Respiración en g O2/g/día
#' @param consumption Vector de consumo por tipo de presa (g/día)
#' @param weight Peso del pez (g)
#' @param temperature Temperatura del agua (°C)
#' @param current_concentration Concentración actual en depredador (μg/g)
#' @param contaminant_params Lista con parámetros de contaminantes
#' @return Lista con resultados de contaminantes
#' @export
calculate_contaminant_accumulation <- function(respiration_o2, consumption, weight, temperature,
                                               current_concentration, contaminant_params) {
  
  # Validaciones básicas
  if (is.null(contaminant_params)) {
    stop("contaminant_params no puede ser NULL")
  }
  
  # Validar valores de entrada
  weight <- check_numeric_value(weight, "weight", min_val = 0.001)
  temperature <- check_numeric_value(temperature, "temperature", min_val = -5, max_val = 50)
  current_concentration <- check_numeric_value(current_concentration, "current_concentration", min_val = 0)
  respiration_o2 <- check_numeric_value(respiration_o2, "respiration_o2", min_val = 0)
  
  # Calcular carga corporal inicial (μg)
  current_burden <- current_concentration * weight
  
  # Determinar modelo a usar
  CONTEQ <- contaminant_params$CONTEQ %||% 1
  
  # Extraer parámetros comunes
  prey_concentrations <- contaminant_params$prey_concentrations %||% rep(1.0, length(consumption))
  
  # Ajustar longitud de vectores si es necesario
  if (length(prey_concentrations) != length(consumption)) {
    if (length(prey_concentrations) == 1) {
      prey_concentrations <- rep(prey_concentrations, length(consumption))
    } else {
      stop("Longitud de prey_concentrations no coincide con consumption")
    }
  }
  
  # Calcular según modelo
  if (CONTEQ == 1) {
    # Modelo 1: Solo captación de alimento
    transfer_efficiency <- contaminant_params$transfer_efficiency %||% rep(0.95, length(consumption))
    
    result <- contaminant_model_1(consumption, prey_concentrations, transfer_efficiency, current_burden)
    
  } else if (CONTEQ == 2) {
    # Modelo 2: Captación + eliminación
    assimilation_efficiency <- contaminant_params$assimilation_efficiency %||% rep(0.80, length(consumption))
    
    result <- contaminant_model_2(consumption, weight, temperature, prey_concentrations,
                                  assimilation_efficiency, current_burden)
    
  } else if (CONTEQ == 3) {
    # Modelo 3: Arnot & Gobas
    assimilation_efficiency <- contaminant_params$assimilation_efficiency %||% rep(0.80, length(consumption))
    gill_efficiency <- contaminant_params$gill_efficiency %||% 0.5
    fish_water_partition <- contaminant_params$fish_water_partition %||% 1000
    water_concentration <- contaminant_params$water_concentration %||% 0.001
    dissolved_fraction <- contaminant_params$dissolved_fraction %||% 0.8
    do_saturation <- contaminant_params$do_saturation %||% 0.9
    
    result <- contaminant_model_3(respiration_o2, consumption, weight, temperature,
                                  prey_concentrations, assimilation_efficiency, current_burden,
                                  gill_efficiency, fish_water_partition, water_concentration,
                                  dissolved_fraction, do_saturation)
  } else {
    warning("Ecuación de contaminantes no válida: ", CONTEQ, ". Usando modelo 1.")
    transfer_efficiency <- contaminant_params$transfer_efficiency %||% rep(0.95, length(consumption))
    result <- contaminant_model_1(consumption, prey_concentrations, transfer_efficiency, current_burden)
  }
  
  # Calcular nueva concentración
  new_concentration <- if (weight > 0) result$new_burden / weight else 0
  
  # Añadir información adicional al resultado
  result$new_concentration <- new_concentration
  result$weight <- weight
  result$model_used <- CONTEQ
  
  return(result)
}

# ============================================================================
# FUNCIONES DE CÁLCULO DE PARÁMETROS PARA MODELO 3
# ============================================================================

#' Calcular eficiencia de captación branquial
#'
#' Calcula eficiencia branquial basada en Kow según Arnot & Gobas (2004)
#'
#' @param kow Coeficiente de partición octanol:agua
#' @return Eficiencia de captación branquial
#' @export
calculate_gill_efficiency <- function(kow) {
  
  kow <- check_numeric_value(kow, "kow", min_val = 0.001)
  
  # Ecuación de Arnot & Gobas (2004)
  efficiency <- 1 / (1.85 + (155 / kow))
  
  # Limitar a rango válido
  efficiency <- clamp(efficiency, 0, 1)
  
  return(efficiency)
}

#' Calcular coeficiente de partición pez:agua
#'
#' Calcula Kbw basado en composición corporal y Kow según Arnot & Gobas (2004)
#'
#' @param fat_fraction Fracción de grasa en el pez
#' @param protein_ash_fraction Fracción de proteína + ceniza
#' @param water_fraction Fracción de agua
#' @param kow Coeficiente de partición octanol:agua
#' @return Coeficiente de partición pez:agua
#' @export
calculate_fish_water_partition <- function(fat_fraction, protein_ash_fraction, water_fraction, kow) {
  
  # Validar fracciones
  fat_fraction <- check_numeric_value(fat_fraction, "fat_fraction", min_val = 0, max_val = 1)
  protein_ash_fraction <- check_numeric_value(protein_ash_fraction, "protein_ash_fraction", min_val = 0, max_val = 1)
  water_fraction <- check_numeric_value(water_fraction, "water_fraction", min_val = 0, max_val = 1)
  kow <- check_numeric_value(kow, "kow", min_val = 0.001)
  
  total_fraction <- fat_fraction + protein_ash_fraction + water_fraction
  if (abs(total_fraction - 1) > 0.1) {
    warning("Las fracciones se desvían significativamente de 1.0: ", round(total_fraction, 3))
  }
  
  # Ecuación de Arnot & Gobas (2004)
  partition_coeff <- fat_fraction * kow + protein_ash_fraction * 0.035 * kow + water_fraction
  
  return(pmax(1, partition_coeff))  # Mínimo de 1
}

#' Calcular fracción disuelta del contaminante
#'
#' Calcula fracción disuelta basada en carbono orgánico según Arnot & Gobas (2004)
#'
#' @param poc_concentration Concentración de carbono orgánico particulado (kg/L)
#' @param doc_concentration Concentración de carbono orgánico disuelto (kg/L)
#' @param kow Coeficiente de partición octanol:agua
#' @return Fracción disuelta
#' @export
calculate_dissolved_fraction <- function(poc_concentration, doc_concentration, kow) {
  
  # Validar entradas
  poc_concentration <- check_numeric_value(poc_concentration, "poc_concentration", min_val = 0)
  doc_concentration <- check_numeric_value(doc_concentration, "doc_concentration", min_val = 0)
  kow <- check_numeric_value(kow, "kow", min_val = 0.001)
  
  # Constantes de Arnot & Gobas (2004)
  a_poc <- 0.35
  a_doc <- 0.08
  
  # Ecuación de Arnot & Gobas (2004)
  denominator <- 1 + poc_concentration * a_poc * kow + doc_concentration * a_doc * kow
  dissolved_fraction <- 1 / denominator
  
  # Limitar a rango válido
  dissolved_fraction <- clamp(dissolved_fraction, 0, 1)
  
  return(dissolved_fraction)
}


# ============================================================================
# FUNCIONES DE UTILIDAD
# ============================================================================

#' Validar parámetros de contaminantes
#'
#' @param contaminant_params Lista con parámetros
#' @return Lista con resultados de validación
#' @export
validate_contaminant_params <- function(contaminant_params) {
  
  validation <- list(
    valid = TRUE,
    warnings = character(),
    errors = character()
  )
  
  CONTEQ <- contaminant_params$CONTEQ %||% 1
  
  if (!CONTEQ %in% 1:3) {
    validation$errors <- c(validation$errors, "CONTEQ debe ser 1, 2, o 3")
    validation$valid <- FALSE
    return(validation)
  }
  
  # Validar concentraciones en presas
  if ("prey_concentrations" %in% names(contaminant_params)) {
    if (any(contaminant_params$prey_concentrations < 0, na.rm = TRUE)) {
      validation$errors <- c(validation$errors, "Concentraciones en presas no pueden ser negativas")
      validation$valid <- FALSE
    }
    
    if (any(contaminant_params$prey_concentrations > 1000, na.rm = TRUE)) {
      validation$warnings <- c(validation$warnings, "Concentraciones en presas muy altas (>1000 μg/g)")
    }
  }
  
  # Validar eficiencias
  efficiency_params <- c("transfer_efficiency", "assimilation_efficiency")
  for (param in efficiency_params) {
    if (param %in% names(contaminant_params)) {
      values <- contaminant_params[[param]]
      if (any(values < 0 | values > 1, na.rm = TRUE)) {
        validation$errors <- c(validation$errors, paste(param, "debe estar entre 0 y 1"))
        validation$valid <- FALSE
      }
    }
  }
  
  # Validaciones específicas para modelo 3
  if (CONTEQ == 3) {
    model3_params <- c("gill_efficiency", "dissolved_fraction")
    for (param in model3_params) {
      if (param %in% names(contaminant_params)) {
        value <- contaminant_params[[param]]
        if (value < 0 || value > 1) {
          validation$errors <- c(validation$errors, paste(param, "debe estar entre 0 y 1"))
          validation$valid <- FALSE
        }
      }
    }
    
    if ("fish_water_partition" %in% names(contaminant_params)) {
      if (contaminant_params$fish_water_partition <= 0) {
        validation$errors <- c(validation$errors, "fish_water_partition debe ser positivo")
        validation$valid <- FALSE
      }
    }
  }
  
  return(validation)
}

