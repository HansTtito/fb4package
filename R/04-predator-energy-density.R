#' Funciones de Densidad Energética del Depredador para el Modelo FB4
#'
#' @name predator-energy-density
#' @aliases predator-energy-density
NULL

# ============================================================================
# FUNCIONES CORE DE DENSIDAD ENERGÉTICA
# ============================================================================

#' Densidad energética desde datos interpolados (Ecuación 1)
#'
#' @param weight Peso del pez (g) - no usado en esta ecuación
#' @param day Día de la simulación
#' @param energy_data Vector con densidades energéticas por día
#' @return Densidad energética (J/g)
#' @keywords internal
predator_energy_eq1 <- function(weight, day, energy_data) {
  if (is.null(energy_data) || length(energy_data) == 0) {
    return(4500)  # Valor por defecto típico
  }
  
  # Asegurar que el día esté en rango válido
  day_index <- clamp(round(day), 1, length(energy_data))
  
  return(energy_data[day_index])
}

#' Densidad energética lineal por segmentos (Ecuación 2)
#'
#' @param weight Peso del pez (g)
#' @param Alpha1 Intercepto para primer segmento
#' @param Beta1 Pendiente para primer segmento
#' @param Alpha2 Intercepto para segundo segmento
#' @param Beta2 Pendiente para segundo segmento
#' @param Cutoff Peso de corte entre segmentos
#' @return Densidad energética (J/g)
#' @keywords internal
predator_energy_eq2 <- function(weight, Alpha1, Beta1, Alpha2, Beta2, Cutoff) {
  if (any(is.na(c(weight, Alpha1, Beta1, Cutoff)))) {
    return(4500)  # Valor por defecto
  }
  
  cutoff_value <- as.numeric(Cutoff)
  
  if (weight < cutoff_value) {
    # Primer segmento
    energy_density <- Alpha1 + Beta1 * weight
  } else {
    # Segundo segmento
    if (any(is.na(c(Alpha2, Beta2)))) {
      # Si no hay parámetros para segundo segmento, usar primero
      energy_density <- Alpha1 + Beta1 * weight
    } else {
      energy_density <- Alpha2 + Beta2 * weight
    }
  }
  
  # Asegurar valores razonables
  return(clamp(energy_density, 1000, 15000))
}

#' Densidad energética función potencia (Ecuación 3)
#'
#' @param weight Peso del pez (g)
#' @param Alpha1 Coeficiente multiplicativo
#' @param Beta1 Exponente
#' @return Densidad energética (J/g)
#' @keywords internal
predator_energy_eq3 <- function(weight, Alpha1, Beta1) {
  if (any(is.na(c(weight, Alpha1, Beta1)))) {
    return(4500)  # Valor por defecto
  }
  
  if (Alpha1 <= 0) {
    warning("Alpha1 debe ser positivo, usando valor por defecto")
    return(4500)
  }
  
  # Limitar exponente para evitar valores extremos
  safe_beta <- clamp(Beta1, -2, 2)
  
  # Calcular densidad energética
  energy_density <- Alpha1 * (weight^safe_beta)
  
  # Asegurar valores razonables
  return(clamp(energy_density, 1000, 15000))
}

# ============================================================================
# FUNCIÓN PRINCIPAL
# ============================================================================

#' Calcular densidad energética del depredador
#'
#' @param weight Peso del pez (g)
#' @param day Día de la simulación (para ecuación 1)
#' @param predator_params Lista con parámetros de densidad energética
#' @param energy_data Vector con datos de densidad energética por día (para ecuación 1)
#' @return Densidad energética (J/g)
#' @export
calculate_predator_energy_density <- function(weight, day = 1, predator_params, energy_data = NULL) {
  
  # Validaciones básicas
  if (is.null(predator_params)) {
    stop("predator_params no puede ser NULL")
  }
  
  # Validar valores de entrada
  weight <- check_numeric_value(weight, "weight", min_val = 0.001)
  day <- check_numeric_value(day, "day", min_val = 1)
  
  # Si existen ED_start y ED_end, usar interpolación diaria
  if (!is.null(predator_params$ED_start) && !is.null(predator_params$ED_end)) {
    duration <- predator_params$duration %||% 365
    proportion <- (day - 1) / max(1, duration - 1)
    energy_density <- predator_params$ED_start + 
      (predator_params$ED_end - predator_params$ED_start) * proportion
    return(clamp(energy_density, 1000, 15000))
  }
  
  # Determinar ecuación a usar
  PREDEDEQ <- predator_params$PREDEDEQ %||% 3
  
  # Calcular densidad energética según ecuación
  if (PREDEDEQ == 1) {
    energy_density <- predator_energy_eq1(weight, day, energy_data)
    
  } else if (PREDEDEQ == 2) {
    Alpha1 <- predator_params$Alpha1
    Beta1  <- predator_params$Beta1
    Alpha2 <- predator_params$Alpha2
    Beta2  <- predator_params$Beta2
    Cutoff <- predator_params$Cutoff
    
    if (any(is.na(c(Alpha1, Beta1, Cutoff)))) {
      warning("Parámetros insuficientes para PREDEDEQ=2, usando ecuación 3")
      Alpha1 <- Alpha1 %||% 4500
      Beta1  <- Beta1 %||% 0
      energy_density <- predator_energy_eq3(weight, Alpha1, Beta1)
    } else {
      energy_density <- predator_energy_eq2(weight, Alpha1, Beta1, Alpha2, Beta2, Cutoff)
    }
    
  } else if (PREDEDEQ == 3) {
    Alpha1 <- predator_params$Alpha1
    Beta1  <- predator_params$Beta1
    
    if (any(is.na(c(Alpha1, Beta1)))) {
      warning("Parámetros insuficientes para PREDEDEQ=3, usando valor por defecto")
      energy_density <- 4500
    } else {
      energy_density <- predator_energy_eq3(weight, Alpha1, Beta1)
    }
    
  } else {
    warning("Ecuación de densidad energética no válida: ", PREDEDEQ, ". Usando ecuación 3.")
    Alpha1 <- predator_params$Alpha1 %||% 4500
    Beta1  <- predator_params$Beta1 %||% 0
    energy_density <- predator_energy_eq3(weight, Alpha1, Beta1)
  }
  
  # Validación final
  if (is.na(energy_density) || !is.finite(energy_density) || energy_density <= 0) {
    warning("Densidad energética inválida calculada, usando valor por defecto")
    energy_density <- 4500
  }
  
  return(energy_density)
}


# ============================================================================
# FUNCIONES AUXILIARES PARA CÁLCULO DE PESO
# ============================================================================

#' Calcular peso final dados energía ganada y densidad energética
#'
#' @param initial_weight Peso inicial (g)
#' @param energy_gain Ganancia neta de energía (J)
#' @param predator_params Lista con parámetros de densidad energética
#' @param spawn_energy Energía perdida por reproducción (J)
#' @return Lista con peso final y densidad energética final
#' @export
calculate_final_weight <- function(initial_weight, energy_gain, predator_params, spawn_energy = 0) {
  
  # Validar entradas
  initial_weight <- check_numeric_value(initial_weight, "initial_weight", min_val = 0.001)
  energy_gain <- check_numeric_value(energy_gain, "energy_gain")
  spawn_energy <- check_numeric_value(spawn_energy, "spawn_energy", min_val = 0)
  
  # Energía neta disponible
  net_energy <- energy_gain - spawn_energy
  
  # Densidad energética inicial
  initial_ed <- calculate_predator_energy_density(initial_weight, day = 1, predator_params)
  initial_body_energy <- initial_weight * initial_ed
  
  PREDEDEQ <- predator_params$PREDEDEQ %||% 3
  
  if (PREDEDEQ == 1) {
    # Usar densidad energética fija (asumimos que no cambia mucho)
    final_ed <- initial_ed
    final_weight <- (initial_body_energy + net_energy) / final_ed
    
  } else if (PREDEDEQ == 3) {
    # Función potencia: resolver analíticamente
    Alpha1 <- predator_params$Alpha1
    Beta1 <- predator_params$Beta1
    
    if (any(is.na(c(Alpha1, Beta1)))) {
      # Fallback a densidad constante
      final_weight <- (initial_body_energy + net_energy) / initial_ed
      final_ed <- initial_ed
    } else {
      final_weight <- solve_weight_power_function(initial_weight, net_energy, Alpha1, Beta1)
      final_ed <- calculate_predator_energy_density(final_weight, day = 1, predator_params)
    }
    
  } else if (PREDEDEQ == 2) {
    # Función lineal por segmentos: usar aproximación iterativa
    final_weight <- solve_weight_power_function(initial_weight, net_energy, Alpha1, Beta1)
    final_ed <- calculate_predator_energy_density(final_weight, day = 1, predator_params)
    
  } else {
    # Fallback a densidad constante
    final_weight <- (initial_body_energy + net_energy) / initial_ed
    final_ed <- initial_ed
  }
  
  # Validar resultado
  if (final_weight <= 0) {
    warning("Peso final negativo o cero, estableciendo peso mínimo")
    final_weight <- 0.001
    final_ed <- calculate_predator_energy_density(final_weight, day = 1, predator_params)
  }
  
  return(list(
    final_weight = final_weight,
    final_energy_density = final_ed
  ))
}

#' Resolver peso final para función potencia
#'
#' @param initial_weight Peso inicial
#' @param net_energy Energía neta
#' @param Alpha1 Coeficiente
#' @param Beta1 Exponente
#' @return Peso final
#' @keywords internal
solve_weight_power_function <- function(initial_weight, net_energy, Alpha1, Beta1) {
  
  initial_body_energy <- initial_weight * Alpha1 * (initial_weight^Beta1)
  
  if (abs(Beta1) < 1e-6) {
    # Beta1 ≈ 0: densidad energética constante
    final_weight <- (initial_body_energy + net_energy) / Alpha1
  } else {
    # Resolver: Alpha1 * W^(Beta1+1) = initial_body_energy + net_energy
    target_energy <- initial_body_energy + net_energy
    
    if (target_energy <= 0) {
      return(0.001)  # Peso mínimo
    }
    
    exponent <- Beta1 + 1
    if (exponent <= 0) {
      # Caso problemático, usar aproximación
      final_weight <- initial_weight * (1 + net_energy / initial_body_energy)
    } else {
      final_weight <- (target_energy / Alpha1)^(1/exponent)
    }
  }
  
  return(pmax(0.001, final_weight))
}


# ============================================================================
# FUNCIONES DE UTILIDAD Y VALIDACIÓN
# ============================================================================

#' Validar parámetros de densidad energética
#'
#' @param predator_params Lista con parámetros
#' @param weight_range Rango de pesos para probar
#' @return Lista con resultados de validación
#' @export
validate_predator_energy_params <- function(predator_params, weight_range = c(1, 1000)) {
  
  validation <- list(
    valid = TRUE,
    errors = character(),
    warnings = character()
  )
  
  PREDEDEQ <- predator_params$PREDEDEQ %||% 3
  
  if (!PREDEDEQ %in% 1:3) {
    validation$errors <- c(validation$errors, "PREDEDEQ debe ser 1, 2, o 3")
    validation$valid <- FALSE
    return(validation)
  }
  
  # Probar cálculos en rango de pesos
  test_weights <- seq(weight_range[1], weight_range[2], length.out = 10)
  
  for (weight in test_weights) {
    tryCatch({
      ed <- calculate_predator_energy_density(weight, 1, predator_params)
      
      if (ed < 1000) {
        validation$warnings <- c(validation$warnings, 
                                 paste("Densidad energética baja para peso", weight, "g:", round(ed)))
      }
      
      if (ed > 15000) {
        validation$warnings <- c(validation$warnings,
                                 paste("Densidad energética alta para peso", weight, "g:", round(ed)))
      }
      
    }, error = function(e) {
      validation$errors <- c(validation$errors, 
                             paste("Error calculando densidad para peso", weight, "g:", e$message))
      validation$valid <- FALSE
    })
  }
  
  # Validaciones específicas por ecuación
  if (PREDEDEQ == 2) {
    required_params <- c("Alpha1", "Beta1", "Cutoff")
    missing_params <- setdiff(required_params, names(predator_params))
    if (length(missing_params) > 0) {
      validation$warnings <- c(validation$warnings,
                               paste("Parámetros faltantes para PREDEDEQ=2:", 
                                     paste(missing_params, collapse = ", ")))
    }
  } else if (PREDEDEQ == 3) {
    required_params <- c("Alpha1", "Beta1")
    missing_params <- setdiff(required_params, names(predator_params))
    if (length(missing_params) > 0) {
      validation$warnings <- c(validation$warnings,
                               paste("Parámetros faltantes para PREDEDEQ=3:", 
                                     paste(missing_params, collapse = ", ")))
    }
  }
  
  return(validation)
}

#' Generar datos de ejemplo para densidad energética
#'
#' @param duration Duración en días
#' @param base_energy Energía base (J/g)
#' @param variation Variación estacional (fracción)
#' @return Vector con densidades energéticas por día
#' @export
generate_example_energy_data <- function(duration = 365, base_energy = 4500, variation = 0.1) {
  
  days <- 1:duration
  
  # Variación estacional simple
  seasonal_effect <- sin(2 * pi * days / 365) * variation
  
  energy_data <- base_energy * (1 + seasonal_effect)
  
  return(energy_data)
}