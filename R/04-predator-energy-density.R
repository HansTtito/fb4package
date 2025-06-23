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
# FUNCIÓN PRINCIPAL DE DENSIDAD ENERGÉTICA
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
  
  # Si existen ED_ini y ED_end, usar interpolación diaria
  if (!is.null(predator_params$ED_ini) && !is.null(predator_params$ED_end)) {
    duration <- 365  # Duración estándar
    proportion <- (day - 1) / max(1, duration - 1)
    energy_density <- predator_params$ED_ini + 
      (predator_params$ED_end - predator_params$ED_ini) * proportion
    return(clamp(energy_density, 1000, 15000))
  }
  
  # Determinar ecuación a usar
  PREDEDEQ <- predator_params$PREDEDEQ %||% 1
  
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
# FUNCIONES PARA RESOLVER PESO FINAL
# ============================================================================

#' Resolver peso final para función potencia (PREDEDEQ = 1 y 3)
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

#' Resolver peso para función lineal por segmentos (PREDEDEQ=2)
#'
#' @param available_energy Energía disponible (J)
#' @param predator_params Parámetros de densidad energética
#' @return Peso final (g)
solve_weight_linear_segments <- function(available_energy, predator_params) {
  
  Alpha1 <- as.numeric(predator_params$Alpha1)
  Beta1 <- as.numeric(predator_params$Beta1)
  Alpha2 <- as.numeric(predator_params$Alpha2)
  Beta2 <- as.numeric(predator_params$Beta2)
  Cutoff <- as.numeric(predator_params$Cutoff)
  
  # Energía en el punto de corte
  cutoff_energy <- Cutoff * (Alpha1 + Beta1 * Cutoff)
  
  if (available_energy <= cutoff_energy) {
    # Usar primer segmento
    if (abs(Beta1) < 1e-10) {
      # Beta1 ≈ 0: ED constante = Alpha1
      final_weight <- available_energy / Alpha1
    } else {
      # Resolver ecuación cuadrática: Beta1*W^2 + Alpha1*W - available_energy = 0
      discriminant <- Alpha1^2 + 4 * Beta1 * available_energy
      if (discriminant < 0) {
        stop("Discriminante negativo en ecuación cuadrática")
      }
      final_weight <- (-Alpha1 + sqrt(discriminant)) / (2 * Beta1)
    }
  } else {
    # Usar segundo segmento
    if (abs(Beta2) < 1e-10) {
      # Beta2 ≈ 0: ED constante = Alpha2
      # Energía para llegar al cutoff + energía adicional
      energy_to_cutoff <- cutoff_energy
      additional_energy <- available_energy - energy_to_cutoff
      final_weight <- Cutoff + additional_energy / Alpha2
    } else {
      # Resolver ecuación cuadrática para segundo segmento
      # Ajustar energía disponible
      adjusted_energy <- available_energy - cutoff_energy + Cutoff * (Alpha2 + Beta2 * Cutoff)
      discriminant <- Alpha2^2 + 4 * Beta2 * adjusted_energy
      if (discriminant < 0) {
        stop("Discriminante negativo en segundo segmento")
      }
      final_weight <- (-Alpha2 + sqrt(discriminant)) / (2 * Beta2)
    }
  }
  
  return(pmax(0.01, final_weight))
}

#' Resolver peso usando método iterativo (método general seguro)
#'
#' @param target_energy Energía corporal objetivo
#' @param predator_params Parámetros del depredador
#' @param day Día actual
#' @param initial_guess Estimación inicial del peso
#' @return Peso final
solve_weight_iterative <- function(target_energy, predator_params, day = 1, initial_guess = 1000) {
  
  # Función objetivo: encontrar peso donde energy_density * weight = target_energy
  objective_function <- function(weight) {
    if (weight <= 0) return(Inf)
    ed <- calculate_predator_energy_density(weight, day, predator_params)
    return(abs(ed * weight - target_energy))
  }
  
  # Usar optimización para encontrar el peso
  tryCatch({
    result <- optimize(objective_function, 
                       interval = c(0.001, 100000), 
                       tol = 1e-6)
    return(result$minimum)
  }, error = function(e) {
    warning("Error en optimización iterativa: ", e$message)
    return(initial_guess)
  })
}

# ============================================================================
# FUNCIÓN PRINCIPAL PARA CALCULAR PESO FINAL
# ============================================================================

#' Calcular peso final usando ecuaciones exactas de FB4 (CORREGIDO)
#'
#' @param initial_weight Peso inicial (g)
#' @param net_energy Energía neta disponible (J)
#' @param spawn_energy Energía perdida por reproducción (J)
#' @param species_params_predator Parámetros del depredador
#' @param day Día actual
#' @param energy_data Vector de densidades energéticas por día (para PREDEDEQ=1)
#' @return Lista con peso final y cambio de peso
#' @export
calculate_final_weight_fb4 <- function(initial_weight, net_energy, spawn_energy = 0, 
                                       species_params_predator, day = 1, energy_data = NULL) {
  
  # Validación básica
  initial_weight <- check_numeric_value(initial_weight, "initial_weight", min_val = 0.001)
  net_energy <- check_numeric_value(net_energy, "net_energy")
  spawn_energy <- check_numeric_value(spawn_energy, "spawn_energy", min_val = 0)
  
  # Energía neta después de reproducción
  net_energy_after_spawn <- net_energy - spawn_energy
  
  # Obtener parámetros
  PREDEDEQ <- as.numeric(species_params_predator$PREDEDEQ %||% 1)
  Alpha1 <- as.numeric(species_params_predator$Alpha1)
  Beta1 <- as.numeric(species_params_predator$Beta1)
  Cutoff <- as.numeric(species_params_predator$Cutoff)
  Alpha2 <- as.numeric(species_params_predator$Alpha2)
  Beta2 <- as.numeric(species_params_predator$Beta2)
  
  # Calcular energía corporal inicial
  initial_ed <- calculate_predator_energy_density(initial_weight, day, species_params_predator, energy_data)
  initial_body_energy <- initial_weight * initial_ed
  target_energy <- initial_body_energy + net_energy_after_spawn
  
  # Si la energía target es muy baja, devolver peso mínimo
  if (target_energy <= 0) {
    return(list(
      final_weight = 0.01,
      final_energy_density = initial_ed,
      weight_change = 0.01 - initial_weight
    ))
  }
  
  # Calcular peso final según PREDEDEQ
  if (PREDEDEQ == 1) {
    final_energy_density_for_calc <- calculate_predator_energy_density(initial_weight, day + 1, species_params_predator, energy_data)
    # Para PREDEDEQ=1, usar datos interpolados (densidad energética constante por día)
    final_weight <- target_energy / final_energy_density_for_calc
    
  } else if (PREDEDEQ == 2) {
    # Densidad energética lineal por segmentos
    final_weight <- solve_weight_linear_segments(target_energy, species_params_predator)
    
  } else if (PREDEDEQ == 3) {
    # Densidad energética como función potencia del peso
    
    if (initial_weight >= Cutoff) {
      # Usar parámetros Alpha1, Beta1
      final_weight <- solve_weight_power_function(
        initial_weight = initial_weight,
        net_energy = net_energy_after_spawn,
        Alpha1 = Alpha1,
        Beta1 = Beta1
      )
    } else {
      # Usar parámetros Alpha2, Beta2
      final_weight <- solve_weight_power_function(
        initial_weight = initial_weight,
        net_energy = net_energy_after_spawn,
        Alpha1 = Alpha2,
        Beta2 = Beta2
      )
    }
    
  } else {
    # Método iterativo como respaldo
    warning("PREDEDEQ no reconocido (", PREDEDEQ, "), usando método iterativo")
    final_weight <- solve_weight_iterative(target_energy, species_params_predator, day, initial_weight)
  }
  
  # Validación final
  if (is.na(final_weight) || final_weight <= 0) {
    warning("Peso final no válido, usando peso mínimo")
    final_weight <- 0.01
  }
  
  # Calcular densidad energética final
  final_ed <- calculate_predator_energy_density(final_weight, day, species_params_predator, energy_data)
  
  return(list(
    final_weight = final_weight,
    final_energy_density = final_ed,
    weight_change = final_weight - initial_weight
  ))
}

# ============================================================================
# FUNCIONES DE UTILIDAD Y VALIDACIÓN
# ============================================================================

#' Validar parámetros de densidad energética
#'
#' @param predator_params Lista con parámetros
#' @param weight_range Rango de pesos para probar
#' @param energy_data Vector de datos de energía (opcional)
#' @return Lista con resultados de validación
#' @export
validate_predator_energy_params <- function(predator_params, weight_range = c(1, 1000), energy_data = NULL) {
  
  validation <- list(
    valid = TRUE,
    errors = character(),
    warnings = character()
  )
  
  PREDEDEQ <- predator_params$PREDEDEQ %||% 1
  
  if (!PREDEDEQ %in% 1:3) {
    validation$errors <- c(validation$errors, "PREDEDEQ debe ser 1, 2, o 3")
    validation$valid <- FALSE
    return(validation)
  }
  
  # Probar cálculos en rango de pesos
  test_weights <- seq(weight_range[1], weight_range[2], length.out = 10)
  
  for (weight in test_weights) {
    tryCatch({
      ed <- calculate_predator_energy_density(weight, 1, predator_params, energy_data)
      
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

