#' Funciones de Densidad Energética del Depredador para el Modelo FB4
#'
#' @name predator-energy-density
#' @aliases predator-energy-density
NULL

# ============================================================================
# FUNCIÓN PRINCIPAL DE DENSIDAD ENERGÉTICA
# ============================================================================

#' Calcular densidad energética del depredador
#'
#' Función principal que calcula la densidad energética del depredador usando
#' diferentes ecuaciones según la configuración del modelo
#'
#' @param W Peso del pez (g)
#' @param day Día de la simulación (para interpolación de datos)
#' @param PREDEDEQ Ecuación de densidad energética (1, 2, o 3)
#' @param alpha1 Intercepto para función alométrica (primer rango)
#' @param beta1 Pendiente para función alométrica (primer rango)
#' @param alpha2 Intercepto para función alométrica (segundo rango)
#' @param beta2 Pendiente para función alométrica (segundo rango)
#' @param cutoff Peso de corte entre rangos (para PREDEDEQ = 2)
#' @param predator_energy_data Datos de energía del depredador (para PREDEDEQ = 1)
#' @return Densidad energética (J/g)
#' @export
#' @examples
#' # Usando ecuación 1 (datos interpolados)
#' pred_En_D(W = 50, day = 100, PREDEDEQ = 1,
#'           predator_energy_data = data.frame(Day = 1:365, Energy = rep(4500, 365)))
#'
#' # Usando ecuación 2 (lineal por segmentos)
#' pred_En_D(W = 50, day = 100, PREDEDEQ = 2,
#'           alpha1 = 3000, beta1 = 20, cutoff = 100,
#'           alpha2 = 5000, beta2 = 5)
#'
#' # Usando ecuación 3 (función potencia)
#' pred_En_D(W = 50, day = 100, PREDEDEQ = 3,
#'           alpha1 = 4000, beta1 = 0.1)
pred_En_D <- function(W, day, PREDEDEQ,
                      alpha1 = NULL, beta1 = NULL,
                      alpha2 = NULL, beta2 = NULL,
                      cutoff = NULL,
                      predator_energy_data = NULL) {

  # Validar entrada
  if (!PREDEDEQ %in% 1:3) {
    stop("PREDEDEQ debe ser 1, 2, o 3")
  }

  if (W <= 0) {
    stop("Peso debe ser positivo")
  }

  # Seleccionar ecuación
  if (PREDEDEQ == 1) {
    # Usar datos interpolados del archivo
    energy_density <- predator_energy_from_data(W, day, predator_energy_data)

  } else if (PREDEDEQ == 2) {
    # Función lineal por segmentos
    energy_density <- predator_energy_linear_segments(W, alpha1, beta1, alpha2, beta2, cutoff)

  } else if (PREDEDEQ == 3) {
    # Función potencia
    energy_density <- predator_energy_power_function(W, alpha1, beta1)
  }

  # Validar salida
  if (energy_density <= 0) {
    warning("Densidad energética no positiva calculada: ", energy_density)
    energy_density <- 1000  # Valor mínimo de seguridad
  }

  return(energy_density)
}

# ============================================================================
# ECUACIÓN 1: DENSIDAD ENERGÉTICA DESDE DATOS
# ============================================================================

#' Densidad energética desde datos interpolados
#'
#' Obtiene la densidad energética interpolando desde datos de entrada
#'
#' @param W Peso del pez (g) - no usado en esta ecuación
#' @param day Día de la simulación
#' @param predator_energy_data Data frame con columnas Day y Energy
#' @return Densidad energética (J/g)
#' @keywords internal
predator_energy_from_data <- function(W, day, predator_energy_data) {

  if (is.null(predator_energy_data)) {
    stop("predator_energy_data requerido para PREDEDEQ = 1")
  }

  if (!"Day" %in% names(predator_energy_data) ||
      !"Energy" %in% names(predator_energy_data)) {
    stop("predator_energy_data debe tener columnas 'Day' y 'Energy'")
  }

  # Interpolar valor para el día especificado
  energy_density <- approx(
    x = predator_energy_data$Day,
    y = predator_energy_data$Energy,
    xout = day,
    method = "linear",
    rule = 2  # Extrapolar con valores extremos
  )$y

  return(energy_density)
}

#' Procesar datos de densidad energética del depredador
#'
#' Prepara y valida datos de densidad energética para interpolación
#'
#' @param data Data frame con datos de densidad energética
#' @param first_day Primer día de simulación
#' @param last_day Último día de simulación
#' @return Vector interpolado de densidades energéticas
#' @export
process_predator_energy_data <- function(data, first_day, last_day) {

  # Validar estructura
  if (!"Day" %in% names(data)) {
    stop("Datos deben tener columna 'Day'")
  }

  energy_col <- intersect(c("Energy", "Energy_Density", "ED"), names(data))
  if (length(energy_col) == 0) {
    stop("Datos deben tener columna de energía ('Energy', 'Energy_Density', o 'ED')")
  }

  # Usar la primera columna de energía encontrada
  energy_col <- energy_col[1]

  # Interpolar para todo el rango de simulación
  sim_days <- first_day:last_day
  interpolated_energy <- approx(
    x = data$Day,
    y = data[[energy_col]],
    xout = sim_days,
    method = "linear",
    rule = 2
  )$y

  # Extender un día más para cálculos finales
  next_day_energy <- approx(
    x = data$Day,
    y = data[[energy_col]],
    xout = last_day + 1,
    method = "linear",
    rule = 2
  )$y

  return(c(interpolated_energy, next_day_energy))
}

# ============================================================================
# ECUACIÓN 2: FUNCIÓN LINEAL POR SEGMENTOS
# ============================================================================

#' Densidad energética lineal por segmentos
#'
#' Calcula densidad energética usando función lineal con dos segmentos
#'
#' @param W Peso del pez (g)
#' @param alpha1 Intercepto para primer segmento
#' @param beta1 Pendiente para primer segmento
#' @param alpha2 Intercepto para segundo segmento
#' @param beta2 Pendiente para segundo segmento
#' @param cutoff Peso de corte entre segmentos
#' @return Densidad energética (J/g)
#' @export
#' @examples
#' # Densidad energética aumenta linealmente hasta 100g, luego más lentamente
#' predator_energy_linear_segments(W = 50, alpha1 = 3000, beta1 = 20,
#'                                 alpha2 = 5000, beta2 = 5, cutoff = 100)
predator_energy_linear_segments <- function(W, alpha1, beta1, alpha2, beta2, cutoff) {

  # Validar parámetros
  if (is.null(alpha1) || is.null(beta1) || is.null(cutoff)) {
    stop("alpha1, beta1 y cutoff son requeridos para PREDEDEQ = 2")
  }

  cutoff <- as.numeric(cutoff)

  if (W < cutoff) {
    # Primer segmento
    if (beta1 == 0) {
      energy_density <- alpha1
    } else {
      energy_density <- alpha1 + beta1 * W
    }
  } else {
    # Segundo segmento
    if (is.null(alpha2) || is.null(beta2)) {
      stop("alpha2 y beta2 son requeridos para pesos >= cutoff")
    }

    if (beta2 == 0) {
      energy_density <- alpha2
    } else {
      energy_density <- alpha2 + beta2 * W
    }
  }

  return(energy_density)
}

#' Validar parámetros de función lineal por segmentos
#'
#' Verifica continuidad y validez de parámetros
#'
#' @param alpha1 Intercepto primer segmento
#' @param beta1 Pendiente primer segmento
#' @param alpha2 Intercepto segundo segmento
#' @param beta2 Pendiente segundo segmento
#' @param cutoff Peso de corte
#' @return Lista con validación y advertencias
#' @export
validate_linear_segments_params <- function(alpha1, beta1, alpha2, beta2, cutoff) {

  warnings <- character()
  valid <- TRUE

  # Verificar continuidad en el punto de corte
  energy_at_cutoff_seg1 <- alpha1 + beta1 * cutoff
  energy_at_cutoff_seg2 <- alpha2 + beta2 * cutoff

  discontinuity <- abs(energy_at_cutoff_seg1 - energy_at_cutoff_seg2)
  if (discontinuity > 100) {  # Tolerancia de 100 J/g
    warnings <- c(warnings,
                  paste("Discontinuidad significativa en cutoff:",
                        round(discontinuity, 1), "J/g"))
  }

  # Verificar que las energías sean positivas en rango típico
  test_weights <- c(1, cutoff/2, cutoff, cutoff*2, cutoff*5)

  for (w in test_weights) {
    energy <- predator_energy_linear_segments(w, alpha1, beta1, alpha2, beta2, cutoff)
    if (energy <= 0) {
      warnings <- c(warnings,
                    paste("Energía no positiva para peso", w, "g:", energy))
      valid <- FALSE
    }
    if (energy > 10000) {
      warnings <- c(warnings,
                    paste("Energía muy alta para peso", w, "g:", energy))
    }
  }

  return(list(
    valid = valid,
    warnings = warnings,
    discontinuity = discontinuity,
    energy_at_cutoff_seg1 = energy_at_cutoff_seg1,
    energy_at_cutoff_seg2 = energy_at_cutoff_seg2
  ))
}

# ============================================================================
# ECUACIÓN 3: FUNCIÓN POTENCIA
# ============================================================================

#' Densidad energética función potencia
#'
#' Calcula densidad energética usando ED = alpha1 * W^beta1
#'
#' @param W Peso del pez (g)
#' @param alpha1 Coeficiente multiplicativo
#' @param beta1 Exponente
#' @return Densidad energética (J/g)
#' @export
#' @examples
#' # Densidad energética que aumenta con el peso
#' predator_energy_power_function(W = 50, alpha1 = 4000, beta1 = 0.1)
#'
#' # Densidad energética constante (beta1 = 0)
#' predator_energy_power_function(W = 50, alpha1 = 4500, beta1 = 0)
predator_energy_power_function <- function(W, alpha1, beta1) {

  # Validar parámetros
  if (is.null(alpha1) || is.null(beta1)) {
    stop("alpha1 y beta1 son requeridos para PREDEDEQ = 3")
  }

  if (alpha1 <= 0) {
    stop("alpha1 debe ser positivo")
  }

  # Calcular densidad energética
  energy_density <- alpha1 * (W^beta1)

  return(energy_density)
}

#' Validar parámetros de función potencia
#'
#' Verifica que los parámetros produzcan valores realistas
#'
#' @param alpha1 Coeficiente multiplicativo
#' @param beta1 Exponente
#' @param weight_range Rango de pesos para probar (vector de 2 elementos)
#' @return Lista con validación y advertencias
#' @export
validate_power_function_params <- function(alpha1, beta1, weight_range = c(1, 1000)) {

  warnings <- character()
  valid <- TRUE

  # Verificar parámetros básicos
  if (alpha1 <= 0) {
    warnings <- c(warnings, "alpha1 debe ser positivo")
    valid <- FALSE
  }

  if (abs(beta1) > 2) {
    warnings <- c(warnings,
                  paste("beta1 fuera de rango típico (-2 a 2):", beta1))
  }

  # Probar en rango de pesos
  test_weights <- c(weight_range[1],
                    exp(seq(log(weight_range[1]), log(weight_range[2]), length.out = 5)))

  energies <- sapply(test_weights, function(w) {
    predator_energy_power_function(w, alpha1, beta1)
  })

  # Verificar rango de energías
  if (any(energies <= 0)) {
    warnings <- c(warnings, "Algunas energías calculadas no son positivas")
    valid <- FALSE
  }

  if (any(energies > 15000)) {
    warnings <- c(warnings, "Algunas energías calculadas son muy altas (>15000 J/g)")
  }

  if (any(energies < 1000)) {
    warnings <- c(warnings, "Algunas energías calculadas son muy bajas (<1000 J/g)")
  }

  # Verificar tendencia
  if (beta1 > 0 && energies[length(energies)] < energies[1]) {
    warnings <- c(warnings, "Tendencia inconsistente: beta1 > 0 pero energía decrece")
  }

  if (beta1 < 0 && energies[length(energies)] > energies[1]) {
    warnings <- c(warnings, "Tendencia inconsistente: beta1 < 0 pero energía aumenta")
  }

  return(list(
    valid = valid,
    warnings = warnings,
    test_weights = test_weights,
    test_energies = energies,
    energy_range = range(energies)
  ))
}

# ============================================================================
# FUNCIONES DE APOYO PARA CÁLCULOS DE PESO
# ============================================================================

#' Calcular peso final dados energía ganada y densidad energética
#'
#' Función auxiliar para calcular el peso final del pez usando diferentes
#' ecuaciones de densidad energética
#'
#' @param initial_weight Peso inicial (g)
#' @param energy_gain Ganancia neta de energía (J)
#' @param spawn_energy Energía perdida por reproducción (J)
#' @param PREDEDEQ Ecuación de densidad energética
#' @param alpha1 Parámetro alpha1
#' @param beta1 Parámetro beta1
#' @param alpha2 Parámetro alpha2 (para PREDEDEQ = 2)
#' @param beta2 Parámetro beta2 (para PREDEDEQ = 2)
#' @param cutoff Peso de corte (para PREDEDEQ = 2)
#' @param initial_energy_density Densidad energética inicial (para PREDEDEQ = 1)
#' @param final_energy_density Densidad energética final (para PREDEDEQ = 1)
#' @return Lista con peso final y densidad energética final
#' @export
calculate_final_weight_with_energy_density <- function(initial_weight,
                                                       energy_gain,
                                                       spawn_energy = 0,
                                                       PREDEDEQ,
                                                       alpha1 = NULL,
                                                       beta1 = NULL,
                                                       alpha2 = NULL,
                                                       beta2 = NULL,
                                                       cutoff = NULL,
                                                       initial_energy_density = NULL,
                                                       final_energy_density = NULL) {

  # Energía neta disponible
  net_energy <- energy_gain - spawn_energy
  initial_body_energy <- initial_weight * initial_energy_density

  if (PREDEDEQ == 1) {
    # Usar densidades energéticas fijas
    if (is.null(final_energy_density)) {
      stop("final_energy_density requerida para PREDEDEQ = 1")
    }

    final_weight <- (initial_body_energy + net_energy) / final_energy_density
    final_ed <- final_energy_density

  } else if (PREDEDEQ == 3) {
    # Función potencia: ED = alpha1 * W^beta1
    # Resolver: net_energy = final_weight * alpha1 * final_weight^beta1 - initial_weight * alpha1 * initial_weight^beta1

    if (beta1 == 0) {
      # Caso especial: densidad energética constante
      final_weight <- (initial_body_energy + net_energy) / alpha1
    } else {
      # Resolver ecuación: alpha1 * W^(beta1+1) = initial_body_energy + net_energy
      final_weight <- ((initial_body_energy + net_energy) / alpha1)^(1/(beta1 + 1))
    }

    final_ed <- predator_energy_power_function(final_weight, alpha1, beta1)

  } else if (PREDEDEQ == 2) {
    # Función lineal por segmentos - más complejo
    final_weight <- solve_linear_segments_weight(
      initial_weight, net_energy, initial_body_energy,
      alpha1, beta1, alpha2, beta2, cutoff
    )

    final_ed <- predator_energy_linear_segments(final_weight, alpha1, beta1, alpha2, beta2, cutoff)
  }

  return(list(
    final_weight = final_weight,
    final_energy_density = final_ed
  ))
}

#' Resolver peso final para función lineal por segmentos
#'
#' Función auxiliar para calcular peso final con PREDEDEQ = 2
#'
#' @param initial_weight Peso inicial
#' @param net_energy Energía neta disponible
#' @param initial_body_energy Energía corporal inicial
#' @param alpha1 Parámetros de la función lineal
#' @param beta1 Parámetros de la función lineal
#' @param alpha2 Parámetros de la función lineal
#' @param beta2 Parámetros de la función lineal
#' @param cutoff Peso de corte
#' @return Peso final
#' @keywords internal
solve_linear_segments_weight <- function(initial_weight, net_energy, initial_body_energy,
                                         alpha1, beta1, alpha2, beta2, cutoff) {

  cutoff <- as.numeric(cutoff)

  if (initial_weight < cutoff) {
    # Peso inicial en primer segmento
    if (beta1 != 0) {
      # Resolver ecuación cuadrática
      # beta1 * W^2 + alpha1 * W = initial_body_energy + net_energy
      a <- beta1
      b <- alpha1
      c <- -(initial_body_energy + net_energy)

      discriminant <- b^2 - 4*a*c
      if (discriminant < 0) {
        stop("Discriminante negativo en cálculo de peso final")
      }

      final_weight <- (-b + sqrt(discriminant)) / (2*a)
    } else {
      # beta1 = 0: densidad energética constante
      final_weight <- (initial_body_energy + net_energy) / alpha1
    }

    # Verificar si cruzamos el cutoff
    if (final_weight > cutoff) {
      # Recalcular considerando el cruce al segundo segmento
      energy_to_cutoff <- cutoff * (alpha1 + beta1 * cutoff) -
        initial_weight * (alpha1 + beta1 * initial_weight)
      remaining_energy <- net_energy - energy_to_cutoff

      if (beta2 != 0) {
        a <- beta2
        b <- alpha2
        c <- -(cutoff * (alpha2 + beta2 * cutoff) + remaining_energy)

        discriminant <- b^2 - 4*a*c
        if (discriminant < 0) {
          stop("Discriminante negativo en segundo segmento")
        }

        final_weight <- (-b + sqrt(discriminant)) / (2*a)
      } else {
        final_weight <- (remaining_energy + cutoff * alpha2) / alpha2
      }
    }

  } else {
    # Peso inicial en segundo segmento
    if (beta2 != 0) {
      a <- beta2
      b <- alpha2
      c <- -(initial_body_energy + net_energy)

      discriminant <- b^2 - 4*a*c
      if (discriminant < 0) {
        stop("Discriminante negativo en segundo segmento")
      }

      final_weight <- (-b + sqrt(discriminant)) / (2*a)
    } else {
      final_weight <- (initial_body_energy + net_energy) / alpha2
    }

    # Verificar si bajamos del cutoff
    if (final_weight < cutoff) {
      # Recalcular considerando el cruce al primer segmento
      energy_loss_to_cutoff <- initial_weight * (alpha2 + beta2 * initial_weight) -
        cutoff * (alpha1 + beta1 * cutoff)
      remaining_energy <- net_energy + energy_loss_to_cutoff

      if (beta1 != 0) {
        a <- beta1
        b <- alpha1
        c <- -(remaining_energy + cutoff * (alpha1 + beta1 * cutoff))

        discriminant <- b^2 - 4*a*c
        if (discriminant < 0) {
          stop("Discriminante negativo en primer segmento")
        }

        final_weight <- (-b + sqrt(discriminant)) / (2*a)
      } else {
        final_weight <- (remaining_energy + cutoff * alpha1) / alpha1
      }
    }
  }

  return(final_weight)
}

# ============================================================================
# FUNCIONES DE VISUALIZACIÓN Y ANÁLISIS
# ============================================================================

#' Graficar densidad energética vs peso
#'
#' Crea gráfico de la relación entre peso y densidad energética
#'
#' @param PREDEDEQ Ecuación a graficar
#' @param weight_range Rango de pesos (vector de 2 elementos)
#' @param n_points Número de puntos para graficar
#' @param ... Parámetros específicos de la ecuación
#' @return Gráfico de densidad energética
#' @export
plot_predator_energy_density <- function(PREDEDEQ,
                                         weight_range = c(1, 500),
                                         n_points = 100,
                                         ...) {

  # Crear secuencia de pesos
  if (PREDEDEQ == 3) {
    # Usar escala logarítmica para función potencia
    weights <- exp(seq(log(weight_range[1]), log(weight_range[2]), length.out = n_points))
  } else {
    weights <- seq(weight_range[1], weight_range[2], length.out = n_points)
  }

  # Calcular densidades energéticas
  params <- list(...)
  energies <- sapply(weights, function(w) {
    pred_En_D(W = w, day = 1, PREDEDEQ = PREDEDEQ, ...)
  })

  # Crear gráfico
  plot(weights, energies, type = "l", lwd = 2, col = "blue",
       xlab = "Peso (g)", ylab = "Densidad Energética (J/g)",
       main = paste("Densidad Energética del Depredador - Ecuación", PREDEDEQ))

  # Agregar líneas de referencia si es función por segmentos
  if (PREDEDEQ == 2 && !is.null(params$cutoff)) {
    abline(v = params$cutoff, lty = 2, col = "red")
    text(params$cutoff, max(energies) * 0.9, "Cutoff", pos = 4, col = "red")
  }

  grid()

  return(invisible(data.frame(Weight = weights, Energy_Density = energies)))
}

#' Comparar ecuaciones de densidad energética
#'
#' Grafica múltiples ecuaciones para comparación
#'
#' @param equations Lista con parámetros para cada ecuación
#' @param weight_range Rango de pesos
#' @param labels Etiquetas para cada ecuación
#' @return Gráfico comparativo
#' @export
compare_predator_energy_equations <- function(equations,
                                              weight_range = c(1, 500),
                                              labels = NULL) {

  if (is.null(labels)) {
    labels <- paste("Ecuación", 1:length(equations))
  }

  colors <- rainbow(length(equations))
  weights <- seq(weight_range[1], weight_range[2], length.out = 200)

  # Configurar gráfico
  plot(NULL, xlim = weight_range, ylim = c(0, 8000),
       xlab = "Peso (g)", ylab = "Densidad Energética (J/g)",
       main = "Comparación de Ecuaciones de Densidad Energética")

  # Graficar cada ecuación
  for (i in seq_along(equations)) {
    eq <- equations[[i]]
    energies <- sapply(weights, function(w) {
      do.call(pred_En_D, c(list(W = w, day = 1), eq))
    })

    lines(weights, energies, lwd = 2, col = colors[i])
  }

  # Agregar leyenda
  legend("topright", legend = labels, col = colors, lwd = 2, cex = 0.8)
  grid()

  invisible(NULL)
}
