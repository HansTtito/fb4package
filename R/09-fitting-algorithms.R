#' Algoritmos de Ajuste para el Modelo FB4
#'
#' @name fitting-algorithms
#' @aliases fitting-algorithms
NULL

# ============================================================================
# FUNCIONES CORE DE AJUSTE
# ============================================================================

#' Búsqueda binaria para ajustar p-value
#'
#' Encuentra el p-value que produce el peso final o consumo total deseado
#'
#' @param target_value Valor objetivo (peso final o consumo total)
#' @param fit_type Tipo de ajuste ("weight", "consumption")
#' @param bioenergetic_params Lista con parámetros del modelo
#' @param p_initial Valor inicial de p para comenzar la búsqueda
#' @param tolerance Tolerancia para la convergencia
#' @param max_iterations Número máximo de iteraciones
#' @return Lista con p-value ajustado y información del ajuste
#' @keywords internal
binary_search_p_value <- function(target_value, fit_type, bioenergetic_params,
                                  p_initial = 0.5, tolerance = 0.001, max_iterations = 25) {
  
  # Validaciones
  target_value <- check_numeric_value(target_value, "target_value", min_val = 0.001)
  
  if (!fit_type %in% c("weight", "consumption")) {
    stop("fit_type debe ser 'weight' o 'consumption'")
  }
  
  # Inicializar parámetros de búsqueda
  p_current <- clamp(p_initial, 0.001, 4.999)
  p_min <- 0.001
  p_max <- 4.999
  
  results <- list(
    p_value = p_current,
    fit_successful = FALSE,
    iterations = 0,
    final_error = Inf,
    target_value = target_value,
    achieved_value = NA
  )
  
  # Bucle de búsqueda binaria
  for (iteration in 1:max_iterations) {
    results$iterations <- iteration
    
    # Ejecutar modelo con p-value actual
    sim_result <- safe_run_bioenergetic_model(bioenergetic_params, p_current)
    
    if (is.null(sim_result)) {
      # Si la simulación falla, ajustar límites
      if (p_current > p_initial) {
        p_max <- p_current
      } else {
        p_min <- p_current
      }
      current_value <- -999  # Valor que indica falla
    } else {
      # Obtener valor actual según tipo de ajuste
      if (fit_type == "weight") {
        current_value <- sim_result$final_weight
      } else {
        current_value <- sim_result$total_consumption
      }
      
      results$achieved_value <- current_value
      
      # Verificar convergencia
      error <- abs(current_value - target_value)
      results$final_error <- error
      
      if (error <= tolerance) {
        results$fit_successful <- TRUE
        results$p_value <- p_current
        break
      }
      
      # Ajustar límites de búsqueda
      if (current_value > target_value) {
        p_max <- p_current
      } else {
        p_min <- p_current
      }
    }
    
    # Calcular nuevo p-value (punto medio)
    p_new <- (p_min + p_max) / 2
    
    # Verificar si hemos alcanzado límites de precisión
    if (abs(p_new - p_current) < 1e-10) {
      break
    }
    
    p_current <- p_new
  }
  
  # Verificación final de límites
  if (p_current <= 0.001) {
    results$warnings <- "p-value en límite inferior"
  } else if (p_current >= 4.999) {
    results$warnings <- "p-value en límite superior"
  }
  
  results$p_value <- p_current
  return(results)
}

#' Ejecutar modelo bioenergético de forma segura
#'
#' Envuelve la ejecución del modelo con manejo de errores
#'
#' @param bioenergetic_params Lista con parámetros del modelo
#' @param p_value Valor de p para la simulación
#' @return Resultado de la simulación o NULL si falla
#' @keywords internal
safe_run_bioenergetic_model <- function(bioenergetic_params, p_value) {
  
  tryCatch({
    # Extraer parámetros necesarios
    initial_weight <- bioenergetic_params$initial_weight %||% 10
    species_params <- bioenergetic_params$species_params
    env_data <- bioenergetic_params$environmental_data
    
    if (is.null(species_params) || is.null(env_data)) {
      stop("Parámetros o datos ambientales faltantes")
    }
    
    # Ejecutar simulación simplificada
    result <- run_bioenergetic_simulation_core(
      initial_weight = initial_weight,
      p_value = p_value,
      species_params = species_params,
      environmental_data = env_data,
      return_daily = FALSE
    )
    
    # Validar resultado
    if (is.null(result) || is.na(result$final_weight) || result$final_weight <= 0) {
      return(NULL)
    }
    
    return(result)
    
  }, error = function(e) {
    return(NULL)
  })
}

#' Simulación bioenergética simplificada para ajuste
#'
#' Versión simplificada del modelo bioenergético para uso en optimización
#'
#' @param initial_weight Peso inicial (g)
#' @param p_value Valor de p
#' @param species_params Parámetros de la especie
#' @param environmental_data Datos ambientales
#' @param return_daily Retornar datos diarios (lógico)
#' @return Resultado de la simulación
#' @keywords internal
run_bioenergetic_simulation_core <- function(initial_weight, p_value, species_params,
                                             environmental_data, return_daily = FALSE) {
  
  # Validar datos de temperatura
  if (is.null(environmental_data$temperature)) {
    stop("Datos de temperatura requeridos")
  }
  
  temp_data <- environmental_data$temperature
  n_days <- nrow(temp_data)
  
  # Inicializar variables
  current_weight <- initial_weight
  total_consumption <- 0
  
  # Variables para salida diaria (si se requiere)
  if (return_daily) {
    daily_results <- data.frame(
      Day = temp_data$Day,
      Temperature = temp_data$Temperature,
      Weight = numeric(n_days),
      Consumption = numeric(n_days),
      stringsAsFactors = FALSE
    )
  }
  
  # Simulación día a día
  for (i in 1:n_days) {
    day <- temp_data$Day[i]
    temperature <- temp_data$Temperature[i]
    
    # Calcular consumo máximo del día
    max_consumption <- calculate_maximum_consumption(
      weight = current_weight,
      temperature = temperature,
      species_params = species_params
    )
    
    # Calcular consumo real basado en p-value
    daily_consumption <- p_value * max_consumption
    
    # Calcular respiración
    daily_respiration <- calculate_daily_respiration(
      weight = current_weight,
      temperature = temperature,
      consumption = daily_consumption,
      species_params = species_params
    )
    
    # Calcular crecimiento neto
    net_energy <- (daily_consumption - daily_respiration) * 
      species_params$energy_density$prey %||% 4000
    
    # Convertir a cambio de peso
    weight_change <- net_energy / (species_params$energy_density$fish %||% 5000)
    
    # Actualizar peso
    current_weight <- pmax(0.1, current_weight + weight_change)
    
    # Acumular consumo total
    total_consumption <- total_consumption + daily_consumption
    
    # Guardar datos diarios si se requiere
    if (return_daily) {
      daily_results$Weight[i] <- current_weight
      daily_results$Consumption[i] <- daily_consumption
    }
    
    # Verificar supervivencia (peso mínimo)
    if (current_weight < 0.1) {
      warning("Peso por debajo del mínimo de supervivencia")
      break
    }
  }
  
  # Preparar resultado
  result <- list(
    final_weight = current_weight,
    total_consumption = total_consumption,
    initial_weight = initial_weight,
    simulation_days = n_days
  )
  
  if (return_daily) {
    result$daily_data <- daily_results
  }
  
  return(result)
}

# ============================================================================
# FUNCIONES PRINCIPALES DE AJUSTE
# ============================================================================

#' Ajustar modelo bioenergético a peso objetivo
#'
#' Encuentra el p-value que produce el peso final deseado
#'
#' @param bioenergetic_obj Objeto de clase Bioenergetic
#' @param target_weight Peso final objetivo (g)
#' @param tolerance Tolerancia para convergencia
#' @param max_iterations Número máximo de iteraciones
#' @return Lista con resultado del ajuste
#' @export
fit_to_target_weight <- function(bioenergetic_obj, target_weight, 
                                 tolerance = 0.001, max_iterations = 25) {
  
  if (!is.Bioenergetic(bioenergetic_obj)) {
    stop("Objeto debe ser de clase Bioenergetic")
  }
  
  # Preparar parámetros para ajuste
  bioenergetic_params <- list(
    initial_weight = bioenergetic_obj$simulation_settings$initial_weight,
    species_params = bioenergetic_obj$species_parameters,
    environmental_data = bioenergetic_obj$environmental_data
  )
  
  # Ejecutar búsqueda binaria
  fit_result <- binary_search_p_value(
    target_value = target_weight,
    fit_type = "weight",
    bioenergetic_params = bioenergetic_params,
    tolerance = tolerance,
    max_iterations = max_iterations
  )
  
  # Ejecutar simulación final con p-value encontrado
  if (fit_result$fit_successful) {
    final_simulation <- run_bioenergetic_simulation_core(
      initial_weight = bioenergetic_params$initial_weight,
      p_value = fit_result$p_value,
      species_params = bioenergetic_params$species_params,
      environmental_data = bioenergetic_params$environmental_data,
      return_daily = TRUE
    )
    
    fit_result$simulation_result <- final_simulation
  }
  
  return(fit_result)
}

#' Ajustar modelo bioenergético a consumo objetivo
#'
#' Encuentra el p-value que produce el consumo total deseado
#'
#' @param bioenergetic_obj Objeto de clase Bioenergetic
#' @param target_consumption Consumo total objetivo (g)
#' @param tolerance Tolerancia para convergencia
#' @param max_iterations Número máximo de iteraciones
#' @return Lista con resultado del ajuste
#' @export
fit_to_target_consumption <- function(bioenergetic_obj, target_consumption,
                                      tolerance = 0.001, max_iterations = 25) {
  
  if (!is.Bioenergetic(bioenergetic_obj)) {
    stop("Objeto debe ser de clase Bioenergetic")
  }
  
  # Preparar parámetros para ajuste
  bioenergetic_params <- list(
    initial_weight = bioenergetic_obj$simulation_settings$initial_weight,
    species_params = bioenergetic_obj$species_parameters,
    environmental_data = bioenergetic_obj$environmental_data
  )
  
  # Ejecutar búsqueda binaria
  fit_result <- binary_search_p_value(
    target_value = target_consumption,
    fit_type = "consumption",
    bioenergetic_params = bioenergetic_params,
    tolerance = tolerance,
    max_iterations = max_iterations
  )
  
  # Ejecutar simulación final con p-value encontrado
  if (fit_result$fit_successful) {
    final_simulation <- run_bioenergetic_simulation_core(
      initial_weight = bioenergetic_params$initial_weight,
      p_value = fit_result$p_value,
      species_params = bioenergetic_params$species_params,
      environmental_data = bioenergetic_params$environmental_data,
      return_daily = TRUE
    )
    
    fit_result$simulation_result <- final_simulation
  }
  
  return(fit_result)
}

#' Ejecutar modelo con p-value específico
#'
#' Ejecuta el modelo bioenergético con un p-value determinado
#'
#' @param bioenergetic_obj Objeto de clase Bioenergetic
#' @param p_value Valor de p a usar
#' @param return_daily Retornar datos diarios
#' @return Resultado de la simulación
#' @export
run_with_p_value <- function(bioenergetic_obj, p_value, return_daily = TRUE) {
  
  if (!is.Bioenergetic(bioenergetic_obj)) {
    stop("Objeto debe ser de clase Bioenergetic")
  }
  
  # Validar p-value
  p_value <- check_numeric_value(p_value, "p_value", min_val = 0.001, max_val = 5)
  
  # Ejecutar simulación
  result <- run_bioenergetic_simulation_core(
    initial_weight = bioenergetic_obj$simulation_settings$initial_weight,
    p_value = p_value,
    species_params = bioenergetic_obj$species_parameters,
    environmental_data = bioenergetic_obj$environmental_data,
    return_daily = return_daily
  )
  
  # Añadir información del p-value
  result$p_value <- p_value
  result$calculation_method <- "p_value"
  
  return(result)
}

# ============================================================================
# FUNCIONES DE CALIBRACIÓN
# ============================================================================

#' Calibrar parámetro de especie simple
#'
#' Ajusta un parámetro específico para alcanzar un objetivo
#'
#' @param bioenergetic_obj Objeto de clase Bioenergetic
#' @param parameter_name Nombre del parámetro a calibrar
#' @param target_value Valor objetivo
#' @param target_metric Métrica objetivo ("final_weight", "total_consumption")
#' @param parameter_range Rango de búsqueda para el parámetro
#' @param tolerance Tolerancia para convergencia
#' @return Lista con resultado de la calibración
#' @export
calibrate_single_parameter <- function(bioenergetic_obj, parameter_name, target_value,
                                       target_metric = "final_weight",
                                       parameter_range = NULL, tolerance = 0.01) {
  
  if (!is.Bioenergetic(bioenergetic_obj)) {
    stop("Objeto debe ser de clase Bioenergetic")
  }
  
  # Determinar rango de búsqueda si no se especifica
  if (is.null(parameter_range)) {
    current_value <- get_parameter_value(bioenergetic_obj$species_parameters, parameter_name)
    if (is.null(current_value)) {
      stop("Parámetro no encontrado: ", parameter_name)
    }
    parameter_range <- c(current_value * 0.1, current_value * 10)
  }
  
  # Función objetivo para optimización
  objective_function <- function(param_value) {
    # Crear copia temporal del objeto
    temp_obj <- bioenergetic_obj
    temp_obj$species_parameters <- set_parameter_value(
      temp_obj$species_parameters, parameter_name, param_value
    )
    
    # Ejecutar simulación
    result <- tryCatch({
      run_bioenergetic_simulation_core(
        initial_weight = temp_obj$simulation_settings$initial_weight,
        p_value = 0.5,  # p-value fijo para calibración
        species_params = temp_obj$species_parameters,
        environmental_data = temp_obj$environmental_data,
        return_daily = FALSE
      )
    }, error = function(e) NULL)
    
    if (is.null(result)) {
      return(1e6)  # Error grande si falla la simulación
    }
    
    # Calcular error según métrica objetivo
    if (target_metric == "final_weight") {
      achieved_value <- result$final_weight
    } else if (target_metric == "total_consumption") {
      achieved_value <- result$total_consumption
    } else {
      stop("target_metric no válido")
    }
    
    return(abs(achieved_value - target_value))
  }
  
  # Optimización usando búsqueda en cuadrícula simple
  search_values <- seq(parameter_range[1], parameter_range[2], length.out = 50)
  errors <- sapply(search_values, objective_function)
  
  # Encontrar mejor valor
  best_idx <- which.min(errors)
  best_parameter <- search_values[best_idx]
  best_error <- errors[best_idx]
  
  # Verificar convergencia
  fit_successful <- best_error <= tolerance
  
  # Ejecutar simulación final
  final_obj <- bioenergetic_obj
  final_obj$species_parameters <- set_parameter_value(
    final_obj$species_parameters, parameter_name, best_parameter
  )
  
  final_simulation <- run_bioenergetic_simulation_core(
    initial_weight = final_obj$simulation_settings$initial_weight,
    p_value = 0.5,
    species_params = final_obj$species_parameters,
    environmental_data = final_obj$environmental_data,
    return_daily = TRUE
  )
  
  return(list(
    parameter_name = parameter_name,
    calibrated_value = best_parameter,
    target_value = target_value,
    achieved_value = if (target_metric == "final_weight") final_simulation$final_weight else final_simulation$total_consumption,
    final_error = best_error,
    fit_successful = fit_successful,
    simulation_result = final_simulation
  ))
}

# ============================================================================
# FUNCIONES DE UTILIDAD
# ============================================================================

#' Validar resultado de ajuste
#'
#' @param fit_result Resultado de función de ajuste
#' @return Lista con validación
#' @export
validate_fit_result <- function(fit_result) {
  
  validation <- list(
    valid = TRUE,
    warnings = character(),
    errors = character()
  )
  
  # Verificar estructura básica
  required_fields <- c("p_value", "fit_successful", "iterations")
  missing_fields <- setdiff(required_fields, names(fit_result))
  
  if (length(missing_fields) > 0) {
    validation$errors <- c(validation$errors, 
                           paste("Campos faltantes:", paste(missing_fields, collapse = ", ")))
    validation$valid <- FALSE
  }
  
  # Verificar convergencia
  if ("fit_successful" %in% names(fit_result)) {
    if (!fit_result$fit_successful) {
      validation$warnings <- c(validation$warnings, "El ajuste no convergió")
    }
  }
  
  # Verificar p-value
  if ("p_value" %in% names(fit_result)) {
    p_val <- fit_result$p_value
    if (p_val <= 0.001) {
      validation$warnings <- c(validation$warnings, "p-value muy bajo (posible subalimentación)")
    } else if (p_val >= 4.999) {
      validation$warnings <- c(validation$warnings, "p-value muy alto (posible sobrealimentación)")
    }
  }
  
  # Verificar error final
  if ("final_error" %in% names(fit_result)) {
    if (!is.na(fit_result$final_error) && fit_result$final_error > 0.1) {
      validation$warnings <- c(validation$warnings, "Error de ajuste alto")
    }
  }
  
  return(validation)
}

#' Generar resumen de ajuste
#'
#' @param fit_result Resultado de función de ajuste
#' @return Data frame con resumen
#' @export
summarize_fit_result <- function(fit_result) {
  
  summary_data <- data.frame(
    Parameter = character(),
    Value = character(),
    stringsAsFactors = FALSE
  )
  
  # Información básica
  if ("p_value" %in% names(fit_result)) {
    summary_data <- rbind(summary_data, 
                          data.frame(Parameter = "P-value", 
                                     Value = round(fit_result$p_value, 4)))
  }
  
  if ("fit_successful" %in% names(fit_result)) {
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Fit successful",
                                     Value = fit_result$fit_successful))
  }
  
  if ("iterations" %in% names(fit_result)) {
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Iterations",
                                     Value = fit_result$iterations))
  }
  
  if ("final_error" %in% names(fit_result)) {
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Final error",
                                     Value = round(fit_result$final_error, 4)))
  }
  
  if ("target_value" %in% names(fit_result)) {
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Target value",
                                     Value = round(fit_result$target_value, 2)))
  }
  
  if ("achieved_value" %in% names(fit_result)) {
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Achieved value",
                                     Value = round(fit_result$achieved_value, 2)))
  }
  
  return(summary_data)
}