#' Algoritmos de Ajuste para el Modelo FB4 - Versión Integrada
#'
#' @name fitting-algorithms-fb4
#' @aliases fitting-algorithms-fb4
NULL

# ============================================================================
# FUNCIONES PRINCIPALES DE AJUSTE INTEGRADAS
# ============================================================================

#' Algoritmo de búsqueda binaria para ajuste FB4
#'
#' Implementación robusta del algoritmo de búsqueda binaria basado en las funciones críticas
#'
#' @param species_params Parámetros de la especie extraídos
#' @param initial_weight Peso inicial (g)
#' @param fit_to Tipo de ajuste ("weight" o "consumption")
#' @param fit_value Valor objetivo
#' @param processed_data Datos ambientales procesados
#' @param model_options Opciones del modelo
#' @param oxycal Coeficiente oxicalórico
#' @param population_size Número de individuos
#' @param output_daily Retornar salida diaria
#' @param tolerance Tolerancia para convergencia
#' @param max_iterations Número máximo de iteraciones
#' @return Lista con resultado del ajuste y simulación final
fit_fb4_binary_search <- function(species_params, initial_weight, fit_to, fit_value,
                                  processed_data, model_options, oxycal = 13560,
                                  population_size = 1, output_daily = TRUE,
                                  tolerance = 0.001, max_iterations = 25) {
  
  # Validaciones
  if (!fit_to %in% c("weight", "consumption")) {
    stop("fit_to debe ser 'weight' o 'consumption'")
  }
  
  fit_value <- check_numeric_value(fit_value, "fit_value", min_val = 0.001)
  
  # Ejecutar búsqueda binaria usando la función crítica mejorada
  fit_result <- binary_search_p_value_robust(
    target_value = fit_value,
    fit_type = fit_to,
    species_params = species_params,
    initial_weight = initial_weight,
    processed_data = processed_data,
    model_options = model_options,
    oxycal = oxycal,
    tolerance = tolerance,
    max_iterations = max_iterations
  )
  
  # Si el ajuste fue exitoso, ejecutar simulación completa final
  if (fit_result$fit_successful) {
    final_simulation <- run_fb4_simulation_complete(
      initial_weight = initial_weight,
      p_value = fit_result$p_value,
      species_params = species_params,
      processed_data = processed_data,
      model_options = model_options,
      oxycal = oxycal,
      population_size = population_size,
      output_daily = output_daily
    )
    
    # Combinar resultados
    result <- list(
      # Información del ajuste
      fit_info = fit_result,
      # Resultados de la simulación
      final_weight = final_simulation$final_weight,
      total_consumption = final_simulation$total_consumption,
      p_value = fit_result$p_value,
      fit_successful = fit_result$fit_successful,
      fit_iterations = fit_result$iterations,
      fit_error = fit_result$final_error
    )
    
    # Añadir salida diaria si se solicita
    if (output_daily) {
      result$daily_output <- final_simulation$daily_output
    }
    
  } else {
    # Si el ajuste falló, retornar información de error
    result <- list(
      fit_info = fit_result,
      final_weight = NA,
      total_consumption = NA,
      p_value = fit_result$p_value,
      fit_successful = FALSE,
      fit_iterations = fit_result$iterations,
      fit_error = fit_result$final_error,
      error_message = "El ajuste no convergió dentro de las iteraciones permitidas"
    )
  }
  
  return(result)
}

#' Búsqueda binaria robusta para p-value (versión mejorada)
#'
#' Versión mejorada basada en la función crítica del primer script
#'
#' @param target_value Valor objetivo (peso final o consumo total)
#' @param fit_type Tipo de ajuste ("weight" o "consumption")
#' @param species_params Parámetros de la especie
#' @param initial_weight Peso inicial
#' @param processed_data Datos procesados
#' @param model_options Opciones del modelo
#' @param oxycal Coeficiente oxicalórico
#' @param tolerance Tolerancia para convergencia
#' @param max_iterations Número máximo de iteraciones
#' @return Lista con p-value ajustado y información del ajuste
binary_search_p_value_robust <- function(target_value, fit_type,
                                         species_params, initial_weight,
                                         processed_data, model_options,
                                         oxycal = 13560,
                                         tolerance = 0.001,
                                         max_iterations = 25) {
  
  # Rango inicial para p_value
  lower <- 0.01
  upper <- 5
  
  iteration <- 0
  fit_successful <- FALSE
  best_p <- NA
  best_error <- Inf
  
  # PRINT INICIAL
  cat("\n=== INICIO BÚSQUEDA BINARIA ===\n")
  cat("Target value:", target_value, "\n")
  cat("Fit type:", fit_type, "\n")
  cat("Initial weight:", initial_weight, "\n")
  cat("Rango inicial: [", lower, ",", upper, "]\n")
  cat("Tolerancia:", tolerance, "\n\n")
  
  while (iteration < max_iterations) {
    iteration <- iteration + 1
    mid_p <- (lower + upper) / 2
    
    # PRINT ITERACIÓN
    cat("Iteration", iteration, "p-value =", mid_p)
    
    # Ejecutar simulación con p_value candidato
    sim <- run_fb4_simulation_complete(
      initial_weight = initial_weight,
      p_value = mid_p,
      species_params = species_params,
      processed_data = processed_data,
      model_options = model_options,
      oxycal = oxycal,
      population_size = 1,
      output_daily = FALSE
    )
    
    # Seleccionar métrica a evaluar según fit_type
    metric <- if (fit_type == "weight") sim$final_weight else sim$total_consumption
    
    error <- abs(metric - target_value)
    
    # PRINT RESULTADOS DE LA ITERACIÓN
    cat(" W.p =", round(metric, 6), "Target =", target_value)
    cat(" Error =", round(error, 6))
    
    # Guardar mejor resultado
    if (error < best_error) {
      best_error <- error
      best_p <- mid_p
      cat(" [MEJOR]")
    }
    
    # Revisar convergencia
    if (error <= tolerance) {
      fit_successful <- TRUE
      cat(" [CONVERGIÓ]\n")
      break
    }
    
    # Ajustar rango para siguiente iteración
    if (metric < target_value) {
      lower <- mid_p
      cat(" [AUMENTAR p]")
    } else {
      upper <- mid_p
      cat(" [REDUCIR p]")
    }
    
    cat(" Nuevo rango: [", round(lower, 6), ",", round(upper, 6), "]\n")
  }
  
  # PRINT FINAL
  cat("\n=== RESULTADO FINAL ===\n")
  cat("Convergió:", fit_successful, "\n")
  cat("Iteraciones:", iteration, "\n")
  cat("Mejor p-value:", best_p, "\n")
  cat("Error final:", best_error, "\n")
  cat("=======================\n\n")
  
  return(list(
    p_value = best_p,
    fit_successful = fit_successful,
    iterations = iteration,
    final_error = best_error
  ))
}