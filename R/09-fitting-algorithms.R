#' Algoritmos de Ajuste para el Modelo FB4
#'
#' @name fitting-algorithms
#' @aliases fitting-algorithms
NULL

#' Algoritmo de búsqueda binaria para ajustar p-value
#'
#' Encuentra el p-value que produce el peso final o consumo total deseado
#'
#' @param p_initial Valor inicial de p para comenzar la búsqueda
#' @param initial_weight Peso inicial del pez (g)
#' @param target_value Valor objetivo (peso final o consumo total)
#' @param tolerance Tolerancia para la convergencia
#' @param max_iterations Número máximo de iteraciones
#' @param fit_type Tipo de ajuste ("Weight" o "Consumption")
#' @param species_params Parámetros de la especie
#' @param processed_data Datos procesados de entrada
#' @param model_options Opciones del modelo
#' @param oxycal Coeficiente oxicalórico
#' @param population_size Tamaño de población
#' @param progress_callback Función callback para progreso
#'
#' @return Lista con p-value ajustado y bandera de éxito
#' @export
fit_p_value <- function(p_initial = 0.5,
                        initial_weight,
                        target_value,
                        tolerance = 0.0001,
                        max_iterations = 25,
                        fit_type = "Weight",
                        species_params,
                        processed_data,
                        model_options,
                        oxycal = 13560,
                        population_size = 1,
                        progress_callback = NULL) {

  # Validar entradas
  if (!fit_type %in% c("Weight", "Consumption")) {
    stop("fit_type debe ser 'Weight' o 'Consumption'")
  }

  if (target_value <= 0) {
    stop("target_value debe ser positivo")
  }

  # Inicializar parámetros de búsqueda
  p <- p_initial
  n_iter <- 0
  p_max <- 5.00  # límite superior
  p_min <- 0.00  # límite inferior
  fit_successful <- FALSE

  # Mostrar progreso si se proporciona callback
  if (!is.null(progress_callback)) {
    progress_callback(0, "Iniciando ajuste de p-value...")
  }

  # Bucle de búsqueda binaria
  repeat {
    n_iter <- n_iter + 1

    # Actualizar progreso
    if (!is.null(progress_callback)) {
      progress_callback(n_iter / max_iterations,
                        paste("Iteración", n_iter, "- p =", round(p, 4)))
    }

    # Ejecutar modelo con p-value actual
    result <- tryCatch({
      run_growth_model_internal(
        initial_weight = initial_weight,
        p_value = p,
        species_params = species_params,
        processed_data = processed_data,
        model_options = model_options,
        oxycal = oxycal,
        population_size = population_size,
        output_type = "final_only"
      )
    }, error = function(e) {
      # Si hay error (ej. peso negativo), retornar valor extremo
      if (fit_type == "Weight") {
        return(list(final_weight = -999))
      } else {
        return(list(total_consumption = -999))
      }
    })

    # Obtener valor actual según tipo de ajuste
    if (fit_type == "Weight") {
      current_value <- result$final_weight
    } else {
      current_value <- result$total_consumption
    }

    # Verificar si hubo error en la simulación
    if (current_value < 0) {
      # Peso o consumo negativo indica error
      if (abs(current_value - target_value) <= tolerance) {
        # Caso raro donde el error da el valor objetivo
        fit_successful <- TRUE
        break
      } else {
        # Ajustar límites para evitar esta región
        if (p > 0.5) {
          p_max <- p
        } else {
          p_min <- p
        }
      }
    } else {
      # Verificar convergencia
      if (abs(current_value - target_value) <= tolerance) {
        fit_successful <- TRUE
        break
      }
    }

    # Verificar límite de iteraciones
    if (n_iter >= max_iterations) {
      break
    }

    # Ajustar límites de búsqueda
    if (current_value > target_value) {
      p_max <- p
    } else {
      p_min <- p
    }

    # Calcular nuevo p-value (punto medio)
    p_new <- (p_min + p_max) / 2

    # Verificar si hemos alcanzado límites de precisión
    if (abs(p_new - p) < 1e-10) {
      break
    }

    p <- p_new
  }

  # Verificar si p está en límites extremos
  if (p < 0.0000002) {
    warning("p-value en límite inferior. Posible ajuste inadecuado.")
  } else if (p >= 4.9999998) {
    warning("p-value en límite superior. Posible ajuste inadecuado.")
  }

  if (!is.null(progress_callback)) {
    if (fit_successful) {
      progress_callback(1, paste("Ajuste exitoso. p =", round(p, 6)))
    } else {
      progress_callback(1, "Ajuste no convergió dentro de la tolerancia")
    }
  }

  return(list(
    p_value = p,
    fit_successful = fit_successful,
    n_iterations = n_iter,
    final_error = if(exists("current_value")) abs(current_value - target_value) else NA,
    target_value = target_value,
    achieved_value = if(exists("current_value")) current_value else NA
  ))
}

#' Ejecutar modelo FB4 con ajuste de p-value
#'
#' @param species_params Parámetros de la especie
#' @param initial_weight Peso inicial
#' @param fit_to Tipo de ajuste
#' @param fit_value Valor objetivo
#' @param processed_data Datos procesados
#' @param model_options Opciones del modelo
#' @param oxycal Coeficiente oxicalórico
#' @param population_size Tamaño de población
#' @param output_daily Incluir salida diaria
#' @param progress_callback Función callback para progreso
#' @return Resultado de la simulación
#' @keywords internal
run_fb4_with_fitting <- function(species_params,
                                 initial_weight,
                                 fit_to,
                                 fit_value,
                                 processed_data,
                                 model_options,
                                 oxycal,
                                 population_size,
                                 output_daily,
                                 progress_callback) {

  # Ejecutar ajuste de p-value
  fit_result <- fit_p_value(
    p_initial = 0.5,
    initial_weight = initial_weight,
    target_value = fit_value,
    tolerance = 0.0001,
    max_iterations = 25,
    fit_type = fit_to,
    species_params = species_params,
    processed_data = processed_data,
    model_options = model_options,
    oxycal = oxycal,
    population_size = population_size,
    progress_callback = progress_callback
  )

  # Ejecutar simulación final con p-value ajustado
  if (fit_result$fit_successful) {
    final_result <- run_growth_model_internal(
      initial_weight = initial_weight,
      p_value = fit_result$p_value,
      species_params = species_params,
      processed_data = processed_data,
      model_options = model_options,
      oxycal = oxycal,
      population_size = population_size,
      output_type = if(output_daily) "full" else "summary_only"
    )
  } else {
    # Si el ajuste falló, crear resultado mínimo
    final_result <- create_failed_result(initial_weight, processed_data$n_days)
  }

  # Agregar información del ajuste
  final_result$fit_successful <- fit_result$fit_successful
  final_result$p_value <- fit_result$p_value
  final_result$fit_iterations <- fit_result$n_iterations
  final_result$fit_error <- fit_result$final_error

  return(final_result)
}

#' Ejecutar modelo FB4 sin ajuste (p-value directo)
#'
#' @param species_params Parámetros de la especie
#' @param initial_weight Peso inicial
#' @param fit_to Tipo de ejecución
#' @param fit_value Valor de p o ración
#' @param processed_data Datos procesados
#' @param model_options Opciones del modelo
#' @param oxycal Coeficiente oxicalórico
#' @param population_size Tamaño de población
#' @param output_daily Incluir salida diaria
#' @param progress_callback Función callback para progreso
#' @return Resultado de la simulación
#' @keywords internal
run_fb4_direct <- function(species_params,
                           initial_weight,
                           fit_to,
                           fit_value,
                           processed_data,
                           model_options,
                           oxycal,
                           population_size,
                           output_daily,
                           progress_callback) {

  # Determinar p-value o método según fit_to
  if (fit_to == "p-value") {
    p_value <- fit_value
    calculation_method <- "p_value"
  } else if (fit_to == "Ration") {
    # Ración como % del peso corporal
    ration_percent <- fit_value / 100
    p_value <- NULL
    calculation_method <- "ration_percent"
  } else if (fit_to == "Ration_prey") {
    # Ración como gramos de presa por día
    ration_grams <- fit_value
    p_value <- NULL
    calculation_method <- "ration_grams"
  } else {
    stop("fit_to no válido para ejecución directa: ", fit_to)
  }

  # Ejecutar modelo
  result <- run_growth_model_internal(
    initial_weight = initial_weight,
    p_value = p_value,
    ration_percent = if(exists("ration_percent")) ration_percent else NULL,
    ration_grams = if(exists("ration_grams")) ration_grams else NULL,
    calculation_method = calculation_method,
    species_params = species_params,
    processed_data = processed_data,
    model_options = model_options,
    oxycal = oxycal,
    population_size = population_size,
    output_type = if(output_daily) "full" else "summary_only",
    progress_callback = progress_callback
  )

  # Agregar información de ejecución
  result$fit_successful <- TRUE  # Siempre exitoso para ejecución directa
  result$p_value <- if(calculation_method == "p_value") p_value else NA
  result$calculation_method <- calculation_method

  return(result)
}

#' Crear resultado fallido mínimo
#'
#' @param initial_weight Peso inicial
#' @param n_days Número de días
#' @return Resultado mínimo para casos de falla
#' @keywords internal
create_failed_result <- function(initial_weight, n_days) {

  # Crear data frame mínimo con una fila
  daily_output <- data.frame(
    Day = 1,
    Temperature.C = NA,
    Weight.g = initial_weight,
    Consumption.g = 0,
    stringsAsFactors = FALSE
  )

  # Agregar columnas adicionales con valores por defecto
  additional_cols <- c(
    "Population.Number", "Population.Biomass.g",
    "Specific.Growth.Rate.J.g.d", "Specific.Consumption.Rate.J.g.d",
    "Specific.Egestion.Rate.J.g.d", "Specific.Excretion.Rate.J.g.d",
    "Specific.Respiration.Rate.J.g.d", "Specific.SDA.Rate.J.g.d",
    "Net.Production.g", "Gross.Production.g"
  )

  for (col in additional_cols) {
    daily_output[[col]] <- 0
  }

  summary <- data.frame(
    Parameter = c("p-value", "Total consumption (g)", "Final weight (g)", "Error"),
    Value = c("FAILED", "0", as.character(initial_weight), "Simulation failed"),
    stringsAsFactors = FALSE
  )

  energy_balance <- list(
    balanced = FALSE,
    relative_error = NA,
    message = "Energy balance not calculated due to simulation failure"
  )

  return(list(
    daily_output = daily_output,
    summary = summary,
    energy_balance = energy_balance,
    final_weight = initial_weight,
    total_consumption = 0
  ))
}

#' Optimizar parámetros del modelo usando múltiples objetivos
#'
#' Permite ajustar múltiples parámetros del modelo para cumplir varios objetivos
#'
#' @param objectives Lista de objetivos (ej: list(final_weight = 50, total_consumption = 100))
#' @param parameters_to_fit Vector con nombres de parámetros a ajustar
#' @param initial_guess Valores iniciales para parámetros
#' @param species_params Parámetros de la especie
#' @param processed_data Datos procesados
#' @param model_options Opciones del modelo
#' @param method Método de optimización ("Nelder-Mead", "BFGS", etc.)
#' @param control Lista de control para optimización
#'
#' @return Resultado de la optimización
#' @export
optimize_fb4_parameters <- function(objectives,
                                    parameters_to_fit,
                                    initial_guess,
                                    species_params,
                                    processed_data,
                                    model_options,
                                    method = "Nelder-Mead",
                                    control = list(maxit = 100)) {

  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Paquete 'stats' requerido para optimización")
  }

  # Validar entradas
  if (length(parameters_to_fit) != length(initial_guess)) {
    stop("parameters_to_fit y initial_guess deben tener la misma longitud")
  }

  # Función objetivo para minimizar
  objective_function <- function(params) {
    # Actualizar parámetros de la especie
    updated_params <- species_params
    for (i in seq_along(parameters_to_fit)) {
      param_name <- parameters_to_fit[i]
      param_value <- params[i]

      # Actualizar parámetro en la estructura correspondiente
      if (param_name %in% names(updated_params$consumption)) {
        updated_params$consumption[[param_name]] <- param_value
      } else if (param_name %in% names(updated_params$respiration)) {
        updated_params$respiration[[param_name]] <- param_value
      } else {
        stop("Parámetro no reconocido: ", param_name)
      }
    }

    # Ejecutar modelo
    tryCatch({
      result <- run_growth_model_internal(
        initial_weight = objectives$initial_weight %||% 10,
        p_value = 0.5,
        species_params = updated_params,
        processed_data = processed_data,
        model_options = model_options,
        output_type = "summary_only"
      )

      # Calcular error total
      total_error <- 0

      if ("final_weight" %in% names(objectives)) {
        weight_error <- (result$final_weight - objectives$final_weight)^2
        total_error <- total_error + weight_error
      }

      if ("total_consumption" %in% names(objectives)) {
        cons_error <- (result$total_consumption - objectives$total_consumption)^2
        total_error <- total_error + cons_error
      }

      return(sqrt(total_error))

    }, error = function(e) {
      return(1e6)  # Error muy grande si la simulación falla
    })
  }

  # Ejecutar optimización
  opt_result <- stats::optim(
    par = initial_guess,
    fn = objective_function,
    method = method,
    control = control
  )

  # Preparar resultado
  optimized_params <- opt_result$par
  names(optimized_params) <- parameters_to_fit

  # Ejecutar simulación final con parámetros optimizados
  final_params <- species_params
  for (i in seq_along(parameters_to_fit)) {
    param_name <- parameters_to_fit[i]
    param_value <- optimized_params[i]

    if (param_name %in% names(final_params$consumption)) {
      final_params$consumption[[param_name]] <- param_value
    } else if (param_name %in% names(final_params$respiration)) {
      final_params$respiration[[param_name]] <- param_value
    }
  }

  final_simulation <- run_growth_model_internal(
    initial_weight = objectives$initial_weight %||% 10,
    p_value = 0.5,
    species_params = final_params,
    processed_data = processed_data,
    model_options = model_options,
    output_type = "full"
  )

  return(list(
    optimized_parameters = optimized_params,
    optimization_result = opt_result,
    final_simulation = final_simulation,
    convergence = opt_result$convergence == 0,
    final_error = opt_result$value,
    n_evaluations = opt_result$counts
  ))
}

#' Análisis de sensibilidad para parámetros del modelo
#'
#' Evalúa cómo cambios en parámetros afectan los resultados del modelo
#'
#' @param parameter_name Nombre del parámetro a analizar
#' @param parameter_range Vector con rango de valores a probar
#' @param baseline_params Parámetros base de la especie
#' @param processed_data Datos procesados
#' @param model_options Opciones del modelo
#' @param output_metrics Vector con métricas a evaluar
#'
#' @return Data frame con resultados del análisis de sensibilidad
#' @export
sensitivity_analysis <- function(parameter_name,
                                 parameter_range,
                                 baseline_params,
                                 processed_data,
                                 model_options,
                                 output_metrics = c("final_weight", "total_consumption")) {

  results <- data.frame(
    parameter_value = parameter_range,
    stringsAsFactors = FALSE
  )

  # Inicializar columnas para métricas
  for (metric in output_metrics) {
    results[[metric]] <- NA
  }

  # Ejecutar simulaciones para cada valor del parámetro
  for (i in seq_along(parameter_range)) {
    param_value <- parameter_range[i]

    # Crear copia de parámetros y modificar el parámetro objetivo
    modified_params <- baseline_params

    # Determinar dónde está el parámetro
    if (parameter_name %in% names(modified_params$consumption)) {
      modified_params$consumption[[parameter_name]] <- param_value
    } else if (parameter_name %in% names(modified_params$respiration)) {
      modified_params$respiration[[parameter_name]] <- param_value
    } else if (parameter_name %in% names(modified_params$egestion)) {
      modified_params$egestion[[parameter_name]] <- param_value
    } else if (parameter_name %in% names(modified_params$excretion)) {
      modified_params$excretion[[parameter_name]] <- param_value
    } else {
      stop("Parámetro no encontrado: ", parameter_name)
    }

    # Ejecutar simulación
    tryCatch({
      sim_result <- run_growth_model_internal(
        initial_weight = 10,  # Peso estándar para comparación
        p_value = 0.5,
        species_params = modified_params,
        processed_data = processed_data,
        model_options = model_options,
        output_type = "summary_only"
      )

      # Extraer métricas
      if ("final_weight" %in% output_metrics) {
        results$final_weight[i] <- sim_result$final_weight
      }
      if ("total_consumption" %in% output_metrics) {
        results$total_consumption[i] <- sim_result$total_consumption
      }

    }, error = function(e) {
      # Dejar NA si hay error
      warning("Error en simulación para ", parameter_name, " = ", param_value, ": ", e$message)
    })
  }

  # Calcular sensibilidades relativas
  baseline_idx <- which.min(abs(parameter_range - baseline_params[[parameter_name]]))
  if (length(baseline_idx) == 0) {
    warning("Valor baseline no encontrado en el rango")
    baseline_idx <- ceiling(length(parameter_range) / 2)
  }

  for (metric in output_metrics) {
    if (metric %in% names(results)) {
      baseline_value <- results[[metric]][baseline_idx]
      if (!is.na(baseline_value) && baseline_value != 0) {
        sensitivity_col <- paste0(metric, "_sensitivity")
        results[[sensitivity_col]] <- (results[[metric]] - baseline_value) / baseline_value * 100
      }
    }
  }

  return(results)
}

#' Operador %||% para valores por defecto
#'
#' @param x Valor a probar
#' @param y Valor por defecto si x es NULL
#' @return x si no es NULL, y en caso contrario
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
