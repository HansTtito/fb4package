#' Métodos de Visualización y Análisis para FB4
#'
#' @name visualization-analysis
#' @aliases visualization-analysis
NULL

#' Método plot para objetos fb4_result
#'
#' @param x Objeto fb4_result
#' @param type Tipo de gráfico ("growth", "consumption", "energy", "population")
#' @param ... Argumentos adicionales para plot
#' @return NULL (genera gráfico)
#' @export
#' @method plot fb4_result
plot.fb4_result <- function(x, type = "growth", ...) {

  if (is.null(x$daily_output)) {
    stop("Objeto fb4_result debe incluir daily_output para graficar")
  }

  daily_data <- x$daily_output

  if (type == "growth") {
    plot_growth_trajectory(daily_data, ...)
  } else if (type == "consumption") {
    plot_consumption_rates(daily_data, ...)
  } else if (type == "energy") {
    plot_energy_budget(daily_data, ...)
  } else if (type == "population") {
    plot_population_dynamics(daily_data, ...)
  } else {
    stop("Tipo de gráfico no reconocido: ", type)
  }
}

#' Método summary para objetos fb4_result
#'
#' @param object Objeto fb4_result
#' @param ... Argumentos adicionales
#' @return Resumen del objeto
#' @export
#' @method summary fb4_result
summary.fb4_result <- function(object, ...) {

  cat("=== Resumen del Modelo FB4 ===\n\n")

  # Información básica
  cat("Especie:", object$parameters_used$species, "\n")
  cat("Duración:", nrow(object$daily_output), "días\n")
  cat("Peso inicial:", object$parameters_used$initial_conditions$initial_weight, "g\n")
  cat("Peso final:", round(object$model_info$final_weight, 2), "g\n")
  cat("Crecimiento total:", round(object$model_info$final_weight -
                                    object$parameters_used$initial_conditions$initial_weight, 2), "g\n")

  # Información del ajuste
  if (object$model_info$fit_successful) {
    cat("Ajuste exitoso con p-value =", round(object$model_info$p_value, 4), "\n")
  } else {
    cat("Ajuste no exitoso\n")
  }

  # Balance energético
  cat("\nBalance energético:", object$energy_balance$message, "\n")
  if (!object$energy_balance$balanced) {
    cat("Error relativo:", round(object$energy_balance$relative_error * 100, 4), "%\n")
  }

  # Tiempo de ejecución
  cat("Tiempo de ejecución:", round(object$model_info$execution_time, 3), "segundos\n")

  cat("\n=== Resumen de Parámetros ===\n")
  print(object$summary)

  invisible(object)
}

#' Graficar trayectoria de crecimiento
#'
#' @param daily_data Datos diarios
#' @param main Título del gráfico
#' @param xlab Etiqueta del eje X
#' @param ylab Etiqueta del eje Y
#' @param ... Argumentos adicionales para plot
#' @return NULL
#' @keywords internal
plot_growth_trajectory <- function(daily_data, main = "Trayectoria de Crecimiento",
                                   xlab = "Día", ylab = "Peso (g)", ...) {

  plot(daily_data$Day, daily_data$Weight.g, type = "l", lwd = 2, col = "blue",
       main = main, xlab = xlab, ylab = ylab, ...)

  grid()

  # Agregar información final
  final_day <- max(daily_data$Day, na.rm = TRUE)
  final_weight <- daily_data$Weight.g[daily_data$Day == final_day]

  text(final_day * 0.7, final_weight * 0.9,
       paste("Peso final:", round(final_weight, 2), "g"),
       col = "blue", cex = 0.9)
}

#' Graficar tasas de consumo
#'
#' @param daily_data Datos diarios
#' @param main Título del gráfico
#' @param ... Argumentos adicionales
#' @return NULL
#' @keywords internal
plot_consumption_rates <- function(daily_data, main = "Tasas de Consumo", ...) {

  par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))

  # Consumo específico en g/g/d
  plot(daily_data$Day, daily_data$Specific.Consumption.Rate.g.g.d,
       type = "l", lwd = 2, col = "red",
       main = paste(main, "- Consumo Específico"),
       xlab = "Día", ylab = "Consumo (g/g/d)")
  grid()

  # Consumo total diario
  plot(daily_data$Day, daily_data$Consumption.g,
       type = "l", lwd = 2, col = "darkgreen",
       main = paste(main, "- Consumo Total Diario"),
       xlab = "Día", ylab = "Consumo (g/d)")
  grid()

  par(mfrow = c(1, 1))
}

#' Graficar presupuesto energético
#'
#' @param daily_data Datos diarios
#' @param main Título del gráfico
#' @param ... Argumentos adicionales
#' @return NULL
#' @keywords internal
plot_energy_budget <- function(daily_data, main = "Presupuesto Energético", ...) {

  # Preparar datos
  consumption <- daily_data$Specific.Consumption.Rate.J.g.d
  respiration <- daily_data$Specific.Respiration.Rate.J.g.d
  egestion <- daily_data$Specific.Egestion.Rate.J.g.d
  excretion <- daily_data$Specific.Excretion.Rate.J.g.d
  sda <- daily_data$Specific.SDA.Rate.J.g.d
  growth <- daily_data$Specific.Growth.Rate.J.g.d

  # Gráfico de barras apiladas para mostrar destino de la energía
  energy_matrix <- rbind(growth, respiration, egestion, excretion, sda)
  rownames(energy_matrix) <- c("Crecimiento", "Respiración", "Egestion", "Excreción", "SDA")

  # Seleccionar algunos puntos para mostrar (cada 10 días)
  sample_days <- seq(1, length(daily_data$Day), by = max(1, length(daily_data$Day) %/% 20))

  barplot(energy_matrix[, sample_days],
          names.arg = daily_data$Day[sample_days],
          col = c("green", "red", "orange", "yellow", "purple"),
          main = main,
          xlab = "Día", ylab = "Energía (J/g/d)",
          legend.text = TRUE,
          args.legend = list(x = "topright", cex = 0.8))
}

#' Graficar dinámicas poblacionales
#'
#' @param daily_data Datos diarios
#' @param main Título del gráfico
#' @param ... Argumentos adicionales
#' @return NULL
#' @keywords internal
plot_population_dynamics <- function(daily_data, main = "Dinámicas Poblacionales", ...) {

  if (!"Population.Number" %in% names(daily_data)) {
    stop("Datos de población no disponibles")
  }

  par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))

  # Número de individuos
  plot(daily_data$Day, daily_data$Population.Number,
       type = "l", lwd = 2, col = "blue",
       main = paste(main, "- Número de Individuos"),
       xlab = "Día", ylab = "Número")
  grid()

  # Biomasa poblacional
  plot(daily_data$Day, daily_data$Population.Biomass.g,
       type = "l", lwd = 2, col = "darkgreen",
       main = paste(main, "- Biomasa Poblacional"),
       xlab = "Día", ylab = "Biomasa (g)")
  grid()

  par(mfrow = c(1, 1))
}

#' Crear dashboard de resultados
#'
#' @param fb4_result Objeto fb4_result
#' @param save_path Ruta para guardar el gráfico (opcional)
#' @return NULL
#' @export
create_fb4_dashboard <- function(fb4_result, save_path = NULL) {

  if (!is.null(save_path)) {
    png(save_path, width = 1200, height = 800, res = 100)
  }

  par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))

  daily_data <- fb4_result$daily_output

  # 1. Crecimiento
  plot(daily_data$Day, daily_data$Weight.g, type = "l", lwd = 2, col = "blue",
       main = "Crecimiento", xlab = "Día", ylab = "Peso (g)")
  grid()

  # 2. Temperatura
  plot(daily_data$Day, daily_data$Temperature.C, type = "l", lwd = 2, col = "red",
       main = "Temperatura", xlab = "Día", ylab = "Temperatura (°C)")
  grid()

  # 3. Consumo específico
  plot(daily_data$Day, daily_data$Specific.Consumption.Rate.g.g.d,
       type = "l", lwd = 2, col = "darkgreen",
       main = "Tasa de Consumo", xlab = "Día", ylab = "Consumo (g/g/d)")
  grid()

  # 4. Tasa de crecimiento específico
  plot(daily_data$Day, daily_data$Specific.Growth.Rate.g.g.d,
       type = "l", lwd = 2, col = "purple",
       main = "Tasa de Crecimiento", xlab = "Día", ylab = "Crecimiento (g/g/d)")
  grid()

  # 5. Consumo acumulativo
  plot(daily_data$Day, daily_data$Cum.Cons.g, type = "l", lwd = 2, col = "orange",
       main = "Consumo Acumulativo", xlab = "Día", ylab = "Consumo (g)")
  grid()

  # 6. Densidad energética del depredador
  plot(daily_data$Day, daily_data$Final.Predator.Energy.Density.J.g,
       type = "l", lwd = 2, col = "brown",
       main = "Densidad Energética", xlab = "Día", ylab = "Energía (J/g)")
  grid()

  if (!is.null(save_path)) {
    dev.off()
    message("Dashboard guardado en: ", save_path)
  }
}

#' Comparar múltiples simulaciones
#'
#' @param results_list Lista de objetos fb4_result
#' @param labels Vector con etiquetas para cada simulación
#' @param metric Métrica a comparar ("weight", "consumption", "growth_rate")
#' @param main Título del gráfico
#' @return NULL
#' @export
compare_fb4_simulations <- function(results_list, labels = NULL,
                                    metric = "weight", main = "Comparación de Simulaciones") {

  if (is.null(labels)) {
    labels <- paste("Simulación", 1:length(results_list))
  }

  if (length(labels) != length(results_list)) {
    stop("Número de etiquetas debe coincidir con número de resultados")
  }

  # Configurar colores
  colors <- rainbow(length(results_list))

  # Determinar qué variable graficar
  if (metric == "weight") {
    var_name <- "Weight.g"
    ylab <- "Peso (g)"
  } else if (metric == "consumption") {
    var_name <- "Cum.Cons.g"
    ylab <- "Consumo Acumulativo (g)"
  } else if (metric == "growth_rate") {
    var_name <- "Specific.Growth.Rate.g.g.d"
    ylab <- "Tasa de Crecimiento (g/g/d)"
  } else {
    stop("Métrica no reconocida: ", metric)
  }

  # Preparar gráfico
  plot_initialized <- FALSE

  for (i in seq_along(results_list)) {
    result <- results_list[[i]]

    if (is.null(result$daily_output)) {
      warning("Resultado ", i, " no tiene daily_output, omitiendo")
      next
    }

    daily_data <- result$daily_output

    if (!plot_initialized) {
      plot(daily_data$Day, daily_data[[var_name]],
           type = "l", lwd = 2, col = colors[i],
           main = main, xlab = "Día", ylab = ylab,
           ylim = range(sapply(results_list, function(r) {
             if (!is.null(r$daily_output)) range(r$daily_output[[var_name]], na.rm = TRUE) else c(0, 1)
           }), na.rm = TRUE))
      plot_initialized <- TRUE
    } else {
      lines(daily_data$Day, daily_data[[var_name]],
            lwd = 2, col = colors[i])
    }
  }

  # Agregar leyenda
  legend("topleft", legend = labels, col = colors, lwd = 2, cex = 0.8)
  grid()
}

#' Análisis estadístico de resultados FB4
#'
#' @param fb4_result Objeto fb4_result
#' @return Lista con estadísticas descriptivas
#' @export
analyze_fb4_statistics <- function(fb4_result) {

  if (is.null(fb4_result$daily_output)) {
    stop("Objeto fb4_result debe incluir daily_output para análisis")
  }

  daily_data <- fb4_result$daily_output

  # Estadísticas de crecimiento
  growth_stats <- list(
    initial_weight = daily_data$Weight.g[1],
    final_weight = tail(daily_data$Weight.g, 1),
    total_growth = tail(daily_data$Weight.g, 1) - daily_data$Weight.g[1],
    mean_growth_rate = mean(daily_data$Specific.Growth.Rate.g.g.d, na.rm = TRUE),
    max_growth_rate = max(daily_data$Specific.Growth.Rate.g.g.d, na.rm = TRUE),
    min_growth_rate = min(daily_data$Specific.Growth.Rate.g.g.d, na.rm = TRUE)
  )

  # Estadísticas de consumo
  consumption_stats <- list(
    total_consumption = tail(daily_data$Cum.Cons.g, 1),
    mean_daily_consumption = mean(daily_data$Consumption.g, na.rm = TRUE),
    mean_consumption_rate = mean(daily_data$Specific.Consumption.Rate.g.g.d, na.rm = TRUE),
    max_consumption_rate = max(daily_data$Specific.Consumption.Rate.g.g.d, na.rm = TRUE),
    min_consumption_rate = min(daily_data$Specific.Consumption.Rate.g.g.d, na.rm = TRUE)
  )

  # Estadísticas energéticas
  energy_stats <- list(
    mean_predator_energy = mean(daily_data$Final.Predator.Energy.Density.J.g, na.rm = TRUE),
    mean_prey_energy = mean(daily_data$Mean.Prey.Energy.Density.J.g, na.rm = TRUE),
    total_net_production = tail(daily_data$Cum.Net.Production.J, 1),
    total_gross_production = tail(daily_data$Cum.Gross.Production.J, 1)
  )

  # Eficiencias
  efficiency_stats <- list()

  if (all(c("Consumption.J", "Net.Production.J") %in% names(daily_data))) {
    total_consumption_j <- tail(daily_data$Cum.Cons.J, 1)
    total_net_production_j <- tail(daily_data$Cum.Net.Production.J, 1)

    efficiency_stats$growth_efficiency <- total_net_production_j / total_consumption_j
    efficiency_stats$food_conversion_efficiency <- growth_stats$total_growth / consumption_stats$total_consumption
  }

  # Estadísticas de temperatura
  temperature_stats <- list(
    mean_temperature = mean(daily_data$Temperature.C, na.rm = TRUE),
    max_temperature = max(daily_data$Temperature.C, na.rm = TRUE),
    min_temperature = min(daily_data$Temperature.C, na.rm = TRUE),
    temperature_range = max(daily_data$Temperature.C, na.rm = TRUE) - min(daily_data$Temperature.C, na.rm = TRUE)
  )

  return(list(
    growth = growth_stats,
    consumption = consumption_stats,
    energy = energy_stats,
    efficiency = efficiency_stats,
    temperature = temperature_stats,
    simulation_days = nrow(daily_data),
    energy_balance = fb4_result$energy_balance
  ))
}

#' Exportar resultados a CSV
#'
#' @param fb4_result Objeto fb4_result
#' @param file_path Ruta del archivo CSV
#' @param include_summary Incluir resumen en archivo separado
#' @return NULL
#' @export
export_fb4_results <- function(fb4_result, file_path, include_summary = TRUE) {

  # Exportar datos diarios
  if (!is.null(fb4_result$daily_output)) {
    write.csv(fb4_result$daily_output, file_path, row.names = FALSE)
    message("Datos diarios exportados a: ", file_path)
  }

  # Exportar resumen si se solicita
  if (include_summary && !is.null(fb4_result$summary)) {
    summary_path <- sub("\\.csv$", "_summary.csv", file_path)
    write.csv(fb4_result$summary, summary_path, row.names = FALSE)
    message("Resumen exportado a: ", summary_path)
  }

  # Exportar estadísticas
  stats <- analyze_fb4_statistics(fb4_result)
  stats_path <- sub("\\.csv$", "_statistics.csv", file_path)

  # Convertir estadísticas a data frame
  stats_df <- data.frame(
    Category = character(),
    Parameter = character(),
    Value = numeric(),
    stringsAsFactors = FALSE
  )

  for (category in names(stats)) {
    if (is.list(stats[[category]])) {
      for (param in names(stats[[category]])) {
        stats_df <- rbind(stats_df, data.frame(
          Category = category,
          Parameter = param,
          Value = stats[[category]][[param]],
          stringsAsFactors = FALSE
        ))
      }
    } else {
      stats_df <- rbind(stats_df, data.frame(
        Category = "general",
        Parameter = category,
        Value = stats[[category]],
        stringsAsFactors = FALSE
      ))
    }
  }

  write.csv(stats_df, stats_path, row.names = FALSE)
  message("Estadísticas exportadas a: ", stats_path)
}

