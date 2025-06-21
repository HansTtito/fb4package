#' Clases S3 para el Modelo Bioenergético FB4
#'
#' @name bioenergetic-classes
#' @aliases bioenergetic-classes
NULL

# ============================================================================
# CLASE PRINCIPAL: Bioenergetic
# ============================================================================

#' Constructor para objetos Bioenergetic
#'
#' Crea un objeto de clase Bioenergetic que encapsula todos los componentes
#' del modelo bioenergético de peces.
#'
#' @param species_params Lista con parámetros de la especie
#' @param environmental_data Lista con datos ambientales (temperatura, etc.)
#' @param diet_data Lista con datos de dieta y energía de presas
#' @param model_options Lista con opciones del modelo
#' @param simulation_settings Lista con configuración de la simulación
#' @return Objeto de clase "Bioenergetic"
#' @export
#' @examples
#' \dontrun{
#' # Crear modelo bioenergético
#' modelo <- Bioenergetic$new(
#'   species = "Bluegill",
#'   initial_weight = 10,
#'   duration = 365
#' )
#' }
Bioenergetic <- function(species_params = NULL,
                         environmental_data = NULL,
                         diet_data = NULL,
                         model_options = list(),
                         simulation_settings = list()) {

  # Validar entradas básicas
  if (is.null(species_params)) {
    stop("species_params es requerido")
  }

  # Crear objeto
  obj <- list(
    species_params = species_params,
    environmental_data = environmental_data,
    diet_data = diet_data,
    model_options = model_options,
    simulation_settings = simulation_settings,
    results = NULL,
    fitted = FALSE,
    validated = FALSE
  )

  # Asignar clases
  class(obj) <- c("Bioenergetic", "list")

  # Validar el objeto
  validate_bioenergetic(obj)

  return(obj)
}

#' Constructor simplificado para modelos bioenergéticos
#'
#' @param species Nombre o número de especie
#' @param initial_weight Peso inicial (g)
#' @param duration Duración en días
#' @param temperature_avg Temperatura promedio (°C)
#' @param temperature_variation Variación de temperatura (°C)
#' @param diet_composition Lista con composición de dieta
#' @param ... Argumentos adicionales
#' @return Objeto Bioenergetic
#' @export
new_bioenergetic <- function(species,
                             initial_weight,
                             duration = 365,
                             temperature_avg = 20,
                             temperature_variation = 5,
                             diet_composition = list(Zooplankton = 1.0),
                             ...) {

  # Cargar parámetros de especies
  data("parameters_official", package = "FB4", envir = environment())

  # Determinar parámetros de especies
  if (is.character(species)) {
    species_num <- which(parameters_official$Species == species)
    if (length(species_num) == 0) {
      stop("Especie no encontrada: ", species)
    }
  } else {
    species_num <- species
  }

  species_params <- extract_species_parameters(parameters_official, species_num)

  # Crear datos ambientales sintéticos
  days <- 1:duration
  temperature <- temperature_avg + temperature_variation * sin(2 * pi * days / 365)
  environmental_data <- list(
    temperature = data.frame(Day = days, Temperature = temperature),
    days = days,
    duration = duration
  )

  # Crear datos de dieta sintéticos
  diet_names <- names(diet_composition)
  diet_props <- as.numeric(diet_composition)

  diet_df <- data.frame(Day = days)
  energy_df <- data.frame(Day = days)

  # Energías típicas por tipo de presa
  typical_energies <- list(
    Zooplankton = 3500, Invertebrates = 4200, Fish = 5500,
    Detritus = 2000, Benthos = 4000, Insects = 4500
  )

  for (i in seq_along(diet_names)) {
    diet_df[[diet_names[i]]] <- diet_props[i]
    energy <- typical_energies[[diet_names[i]]] %||% 3500
    energy_df[[diet_names[i]]] <- energy
  }

  diet_data <- list(
    proportions = diet_df,
    energies = energy_df,
    prey_names = diet_names
  )

  # Configuración de simulación
  simulation_settings <- list(
    initial_weight = initial_weight,
    duration = duration,
    fit_to = "Weight",
    fit_value = NA,
    oxycal = 13560,
    population_size = 1
  )

  # Opciones del modelo
  model_options <- list(
    calc_mortality = FALSE,
    calc_reproduction = FALSE,
    calc_contaminant = FALSE,
    calc_nutrient = FALSE,
    output_daily = TRUE
  )

  # Crear objeto
  Bioenergetic(
    species_params = species_params,
    environmental_data = environmental_data,
    diet_data = diet_data,
    model_options = model_options,
    simulation_settings = simulation_settings
  )
}

# ============================================================================
# MÉTODOS PARA LA CLASE Bioenergetic
# ============================================================================

#' Validar objeto Bioenergetic
#'
#' @param x Objeto Bioenergetic
#' @return TRUE si es válido, error si no
#' @keywords internal
validate_bioenergetic <- function(x) {

  if (!inherits(x, "Bioenergetic")) {
    stop("Objeto debe ser de clase 'Bioenergetic'")
  }

  # Validar componentes requeridos
  required_components <- c("species_params", "environmental_data", "diet_data",
                           "model_options", "simulation_settings")

  missing <- setdiff(required_components, names(x))
  if (length(missing) > 0) {
    stop("Componentes faltantes: ", paste(missing, collapse = ", "))
  }

  # Validar species_params
  if (!is.list(x$species_params)) {
    stop("species_params debe ser una lista")
  }

  # Validar environmental_data
  if (!is.null(x$environmental_data$temperature)) {
    if (!"Temperature" %in% names(x$environmental_data$temperature)) {
      stop("environmental_data$temperature debe tener columna 'Temperature'")
    }
  }

  return(TRUE)
}

#' Método print para objetos Bioenergetic
#'
#' @param x Objeto Bioenergetic
#' @param ... Argumentos adicionales
#' @export
print.Bioenergetic <- function(x, ...) {
  cat("=== Modelo Bioenergético FB4 ===\n\n")

  # Información de la especie
  if (!is.null(x$species_params$species_info)) {
    cat("Especie:", x$species_params$species_info$name, "\n")
    cat("Nombre científico:", x$species_params$species_info$scientific_name, "\n")
  }

  # Configuración de simulación
  if (!is.null(x$simulation_settings)) {
    cat("Peso inicial:", x$simulation_settings$initial_weight, "g\n")
    cat("Duración:", x$simulation_settings$duration %||% "No especificada", "días\n")
  }

  # Estado del modelo
  cat("Estado: ")
  if (x$fitted) {
    cat("Ajustado")
    if (!is.null(x$results)) {
      cat(" | Resultados disponibles")
    }
  } else {
    cat("No ajustado")
  }
  cat("\n")

  # Opciones habilitadas
  enabled_options <- names(x$model_options)[sapply(x$model_options, isTRUE)]
  if (length(enabled_options) > 0) {
    cat("Sub-modelos habilitados:", paste(enabled_options, collapse = ", "), "\n")
  }

  cat("\n")
  invisible(x)
}

#' Método summary para objetos Bioenergetic
#'
#' @param object Objeto Bioenergetic
#' @param ... Argumentos adicionales
#' @export
summary.Bioenergetic <- function(object, ...) {

  cat("=== Resumen del Modelo Bioenergético ===\n\n")

  # Información básica
  print(object)

  # Parámetros de especies (resumen)
  cat("=== Parámetros de Especies ===\n")
  if (!is.null(object$species_params$consumption)) {
    cat("Ecuación de consumo:", object$species_params$consumption$CEQ, "\n")
  }
  if (!is.null(object$species_params$respiration)) {
    cat("Ecuación de respiración:", object$species_params$respiration$REQ, "\n")
  }

  # Datos ambientales
  cat("\n=== Datos Ambientales ===\n")
  if (!is.null(object$environmental_data$temperature)) {
    temp_data <- object$environmental_data$temperature$Temperature
    cat("Temperatura - Min:", round(min(temp_data, na.rm = TRUE), 1), "°C")
    cat(" | Max:", round(max(temp_data, na.rm = TRUE), 1), "°C")
    cat(" | Media:", round(mean(temp_data, na.rm = TRUE), 1), "°C\n")
  }

  # Datos de dieta
  cat("\n=== Composición de Dieta ===\n")
  if (!is.null(object$diet_data$prey_names)) {
    cat("Tipos de presa:", paste(object$diet_data$prey_names, collapse = ", "), "\n")
  }

  # Resultados si están disponibles
  if (!is.null(object$results)) {
    cat("\n=== Resultados ===\n")
    if (!is.null(object$results$summary)) {
      print(object$results$summary)
    }
  }

  invisible(object)
}

# ============================================================================
# MÉTODOS DE CONFIGURACIÓN
# ============================================================================

#' Configurar datos ambientales
#'
#' @param x Objeto Bioenergetic
#' @param temperature_data Data frame con datos de temperatura
#' @param ... Datos ambientales adicionales
#' @return Objeto Bioenergetic modificado
#' @export
set_environment <- function(x, temperature_data, ...) {
  UseMethod("set_environment")
}

#' @export
set_environment.Bioenergetic <- function(x, temperature_data, ...) {

  validate_time_series_data(temperature_data, "temperature_data",
                            required_cols = c("Day", "Temperature"))

  x$environmental_data$temperature <- temperature_data

  # Datos adicionales
  additional_data <- list(...)
  x$environmental_data <- c(x$environmental_data, additional_data)

  # Resetear estado de ajuste
  x$fitted <- FALSE
  x$results <- NULL

  return(x)
}

#' Configurar datos de dieta
#'
#' @param x Objeto Bioenergetic
#' @param diet_proportions Data frame con proporciones de dieta
#' @param prey_energies Data frame con energías de presas
#' @return Objeto Bioenergetic modificado
#' @export
set_diet <- function(x, diet_proportions, prey_energies) {
  UseMethod("set_diet")
}

#' @export
set_diet.Bioenergetic <- function(x, diet_proportions, prey_energies) {

  validate_time_series_data(diet_proportions, "diet_proportions",
                            required_cols = "Day", min_cols = 2)
  validate_time_series_data(prey_energies, "prey_energies",
                            required_cols = "Day", min_cols = 2)

  # Verificar que tengan las mismas columnas de presas
  diet_prey_cols <- setdiff(names(diet_proportions), "Day")
  energy_prey_cols <- setdiff(names(prey_energies), "Day")

  if (!identical(sort(diet_prey_cols), sort(energy_prey_cols))) {
    stop("Las columnas de presas deben ser iguales en ambos data frames")
  }

  x$diet_data$proportions <- diet_proportions
  x$diet_data$energies <- prey_energies
  x$diet_data$prey_names <- diet_prey_cols

  # Resetear estado
  x$fitted <- FALSE
  x$results <- NULL

  return(x)
}

#' Configurar opciones del modelo
#'
#' @param x Objeto Bioenergetic
#' @param ... Opciones del modelo como argumentos nombrados
#' @return Objeto Bioenergetic modificado
#' @export
set_options <- function(x, ...) {
  UseMethod("set_options")
}

#' @export
set_options.Bioenergetic <- function(x, ...) {

  new_options <- list(...)

  # Validar opciones conocidas
  valid_options <- c("calc_mortality", "calc_reproduction", "calc_contaminant",
                     "calc_nutrient", "output_daily")

  unknown_options <- setdiff(names(new_options), valid_options)
  if (length(unknown_options) > 0) {
    warning("Opciones desconocidas: ", paste(unknown_options, collapse = ", "))
  }

  # Actualizar opciones
  x$model_options <- modifyList(x$model_options, new_options)

  # Resetear estado
  x$fitted <- FALSE
  x$results <- NULL

  return(x)
}

#' Configurar parámetros de simulación
#'
#' @param x Objeto Bioenergetic
#' @param ... Parámetros de simulación
#' @return Objeto Bioenergetic modificado
#' @export
set_simulation <- function(x, ...) {
  UseMethod("set_simulation")
}

#' @export
set_simulation.Bioenergetic <- function(x, ...) {

  new_settings <- list(...)

  # Validar parámetros críticos
  if ("initial_weight" %in% names(new_settings)) {
    if (new_settings$initial_weight <= 0) {
      stop("initial_weight debe ser positivo")
    }
  }

  if ("fit_to" %in% names(new_settings)) {
    valid_fit_types <- c("Weight", "Consumption", "Ration", "Ration_prey", "p-value")
    if (!new_settings$fit_to %in% valid_fit_types) {
      stop("fit_to debe ser uno de: ", paste(valid_fit_types, collapse = ", "))
    }
  }

  # Actualizar configuración
  x$simulation_settings <- modifyList(x$simulation_settings, new_settings)

  # Resetear estado
  x$fitted <- FALSE
  x$results <- NULL

  return(x)
}

# ============================================================================
# MÉTODOS DE EJECUCIÓN
# ============================================================================

#' Ajustar modelo bioenergético
#'
#' @param x Objeto Bioenergetic
#' @param target_value Valor objetivo para el ajuste
#' @param fit_to Tipo de ajuste ("Weight", "Consumption", etc.)
#' @param ... Argumentos adicionales
#' @return Objeto Bioenergetic con resultados
#' @export
fit <- function(x, target_value = NULL, fit_to = NULL, ...) {
  UseMethod("fit")
}

#' @export
fit.Bioenergetic <- function(x, target_value = NULL, fit_to = NULL, ...) {

  # Usar valores por defecto si no se especifican
  if (is.null(fit_to)) {
    fit_to <- x$simulation_settings$fit_to %||% "Weight"
  }

  if (is.null(target_value)) {
    target_value <- x$simulation_settings$fit_value
    if (is.null(target_value) || is.na(target_value)) {
      stop("target_value debe especificarse o estar en simulation_settings")
    }
  }

  # Validar que tenemos todos los datos necesarios
  if (is.null(x$environmental_data$temperature)) {
    stop("Datos de temperatura requeridos. Use set_environment()")
  }

  if (is.null(x$diet_data$proportions) || is.null(x$diet_data$energies)) {
    stop("Datos de dieta requeridos. Use set_diet()")
  }

  # Preparar datos para el modelo
  processed_data <- process_input_data(
    temperature_data = x$environmental_data$temperature,
    diet_data = x$diet_data$proportions,
    prey_energy_data = x$diet_data$energies,
    first_day = 1,
    last_day = nrow(x$environmental_data$temperature)
  )

  # Ejecutar modelo
  if (fit_to %in% c("Weight", "Consumption")) {
    results <- run_fb4_with_fitting(
      species_params = x$species_params,
      initial_weight = x$simulation_settings$initial_weight,
      fit_to = fit_to,
      fit_value = target_value,
      processed_data = processed_data,
      model_options = x$model_options,
      oxycal = x$simulation_settings$oxycal %||% 13560,
      population_size = x$simulation_settings$population_size %||% 1,
      output_daily = x$model_options$output_daily %||% TRUE,
      progress_callback = NULL
    )
  } else {
    results <- run_fb4_direct(
      species_params = x$species_params,
      initial_weight = x$simulation_settings$initial_weight,
      fit_to = fit_to,
      fit_value = target_value,
      processed_data = processed_data,
      model_options = x$model_options,
      oxycal = x$simulation_settings$oxycal %||% 13560,
      population_size = x$simulation_settings$population_size %||% 1,
      output_daily = x$model_options$output_daily %||% TRUE,
      progress_callback = NULL
    )
  }

  # Guardar resultados
  x$results <- results
  x$fitted <- TRUE

  # Validar resultados
  validation <- validate_fb4_results_internal(results)
  x$validated <- validation$valid

  if (!validation$valid) {
    warning("Resultados no pasaron validación: ",
            paste(validation$errors, collapse = "; "))
  }

  return(x)
}

#' Método predict para objetos Bioenergetic
#'
#' @param object Objeto Bioenergetic ajustado
#' @param newdata Nuevos datos ambientales (opcional)
#' @param ... Argumentos adicionales
#' @return Predicciones del modelo
#' @export
predict.Bioenergetic <- function(object, newdata = NULL, ...) {

  if (!object$fitted) {
    stop("Modelo debe ser ajustado antes de hacer predicciones")
  }

  if (is.null(newdata)) {
    # Usar datos originales
    return(object$results$daily_output)
  } else {
    # Ejecutar con nuevos datos
    # Implementar lógica para nuevos datos
    stop("Predicción con nuevos datos no implementada aún")
  }
}

# ============================================================================
# MÉTODOS DE VISUALIZACIÓN
# ============================================================================

#' Método plot para objetos Bioenergetic
#'
#' @param x Objeto Bioenergetic
#' @param type Tipo de gráfico
#' @param ... Argumentos adicionales
#' @export
plot.Bioenergetic <- function(x, type = "growth", ...) {

  if (!x$fitted) {
    stop("Modelo debe ser ajustado antes de graficar")
  }

  if (is.null(x$results$daily_output)) {
    stop("Resultados diarios no disponibles")
  }

  # Usar método de fb4_result
  plot.fb4_result(x$results, type = type, ...)
}

# ============================================================================
# FUNCIONES DE CONVENIENCIA
# ============================================================================

#' Verificar si un objeto es de clase Bioenergetic
#'
#' @param x Objeto a verificar
#' @return TRUE si es Bioenergetic, FALSE si no
#' @export
is.Bioenergetic <- function(x) {
  inherits(x, "Bioenergetic")
}

#' Extraer resultados de un objeto Bioenergetic
#'
#' @param x Objeto Bioenergetic
#' @param component Componente a extraer ("daily_output", "summary", "all")
#' @return Resultados extraídos
#' @export
get_results <- function(x, component = "all") {
  UseMethod("get_results")
}

#' @export
get_results.Bioenergetic <- function(x, component = "all") {

  if (!x$fitted) {
    stop("Modelo no ha sido ajustado")
  }

  if (component == "all") {
    return(x$results)
  } else if (component == "daily_output") {
    return(x$results$daily_output)
  } else if (component == "summary") {
    return(x$results$summary)
  } else {
    if (component %in% names(x$results)) {
      return(x$results[[component]])
    } else {
      stop("Componente no encontrado: ", component)
    }
  }
}

#' Función de validación interna para resultados
#'
#' @param results Resultados del modelo
#' @return Lista con validación
#' @keywords internal
validate_fb4_results_internal <- function(results) {

  validation <- list(valid = TRUE, errors = character(), warnings = character())

  if (!results$fit_successful) {
    validation$warnings <- c(validation$warnings, "Ajuste no exitoso")
  }

  if (!is.null(results$daily_output)) {
    if (any(results$daily_output$Weight.g <= 0, na.rm = TRUE)) {
      validation$errors <- c(validation$errors, "Pesos negativos detectados")
      validation$valid <- FALSE
    }
  }

  return(validation)
}
