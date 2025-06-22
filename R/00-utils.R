#' Funciones Utilitarias para el Modelo FB4
#'
#' @name utils
#' @aliases utils
NULL

# ============================================================================
# VALIDACIONES ESENCIALES
# ============================================================================

#' Validar datos de series de tiempo
#'
#' @param data Datos a validar
#' @param data_name Nombre del conjunto de datos (para mensajes de error)
#' @param required_cols Columnas requeridas
#' @param min_cols Número mínimo de columnas
#' @return NULL (lanza error si inválido)
#' @keywords internal
validate_time_series_data <- function(data, data_name, required_cols = NULL, min_cols = NULL) {
  
  if (is.null(data)) {
    stop(data_name, " no puede ser NULL")
  }
  
  if (!is.data.frame(data)) {
    stop(data_name, " debe ser un data.frame")
  }
  
  if (nrow(data) == 0) {
    stop(data_name, " no puede estar vacío")
  }
  
  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop("Columnas faltantes en ", data_name, ": ", paste(missing_cols, collapse = ", "))
    }
  }
  
  if (!is.null(min_cols) && ncol(data) < min_cols) {
    stop(data_name, " debe tener al menos ", min_cols, " columnas")
  }
  
  if ("Day" %in% names(data)) {
    if (!is.numeric(data$Day)) {
      stop("Columna 'Day' en ", data_name, " debe ser numérica")
    }
    if (any(!is.finite(data$Day))) {
      stop("Columna 'Day' en ", data_name, " contiene valores no finitos")
    }
    if (anyDuplicated(data$Day)) {
      warning("Columna 'Day' en ", data_name, " contiene valores duplicados")
    }
    if (any(diff(data$Day) <= 0)) {
      warning("Columna 'Day' en ", data_name, " no está en orden ascendente")
    }
  }
}

#' Validar parámetros básicos del modelo
#'
#' @param initial_weight Peso inicial
#' @param duration Duración en días
#' @return NULL (lanza error si inválido)
#' @keywords internal
validate_basic_params <- function(initial_weight, duration) {
  if (!is.numeric(initial_weight) || initial_weight <= 0) {
    stop("initial_weight debe ser un número positivo")
  }
  if (!is.numeric(duration) || duration <= 0) {
    stop("duration debe ser un número positivo")
  }
  if (duration > 10000) {
    warning("Duración muy larga (>10000 días), esto puede causar problemas de rendimiento")
  }
}

#' Validar consistencia entre datos de dieta y energía
#'
#' @param diet_data Data frame con proporciones de dieta
#' @param energy_data Data frame con energías de presas
#' @return NULL (lanza error si inconsistente)
#' @keywords internal
validate_diet_consistency <- function(diet_data, energy_data) {
  diet_prey_cols <- setdiff(names(diet_data), "Day")
  energy_prey_cols <- setdiff(names(energy_data), "Day")
  
  if (length(diet_prey_cols) == 0) {
    stop("diet_data debe tener al menos una columna de presa además de 'Day'")
  }
  
  if (!identical(sort(diet_prey_cols), sort(energy_prey_cols))) {
    stop("Las columnas de presas deben ser iguales en diet_data y energy_data")
  }
  
  diet_sums <- rowSums(diet_data[diet_prey_cols], na.rm = TRUE)
  if (any(abs(diet_sums - 1) > 0.1, na.rm = TRUE)) {
    warning("Algunas proporciones de dieta se desvían significativamente de 1.0")
  }
  
  if (any(as.matrix(diet_data[diet_prey_cols]) < 0, na.rm = TRUE)) {
    stop("Las proporciones de dieta no pueden ser negativas")
  }
  
  if (any(as.matrix(energy_data[energy_prey_cols]) <= 0, na.rm = TRUE)) {
    stop("Las energías de presas deben ser positivas")
  }
}

# ============================================================================
# FUNCIONES DE PROCESAMIENTO DE DATOS
# ============================================================================

#' Procesar datos de entrada para el modelo
#'
#' @param temperature_data Data frame con columnas Day y Temperature
#' @param diet_data Data frame con proporciones de dieta
#' @param prey_energy_data Data frame con energías de presas
#' @param first_day Primer día de simulación
#' @param last_day Último día de simulación
#' @param interp_method Método de interpolación (por defecto "linear")
#' @return Lista con datos procesados
#' @export
process_input_data <- function(temperature_data, diet_data, prey_energy_data, 
                               first_day = 1, last_day = NULL, interp_method = "linear") {
  
  if (is.null(last_day)) {
    last_day <- max(temperature_data$Day)
  }
  
  if (first_day >= last_day || first_day < 1) {
    stop("Rango de días inválido: first_day debe ser < last_day y >= 1")
  }
  
  validate_time_series_data(temperature_data, "temperature_data", c("Day", "Temperature"))
  validate_time_series_data(diet_data, "diet_data", "Day", min_cols = 2)
  validate_time_series_data(prey_energy_data, "prey_energy_data", "Day", min_cols = 2)
  validate_diet_consistency(diet_data, prey_energy_data)
  
  temp_filtered   <- temperature_data[temperature_data$Day >= first_day & temperature_data$Day <= last_day, ]
  diet_filtered   <- diet_data[diet_data$Day >= first_day & diet_data$Day <= last_day, ]
  energy_filtered <- prey_energy_data[prey_energy_data$Day >= first_day & prey_energy_data$Day <= last_day, ]
  
  if (nrow(temp_filtered) == 0) stop("No hay datos de temperatura en el rango especificado")
  if (nrow(diet_filtered) == 0) stop("No hay datos de dieta en el rango especificado")
  
  all_days <- first_day:last_day
  
  temp_complete   <- interpolate_missing_days(temp_filtered, all_days, "Temperature", interp_method)
  diet_complete   <- interpolate_missing_days(diet_filtered, all_days, setdiff(names(diet_filtered), "Day"), interp_method)
  energy_complete <- interpolate_missing_days(energy_filtered, all_days, setdiff(names(energy_filtered), "Day"), interp_method)
  
  prey_names <- setdiff(names(diet_data), "Day")
  
  return(list(
    temperature = temp_complete,
    diet_proportions = diet_complete,
    prey_energies = energy_complete,
    prey_names = prey_names,
    first_day = first_day,
    last_day = last_day,
    duration = last_day - first_day + 1
  ))
}

#' Interpolar datos faltantes por días
#'
#' @param data Data frame con datos
#' @param all_days Vector con todos los días necesarios
#' @param value_cols Columnas a interpolar
#' @param method Método de interpolación: "linear" o "spline"
#' @return Data frame con datos completos
#' @keywords internal
interpolate_missing_days <- function(data, all_days, value_cols, method = "linear") {
  complete_data <- data.frame(Day = all_days)
  merged_data <- merge(complete_data, data, by = "Day", all.x = TRUE)
  
  for (col in value_cols) {
    if (col %in% names(merged_data)) {
      x <- merged_data$Day[!is.na(merged_data[[col]])]
      y <- merged_data[[col]][!is.na(merged_data[[col]])]
      if (length(x) >= 2) {
        if (method == "spline") {
          merged_data[[col]] <- spline(x, y, xout = merged_data$Day)$y
        } else {
          merged_data[[col]] <- approx(x, y, xout = merged_data$Day, method = "linear", rule = 2)$y
        }
      } else {
        warning("No se puede interpolar la columna ", col, ": datos insuficientes")
      }
    }
  }
  
  return(merged_data)
}

# ============================================================================
# FUNCIONES DE UTILIDAD MATEMÁTICA
# ============================================================================

safe_sqrt <- function(x, min_val = 0) {
  if (is.na(x) || !is.finite(x)) return(NA_real_)
  if (x < min_val) return(min_val)
  return(sqrt(x))
}

safe_exp <- function(x, max_exp = 700) {
  if (is.na(x) || !is.finite(x)) return(NA_real_)
  if (x > max_exp) return(exp(max_exp))
  if (x < -max_exp) return(0)
  return(exp(x))
}

clamp <- function(x, min_val, max_val) {
  pmax(min_val, pmin(max_val, x))
}

# ============================================================================
# FUNCIONES DE VALIDACIÓN DE PARÁMETROS
# ============================================================================

#' Validar parámetros de especies para FB4
#'
#' @param species_params Lista con parámetros de especies
#' @return Lista con campos: valid (bool), errors, warnings
#' @export
validate_species_params <- function(species_params) {
  validation <- list(valid = TRUE, errors = character(), warnings = character())
  
  required_groups <- c("consumption", "respiration")
  missing_groups <- setdiff(required_groups, names(species_params))
  if (length(missing_groups) > 0) {
    validation$errors <- c(validation$errors, paste("Grupos faltantes:", paste(missing_groups, collapse = ", ")))
    validation$valid <- FALSE
  }
  
  if ("consumption" %in% names(species_params)) {
    cons <- validate_consumption_params(species_params$consumption)
    validation$errors <- c(validation$errors, cons$errors)
    validation$warnings <- c(validation$warnings, cons$warnings)
    if (!cons$valid) validation$valid <- FALSE
  }
  
  if ("respiration" %in% names(species_params)) {
    resp <- validate_respiration_params(species_params$respiration)
    validation$errors <- c(validation$errors, resp$errors)
    validation$warnings <- c(validation$warnings, resp$warnings)
    if (!resp$valid) validation$valid <- FALSE
  }
  
  return(validation)
}

validate_consumption_params <- function(consumption_params) {
  validation <- list(valid = TRUE, errors = character(), warnings = character())
  critical <- c("CA", "CB", "CQ", "CTO", "CTM", "CTL")
  
  for (param in critical) {
    if (is.null(consumption_params[[param]]) || is.na(consumption_params[[param]])) {
      validation$errors <- c(validation$errors, paste("Parámetro faltante:", param))
      validation$valid <- FALSE
    }
  }
  
  if (!is.null(consumption_params$CTO) && !is.null(consumption_params$CTM)) {
    if (consumption_params$CTO >= consumption_params$CTM) {
      validation$warnings <- c(validation$warnings, "CTO debería ser menor que CTM")
    }
  }
  
  if (!is.null(consumption_params$CTM) && !is.null(consumption_params$CTL)) {
    if (consumption_params$CTM >= consumption_params$CTL) {
      validation$warnings <- c(validation$warnings, "CTM debería ser menor que CTL")
    }
  }
  
  return(validation)
}

validate_respiration_params <- function(respiration_params) {
  validation <- list(valid = TRUE, errors = character(), warnings = character())
  critical <- c("RA", "RB", "RQ", "RTO", "RTM", "RTL")
  
  for (param in critical) {
    if (is.null(respiration_params[[param]]) || is.na(respiration_params[[param]])) {
      validation$errors <- c(validation$errors, paste("Parámetro faltante:", param))
      validation$valid <- FALSE
    }
  }
  
  for (param in c("RA", "RB")) {
    if (!is.null(respiration_params[[param]]) && !is.na(respiration_params[[param]])) {
      if (respiration_params[[param]] <= 0) {
        validation$errors <- c(validation$errors, paste(param, "debe ser positivo"))
        validation$valid <- FALSE
      }
    }
  }
  
  return(validation)
}

# ============================================================================
# FUNCIONES DE MANEJO DE ERRORES
# ============================================================================

check_numeric_value <- function(value, name, min_val = -Inf, max_val = Inf) {
  if (is.null(value) || !is.numeric(value) || !is.finite(value)) {
    stop(name, " debe ser un valor numérico válido")
  }
  if (length(value) != 1) stop(name, " debe ser un valor escalar")
  if (value < min_val || value > max_val) {
    stop(name, " debe estar entre ", min_val, " y ", max_val)
  }
  return(as.numeric(value))
}

# ============================================================================
# OPERADORES
# ============================================================================

#' Operador de null coalescing
#'
#' @param x Primer valor
#' @param y Valor por defecto si x es NULL
#' @return x si no es NULL, y en caso contrario
#' @export
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Operador de concatenación de strings
#' @keywords internal
`%+%` <- function(a, b) paste0(a, b)
