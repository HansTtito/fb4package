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
    fitted = FALSE
  )
  
  # Asignar clases
  class(obj) <- c("Bioenergetic", "list")
  
  # Validar el objeto
  validate_bioenergetic(obj)
  
  return(obj)
}

#' Constructor simplificado para modelos bioenergéticos
#'
#' @param species Nombre científico de la especie o número de índice
#' @param initial_weight Peso inicial (g)
#' @param life_stage Estadio de vida (por defecto usa el primero disponible)
#' @param duration Duración en días
#' @param temperature_avg Temperatura promedio (°C)
#' @param temperature_variation Variación de temperatura (°C)
#' @param diet_composition Lista con composición de dieta
#' @param ... Argumentos adicionales
#' @return Objeto Bioenergetic
#' @export
new_bioenergetic <- function(species,
                             initial_weight,
                             life_stage = NULL,
                             duration = 365,
                             temperature_avg = 20,
                             temperature_variation = 5,
                             diet_composition = list(Zooplankton = 1.0),
                             ...) {
  
  # Cargar base de datos fish4_parameters
  if (!exists("fish4_parameters")) {
    data("fish4_parameters", package = "fb4package", envir = environment())
  }
  
  # Determinar parámetros de especies
  if (is.character(species)) {
    if (!species %in% names(fish4_parameters)) {
      available <- names(fish4_parameters)
      stop("Especie '", species, "' no encontrada. Especies disponibles: ", 
           paste(head(available, 10), collapse = ", "), 
           if (length(available) > 10) " (y más...)" else "")
    }
    species_data <- fish4_parameters[[species]]
  } else if (is.numeric(species)) {
    if (species < 1 || species > length(fish4_parameters)) {
      stop("Índice de especie fuera de rango: ", species, 
           ". Debe estar entre 1 y ", length(fish4_parameters))
    }
    species_data <- fish4_parameters[[species]]
  } else {
    stop("species debe ser un nombre científico (character) o un índice (numeric)")
  }
  
  # Determinar estadio de vida
  available_stages <- names(species_data$life_stages)
  if (is.null(life_stage)) {
    life_stage <- available_stages[1]
    message("Usando estadio de vida: ", life_stage)
  } else {
    if (!life_stage %in% available_stages) {
      stop("Estadio '", life_stage, "' no disponible para esta especie. ",
           "Disponibles: ", paste(available_stages, collapse = ", "))
    }
  }
  
  # Extraer parámetros del estadio específico
  species_params <- species_data$life_stages[[life_stage]]
  
  # Añadir información de la especie
  species_params$species_info <- species_data$species_info
  
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
  
  # Normalizar proporciones
  diet_props <- diet_props / sum(diet_props)
  
  diet_df <- data.frame(Day = days)
  energy_df <- data.frame(Day = days)
  
  # Energías típicas por tipo de presa (J/g)
  typical_energies <- list(
    Zooplankton = 3500, Invertebrates = 4200, Fish = 5500,
    Detritus = 2000, Benthos = 4000, Insects = 4500,
    Phytoplankton = 2500, Crustaceans = 4000, Mollusks = 3800
  )
  
  for (i in seq_along(diet_names)) {
    diet_df[[diet_names[i]]] <- diet_props[i]
    energy <- typical_energies[[diet_names[i]]] 
    if (is.null(energy)) energy <- 3500  # valor por defecto
    energy_df[[diet_names[i]]] <- energy
  }
  
  # Configuración de simulación
  simulation_settings <- list(
    initial_weight = initial_weight,
    duration = duration
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
  obj <- list(
    species_parameters = species_params,
    environmental_data = environmental_data,
    simulation_settings = simulation_settings,
    model_options = model_options
  )
  
  class(obj) <- c("Bioenergetic", "list")
  return(obj)
}

# ============================================================================
# MÉTODOS BÁSICOS
# ============================================================================

#' Método print para objetos Bioenergetic
#'
#' @param x Objeto Bioenergetic
#' @param ... Argumentos adicionales
#' @export
print.Bioenergetic <- function(x, ...) {
  cat("=== Modelo Bioenergético FB4 ===\n\n")
  
  # Información de la especie
  if (!is.null(x$species_params$species_info)) {
    if (!is.null(x$species_params$species_info$scientific_name)) {
      cat("Especie:", x$species_params$species_info$scientific_name, "\n")
    }
    if (!is.null(x$species_params$species_info$common_name)) {
      cat("Nombre común:", x$species_params$species_info$common_name, "\n")
    }
    if (!is.null(x$species_params$species_info$family)) {
      cat("Familia:", x$species_params$species_info$family, "\n")
    }
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
    if (!is.null(object$species_params$consumption$CEQ)) {
      cat("Ecuación de consumo:", object$species_params$consumption$CEQ, "\n")
    }
  }
  if (!is.null(object$species_params$respiration)) {
    if (!is.null(object$species_params$respiration$REQ)) {
      cat("Ecuación de respiración:", object$species_params$respiration$REQ, "\n")
    }
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
    } else if (!is.null(object$results$daily_output)) {
      cat("Datos diarios disponibles con", nrow(object$results$daily_output), "registros\n")
    }
  }
  
  invisible(object)
}

# ============================================================================
# MÉTODOS DE CONFIGURACIÓN ESENCIALES
# ============================================================================

#' Configurar datos ambientales
#'
#' @param x Objeto Bioenergetic
#' @param temperature_data Data frame con columnas Day y Temperature
#' @return Objeto Bioenergetic modificado
#' @export
set_environment <- function(x, temperature_data) {
  UseMethod("set_environment")
}

#' @export
set_environment.Bioenergetic <- function(x, temperature_data) {
  
  # Validar estructura
  if (!is.data.frame(temperature_data)) {
    stop("temperature_data debe ser un data.frame")
  }
  
  required_cols <- c("Day", "Temperature")
  missing_cols <- setdiff(required_cols, names(temperature_data))
  if (length(missing_cols) > 0) {
    stop("Columnas faltantes en temperature_data: ", paste(missing_cols, collapse = ", "))
  }
  
  x$environmental_data$temperature <- temperature_data
  x$environmental_data$duration <- max(temperature_data$Day)
  
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
  
  # Validar estructura básica
  if (!is.data.frame(diet_proportions) || !is.data.frame(prey_energies)) {
    stop("Ambos argumentos deben ser data.frames")
  }
  
  if (!"Day" %in% names(diet_proportions) || !"Day" %in% names(prey_energies)) {
    stop("Ambos data.frames deben tener columna 'Day'")
  }
  
  # Verificar que tengan las mismas columnas de presas
  diet_prey_cols <- setdiff(names(diet_proportions), "Day")
  energy_prey_cols <- setdiff(names(prey_energies), "Day")
  
  if (length(diet_prey_cols) == 0 || length(energy_prey_cols) == 0) {
    stop("Debe haber al menos una columna de presa además de 'Day'")
  }
  
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

# ============================================================================
# MÉTODOS DE EJECUCIÓN SIMPLIFICADOS
# ============================================================================

#' Simular modelo bioenergético
#'
#' @param x Objeto Bioenergetic
#' @param ... Argumentos adicionales pasados al simulador
#' @return Objeto Bioenergetic con resultados
#' @export
simulate <- function(x, ...) {
  UseMethod("simulate")
}

#' @export
simulate.Bioenergetic <- function(x, ...) {
  
  # Validar que tenemos todos los datos necesarios
  if (is.null(x$environmental_data$temperature)) {
    stop("Datos de temperatura requeridos. Use set_environment()")
  }
  
  if (is.null(x$diet_data$proportions) || is.null(x$diet_data$energies)) {
    stop("Datos de dieta requeridos. Use set_diet()")
  }
  
  # Aquí iría la llamada a tu función de simulación FB4
  # Por ahora, creo resultados simulados para testing
  
  days <- x$environmental_data$temperature$Day
  n_days <- length(days)
  
  # Simular crecimiento simple
  initial_weight <- x$simulation_settings$initial_weight
  weights <- numeric(n_days)
  weights[1] <- initial_weight
  
  # Crecimiento simple basado en temperatura
  temps <- x$environmental_data$temperature$Temperature
  for (i in 2:n_days) {
    growth_rate <- pmax(0, (temps[i] - 5) / 20 * 0.02)  # Crecimiento simple
    weights[i] <- weights[i-1] * (1 + growth_rate)
  }
  
  # Crear resultados sintéticos
  daily_output <- data.frame(
    Day = days,
    Temperature = temps,
    Weight.g = weights,
    Consumption = weights * 0.05,  # 5% del peso corporal
    Respiration = weights * 0.02,   # 2% del peso corporal
    Growth = c(0, diff(weights))
  )
  
  results <- list(
    daily_output = daily_output,
    summary = list(
      initial_weight = initial_weight,
      final_weight = weights[n_days],
      total_growth = weights[n_days] - initial_weight,
      duration = n_days
    ),
    fit_successful = TRUE
  )
  
  # Guardar resultados
  x$results <- results
  x$fitted <- TRUE
  
  return(x)
}

# ============================================================================
# FUNCIONES DE UTILIDAD
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
    stop("Modelo no ha sido ajustado. Use simulate() primero.")
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
      stop("Componente '", component, "' no encontrado. ",
           "Disponibles: ", paste(names(x$results), collapse = ", "))
    }
  }
}

#' Listar especies disponibles en la base de datos
#'
#' @param fish4_db Base de datos fish4_parameters (opcional)
#' @param family Filtrar por familia (opcional)
#' @return Vector con nombres de especies
#' @export
list_species <- function(fish4_db = NULL, family = NULL) {
  
  # Cargar base de datos si no se proporciona
  if (is.null(fish4_db)) {
    if (exists("fish4_parameters")) {
      fish4_db <- fish4_parameters
    } else if (file.exists("fish4_parameters.RData")) {
      load("fish4_parameters.RData")
      fish4_db <- fish4_parameters
    } else {
      stop("Base de datos fish4_parameters no encontrada")
    }
  }
  
  species_names <- names(fish4_db)
  
  if (!is.null(family)) {
    # Filtrar por familia
    families <- sapply(fish4_db, function(x) {
      if (!is.null(x$species_info$family)) {
        return(x$species_info$family)
      } else {
        return(NA)
      }
    })
    species_names <- species_names[families == family & !is.na(families)]
  }
  
  return(species_names)
}

#' Obtener información de una especie
#'
#' @param species Nombre científico de la especie
#' @param fish4_db Base de datos fish4_parameters (opcional)
#' @return Lista con información de la especie
#' @export
species_info <- function(species, fish4_db = NULL) {
  
  # Cargar base de datos si no se proporciona
  if (is.null(fish4_db)) {
    if (exists("fish4_parameters")) {
      fish4_db <- fish4_parameters
    } else if (file.exists("fish4_parameters.RData")) {
      load("fish4_parameters.RData")
      fish4_db <- fish4_parameters
    } else {
      stop("Base de datos fish4_parameters no encontrada")
    }
  }
  
  if (!species %in% names(fish4_db)) {
    stop("Especie '", species, "' no encontrada")
  }
  
  species_data <- fish4_db[[species]]
  
  info <- list(
    scientific_name = species,
    species_info = species_data$species_info,
    available_life_stages = names(species_data$life_stages),
    sources = species_data$sources
  )
  
  return(info)
}



#' Obtener valor de parámetro
#' @keywords internal
get_parameter_value <- function(species_params, parameter_name) {
  # Buscar en todas las categorías
  for (category in names(species_params)) {
    if (parameter_name %in% names(species_params[[category]])) {
      return(species_params[[category]][[parameter_name]])
    }
  }
  return(NULL)
}

#' Establecer valor de parámetro
#' @keywords internal
set_parameter_value <- function(species_params, parameter_name, value) {
  # Buscar en todas las categorías y actualizar
  for (category in names(species_params)) {
    if (parameter_name %in% names(species_params[[category]])) {
      species_params[[category]][[parameter_name]] <- value
      return(species_params)
    }
  }
  
  # Si no se encuentra, añadir a consumption por defecto
  species_params$consumption[[parameter_name]] <- value
  return(species_params)
}



#' Agregar datos de temperatura a objeto Bioenergetic
#'
#' @param bio_obj Objeto Bioenergetic
#' @param temperature_data Data frame con columnas Day y Temperature
#' @return Objeto Bioenergetic modificado
#' @export
add_temperature_data <- function(bio_obj, temperature_data) {
  
  if (!is.data.frame(temperature_data)) {
    stop("temperature_data debe ser un data.frame")
  }
  
  required_cols <- c("Day", "Temperature")
  missing_cols <- setdiff(required_cols, names(temperature_data))
  if (length(missing_cols) > 0) {
    stop("Columnas faltantes: ", paste(missing_cols, collapse = ", "))
  }
  
  bio_obj$environmental_data$temperature <- temperature_data
  bio_obj$environmental_data$duration <- max(temperature_data$Day)
  
  return(bio_obj)
}

#' Agregar datos de dieta a objeto Bioenergetic
#'
#' @param bio_obj Objeto Bioenergetic
#' @param diet_data Data frame con proporciones de dieta
#' @return Objeto Bioenergetic modificado
#' @export
add_diet_data <- function(bio_obj, diet_data) {
  
  if (!is.data.frame(diet_data)) {
    stop("diet_data debe ser un data.frame")
  }
  
  if (!"Day" %in% names(diet_data)) {
    stop("diet_data debe tener columna 'Day'")
  }
  
  prey_cols <- setdiff(names(diet_data), "Day")
  if (length(prey_cols) == 0) {
    stop("diet_data debe tener al menos una columna de presa")
  }
  
  # Crear energías por defecto
  energy_data <- diet_data[, "Day", drop = FALSE]
  for (prey in prey_cols) {
    energy_data[[prey]] <- 4000  # J/g por defecto
  }
  
  bio_obj$environmental_data$diet <- diet_data
  bio_obj$environmental_data$prey_energy <- energy_data
  
  return(bio_obj)
}

