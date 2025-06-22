#' Clases S3 para el Modelo Bioenergético FB4
#'
#' @name bioenergetic-classes
#' @aliases bioenergetic-classes
NULL

#' Operador de valor por defecto
#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a)) a else b

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
Bioenergetic <- function(species_params,
                         environmental_data = NULL,
                         diet_data = NULL,
                         model_options = list(),
                         simulation_settings = list()) {
  stopifnot(!is.null(species_params))
  
  structure(
    list(
      species_params = species_params,
      environmental_data = environmental_data,
      diet_data = diet_data,
      model_options = model_options,
      simulation_settings = simulation_settings,
      results = NULL,
      fitted = FALSE
    ),
    class = c("Bioenergetic", "list")
  )
}

# ============================================================================
# CONSTRUCTOR SIMPLIFICADO
# ============================================================================

#' Constructor simplificado para modelos bioenergéticos
#'
#' @param species Nombre científico o índice de especie
#' @param initial_weight Peso inicial (g)
#' @param life_stage Estadio de vida
#' @param duration Duración (días)
#' @param temperature_avg Temperatura promedio (°C)
#' @param temperature_variation Variación (°C)
#' @param diet_composition Composición de dieta
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
  if (!exists("fish4_parameters")) {
    data("fish4_parameters", package = "fb4package", envir = environment())
  }
  
  # Seleccionar especie
  species_data <- if (is.character(species)) {
    match <- fish4_parameters[[species]]
    if (is.null(match)) {
      stop("Especie no encontrada en fish4_parameters")
    }
    match
  } else if (is.numeric(species) && species >= 1 && species <= length(fish4_parameters)) {
    fish4_parameters[[species]]
  } else {
    stop("Especie debe ser nombre o índice válido")
  }
  
  # Seleccionar estadio de vida
  stages <- names(species_data$life_stages)
  if (is.null(life_stage)) life_stage <- stages[1]
  if (!life_stage %in% stages) stop("Estadio de vida inválido")
  params <- species_data$life_stages[[life_stage]]
  params$species_info <- species_data$species_info
  
  # Temperatura simulada
  days <- 1:duration
  temperature <- temperature_avg + temperature_variation * sin(2 * pi * days / 365)
  env <- list(
    temperature = data.frame(Day = days, Temperature = temperature),
    days = days,
    duration = duration
  )
  
  # Dieta
  diet_names <- names(diet_composition)
  diet_props <- as.numeric(diet_composition)
  diet_props <- diet_props / sum(diet_props)
  
  typical_energies <- list(
    Zooplankton = 3500, Invertebrates = 4200, Fish = 5500,
    Detritus = 2000, Benthos = 4000, Insects = 4500,
    Phytoplankton = 2500, Crustaceans = 4000, Mollusks = 3800
  )
  
  diet_df <- energy_df <- data.frame(Day = days)
  for (i in seq_along(diet_names)) {
    diet_df[[diet_names[i]]] <- diet_props[i]
    energy_df[[diet_names[i]]] <- typical_energies[[diet_names[i]]] %||% 3500
  }
  
  sim <- list(initial_weight = initial_weight, duration = duration)
  opts <- list(calc_mortality = FALSE, calc_reproduction = FALSE, 
               calc_contaminant = FALSE, calc_nutrient = FALSE, output_daily = TRUE)
  
  Bioenergetic(params, env, list(proportions = diet_df, energies = energy_df, prey_names = diet_names), opts, sim)
}

# ============================================================================
# MÉTODOS S3
# ============================================================================

#' @export
print.Bioenergetic <- function(x, ...) {
  cat("=== Modelo Bioenergético FB4 ===\n\n")
  info <- x$species_params$species_info
  if (!is.null(info)) {
    cat("Especie:", info$scientific_name %||% "", "\n")
    cat("Nombre común:", info$common_name %||% "", "\n")
    cat("Familia:", info$family %||% "", "\n")
  }
  cat("Peso inicial:", x$simulation_settings$initial_weight, "g\n")
  cat("Duración:", x$simulation_settings$duration, "días\n")
  cat("Estado:", if (x$fitted) "Ajustado" else "No ajustado", "\n")
  mods <- names(Filter(isTRUE, x$model_options))
  if (length(mods) > 0) cat("Sub-modelos habilitados:", paste(mods, collapse = ", "), "\n")
  invisible(x)
}

#' @export
summary.Bioenergetic <- function(object, ...) {
  print(object)
  cat("\n=== Parámetros ===\n")
  cat("Ecuación de consumo:", get_parameter_value(object$species_params, "CEQ") %||% "NA", "\n")
  cat("Ecuación de respiración:", get_parameter_value(object$species_params, "REQ") %||% "NA", "\n")
  cat("\n=== Temperatura ===\n")
  temps <- object$environmental_data$temperature$Temperature
  cat("Min:", min(temps), "| Max:", max(temps), "| Media:", mean(temps), "°C\n")
  cat("\n=== Dieta ===\n")
  cat("Presas:", paste(object$diet_data$prey_names, collapse = ", "), "\n")
  invisible(object)
}

# ============================================================================
# CONFIGURACIÓN
# ============================================================================

#' @export
set_environment <- function(x, temperature_data) UseMethod("set_environment")

#' @export
set_environment.Bioenergetic <- function(x, temperature_data) {
  stopifnot(is.data.frame(temperature_data), all(c("Day", "Temperature") %in% names(temperature_data)))
  x$environmental_data$temperature <- temperature_data
  x$environmental_data$duration <- max(temperature_data$Day)
  x$fitted <- FALSE; x$results <- NULL
  x
}

#' @export
set_diet <- function(x, diet_proportions, prey_energies) UseMethod("set_diet")

#' @export
set_diet.Bioenergetic <- function(x, diet_proportions, prey_energies) {
  stopifnot(all(c("Day") %in% names(diet_proportions)),
            all(c("Day") %in% names(prey_energies)))
  prey_cols <- setdiff(names(diet_proportions), "Day")
  stopifnot(identical(sort(prey_cols), sort(setdiff(names(prey_energies), "Day"))))
  x$diet_data <- list(proportions = diet_proportions, energies = prey_energies, prey_names = prey_cols)
  x$fitted <- FALSE; x$results <- NULL
  x
}

# ============================================================================
# SIMULACIÓN
# ============================================================================

#' @export
simulate <- function(x, ...) UseMethod("simulate")

#' @export
simulate.Bioenergetic <- function(x, ...) {
  stopifnot(!is.null(x$environmental_data$temperature), !is.null(x$diet_data$proportions))
  
  days <- x$environmental_data$temperature$Day
  temp <- x$environmental_data$temperature$Temperature
  w <- numeric(length(days)); w[1] <- x$simulation_settings$initial_weight
  for (i in 2:length(days)) {
    rate <- pmax(0, (temp[i] - 5) / 20 * 0.02)
    w[i] <- w[i-1] * (1 + rate)
  }
  
  daily_output <- data.frame(
    Day = days,
    Temperature = temp,
    Weight.g = w,
    Consumption = w * 0.05,
    Respiration = w * 0.02,
    Growth = c(0, diff(w))
  )
  
  x$results <- list(
    daily_output = daily_output,
    summary = list(
      initial_weight = w[1],
      final_weight = w[length(w)],
      total_growth = w[length(w)] - w[1],
      duration = length(w)
    ),
    fit_successful = TRUE
  )
  x$fitted <- TRUE
  x
}

# ============================================================================
# UTILIDADES
# ============================================================================

#' @export
is.Bioenergetic <- function(x) inherits(x, "Bioenergetic")

#' @export
get_results <- function(x, component = "all") UseMethod("get_results")

#' @export
get_results.Bioenergetic <- function(x, component = "all") {
  stopifnot(x$fitted)
  if (component == "all") return(x$results)
  if (!component %in% names(x$results)) stop("Componente no encontrado")
  x$results[[component]]
}

#' @export
list_species <- function(fish4_db = NULL, family = NULL) {
  if (is.null(fish4_db)) {
    if (exists("fish4_parameters")) fish4_db <- fish4_parameters
    else if (file.exists("fish4_parameters.RData")) {
      load("fish4_parameters.RData"); fish4_db <- fish4_parameters
    } else stop("Base de datos no encontrada")
  }
  sp <- names(fish4_db)
  if (!is.null(family)) {
    fams <- sapply(fish4_db, function(x) x$species_info$family %||% NA)
    sp <- sp[fams == family & !is.na(fams)]
  }
  sp
}

#' @export
species_info <- function(species, fish4_db = NULL) {
  if (is.null(fish4_db)) {
    if (exists("fish4_parameters")) fish4_db <- fish4_parameters
    else if (file.exists("fish4_parameters.RData")) {
      load("fish4_parameters.RData"); fish4_db <- fish4_parameters
    } else stop("Base de datos no encontrada")
  }
  stopifnot(species %in% names(fish4_db))
  sp <- fish4_db[[species]]
  list(
    scientific_name = species,
    species_info = sp$species_info,
    available_life_stages = names(sp$life_stages),
    sources = sp$sources
  )
}

#' @keywords internal
get_parameter_value <- function(params, param) {
  for (cat in names(params)) {
    if (param %in% names(params[[cat]])) return(params[[cat]][[param]])
  }
  NULL
}

#' @keywords internal
set_parameter_value <- function(params, param, value) {
  for (cat in names(params)) {
    if (param %in% names(params[[cat]])) {
      params[[cat]][[param]] <- value
      return(params)
    }
  }
  stop("Parámetro '", param, "' no encontrado en ninguna categoría.")
}

