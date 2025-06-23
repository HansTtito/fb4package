#' Funciones de Mortalidad y Reproducción para el Modelo FB4
#'
#' @name mortality-reproduction
#' @aliases mortality-reproduction
NULL

# ============================================================================
# FUNCIONES CORE DE MORTALIDAD
# ============================================================================

#' Calcular supervivencia diaria combinada
#'
#' Calcula la supervivencia considerando múltiples fuentes de mortalidad
#'
#' @param mortality_rates Vector de tasas de mortalidad por fuente (fracción diaria 0-1)
#' @param method Método de combinación ("independent", "additive")
#' @return Tasa de supervivencia combinada
#' @keywords internal
calculate_combined_survival <- function(mortality_rates, method = "independent") {
  
  # Validar entradas
  mortality_rates <- mortality_rates[!is.na(mortality_rates)]
  
  if (any(mortality_rates < 0 | mortality_rates > 1)) {
    warning("Tasas de mortalidad fuera de rango [0,1], corrigiendo")
    mortality_rates <- clamp(mortality_rates, 0, 1)
  }
  
  if (method == "independent") {
    # Supervivencia independiente: S = Π(1 - mi)
    survival_rate <- prod(1 - mortality_rates)
  } else if (method == "additive") {
    # Mortalidad aditiva: M = Σmi, S = 1 - M
    combined_mortality <- sum(mortality_rates)
    survival_rate <- max(0, 1 - combined_mortality)
  } else {
    stop("method debe ser 'independent' o 'additive'")
  }
  
  return(clamp(survival_rate, 0, 1))
}

#' Calcular mortalidad dependiente del peso
#'
#' Ajusta la mortalidad basada en el peso actual del pez
#'
#' @param current_weight Peso actual (g)
#' @param base_mortality Tasa base de mortalidad diaria
#' @param weight_threshold Peso umbral por debajo del cual aumenta mortalidad
#' @param starvation_factor Factor de multiplicación por inanición
#' @param initial_weight Peso inicial para comparación (opcional)
#' @return Tasa de mortalidad ajustada
#' @keywords internal
calculate_weight_dependent_mortality <- function(current_weight, base_mortality,
                                                 weight_threshold = 0,
                                                 starvation_factor = 5,
                                                 initial_weight = NULL) {
  
  mortality_rate <- base_mortality
  
  # Mortalidad por peso absoluto bajo
  if (weight_threshold > 0 && current_weight < weight_threshold) {
    weight_ratio <- current_weight / weight_threshold
    starvation_effect <- starvation_factor * (1 - weight_ratio)
    mortality_rate <- mortality_rate + starvation_effect * base_mortality
  }
  
  # Mortalidad por pérdida relativa de peso
  if (!is.null(initial_weight) && initial_weight > 0) {
    weight_loss_fraction <- 1 - (current_weight / initial_weight)
    if (weight_loss_fraction > 0.5) {  # Más del 50% de pérdida de peso
      severe_loss_effect <- 2 * (weight_loss_fraction - 0.5)
      mortality_rate <- mortality_rate + severe_loss_effect * base_mortality
    }
  }
  
  return(clamp(mortality_rate, 0, 0.99))
}

#' Calcular mortalidad dependiente de temperatura
#'
#' Ajusta la mortalidad basada en estrés térmico
#'
#' @param temperature Temperatura actual (°C)
#' @param base_mortality Tasa base de mortalidad diaria
#' @param optimal_temp Temperatura óptima (°C)
#' @param thermal_tolerance Rango de tolerancia térmica (°C)
#' @param stress_factor Factor de multiplicación por estrés térmico
#' @return Tasa de mortalidad ajustada
#' @keywords internal
calculate_temperature_dependent_mortality <- function(temperature, base_mortality,
                                                      optimal_temp = 20,
                                                      thermal_tolerance = 10,
                                                      stress_factor = 2) {
  
  temp_deviation <- abs(temperature - optimal_temp)
  
  if (temp_deviation > thermal_tolerance) {
    thermal_stress <- (temp_deviation - thermal_tolerance) / thermal_tolerance
    stress_mortality <- stress_factor * thermal_stress * base_mortality
    total_mortality <- base_mortality + stress_mortality
  } else {
    total_mortality <- base_mortality
  }
  
  return(clamp(total_mortality, 0, 0.99))
}

# ============================================================================
# FUNCIONES CORE DE REPRODUCCIÓN
# ============================================================================

#' Calcular pérdida de peso por reproducción
#'
#' Calcula la pérdida de peso y energía durante eventos reproductivos
#'
#' @param spawn_fraction Fracción de peso perdido en reproducción (0-1)
#' @param current_weight Peso actual del pez (g)
#' @param energy_density Densidad energética del tejido reproductivo (J/g)
#' @return Lista con pérdidas de peso y energía
#' @keywords internal
calculate_reproductive_loss <- function(spawn_fraction, current_weight, energy_density = 5000) {
  
  # Validar entradas
  spawn_fraction <- clamp(spawn_fraction, 0, 1)
  current_weight <- check_numeric_value(current_weight, "current_weight", min_val = 0.001)
  energy_density <- check_numeric_value(energy_density, "energy_density", min_val = 100)
  
  # Cálculos
  weight_loss <- spawn_fraction * current_weight
  energy_loss <- weight_loss * energy_density
  
  return(list(
    weight_loss = weight_loss,
    energy_loss = energy_loss,
    spawn_fraction = spawn_fraction,
    remaining_weight = current_weight - weight_loss
  ))
}

#' Generar patrón reproductivo estacional
#'
#' Crea un patrón de reproducción estacional simplificado
#'
#' @param days Vector de días del año
#' @param peak_day Día del pico reproductivo
#' @param duration Duración del período reproductivo (días)
#' @param max_spawn_fraction Fracción máxima de peso perdido
#' @param pattern_type Tipo de patrón ("gaussian", "uniform", "pulse")
#' @return Vector con fracciones reproductivas por día
#' @keywords internal
generate_reproduction_pattern <- function(days, peak_day, duration,
                                          max_spawn_fraction = 0.15,
                                          pattern_type = "gaussian") {
  
  n_days <- length(days)
  spawn_fractions <- rep(0, n_days)
  
  if (pattern_type == "gaussian") {
    # Patrón gaussiano centrado en peak_day
    for (i in seq_along(days)) {
      day <- days[i]
      
      # Distancia al pico (considerando circularidad del año)
      dist_to_peak <- min(abs(day - peak_day), 365 - abs(day - peak_day))
      
      if (dist_to_peak <= duration/2) {
        # Función gaussiana
        sigma <- duration / 4  # Desviación estándar
        spawn_fractions[i] <- max_spawn_fraction * exp(-0.5 * (dist_to_peak / sigma)^2)
      }
    }
    
  } else if (pattern_type == "uniform") {
    # Patrón uniforme durante el período
    start_day <- peak_day - duration/2
    end_day <- peak_day + duration/2
    
    for (i in seq_along(days)) {
      day <- days[i]
      
      # Verificar si está en período reproductivo (considerando circularidad)
      in_season <- FALSE
      if (start_day >= 1 && end_day <= 365) {
        in_season <- day >= start_day && day <= end_day
      } else {
        # Manejar casos donde el período cruza el año
        if (start_day < 1) {
          in_season <- day >= (start_day + 365) || day <= end_day
        } else if (end_day > 365) {
          in_season <- day >= start_day || day <= (end_day - 365)
        }
      }
      
      if (in_season) {
        spawn_fractions[i] <- max_spawn_fraction / duration
      }
    }
    
  } else if (pattern_type == "pulse") {
    # Evento de desove puntual
    closest_day_index <- which.min(abs(days - peak_day))
    if (length(closest_day_index) > 0) {
      spawn_fractions[closest_day_index] <- max_spawn_fraction
    }
  }
  
  return(pmax(0, spawn_fractions))
}

# ============================================================================
# FUNCIÓN PRINCIPAL DE MORTALIDAD Y REPRODUCCIÓN
# ============================================================================

#' Calcular mortalidad y reproducción diaria
#'
#' Función principal para calcular efectos de mortalidad y reproducción
#'
#' @param current_weight Peso actual del pez (g)
#' @param temperature Temperatura del agua (°C)
#' @param day_of_year Día del año (1-365)
#' @param mortality_params Lista con parámetros de mortalidad
#' @param reproduction_params Lista con parámetros de reproducción (opcional)
#' @param initial_weight Peso inicial para cálculos relativos (opcional)
#' @return Lista con resultados de mortalidad y reproducción
#' @export
calculate_mortality_reproduction <- function(current_weight, temperature, day_of_year,
                                             mortality_params, reproduction_params = NULL,
                                             initial_weight = NULL) {
  
  # Validaciones básicas
  current_weight <- check_numeric_value(current_weight, "current_weight", min_val = 0.001)
  temperature <- check_numeric_value(temperature, "temperature", min_val = -5, max_val = 50)
  day_of_year <- check_numeric_value(day_of_year, "day_of_year", min_val = 1, max_val = 365)
  
  if (is.null(mortality_params)) {
    stop("mortality_params no puede ser NULL")
  }
  
  # Extraer parámetros de mortalidad con valores por defecto
  base_mortality <- mortality_params$base_mortality %||% 0.001
  natural_mortality <- mortality_params$natural_mortality %||% base_mortality
  fishing_mortality <- mortality_params$fishing_mortality %||% 0
  predation_mortality <- mortality_params$predation_mortality %||% 0
  
  # Parámetros de dependencia
  weight_threshold <- mortality_params$weight_threshold %||% 0
  starvation_factor <- mortality_params$starvation_factor %||% 5
  optimal_temp <- mortality_params$optimal_temp %||% 20
  thermal_tolerance <- mortality_params$thermal_tolerance %||% 10
  stress_factor <- mortality_params$stress_factor %||% 2
  
  # Calcular mortalidad dependiente del peso
  weight_adjusted_mortality <- calculate_weight_dependent_mortality(
    current_weight = current_weight,
    base_mortality = natural_mortality,
    weight_threshold = weight_threshold,
    starvation_factor = starvation_factor,
    initial_weight = initial_weight
  )
  
  # Calcular mortalidad dependiente de temperatura
  temp_adjusted_mortality <- calculate_temperature_dependent_mortality(
    temperature = temperature,
    base_mortality = weight_adjusted_mortality,
    optimal_temp = optimal_temp,
    thermal_tolerance = thermal_tolerance,
    stress_factor = stress_factor
  )
  
  # Combinar todas las fuentes de mortalidad
  all_mortality_rates <- c(
    natural = temp_adjusted_mortality,
    fishing = fishing_mortality,
    predation = predation_mortality
  )
  
  # Calcular supervivencia combinada
  survival_rate <- calculate_combined_survival(all_mortality_rates, method = "independent")
  combined_mortality <- 1 - survival_rate
  
  # Resultado de mortalidad
  mortality_result <- list(
    survival_rate = survival_rate,
    combined_mortality = combined_mortality,
    natural_mortality = temp_adjusted_mortality,
    fishing_mortality = fishing_mortality,
    predation_mortality = predation_mortality,
    weight_effect = weight_adjusted_mortality - natural_mortality,
    temperature_effect = temp_adjusted_mortality - weight_adjusted_mortality
  )
  
  # Calcular reproducción si se especifica
  reproduction_result <- NULL
  if (!is.null(reproduction_params)) {
    
    # Extraer parámetros reproductivos
    spawn_pattern <- reproduction_params$spawn_pattern %||% rep(0, 365)
    energy_density <- reproduction_params$energy_density %||% 5000
    
    # Obtener fracción reproductiva para el día actual
    if (length(spawn_pattern) >= day_of_year) {
      spawn_fraction <- spawn_pattern[day_of_year]
    } else {
      spawn_fraction <- 0
    }
    
    # Calcular pérdidas reproductivas si hay desove
    if (spawn_fraction > 0) {
      reproduction_result <- calculate_reproductive_loss(
        spawn_fraction = spawn_fraction,
        current_weight = current_weight,
        energy_density = energy_density
      )
    } else {
      reproduction_result <- list(
        weight_loss = 0,
        energy_loss = 0,
        spawn_fraction = 0,
        remaining_weight = current_weight
      )
    }
  }
  
  return(list(
    mortality = mortality_result,
    reproduction = reproduction_result,
    day_of_year = day_of_year,
    current_weight = current_weight,
    temperature = temperature
  ))
}

# ============================================================================
# FUNCIONES DE CONVERSIÓN Y UTILIDADES
# ============================================================================


#' Calcular supervivencia acumulativa
#'
#' Calcula la supervivencia a lo largo de múltiples períodos
#'
#' @param survival_rates Vector de tasas de supervivencia diarias
#' @param initial_population Población inicial
#' @return Vector con población superviviente por día
#' @export
calculate_cumulative_survival <- function(survival_rates, initial_population = 1) {
  
  n_days <- length(survival_rates)
  population <- numeric(n_days)
  population[1] <- initial_population
  
  for (i in 2:n_days) {
    population[i] <- population[i-1] * survival_rates[i-1]
  }
  
  return(population)
}

#' Validar parámetros de mortalidad y reproducción
#'
#' Verifica que los parámetros estén en rangos realistas
#'
#' @param mortality_params Lista con parámetros de mortalidad
#' @param reproduction_params Lista con parámetros de reproducción (opcional)
#' @param species_type Tipo de especie para validación
#' @return Lista con resultados de validación
#' @export
validate_mortality_reproduction_params <- function(mortality_params, reproduction_params = NULL,
                                                   species_type = "general") {
  
  validation <- list(
    valid = TRUE,
    warnings = character(),
    errors = character()
  )
  
  # Rangos típicos por tipo de especie (mortalidad diaria)
  typical_mortality_range <- switch(species_type,
                                    "small_fish" = c(0.0001, 0.01),     # 0.01% - 1% diaria
                                    "large_fish" = c(0.00001, 0.005),   # 0.001% - 0.5% diaria
                                    "general" = c(0.00001, 0.01)        # 0.001% - 1% diaria
  )
  
  # Validar parámetros de mortalidad
  if (!is.null(mortality_params)) {
    
    # Validar mortalidad base
    if ("base_mortality" %in% names(mortality_params)) {
      base_mort <- mortality_params$base_mortality
      if (base_mort < 0 || base_mort > 1) {
        validation$errors <- c(validation$errors, "base_mortality debe estar entre 0 y 1")
        validation$valid <- FALSE
      }
      
      if (base_mort < typical_mortality_range[1] || base_mort > typical_mortality_range[2]) {
        validation$warnings <- c(validation$warnings, 
                                 paste("base_mortality fuera de rango típico para", species_type))
      }
    }
    
    # Validar otros tipos de mortalidad
    mortality_types <- c("natural_mortality", "fishing_mortality", "predation_mortality")
    for (mort_type in mortality_types) {
      if (mort_type %in% names(mortality_params)) {
        mort_value <- mortality_params[[mort_type]]
        if (mort_value < 0 || mort_value > 1) {
          validation$errors <- c(validation$errors, 
                                 paste(mort_type, "debe estar entre 0 y 1"))
          validation$valid <- FALSE
        }
      }
    }
    
    # Validar parámetros de temperatura
    if ("optimal_temp" %in% names(mortality_params)) {
      opt_temp <- mortality_params$optimal_temp
      if (opt_temp < -5 || opt_temp > 40) {
        validation$warnings <- c(validation$warnings, 
                                 "optimal_temp fuera de rango típico (-5 a 40°C)")
      }
    }
    
    if ("thermal_tolerance" %in% names(mortality_params)) {
      tolerance <- mortality_params$thermal_tolerance
      if (tolerance <= 0) {
        validation$errors <- c(validation$errors, "thermal_tolerance debe ser positivo")
        validation$valid <- FALSE
      }
    }
  }
  
  # Validar parámetros de reproducción
  if (!is.null(reproduction_params)) {
    
    if ("spawn_pattern" %in% names(reproduction_params)) {
      spawn_pattern <- reproduction_params$spawn_pattern
      
      if (any(spawn_pattern < 0 | spawn_pattern > 1, na.rm = TRUE)) {
        validation$errors <- c(validation$errors, 
                               "spawn_pattern debe estar entre 0 y 1")
        validation$valid <- FALSE
      }
      
      # Verificar reproducción anual total
      total_annual_spawn <- sum(spawn_pattern, na.rm = TRUE)
      if (total_annual_spawn > 0.5) {
        validation$warnings <- c(validation$warnings, 
                                 "Reproducción anual total muy alta (>50% del peso)")
      }
    }
    
    if ("energy_density" %in% names(reproduction_params)) {
      energy_density <- reproduction_params$energy_density
      if (energy_density <= 0) {
        validation$errors <- c(validation$errors, "energy_density debe ser positivo")
        validation$valid <- FALSE
      }
      
      if (energy_density < 1000 || energy_density > 20000) {
        validation$warnings <- c(validation$warnings, 
                                 "energy_density fuera de rango típico (1000-20000 J/g)")
      }
    }
  }
  
  return(validation)
}

