#' Funciones de Mortalidad y Reproducción para el Modelo FB4
#'
#' @name mortality-reproduction
#' @aliases mortality-reproduction
NULL

# ============================================================================
# FUNCIONES DE MORTALIDAD
# ============================================================================

#' Procesar datos de mortalidad para el modelo FB4
#'
#' Convierte datos de mortalidad en tasas de supervivencia diarias y calcula
#' dinámicas poblacionales considerando múltiples fuentes de mortalidad
#'
#' @param mortality_data Data frame con datos de mortalidad por día y tipo
#' @param first_day Primer día de simulación
#' @param last_day Último día de simulación
#' @param initial_population Población inicial
#' @return Lista con datos de mortalidad procesados
#' @export
#' @examples
#' # Crear datos de mortalidad
#' mort_data <- data.frame(
#'   Day = 1:365,
#'   natural = rep(0.001, 365),      # 0.1% mortalidad natural diaria
#'   fishing = c(rep(0, 100), rep(0.005, 50), rep(0, 215))  # Pesca estacional
#' )
#'
#' processed <- process_mortality_data(mort_data, 1, 365, 1000)
process_mortality_data <- function(mortality_data, first_day, last_day,
                                   initial_population = 1) {

  # Validar entrada
  if (!"Day" %in% names(mortality_data)) {
    stop("mortality_data debe tener columna 'Day'")
  }

  mortality_types <- setdiff(names(mortality_data), "Day")
  if (length(mortality_types) == 0) {
    stop("mortality_data debe tener al menos una columna de mortalidad")
  }

  # Interpolar datos para días de simulación
  sim_days <- first_day:last_day
  n_days <- length(sim_days)
  n_types <- length(mortality_types)

  # Matriz de tasas de mortalidad
  mortality_rates <- array(0, dim = c(n_days, n_types))
  colnames(mortality_rates) <- mortality_types

  for (i in seq_along(mortality_types)) {
    mortality_rates[, i] <- approx(
      x = mortality_data$Day,
      y = mortality_data[[mortality_types[i]]],
      xout = sim_days,
      method = "constant",
      rule = 2
    )$y
  }

  # Validar rangos de mortalidad
  if (any(mortality_rates < 0 | mortality_rates > 1)) {
    stop("Tasas de mortalidad deben estar entre 0 y 1")
  }

  # Calcular intervalos de mortalidad
  mortality_intervals <- calculate_mortality_intervals(mortality_rates, mortality_types)

  # Calcular supervivencia poblacional
  population_dynamics <- calculate_population_survival(
    mortality_rates, mortality_intervals, initial_population
  )

  return(list(
    days = sim_days,
    n_days = n_days,
    mortality_rates = mortality_rates,
    mortality_intervals = mortality_intervals,
    mortality_types = mortality_types,
    population_numbers = population_dynamics$population_numbers,
    survival_rates = population_dynamics$survival_rates,
    mortality_numbers = population_dynamics$mortality_numbers,
    initial_population = initial_population
  ))
}

#' Calcular intervalos de mortalidad
#'
#' Determina la duración de períodos con tasas de mortalidad constantes
#'
#' @param mortality_rates Matriz de tasas de mortalidad
#' @param mortality_types Nombres de tipos de mortalidad
#' @return Matriz con intervalos de mortalidad
#' @keywords internal
calculate_mortality_intervals <- function(mortality_rates, mortality_types) {

  n_days <- nrow(mortality_rates)
  n_types <- length(mortality_types)

  mortality_intervals <- array(1, dim = c(n_days, n_types))
  colnames(mortality_intervals) <- paste0(mortality_types, "_interval")

  for (j in seq_along(mortality_types)) {
    int_start <- 1
    int_dur <- 0

    for (i in 1:(n_days - 1)) {
      int_dur <- int_dur + 1

      # Si la tasa cambia o llegamos al final
      if (mortality_rates[i + 1, j] != mortality_rates[i, j]) {
        # Asignar duración al intervalo
        mortality_intervals[int_start:i, j] <- int_dur
        int_dur <- 0
        int_start <- i + 1
      }
    }

    # Manejar último intervalo
    int_dur <- int_dur + 1
    mortality_intervals[int_start:n_days, j] <- int_dur
  }

  return(mortality_intervals)
}

#' Calcular supervivencia poblacional
#'
#' Calcula la dinámica poblacional considerando múltiples fuentes de mortalidad
#'
#' @param mortality_rates Matriz de tasas de mortalidad
#' @param mortality_intervals Matriz de intervalos de mortalidad
#' @param initial_population Población inicial
#' @return Lista con dinámicas poblacionales
#' @keywords internal
calculate_population_survival <- function(mortality_rates, mortality_intervals,
                                          initial_population) {

  n_days <- nrow(mortality_rates)
  n_types <- ncol(mortality_rates)

  # Inicializar vectores
  population_numbers <- numeric(n_days)
  survival_rates <- numeric(n_days)
  mortality_numbers <- array(0, dim = c(n_days, n_types))
  colnames(mortality_numbers) <- paste0(colnames(mortality_rates), "_deaths")

  # Población inicial
  population_numbers[1] <- initial_population
  survival_rates[1] <- 1

  # Calcular dinámica diaria
  for (i in 2:n_days) {
    current_pop <- population_numbers[i - 1]

    # Calcular supervivencia combinada para el día
    combined_survival <- 1

    for (j in 1:n_types) {
      mort_rate <- mortality_rates[i - 1, j]
      interval <- mortality_intervals[i - 1, j]

      if (interval > 0 && mort_rate > 0) {
        # Supervivencia diaria = (1 - tasa_período)^(1/duración_período)
        daily_survival <- (1 - mort_rate)^(1/interval)
        combined_survival <- combined_survival * daily_survival

        # Calcular muertes por tipo
        type_mortality <- current_pop * (1 - daily_survival)
        mortality_numbers[i - 1, j] <- type_mortality
      }
    }

    # Actualizar población
    survival_rates[i] <- combined_survival
    population_numbers[i] <- current_pop * combined_survival
  }

  return(list(
    population_numbers = population_numbers,
    survival_rates = survival_rates,
    mortality_numbers = mortality_numbers
  ))
}

#' Calcular tasas de mortalidad instantánea
#'
#' Convierte tasas de mortalidad en tasas instantáneas (Z)
#'
#' @param mortality_rate Tasa de mortalidad (fracción que muere)
#' @param time_period Período de tiempo para la tasa
#' @return Tasa de mortalidad instantánea
#' @export
#' @examples
#' # Convertir 20% mortalidad anual a tasa instantánea
#' Z_annual <- calculate_instantaneous_mortality(0.20, 365)
#'
#' # Convertir a tasa diaria
#' Z_daily <- Z_annual / 365
calculate_instantaneous_mortality <- function(mortality_rate, time_period = 1) {

  if (mortality_rate < 0 || mortality_rate >= 1) {
    stop("mortality_rate debe estar entre 0 y 1")
  }

  if (time_period <= 0) {
    stop("time_period debe ser positivo")
  }

  # Z = -ln(1 - mortalidad) / tiempo
  Z <- -log(1 - mortality_rate) / time_period

  return(Z)
}

#' Convertir tasa instantánea a tasa de mortalidad
#'
#' Convierte tasa instantánea (Z) a fracción que muere
#'
#' @param Z Tasa de mortalidad instantánea
#' @param time_period Período de tiempo
#' @return Tasa de mortalidad (fracción)
#' @export
#' @examples
#' # Convertir Z = 0.5 año^-1 a mortalidad anual
#' annual_mortality <- instantaneous_to_mortality_rate(0.5, 365)
instantaneous_to_mortality_rate <- function(Z, time_period = 1) {

  if (Z < 0) {
    stop("Z debe ser no negativo")
  }

  if (time_period <= 0) {
    stop("time_period debe ser positivo")
  }

  # mortalidad = 1 - exp(-Z * tiempo)
  mortality_rate <- 1 - exp(-Z * time_period)

  return(mortality_rate)
}

# ============================================================================
# FUNCIONES DE REPRODUCCIÓN
# ============================================================================

#' Procesar datos de reproducción para el modelo FB4
#'
#' Procesa datos de eventos reproductivos y pérdida de peso por desove
#'
#' @param reproduction_data Data frame con datos de reproducción por día
#' @param first_day Primer día de simulación
#' @param last_day Último día de simulación
#' @return Vector con fracciones de peso perdido por día
#' @export
#' @examples
#' # Crear datos de reproducción
#' repro_data <- data.frame(
#'   Day = 1:365,
#'   Reproduction = c(rep(0, 120), rep(0.15, 5), rep(0, 240))  # Desove en primavera
#' )
#'
#' processed <- process_reproduction_data(repro_data, 1, 365)
process_reproduction_data <- function(reproduction_data, first_day, last_day) {

  # Validar entrada
  if (!"Day" %in% names(reproduction_data)) {
    stop("reproduction_data debe tener columna 'Day'")
  }

  # Detectar columna de reproducción
  repro_col <- intersect(c("Reproduction", "Spawning", "Spawn_Fraction"),
                         names(reproduction_data))
  if (length(repro_col) == 0) {
    stop("reproduction_data debe tener columna 'Reproduction', 'Spawning' o 'Spawn_Fraction'")
  }

  repro_col <- repro_col[1]  # Usar la primera encontrada

  # Interpolar para días de simulación
  sim_days <- first_day:last_day

  reproduction_fractions <- approx(
    x = reproduction_data$Day,
    y = reproduction_data[[repro_col]],
    xout = sim_days,
    method = "constant",
    rule = 2
  )$y

  # Validar rangos
  if (any(reproduction_fractions < 0 | reproduction_fractions > 1)) {
    warning("Fracciones de reproducción fuera de rango (0-1), ajustando")
    reproduction_fractions <- pmax(0, pmin(1, reproduction_fractions))
  }

  return(reproduction_fractions)
}

#' Calcular energía perdida por reproducción
#'
#' Calcula la energía perdida durante eventos reproductivos
#'
#' @param spawn_fraction Fracción de peso perdido en reproducción
#' @param current_weight Peso actual del pez (g)
#' @param energy_density Densidad energética del pez (J/g)
#' @param gonad_energy_density Densidad energética de gónadas (J/g, opcional)
#' @return Energía perdida en reproducción (J)
#' @export
#' @examples
#' # Calcular pérdida energética por desove
#' energy_loss <- calculate_reproductive_energy_loss(
#'   spawn_fraction = 0.12,
#'   current_weight = 150,
#'   energy_density = 5200
#' )
calculate_reproductive_energy_loss <- function(spawn_fraction, current_weight,
                                               energy_density,
                                               gonad_energy_density = NULL) {

  if (spawn_fraction < 0 || spawn_fraction > 1) {
    stop("spawn_fraction debe estar entre 0 y 1")
  }

  if (current_weight <= 0) {
    stop("current_weight debe ser positivo")
  }

  if (energy_density <= 0) {
    stop("energy_density debe ser positivo")
  }

  # Peso perdido en reproducción
  weight_loss <- spawn_fraction * current_weight

  # Densidad energética de material reproductivo
  if (is.null(gonad_energy_density)) {
    # Usar densidad energética del cuerpo si no se especifica
    reproductive_energy_density <- energy_density
  } else {
    reproductive_energy_density <- gonad_energy_density
  }

  # Energía perdida
  energy_loss <- weight_loss * reproductive_energy_density

  return(energy_loss)
}

#' Modelar ciclo reproductivo estacional
#'
#' Genera un patrón estacional de reproducción
#'
#' @param days Vector de días del año (1-365)
#' @param peak_day Día del pico reproductivo
#' @param duration Duración del período reproductivo (días)
#' @param max_spawn_fraction Fracción máxima de peso perdido
#' @param spawn_pattern Patrón de desove ("single", "multiple", "extended")
#' @return Vector con fracciones de reproducción por día
#' @export
#' @examples
#' # Ciclo reproductivo primaveral
#' spring_spawn <- model_seasonal_reproduction(
#'   days = 1:365,
#'   peak_day = 120,  # Finales de abril
#'   duration = 30,
#'   max_spawn_fraction = 0.15,
#'   spawn_pattern = "single"
#' )
#'
#' # Múltiples eventos de desove
#' multiple_spawn <- model_seasonal_reproduction(
#'   days = 1:365,
#'   peak_day = 150,
#'   duration = 60,
#'   max_spawn_fraction = 0.08,
#'   spawn_pattern = "multiple"
#' )
model_seasonal_reproduction <- function(days, peak_day, duration,
                                        max_spawn_fraction = 0.15,
                                        spawn_pattern = "single") {

  n_days <- length(days)
  spawn_fractions <- rep(0, n_days)

  if (spawn_pattern == "single") {
    # Un solo evento de desove
    for (i in seq_along(days)) {
      day <- days[i]

      # Distancia al pico (considerando circularidad del año)
      dist_to_peak <- min(abs(day - peak_day), 365 - abs(day - peak_day))

      if (dist_to_peak <= duration/2) {
        # Función gaussiana para el desove
        spawn_fractions[i] <- max_spawn_fraction *
          exp(-0.5 * (dist_to_peak / (duration/4))^2)
      }
    }

  } else if (spawn_pattern == "multiple") {
    # Múltiples eventos de desove
    n_events <- 3
    event_spacing <- duration / n_events

    for (event in 1:n_events) {
      event_day <- peak_day + (event - 2) * event_spacing
      event_duration <- duration / (n_events * 1.5)
      event_fraction <- max_spawn_fraction / n_events

      for (i in seq_along(days)) {
        day <- days[i]
        dist_to_event <- min(abs(day - event_day), 365 - abs(day - event_day))

        if (dist_to_event <= event_duration/2) {
          spawn_fractions[i] <- spawn_fractions[i] + event_fraction *
            exp(-0.5 * (dist_to_event / (event_duration/4))^2)
        }
      }
    }

  } else if (spawn_pattern == "extended") {
    # Desove extendido con múltiples pulsos
    start_day <- peak_day - duration/2
    end_day <- peak_day + duration/2

    for (i in seq_along(days)) {
      day <- days[i]

      # Verificar si está en período reproductivo
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
        # Intensidad variable durante la temporada
        season_progress <- abs(day - peak_day) / (duration/2)
        base_intensity <- max_spawn_fraction * (1 - season_progress^2)

        # Agregar variabilidad con múltiples pulsos
        pulse_frequency <- 2 * pi * 4 / duration  # 4 pulsos por temporada
        pulse_modifier <- 1 + 0.3 * sin(pulse_frequency * (day - start_day))

        spawn_fractions[i] <- base_intensity * pulse_modifier
      }
    }
  }

  # Asegurar que no exceda límites
  spawn_fractions <- pmax(0, pmin(1, spawn_fractions))

  return(spawn_fractions)
}

#' Calcular GSI (Gonadosomatic Index) desde fracción de desove
#'
#' Convierte fracciones de desove a índice gonadosomático teórico
#'
#' @param spawn_fraction Fracción de peso perdido en desove
#' @param gsi_max Índice gonadosomático máximo antes del desove
#' @param days_to_develop Días necesarios para desarrollar gónadas
#' @return Vector con GSI estimado
#' @export
#' @examples
#' # Calcular GSI desde patrón de desove
#' spawn_pattern <- c(rep(0, 100), 0.15, rep(0, 264))
#' gsi_values <- calculate_gsi_from_spawn(spawn_pattern, gsi_max = 12)
calculate_gsi_from_spawn <- function(spawn_fraction, gsi_max = 10,
                                     days_to_develop = 60) {

  n_days <- length(spawn_fraction)
  gsi <- rep(0, n_days)

  # Encontrar días de desove
  spawn_days <- which(spawn_fraction > 0)

  for (spawn_day in spawn_days) {
    # Calcular GSI máximo para este evento
    max_gsi_event <- gsi_max * spawn_fraction[spawn_day] / max(spawn_fraction)

    # Desarrollo pre-desove
    dev_start <- max(1, spawn_day - days_to_develop)
    dev_days <- (dev_start:(spawn_day-1)) - dev_start + 1

    if (length(dev_days) > 0) {
      # Desarrollo sigmoidal
      sigmoid_curve <- max_gsi_event / (1 + exp(-0.1 * (dev_days - days_to_develop/2)))
      gsi[dev_start:(spawn_day-1)] <- pmax(gsi[dev_start:(spawn_day-1)], sigmoid_curve)
    }

    # Desove (caída abrupta)
    gsi[spawn_day] <- 0

    # Recuperación post-desove (opcional)
    recovery_days <- min(30, n_days - spawn_day)
    if (recovery_days > 0) {
      recovery_curve <- max_gsi_event * 0.1 * exp(-0.1 * (1:recovery_days))
      end_recovery <- min(spawn_day + recovery_days, n_days)
      gsi[(spawn_day+1):end_recovery] <- pmax(gsi[(spawn_day+1):end_recovery],
                                              recovery_curve[1:(end_recovery-spawn_day)])
    }
  }

  return(gsi)
}

# ============================================================================
# FUNCIONES DE ANÁLISIS POBLACIONAL
# ============================================================================

#' Calcular parámetros poblacionales
#'
#' Calcula métricas poblacionales básicas a partir de datos de mortalidad
#'
#' @param mortality_data Output de process_mortality_data
#' @return Lista con parámetros poblacionales
#' @export
calculate_population_parameters <- function(mortality_data) {

  n_days <- mortality_data$n_days
  pop_numbers <- mortality_data$population_numbers

  # Supervivencia total
  total_survival <- pop_numbers[n_days] / pop_numbers[1]

  # Mortalidad total
  total_mortality <- 1 - total_survival

  # Tasa de mortalidad instantánea promedio
  if (total_survival > 0) {
    average_Z <- -log(total_survival) / n_days
  } else {
    average_Z <- Inf
  }

  # Vida media (días hasta que población se reduce a la mitad)
  if (average_Z > 0) {
    half_life <- log(2) / average_Z
  } else {
    half_life <- Inf
  }

  # Tiempo de duplicación poblacional (si hubiera crecimiento)
  doubling_time <- log(2) / abs(average_Z)

  # Calcular mortalidad por tipo
  mortality_by_type <- colSums(mortality_data$mortality_numbers, na.rm = TRUE)
  total_deaths <- sum(mortality_by_type)

  if (total_deaths > 0) {
    mortality_proportions <- mortality_by_type / total_deaths
  } else {
    mortality_proportions <- rep(0, length(mortality_by_type))
    names(mortality_proportions) <- names(mortality_by_type)
  }

  return(list(
    simulation_days = n_days,
    initial_population = mortality_data$initial_population,
    final_population = pop_numbers[n_days],
    total_survival = total_survival,
    total_mortality = total_mortality,
    average_daily_mortality = total_mortality / n_days,
    average_Z = average_Z,
    half_life_days = half_life,
    doubling_time_days = doubling_time,
    total_deaths = total_deaths,
    mortality_by_type = mortality_by_type,
    mortality_proportions = mortality_proportions
  ))
}

#' Proyectar población a largo plazo
#'
#' Proyecta dinámica poblacional usando tasas promedio
#'
#' @param initial_population Población inicial
#' @param mortality_rates Vector de tasas de mortalidad promedio por tipo
#' @param recruitment_rate Tasa de reclutamiento (opcional)
#' @param projection_days Días a proyectar
#' @return Data frame con proyección poblacional
#' @export
project_population_longterm <- function(initial_population, mortality_rates,
                                        recruitment_rate = 0, projection_days = 365) {

  # Calcular tasa de mortalidad combinada
  combined_mortality <- 1 - prod(1 - mortality_rates)
  combined_survival <- 1 - combined_mortality

  # Tasa neta (supervivencia + reclutamiento - 1)
  net_rate <- combined_survival + recruitment_rate - 1

  # Proyección
  days <- 1:projection_days
  population <- numeric(projection_days)
  population[1] <- initial_population

  for (i in 2:projection_days) {
    population[i] <- population[i-1] * (1 + net_rate)

    # Evitar poblaciones negativas
    if (population[i] < 0) {
      population[i:projection_days] <- 0
      break
    }
  }

  # Calcular tasas de cambio
  growth_rate <- diff(population)
  percent_change <- c(0, (growth_rate / population[-projection_days]) * 100)

  return(data.frame(
    Day = days,
    Population = population,
    Daily_Change = c(0, growth_rate),
    Percent_Change = percent_change
  ))
}

#' Calcular tablas de vida
#'
#' Genera tablas de vida estándar para análisis demográfico
#'
#' @param mortality_data Output de process_mortality_data
#' @param age_classes Vector de clases de edad (opcional)
#' @return Data frame con tabla de vida
#' @export
#' @examples
#' # Crear datos de mortalidad
#' mort_data <- data.frame(
#'   Day = 1:365,
#'   natural = rep(0.002, 365)
#' )
#' processed <- process_mortality_data(mort_data, 1, 365, 1000)
#' life_table <- calculate_life_table(processed)
calculate_life_table <- function(mortality_data, age_classes = NULL) {

  n_days <- mortality_data$n_days

  if (is.null(age_classes)) {
    # Crear clases de edad por defecto (mensual)
    age_classes <- seq(0, n_days, by = 30)
    if (tail(age_classes, 1) < n_days) {
      age_classes <- c(age_classes, n_days)
    }
  }

  n_classes <- length(age_classes) - 1

  # Inicializar tabla de vida
  life_table <- data.frame(
    age_class = 1:n_classes,
    age_start = age_classes[-length(age_classes)],
    age_end = age_classes[-1],
    lx = numeric(n_classes),        # Supervivientes al inicio
    dx = numeric(n_classes),        # Muertes en el intervalo
    qx = numeric(n_classes),        # Probabilidad de muerte
    px = numeric(n_classes),        # Probabilidad de supervivencia
    ex = numeric(n_classes)         # Expectativa de vida
  )

  # Calcular supervivientes y muertes por clase
  for (i in 1:n_classes) {
    start_day <- max(1, age_classes[i] + 1)
    end_day <- min(n_days, age_classes[i + 1])

    if (start_day <= end_day) {
      life_table$lx[i] <- mortality_data$population_numbers[start_day]

      if (i < n_classes) {
        next_start <- max(1, age_classes[i + 1] + 1)
        if (next_start <= n_days) {
          life_table$dx[i] <- life_table$lx[i] - mortality_data$population_numbers[next_start]
        } else {
          life_table$dx[i] <- life_table$lx[i]
        }
      } else {
        life_table$dx[i] <- life_table$lx[i]
      }

      # Probabilidades
      if (life_table$lx[i] > 0) {
        life_table$qx[i] <- life_table$dx[i] / life_table$lx[i]
        life_table$px[i] <- 1 - life_table$qx[i]
      }
    }
  }

  # Calcular expectativa de vida
  for (i in 1:n_classes) {
    if (life_table$lx[i] > 0) {
      future_life <- sum(life_table$lx[i:n_classes] * (age_classes[2:(n_classes-i+2)] - age_classes[1:(n_classes-i+1)]))
      life_table$ex[i] <- future_life / life_table$lx[i]
    }
  }

  return(life_table)
}

# ============================================================================
# FUNCIONES DE VALIDACIÓN
# ============================================================================

#' Validar parámetros de mortalidad y reproducción
#'
#' Verifica que los parámetros estén en rangos realistas
#'
#' @param mortality_rates Vector o matriz de tasas de mortalidad
#' @param reproduction_fractions Vector de fracciones reproductivas
#' @param species_type Tipo de especie ("small_fish", "large_fish", "general")
#' @return Lista con validación
#' @export
validate_mortality_reproduction_params <- function(mortality_rates = NULL,
                                                   reproduction_fractions = NULL,
                                                   species_type = "general") {

  warnings <- character()
  errors <- character()
  valid <- TRUE

  # Rangos típicos por tipo de especie (mortalidad diaria)
  mortality_ranges <- list(
    small_fish = c(0.0001, 0.01),    # 0.01% - 1% diaria
    large_fish = c(0.00001, 0.005),  # 0.001% - 0.5% diaria
    general = c(0.00001, 0.01)       # 0.001% - 1% diaria
  )

  # Rangos de reproducción (fracción de peso perdido)
  reproduction_ranges <- list(
    small_fish = c(0.05, 0.25),      # 5% - 25%
    large_fish = c(0.10, 0.20),      # 10% - 20%
    general = c(0.05, 0.30)          # 5% - 30%
  )

  if (!species_type %in% names(mortality_ranges)) {
    warnings <- c(warnings, paste("Tipo de especie no reconocido:", species_type))
    species_type <- "general"
  }

  # Validar mortalidad
  if (!is.null(mortality_rates)) {
    if (any(mortality_rates < 0 | mortality_rates > 1)) {
      errors <- c(errors, "Tasas de mortalidad deben estar entre 0 y 1")
      valid <- FALSE
    }

    range_mort <- mortality_ranges[[species_type]]
    if (any(mortality_rates < range_mort[1] | mortality_rates > range_mort[2])) {
      warnings <- c(warnings,
                    paste("Tasas de mortalidad fuera de rango típico para", species_type))
    }

    # Verificar que la mortalidad combinada no sea excesiva
    if (is.matrix(mortality_rates)) {
      combined_mortality <- apply(mortality_rates, 1, function(x) 1 - prod(1 - x))
    } else {
      combined_mortality <- 1 - prod(1 - mortality_rates)
    }

    if (any(combined_mortality > 0.1)) {
      warnings <- c(warnings, "Mortalidad combinada muy alta (>10% diaria)")
    }
  }

  # Validar reproducción
  if (!is.null(reproduction_fractions)) {
    if (any(reproduction_fractions < 0 | reproduction_fractions > 1)) {
      errors <- c(errors, "Fracciones reproductivas deben estar entre 0 y 1")
      valid <- FALSE
    }

    range_repro <- reproduction_ranges[[species_type]]
    max_repro <- max(reproduction_fractions)
    if (max_repro > 0 && (max_repro < range_repro[1] || max_repro > range_repro[2])) {
      warnings <- c(warnings,
                    paste("Fracción reproductiva máxima fuera de rango típico para", species_type))
    }

    # Verificar que no haya reproducción excesiva
    total_annual_reproduction <- sum(reproduction_fractions)
    if (total_annual_reproduction > 0.5) {
      warnings <- c(warnings, "Reproducción anual total muy alta (>50% del peso)")
    }
  }

  return(list(
    valid = valid,
    warnings = warnings,
    errors = errors,
    n_warnings = length(warnings),
    n_errors = length(errors),
    species_type = species_type,
    mortality_range = if(!is.null(mortality_rates)) mortality_ranges[[species_type]] else NULL,
    reproduction_range = if(!is.null(reproduction_fractions)) reproduction_ranges[[species_type]] else NULL
  ))
}

# ============================================================================
# FUNCIONES DE INTEGRACIÓN CON MODELO BIOENERGÉTICO
# ============================================================================

#' Aplicar mortalidad al modelo bioenergético
#'
#' Integra mortalidad dependiente del peso en el modelo FB4
#'
#' @param current_weight Peso actual del pez (g)
#' @param base_mortality_rate Tasa base de mortalidad diaria
#' @param weight_effect_slope Pendiente del efecto del peso en mortalidad
#' @param critical_weight Peso crítico por debajo del cual aumenta mortalidad
#' @param starvation_threshold Umbral de inanición (fracción del peso inicial)
#' @param initial_weight Peso inicial para comparación
#' @return Tasa de mortalidad ajustada por peso
#' @export
#' @examples
#' # Calcular mortalidad dependiente del peso
#' mortality_rate <- apply_weight_dependent_mortality(
#'   current_weight = 80,
#'   base_mortality_rate = 0.001,
#'   weight_effect_slope = -0.0001,
#'   critical_weight = 50,
#'   starvation_threshold = 0.3,
#'   initial_weight = 100
#' )
apply_weight_dependent_mortality <- function(current_weight,
                                             base_mortality_rate = 0.001,
                                             weight_effect_slope = 0,
                                             critical_weight = 0,
                                             starvation_threshold = 0.2,
                                             initial_weight = NULL) {

  mortality_rate <- base_mortality_rate

  # Efecto del peso absoluto
  if (weight_effect_slope != 0) {
    weight_effect <- weight_effect_slope * (current_weight - critical_weight)
    mortality_rate <- mortality_rate + weight_effect
  }

  # Mortalidad por inanición
  if (!is.null(initial_weight)) {
    weight_fraction <- current_weight / initial_weight

    if (weight_fraction < starvation_threshold) {
      # Mortalidad exponencial al acercarse a inanición
      starvation_effect <- exp(-10 * (weight_fraction - starvation_threshold))
      mortality_rate <- mortality_rate + 0.1 * starvation_effect
    }
  }

  # Mortalidad por peso crítico
  if (critical_weight > 0 && current_weight < critical_weight) {
    critical_effect <- exp(-0.1 * (current_weight / critical_weight))
    mortality_rate <- mortality_rate + 0.05 * critical_effect
  }

  # Asegurar que esté en rango válido
  mortality_rate <- pmax(0, pmin(0.99, mortality_rate))

  return(mortality_rate)
}

#' Calcular probabilidad de supervivencia diaria
#'
#' Calcula la probabilidad de que un pez sobreviva un día dado su estado
#'
#' @param current_weight Peso actual (g)
#' @param energy_reserves Reservas energéticas actuales (J)
#' @param temperature Temperatura del agua (°C)
#' @param base_survival Supervivencia base diaria
#' @param weight_dependence Dependencia del peso en supervivencia
#' @param energy_dependence Dependencia de energía en supervivencia
#' @param temp_optimum Temperatura óptima
#' @param temp_tolerance Tolerancia térmica
#' @return Probabilidad de supervivencia (0-1)
#' @export
calculate_daily_survival_probability <- function(current_weight,
                                                 energy_reserves = NULL,
                                                 temperature = 15,
                                                 base_survival = 0.999,
                                                 weight_dependence = 0.0001,
                                                 energy_dependence = 0.00001,
                                                 temp_optimum = 15,
                                                 temp_tolerance = 10) {

  survival_prob <- base_survival

  # Efecto del peso
  if (weight_dependence != 0) {
    weight_effect <- weight_dependence * log(current_weight)
    survival_prob <- survival_prob + weight_effect
  }

  # Efecto de las reservas energéticas
  if (!is.null(energy_reserves) && energy_dependence != 0) {
    energy_effect <- energy_dependence * log(pmax(1, energy_reserves))
    survival_prob <- survival_prob + energy_effect
  }

  # Efecto de la temperatura
  temp_diff <- abs(temperature - temp_optimum)
  if (temp_diff > temp_tolerance) {
    temp_stress <- (temp_diff - temp_tolerance) / temp_tolerance
    temp_effect <- -0.01 * temp_stress^2
    survival_prob <- survival_prob + temp_effect
  }

  # Asegurar que esté en rango válido
  survival_prob <- pmax(0.001, pmin(0.999, survival_prob))

  return(survival_prob)
}

#' Simular eventos de mortalidad estocásticos
#'
#' Simula eventos de mortalidad usando procesos estocásticos
#'
#' @param survival_probability Probabilidad de supervivencia diaria
#' @param n_individuals Número de individuos en la simulación
#' @param mortality_type Tipo de mortalidad ("binomial", "poisson")
#' @param random_seed Semilla para reproducibilidad
#' @return Lista con resultados de mortalidad
#' @export
#' @examples
#' # Simular mortalidad estocástica
#' mortality_event <- simulate_stochastic_mortality(
#'   survival_probability = 0.995,
#'   n_individuals = 1000,
#'   mortality_type = "binomial"
#' )
simulate_stochastic_mortality <- function(survival_probability,
                                          n_individuals = 1,
                                          mortality_type = "binomial",
                                          random_seed = NULL) {

  if (!is.null(random_seed)) {
    set.seed(random_seed)
  }

  mortality_probability <- 1 - survival_probability

  if (mortality_type == "binomial") {
    # Distribución binomial
    deaths <- rbinom(1, n_individuals, mortality_probability)
    survivors <- n_individuals - deaths

  } else if (mortality_type == "poisson") {
    # Distribución de Poisson (para eventos raros)
    expected_deaths <- n_individuals * mortality_probability
    deaths <- rpois(1, expected_deaths)
    survivors <- max(0, n_individuals - deaths)

  } else {
    stop("mortality_type debe ser 'binomial' o 'poisson'")
  }

  return(list(
    initial_population = n_individuals,
    deaths = deaths,
    survivors = survivors,
    mortality_rate = deaths / n_individuals,
    survival_rate = survivors / n_individuals
  ))
}

# ============================================================================
# FUNCIONES DE VISUALIZACIÓN
# ============================================================================

#' Graficar dinámica de mortalidad y población
#'
#' Crea gráficos de mortalidad y cambios poblacionales
#'
#' @param mortality_data Output de process_mortality_data
#' @param plot_type Tipo de gráfico ("population", "mortality", "both")
#' @param save_plot Guardar gráfico en archivo
#' @param filename Nombre del archivo (si save_plot = TRUE)
#' @return Gráfico de dinámica poblacional
#' @export
plot_mortality_dynamics <- function(mortality_data, plot_type = "both",
                                    save_plot = FALSE, filename = "mortality_dynamics.png") {

  days <- mortality_data$days

  if (save_plot) {
    png(filename, width = 1200, height = 800, res = 150)
  }

  if (plot_type %in% c("population", "both")) {
    if (plot_type == "both") par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))

    # Gráfico de población
    plot(days, mortality_data$population_numbers,
         type = "l", lwd = 2, col = "blue",
         xlab = "Día", ylab = "Número de Individuos",
         main = "Dinámica Poblacional")
    grid()

    # Agregar información final
    final_pop <- tail(mortality_data$population_numbers, 1)
    initial_pop <- mortality_data$initial_population
    survival_percent <- (final_pop / initial_pop) * 100

    text(max(days) * 0.7, final_pop * 1.2,
         paste("Supervivencia:", round(survival_percent, 1), "%"),
         col = "blue", cex = 1.2)
  }

  if (plot_type %in% c("mortality", "both")) {
    # Gráfico de tasas de mortalidad
    mort_rates <- mortality_data$mortality_rates
    colors <- rainbow(ncol(mort_rates))

    plot(days, mort_rates[, 1], type = "l", lwd = 2, col = colors[1],
         xlab = "Día", ylab = "Tasa de Mortalidad",
         main = "Tasas de Mortalidad por Tipo",
         ylim = range(mort_rates))

    if (ncol(mort_rates) > 1) {
      for (i in 2:ncol(mort_rates)) {
        lines(days, mort_rates[, i], lwd = 2, col = colors[i])
      }
    }

    legend("topright", legend = mortality_data$mortality_types,
           col = colors, lwd = 2, cex = 0.8)
    grid()
  }

  if (plot_type == "both") par(mfrow = c(1, 1))

  if (save_plot) {
    dev.off()
    cat("Gráfico guardado como:", filename, "\n")
  }
}

#' Graficar patrones reproductivos
#'
#' Visualiza patrones estacionales de reproducción
#'
#' @param reproduction_data Vector de fracciones reproductivas o data frame
#' @param days Vector de días (opcional)
#' @param add_gsi Agregar gráfico de GSI estimado
#' @param save_plot Guardar gráfico en archivo
#' @param filename Nombre del archivo (si save_plot = TRUE)
#' @return Gráfico de patrón reproductivo
#' @export
plot_reproduction_pattern <- function(reproduction_data, days = NULL,
                                      add_gsi = FALSE, save_plot = FALSE,
                                      filename = "reproduction_pattern.png") {

  if (is.data.frame(reproduction_data)) {
    days <- reproduction_data$Day
    repro_fractions <- reproduction_data[[2]]  # Asumir 2da columna
  } else {
    repro_fractions <- reproduction_data
    if (is.null(days)) {
      days <- 1:length(repro_fractions)
    }
  }

  if (save_plot) {
    png(filename, width = 1200, height = 600, res = 150)
  }

  # Configurar layout si se incluye GSI
  if (add_gsi) {
    par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))
  }

  # Identificar eventos reproductivos
  spawn_events <- which(repro_fractions > 0)

  # Gráfico principal de reproducción
  plot(days, repro_fractions * 100,
       type = "l", lwd = 2, col = "red",
       xlab = "Día del Año", ylab = "Fracción de Desove (%)",
       main = "Patrón Reproductivo Estacional")

  # Resaltar eventos de desove
  if (length(spawn_events) > 0) {
    points(days[spawn_events], repro_fractions[spawn_events] * 100,
           pch = 19, col = "darkred", cex = 1.2)
  }

  # Agregar líneas de referencia estacionales
  abline(v = c(80, 172, 266, 355), col = "gray", lty = 2)  # Equinoccios y solsticios
  text(c(80, 172, 266, 355), rep(max(repro_fractions) * 90, 4),
       c("Primavera", "Verano", "Otoño", "Invierno"),
       col = "gray", cex = 0.8)

  grid()

  # Agregar estadísticas
  total_spawn <- sum(repro_fractions) * 100
  peak_spawn <- max(repro_fractions) * 100
  n_events <- length(spawn_events)

  legend("topright",
         legend = c(paste("Total anual:", round(total_spawn, 1), "%"),
                    paste("Pico máximo:", round(peak_spawn, 1), "%"),
                    paste("Eventos:", n_events)),
         bty = "n", cex = 0.9)

  # Gráfico de GSI si se solicita
  if (add_gsi) {
    gsi_values <- calculate_gsi_from_spawn(repro_fractions)

    plot(days, gsi_values,
         type = "l", lwd = 2, col = "orange",
         xlab = "Día del Año", ylab = "GSI Estimado (%)",
         main = "Índice Gonadosomático Estimado")

    # Resaltar picos de GSI
    gsi_peaks <- which(diff(sign(diff(gsi_values))) == -2) + 1
    if (length(gsi_peaks) > 0) {
      points(days[gsi_peaks], gsi_values[gsi_peaks],
             pch = 17, col = "darkorange", cex = 1.2)
    }

    grid()
    par(mfrow = c(1, 1))
  }

  if (save_plot) {
    dev.off()
    cat("Gráfico guardado como:", filename, "\n")
  }
}

#' Graficar tabla de vida
#'
#' Visualiza curvas de supervivencia y mortalidad
#'
#' @param life_table Output de calculate_life_table
#' @param plot_type Tipo de gráfico ("survival", "mortality", "both")
#' @param save_plot Guardar gráfico en archivo
#' @param filename Nombre del archivo (si save_plot = TRUE)
#' @return Gráfico de tabla de vida
#' @export
plot_life_table <- function(life_table, plot_type = "both",
                            save_plot = FALSE, filename = "life_table.png") {

  if (save_plot) {
    png(filename, width = 1200, height = 600, res = 150)
  }

  if (plot_type == "both") {
    par(mfrow = c(1, 2))
  }

  # Normalizar supervivientes
  lx_norm <- life_table$lx / life_table$lx[1]

  if (plot_type %in% c("survival", "both")) {
    # Curva de supervivencia
    plot(life_table$age_start, lx_norm,
         type = "l", lwd = 3, col = "blue",
         xlab = "Edad (días)", ylab = "Proporción de Supervivientes",
         main = "Curva de Supervivencia",
         ylim = c(0, 1))

    # Agregar puntos
    points(life_table$age_start, lx_norm, pch = 19, col = "blue", cex = 0.8)

    # Línea de 50% supervivencia
    abline(h = 0.5, col = "red", lty = 2)

    # Calcular y mostrar edad de 50% supervivencia
    age_50 <- approx(lx_norm, life_table$age_start, xout = 0.5)$y
    if (!is.na(age_50)) {
      abline(v = age_50, col = "red", lty = 2)
      text(age_50, 0.6, paste("50% supervivencia\n", round(age_50), "días"),
           col = "red", pos = 4)
    }

    grid()
  }

  if (plot_type %in% c("mortality", "both")) {
    # Tasas de mortalidad
    plot(life_table$age_start, life_table$qx,
         type = "b", lwd = 2, col = "red", pch = 19,
         xlab = "Edad (días)", ylab = "Tasa de Mortalidad (qx)",
         main = "Tasas de Mortalidad por Edad")

    grid()

    # Agregar estadísticas
    mean_qx <- mean(life_table$qx, na.rm = TRUE)
    abline(h = mean_qx, col = "darkred", lty = 2)
    text(max(life_table$age_start) * 0.7, mean_qx * 1.1,
         paste("Mortalidad promedio:", round(mean_qx, 4)),
         col = "darkred")
  }

  if (plot_type == "both") {
    par(mfrow = c(1, 1))
  }

  if (save_plot) {
    dev.off()
    cat("Gráfico guardado como:", filename, "\n")
  }
}

# ============================================================================
# FUNCIONES AUXILIARES Y UTILIDADES
# ============================================================================

#' Convertir entre unidades de tiempo para mortalidad
#'
#' Convierte tasas de mortalidad entre diferentes escalas temporales
#'
#' @param mortality_rate Tasa de mortalidad
#' @param from_period Período original ("day", "week", "month", "year")
#' @param to_period Período objetivo ("day", "week", "month", "year")
#' @param method Método de conversión ("instantaneous", "compound")
#' @return Tasa de mortalidad convertida
#' @export
#' @examples
#' # Convertir mortalidad anual a diaria
#' daily_mort <- convert_mortality_units(0.2, "year", "day")
#'
#' # Convertir mortalidad diaria a mensual
#' monthly_mort <- convert_mortality_units(0.001, "day", "month")
convert_mortality_units <- function(mortality_rate, from_period, to_period,
                                    method = "instantaneous") {

  # Días por período
  period_days <- list(
    day = 1,
    week = 7,
    month = 30.44,  # Promedio mensual
    year = 365.25
  )

  if (!from_period %in% names(period_days)) {
    stop("from_period debe ser 'day', 'week', 'month', o 'year'")
  }

  if (!to_period %in% names(period_days)) {
    stop("to_period debe ser 'day', 'week', 'month', o 'year'")
  }

  from_days <- period_days[[from_period]]
  to_days <- period_days[[to_period]]

  if (method == "instantaneous") {
    # Usar tasa instantánea
    Z <- -log(1 - mortality_rate) / from_days
    converted_rate <- 1 - exp(-Z * to_days)

  } else if (method == "compound") {
    # Método de interés compuesto
    daily_survival <- (1 - mortality_rate)^(1/from_days)
    converted_rate <- 1 - daily_survival^to_days

  } else {
    stop("method debe ser 'instantaneous' o 'compound'")
  }

  return(converted_rate)
}

#' Generar resumen de parámetros reproductivos
#'
#' Calcula estadísticas descriptivas de patrones reproductivos
#'
#' @param reproduction_fractions Vector de fracciones reproductivas
#' @param days Vector de días correspondientes
#' @return Lista con estadísticas reproductivas
#' @export
summarize_reproduction_parameters <- function(reproduction_fractions, days = NULL) {

  if (is.null(days)) {
    days <- 1:length(reproduction_fractions)
  }

  # Identificar eventos reproductivos
  spawn_events <- which(reproduction_fractions > 0)

  # Estadísticas básicas
  total_spawn <- sum(reproduction_fractions)
  peak_spawn <- max(reproduction_fractions)
  mean_spawn <- mean(reproduction_fractions[spawn_events])

  # Timing reproductivo
  if (length(spawn_events) > 0) {
    spawn_start <- min(days[spawn_events])
    spawn_end <- max(days[spawn_events])
    spawn_duration <- spawn_end - spawn_start + 1
    peak_day <- days[which.max(reproduction_fractions)]

    # Centroide del desove (centro de masa temporal)
    spawn_centroid <- weighted.mean(days[spawn_events],
                                    reproduction_fractions[spawn_events])
  } else {
    spawn_start <- NA
    spawn_end <- NA
    spawn_duration <- 0
    peak_day <- NA
    spawn_centroid <- NA
  }

  # Distribución temporal
  n_events <- length(spawn_events)
  if (n_events > 1) {
    spawn_intervals <- diff(days[spawn_events])
    mean_interval <- mean(spawn_intervals)
    spawn_frequency <- 365 / mean_interval
  } else {
    mean_interval <- NA
    spawn_frequency <- NA
  }

  return(list(
    total_annual_spawn = total_spawn,
    peak_spawn_fraction = peak_spawn,
    mean_spawn_fraction = mean_spawn,
    n_spawn_events = n_events,
    spawn_start_day = spawn_start,
    spawn_end_day = spawn_end,
    spawn_duration_days = spawn_duration,
    peak_spawn_day = peak_day,
    spawn_centroid_day = spawn_centroid,
    mean_interval_days = mean_interval,
    annual_spawn_frequency = spawn_frequency,
    spawn_season = classify_spawn_season(spawn_centroid)
  ))
}

#' Clasificar temporada de desove
#'
#' Clasifica la temporada reproductiva basada en el timing
#'
#' @param spawn_centroid Día centroide del desove
#' @return Temporada reproductiva
#' @keywords internal
classify_spawn_season <- function(spawn_centroid) {

  if (is.na(spawn_centroid)) {
    return("No reproductivo")
  }

  # Definir temporadas (hemisferio sur por defecto)
  if (spawn_centroid >= 355 || spawn_centroid <= 79) {
    return("Verano")
  } else if (spawn_centroid >= 80 && spawn_centroid <= 171) {
    return("Otoño")
  } else if (spawn_centroid >= 172 && spawn_centroid <= 265) {
    return("Invierno")
  } else {
    return("Primavera")
  }
}
