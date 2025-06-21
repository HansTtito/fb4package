#' Funciones de Procesamiento de Datos para FB4
#'
#' @name data-processing
#' @aliases data-processing
NULL

#' Procesar datos de entrada para el modelo FB4
#'
#' Interpola y procesa todos los datos de entrada necesarios para la simulación
#'
#' @param temperature_data Datos de temperatura
#' @param diet_data Datos de dieta
#' @param prey_energy_data Datos de energía de presas
#' @param first_day Primer día de simulación
#' @param last_day Último día de simulación
#' @param mortality_data Datos de mortalidad (opcional)
#' @param reproduction_data Datos de reproducción (opcional)
#' @param contaminant_data Datos de contaminantes (opcional)
#' @param nutrient_data Datos de nutrientes (opcional)
#' @param predator_energy_data Datos de energía del depredador (opcional)
#' @param indigestible_prey_data Datos de presas indigeribles (opcional)
#' @return Lista con datos procesados e interpolados
#' @export
process_input_data <- function(temperature_data,
                               diet_data,
                               prey_energy_data,
                               first_day,
                               last_day,
                               mortality_data = NULL,
                               reproduction_data = NULL,
                               contaminant_data = NULL,
                               nutrient_data = NULL,
                               predator_energy_data = NULL,
                               indigestible_prey_data = NULL) {

  # Crear secuencia de días para la simulación
  sim_days <- first_day:last_day
  n_days <- length(sim_days)

  # Procesar temperatura
  temperature <- interpolate_time_series(
    data = temperature_data,
    value_col = "Temperature",
    target_days = sim_days,
    method = "linear"
  )

  # Procesar datos de dieta
  prey_names <- setdiff(names(diet_data), "Day")
  n_prey <- length(prey_names)

  diet_props <- array(0, dim = c(n_days, n_prey))
  colnames(diet_props) <- prey_names

  for (i in seq_along(prey_names)) {
    diet_props[, i] <- interpolate_time_series(
      data = diet_data,
      value_col = prey_names[i],
      target_days = sim_days,
      method = "linear"
    )
  }

  # Procesar energías de presas
  prey_energies <- array(0, dim = c(n_days, n_prey))
  colnames(prey_energies) <- prey_names

  for (i in seq_along(prey_names)) {
    prey_energies[, i] <- interpolate_time_series(
      data = prey_energy_data,
      value_col = prey_names[i],
      target_days = sim_days,
      method = "linear"
    )
  }

  # Resultado base
  result <- list(
    days = sim_days,
    n_days = n_days,
    temperature = temperature,
    diet_proportions = diet_props,
    prey_energies = prey_energies,
    prey_names = prey_names,
    n_prey = n_prey
  )

  # Procesar datos opcionales
  if (!is.null(predator_energy_data)) {
    result$predator_energy <- interpolate_time_series(
      data = predator_energy_data,
      value_col = "Energy_Density",
      target_days = sim_days,
      method = "linear"
    )
  }

  if (!is.null(indigestible_prey_data)) {
    indigestible_props <- array(0, dim = c(n_days, n_prey))
    colnames(indigestible_props) <- prey_names

    for (i in seq_along(prey_names)) {
      if (prey_names[i] %in% names(indigestible_prey_data)) {
        indigestible_props[, i] <- interpolate_time_series(
          data = indigestible_prey_data,
          value_col = prey_names[i],
          target_days = sim_days,
          method = "constant"
        )
      }
    }
    result$indigestible_proportions <- indigestible_props
  }

  if (!is.null(mortality_data)) {
    result$mortality <- process_mortality_data(mortality_data, sim_days)
  }

  if (!is.null(reproduction_data)) {
    result$reproduction <- interpolate_time_series(
      data = reproduction_data,
      value_col = "Reproduction",
      target_days = sim_days,
      method = "constant"
    )
  }

  if (!is.null(contaminant_data)) {
    result$contaminant <- process_contaminant_data(contaminant_data, sim_days, prey_names)
  }

  if (!is.null(nutrient_data)) {
    result$nutrients <- process_nutrient_data(nutrient_data, sim_days, prey_names)
  }

  return(result)
}

#' Interpolar series de tiempo
#'
#' Función auxiliar para interpolar datos de series de tiempo
#'
#' @param data Data frame con datos
#' @param value_col Nombre de la columna con valores
#' @param target_days Vector de días objetivo
#' @param method Método de interpolación ("linear", "constant")
#' @return Vector interpolado
#' @keywords internal
interpolate_time_series <- function(data, value_col, target_days, method = "linear") {

  if (!"Day" %in% names(data)) {
    stop("Datos deben tener columna 'Day'")
  }

  if (!value_col %in% names(data)) {
    stop("Columna '", value_col, "' no encontrada en los datos")
  }

  # Obtener rango de días en los datos
  data_days <- data$Day
  values <- data[[value_col]]

  # Verificar que hay suficientes datos
  if (length(data_days) < 2) {
    warning("Datos insuficientes para interpolación, usando valor constante")
    return(rep(values[1], length(target_days)))
  }

  # Extender datos si es necesario
  min_target <- min(target_days)
  max_target <- max(target_days)

  if (min_target < min(data_days) || max_target > max(data_days)) {
    warning("Días objetivo fuera del rango de datos, extrapolando")
  }

  # Interpolar
  if (method == "linear") {
    interpolated <- approx(x = data_days, y = values,
                           xout = target_days, method = "linear",
                           rule = 2)$y
  } else if (method == "constant") {
    interpolated <- approx(x = data_days, y = values,
                           xout = target_days, method = "constant",
                           rule = 2)$y
  } else {
    stop("Método de interpolación no reconocido: ", method)
  }

  return(interpolated)
}

#' Procesar datos de mortalidad
#'
#' @param mortality_data Datos de mortalidad
#' @param sim_days Días de simulación
#' @return Lista con datos de mortalidad procesados
#' @keywords internal
process_mortality_data <- function(mortality_data, sim_days) {

  n_days <- length(sim_days)

  # Identificar tipos de mortalidad
  mort_types <- setdiff(names(mortality_data), "Day")
  n_mort_types <- length(mort_types)

  # Interpolar cada tipo de mortalidad
  mortality_rates <- array(0, dim = c(n_days, n_mort_types))
  colnames(mortality_rates) <- mort_types

  for (i in seq_along(mort_types)) {
    mortality_rates[, i] <- interpolate_time_series(
      data = mortality_data,
      value_col = mort_types[i],
      target_days = sim_days,
      method = "constant"
    )
  }

  # Calcular intervalos de mortalidad
  mortality_intervals <- calculate_mortality_intervals(mortality_rates, mort_types)

  # Calcular supervivencia poblacional
  population_survival <- calculate_population_survival(mortality_rates, mortality_intervals)

  return(list(
    rates = mortality_rates,
    intervals = mortality_intervals,
    population_survival = population_survival,
    types = mort_types
  ))
}

#' Calcular intervalos de mortalidad
#'
#' @param mortality_rates Tasas de mortalidad
#' @param mort_types Tipos de mortalidad
#' @return Array con intervalos de mortalidad
#' @keywords internal
calculate_mortality_intervals <- function(mortality_rates, mort_types) {

  n_days <- nrow(mortality_rates)
  n_types <- length(mort_types)

  mortality_intervals <- array(0, dim = c(n_days, n_types))
  colnames(mortality_intervals) <- paste0(mort_types, "_interval")

  for (j in seq_along(mort_types)) {
    int_start <- 1
    int_dur <- 0

    for (i in 1:(n_days - 1)) {
      int_dur <- ifelse(mortality_rates[i + 1, j] != mortality_rates[i, j],
                        int_dur + 1, int_dur + 1)

      if (mortality_rates[i + 1, j] != mortality_rates[i, j] || i == n_days - 1) {
        mortality_intervals[int_start:i, j] <- int_dur
        int_dur <- 0
        int_start <- i + 1
      }
    }

    # Último día
    mortality_intervals[n_days, j] <- max(1, mortality_intervals[n_days - 1, j])
  }

  return(mortality_intervals)
}

#' Calcular supervivencia poblacional
#'
#' @param mortality_rates Tasas de mortalidad
#' @param mortality_intervals Intervalos de mortalidad
#' @return Vector con supervivencia poblacional
#' @keywords internal
calculate_population_survival <- function(mortality_rates, mortality_intervals) {

  n_days <- nrow(mortality_rates)
  n_types <- ncol(mortality_rates)

  population_numbers <- numeric(n_days)
  population_numbers[1] <- 1  # Empezar con población normalizada

  for (i in 2:n_days) {
    # Calcular supervivencia combinada
    combined_survival <- 1

    for (j in 1:n_types) {
      mort_rate <- mortality_rates[i - 1, j]
      interval <- mortality_intervals[i - 1, j]

      if (interval > 0) {
        daily_survival <- (1 - mort_rate)^(1/interval)
        combined_survival <- combined_survival * daily_survival
      }
    }

    population_numbers[i] <- population_numbers[i - 1] * combined_survival
  }

  return(population_numbers)
}

#' Procesar datos de contaminantes
#'
#' @param contaminant_data Lista con datos de contaminantes
#' @param sim_days Días de simulación
#' @param prey_names Nombres de las presas
#' @return Lista con datos de contaminantes procesados
#' @keywords internal
process_contaminant_data <- function(contaminant_data, sim_days, prey_names) {

  n_days <- length(sim_days)
  n_prey <- length(prey_names)

  result <- list()

  # Procesar concentraciones en presas
  if ("prey_concentrations" %in% names(contaminant_data)) {
    prey_conc_data <- contaminant_data$prey_concentrations
    prey_concentrations <- array(0, dim = c(n_days, n_prey))
    colnames(prey_concentrations) <- prey_names

    for (i in seq_along(prey_names)) {
      if (prey_names[i] %in% names(prey_conc_data)) {
        prey_concentrations[, i] <- interpolate_time_series(
          data = prey_conc_data,
          value_col = prey_names[i],
          target_days = sim_days,
          method = "linear"
        )
      }
    }
    result$prey_concentrations <- prey_concentrations
  }

  # Procesar eficiencias de asimilación
  if ("assimilation_efficiency" %in% names(contaminant_data)) {
    assim_data <- contaminant_data$assimilation_efficiency
    assimilation_eff <- array(0, dim = c(n_days, n_prey))
    colnames(assimilation_eff) <- prey_names

    for (i in seq_along(prey_names)) {
      if (prey_names[i] %in% names(assim_data)) {
        assimilation_eff[, i] <- interpolate_time_series(
          data = assim_data,
          value_col = prey_names[i],
          target_days = sim_days,
          method = "constant"
        )
      }
    }
    result$assimilation_efficiency <- assimilation_eff
  }

  # Procesar eficiencias de transferencia
  if ("transfer_efficiency" %in% names(contaminant_data)) {
    transfer_data <- contaminant_data$transfer_efficiency
    transfer_eff <- array(0, dim = c(n_days, n_prey))
    colnames(transfer_eff) <- prey_names

    for (i in seq_along(prey_names)) {
      if (prey_names[i] %in% names(transfer_data)) {
        transfer_eff[, i] <- interpolate_time_series(
          data = transfer_data,
          value_col = prey_names[i],
          target_days = sim_days,
          method = "constant"
        )
      }
    }
    result$transfer_efficiency <- transfer_eff
  }

  return(result)
}

#' Procesar datos de nutrientes
#'
#' @param nutrient_data Lista con datos de nutrientes
#' @param sim_days Días de simulación
#' @param prey_names Nombres de las presas
#' @return Lista con datos de nutrientes procesados
#' @keywords internal
process_nutrient_data <- function(nutrient_data, sim_days, prey_names) {

  n_days <- length(sim_days)
  n_prey <- length(prey_names)

  result <- list()

  # Procesar datos de fósforo
  if ("phosphorus" %in% names(nutrient_data)) {
    phos_data <- nutrient_data$phosphorus

    # Eficiencia de asimilación de fósforo
    if ("assimilation_efficiency" %in% names(phos_data)) {
      phos_ae <- array(0, dim = c(n_days, n_prey))
      colnames(phos_ae) <- prey_names

      ae_data <- phos_data$assimilation_efficiency
      for (i in seq_along(prey_names)) {
        if (prey_names[i] %in% names(ae_data)) {
          phos_ae[, i] <- interpolate_time_series(
            data = ae_data,
            value_col = prey_names[i],
            target_days = sim_days,
            method = "linear"
          )
        }
      }
      result$phosphorus_ae <- phos_ae
    }

    # Concentración en presas
    if ("prey_concentration" %in% names(phos_data)) {
      phos_prey_conc <- array(0, dim = c(n_days, n_prey))
      colnames(phos_prey_conc) <- prey_names

      conc_data <- phos_data$prey_concentration
      for (i in seq_along(prey_names)) {
        if (prey_names[i] %in% names(conc_data)) {
          phos_prey_conc[, i] <- interpolate_time_series(
            data = conc_data,
            value_col = prey_names[i],
            target_days = sim_days,
            method = "constant"
          )
        }
      }
      result$phosphorus_prey_concentration <- phos_prey_conc
    }

    # Concentración en depredador
    if ("predator_concentration" %in% names(phos_data)) {
      result$phosphorus_predator_concentration <- interpolate_time_series(
        data = phos_data$predator_concentration,
        value_col = "Concentration",
        target_days = sim_days,
        method = "constant"
      )
    }
  }

  # Procesar datos de nitrógeno (similar al fósforo)
  if ("nitrogen" %in% names(nutrient_data)) {
    nit_data <- nutrient_data$nitrogen

    # Eficiencia de asimilación de nitrógeno
    if ("assimilation_efficiency" %in% names(nit_data)) {
      nit_ae <- array(0, dim = c(n_days, n_prey))
      colnames(nit_ae) <- prey_names

      ae_data <- nit_data$assimilation_efficiency
      for (i in seq_along(prey_names)) {
        if (prey_names[i] %in% names(ae_data)) {
          nit_ae[, i] <- interpolate_time_series(
            data = ae_data,
            value_col = prey_names[i],
            target_days = sim_days,
            method = "linear"
          )
        }
      }
      result$nitrogen_ae <- nit_ae
    }

    # Concentración en presas
    if ("prey_concentration" %in% names(nit_data)) {
      nit_prey_conc <- array(0, dim = c(n_days, n_prey))
      colnames(nit_prey_conc) <- prey_names

      conc_data <- nit_data$prey_concentration
      for (i in seq_along(prey_names)) {
        if (prey_names[i] %in% names(conc_data)) {
          nit_prey_conc[, i] <- interpolate_time_series(
            data = conc_data,
            value_col = prey_names[i],
            target_days = sim_days,
            method = "constant"
          )
        }
      }
      result$nitrogen_prey_concentration <- nit_prey_conc
    }

    # Concentración en depredador
    if ("predator_concentration" %in% names(nit_data)) {
      result$nitrogen_predator_concentration <- interpolate_time_series(
        data = nit_data$predator_concentration,
        value_col = "Concentration",
        target_days = sim_days,
        method = "constant"
      )
    }
  }

  return(result)
}

#' Validar consistencia de datos procesados
#'
#' @param processed_data Datos procesados
#' @return Lista con resultados de validación
#' @export
validate_processed_data <- function(processed_data) {

  issues <- list()

  # Verificar que las proporciones de dieta sumen ~1
  diet_sums <- rowSums(processed_data$diet_proportions)
  if (any(abs(diet_sums - 1) > 0.01)) {
    issues$diet_proportions <- "Algunas proporciones de dieta no suman 1.0"
  }

  # Verificar valores de temperatura
  if (any(processed_data$temperature < -5 | processed_data$temperature > 50)) {
    issues$temperature <- "Temperaturas fuera de rango típico (-5 a 50°C)"
  }

  # Verificar energías de presas
  if (any(processed_data$prey_energies < 1000 | processed_data$prey_energies > 10000)) {
    issues$prey_energies <- "Energías de presas fuera de rango típico (1000-10000 J/g)"
  }

  # Verificar datos de mortalidad si existen
  if ("mortality" %in% names(processed_data)) {
    mort_rates <- processed_data$mortality$rates
    if (any(mort_rates < 0 | mort_rates > 1)) {
      issues$mortality <- "Tasas de mortalidad fuera de rango (0-1)"
    }
  }

  return(list(
    valid = length(issues) == 0,
    issues = issues,
    n_issues = length(issues)
  ))
}
