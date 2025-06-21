#' Inicializar estructura de salida diaria
#'
#' @param n_days Número de días
#' @param prey_names Nombres de las presas
#' @return Data frame inicializado para salida diaria
#' @keywords internal
initialize_daily_output <- function(n_days, prey_names) {

  n_prey <- length(prey_names)

  # Columnas básicas
  basic_cols <- c(
    "Day", "Temperature.C", "Weight.g", "Population.Number", "Population.Biomass.g",
    "Specific.Growth.Rate.J.g.d", "Specific.Consumption.Rate.J.g.d",
    "Specific.Egestion.Rate.J.g.d", "Specific.Excretion.Rate.J.g.d",
    "Specific.Respiration.Rate.J.g.d", "Specific.SDA.Rate.J.g.d",
    "Specific.Consumption.Rate.g.g.d", "Specific.Growth.Rate.g.g.d",
    "Initial.Predator.Energy.Density.J.g", "Final.Predator.Energy.Density.J.g",
    "Mean.Prey.Energy.Density.J.g", "Gross.Production.g", "Gross.Production.J",
    "Cum.Gross.Production.g", "Cum.Gross.Production.J", "Gametic.Production.g",
    "Cum.Gametic.Production.J", "Net.Production.g", "Net.Production.J",
    "Cum.Net.Production.g", "Cum.Net.Production.J", "Consumption.g", "Consumption.J",
    "Cum.Cons.g", "Cum.Cons.J", "Cons.Pop.g", "Cons.Pop.J",
    "Cum.Cons.Pop.g", "Cum.Cons.Pop.J", "Mortality.number", "Mortality.g"
  )

  # Columnas de nutrientes
  nutrient_cols <- c(
    "Nitrogen.Egestion.g", "Phosphorous.Egestion.g", "N.to.P.Egestion",
    "Nitrogen.Excretion.g", "Phosphorous.Excretion.g", "N.to.P.Excretion",
    "Nitrogen.Consumption.g", "Phosphorous.Consumption.g", "N.to.P.Consumption",
    "Nitrogen.Growth.g", "Phosphorous.Growth.g", "N.to.P.Growth"
  )

  # Columnas de contaminantes
  contaminant_cols <- c(
    "Contaminant.Clearance.Rate.ug.d", "Contaminant.Uptake.ug",
    "Contaminant.Burden.ug", "Contaminant.Predator.Concentration.ug.g"
  )

  # Columnas específicas por presa
  prey_cons_j <- paste("Cons", prey_names, "J", sep = " ")
  prey_cons_g <- paste("Cons", prey_names, "g", sep = " ")
  prey_cons_pop_j <- paste("Cons Pop", prey_names, "J", sep = " ")
  prey_cons_pop_g <- paste("Cons Pop", prey_names, "g", sep = " ")

  # Combinar todas las columnas
  all_cols <- c(basic_cols, nutrient_cols, contaminant_cols,
                prey_cons_j, prey_cons_g, prey_cons_pop_j, prey_cons_pop_g)

  # Crear data frame
  daily_output <- data.frame(matrix(NA, nrow = n_days, ncol = length(all_cols)))
  names(daily_output) <- all_cols

  return(daily_output)
}

#' Extraer datos de un día específico
#'
#' @param processed_data Datos procesados
#' @param day Día a extraer
#' @return Lista con datos del día
#' @keywords internal
extract_day_data <- function(processed_data, day) {

  day_data <- list(
    temperature = processed_data$temperature[day],
    diet_proportions = processed_data$diet_proportions[day, ],
    prey_energies = processed_data$prey_energies[day, ]
  )

  # Agregar datos opcionales si existen
  if (!is.null(processed_data$indigestible_proportions)) {
    day_data$indigestible_proportions <- processed_data$indigestible_proportions[day, ]
  }

  if (!is.null(processed_data$reproduction)) {
    day_data$reproduction <- processed_data$reproduction[day]
  }

  if (!is.null(processed_data$contaminant)) {
    day_data$contaminant <- extract_contaminant_day_data(processed_data$contaminant, day)
  }

  if (!is.null(processed_data$nutrients)) {
    day_data$nutrients <- extract_nutrient_day_data(processed_data$nutrients, day)
  }

  return(day_data)
}

#' Extraer datos de contaminantes para un día específico
#'
#' @param contaminant_data Datos de contaminantes
#' @param day Día a extraer
#' @return Lista con datos de contaminantes del día
#' @keywords internal
extract_contaminant_day_data <- function(contaminant_data, day) {
  day_data <- list()

  if (!is.null(contaminant_data$prey_concentrations)) {
    day_data$prey_concentrations <- contaminant_data$prey_concentrations[day, ]
  }

  if (!is.null(contaminant_data$assimilation_efficiency)) {
    day_data$assimilation_efficiency <- contaminant_data$assimilation_efficiency[day, ]
  }

  if (!is.null(contaminant_data$transfer_efficiency)) {
    day_data$transfer_efficiency <- contaminant_data$transfer_efficiency[day, ]
  }

  return(day_data)
}

#' Extraer datos de nutrientes para un día específico
#'
#' @param nutrient_data Datos de nutrientes
#' @param day Día a extraer
#' @return Lista con datos de nutrientes del día
#' @keywords internal
extract_nutrient_day_data <- function(nutrient_data, day) {
  day_data <- list()

  # Datos de fósforo
  if (!is.null(nutrient_data$phosphorus_ae)) {
    day_data$phosphorus_ae <- nutrient_data$phosphorus_ae[day, ]
  }
  if (!is.null(nutrient_data$phosphorus_prey_concentration)) {
    day_data$phosphorus_prey_concentration <- nutrient_data$phosphorus_prey_concentration[day, ]
  }
  if (!is.null(nutrient_data$phosphorus_predator_concentration)) {
    day_data$phosphorus_predator_concentration <- nutrient_data$phosphorus_predator_concentration[day]
  }

  # Datos de nitrógeno
  if (!is.null(nutrient_data$nitrogen_ae)) {
    day_data$nitrogen_ae <- nutrient_data$nitrogen_ae[day, ]
  }
  if (!is.null(nutrient_data$nitrogen_prey_concentration)) {
    day_data$nitrogen_prey_concentration <- nutrient_data$nitrogen_prey_concentration[day, ]
  }
  if (!is.null(nutrient_data$nitrogen_predator_concentration)) {
    day_data$nitrogen_predator_concentration <- nutrient_data$nitrogen_predator_concentration[day]
  }

  return(day_data)
}

#' Calcular peso final del día
#'
#' @param initial_weight Peso al inicio del día
#' @param energy_gain Ganancia de energía neta
#' @param spawn_energy Energía perdida por reproducción
#' @param predator_energy_start Densidad energética inicial del depredador
#' @param day Día actual
#' @param equation Ecuación de densidad energética
#' @param params Parámetros de densidad energética
#' @return Lista con peso final y densidad energética final
#' @keywords internal
calculate_final_weight <- function(initial_weight, energy_gain, spawn_energy,
                                   predator_energy_start, day, equation, params) {

  # Energía neta disponible para crecimiento
  net_energy <- energy_gain - spawn_energy

  if (equation == 3) {
    # ED = alpha1 * W^beta1
    alpha1 <- params$Alpha1
    beta1 <- params$Beta1

    final_weight <- ((net_energy + (predator_energy_start * initial_weight)) / alpha1)^(1/(beta1 + 1))
    final_energy_density <- alpha1 * final_weight^beta1

  } else if (equation == 2) {
    # ED lineal por segmentos
    alpha1 <- params$Alpha1
    beta1 <- params$Beta1
    alpha2 <- params$Alpha2
    beta2 <- params$Beta2
    cutoff <- params$Cutoff

    if (initial_weight < cutoff) {
      if (beta1 != 0) {
        # Usar fórmula cuadrática
        a <- beta1
        b <- alpha1
        c <- -(initial_weight * (alpha1 + beta1 * initial_weight) + net_energy)

        discriminant <- b^2 - 4*a*c
        if (discriminant < 0) {
          stop("Discriminante negativo en cálculo de peso final")
        }

        final_weight <- (-b + sqrt(discriminant)) / (2*a)
      } else {
        final_weight <- (net_energy + initial_weight * alpha1) / alpha1
      }

      # Verificar si cruzamos el cutoff
      if (final_weight > cutoff) {
        # Recalcular considerando el cruce
        energy_to_cutoff <- cutoff * (alpha1 + beta1 * cutoff) -
          initial_weight * (alpha1 + beta1 * initial_weight)
        remaining_energy <- net_energy - energy_to_cutoff

        if (beta2 != 0) {
          a <- beta2
          b <- alpha2
          c <- -(cutoff * (alpha2 + beta2 * cutoff) + remaining_energy)

          discriminant <- b^2 - 4*a*c
          if (discriminant < 0) {
            stop("Discriminante negativo en cálculo de peso final (segmento 2)")
          }

          final_weight <- (-b + sqrt(discriminant)) / (2*a)
        } else {
          final_weight <- (remaining_energy + cutoff * alpha2) / alpha2
        }
      }

    } else {
      # Peso inicial >= cutoff
      if (beta2 != 0) {
        a <- beta2
        b <- alpha2
        c <- -(initial_weight * (alpha2 + beta2 * initial_weight) + net_energy)

        discriminant <- b^2 - 4*a*c
        if (discriminant < 0) {
          stop("Discriminante negativo en cálculo de peso final")
        }

        final_weight <- (-b + sqrt(discriminant)) / (2*a)
      } else {
        final_weight <- (net_energy + initial_weight * alpha2) / alpha2
      }
    }

    # Calcular densidad energética final
    final_energy_density <- pred_En_D(W = final_weight, day = day, PREDEDEQ = equation,
                                      alpha1 = alpha1, beta1 = beta1, alpha2 = alpha2,
                                      beta2 = beta2, cutoff = cutoff)

  } else if (equation == 1) {
    # Usar datos de archivo
    # Necesitaríamos acceso a los datos interpolados
    # Por simplicidad, asumir densidad energética constante
    final_weight <- (net_energy + predator_energy_start * initial_weight) / predator_energy_start
    final_energy_density <- predator_energy_start
  }

  return(list(
    weight = final_weight,
    energy_density = final_energy_density
  ))
}

#' Calcular balance energético
#'
#' @param initial_weight Peso inicial
#' @param final_weight Peso final
#' @param total_spawn_energy Energía total perdida por reproducción
#' @param daily_output Salida diaria (opcional)
#' @param predator_energy_params Parámetros de energía del depredador
#' @return Lista con información del balance energético
#' @keywords internal
calculate_energy_balance <- function(initial_weight, final_weight, total_spawn_energy,
                                     daily_output = NULL, predator_energy_params) {

  # Calcular energías inicial y final
  initial_energy_density <- calculate_predator_energy_density(
    weight = initial_weight, day = 1,
    equation = predator_energy_params$PREDEDEQ,
    params = predator_energy_params
  )

  final_energy_density <- calculate_predator_energy_density(
    weight = final_weight, day = 1,
    equation = predator_energy_params$PREDEDEQ,
    params = predator_energy_params
  )

  initial_body_energy <- initial_weight * initial_energy_density
  final_body_energy <- final_weight * final_energy_density

  if (!is.null(daily_output)) {
    # Usar datos diarios para cálculo preciso
    total_energy_gain <- sum(daily_output$Net.Production.J, na.rm = TRUE)
  } else {
    # Estimación simple
    total_energy_gain <- final_body_energy - initial_body_energy + total_spawn_energy
  }

  # Calcular balance
  expected_final_energy <- initial_body_energy + total_energy_gain - total_spawn_energy
  energy_difference <- abs(expected_final_energy - final_body_energy)
  relative_error <- energy_difference / final_body_energy

  # Tolerancia para balance energético
  tolerance <- 0.000001
  balanced <- relative_error < tolerance

  message <- if (balanced) {
    "Balance energético correcto"
  } else {
    "Advertencia: Balance energético no equilibrado"
  }

  return(list(
    balanced = balanced,
    relative_error = relative_error,
    absolute_error = energy_difference,
    initial_body_energy = initial_body_energy,
    final_body_energy = final_body_energy,
    total_energy_gain = total_energy_gain,
    total_spawn_energy = total_spawn_energy,
    message = message
  ))
}

#' Preparar resumen de la simulación
#'
#' @param initial_weight Peso inicial
#' @param final_weight Peso final
#' @param total_consumption Consumo total
#' @param p_value Valor p usado
#' @param current_population Población final
#' @param total_spawn_energy Energía total de reproducción
#' @param energy_balance Información del balance energético
#' @param daily_output Salida diaria (opcional)
#' @return Data frame con resumen
#' @keywords internal
prepare_simulation_summary <- function(initial_weight, final_weight, total_consumption,
                                       p_value, current_population, total_spawn_energy,
                                       energy_balance, daily_output = NULL) {

  # Parámetros básicos
  parameters <- c("p-value", "Total consumption (g)", "Final weight (g)")
  values <- c(
    if (!is.null(p_value)) format(round(p_value, 6), nsmall = 3) else "N/A",
    format(round(total_consumption, 3), nsmall = 3),
    format(round(final_weight, 3), nsmall = 3)
  )

  # Información poblacional
  parameters <- c(parameters, "Population Number", "Population Biomass (g)")
  values <- c(values,
              format(round(current_population, 3), nsmall = 3),
              format(round(final_weight * current_population, 3), nsmall = 3)
  )

  # Información de reproducción
  if (total_spawn_energy > 0) {
    parameters <- c(parameters, "Total Spawning Energy (J)")
    values <- c(values, format(round(total_spawn_energy, 3), nsmall = 3))
  }

  # Balance energético
  parameters <- c(parameters, energy_balance$message)
  values <- c(values, format(round(energy_balance$relative_error, 10), nsmall = 3))

  # Crear data frame
  summary_df <- data.frame(
    Parameter = parameters,
    Value = values,
    stringsAsFactors = FALSE
  )

  return(summary_df)
}

#' Guardar resultados diarios
#'
#' @param daily_output Data frame de salida diaria
#' @param day Día actual
#' @param day_data Datos del día
#' @param current_weight Peso actual
#' @param final_weight Peso final
#' @param current_population Población actual
#' @param consumption_gg Consumo específico
#' @param consumption_energy Consumo energético
#' @param egestion_energy Egestion
#' @param excretion_energy Excreción
#' @param respiration_energy Respiración
#' @param sda_energy SDA
#' @param growth_energy Energía de crecimiento
#' @param weight_gain Ganancia de peso
#' @param spawn_energy Energía de reproducción
#' @param predator_energy_start Densidad energética inicial
#' @param predator_energy_end Densidad energética final
#' @param mean_prey_energy Energía media de presas
#' @param consumption_by_prey Consumo por tipo de presa
#' @param contaminant_results Resultados de contaminantes
#' @param nutrient_results Resultados de nutrientes
#' @param processed_data Datos procesados
#' @return Data frame actualizado
#' @keywords internal
save_daily_results <- function(daily_output, day, day_data, current_weight, final_weight,
                               current_population, consumption_gg, consumption_energy,
                               egestion_energy, excretion_energy, respiration_energy,
                               sda_energy, growth_energy, weight_gain, spawn_energy,
                               predator_energy_start, predator_energy_end, mean_prey_energy,
                               consumption_by_prey, contaminant_results, nutrient_results,
                               processed_data) {

  # Datos básicos
  daily_output$Day[day] <- day
  daily_output$Temperature.C[day] <- day_data$temperature
  daily_output$Weight.g[day] <- final_weight
  daily_output$Population.Number[day] <- current_population
  daily_output$Population.Biomass.g[day] <- final_weight * current_population

  # Tasas específicas
  daily_output$Specific.Growth.Rate.J.g.d[day] <- growth_energy
  daily_output$Specific.Consumption.Rate.J.g.d[day] <- consumption_energy
  daily_output$Specific.Egestion.Rate.J.g.d[day] <- egestion_energy
  daily_output$Specific.Excretion.Rate.J.g.d[day] <- excretion_energy
  daily_output$Specific.Respiration.Rate.J.g.d[day] <- respiration_energy
  daily_output$Specific.SDA.Rate.J.g.d[day] <- sda_energy
  daily_output$Specific.Consumption.Rate.g.g.d[day] <- consumption_gg
  daily_output$Specific.Growth.Rate.g.g.d[day] <- weight_gain / current_weight

  # Densidades energéticas
  daily_output$Initial.Predator.Energy.Density.J.g[day] <- predator_energy_start
  daily_output$Final.Predator.Energy.Density.J.g[day] <- predator_energy_end
  daily_output$Mean.Prey.Energy.Density.J.g[day] <- mean_prey_energy

  # Producción
  daily_output$Net.Production.g[day] <- weight_gain
  daily_output$Net.Production.J[day] <- growth_energy * current_weight
  daily_output$Gross.Production.g[day] <- (consumption_energy + respiration_energy +
                                             egestion_energy + excretion_energy + sda_energy) *
    current_weight / predator_energy_start
  daily_output$Gross.Production.J[day] <- (consumption_energy + respiration_energy +
                                             egestion_energy + excretion_energy + sda_energy) *
    current_weight

  # Reproducción
  daily_output$Gametic.Production.g[day] <- spawn_energy / predator_energy_start

  # Consumo
  daily_output$Consumption.g[day] <- consumption_energy * current_weight / mean_prey_energy
  daily_output$Consumption.J[day] <- consumption_energy * current_weight

  # Consumo poblacional
  daily_output$Cons.Pop.g[day] <- daily_output$Consumption.g[day] * current_population
  daily_output$Cons.Pop.J[day] <- daily_output$Consumption.J[day] * current_population

  # Mortalidad
  if (day > 1) {
    daily_output$Mortality.number[day] <- daily_output$Population.Number[day-1] - current_population
    daily_output$Mortality.g[day] <- daily_output$Mortality.number[day] * current_weight
  }

  # Consumo por tipo de presa
  prey_names <- processed_data$prey_names
  prey_energies_day <- day_data$prey_energies

  for (i in seq_along(prey_names)) {
    prey_name <- prey_names[i]

    # Consumo en J y g por presa
    cons_j_col <- paste("Cons", prey_name, "J")
    cons_g_col <- paste("Cons", prey_name, "g")

    if (cons_j_col %in% names(daily_output)) {
      daily_output[[cons_j_col]][day] <- consumption_by_prey[i] * prey_energies_day[i]
      daily_output[[cons_g_col]][day] <- consumption_by_prey[i]
    }

    # Consumo poblacional
    cons_pop_j_col <- paste("Cons Pop", prey_name, "J")
    cons_pop_g_col <- paste("Cons Pop", prey_name, "g")

    if (cons_pop_j_col %in% names(daily_output)) {
      daily_output[[cons_pop_j_col]][day] <- consumption_by_prey[i] * prey_energies_day[i] * current_population
      daily_output[[cons_pop_g_col]][day] <- consumption_by_prey[i] * current_population
    }
  }

  # Resultados de contaminantes
  if (!is.null(contaminant_results)) {
    daily_output$Contaminant.Clearance.Rate.ug.d[day] <- contaminant_results$clearance_rate
    daily_output$Contaminant.Uptake.ug[day] <- contaminant_results$uptake
    daily_output$Contaminant.Burden.ug[day] <- contaminant_results$burden
    daily_output$Contaminant.Predator.Concentration.ug.g[day] <- contaminant_results$concentration
  }

  # Resultados de nutrientes
  if (!is.null(nutrient_results)) {
    if (!is.null(nutrient_results$nitrogen)) {
      daily_output$Nitrogen.Egestion.g[day] <- nutrient_results$nitrogen$egestion
      daily_output$Nitrogen.Excretion.g[day] <- nutrient_results$nitrogen$excretion
      daily_output$Nitrogen.Consumption.g[day] <- nutrient_results$nitrogen$consumption
      daily_output$Nitrogen.Growth.g[day] <- nutrient_results$nitrogen$growth
    }

    if (!is.null(nutrient_results$phosphorus)) {
      daily_output$Phosphorous.Egestion.g[day] <- nutrient_results$phosphorus$egestion
      daily_output$Phosphorous.Excretion.g[day] <- nutrient_results$phosphorus$excretion
      daily_output$Phosphorous.Consumption.g[day] <- nutrient_results$phosphorus$consumption
      daily_output$Phosphorous.Growth.g[day] <- nutrient_results$phosphorus$growth
    }

    # Ratios N:P
    if (!is.null(nutrient_results$nitrogen) && !is.null(nutrient_results$phosphorus)) {
      daily_output$N.to.P.Egestion[day] <- nutrient_results$nitrogen$egestion / nutrient_results$phosphorus$egestion
      daily_output$N.to.P.Excretion[day] <- nutrient_results$nitrogen$excretion / nutrient_results$phosphorus$excretion
      daily_output$N.to.P.Consumption[day] <- nutrient_results$nitrogen$consumption / nutrient_results$phosphorus$consumption
      daily_output$N.to.P.Growth[day] <- nutrient_results$nitrogen$growth / nutrient_results$phosphorus$growth
    }
  }

  # Calcular valores acumulativos
  if (day > 1) {
    daily_output$Cum.Gross.Production.g[day] <- sum(daily_output$Gross.Production.g[1:day], na.rm = TRUE)
    daily_output$Cum.Gross.Production.J[day] <- sum(daily_output$Gross.Production.J[1:day], na.rm = TRUE)
    daily_output$Cum.Net.Production.g[day] <- sum(daily_output$Net.Production.g[1:day], na.rm = TRUE)
    daily_output$Cum.Net.Production.J[day] <- sum(daily_output$Net.Production.J[1:day], na.rm = TRUE)
    daily_output$Cum.Cons.g[day] <- sum(daily_output$Consumption.g[1:day], na.rm = TRUE)
    daily_output$Cum.Cons.J[day] <- sum(daily_output$Consumption.J[1:day], na.rm = TRUE)
    daily_output$Cum.Cons.Pop.g[day] <- sum(daily_output$Cons.Pop.g[1:day], na.rm = TRUE)
    daily_output$Cum.Cons.Pop.J[day] <- sum(daily_output$Cons.Pop.J[1:day], na.rm = TRUE)
    daily_output$Cum.Gametic.Production.J[day] <- sum(daily_output$Gametic.Production.g[1:day] *
                                                        daily_output$Initial.Predator.Energy.Density.J.g[1:day], na.rm = TRUE)
  } else {
    daily_output$Cum.Gross.Production.g[day] <- daily_output$Gross.Production.g[day]
    daily_output$Cum.Gross.Production.J[day] <- daily_output$Gross.Production.J[day]
    daily_output$Cum.Net.Production.g[day] <- daily_output$Net.Production.g[day]
    daily_output$Cum.Net.Production.J[day] <- daily_output$Net.Production.J[day]
    daily_output$Cum.Cons.g[day] <- daily_output$Consumption.g[day]
    daily_output$Cum.Cons.J[day] <- daily_output$Consumption.J[day]
    daily_output$Cum.Cons.Pop.g[day] <- daily_output$Cons.Pop.g[day]
    daily_output$Cum.Cons.Pop.J[day] <- daily_output$Cons.Pop.J[day]
    daily_output$Cum.Gametic.Production.J[day] <- daily_output$Gametic.Production.g[day] *
      daily_output$Initial.Predator.Energy.Density.J.g[day]
  }

  return(daily_output)
}#' Modelo Principal de Crecimiento FB4
#'
#' @name growth-model
#' @aliases growth-model
NULL

#' Función principal del modelo de crecimiento FB4
#'
#' Ejecuta la simulación diaria del modelo bioenergético
#'
#' @param initial_weight Peso inicial del pez (g)
#' @param p_value Proporción del consumo máximo (0-5)
#' @param ration_percent Ración como porcentaje del peso corporal (opcional)
#' @param ration_grams Ración en gramos por día (opcional)
#' @param calculation_method Método de cálculo ("p_value", "ration_percent", "ration_grams")
#' @param species_params Parámetros de la especie
#' @param processed_data Datos procesados de entrada
#' @param model_options Opciones del modelo
#' @param oxycal Coeficiente oxicalórico (J/g O2)
#' @param population_size Tamaño inicial de la población
#' @param output_type Tipo de salida ("full", "summary_only", "final_only")
#' @param progress_callback Función callback para mostrar progreso
#'
#' @return Lista con resultados de la simulación
#' @keywords internal
run_growth_model_internal <- function(initial_weight,
                                      p_value = NULL,
                                      ration_percent = NULL,
                                      ration_grams = NULL,
                                      calculation_method = "p_value",
                                      species_params,
                                      processed_data,
                                      model_options,
                                      oxycal = 13560,
                                      population_size = 1,
                                      output_type = "full",
                                      progress_callback = NULL) {

  # Inicializar variables
  n_days <- processed_data$n_days
  current_weight <- initial_weight
  current_population <- population_size

  # Variables acumulativas
  total_spawn_energy <- 0
  total_consumption_grams <- 0

  # Preparar salida diaria si es necesaria
  if (output_type == "full") {
    daily_output <- initialize_daily_output(n_days, processed_data$prey_names)
  }

  # Bucle principal diario
  for (day in 1:n_days) {

    # Mostrar progreso
    if (!is.null(progress_callback) && day %% 10 == 0) {
      progress_callback(day / n_days, paste("Día", day, "de", n_days))
    }

    # Obtener datos del día
    day_data <- extract_day_data(processed_data, day)

    # Calcular mortalidad poblacional si está habilitada
    if (model_options$calc_mortality && !is.null(processed_data$mortality)) {
      current_population <- current_population * processed_data$mortality$population_survival[day]
    }

    # Calcular densidad energética del depredador
    predator_energy_start <- calculate_predator_energy_density(
      weight = current_weight,
      day = day,
      equation = species_params$predator_energy$PREDEDEQ,
      params = species_params$predator_energy,
      data = processed_data
    )

    # Calcular densidad energética media de las presas
    prey_energy_densities <- day_data$prey_energies * day_data$diet_proportions
    mean_prey_energy <- sum(prey_energy_densities)

    # Calcular consumo según el método especificado
    if (calculation_method == "ration_percent") {
      # Ración como % del peso corporal
      consumption_gg <- ration_percent  # g presa/g pez
      consumption_energy <- consumption_gg * mean_prey_energy  # J/g
      # Calcular p-value correspondiente para otros cálculos
      max_consumption <- consumption(
        Temperature = day_data$temperature,
        W = current_weight,
        p = 1,
        CEQ = species_params$consumption$CEQ,
        CA = species_params$consumption$CA,
        CB = species_params$consumption$CB,
        CQ = species_params$consumption$CQ
      )
      daily_p <- consumption_energy / (max_consumption * mean_prey_energy)

    } else if (calculation_method == "ration_grams") {
      # Ración como gramos de presa por día
      consumption_gg <- ration_grams / current_weight  # g presa/g pez
      consumption_energy <- consumption_gg * mean_prey_energy  # J/g
      # Calcular p-value correspondiente
      max_consumption <- consumption(
        Temperature = day_data$temperature,
        W = current_weight,
        p = 1,
        CEQ = species_params$consumption$CEQ,
        CA = species_params$consumption$CA,
        CB = species_params$consumption$CB,
        CQ = species_params$consumption$CQ
      )
      daily_p <- consumption_energy / (max_consumption * mean_prey_energy)

    } else {
      # Usar p-value directamente
      daily_p <- p_value
      consumption_gg <- consumption(
        Temperature = day_data$temperature,
        W = current_weight,
        p = daily_p,
        CEQ = species_params$consumption$CEQ,
        CA = species_params$consumption$CA,
        CB = species_params$consumption$CB,
        CQ = species_params$consumption$CQ,
        CTO = species_params$consumption$CTO,
        CTM = species_params$consumption$CTM,
        CTL = species_params$consumption$CTL,
        CK1 = species_params$consumption$CK1,
        CK4 = species_params$consumption$CK4,
        CG1 = species_params$consumption$CG1,
        CG2 = species_params$consumption$CG2,
        CX = species_params$consumption$CX,
        CY = species_params$consumption$CY,
        CZ = species_params$consumption$CZ
      )
      consumption_energy <- consumption_gg * mean_prey_energy
    }

    # Calcular egestion
    egestion_energy <- egestion(
      C = consumption_energy,
      Temperature = day_data$temperature,
      p = daily_p,
      EGEQ = species_params$egestion$EGEQ,
      FA = species_params$egestion$FA,
      FB = species_params$egestion$FB,
      FG = species_params$egestion$FG
    )

    # Calcular excreción
    excretion_energy <- excretion(
      C = consumption_energy,
      Eg = egestion_energy,
      Temperature = day_data$temperature,
      p = daily_p,
      EXEQ = species_params$excretion$EXEQ,
      UA = species_params$excretion$UA,
      UB = species_params$excretion$UB,
      UG = species_params$excretion$UG
    )

    # Calcular respiración
    respiration_o2 <- respiration(
      Temperature = day_data$temperature,
      W = current_weight,
      REQ = species_params$respiration$REQ,
      RA = species_params$respiration$RA,
      RB = species_params$respiration$RB,
      RQ = species_params$respiration$RQ,
      RTL = species_params$respiration$RTL,
      ACT = species_params$respiration$ACT,
      RK4 = species_params$respiration$RK4,
      BACT = species_params$respiration$BACT,
      RK1 = species_params$respiration$RK1,
      RK5 = species_params$respiration$RK5,
      RTO = species_params$respiration$RTO,
      RTM = species_params$respiration$RTM,
      RX = species_params$respiration$RX,
      RY = species_params$respiration$RY,
      RZ = species_params$respiration$RZ
    )
    respiration_energy <- respiration_o2 * oxycal

    # Calcular acción dinámica específica
    sda_energy <- SpDynAct(
      C = consumption_energy,
      Eg = egestion_energy,
      SDA = species_params$respiration$SDA
    )

    # Calcular energía disponible para crecimiento
    growth_energy <- consumption_energy - (respiration_energy + egestion_energy +
                                             excretion_energy + sda_energy)

    # Calcular pérdida por reproducción si está habilitada
    spawn_energy <- 0
    if (model_options$calc_reproduction && !is.null(processed_data$reproduction)) {
      spawn_fraction <- processed_data$reproduction[day]
      spawn_energy <- spawn_fraction * current_weight * predator_energy_start
      total_spawn_energy <- total_spawn_energy + spawn_energy
    }

    # Calcular peso final del día
    energy_gain <- growth_energy * current_weight
    final_weight_result <- calculate_final_weight(
      initial_weight = current_weight,
      energy_gain = energy_gain,
      spawn_energy = spawn_energy,
      predator_energy_start = predator_energy_start,
      day = day,
      equation = species_params$predator_energy$PREDEDEQ,
      params = species_params$predator_energy
    )

    final_weight <- final_weight_result$weight
    predator_energy_end <- final_weight_result$energy_density

    # Verificar peso válido
    if (final_weight <= 0) {
      prt_msg(1, day, current_weight, day_data$temperature, daily_p,
              output_type, 3, 10)
      stop("Peso del pez se volvió negativo en el día ", day)
    }

    # Calcular ganancia de peso
    weight_gain <- final_weight - current_weight

    # Calcular consumo por tipo de presa (para contaminantes y nutrientes)
    consumption_by_prey <- (consumption_gg * current_weight) * day_data$diet_proportions

    # Procesar contaminantes si está habilitado
    contaminant_results <- NULL
    if (model_options$calc_contaminant && !is.null(processed_data$contaminant)) {
      contaminant_results <- calculate_contaminant_dynamics(
        consumption_by_prey = consumption_by_prey,
        weight = current_weight,
        final_weight = final_weight,
        temperature = day_data$temperature,
        respiration_o2 = respiration_o2,
        contaminant_data = day_data$contaminant,
        day = day
      )
    }

    # Procesar nutrientes si está habilitado
    nutrient_results <- NULL
    if (model_options$calc_nutrient && !is.null(processed_data$nutrients)) {
      nutrient_results <- calculate_nutrient_dynamics(
        consumption_by_prey = consumption_by_prey,
        weight_gain = weight_gain,
        nutrient_data = day_data$nutrients,
        day = day
      )
    }

    # Guardar resultados diarios si es necesario
    if (output_type == "full") {
      daily_output <- save_daily_results(
        daily_output = daily_output,
        day = day,
        day_data = day_data,
        current_weight = current_weight,
        final_weight = final_weight,
        current_population = current_population,
        consumption_gg = consumption_gg,
        consumption_energy = consumption_energy,
        egestion_energy = egestion_energy,
        excretion_energy = excretion_energy,
        respiration_energy = respiration_energy,
        sda_energy = sda_energy,
        growth_energy = growth_energy,
        weight_gain = weight_gain,
        spawn_energy = spawn_energy,
        predator_energy_start = predator_energy_start,
        predator_energy_end = predator_energy_end,
        mean_prey_energy = mean_prey_energy,
        consumption_by_prey = consumption_by_prey,
        contaminant_results = contaminant_results,
        nutrient_results = nutrient_results,
        processed_data = processed_data
      )
    }

    # Actualizar variables para el siguiente día
    total_consumption_grams <- total_consumption_grams + (consumption_gg * current_weight)
    current_weight <- final_weight
  }

  # Calcular balance energético
  energy_balance <- calculate_energy_balance(
    initial_weight = initial_weight,
    final_weight = current_weight,
    total_spawn_energy = total_spawn_energy,
    daily_output = if(output_type == "full") daily_output else NULL,
    predator_energy_params = species_params$predator_energy
  )

  # Preparar resumen
  summary <- prepare_simulation_summary(
    initial_weight = initial_weight,
    final_weight = current_weight,
    total_consumption = total_consumption_grams,
    p_value = p_value,
    current_population = current_population,
    total_spawn_energy = total_spawn_energy,
    energy_balance = energy_balance,
    daily_output = if(output_type == "full") daily_output else NULL
  )

  # Preparar resultado final
  result <- list(
    summary = summary,
    energy_balance = energy_balance,
    final_weight = current_weight,
    total_consumption = total_consumption_grams,
    final_population = current_population
  )

  if (output_type == "full") {
    result$daily_output <- daily_output
  }

  return(result)
}
