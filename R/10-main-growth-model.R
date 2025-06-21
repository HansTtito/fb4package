#' Motor de Simulación Principal FB4
#'
#' @name simulation-engine
#' @aliases simulation-engine
NULL

# ============================================================================
# FUNCIONES CORE DE SIMULACIÓN DIARIA
# ============================================================================

#' Calcular consumo diario
#'
#' Calcula el consumo diario basado en el método especificado
#'
#' @param current_weight Peso actual del pez (g)
#' @param temperature Temperatura del agua (°C)
#' @param p_value Proporción del consumo máximo (0-5)
#' @param ration_percent Ración como porcentaje del peso corporal
#' @param ration_grams Ración en gramos por día
#' @param method Método de cálculo ("p_value", "ration_percent", "ration_grams")
#' @param consumption_params Parámetros de consumo de la especie
#' @param mean_prey_energy Densidad energética media de las presas (J/g)
#' @return Lista con consumo específico y energético
#' @keywords internal
calculate_daily_consumption <- function(current_weight, temperature, p_value = NULL,
                                        ration_percent = NULL, ration_grams = NULL,
                                        method = "p_value", consumption_params, mean_prey_energy) {
  
  # Validar parámetros según método
  if (method == "p_value" && is.null(p_value)) {
    stop("p_value requerido para método p_value")
  }
  if (method == "ration_percent" && is.null(ration_percent)) {
    stop("ration_percent requerido para método ration_percent")
  }
  if (method == "ration_grams" && is.null(ration_grams)) {
    stop("ration_grams requerido para método ration_grams")
  }
  
  # Calcular consumo máximo una vez
  max_consumption_gg <- calculate_consumption(temperature, current_weight, 1.0, 
                                              consumption_params, method = "maximum")
  
  if (method == "ration_percent") {
    # Ración como % del peso corporal
    consumption_gg <- ration_percent / 100  # g presa/g pez
    consumption_energy <- consumption_gg * mean_prey_energy  # J/g
    
    # Calcular p-value equivalente
    effective_p <- if (max_consumption_gg > 0) consumption_gg / max_consumption_gg else 0
    
  } else if (method == "ration_grams") {
    # Ración como gramos de presa por día
    consumption_gg <- ration_grams / current_weight  # g presa/g pez
    consumption_energy <- consumption_gg * mean_prey_energy  # J/g
    
    # Calcular p-value equivalente
    effective_p <- if (max_consumption_gg > 0) consumption_gg / max_consumption_gg else 0
    
  } else {  # method == "p_value"
    # Usar p-value directamente
    effective_p <- p_value
    consumption_gg <- calculate_consumption(temperature, current_weight, effective_p, 
                                            consumption_params, method = "rate")
    consumption_energy <- consumption_gg * mean_prey_energy
  }
  
  # Validar resultados
  effective_p <- pmax(0, pmin(5, effective_p))  # Limitar rango
  
  return(list(
    consumption_gg = pmax(0, consumption_gg),
    consumption_energy = pmax(0, consumption_energy),
    effective_p = effective_p
  ))
}


#' Calcular procesos metabólicos diarios
#'
#' Calcula egestion, excreción, respiración y SDA para un día
#'
#' @param consumption_energy Consumo energético (J/g/día)
#' @param current_weight Peso actual (g)
#' @param temperature Temperatura (°C)
#' @param p_value P-value efectivo
#' @param species_params Parámetros de la especie
#' @param oxycal Coeficiente oxicalórico (J/g O2)
#' @return Lista con procesos metabólicos
#' @keywords internal
calculate_daily_metabolism <- function(consumption_energy, current_weight, temperature,
                                       p_value, species_params, oxycal = 13560) {
  
  # Calcular egestion
  egestion_energy <- calculate_egestion_rate(
    consumption = consumption_energy,
    temperature = temperature,
    p_value = p_value,
    egestion_params = species_params$egestion
  )
  
  # Calcular excreción
  excretion_energy <- calculate_excretion_rate(
    consumption = consumption_energy,
    egestion = egestion_energy,
    temperature = temperature,
    p_value = p_value,
    excretion_params = species_params$excretion
  )
  
  # Calcular respiración
  respiration_o2 <- calculate_respiration_rate(
    weight = current_weight,
    temperature = temperature,
    respiration_params = species_params$respiration
  )
  respiration_energy <- respiration_o2 * oxycal
  
  # Calcular SDA (Acción Dinámica Específica)
  sda_energy <- calculate_sda(
    consumption = consumption_energy,
    egestion = egestion_energy,
    sda_coefficient = species_params$respiration$SDA %||% 0.1
  )
  
  return(list(
    egestion_energy = egestion_energy,
    excretion_energy = excretion_energy,
    respiration_energy = respiration_energy,
    respiration_o2 = respiration_o2,
    sda_energy = sda_energy,
    net_energy = consumption_energy - egestion_energy - excretion_energy - respiration_energy - sda_energy
  ))
}

#' Calcular crecimiento y peso final diario
#'
#' Calcula el peso final del día considerando crecimiento y reproducción
#'
#' @param current_weight Peso inicial del día (g)
#' @param net_energy Energía neta disponible (J/g/día)
#' @param spawn_energy Energía perdida por reproducción (J)
#' @param predator_energy_density Densidad energética del depredador (J/g)
#' @return Lista con peso final y cambio de peso
#' @keywords internal
calculate_daily_growth <- function(current_weight, net_energy, spawn_energy,
                                   predator_energy_density) {
  
  # Energía total disponible para el día
  total_energy_gain <- net_energy * current_weight
  
  # Energía neta después de reproducción
  net_energy_after_spawn <- total_energy_gain - spawn_energy
  
  # Calcular cambio de peso
  weight_change <- net_energy_after_spawn / predator_energy_density
  
  # Peso final
  final_weight <- pmax(0.01, current_weight + weight_change)  # Mínimo 0.01g
  
  return(list(
    final_weight = final_weight,
    weight_change = weight_change,
    net_energy_gain = net_energy_after_spawn
  ))
}

#' Calcular pérdida por reproducción
#'
#' Calcula la energía y peso perdido por eventos reproductivos
#'
#' @param current_weight Peso actual (g)
#' @param spawn_fraction Fracción de peso perdido en reproducción
#' @param predator_energy_density Densidad energética del depredador (J/g)
#' @return Lista con pérdidas reproductivas
#' @keywords internal
calculate_reproductive_loss <- function(current_weight, spawn_fraction, predator_energy_density) {
  
  if (is.na(spawn_fraction) || spawn_fraction <= 0) {
    return(list(spawn_energy = 0, spawn_weight = 0))
  }
  
  spawn_fraction <- clamp(spawn_fraction, 0, 1)
  spawn_weight <- spawn_fraction * current_weight
  spawn_energy <- spawn_weight * predator_energy_density
  
  return(list(
    spawn_energy = spawn_energy,
    spawn_weight = spawn_weight
  ))
}

# ============================================================================
# FUNCIÓN PRINCIPAL DE SIMULACIÓN
# ============================================================================

#' Motor de simulación bioenergética principal
#'
#' Ejecuta la simulación diaria del modelo bioenergético FB4
#'
#' @param bioenergetic_obj Objeto de clase Bioenergetic
#' @param p_value Proporción del consumo máximo (0-5, opcional)
#' @param ration_percent Ración como porcentaje del peso corporal (opcional)
#' @param ration_grams Ración en gramos por día (opcional)
#' @param calculation_method Método de cálculo ("p_value", "ration_percent", "ration_grams")
#' @param return_daily Retornar datos diarios detallados
#' @param oxycal Coeficiente oxicalórico (J/g O2)
#' @return Lista con resultados de la simulación
#' @export
run_bioenergetic_simulation <- function(bioenergetic_obj, p_value = NULL, ration_percent = NULL,
                                        ration_grams = NULL, calculation_method = "p_value",
                                        return_daily = TRUE, oxycal = 13560) {
  
  if (!is.Bioenergetic(bioenergetic_obj)) {
    stop("Objeto debe ser de clase Bioenergetic")
  }
  
  # Validar método de cálculo
  if (!calculation_method %in% c("p_value", "ration_percent", "ration_grams")) {
    stop("calculation_method debe ser 'p_value', 'ration_percent', o 'ration_grams'")
  }
  
  # Validar parámetros según método
  if (calculation_method == "p_value" && is.null(p_value)) {
    stop("p_value requerido para método p_value")
  }
  if (calculation_method == "ration_percent" && is.null(ration_percent)) {
    stop("ration_percent requerido para método ration_percent")
  }
  if (calculation_method == "ration_grams" && is.null(ration_grams)) {
    stop("ration_grams requerido para método ration_grams")
  }
  
  # Extraer datos del objeto
  initial_weight <- bioenergetic_obj$simulation_settings$initial_weight
  species_params <- bioenergetic_obj$species_parameters
  env_data <- bioenergetic_obj$environmental_data
  
  # Validar datos ambientales
  if (is.null(env_data$temperature)) {
    stop("Datos de temperatura requeridos")
  }
  
  temp_data <- env_data$temperature
  n_days <- nrow(temp_data)
  
  # Inicializar variables de simulación
  current_weight <- initial_weight
  total_consumption <- 0
  total_spawn_energy <- 0
  
  # Preparar datos de salida diaria si se requiere
  if (return_daily) {
    daily_results <- data.frame(
      Day = temp_data$Day,
      Temperature = temp_data$Temperature,
      Weight = numeric(n_days),
      Consumption_gg = numeric(n_days),
      Consumption_Energy = numeric(n_days),
      Egestion = numeric(n_days),
      Excretion = numeric(n_days),
      Respiration = numeric(n_days),
      SDA = numeric(n_days),
      Growth_Energy = numeric(n_days),
      Weight_Change = numeric(n_days),
      Spawn_Energy = numeric(n_days),
      Effective_P = numeric(n_days),
      stringsAsFactors = FALSE
    )
  }
  
  # Datos de dieta (usar proporción uniforme si no se especifica)
  if (is.null(env_data$diet)) {
    # Asumir dieta uniforme de una presa con energía típica
    diet_proportions <- 1
    prey_energies <- 4000  # J/g típico
  } else {
    diet_proportions <- env_data$diet$proportions
    prey_energies <- env_data$diet$energy_densities
  }
  
  # Calcular energía media de presa (constante si no hay variación temporal)
  if (is.matrix(diet_proportions)) {
    mean_prey_energy_data <- rowSums(diet_proportions * prey_energies)
  } else {
    mean_prey_energy_data <- rep(sum(diet_proportions * prey_energies), n_days)
  }
  
  # Loop principal de simulación diaria
  for (day in 1:n_days) {
    temperature <- temp_data$Temperature[day]
    mean_prey_energy <- mean_prey_energy_data[min(day, length(mean_prey_energy_data))]
    
    # Calcular densidad energética del depredador
    predator_energy_density <- calculate_predator_energy_density(
      weight = current_weight,
      day = day,
      energy_params = species_params$energy_density
    )
    
    # Calcular consumo del día
    consumption_result <- calculate_daily_consumption(
      current_weight = current_weight,
      temperature = temperature,
      p_value = p_value,
      ration_percent = ration_percent,
      ration_grams = ration_grams,
      method = calculation_method,
      consumption_params = species_params$consumption,
      mean_prey_energy = mean_prey_energy
    )
    
    # Calcular procesos metabólicos
    metabolism_result <- calculate_daily_metabolism(
      consumption_energy = consumption_result$consumption_energy,
      current_weight = current_weight,
      temperature = temperature,
      p_value = consumption_result$effective_p,
      species_params = species_params,
      oxycal = oxycal
    )
    
    # Calcular pérdida por reproducción si hay datos
    spawn_energy <- 0
    if (!is.null(env_data$reproduction)) {
      spawn_fraction <- get_daily_value(env_data$reproduction, day)
      if (!is.na(spawn_fraction) && spawn_fraction > 0) {
        spawn_result <- calculate_reproductive_loss(
          current_weight = current_weight,
          spawn_fraction = spawn_fraction,
          predator_energy_density = predator_energy_density
        )
        spawn_energy <- spawn_result$spawn_energy
        total_spawn_energy <- total_spawn_energy + spawn_energy
      }
    }
    
    # Calcular crecimiento y peso final
    growth_result <- calculate_daily_growth(
      current_weight = current_weight,
      net_energy = metabolism_result$net_energy,
      spawn_energy = spawn_energy,
      predator_energy_density = predator_energy_density
    )
    
    # Validar peso resultante
    if (growth_result$final_weight <= 0) {
      warning("Peso negativo en día ", day, ". Simulación terminada.")
      break
    }
    
    # Guardar resultados diarios si se requiere
    if (return_daily) {
      daily_results$Weight[day] <- growth_result$final_weight
      daily_results$Consumption_gg[day] <- consumption_result$consumption_gg
      daily_results$Consumption_Energy[day] <- consumption_result$consumption_energy
      daily_results$Egestion[day] <- metabolism_result$egestion_energy
      daily_results$Excretion[day] <- metabolism_result$excretion_energy
      daily_results$Respiration[day] <- metabolism_result$respiration_energy
      daily_results$SDA[day] <- metabolism_result$sda_energy
      daily_results$Growth_Energy[day] <- metabolism_result$net_energy
      daily_results$Weight_Change[day] <- growth_result$weight_change
      daily_results$Spawn_Energy[day] <- spawn_energy
      daily_results$Effective_P[day] <- consumption_result$effective_p
    }
    
    # Actualizar variables para siguiente día
    total_consumption <- total_consumption + (consumption_result$consumption_gg * current_weight)
    current_weight <- growth_result$final_weight
  }
  
  # Calcular métricas finales
  weight_gain <- current_weight - initial_weight
  growth_rate <- (log(current_weight) - log(initial_weight)) / n_days
  
  # Preparar resultado
  result <- list(
    initial_weight = initial_weight,
    final_weight = current_weight,
    weight_gain = weight_gain,
    total_consumption = total_consumption,
    daily_growth_rate = growth_rate,
    total_spawn_energy = total_spawn_energy,
    simulation_days = n_days,
    calculation_method = calculation_method
  )
  
  # Agregar datos diarios si se solicitan
  if (return_daily) {
    result$daily_data <- daily_results
  }
  
  # Agregar información específica del método
  if (calculation_method == "p_value") {
    result$p_value <- p_value
  } else if (calculation_method == "ration_percent") {
    result$ration_percent <- ration_percent
  } else if (calculation_method == "ration_grams") {
    result$ration_grams <- ration_grams
  }
  
  return(result)
}

# ============================================================================
# FUNCIONES DE UTILIDAD PARA SIMULACIÓN
# ============================================================================


#' Calcular consumo máximo
#'
#' @param weight Peso del pez (g)
#' @param temperature Temperatura (°C)
#' @param species_params Parámetros de la especie
#' @return Consumo máximo (g/g/día)
#' @keywords internal
calculate_maximum_consumption <- function(weight, temperature, species_params) {
  return(calculate_consumption(temperature, weight, 1.0, species_params$consumption, method = "maximum"))
}

#' Calcular respiración diaria
#'
#' @param weight Peso del pez (g)
#' @param temperature Temperatura (°C)
#' @param consumption Consumo del día
#' @param species_params Parámetros de la especie
#' @return Respiración diaria
#' @keywords internal
calculate_daily_respiration <- function(weight, temperature, consumption, species_params) {
  return(calculate_respiration(temperature, weight, species_params$respiration, consumption))
}


#' Calcular densidad energética del depredador
#'
#' @param weight Peso actual (g)
#' @param day Día de simulación
#' @param energy_params Parámetros de densidad energética
#' @return Densidad energética (J/g)
#' @keywords internal
calculate_predator_energy_density <- function(weight, day, energy_params) {
  
  # Ecuación por defecto si no se especifica
  equation <- energy_params$equation %||% 3
  
  if (equation == 3) {
    # ED = alpha * W^beta
    alpha <- energy_params$alpha %||% 3500
    beta <- energy_params$beta %||% 0.1
    energy_density <- alpha * (weight^beta)
    
  } else if (equation == 2) {
    # Ecuación lineal por segmentos
    alpha1 <- energy_params$alpha1 %||% 3000
    beta1 <- energy_params$beta1 %||% 10
    alpha2 <- energy_params$alpha2 %||% 4000
    beta2 <- energy_params$beta2 %||% 5
    cutoff <- energy_params$cutoff %||% 50
    
    if (weight < cutoff) {
      energy_density <- alpha1 + beta1 * weight
    } else {
      energy_density <- alpha2 + beta2 * weight
    }
    
  } else {
    # Densidad energética constante por defecto
    energy_density <- energy_params$constant %||% 4000
  }
  
  return(max(1000, energy_density))  # Mínimo biológico
}

#' Validar resultado de simulación
#'
#' @param simulation_result Resultado de run_bioenergetic_simulation
#' @return Lista con validación
#' @export
validate_simulation_result <- function(simulation_result) {
  
  validation <- list(
    valid = TRUE,
    warnings = character(),
    errors = character()
  )
  
  # Verificar estructura básica
  required_fields <- c("initial_weight", "final_weight", "total_consumption", "simulation_days")
  missing_fields <- setdiff(required_fields, names(simulation_result))
  
  if (length(missing_fields) > 0) {
    validation$errors <- c(validation$errors,
                           paste("Campos faltantes:", paste(missing_fields, collapse = ", ")))
    validation$valid <- FALSE
  }
  
  # Verificar valores biológicamente realistas
  if ("final_weight" %in% names(simulation_result)) {
    if (simulation_result$final_weight <= 0) {
      validation$errors <- c(validation$errors, "Peso final no válido (≤ 0)")
      validation$valid <- FALSE
    }
    
    # Verificar crecimiento extremo
    if ("initial_weight" %in% names(simulation_result)) {
      weight_ratio <- simulation_result$final_weight / simulation_result$initial_weight
      if (weight_ratio > 100) {
        validation$warnings <- c(validation$warnings, "Crecimiento muy alto (>100x)")
      } else if (weight_ratio < 0.1) {
        validation$warnings <- c(validation$warnings, "Pérdida de peso muy alta (>90%)")
      }
    }
  }
  
  # Verificar consumo
  if ("total_consumption" %in% names(simulation_result)) {
    if (simulation_result$total_consumption < 0) {
      validation$errors <- c(validation$errors, "Consumo total negativo")
      validation$valid <- FALSE
    }
  }
  
  # Verificar datos diarios si están presentes
  if ("daily_data" %in% names(simulation_result)) {
    daily_data <- simulation_result$daily_data
    
    # Verificar pesos no negativos
    if (any(daily_data$Weight <= 0, na.rm = TRUE)) {
      validation$errors <- c(validation$errors, "Pesos negativos en datos diarios")
      validation$valid <- FALSE
    }
    
    # Verificar valores extremos
    if (any(daily_data$Consumption_gg > 2, na.rm = TRUE)) {
      validation$warnings <- c(validation$warnings, "Consumo específico muy alto (>2 g/g/d)")
    }
  }
  
  return(validation)
}

#' Generar resumen de simulación
#'
#' @param simulation_result Resultado de run_bioenergetic_simulation
#' @return Data frame con resumen
#' @export
summarize_simulation_result <- function(simulation_result) {
  
  summary_data <- data.frame(
    Parameter = character(),
    Value = character(),
    Units = character(),
    stringsAsFactors = FALSE
  )
  
  # Información básica
  if ("initial_weight" %in% names(simulation_result)) {
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Initial weight",
                                     Value = round(simulation_result$initial_weight, 2),
                                     Units = "g"))
  }
  
  if ("final_weight" %in% names(simulation_result)) {
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Final weight",
                                     Value = round(simulation_result$final_weight, 2),
                                     Units = "g"))
  }
  
  if ("weight_gain" %in% names(simulation_result)) {
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Weight gain",
                                     Value = round(simulation_result$weight_gain, 2),
                                     Units = "g"))
  }
  
  if ("total_consumption" %in% names(simulation_result)) {
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Total consumption",
                                     Value = round(simulation_result$total_consumption, 2),
                                     Units = "g"))
  }
  
  if ("daily_growth_rate" %in% names(simulation_result)) {
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Daily growth rate",
                                     Value = round(simulation_result$daily_growth_rate, 4),
                                     Units = "d^-1"))
  }
  
  if ("simulation_days" %in% names(simulation_result)) {
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Simulation days",
                                     Value = simulation_result$simulation_days,
                                     Units = "days"))
  }
  
  # Información específica del método
  if ("p_value" %in% names(simulation_result)) {
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "P-value",
                                     Value = round(simulation_result$p_value, 4),
                                     Units = ""))
  }
  
  if ("ration_percent" %in% names(simulation_result)) {
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Ration percent",
                                     Value = round(simulation_result$ration_percent, 2),
                                     Units = "% body weight"))
  }
  
  if ("ration_grams" %in% names(simulation_result)) {
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Ration grams",
                                     Value = round(simulation_result$ration_grams, 2),
                                     Units = "g/day"))
  }
  
  return(summary_data)
}

# ============================================================================
# FUNCIÓN DE ALTO NIVEL PARA USUARIOS
# ============================================================================

#' Ejecutar simulación bioenergética completa
#'
#' Función principal de alto nivel para ejecutar simulaciones bioenergéticas
#'
#' @param species_name Nombre de la especie
#' @param initial_weight Peso inicial (g)
#' @param temperature_data Data frame con datos de temperatura
#' @param simulation_days Número de días a simular (opcional)
#' @param p_value Proporción del consumo máximo (opcional)
#' @param diet_data Data frame con datos de dieta (opcional)
#' @param reproduction_data Vector con eventos reproductivos (opcional)
#' @param return_daily Retornar datos diarios detallados
#' @return Lista con resultados de la simulación
#' @export
simulate_fish_growth <- function(species_name, initial_weight, temperature_data,
                                 simulation_days = NULL, p_value = 0.5, diet_data = NULL,
                                 reproduction_data = NULL, return_daily = TRUE) {
  
  # Crear objeto bioenergético
  bioenergetic_obj <- new_bioenergetic(species_name, initial_weight)
  
  # Agregar datos de temperatura
  bioenergetic_obj <- add_temperature_data(bioenergetic_obj, temperature_data)
  
  # Agregar datos de dieta si se proporcionan
  if (!is.null(diet_data)) {
    bioenergetic_obj <- add_diet_data(bioenergetic_obj, diet_data)
  }
  
  # Agregar datos de reproducción si se proporcionan
  if (!is.null(reproduction_data)) {
    bioenergetic_obj$environmental_data$reproduction <- reproduction_data
  }
  
  # Ejecutar simulación
  result <- run_bioenergetic_simulation(
    bioenergetic_obj = bioenergetic_obj,
    p_value = p_value,
    calculation_method = "p_value",
    return_daily = return_daily
  )
  
  return(result)
}