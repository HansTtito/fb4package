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
                                       p_value, species_params, oxycal = 13560, 
                                       diet_proportions = NULL, indigestible_fractions = NULL) {
  
  # Calcular fracción indigerible total si se proporcionan datos
  total_indigestible_fraction <- 0
  if (!is.null(diet_proportions) && !is.null(indigestible_fractions)) {
    total_indigestible_fraction <- sum(diet_proportions * indigestible_fractions)
  }
  
  # Calcular egestion
  egestion_energy <- calculate_egestion(
    consumption = consumption_energy,
    temperature = temperature,
    p_value = p_value,
    egestion_params = species_params$egestion,
    indigestible_fraction = total_indigestible_fraction
  )
  
  # Calcular excreción
  excretion_energy <- calculate_excretion(
    consumption = consumption_energy,
    egestion = egestion_energy,
    temperature = temperature,
    p_value = p_value,
    excretion_params = species_params$excretion
  )
  
  # Calcular respiración
  respiration_o2 <- calculate_respiration(
    weight = current_weight,
    temperature = temperature,
    respiration_params = species_params$respiration,
    activity_params = species_params$activity
  )
  respiration_energy <- respiration_o2 * oxycal
  
  # Calcular SDA (Acción Dinámica Específica)
  sda_energy <- calculate_sda(
    consumption = consumption_energy,
    egestion = egestion_energy,
    SDA_coeff = species_params$sda$SDA %||% 0.1
  )
  
  return(list(
    egestion_energy = egestion_energy,
    excretion_energy = excretion_energy,
    respiration_energy = respiration_energy,
    respiration_o2 = (respiration_energy + sda_energy)/oxycal,
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



