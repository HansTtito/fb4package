#' Funciones de Regeneración de Nutrientes para el Modelo FB4
#'
#' @name nutrient-regeneration
#' @aliases nutrient-regeneration
NULL

# ============================================================================
# FUNCIONES CORE DE ASIGNACIÓN DE NUTRIENTES
# ============================================================================

#' Asignación de fósforo en el modelo bioenergético
#'
#' Calcula el balance de fósforo en consumo, crecimiento, excreción y egestion
#'
#' @param consumption Vector de consumo por tipo de presa (g/día)
#' @param prey_p_concentrations Vector de concentraciones de P en presas (g P/g peso húmedo)
#' @param p_assimilation_efficiency Vector de eficiencias de asimilación de P (fracción 0-1)
#' @param weight_gain Ganancia de peso del depredador (g/día)
#' @param predator_p_concentration Concentración de P en depredador (g P/g peso húmedo)
#' @return Lista con flujos de fósforo
#' @keywords internal
calculate_phosphorus_allocation <- function(consumption, prey_p_concentrations, 
                                            p_assimilation_efficiency, weight_gain,
                                            predator_p_concentration) {
  
  # Validar entradas
  if (length(consumption) != length(prey_p_concentrations) || 
      length(consumption) != length(p_assimilation_efficiency)) {
    stop("Vectores de entrada deben tener la misma longitud")
  }
  
  # Validar rangos de eficiencias
  if (any(p_assimilation_efficiency < 0 | p_assimilation_efficiency > 1, na.rm = TRUE)) {
    warning("Eficiencias de asimilación fuera de rango [0,1], corrigiendo")
    p_assimilation_efficiency <- clamp(p_assimilation_efficiency, 0, 1)
  }
  
  # Validar concentraciones
  if (any(prey_p_concentrations < 0, na.rm = TRUE) || predator_p_concentration < 0) {
    warning("Concentraciones negativas detectadas, corrigiendo a 0")
    prey_p_concentrations <- pmax(0, prey_p_concentrations, na.rm = TRUE)
    predator_p_concentration <- max(0, predator_p_concentration)
  }
  
  # 1. Fósforo consumido por tipo de presa (g P/día)
  p_consumption_by_prey <- consumption * prey_p_concentrations
  
  # 2. Fósforo total consumido (g P/día)
  p_consumed_total <- sum(p_consumption_by_prey, na.rm = TRUE)
  
  # 3. Fósforo incorporado en crecimiento (g P/día)
  p_growth <- pmax(0, weight_gain) * predator_p_concentration
  
  # 4. Fósforo asimilado total (g P/día)
  p_assimilated <- sum(p_assimilation_efficiency * p_consumption_by_prey, na.rm = TRUE)
  
  # 5. Fósforo excretado (g P/día)
  # P excretado = P asimilado - P crecimiento
  p_excretion <- pmax(0, p_assimilated - p_growth)
  
  # 6. Fósforo en egestion (g P/día)
  # P egestion = P consumido - P asimilado
  p_egestion <- pmax(0, p_consumed_total - p_assimilated)
  
  # Verificar balance de masa
  balance_check <- abs(p_consumed_total - (p_growth + p_excretion + p_egestion))
  if (balance_check > 1e-8) {
    warning("Desbalance de masa en fósforo: ", signif(balance_check, 3))
  }
  
  return(list(
    consumed = p_consumed_total,
    growth = p_growth,
    excretion = p_excretion,
    egestion = p_egestion,
    assimilated = p_assimilated,
    assimilation_efficiency = if (p_consumed_total > 0) p_assimilated / p_consumed_total else 0
  ))
}

#' Asignación de nitrógeno en el modelo bioenergético
#'
#' Calcula el balance de nitrógeno en consumo, crecimiento, excreción y egestion
#'
#' @param consumption Vector de consumo por tipo de presa (g/día)
#' @param prey_n_concentrations Vector de concentraciones de N en presas (g N/g peso húmedo)
#' @param n_assimilation_efficiency Vector de eficiencias de asimilación de N (fracción 0-1)
#' @param weight_gain Ganancia de peso del depredador (g/día)
#' @param predator_n_concentration Concentración de N en depredador (g N/g peso húmedo)
#' @return Lista con flujos de nitrógeno
#' @keywords internal
calculate_nitrogen_allocation <- function(consumption, prey_n_concentrations,
                                          n_assimilation_efficiency, weight_gain,
                                          predator_n_concentration) {
  
  # Validar entradas
  if (length(consumption) != length(prey_n_concentrations) || 
      length(consumption) != length(n_assimilation_efficiency)) {
    stop("Vectores de entrada deben tener la misma longitud")
  }
  
  # Validar rangos de eficiencias
  if (any(n_assimilation_efficiency < 0 | n_assimilation_efficiency > 1, na.rm = TRUE)) {
    warning("Eficiencias de asimilación fuera de rango [0,1], corrigiendo")
    n_assimilation_efficiency <- clamp(n_assimilation_efficiency, 0, 1)
  }
  
  # Validar concentraciones
  if (any(prey_n_concentrations < 0, na.rm = TRUE) || predator_n_concentration < 0) {
    warning("Concentraciones negativas detectadas, corrigiendo a 0")
    prey_n_concentrations <- pmax(0, prey_n_concentrations, na.rm = TRUE)
    predator_n_concentration <- max(0, predator_n_concentration)
  }
  
  # 1. Nitrógeno consumido por tipo de presa (g N/día)
  n_consumption_by_prey <- consumption * prey_n_concentrations
  
  # 2. Nitrógeno total consumido (g N/día)
  n_consumed_total <- sum(n_consumption_by_prey, na.rm = TRUE)
  
  # 3. Nitrógeno incorporado en crecimiento (g N/día)
  n_growth <- pmax(0, weight_gain) * predator_n_concentration
  
  # 4. Nitrógeno asimilado total (g N/día)
  n_assimilated <- sum(n_assimilation_efficiency * n_consumption_by_prey, na.rm = TRUE)
  
  # 5. Nitrógeno excretado (g N/día)
  # N excretado = N asimilado - N crecimiento
  n_excretion <- pmax(0, n_assimilated - n_growth)
  
  # 6. Nitrógeno en egestion (g N/día)
  # N egestion = N consumido - N asimilado
  n_egestion <- pmax(0, n_consumed_total - n_assimilated)
  
  # Verificar balance de masa
  balance_check <- abs(n_consumed_total - (n_growth + n_excretion + n_egestion))
  if (balance_check > 1e-8) {
    warning("Desbalance de masa en nitrógeno: ", signif(balance_check, 3))
  }
  
  return(list(
    consumed = n_consumed_total,
    growth = n_growth,
    excretion = n_excretion,
    egestion = n_egestion,
    assimilated = n_assimilated,
    assimilation_efficiency = if (n_consumed_total > 0) n_assimilated / n_consumed_total else 0
  ))
}

# ============================================================================
# FUNCIÓN PRINCIPAL DE NUTRIENTES
# ============================================================================

#' Calcular balance de nutrientes
#'
#' Función principal para calcular los flujos de nitrógeno y fósforo
#'
#' @param consumption Vector de consumo por tipo de presa (g/día)
#' @param weight_gain Ganancia de peso del depredador (g/día)
#' @param nutrient_params Lista con parámetros de nutrientes
#' @return Lista con resultados de nutrientes
#' @export
calculate_nutrient_balance <- function(consumption, weight_gain, nutrient_params) {
  
  # Validaciones básicas
  if (is.null(nutrient_params)) {
    stop("nutrient_params no puede ser NULL")
  }
  
  weight_gain <- check_numeric_value(weight_gain, "weight_gain", min_val = -Inf)
  
  # Extraer parámetros con valores por defecto
  prey_n_concentrations <- nutrient_params$prey_n_concentrations %||% 
    rep(0.08, length(consumption))  # 8% N típico
  prey_p_concentrations <- nutrient_params$prey_p_concentrations %||% 
    rep(0.01, length(consumption))  # 1% P típico
  
  predator_n_concentration <- nutrient_params$predator_n_concentration %||% 0.09  # 9% N
  predator_p_concentration <- nutrient_params$predator_p_concentration %||% 0.015  # 1.5% P
  
  n_assimilation_efficiency <- nutrient_params$n_assimilation_efficiency %||% 
    rep(0.85, length(consumption))
  p_assimilation_efficiency <- nutrient_params$p_assimilation_efficiency %||% 
    rep(0.80, length(consumption))
  
  # Ajustar longitudes de vectores si es necesario
  n_prey <- length(consumption)
  
  if (length(prey_n_concentrations) == 1) {
    prey_n_concentrations <- rep(prey_n_concentrations, n_prey)
  } else if (length(prey_n_concentrations) != n_prey) {
    stop("Longitud de prey_n_concentrations no coincide con consumption")
  }
  
  if (length(prey_p_concentrations) == 1) {
    prey_p_concentrations <- rep(prey_p_concentrations, n_prey)
  } else if (length(prey_p_concentrations) != n_prey) {
    stop("Longitud de prey_p_concentrations no coincide con consumption")
  }
  
  if (length(n_assimilation_efficiency) == 1) {
    n_assimilation_efficiency <- rep(n_assimilation_efficiency, n_prey)
  } else if (length(n_assimilation_efficiency) != n_prey) {
    stop("Longitud de n_assimilation_efficiency no coincide con consumption")
  }
  
  if (length(p_assimilation_efficiency) == 1) {
    p_assimilation_efficiency <- rep(p_assimilation_efficiency, n_prey)
  } else if (length(p_assimilation_efficiency) != n_prey) {
    stop("Longitud de p_assimilation_efficiency no coincide con consumption")
  }
  
  # Calcular flujos de nitrógeno
  nitrogen_result <- calculate_nitrogen_allocation(
    consumption = consumption,
    prey_n_concentrations = prey_n_concentrations,
    n_assimilation_efficiency = n_assimilation_efficiency,
    weight_gain = weight_gain,
    predator_n_concentration = predator_n_concentration
  )
  
  # Calcular flujos de fósforo
  phosphorus_result <- calculate_phosphorus_allocation(
    consumption = consumption,
    prey_p_concentrations = prey_p_concentrations,
    p_assimilation_efficiency = p_assimilation_efficiency,
    weight_gain = weight_gain,
    predator_p_concentration = predator_p_concentration
  )
  
  # Calcular ratios N:P
  np_ratios <- calculate_np_ratios(nitrogen_result, phosphorus_result)
  
  # Calcular eficiencias
  efficiencies <- calculate_nutrient_efficiencies(nitrogen_result, phosphorus_result)
  
  return(list(
    nitrogen = nitrogen_result,
    phosphorus = phosphorus_result,
    np_ratios = np_ratios,
    efficiencies = efficiencies,
    weight_gain = weight_gain
  ))
}

# ============================================================================
# FUNCIONES DE ANÁLISIS
# ============================================================================

#' Calcular ratios N:P para todos los procesos
#'
#' Calcula ratios molares y de masa N:P para consumo, crecimiento, excreción y egestion
#'
#' @param nitrogen_fluxes Lista resultado de calculate_nitrogen_allocation
#' @param phosphorus_fluxes Lista resultado de calculate_phosphorus_allocation
#' @param ratio_type Tipo de ratio ("mass" o "molar")
#' @return Lista con ratios N:P
#' @export
calculate_np_ratios <- function(nitrogen_fluxes, phosphorus_fluxes, ratio_type = "mass") {
  
  if (!ratio_type %in% c("mass", "molar")) {
    stop("ratio_type debe ser 'mass' o 'molar'")
  }
  
  # Factores de conversión para ratios molares
  atomic_weight_N <- 14.007
  atomic_weight_P <- 30.974
  
  # Procesos a calcular
  processes <- c("consumed", "growth", "excretion", "egestion")
  ratios <- numeric(length(processes))
  names(ratios) <- processes
  
  for (i in seq_along(processes)) {
    process <- processes[i]
    
    n_flux <- nitrogen_fluxes[[process]]
    p_flux <- phosphorus_fluxes[[process]]
    
    if (is.null(n_flux) || is.null(p_flux)) {
      ratios[i] <- NA
      next
    }
    
    if (p_flux == 0) {
      ratios[i] <- if (n_flux == 0) NaN else Inf
    } else {
      if (ratio_type == "mass") {
        ratios[i] <- n_flux / p_flux
      } else {  # molar
        mol_n <- n_flux / atomic_weight_N
        mol_p <- p_flux / atomic_weight_P
        ratios[i] <- mol_n / mol_p
      }
    }
  }
  
  return(list(
    ratios = ratios,
    ratio_type = ratio_type,
    redfield_ratio = if (ratio_type == "molar") 16 else 7.2
  ))
}

#' Calcular eficiencias de retención de nutrientes
#'
#' Calcula eficiencias de asimilación y retención para N y P
#'
#' @param nitrogen_fluxes Lista resultado de calculate_nitrogen_allocation
#' @param phosphorus_fluxes Lista resultado de calculate_phosphorus_allocation
#' @return Lista con eficiencias calculadas
#' @export
calculate_nutrient_efficiencies <- function(nitrogen_fluxes, phosphorus_fluxes) {
  
  # Eficiencias de nitrógeno
  n_consumed <- nitrogen_fluxes$consumed
  n_growth <- nitrogen_fluxes$growth
  n_excretion <- nitrogen_fluxes$excretion
  n_assimilated <- nitrogen_fluxes$assimilated
  
  n_retention_efficiency <- if (n_consumed > 0) n_growth / n_consumed else 0
  n_excretion_rate <- if (n_consumed > 0) n_excretion / n_consumed else 0
  n_growth_efficiency <- if (n_assimilated > 0) n_growth / n_assimilated else 0
  
  # Eficiencias de fósforo
  p_consumed <- phosphorus_fluxes$consumed
  p_growth <- phosphorus_fluxes$growth
  p_excretion <- phosphorus_fluxes$excretion
  p_assimilated <- phosphorus_fluxes$assimilated
  
  p_retention_efficiency <- if (p_consumed > 0) p_growth / p_consumed else 0
  p_excretion_rate <- if (p_consumed > 0) p_excretion / p_consumed else 0
  p_growth_efficiency <- if (p_assimilated > 0) p_growth / p_assimilated else 0
  
  return(list(
    nitrogen = list(
      assimilation_efficiency = nitrogen_fluxes$assimilation_efficiency,
      retention_efficiency = n_retention_efficiency,
      excretion_rate = n_excretion_rate,
      growth_efficiency = n_growth_efficiency
    ),
    phosphorus = list(
      assimilation_efficiency = phosphorus_fluxes$assimilation_efficiency,
      retention_efficiency = p_retention_efficiency,
      excretion_rate = p_excretion_rate,
      growth_efficiency = p_growth_efficiency
    ),
    # Eficiencias relativas
    relative_n_retention = if (p_retention_efficiency > 0) n_retention_efficiency / p_retention_efficiency else NA,
    relative_n_excretion = if (p_excretion_rate > 0) n_excretion_rate / p_excretion_rate else NA
  ))
}

#' Validar concentraciones de nutrientes
#'
#' Verifica que las concentraciones estén en rangos biológicamente realistas
#'
#' @param nutrient_concentrations Lista con concentraciones de N y P
#' @param organism_type Tipo de organismo para validación
#' @return Lista con resultados de validación
#' @export
validate_nutrient_concentrations <- function(nutrient_concentrations, organism_type = "fish") {
  
  validation <- list(
    valid = TRUE,
    warnings = character(),
    errors = character()
  )
  
  # Rangos típicos (g/g peso húmedo)
  typical_ranges <- list(
    fish = list(nitrogen = c(0.08, 0.12), phosphorus = c(0.01, 0.02)),
    zooplankton = list(nitrogen = c(0.07, 0.11), phosphorus = c(0.008, 0.015)),
    invertebrates = list(nitrogen = c(0.06, 0.10), phosphorus = c(0.006, 0.012))
  )
  
  if (!organism_type %in% names(typical_ranges)) {
    validation$warnings <- c(validation$warnings, 
                             paste("Tipo de organismo no reconocido:", organism_type))
    organism_type <- "fish"
  }
  
  ranges <- typical_ranges[[organism_type]]
  
  # Validar nitrógeno
  if ("nitrogen" %in% names(nutrient_concentrations)) {
    n_values <- nutrient_concentrations$nitrogen
    
    if (any(n_values < 0, na.rm = TRUE)) {
      validation$errors <- c(validation$errors, "Concentraciones de nitrógeno negativas")
      validation$valid <- FALSE
    }
    
    if (any(n_values < ranges$nitrogen[1] | n_values > ranges$nitrogen[2], na.rm = TRUE)) {
      validation$warnings <- c(validation$warnings, 
                               paste("Concentraciones de N fuera de rango típico para", organism_type))
    }
  }
  
  # Validar fósforo
  if ("phosphorus" %in% names(nutrient_concentrations)) {
    p_values <- nutrient_concentrations$phosphorus
    
    if (any(p_values < 0, na.rm = TRUE)) {
      validation$errors <- c(validation$errors, "Concentraciones de fósforo negativas")
      validation$valid <- FALSE
    }
    
    if (any(p_values < ranges$phosphorus[1] | p_values > ranges$phosphorus[2], na.rm = TRUE)) {
      validation$warnings <- c(validation$warnings, 
                               paste("Concentraciones de P fuera de rango típico para", organism_type))
    }
  }
  
  # Validar ratio N:P
  if ("nitrogen" %in% names(nutrient_concentrations) && 
      "phosphorus" %in% names(nutrient_concentrations)) {
    
    n_values <- nutrient_concentrations$nitrogen
    p_values <- nutrient_concentrations$phosphorus
    
    # Evitar división por cero
    valid_indices <- !is.na(p_values) & p_values > 0
    if (any(valid_indices)) {
      np_ratios <- n_values[valid_indices] / p_values[valid_indices]
      
      # Rango típico para ratios N:P (masa): 4-15
      if (any(np_ratios < 2 | np_ratios > 20, na.rm = TRUE)) {
        validation$warnings <- c(validation$warnings, 
                                 "Ratios N:P fuera de rango típico (2-20)")
      }
    }
  }
  
  return(validation)
}

# ============================================================================
# FUNCIONES DE ALTO NIVEL
# ============================================================================


#' Estimar impacto ecosistémico de excreción de nutrientes
#'
#' Calcula el impacto potencial en el ecosistema de la excreción de nutrientes
#'
#' @param daily_n_excretion Excreción diaria de nitrógeno (g N/día)
#' @param daily_p_excretion Excreción diaria de fósforo (g P/día)
#' @param fish_biomass Biomasa total de peces en el sistema (g)
#' @param water_volume Volumen de agua del sistema (L)
#' @param simulation_days Número de días de simulación
#' @return Lista con estimaciones de impacto ecosistémico
#' @export
calculate_ecosystem_impact <- function(daily_n_excretion, daily_p_excretion, fish_biomass,
                                       water_volume, simulation_days) {
  
  # Validar entradas
  fish_biomass <- check_numeric_value(fish_biomass, "fish_biomass", min_val = 0)
  water_volume <- check_numeric_value(water_volume, "water_volume", min_val = 1)
  simulation_days <- check_numeric_value(simulation_days, "simulation_days", min_val = 1)
  
  # Excreción total para toda la población de peces
  total_n_excretion_per_day <- daily_n_excretion * fish_biomass  # g N/día
  total_p_excretion_per_day <- daily_p_excretion * fish_biomass  # g P/día
  
  # Excreción acumulada durante período de simulación
  total_n_excretion <- total_n_excretion_per_day * simulation_days  # g N
  total_p_excretion <- total_p_excretion_per_day * simulation_days  # g P
  
  # Concentraciones potenciales en agua (mg/L)
  # Asumiendo mezcla completa y sin pérdidas
  n_concentration_mgL <- (total_n_excretion / water_volume) * 1000
  p_concentration_mgL <- (total_p_excretion / water_volume) * 1000
  
  # Adición diaria de nutrientes (mg/L/día)
  daily_n_addition <- (total_n_excretion_per_day / water_volume) * 1000
  daily_p_addition <- (total_p_excretion_per_day / water_volume) * 1000
  
  # Ratio N:P en excreción
  excretion_np_ratio <- if (total_p_excretion > 0) total_n_excretion / total_p_excretion else Inf
  
  # Estimación simple del estado trófico basado en P
  trophic_status <- if (p_concentration_mgL < 0.01) {
    "Oligotrófico"
  } else if (p_concentration_mgL < 0.05) {
    "Mesotrófico"
  } else {
    "Eutrófico"
  }
  
  return(list(
    # Excreción total
    total_n_excretion_g = total_n_excretion,
    total_p_excretion_g = total_p_excretion,
    
    # Tasas diarias
    daily_n_excretion_g = total_n_excretion_per_day,
    daily_p_excretion_g = total_p_excretion_per_day,
    
    # Concentraciones potenciales
    n_concentration_mgL = n_concentration_mgL,
    p_concentration_mgL = p_concentration_mgL,
    daily_n_addition_mgL = daily_n_addition,
    daily_p_addition_mgL = daily_p_addition,
    
    # Evaluación
    excretion_np_ratio = excretion_np_ratio,
    trophic_status = trophic_status,
    
    # Parámetros del sistema
    fish_biomass_g = fish_biomass,
    water_volume_L = water_volume,
    simulation_days = simulation_days
  ))
}

#' Generar parámetros de ejemplo para nutrientes
#'
#' Crea parámetros típicos para diferentes tipos de organismos
#'
#' @param organism_type Tipo de organismo ("fish", "zooplankton", "invertebrates")
#' @param n_prey Número de tipos de presa
#' @return Lista con parámetros de ejemplo
#' @export
generate_example_nutrient_params <- function(organism_type = "fish", n_prey = 2) {
  
  # Parámetros típicos por tipo de organismo
  if (organism_type == "fish") {
    params <- list(
      # Concentraciones en presas (g/g peso húmedo)
      prey_n_concentrations = c(0.095, 0.072)[1:n_prey],  # 9.5% y 7.2% N
      prey_p_concentrations = c(0.012, 0.008)[1:n_prey],  # 1.2% y 0.8% P
      
      # Concentraciones en depredador
      predator_n_concentration = 0.098,  # 9.8% N
      predator_p_concentration = 0.015,  # 1.5% P
      
      # Eficiencias de asimilación
      n_assimilation_efficiency = rep(0.85, n_prey),  # 85% para N
      p_assimilation_efficiency = rep(0.80, n_prey)   # 80% para P
    )
    
  } else if (organism_type == "zooplankton") {
    params <- list(
      prey_n_concentrations = c(0.080, 0.060)[1:n_prey],  # 8% y 6% N
      prey_p_concentrations = c(0.010, 0.007)[1:n_prey],  # 1% y 0.7% P
      
      predator_n_concentration = 0.090,  # 9% N
      predator_p_concentration = 0.012,  # 1.2% P
      
      n_assimilation_efficiency = rep(0.90, n_prey),  # 90% para N
      p_assimilation_efficiency = rep(0.85, n_prey)   # 85% para P
    )
    
  } else if (organism_type == "invertebrates") {
    params <- list(
      prey_n_concentrations = c(0.070, 0.055)[1:n_prey],  # 7% y 5.5% N
      prey_p_concentrations = c(0.008, 0.006)[1:n_prey],  # 0.8% y 0.6% P
      
      predator_n_concentration = 0.075,  # 7.5% N
      predator_p_concentration = 0.010,  # 1% P
      
      n_assimilation_efficiency = rep(0.75, n_prey),  # 75% para N
      p_assimilation_efficiency = rep(0.70, n_prey)   # 70% para P
    )
    
  } else {
    # Parámetros genéricos
    params <- list(
      prey_n_concentrations = rep(0.08, n_prey),
      prey_p_concentrations = rep(0.01, n_prey),
      predator_n_concentration = 0.09,
      predator_p_concentration = 0.015,
      n_assimilation_efficiency = rep(0.80, n_prey),
      p_assimilation_efficiency = rep(0.75, n_prey)
    )
  }
  
  return(params)
}

# ============================================================================
# FUNCIONES DE ANÁLISIS COMPARATIVO
# ============================================================================

#' Comparar con ratios de Redfield
#'
#' Compara ratios N:P calculados con el ratio de Redfield
#'
#' @param np_ratios Lista resultado de calculate_np_ratios
#' @return Data frame con comparación
#' @export
compare_with_redfield <- function(np_ratios) {
  
  redfield_ratio <- np_ratios$redfield_ratio
  
  comparison <- data.frame(
    Process = names(np_ratios$ratios),
    NP_Ratio = np_ratios$ratios,
    Redfield_Ratio = redfield_ratio,
    Difference = np_ratios$ratios - redfield_ratio,
    Relative_Difference = ((np_ratios$ratios - redfield_ratio) / redfield_ratio) * 100,
    stringsAsFactors = FALSE
  )
  
  # Añadir interpretación
  comparison$Interpretation <- ifelse(
    is.infinite(comparison$NP_Ratio), "Sin P disponible",
    ifelse(is.nan(comparison$NP_Ratio), "Sin flujo",
           ifelse(comparison$NP_Ratio > redfield_ratio,
                  "Rico en N relativo a P",
                  "Pobre en N relativo a P"))
  )
  
  return(comparison)
}

#' Calcular balance estequiométrico
#'
#' Analiza limitaciones nutricionales basadas en ratios N:P
#'
#' @param nutrient_balance Lista resultado de calculate_nutrient_balance
#' @return Lista con análisis estequiométrico
#' @export
calculate_stoichiometric_balance <- function(nutrient_balance) {
  
  np_ratios <- nutrient_balance$np_ratios
  nitrogen <- nutrient_balance$nitrogen
  phosphorus <- nutrient_balance$phosphorus
  
  # Determinar limitación nutricional en el consumo
  consumption_np <- np_ratios$ratios["consumed"]
  redfield_ratio <- np_ratios$redfield_ratio
  
  if (is.finite(consumption_np)) {
    if (consumption_np > redfield_ratio) {
      nutrient_limitation <- "Limitado por P"
      limiting_nutrient <- "phosphorus"
    } else {
      nutrient_limitation <- "Limitado por N"
      limiting_nutrient <- "nitrogen"
    }
  } else {
    nutrient_limitation <- "Indeterminado"
    limiting_nutrient <- "unknown"
  }
  
  # Calcular exceso nutricional
  if (limiting_nutrient == "phosphorus") {
    # Exceso de N relativo a P
    excess_factor <- consumption_np / redfield_ratio
    excess_nutrient <- "nitrogen"
  } else if (limiting_nutrient == "nitrogen") {
    # Exceso de P relativo a N
    excess_factor <- redfield_ratio / consumption_np
    excess_nutrient <- "phosphorus"
  } else {
    excess_factor <- 1
    excess_nutrient <- "none"
  }
  
  # Eficiencia de uso de nutriente limitante
  if (limiting_nutrient == "nitrogen") {
    limiting_efficiency <- nutrient_balance$efficiencies$nitrogen$retention_efficiency
  } else if (limiting_nutrient == "phosphorus") {
    limiting_efficiency <- nutrient_balance$efficiencies$phosphorus$retention_efficiency
  } else {
    limiting_efficiency <- NA
  }
  
  return(list(
    nutrient_limitation = nutrient_limitation,
    limiting_nutrient = limiting_nutrient,
    excess_nutrient = excess_nutrient,
    excess_factor = excess_factor,
    limiting_efficiency = limiting_efficiency,
    consumption_np_ratio = consumption_np,
    redfield_ratio = redfield_ratio,
    np_deviation = consumption_np - redfield_ratio
  ))
}


# ============================================================================
# FUNCIÓN DE EJEMPLO INTEGRADO
# ============================================================================

#' Ejemplo completo de análisis de nutrientes
#'
#' Demuestra el uso integrado de las funciones de nutrientes
#'
#' @param consumption Vector de consumo por presa (g/día)
#' @param weight_gain Ganancia de peso (g/día)
#' @param organism_type Tipo de organismo para parámetros por defecto
#' @param custom_params Parámetros personalizados (opcional)
#' @return Lista con análisis completo
#' @export
nutrient_analysis_example <- function(consumption, weight_gain, organism_type = "fish",
                                      custom_params = NULL) {
  
  # Usar parámetros personalizados o generar ejemplos
  if (is.null(custom_params)) {
    nutrient_params <- generate_example_nutrient_params(organism_type, length(consumption))
  } else {
    nutrient_params <- custom_params
  }
  
  # 1. Calcular balance de nutrientes
  balance <- calculate_nutrient_balance(consumption, weight_gain, nutrient_params)
  
  # 2. Análisis estequiométrico
  stoichiometry <- calculate_stoichiometric_balance(balance)
  
  # 3. Comparación con Redfield
  redfield_comparison <- compare_with_redfield(balance$np_ratios)
  
  # 4. Validar concentraciones
  validation <- validate_nutrient_concentrations(
    list(nitrogen = nutrient_params$predator_n_concentration,
         phosphorus = nutrient_params$predator_p_concentration),
    organism_type = organism_type
  )
  
  # 5. Crear tabla resumen
  summary_table <- data.frame(
    Process = c("Consumed", "Growth", "Excretion", "Egestion"),
    Nitrogen_g = c(balance$nitrogen$consumed, balance$nitrogen$growth,
                   balance$nitrogen$excretion, balance$nitrogen$egestion),
    Phosphorus_g = c(balance$phosphorus$consumed, balance$phosphorus$growth,
                     balance$phosphorus$excretion, balance$phosphorus$egestion),
    NP_Ratio = balance$np_ratios$ratios,
    stringsAsFactors = FALSE
  )
  
  # 6. Calcular impacto ecosistémico (ejemplo con valores típicos)
  ecosystem_impact <- calculate_ecosystem_impact(
    daily_n_excretion = balance$nitrogen$excretion,
    daily_p_excretion = balance$phosphorus$excretion,
    fish_biomass = 1000,  # 1 kg de peces
    water_volume = 10000,  # 10 m³ de agua
    simulation_days = 365
  )
  
  return(list(
    summary_table = summary_table,
    nutrient_balance = balance,
    stoichiometric_analysis = stoichiometry,
    redfield_comparison = redfield_comparison,
    validation = validation,
    ecosystem_impact = ecosystem_impact,
    input_parameters = list(
      consumption = consumption,
      weight_gain = weight_gain,
      organism_type = organism_type,
      nutrient_params = nutrient_params
    )
  ))
}