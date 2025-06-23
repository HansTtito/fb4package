#' Funciones de Egestión y Excreción para el Modelo FB4
#'
#' @name egestion-excretion
#' @aliases egestion-excretion
NULL

# ============================================================================
# FUNCIONES CORE DE EGESTIÓN
# ============================================================================

#' Modelo de egestión 1 - Básico
#'
#' Modelo simple de egestión con fracción constante del consumo
#'
#' @param consumption Consumo (J/g)
#' @param FA Fracción de egestión
#' @return Egestión (J/g)
#' @keywords internal
egestion_model_1 <- function(consumption, FA) {
  if (is.na(consumption) || is.na(FA) || consumption <= 0) return(0)
  
  egestion <- FA * consumption
  return(pmax(0, pmin(egestion, consumption)))
}

#' Modelo de egestión 2 - Elliott (1976)
#'
#' Modelo de egestión dependiente de temperatura y nivel de alimentación
#'
#' @param consumption Consumo (J/g)
#' @param temperature Temperatura del agua (°C)
#' @param p_value Proporción del consumo máximo (p-value)
#' @param FA Parámetro base de egestión
#' @param FB Coeficiente de dependencia de temperatura
#' @param FG Coeficiente de dependencia del nivel de alimentación
#' @return Egestión (J/g)
#' @keywords internal
egestion_model_2 <- function(consumption, temperature, p_value, FA, FB, FG) {
  if (any(is.na(c(consumption, temperature, p_value, FA, FB, FG))) || consumption <= 0) {
    return(0)
  }
  
  # Cálculos seguros
  safe_temp <- clamp(temperature, 0, 50)
  safe_p <- clamp(p_value, 0, 5)
  safe_FB <- clamp(FB, -5, 5)
  safe_FG <- clamp(FG, -5, 5)
  
  egestion <- FA * (safe_temp^safe_FB) * safe_exp(safe_FG * safe_p) * consumption
  return(pmax(0, pmin(egestion, consumption)))
}

#' Modelo de egestión 3 - Stewart et al. (1983)
#'
#' Modelo que incluye presas indigeribles
#'
#' @param consumption Consumo (J/g)
#' @param temperature Temperatura del agua (°C)
#' @param p_value Proporción del consumo máximo (p-value)
#' @param FA Parámetro base de egestión
#' @param FB Coeficiente de dependencia de temperatura
#' @param FG Coeficiente de dependencia del nivel de alimentación
#' @param indigestible_fraction Fracción indigerible de las presas
#' @return Egestión (J/g)
#' @keywords internal
egestion_model_3 <- function(consumption, temperature, p_value, FA, FB, FG, 
                             indigestible_fraction = 0) {
  
  if (any(is.na(c(consumption, temperature, p_value, FA, FB, FG))) || consumption <= 0) {
    return(0)
  }
  
  # Cálculos seguros
  safe_temp <- clamp(temperature, 0, 50)
  safe_p <- clamp(p_value, 0, 5)
  safe_FB <- clamp(FB, -5, 5)
  safe_FG <- clamp(FG, -5, 5)
  safe_indig <- clamp(indigestible_fraction, 0, 1)
  
  # Calcular eficiencia de egestión base
  PE <- FA * (safe_temp^safe_FB) * safe_exp(safe_FG * safe_p)
  
  # Ajustar por presas indigeribles
  PF <- ((PE - 0.1) / 0.9) * (1 - safe_indig) + safe_indig
  PF <- clamp(PF, 0, 1)
  
  egestion <- PF * consumption
  
  return(pmax(0, pmin(egestion, consumption)))
  
}

#' Modelo de egestión 4 - Elliott (1976) sin p-value
#'
#' Modelo simplificado sin dependencia del nivel de alimentación
#'
#' @param consumption Consumo (J/g)
#' @param temperature Temperatura del agua (°C)
#' @param FA Parámetro base de egestión
#' @param FB Coeficiente de dependencia de temperatura
#' @return Egestión (J/g)
#' @keywords internal
egestion_model_4 <- function(consumption, temperature, FA, FB) {
  if (any(is.na(c(consumption, temperature, FA, FB))) || consumption <= 0) {
    return(0)
  }
  
  safe_temp <- clamp(temperature, 0, 50)
  safe_FB <- clamp(FB, -5, 5)
  
  egestion <- FA * (safe_temp^safe_FB) * consumption
  return(pmax(0, pmin(egestion, consumption)))
}

# ============================================================================
# FUNCIONES CORE DE EXCRECIÓN
# ============================================================================

#' Modelo de excreción 1 - Básico
#'
#' Modelo simple de excreción proporcional a la materia asimilada
#'
#' @param consumption Consumo (J/g)
#' @param egestion Egestión (J/g)
#' @param UA Fracción de excreción
#' @return Excreción (J/g)
#' @keywords internal
excretion_model_1 <- function(consumption, egestion, UA) {
  if (any(is.na(c(consumption, egestion, UA))) || consumption <= 0) {
    return(0)
  }
  
  assimilated <- pmax(0, consumption - egestion)
  excretion <- UA * assimilated
  
  return(pmax(0, pmin(excretion, assimilated)))
}

#' Modelo de excreción 2 - Con dependencia de temperatura y alimentación
#'
#' @param consumption Consumo (J/g)
#' @param egestion Egestión (J/g)
#' @param temperature Temperatura del agua (°C)
#' @param p_value Proporción del consumo máximo (p-value)
#' @param UA Parámetro base de excreción
#' @param UB Coeficiente de dependencia de temperatura
#' @param UG Coeficiente de dependencia del nivel de alimentación
#' @return Excreción (J/g)
#' @keywords internal
excretion_model_2 <- function(consumption, egestion, temperature, p_value, UA, UB, UG) {
  if (any(is.na(c(consumption, egestion, temperature, p_value, UA, UB, UG))) || 
      consumption <= 0) {
    return(0)
  }
  
  assimilated <- pmax(0, consumption - egestion)
  if (assimilated <= 0) return(0)
  
  # Cálculos seguros
  safe_temp <- clamp(temperature, 0, 50)
  safe_p <- clamp(p_value, 0, 5)
  safe_UB <- clamp(UB, -5, 5)
  safe_UG <- clamp(UG, -5, 5)
  
  excretion <- UA * (safe_temp^safe_UB) * safe_exp(safe_UG * safe_p) * assimilated
  return(pmax(0, pmin(excretion, assimilated)))
}

#' Modelo de excreción 3 - Variante del modelo 2
#'
#' @param consumption Consumo (J/g)
#' @param egestion Egestión (J/g)
#' @param temperature Temperatura del agua (°C)
#' @param p_value Proporción del consumo máximo (p-value)
#' @param UA Parámetro base de excreción
#' @param UB Coeficiente de dependencia de temperatura
#' @param UG Coeficiente de dependencia del nivel de alimentación
#' @return Excreción (J/g)
#' @keywords internal
excretion_model_3 <- function(consumption, egestion, temperature, p_value, UA, UB, UG) {
  # Idéntico al modelo 2 en esta implementación
  return(excretion_model_2(consumption, egestion, temperature, p_value, UA, UB, UG))
}

#' Modelo de excreción 4 - Sin dependencia de alimentación
#'
#' @param consumption Consumo (J/g)
#' @param egestion Egestión (J/g)
#' @param temperature Temperatura del agua (°C)
#' @param UA Parámetro base de excreción
#' @param UB Coeficiente de dependencia de temperatura
#' @return Excreción (J/g)
#' @keywords internal
excretion_model_4 <- function(consumption, egestion, temperature, UA, UB) {
  if (any(is.na(c(consumption, egestion, temperature, UA, UB))) || consumption <= 0) {
    return(0)
  }
  
  assimilated <- pmax(0, consumption - egestion)
  if (assimilated <= 0) return(0)
  
  safe_temp <- clamp(temperature, 0, 50)
  safe_UB <- clamp(UB, -5, 5)
  
  excretion <- UA * (safe_temp^safe_UB) * assimilated
  return(pmax(0, pmin(excretion, assimilated)))
}

# ============================================================================
# FUNCIONES PRINCIPALES
# ============================================================================


#' Calcular egestión diaria
#'
#' @param consumption Consumo (J/g)
#' @param temperature Temperatura del agua (°C)
#' @param p_value Proporción del consumo máximo (p-value)
#' @param egestion_params Lista con parámetros de egestión
#' @param indigestible_fraction Fracción indigerible (para modelo 3)
#' @return Egestión (J/g)
#' @export
calculate_egestion <- function(consumption, temperature, p_value, egestion_params, 
                               indigestible_fraction = 0) {
  
  # Validaciones básicas
  if (is.null(egestion_params)) {
    stop("egestion_params no puede ser NULL")
  }
  
  if (!"FA" %in% names(egestion_params)) {
    stop("Parámetro FA requerido para egestión")
  }
  
  # Validar valores de entrada
  consumption <- check_numeric_value(consumption, "consumption", min_val = 0)
  temperature <- check_numeric_value(temperature, "temperature", min_val = -5, max_val = 50)
  p_value <- check_numeric_value(p_value, "p_value", min_val = 0, max_val = 5)
  
  if (consumption == 0) return(0)
  
  # Determinar ecuación a usar
  EGEQ <- egestion_params$EGEQ %||% 1
  FA <- egestion_params$FA
  
  if (is.na(FA)) return(0)
  
  # Calcular egestión según ecuación
  if (EGEQ == 1) {
    egestion <- egestion_model_1(consumption, FA)
    
  } else if (EGEQ == 2) {
    FB <- egestion_params$FB
    FG <- egestion_params$FG
    if (any(is.na(c(FB, FG)))) {
      warning("FB y FG requeridos para EGEQ=2, usando modelo 1")
      egestion <- egestion_model_1(consumption, FA)
    } else {
      egestion <- egestion_model_2(consumption, temperature, p_value, FA, FB, FG)
    }
    
  } else if (EGEQ == 3) {
    FB <- egestion_params$FB
    FG <- egestion_params$FG
    if (any(is.na(c(FB, FG)))) {
      warning("FB y FG requeridos para EGEQ=3, usando modelo 1")
      egestion <- egestion_model_1(consumption, FA)
    } else {
      egestion <- egestion_model_3(consumption, temperature, p_value, FA, FB, FG, 
                                   indigestible_fraction)
    }
    
  } else if (EGEQ == 4) {
    FB <- egestion_params$FB
    if (is.na(FB)) {
      warning("FB requerido para EGEQ=4, usando modelo 1")
      egestion <- egestion_model_1(consumption, FA)
    } else {
      egestion <- egestion_model_4(consumption, temperature, FA, FB)
    }
    
  } else {
    warning("Ecuación de egestión no válida: ", EGEQ, ". Usando modelo 1.")
    egestion <- egestion_model_1(consumption, FA)
  }
  
  return(egestion)
}

#' Calcular excreción diaria
#'
#' @param consumption Consumo (J/g)
#' @param egestion Egestión (J/g)
#' @param temperature Temperatura del agua (°C)
#' @param p_value Proporción del consumo máximo (p-value)
#' @param excretion_params Lista con parámetros de excreción
#' @return Excreción (J/g)
#' @export
calculate_excretion <- function(consumption, egestion, temperature, p_value, excretion_params) {
  
  # Validaciones básicas
  if (is.null(excretion_params)) {
    stop("excretion_params no puede ser NULL")
  }
  
  if (!"UA" %in% names(excretion_params)) {
    stop("Parámetro UA requerido para excreción")
  }
  
  # Validar valores de entrada
  consumption <- check_numeric_value(consumption, "consumption", min_val = 0)
  egestion <- check_numeric_value(egestion, "egestion", min_val = 0)
  temperature <- check_numeric_value(temperature, "temperature", min_val = -5, max_val = 50)
  p_value <- check_numeric_value(p_value, "p_value", min_val = 0, max_val = 5)
  
  if (consumption == 0) return(0)
  
  # Asegurar que egestión no exceda consumo
  egestion <- pmin(egestion, consumption)
  
  # Determinar ecuación a usar
  EXEQ <- excretion_params$EXEQ %||% 1
  UA <- excretion_params$UA
  
  if (is.na(UA)) return(0)
  
  # Calcular excreción según ecuación
  if (EXEQ == 1) {
    excretion <- excretion_model_1(consumption, egestion, UA)
    
  } else if (EXEQ == 2) {
    UB <- excretion_params$UB
    UG <- excretion_params$UG
    if (any(is.na(c(UB, UG)))) {
      warning("UB y UG requeridos para EXEQ=2, usando modelo 1")
      excretion <- excretion_model_1(consumption, egestion, UA)
    } else {
      excretion <- excretion_model_2(consumption, egestion, temperature, p_value, UA, UB, UG)
    }
    
  } else if (EXEQ == 3) {
    UB <- excretion_params$UB
    UG <- excretion_params$UG
    if (any(is.na(c(UB, UG)))) {
      warning("UB y UG requeridos para EXEQ=3, usando modelo 1")
      excretion <- excretion_model_1(consumption, egestion, UA)
    } else {
      excretion <- excretion_model_3(consumption, egestion, temperature, p_value, UA, UB, UG)
    }
    
  } else if (EXEQ == 4) {
    UB <- excretion_params$UB
    if (is.na(UB)) {
      warning("UB requerido para EXEQ=4, usando modelo 1")
      excretion <- excretion_model_1(consumption, egestion, UA)
    } else {
      excretion <- excretion_model_4(consumption, egestion, temperature, UA, UB)
    }
    
  } else {
    warning("Ecuación de excreción no válida: ", EXEQ, ". Usando modelo 1.")
    excretion <- excretion_model_1(consumption, egestion, UA)
  }
  
  return(excretion)
}


# ============================================================================
# FUNCIONES DE UTILIDAD
# ============================================================================

#' Calcular eficiencias de asimilación y retención
#'
#' @param consumption Consumo (J/g)
#' @param egestion Egestión (J/g)
#' @param excretion Excreción (J/g)
#' @return Lista con eficiencias calculadas
#' @export
calculate_efficiencies <- function(consumption, egestion, excretion) {
  
  if (consumption <= 0) {
    return(list(
      assimilation_efficiency = 0,
      net_retention_efficiency = 0,
      gross_efficiency = 0
    ))
  }
  
  assimilated <- consumption - egestion
  net_retained <- assimilated - excretion
  
  return(list(
    assimilation_efficiency = pmax(0, pmin(1, assimilated / consumption)),
    net_retention_efficiency = pmax(0, pmin(1, net_retained / consumption)),
    gross_efficiency = pmax(0, pmin(1, egestion / consumption))
  ))
}

