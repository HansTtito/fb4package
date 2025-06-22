#' Funciones de Respiración para el Modelo FB4
#'
#' @name respiration-functions
#' @aliases respiration-functions
NULL

# ============================================================================
# FUNCIONES CORE DE TEMPERATURA PARA RESPIRACIÓN
# ============================================================================

#' Función de temperatura para respiración - Ecuación 1
#'
#' Implementa la ecuación de temperatura 1 (exponencial simple)
#'
#' @param temperature Temperatura del agua (°C)
#' @param RQ Tasa Q10 para temperaturas bajas
#' @return Factor de temperatura
#' @keywords internal
respiration_temp_eq1 <- function(temperature, RQ) {
  
  if (is.na(temperature) || is.na(RQ)) return(0.001)
  
  # Limitar temperatura para evitar overflow
  safe_temp <- clamp(temperature, -50, 50)
  safe_RQ <- clamp(RQ, -1, 1)
  
  ft <- safe_exp(safe_RQ * safe_temp)
  return(pmax(0.001, ft))  # Mínimo valor para evitar cero
}

#' Función de temperatura para respiración - Ecuación 2
#'
#' Implementa la ecuación de temperatura 2 (Kitchell et al. 1977)
#'
#' @param temperature Temperatura del agua (°C)
#' @param RTM Temperatura máxima (letal)
#' @param RTO Temperatura óptima para respiración
#' @param RX Parámetro calculado
#' @return Factor de temperatura
#' @keywords internal
respiration_temp_eq2 <- function(temperature, RTM, RTO, RX) {
  
  if (any(is.na(c(temperature, RTM, RTO, RX)))) return(0.001)
  
  if (temperature >= RTM) {
    return(0.001)  # Valor mínimo en lugar de cero
  }
  
  if (RTM <= RTO) {
    warning("RTM debe ser mayor que RTO")
    return(0.001)
  }
  
  V <- (RTM - temperature) / (RTM - RTO)
  if (V <= 0) return(0.001)
  
  # Cálculo seguro
  safe_RX <- clamp(RX, 0, 10)
  ft <- V^safe_RX * safe_exp(safe_RX * (1 - V))
  
  return(pmax(0.001, ft))
}

# ============================================================================
# FUNCIONES DE ACTIVIDAD
# ============================================================================

#' Función de actividad para respiración - Ecuación 1
#'
#' Implementa la función de actividad con componente de temperatura
#'
#' @param weight Peso del pez (g)
#' @param temperature Temperatura del agua (°C)
#' @param RTL Temperatura de corte para cambio en relación de actividad
#' @param ACT Multiplicador de actividad
#' @param RK4 Coeficiente de dependencia de masa para velocidad de nado
#' @param BACT Coeficiente de dependencia de temperatura para velocidad de nado
#' @param RK1 Intercepto para velocidad de nado sobre temperatura de corte
#' @param RK5 Coeficiente de temperatura para velocidad de nado
#' @param RTO Temperatura óptima para respiración
#' @return Factor de actividad
#' @keywords internal
calculate_activity_factor <- function(weight, temperature, RTL, ACT, RK4, BACT, RK1, RK5, RTO) {
  
  # Validar parámetros
  if (any(is.na(c(weight, temperature, RTL, ACT, RK4, BACT, RK1, RK5, RTO)))) {
    return(1.0)  # Valor por defecto
  }
  
  # Calcular velocidad según temperatura
  if (temperature <= RTL) {
    VEL <- ACT * weight^RK4 * safe_exp(BACT * temperature)
  } else {
    VEL <- RK1 * weight^RK4 * safe_exp(RK5 * temperature)
  }
  
  # Limitar velocidad para evitar valores extremos
  VEL <- clamp(VEL, 0, 100)
  
  # Calcular factor de actividad
  ACTIVITY <- safe_exp(RTO * VEL)
  
  return(pmax(1.0, ACTIVITY))  # Mínimo de 1.0
}

#' Función simplificada de actividad (constante)
#'
#' @param ACT Multiplicador de actividad constante
#' @return Factor de actividad
#' @keywords internal
calculate_simple_activity <- function(ACT) {
  if (is.na(ACT) || ACT <= 0) return(1.0)
  return(ACT)
}

# ============================================================================
# FUNCIÓN PRINCIPAL DE RESPIRACIÓN (ÚNICA VERSIÓN)
# ============================================================================

#' Calcular respiración diaria
#'
#' Función principal que calcula la respiración basada en temperatura y peso
#'
#' @param temperature Temperatura del agua (°C)
#' @param weight Peso del pez (g)
#' @param respiration_params Lista con parámetros de respiración
#' @param consumption Consumo para calcular SDA (opcional)
#' @return Respiración (g O2/g pez/día)
#' @export
calculate_respiration <- function(temperature, weight, respiration_params, consumption = 0) {
  
  # Validaciones básicas
  if (is.null(respiration_params)) {
    stop("respiration_params no puede ser NULL")
  }
  
  required_basic <- c("RA", "RB", "REQ")
  missing_basic <- setdiff(required_basic, names(respiration_params))
  if (length(missing_basic) > 0) {
    stop("Parámetros básicos faltantes: ", paste(missing_basic, collapse = ", "))
  }
  
  # Validar valores de entrada (usando función interna)
  weight <- check_numeric_value(weight, "weight", min_val = 0.001)
  temperature <- check_numeric_value(temperature, "temperature", min_val = -5, max_val = 50)
  
  # Extraer parámetros básicos
  RA <- respiration_params$RA
  RB <- respiration_params$RB
  REQ <- respiration_params$REQ %||% 1
  
  if (is.na(RA) || is.na(RB)) {
    return(0.001)  # Valor mínimo
  }
  
  # Calcular respiración máxima
  Rmax <- RA * weight^RB
  
  # Calcular factor de temperatura
  ft <- calculate_temperature_factor_respiration(temperature, respiration_params)
  
  # Calcular factor de actividad
  activity_factor <- calculate_activity_factor_respiration(weight, temperature, respiration_params)
  
  # Calcular respiración básica
  basic_respiration <- Rmax * ft * activity_factor
  
  # Agregar SDA si hay consumo
  if (consumption > 0 && !is.null(respiration_params$SDA)) {
    sda_factor <- 1 + respiration_params$SDA * consumption
    total_respiration <- basic_respiration * sda_factor
  } else {
    total_respiration <- basic_respiration
  }
  
  # Asegurar que el resultado sea válido
  if (is.na(total_respiration) || !is.finite(total_respiration)) {
    return(0.001)
  }
  
  return(pmax(0.001, total_respiration))
}

#' Calcular factor de temperatura para respiración
#'
#' @param temperature Temperatura del agua (°C)
#' @param respiration_params Lista con parámetros de respiración
#' @return Factor de temperatura
#' @keywords internal
calculate_temperature_factor_respiration <- function(temperature, respiration_params) {
  
  REQ <- respiration_params$REQ %||% 1
  
  if (REQ == 1) {
    RQ <- respiration_params$RQ
    if (is.null(RQ) || is.na(RQ)) return(1.0)
    return(respiration_temp_eq1(temperature, RQ))
    
  } else if (REQ == 2) {
    # Parámetros necesarios para ecuación 2
    RTM <- respiration_params$RTM
    RTO <- respiration_params$RTO
    RX <- respiration_params$RX
    
    # Si RX no está disponible, calcularlo
    if (is.null(RX) || is.na(RX)) {
      RQ <- respiration_params$RQ
      if (any(is.na(c(RQ, RTM, RTO)))) return(1.0)
      
      extra_params <- calculate_respiration_params_eq2(RQ, RTM, RTO)
      RX <- extra_params$RX
    }
    
    if (any(is.na(c(RTM, RTO, RX)))) return(1.0)
    return(respiration_temp_eq2(temperature, RTM, RTO, RX))
    
  } else {
    warning("Ecuación de respiración no válida: ", REQ, ". Usando ecuación 1.")
    RQ <- respiration_params$RQ %||% 0.1
    return(respiration_temp_eq1(temperature, RQ))
  }
}

#' Calcular factor de actividad para respiración
#'
#' @param weight Peso del pez (g)
#' @param temperature Temperatura del agua (°C)
#' @param respiration_params Lista con parámetros de respiración
#' @return Factor de actividad
#' @keywords internal
calculate_activity_factor_respiration <- function(weight, temperature, respiration_params) {
  
  REQ <- respiration_params$REQ %||% 1
  
  if (REQ == 1) {
    # Actividad compleja con dependencia de temperatura
    required_activity_params <- c("RTL", "ACT", "RK4", "BACT", "RK1", "RK5", "RTO")
    
    # Verificar si tenemos todos los parámetros necesarios
    has_all_params <- all(required_activity_params %in% names(respiration_params))
    
    if (has_all_params) {
      return(calculate_activity_factor(
        weight = weight,
        temperature = temperature,
        RTL = respiration_params$RTL,
        ACT = respiration_params$ACT,
        RK4 = respiration_params$RK4,
        BACT = respiration_params$BACT,
        RK1 = respiration_params$RK1,
        RK5 = respiration_params$RK5,
        RTO = respiration_params$RTO
      ))
    } else {
      # Usar actividad simple si no tenemos todos los parámetros
      ACT <- respiration_params$ACT %||% 1.0
      return(calculate_simple_activity(ACT))
    }
    
  } else if (REQ == 2) {
    # Actividad simple para ecuación 2
    ACT <- respiration_params$ACT %||% 1.0
    return(calculate_simple_activity(ACT))
    
  } else {
    return(1.0)
  }
}

# ============================================================================
# FUNCIONES AUXILIARES
# ============================================================================

#' Calcular parámetros adicionales para ecuación de respiración 2
#'
#' @param RQ Tasa Q10
#' @param RTM Temperatura máxima
#' @param RTO Temperatura óptima
#' @return Lista con RY, RZ, RX
#' @export
calculate_respiration_params_eq2 <- function(RQ, RTM, RTO) {
  
  if (any(is.na(c(RQ, RTM, RTO)))) {
    stop("Todos los parámetros deben ser válidos (no NA)")
  }
  
  if (RTM <= RTO) {
    stop("RTM debe ser mayor que RTO")
  }
  
  if (RQ <= 0) {
    stop("RQ debe ser positivo")
  }
  
  RY <- log(RQ) * (RTM - RTO + 2)
  RZ <- log(RQ) * (RTM - RTO)
  
  # Cálculo seguro de RX
  if (RY == 0) {
    RX <- 1
  } else {
    discriminant <- 1 + 40/RY
    if (discriminant < 0) {
      RX <- 1
    } else {
      RX <- (RZ^2 * (1 + sqrt(discriminant))^2) / 400
    }
  }
  
  return(list(RY = RY, RZ = RZ, RX = RX))
}

#' Calcular Acción Dinámica Específica (SDA)
#'
#' @param consumption_energy Consumo en energía (J/g)
#' @param egestion_energy Egestión en energía (J/g)
#' @param SDA_coeff Coeficiente de acción dinámica específica
#' @return SDA en energía (J/g)
#' @export
calculate_sda <- function(consumption_energy, egestion_energy, SDA_coeff) {
  
  # Validaciones
  consumption_energy <- check_numeric_value(consumption_energy, "consumption_energy", min_val = 0)
  egestion_energy <- check_numeric_value(egestion_energy, "egestion_energy", min_val = 0)
  SDA_coeff <- check_numeric_value(SDA_coeff, "SDA_coeff", min_val = 0, max_val = 1)
  
  # Asegurar que egestión no sea mayor que consumo
  egestion_energy <- pmin(egestion_energy, consumption_energy)
  
  sda <- SDA_coeff * (consumption_energy - egestion_energy)
  
  return(pmax(0, sda))
}

# ============================================================================
# FUNCIÓN PARA CONVERTIR RESPIRACIÓN A UNIDADES DE ENERGÍA
# ============================================================================

#' Convertir respiración de O2 a unidades de energía
#'
#' @param respiration_o2 Respiración en g O2/g pez/día
#' @param oxycal Factor de conversión oxycal (J/g O2)
#' @return Respiración en J/g pez/día
#' @export
convert_respiration_to_energy <- function(respiration_o2, oxycal = 13560) {
  
  respiration_o2 <- check_numeric_value(respiration_o2, "respiration_o2", min_val = 0)
  oxycal <- check_numeric_value(oxycal, "oxycal", min_val = 1000, max_val = 20000)
  
  respiration_energy <- respiration_o2 * oxycal
  
  return(respiration_energy)
}