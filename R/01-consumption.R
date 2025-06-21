#' Funciones de Consumo para el Modelo FB4
#'
#' @name consumption-functions
#' @aliases consumption-functions
NULL

# ============================================================================
# FUNCIONES CORE DE TEMPERATURA PARA CONSUMO
# ============================================================================

#' Función de temperatura para consumo - Ecuación 1
#'
#' Implementa la ecuación de temperatura 1 (exponencial simple)
#'
#' @param temperature Temperatura del agua (°C)
#' @param CQ Coeficiente de temperatura para consumo
#' @return Factor de temperatura
#' @keywords internal
consumption_temp_eq1 <- function(temperature, CQ) {
  if (is.na(temperature) || is.na(CQ)) return(0)
  
  # Limitar temperatura para evitar overflow
  safe_temp <- clamp(temperature, -50, 50)
  safe_CQ <- clamp(CQ, -1, 1)
  
  ft <- safe_exp(safe_CQ * safe_temp)
  return(ft)
}

#' Función de temperatura para consumo - Ecuación 2
#'
#' Implementa la ecuación de temperatura 2 (Kitchell et al. 1977)
#'
#' @param temperature Temperatura del agua (°C)
#' @param CTM Temperatura máxima donde cesa el consumo
#' @param CTO Temperatura óptima de laboratorio
#' @param CX Parámetro calculado
#' @return Factor de temperatura
#' @keywords internal
consumption_temp_eq2 <- function(temperature, CTM, CTO, CX) {
  if (any(is.na(c(temperature, CTM, CTO, CX)))) return(0)
  
  if (temperature >= CTM) {
    return(0)
  }
  
  if (CTM <= CTO) {
    warning("CTM debe ser mayor que CTO")
    return(0)
  }
  
  V <- (CTM - temperature) / (CTM - CTO)
  if (V <= 0) return(0)
  
  # Cálculo seguro
  safe_CX <- clamp(CX, 0, 10)
  ft <- V^safe_CX * safe_exp(safe_CX * (1 - V))
  
  return(pmax(0, ft))
}

#' Función de temperatura para consumo - Ecuación 3
#'
#' Implementa la ecuación de temperatura 3 (Thornton y Lessem 1978)
#'
#' @param temperature Temperatura del agua (°C)
#' @param CQ Coeficiente de temperatura
#' @param CG1 Parámetro calculado 1
#' @param CK1 Fracción pequeña de la tasa máxima
#' @param CG2 Parámetro calculado 2
#' @param CTL Temperatura con tasa reducida
#' @param CK4 Fracción reducida de la tasa máxima
#' @return Factor de temperatura
#' @keywords internal
consumption_temp_eq3 <- function(temperature, CQ, CG1, CK1, CG2, CTL, CK4) {
  if (any(is.na(c(temperature, CQ, CG1, CK1, CG2, CTL, CK4)))) return(0)
  
  # Cálculo del primer componente
  L1 <- safe_exp(CG1 * (temperature - CQ))
  safe_CK1 <- clamp(CK1, 0.001, 0.999)
  KA <- (safe_CK1 * L1) / (1 + safe_CK1 * (L1 - 1))
  
  # Cálculo del segundo componente
  L2 <- safe_exp(CG2 * (CTL - temperature))
  safe_CK4 <- clamp(CK4, 0.001, 0.999)
  KB <- (safe_CK4 * L2) / (1 + safe_CK4 * (L2 - 1))
  
  ft <- KA * KB
  return(pmax(0, ft))
}

#' Función de temperatura para consumo - Ecuación 4
#'
#' Implementa la ecuación de temperatura 4 (polinomial)
#'
#' @param temperature Temperatura del agua (°C)
#' @param CQ Coeficiente lineal
#' @param CK1 Coeficiente cuadrático
#' @param CK4 Coeficiente cúbico
#' @return Factor de temperatura
#' @keywords internal
consumption_temp_eq4 <- function(temperature, CQ, CK1, CK4) {
  if (any(is.na(c(temperature, CQ, CK1, CK4)))) return(0)
  
  # Limitar temperatura para evitar valores extremos
  safe_temp <- clamp(temperature, -50, 50)
  
  exponent <- CQ * safe_temp + CK1 * safe_temp^2 + CK4 * safe_temp^3
  ft <- safe_exp(exponent)
  
  return(pmax(0, ft))
}

# ============================================================================
# FUNCIÓN PRINCIPAL DE CONSUMO
# ============================================================================

#' Calcular consumo diario
#'
#' Función principal que calcula el consumo basado en temperatura, peso y p-value
#'
#' @param temperature Temperatura del agua (°C)
#' @param weight Peso del pez (g)
#' @param p_value Proporción del consumo máximo (0-5)
#' @param consumption_params Lista con parámetros de consumo
#' @param method Método de cálculo ("maximum", "rate", "specific")
#' @return Consumo específico (g presa/g pez/día)
#' @export
calculate_consumption <- function(temperature, weight, p_value, consumption_params, method = "rate") {
  
  # Validaciones básicas (mejoradas)
  if (is.null(consumption_params)) {
    stop("consumption_params no puede ser NULL")
  }
  
  required_basic <- c("CA", "CB", "CEQ")
  missing_basic <- setdiff(required_basic, names(consumption_params))
  if (length(missing_basic) > 0) {
    stop("Parámetros básicos faltantes: ", paste(missing_basic, collapse = ", "))
  }
  
  # Validar valores con función interna mejorada
  weight <- check_numeric_value(weight, "weight", min_val = 0.001)
  p_value <- check_numeric_value(p_value, "p_value", min_val = 0, max_val = 5)
  temperature <- check_numeric_value(temperature, "temperature", min_val = -5, max_val = 50)
  
  # Calcular consumo máximo
  CA <- consumption_params$CA
  CB <- consumption_params$CB
  
  if (is.na(CA) || is.na(CB)) {
    return(0)
  }
  
  Cmax <- CA * weight^CB
  
  # Calcular factor de temperatura según ecuación
  ft <- calculate_temperature_factor_consumption(temperature, consumption_params)
  
  # Retornar según método solicitado
  if (method == "maximum") {
    return(Cmax * ft)
  } else if (method == "rate") {
    return(Cmax * p_value * ft)
  } else if (method == "specific") {
    return(Cmax * ft)  # Para usar con p-value externamente
  }
  
  # Por defecto, calcular consumo total
  consumption_rate <- Cmax * p_value * ft
  
  # Asegurar que el resultado sea válido
  if (is.na(consumption_rate) || !is.finite(consumption_rate)) {
    return(0)
  }
  
  return(pmax(0, consumption_rate))
}

#' Calcular factor de temperatura para consumo
#'
#' @param temperature Temperatura del agua (°C)
#' @param consumption_params Lista con parámetros de consumo
#' @return Factor de temperatura
#' @keywords internal
calculate_temperature_factor_consumption <- function(temperature, consumption_params) {
  
  CEQ <- consumption_params$CEQ %||% 1
  
  if (CEQ == 1) {
    CQ <- consumption_params$CQ
    if (is.null(CQ) || is.na(CQ)) return(0)
    return(consumption_temp_eq1(temperature, CQ))
    
  } else if (CEQ == 2) {
    # Parámetros necesarios para ecuación 2
    CTM <- consumption_params$CTM
    CTO <- consumption_params$CTO
    CX <- consumption_params$CX
    
    # Si CX no está disponible, calcularlo
    if (is.null(CX) || is.na(CX)) {
      CQ <- consumption_params$CQ
      if (any(is.na(c(CQ, CTM, CTO)))) return(0)
      
      extra_params <- calculate_consumption_params_eq2(CQ, CTM, CTO)
      CX <- extra_params$CX
    }
    
    if (any(is.na(c(CTM, CTO, CX)))) return(0)
    return(consumption_temp_eq2(temperature, CTM, CTO, CX))
    
  } else if (CEQ == 3) {
    # Parámetros necesarios para ecuación 3
    CQ <- consumption_params$CQ
    CG1 <- consumption_params$CG1
    CK1 <- consumption_params$CK1
    CG2 <- consumption_params$CG2
    CTL <- consumption_params$CTL
    CK4 <- consumption_params$CK4
    
    # Si CG1 y CG2 no están disponibles, calcularlos
    if (is.null(CG1) || is.null(CG2) || is.na(CG1) || is.na(CG2)) {
      CTO <- consumption_params$CTO
      CTM <- consumption_params$CTM
      if (any(is.na(c(CTO, CQ, CTL, CTM, CK1, CK4)))) return(0)
      
      extra_params <- calculate_consumption_params_eq3(CTO, CQ, CTL, CTM, CK1, CK4)
      CG1 <- extra_params$CG1
      CG2 <- extra_params$CG2
    }
    
    if (any(is.na(c(CQ, CG1, CK1, CG2, CTL, CK4)))) return(0)
    return(consumption_temp_eq3(temperature, CQ, CG1, CK1, CG2, CTL, CK4))
    
  } else if (CEQ == 4) {
    CQ <- consumption_params$CQ
    CK1 <- consumption_params$CK1
    CK4 <- consumption_params$CK4
    
    if (any(is.na(c(CQ, CK1, CK4)))) return(0)
    return(consumption_temp_eq4(temperature, CQ, CK1, CK4))
    
  } else {
    warning("Ecuación de consumo no válida: ", CEQ, ". Usando ecuación 1.")
    CQ <- consumption_params$CQ %||% 0.1
    return(consumption_temp_eq1(temperature, CQ))
  }
}

# ============================================================================
# FUNCIONES AUXILIARES PARA CÁLCULO DE PARÁMETROS
# ============================================================================

#' Calcular parámetros adicionales para ecuación de consumo 2
#'
#' @param CQ Coeficiente de temperatura
#' @param CTM Temperatura máxima
#' @param CTO Temperatura óptima
#' @return Lista con CY, CZ, CX
#' @export
calculate_consumption_params_eq2 <- function(CQ, CTM, CTO) {
  
  if (any(is.na(c(CQ, CTM, CTO)))) {
    stop("Todos los parámetros deben ser válidos (no NA)")
  }
  
  if (CTM <= CTO) {
    stop("CTM debe ser mayor que CTO")
  }
  
  if (CQ <= 0) {
    stop("CQ debe ser positivo")
  }
  
  CY <- log(CQ) * (CTM - CTO + 2)
  CZ <- log(CQ) * (CTM - CTO)
  
  # Cálculo seguro de CX
  if (CY == 0) {
    CX <- 1
  } else {
    discriminant <- 1 + 40/CY
    if (discriminant < 0) {
      CX <- 1
    } else {
      CX <- (CZ^2 * (1 + sqrt(discriminant))^2) / 400
    }
  }
  
  return(list(CY = CY, CZ = CZ, CX = CX))
}

#' Calcular parámetros adicionales para ecuación de consumo 3
#'
#' @param CTO Temperatura óptima
#' @param CQ Coeficiente de temperatura
#' @param CTL Temperatura con tasa reducida
#' @param CTM Temperatura máxima
#' @param CK1 Fracción pequeña de tasa máxima
#' @param CK4 Fracción reducida de tasa máxima
#' @return Lista con CG1, CG2
#' @export
calculate_consumption_params_eq3 <- function(CTO, CQ, CTL, CTM, CK1, CK4) {
  
  if (any(is.na(c(CTO, CQ, CTL, CTM, CK1, CK4)))) {
    stop("Todos los parámetros deben ser válidos (no NA)")
  }
  
  if (CTO <= CQ) {
    stop("CTO debe ser mayor que CQ")
  }
  
  if (CTL <= CTM) {
    stop("CTL debe ser mayor que CTM")
  }
  
  # Limitar CK1 y CK4 para evitar problemas numéricos
  safe_CK1 <- clamp(CK1, 0.001, 0.999)
  safe_CK4 <- clamp(CK4, 0.001, 0.999)
  
  # Cálculos seguros
  numerator1 <- 0.98 * (1 - safe_CK1)
  denominator1 <- safe_CK1 * 0.02
  
  numerator2 <- 0.98 * (1 - safe_CK4)
  denominator2 <- safe_CK4 * 0.02
  
  if (denominator1 <= 0 || denominator2 <= 0) {
    stop("Parámetros CK1 o CK4 causan división por cero")
  }
  
  CG1 <- (1/(CTO - CQ)) * log(numerator1 / denominator1)
  CG2 <- (1/(CTL - CTM)) * log(numerator2 / denominator2)
  
  return(list(CG1 = CG1, CG2 = CG2))
}

