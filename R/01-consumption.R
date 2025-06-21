#' Funciones de Consumo para el Modelo FB4
#'
#' @name consumption-functions
#' @aliases consumption-functions
NULL

#' Función de temperatura para consumo - Ecuación 1
#'
#' Implementa la ecuación de temperatura 1 (Hanson et al. 1997; Stewart et al. 1983)
#'
#' @param Temperature Temperatura del agua (°C)
#' @param CQ Coeficiente de temperatura para consumo
#' @return Factor de temperatura
#' @export
Cf1T <- function(Temperature, CQ) {
  ft <- exp(CQ * Temperature)
  return(ft)
}

#' Función de temperatura para consumo - Ecuación 2
#'
#' Implementa la ecuación de temperatura 2 (Hanson et al. 1997; Kitchell et al. 1977)
#'
#' @param Temperature Temperatura del agua (°C)
#' @param CTM Temperatura máxima donde cesa el consumo
#' @param CTO Temperatura óptima de laboratorio
#' @param CX Parámetro calculado
#' @return Factor de temperatura
#' @export
Cf2T <- function(Temperature, CTM, CTO, CX) {
  if (Temperature < CTM) {
    V <- (CTM - Temperature) / (CTM - CTO)
    ft <- V^CX * exp(CX * (1 - V))
  } else if (Temperature >= CTM) {
    ft <- 0
  }

  if(ft < 0) ft <- 0  # prevenir valores negativos
  return(ft)
}

#' Función de temperatura para consumo - Ecuación 3
#'
#' Implementa la ecuación de temperatura 3 (Hanson et al. 1997; Thornton y Lessem 1978)
#'
#' @param Temperature Temperatura del agua (°C)
#' @param CQ Coeficiente de temperatura
#' @param CG1 Parámetro calculado 1
#' @param CK1 Fracción pequeña de la tasa máxima
#' @param CG2 Parámetro calculado 2
#' @param CTL Temperatura con tasa reducida
#' @param CK4 Fracción reducida de la tasa máxima
#' @return Factor de temperatura
#' @export
Cf3T <- function(Temperature, CQ, CG1, CK1, CG2, CTL, CK4) {
  L1 <- exp(CG1 * (Temperature - CQ))
  KA <- (CK1 * L1) / (1 + CK1 * (L1 - 1))
  L2 <- exp(CG2 * (CTL - Temperature))
  KB <- (CK4 * L2) / (1 + CK4 * (L2 - 1))
  ft <- KA * KB
  return(ft)
}

#' Función de temperatura para consumo - Ecuación 4
#'
#' Implementa la ecuación de temperatura 4 (Bevelhimer et al. 1985)
#'
#' @param Temperature Temperatura del agua (°C)
#' @param CQ Coeficiente de temperatura
#' @param CK1 Coeficiente de segundo orden
#' @param CK4 Coeficiente de tercer orden
#' @return Factor de temperatura
#' @export
Cf4T <- function(Temperature, CQ, CK1, CK4) {
  ft <- exp(CQ * Temperature + CK1 * Temperature^2 + CK4 * Temperature^3)
  if(ft < 0) ft <- 0  # prevenir valores negativos
  return(ft)
}

#' Función principal de consumo
#'
#' Calcula el consumo basado en temperatura, peso, p-value y ecuación seleccionada
#'
#' @param Temperature Temperatura del agua (°C)
#' @param W Peso del pez (g)
#' @param p Proporción del consumo máximo (0-5)
#' @param CEQ Número de ecuación de consumo (1-4)
#' @param CA Intercepto para función masa alométrica
#' @param CB Pendiente para función masa alométrica
#' @param ... Parámetros adicionales según la ecuación seleccionada
#' @return Consumo específico (g presa/g pez/día)
#' @export
#' @examples
#' # Usando ecuación 1
#' consumption(Temperature = 20, W = 100, p = 0.5, CEQ = 1,
#'             CA = 0.303, CB = -0.275, CQ = 0.08)
consumption <- function(Temperature, W, p, CEQ, CA, CB, ...) {

  # Validación de entrada
  if (!CEQ %in% 1:4) stop("CEQ debe ser 1, 2, 3, o 4")
  if (p < 0 || p > 5) warning("p-value fuera del rango típico (0-5)")

  params <- list(...)

  Cmax <- CA * W^CB

  # Seleccionar función de temperatura según CEQ
  if (CEQ == 1) {
    if (!"CQ" %in% names(params)) stop("CQ requerido para CEQ = 1")
    ft <- Cf1T(Temperature, params$CQ)

  } else if (CEQ == 2) {
    required_params <- c("CTM", "CTO", "CX")
    missing <- setdiff(required_params, names(params))
    if (length(missing) > 0) stop(paste("Parámetros faltantes:", paste(missing, collapse = ", ")))
    ft <- Cf2T(Temperature, params$CTM, params$CTO, params$CX)

  } else if (CEQ == 3) {
    required_params <- c("CQ", "CG1", "CK1", "CG2", "CTL", "CK4")
    missing <- setdiff(required_params, names(params))
    if (length(missing) > 0) stop(paste("Parámetros faltantes:", paste(missing, collapse = ", ")))
    ft <- Cf3T(Temperature, params$CQ, params$CG1, params$CK1,
               params$CG2, params$CTL, params$CK4)

  } else if (CEQ == 4) {
    required_params <- c("CQ", "CK1", "CK4")
    missing <- setdiff(required_params, names(params))
    if (length(missing) > 0) stop(paste("Parámetros faltantes:", paste(missing, collapse = ", ")))
    ft <- Cf4T(Temperature, params$CQ, params$CK1, params$CK4)
  }

  consumption_rate <- Cmax * p * ft
  return(consumption_rate)
}

#' Calcular parámetros adicionales para ecuación de consumo 2
#'
#' @param CQ Coeficiente de temperatura
#' @param CTM Temperatura máxima
#' @param CTO Temperatura óptima
#' @return Lista con CY, CZ, CX
#' @export
calculate_consumption_params_eq2 <- function(CQ, CTM, CTO) {
  CY <- log(CQ) * (CTM - CTO + 2)
  CZ <- log(CQ) * (CTM - CTO)
  CX <- (CZ^2 * (1 + (1 + 40/CY)^0.5)^2) / 400

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
  CG1 <- (1/(CTO - CQ)) * log((0.98 * (1 - CK1)) / (CK1 * 0.02))
  CG2 <- (1/(CTL - CTM)) * log((0.98 * (1 - CK4)) / (CK4 * 0.02))

  return(list(CG1 = CG1, CG2 = CG2))
}
