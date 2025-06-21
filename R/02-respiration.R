#' Funciones de Respiración para el Modelo FB4
#'
#' @name respiration-functions
#' @aliases respiration-functions
NULL

#' Función de temperatura para respiración - Ecuación 1
#'
#' Implementa la ecuación de temperatura 1 para respiración (Hanson et al. 1997; Stewart et al. 1983)
#'
#' @param Temperature Temperatura del agua (°C)
#' @param RQ Tasa Q10 para temperaturas bajas
#' @return Factor de temperatura
#' @export
Rf1T <- function(Temperature, RQ) {
  ft <- exp(RQ * Temperature)
  return(ft)
}

#' Función de actividad para respiración - Ecuación 1
#'
#' Implementa la función de actividad con componente de temperatura
#'
#' @param W Peso del pez (g)
#' @param Temperature Temperatura del agua (°C)
#' @param RTL Temperatura de corte para cambio en relación de actividad
#' @param ACT Multiplicador de actividad
#' @param RK4 Coeficiente de dependencia de masa para velocidad de nado
#' @param BACT Coeficiente de dependencia de temperatura para velocidad de nado
#' @param RK1 Intercepto para velocidad de nado sobre temperatura de corte
#' @param RK5 Coeficiente de temperatura para velocidad de nado
#' @param RTO Temperatura óptima para respiración
#' @return Factor de actividad
#' @export
RACTf1T <- function(W, Temperature, RTL, ACT, RK4, BACT, RK1, RK5, RTO) {
  if (Temperature <= RTL) {
    VEL <- ACT * W^RK4 * exp(BACT * Temperature)
  } else if (Temperature > RTL) {
    VEL <- RK1 * W^RK4 * exp(RK5 * Temperature)
  }

  ACTIVITY <- exp(RTO * VEL)
  return(ACTIVITY)
}

#' Función de temperatura para respiración - Ecuación 2
#'
#' Implementa la ecuación de temperatura 2 para respiración (Hanson et al. 1997; Kitchell et al. 1977)
#'
#' @param Temperature Temperatura del agua (°C)
#' @param RTM Temperatura máxima (letal)
#' @param RTO Temperatura óptima para respiración
#' @param RX Parámetro calculado
#' @return Factor de temperatura
#' @export
Rf2T <- function(Temperature, RTM, RTO, RX) {
  if (Temperature < RTM) {
    V <- (RTM - Temperature) / (RTM - RTO)
    ft <- V^RX * exp(RX * (1 - V))
  } else if (Temperature >= RTM) {
    ft <- 0.000001
  }

  if (ft < 0) ft <- 0.000001
  return(ft)
}

#' Función principal de respiración
#'
#' Calcula la respiración basada en temperatura, peso y ecuación seleccionada
#'
#' @param Temperature Temperatura del agua (°C)
#' @param W Peso del pez (g)
#' @param REQ Número de ecuación de respiración (1-2)
#' @param RA Peso específico de oxígeno consumido por pez de 1g a 0°C
#' @param RB Pendiente de función masa alométrica para metabolismo estándar
#' @param ... Parámetros adicionales según la ecuación seleccionada
#' @return Respiración (g O2/g/día)
#' @export
#' @examples
#' # Usando ecuación 1
#' respiration(Temperature = 20, W = 100, REQ = 1,
#'             RA = 0.0038, RB = -0.355, RQ = 0.08,
#'             RTL = 24, ACT = 1, RK4 = 0.13, BACT = 0.08,
#'             RK1 = 1, RK5 = 0.13, RTO = 0.030)
respiration <- function(Temperature, W, REQ, RA, RB, ...) {

  # Validación de entrada
  if (!REQ %in% 1:2) stop("REQ debe ser 1 o 2")

  params <- list(...)

  Rmax <- RA * W^RB

  if (REQ == 1) {
    required_params <- c("RQ", "RTL", "ACT", "RK4", "BACT", "RK1", "RK5", "RTO")
    missing <- setdiff(required_params, names(params))
    if (length(missing) > 0) stop(paste("Parámetros faltantes:", paste(missing, collapse = ", ")))

    ft <- Rf1T(Temperature, params$RQ)
    ACTIVITY <- RACTf1T(W, Temperature, params$RTL, params$ACT, params$RK4,
                        params$BACT, params$RK1, params$RK5, params$RTO)

  } else if (REQ == 2) {
    required_params <- c("RTM", "RTO", "RX", "ACT")
    missing <- setdiff(required_params, names(params))
    if (length(missing) > 0) stop(paste("Parámetros faltantes:", paste(missing, collapse = ", ")))

    ft <- Rf2T(Temperature, params$RTM, params$RTO, params$RX)
    ACTIVITY <- params$ACT
  }

  R <- Rmax * ft * ACTIVITY
  return(R)
}

#' Calcular parámetros adicionales para ecuación de respiración 2
#'
#' @param RQ Tasa Q10
#' @param RTM Temperatura máxima
#' @param RTO Temperatura óptima
#' @return Lista con RY, RZ, RX
#' @export
calculate_respiration_params_eq2 <- function(RQ, RTM, RTO) {
  RY <- log(RQ) * (RTM - RTO + 2)
  RZ <- log(RQ) * (RTM - RTO)
  RX <- (RZ^2 * (1 + (1 + 40/RY)^0.5)^2) / 400

  return(list(RY = RY, RZ = RZ, RX = RX))
}

#' Función de acción dinámica específica
#'
#' Calcula la acción dinámica específica (SDA)
#'
#' @param C Consumo (J/g)
#' @param Eg Egestion (J/g)
#' @param SDA Coeficiente de acción dinámica específica
#' @return SDA (J/g)
#' @export
SpDynAct <- function(C, Eg, SDA) {
  S <- SDA * (C - Eg)
  return(S)
}
