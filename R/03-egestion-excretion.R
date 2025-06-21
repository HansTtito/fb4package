#' Funciones de Egestion y Excreción para el Modelo FB4
#'
#' @name egestion-excretion
#' @aliases egestion-excretion
NULL

# ============================================================================
# FUNCIONES DE EGESTION
# ============================================================================

#' Modelo de egestion 1 - Básico
#'
#' Modelo simple de egestion con fracción constante del consumo
#'
#' @param C Consumo (J/g)
#' @param FA Fracción de egestion
#' @return Egestion (J/g)
#' @export
egestion1 <- function(C, FA) {
  Eg <- FA * C
  return(Eg)
}

#' Modelo de egestion 2 - Elliott (1976)
#'
#' Modelo de egestion dependiente de temperatura y nivel de alimentación
#'
#' @param C Consumo (J/g)
#' @param Temperature Temperatura del agua (°C)
#' @param p Proporción del consumo máximo (p-value)
#' @param FA Parámetro base de egestion
#' @param FB Coeficiente de dependencia de temperatura
#' @param FG Coeficiente de dependencia del nivel de alimentación
#' @return Egestion (J/g)
#' @export
egestion2 <- function(C, Temperature, p, FA, FB, FG) {
  Eg <- FA * (Temperature^FB) * exp(FG * p) * C
  return(Eg)
}

#' Modelo de egestion 3 - Stewart et al. (1983)
#'
#' Modelo que incluye presas indigeribles
#'
#' @param C Consumo (J/g)
#' @param Temperature Temperatura del agua (°C)
#' @param p Proporción del consumo máximo (p-value)
#' @param FA Parámetro base de egestion
#' @param FB Coeficiente de dependencia de temperatura
#' @param FG Coeficiente de dependencia del nivel de alimentación
#' @param indigestible_fraction Fracción indigerible de las presas
#' @return Egestion (J/g)
#' @export
egestion3 <- function(C, Temperature, p, FA, FB, FG, indigestible_fraction = 0) {
  # Calcular eficiencia de egestion base
  PE <- FA * (Temperature^FB) * exp(FG * p)

  # Ajustar por presas indigeribles
  PFF <- indigestible_fraction  # Fracción indigerible
  PF <- ((PE - 0.1) / 0.9) * (1 - PFF) + PFF

  # Calcular egestion final
  Eg <- PF * C

  return(Eg)
}

#' Modelo de egestion 4 - Elliott (1976) sin p-value
#'
#' Modelo simplificado sin dependencia del nivel de alimentación
#'
#' @param C Consumo (J/g)
#' @param Temperature Temperatura del agua (°C)
#' @param FA Parámetro base de egestion
#' @param FB Coeficiente de dependencia de temperatura
#' @return Egestion (J/g)
#' @export
egestion4 <- function(C, Temperature, FA, FB) {
  Eg <- FA * (Temperature^FB) * C
  return(Eg)
}

#' Función principal de egestion
#'
#' Calcula la egestion usando la ecuación especificada
#'
#' @param C Consumo (J/g)
#' @param Temperature Temperatura del agua (°C)
#' @param p Proporción del consumo máximo (p-value)
#' @param EGEQ Número de ecuación de egestion (1-4)
#' @param FA Parámetro base de egestion
#' @param FB Coeficiente de dependencia de temperatura (opcional)
#' @param FG Coeficiente de dependencia del nivel de alimentación (opcional)
#' @param indigestible_fraction Fracción indigerible de presas (para ecuación 3)
#' @return Egestion (J/g)
#' @export
#' @examples
#' # Usando ecuación 1 (básica)
#' egestion(C = 100, Temperature = 20, p = 0.5, EGEQ = 1, FA = 0.15)
#'
#' # Usando ecuación 2 (Elliott 1976)
#' egestion(C = 100, Temperature = 20, p = 0.5, EGEQ = 2,
#'          FA = 0.15, FB = 0.5, FG = 0.1)
egestion <- function(C, Temperature, p, EGEQ, FA, FB = NULL, FG = NULL,
                     indigestible_fraction = 0) {

  # Validar entrada
  if (!EGEQ %in% 1:4) {
    stop("EGEQ debe ser 1, 2, 3, o 4")
  }

  if (C < 0) {
    warning("Consumo negativo detectado")
    return(0)
  }

  # Seleccionar ecuación
  if (EGEQ == 1) {
    Eg <- egestion1(C = C, FA = FA)

  } else if (EGEQ == 2) {
    if (is.null(FB) || is.null(FG)) {
      stop("FB y FG son requeridos para EGEQ = 2")
    }
    Eg <- egestion2(C = C, Temperature = Temperature, p = p,
                    FA = FA, FB = FB, FG = FG)

  } else if (EGEQ == 3) {
    if (is.null(FB) || is.null(FG)) {
      stop("FB y FG son requeridos para EGEQ = 3")
    }
    Eg <- egestion3(C = C, Temperature = Temperature, p = p,
                    FA = FA, FB = FB, FG = FG,
                    indigestible_fraction = indigestible_fraction)

  } else if (EGEQ == 4) {
    if (is.null(FB)) {
      stop("FB es requerido para EGEQ = 4")
    }
    Eg <- egestion4(C = C, Temperature = Temperature, FA = FA, FB = FB)
  }

  # Validar salida
  if (Eg < 0) {
    warning("Egestion negativa calculada, estableciendo a 0")
    Eg <- 0
  }

  if (Eg > C) {
    warning("Egestion mayor que consumo, estableciendo a consumo")
    Eg <- C
  }

  return(Eg)
}

# ============================================================================
# FUNCIONES DE EXCRECIÓN
# ============================================================================

#' Modelo de excreción 1 - Básico
#'
#' Modelo simple de excreción proporcional a la materia asimilada
#'
#' @param C Consumo (J/g)
#' @param Eg Egestion (J/g)
#' @param UA Fracción de excreción
#' @return Excreción (J/g)
#' @export
excretion1 <- function(C, Eg, UA) {
  U <- UA * (C - Eg)
  return(U)
}

#' Modelo de excreción 2 - Con dependencia de temperatura y alimentación
#'
#' Modelo que incluye efectos de temperatura y nivel de alimentación
#'
#' @param C Consumo (J/g)
#' @param Temperature Temperatura del agua (°C)
#' @param p Proporción del consumo máximo (p-value)
#' @param Eg Egestion (J/g)
#' @param UA Parámetro base de excreción
#' @param UB Coeficiente de dependencia de temperatura
#' @param UG Coeficiente de dependencia del nivel de alimentación
#' @return Excreción (J/g)
#' @export
excretion2 <- function(C, Temperature, p, Eg, UA, UB, UG) {
  U <- UA * (Temperature^UB) * exp(UG * p) * (C - Eg)
  return(U)
}

#' Modelo de excreción 3 - Variante del modelo 2
#'
#' Similar al modelo 2 pero con formulación ligeramente diferente
#'
#' @param C Consumo (J/g)
#' @param Temperature Temperatura del agua (°C)
#' @param p Proporción del consumo máximo (p-value)
#' @param Eg Egestion (J/g)
#' @param UA Parámetro base de excreción
#' @param UB Coeficiente de dependencia de temperatura
#' @param UG Coeficiente de dependencia del nivel de alimentación
#' @return Excreción (J/g)
#' @export
excretion3 <- function(C, Temperature, p, Eg, UA, UB, UG) {
  # Idéntico al modelo 2 en la implementación actual
  U <- UA * (Temperature^UB) * exp(UG * p) * (C - Eg)
  return(U)
}

#' Modelo de excreción 4 - Sin dependencia de alimentación
#'
#' Modelo que solo depende de temperatura
#'
#' @param C Consumo (J/g)
#' @param Temperature Temperatura del agua (°C)
#' @param Eg Egestion (J/g)
#' @param UA Parámetro base de excreción
#' @param UB Coeficiente de dependencia de temperatura
#' @return Excreción (J/g)
#' @export
excretion4 <- function(C, Temperature, Eg, UA, UB) {
  U <- UA * (Temperature^UB) * (C - Eg)
  return(U)
}

#' Función principal de excreción
#'
#' Calcula la excreción usando la ecuación especificada
#'
#' @param C Consumo (J/g)
#' @param Eg Egestion (J/g)
#' @param Temperature Temperatura del agua (°C)
#' @param p Proporción del consumo máximo (p-value)
#' @param EXEQ Número de ecuación de excreción (1-4)
#' @param UA Parámetro base de excreción
#' @param UB Coeficiente de dependencia de temperatura (opcional)
#' @param UG Coeficiente de dependencia del nivel de alimentación (opcional)
#' @return Excreción (J/g)
#' @export
#' @examples
#' # Usando ecuación 1 (básica)
#' excretion(C = 100, Eg = 15, Temperature = 20, p = 0.5,
#'           EXEQ = 1, UA = 0.08)
#'
#' # Usando ecuación 2 (completa)
#' excretion(C = 100, Eg = 15, Temperature = 20, p = 0.5,
#'           EXEQ = 2, UA = 0.08, UB = 0.3, UG = 0.05)
excretion <- function(C, Eg, Temperature, p, EXEQ, UA, UB = NULL, UG = NULL) {

  # Validar entrada
  if (!EXEQ %in% 1:4) {
    stop("EXEQ debe ser 1, 2, 3, o 4")
  }

  if (C < 0 || Eg < 0) {
    warning("Consumo o egestion negativos detectados")
    return(0)
  }

  if (Eg > C) {
    warning("Egestion mayor que consumo")
    return(0)
  }

  # Calcular materia asimilada
  assimilated <- C - Eg

  if (assimilated <= 0) {
    return(0)
  }

  # Seleccionar ecuación
  if (EXEQ == 1) {
    U <- excretion1(C = C, Eg = Eg, UA = UA)

  } else if (EXEQ == 2) {
    if (is.null(UB) || is.null(UG)) {
      stop("UB y UG son requeridos para EXEQ = 2")
    }
    U <- excretion2(C = C, Temperature = Temperature, p = p, Eg = Eg,
                    UA = UA, UB = UB, UG = UG)

  } else if (EXEQ == 3) {
    if (is.null(UB) || is.null(UG)) {
      stop("UB y UG son requeridos para EXEQ = 3")
    }
    U <- excretion3(C = C, Temperature = Temperature, p = p, Eg = Eg,
                    UA = UA, UB = UB, UG = UG)

  } else if (EXEQ == 4) {
    if (is.null(UB)) {
      stop("UB es requerido para EXEQ = 4")
    }
    U <- excretion4(C = C, Temperature = Temperature, Eg = Eg,
                    UA = UA, UB = UB)
  }

  # Validar salida
  if (U < 0) {
    warning("Excreción negativa calculada, estableciendo a 0")
    U <- 0
  }

  if (U > assimilated) {
    warning("Excreción mayor que materia asimilada, estableciendo a materia asimilada")
    U <- assimilated
  }

  return(U)
}

# ============================================================================
# FUNCIONES AUXILIARES Y DE VALIDACIÓN
# ============================================================================

#' Calcular eficiencia de asimilación
#'
#' Calcula la eficiencia con la que el alimento es asimilado
#'
#' @param C Consumo (J/g)
#' @param Eg Egestion (J/g)
#' @return Eficiencia de asimilación (fracción)
#' @export
calculate_assimilation_efficiency <- function(C, Eg) {
  if (C <= 0) {
    return(0)
  }

  efficiency <- (C - Eg) / C

  # Asegurar que esté en rango válido
  efficiency <- pmax(0, pmin(1, efficiency))

  return(efficiency)
}

#' Calcular eficiencia de retención neta
#'
#' Calcula la eficiencia con la que la materia asimilada es retenida
#'
#' @param C Consumo (J/g)
#' @param Eg Egestion (J/g)
#' @param Ex Excreción (J/g)
#' @return Eficiencia de retención neta (fracción)
#' @export
calculate_net_retention_efficiency <- function(C, Eg, Ex) {
  if (C <= 0) {
    return(0)
  }

  net_retained <- C - Eg - Ex
  efficiency <- net_retained / C

  # Asegurar que esté en rango válido
  efficiency <- pmax(0, pmin(1, efficiency))

  return(efficiency)
}

#' Validar parámetros de egestion y excreción
#'
#' Verifica que los parámetros estén en rangos biológicamente realistas
#'
#' @param FA Parámetro de egestion
#' @param UA Parámetro de excreción
#' @param FB Coeficiente de temperatura para egestion (opcional)
#' @param UB Coeficiente de temperatura para excreción (opcional)
#' @param FG Coeficiente de alimentación para egestion (opcional)
#' @param UG Coeficiente de alimentación para excreción (opcional)
#' @return Lista con validación y advertencias
#' @export
validate_egestion_excretion_params <- function(FA, UA, FB = NULL, UB = NULL,
                                               FG = NULL, UG = NULL) {

  warnings <- character()
  valid <- TRUE

  # Validar FA (fracción de egestion)
  if (FA < 0 || FA > 1) {
    warnings <- c(warnings, paste("FA fuera de rango típico (0-1):", FA))
    if (FA < 0 || FA > 2) {
      valid <- FALSE
    }
  }

  # Validar UA (fracción de excreción)
  if (UA < 0 || UA > 1) {
    warnings <- c(warnings, paste("UA fuera de rango típico (0-1):", UA))
    if (UA < 0 || UA > 2) {
      valid <- FALSE
    }
  }

  # Validar coeficientes de temperatura
  if (!is.null(FB)) {
    if (FB < -2 || FB > 3) {
      warnings <- c(warnings, paste("FB fuera de rango típico (-2 a 3):", FB))
    }
  }

  if (!is.null(UB)) {
    if (UB < -2 || UB > 3) {
      warnings <- c(warnings, paste("UB fuera de rango típico (-2 a 3):", UB))
    }
  }

  # Validar coeficientes de alimentación
  if (!is.null(FG)) {
    if (abs(FG) > 2) {
      warnings <- c(warnings, paste("FG fuera de rango típico (-2 a 2):", FG))
    }
  }

  if (!is.null(UG)) {
    if (abs(UG) > 2) {
      warnings <- c(warnings, paste("UG fuera de rango típico (-2 a 2):", UG))
    }
  }

  return(list(
    valid = valid,
    warnings = warnings,
    n_warnings = length(warnings)
  ))
}

#' Calcular balance energético para egestion y excreción
#'
#' Verifica que las pérdidas no excedan el consumo
#'
#' @param C Consumo (J/g)
#' @param Eg Egestion (J/g)
#' @param Ex Excreción (J/g)
#' @param tolerance Tolerancia para el balance (fracción)
#' @return Lista con información del balance
#' @export
check_egestion_excretion_balance <- function(C, Eg, Ex, tolerance = 0.01) {

  total_losses <- Eg + Ex
  loss_fraction <- total_losses / C

  balanced <- loss_fraction <= (1 + tolerance)

  available_for_metabolism <- C - Eg - Ex

  return(list(
    balanced = balanced,
    consumption = C,
    egestion = Eg,
    excretion = Ex,
    total_losses = total_losses,
    loss_fraction = loss_fraction,
    available_for_metabolism = available_for_metabolism,
    assimilation_efficiency = calculate_assimilation_efficiency(C, Eg),
    net_retention_efficiency = calculate_net_retention_efficiency(C, Eg, Ex)
  ))
}

#' Ejemplo de uso combinado de egestion y excreción
#'
#' Función de demostración que muestra el uso típico
#'
#' @param consumption Consumo (J/g)
#' @param temperature Temperatura (°C)
#' @param p_value Proporción del consumo máximo
#' @param species_params Lista con parámetros de la especie
#' @return Lista con resultados de egestion y excreción
#' @export
#' @examples
#' \dontrun{
#' params <- list(
#'   EGEQ = 2, FA = 0.15, FB = 0.5, FG = 0.1,
#'   EXEQ = 2, UA = 0.08, UB = 0.3, UG = 0.05
#' )
#'
#' resultado <- calculate_egestion_excretion_example(
#'   consumption = 120,
#'   temperature = 18,
#'   p_value = 0.6,
#'   species_params = params
#' )
#' print(resultado)
#' }
calculate_egestion_excretion_example <- function(consumption, temperature, p_value,
                                                 species_params) {

  # Calcular egestion
  egestion_result <- egestion(
    C = consumption,
    Temperature = temperature,
    p = p_value,
    EGEQ = species_params$EGEQ,
    FA = species_params$FA,
    FB = species_params$FB,
    FG = species_params$FG
  )

  # Calcular excreción
  excretion_result <- excretion(
    C = consumption,
    Eg = egestion_result,
    Temperature = temperature,
    p = p_value,
    EXEQ = species_params$EXEQ,
    UA = species_params$UA,
    UB = species_params$UB,
    UG = species_params$UG
  )

  # Verificar balance
  balance <- check_egestion_excretion_balance(
    C = consumption,
    Eg = egestion_result,
    Ex = excretion_result
  )

  return(list(
    consumption = consumption,
    egestion = egestion_result,
    excretion = excretion_result,
    assimilated = consumption - egestion_result,
    net_energy = consumption - egestion_result - excretion_result,
    balance = balance
  ))
}
