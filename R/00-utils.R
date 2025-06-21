#' Funciones Utilitarias para el Modelo FB4
#'
#' @name utils
#' @aliases utils
NULL

#' Validar entradas del modelo FB4
#'
#' @param species Número o nombre de especie
#' @param initial_weight Peso inicial
#' @param fit_to Tipo de ajuste
#' @param fit_value Valor de ajuste
#' @param first_day Primer día
#' @param last_day Último día
#' @param temperature_data Datos de temperatura
#' @param diet_data Datos de dieta
#' @param prey_energy_data Datos de energía de presas
#' @return NULL (solo valida, detiene si hay errores)
#' @keywords internal
validate_fb4_inputs <- function(species, initial_weight, fit_to, fit_value,
                                first_day, last_day, temperature_data,
                                diet_data, prey_energy_data) {

  # Validar especie
  if (is.character(species)) {
    if (nchar(species) == 0) stop("Nombre de especie no puede estar vacío")
  } else if (is.numeric(species)) {
    if (species <= 0 || !is.finite(species)) stop("Número de especie debe ser positivo")
  } else {
    stop("Especie debe ser número o nombre")
  }

  # Validar peso inicial
  if (!is.numeric(initial_weight) || initial_weight <= 0) {
    stop("Peso inicial debe ser un número positivo")
  }

  # Validar tipo de ajuste
  valid_fit_types <- c("Weight", "Consumption", "Ration", "Ration_prey", "p-value")
  if (!fit_to %in% valid_fit_types) {
    stop("fit_to debe ser uno de: ", paste(valid_fit_types, collapse = ", "))
  }

  # Validar valor de ajuste
  if (!is.numeric(fit_value) || !is.finite(fit_value)) {
    stop("fit_value debe ser un número finito")
  }

  if (fit_to == "Weight" && fit_value <= 0) {
    stop("Peso objetivo debe ser positivo")
  }

  if (fit_to == "p-value" && (fit_value < 0 || fit_value > 5)) {
    warning("p-value fuera del rango típico (0-5)")
  }

  # Validar días
  if (!is.numeric(first_day) || !is.numeric(last_day)) {
    stop("first_day y last_day deben ser numéricos")
  }

  if (first_day >= last_day) {
    stop("first_day debe ser menor que last_day")
  }

  if (first_day < 1) {
    stop("first_day debe ser >= 1")
  }

  # Validar datos de temperatura
  validate_time_series_data(temperature_data, "temperature_data",
                            required_cols = c("Day", "Temperature"))

  # Validar datos de dieta
  validate_time_series_data(diet_data, "diet_data",
                            required_cols = "Day", min_cols = 2)

  # Validar datos de energía de presas
  validate_time_series_data(prey_energy_data, "prey_energy_data",
                            required_cols = "Day", min_cols = 2)

  # Verificar que dieta y energía tengan las mismas columnas de presas
  diet_prey_cols <- setdiff(names(diet_data), "Day")
  energy_prey_cols <- setdiff(names(prey_energy_data), "Day")

  if (!identical(sort(diet_prey_cols), sort(energy_prey_cols))) {
    stop("Las columnas de presas deben ser iguales en diet_data y prey_energy_data")
  }

  # Verificar que las proporciones de dieta sumen ~1
  diet_sums <- rowSums(diet_data[diet_prey_cols])
  if (any(abs(diet_sums - 1) > 0.01)) {
    warning("Algunas proporciones de dieta no suman 1.0")
  }
}

#' Validar datos de series de tiempo
#'
#' @param data Datos a validar
#' @param data_name Nombre del conjunto de datos (para mensajes de error)
#' @param required_cols Columnas requeridas
#' @param min_cols Número mínimo de columnas
#' @return NULL
#' @keywords internal
validate_time_series_data <- function(data, data_name, required_cols = NULL, min_cols = NULL) {

  if (is.null(data)) {
    stop(data_name, " no puede ser NULL")
  }

  if (!is.data.frame(data)) {
    stop(data_name, " debe ser un data.frame")
  }

  if (nrow(data) == 0) {
    stop(data_name, " no puede estar vacío")
  }

  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop("Columnas faltantes en ", data_name, ": ", paste(missing_cols, collapse = ", "))
    }
  }

  if (!is.null(min_cols) && ncol(data) < min_cols) {
    stop(data_name, " debe tener al menos ", min_cols, " columnas")
  }

  # Validar columna Day si existe
  if ("Day" %in% names(data)) {
    if (!is.numeric(data$Day)) {
      stop("Columna 'Day' en ", data_name, " debe ser numérica")
    }

    if (any(!is.finite(data$Day))) {
      stop("Columna 'Day' en ", data_name, " contiene valores no finitos")
    }

    if (any(diff(data$Day) <= 0)) {
      warning("Columna 'Day' en ", data_name, " no está en orden ascendente")
    }
  }
}

#' Extraer parámetros de especie
#'
#' @param parms Data frame con parámetros de todas las especies
#' @param species_num Número de especie
#' @return Lista con parámetros de la especie
#' @keywords internal
extract_species_parameters <- function(parms, species_num) {

  if (species_num > nrow(parms)) {
    stop("Número de especie fuera de rango")
  }

  species_row <- parms[species_num, ]

  # Extraer parámetros de consumo
  consumption_params <- list(
    CEQ = species_row$CEQ,
    CA = species_row$CA,
    CB = species_row$CB,
    CQ = species_row$CQ,
    CTO = species_row$CTO,
    CTM = species_row$CTM,
    CTL = species_row$CTL,
    CK1 = species_row$CK1,
    CK4 = species_row$CK4
  )

  # Calcular parámetros adicionales para consumo
  if (consumption_params$CEQ == 2) {
    extra_params <- calculate_consumption_params_eq2(
      consumption_params$CQ, consumption_params$CTM, consumption_params$CTO
    )
    consumption_params <- c(consumption_params, extra_params)
  }

  if (consumption_params$CEQ == 3) {
    extra_params <- calculate_consumption_params_eq3(
      consumption_params$CTO, consumption_params$CQ, consumption_params$CTL,
      consumption_params$CTM, consumption_params$CK1, consumption_params$CK4
    )
    consumption_params <- c(consumption_params, extra_params)
  }

  # Extraer parámetros de respiración
  respiration_params <- list(
    REQ = species_row$REQ,
    RA = species_row$RA,
    RB = species_row$RB,
    RQ = species_row$RQ,
    RTO = species_row$RTO,
    RTM = species_row$RTM,
    RTL = species_row$RTL,
    RK1 = species_row$RK1,
    RK4 = species_row$RK4,
    RK5 = species_row$RK5,
    ACT = species_row$ACT,
    BACT = species_row$BACT,
    SDA = species_row$SDA
  )

  # Calcular parámetros adicionales para respiración
  if (respiration_params$REQ == 2) {
    extra_params <- calculate_respiration_params_eq2(
      respiration_params$RQ, respiration_params$RTM, respiration_params$RTO
    )
    respiration_params <- c(respiration_params, extra_params)
  }

  # Extraer parámetros de egestion y excreción
  egestion_params <- list(
    EGEQ = species_row$EGEQ,
    FA = species_row$FA,
    FB = species_row$FB,
    FG = species_row$FG
  )

  excretion_params <- list(
    EXEQ = species_row$EXEQ,
    UA = species_row$UA,
    UB = species_row$UB,
    UG = species_row$UG
  )

  # Extraer parámetros de densidad energética del depredador
  predator_energy_params <- list(
    PREDEDEQ = species_row$PREDEDEQ,
    ED = species_row$ED,
    Alpha1 = species_row$Alpha1,
    Beta1 = species_row$Beta1,
    Cutoff = species_row$Cutoff,
    Alpha2 = species_row$Alpha2,
    Beta2 = species_row$Beta2
  )

  return(list(
    species_info = list(
      number = species_num,
      name = species_row$Species,
      scientific_name = species_row$Sci_Name,
      source = species_row$Source,
      notes = species_row$Notes
    ),
    consumption = consumption_params,
    respiration = respiration_params,
    egestion = egestion_params,
    excretion = excretion_params,
    predator_energy = predator_energy_params
  ))
}

#' Configurar opciones del modelo
#'
#' @param calc_mortality Calcular mortalidad
#' @param calc_reproduction Calcular reproducción
#' @param calc_contaminant Calcular contaminantes
#' @param calc_nutrient Calcular nutrientes
#' @param ... Opciones adicionales
#' @return Lista con opciones del modelo
#' @keywords internal
setup_model_options <- function(calc_mortality = FALSE,
                                calc_reproduction = FALSE,
                                calc_contaminant = FALSE,
                                calc_nutrient = FALSE,
                                ...) {

  options <- list(
    calc_mortality = calc_mortality,
    calc_reproduction = calc_reproduction,
    calc_contaminant = calc_contaminant,
    calc_nutrient = calc_nutrient
  )

  # Agregar opciones adicionales
  extra_options <- list(...)
  options <- c(options, extra_options)

  return(options)
}

#' Imprimir mensaje de error con información de contexto
#'
#' @param run Número de corrida
#' @param day Día
#' @param wt Peso
#' @param waterT Temperatura del agua
#' @param p Valor p
#' @param outpt Tipo de salida
#' @param msg_num Número de mensaje
#' @param msg_loc Ubicación del mensaje
#' @return NULL (imprime mensaje)
#' @export
prt_msg <- function(run, day, wt, waterT, p, outpt, msg_num, msg_loc) {

  # Mensajes de error predefinidos
  messages <- list(
    "1" = "Error al calcular peso al final del día. Número dentro de sqrt es NaN: no es un número. El pez perdió demasiado peso.",
    "2" = "Error al calcular peso al final del día. Número dentro de sqrt es negativo. El pez perdió demasiado peso.",
    "3" = "El peso del pez se volvió negativo al final de este día.",
    "4" = "No se pudo determinar un p-value adecuado para ajustar el valor deseado."
  )

  if (msg_num == 3 && outpt == "End") {
    message("Mientras se ajustaba el p-value, ")
  }

  if (msg_num == 4 && outpt == "End") {
    message("Mientras se ajustaba el p-value, ")
  }

  message(messages[[as.character(msg_num)]])

  # Crear data frame para el mensaje de error
  error_info <- data.frame(
    Parameter = c("Run", "Day", "W(g)", "T(C)", "p-value", "Msg.number", "Msg.location"),
    Value = c(run, day, wt, waterT, p, msg_num, msg_loc),
    stringsAsFactors = FALSE
  )

  print(error_info)

  return(invisible(NULL))
}

#' Función para salir del programa
#'
#' @return NULL
#' @keywords internal
exit_program <- function() {
  invokeRestart("abort")
}
