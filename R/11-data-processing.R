#' Funciones de Procesamiento de Datos para FB4
#'
#' @name data-processing
#' @aliases data-processing
NULL

# ============================================================================
# FUNCIÓN PRINCIPAL DE INTERPOLACIÓN
# ============================================================================

#' Interpolar series de tiempo
#'
#' Función robusta para interpolar datos temporales
#'
#' @param data Data frame con columna Day y columnas de valores
#' @param value_columns Vector con nombres de columnas a interpolar
#' @param target_days Vector de días objetivo
#' @param method Método de interpolación ("linear", "constant", "spline")
#' @param fill_na_method Método para llenar valores faltantes ("extend", "zero", "mean")
#' @param validate_input Validar estructura de entrada
#' @return Data frame con datos interpolados
#' @keywords internal
interpolate_time_series <- function(data, value_columns, target_days, 
                                    method = "linear", fill_na_method = "extend",
                                    validate_input = TRUE) {
  
  # Validaciones mejoradas
  if (validate_input) {
    if (!"Day" %in% names(data)) {
      stop("Data frame debe tener columna 'Day'")
    }
    
    missing_cols <- setdiff(value_columns, names(data))
    if (length(missing_cols) > 0) {
      stop("Columnas faltantes: ", paste(missing_cols, collapse = ", "))
    }
    
    # Validar que target_days sea numérico y ordenado
    if (!is.numeric(target_days)) {
      stop("target_days debe ser numérico")
    }
    target_days <- sort(unique(target_days))
  }
  
  # Preparar resultado
  n_days <- length(target_days)
  result <- data.frame(Day = target_days)
  
  # Función interna para manejar interpolación
  interpolate_column <- function(values, days, target_days, method) {
    # Remover valores NA
    valid_idx <- !is.na(values) & !is.na(days)
    
    if (sum(valid_idx) < 2) {
      if (sum(valid_idx) == 1) {
        return(rep(values[valid_idx][1], length(target_days)))
      } else {
        return(rep(0, length(target_days)))
      }
    }
    
    clean_days <- days[valid_idx]
    clean_values <- values[valid_idx]
    
    # Realizar interpolación según método
    if (method == "linear") {
      interpolated <- approx(x = clean_days, y = clean_values,
                             xout = target_days, method = "linear", rule = 2)$y
    } else if (method == "constant") {
      interpolated <- approx(x = clean_days, y = clean_values,
                             xout = target_days, method = "constant", rule = 2)$y
    } else if (method == "spline") {
      if (length(clean_days) < 4) {
        interpolated <- approx(x = clean_days, y = clean_values,
                               xout = target_days, method = "linear", rule = 2)$y
      } else {
        interpolated <- spline(x = clean_days, y = clean_values,
                               xout = target_days, method = "natural")$y
      }
    } else {
      stop("Método de interpolación no válido: ", method)
    }
    
    return(interpolated)
  }
  
  # Interpolar cada columna
  for (col in value_columns) {
    interpolated <- interpolate_column(data[[col]], data$Day, target_days, method)
    
    # Manejar valores NA resultantes
    if (any(is.na(interpolated))) {
      if (fill_na_method == "extend") {
        first_valid <- which(!is.na(interpolated))[1]
        last_valid <- tail(which(!is.na(interpolated)), 1)
        
        if (!is.na(first_valid) && first_valid > 1) {
          interpolated[1:(first_valid-1)] <- interpolated[first_valid]
        }
        if (!is.na(last_valid) && last_valid < length(interpolated)) {
          interpolated[(last_valid+1):length(interpolated)] <- interpolated[last_valid]
        }
      } else if (fill_na_method == "zero") {
        interpolated[is.na(interpolated)] <- 0
      } else if (fill_na_method == "mean") {
        mean_val <- mean(interpolated, na.rm = TRUE)
        interpolated[is.na(interpolated)] <- mean_val
      }
    }
    
    result[[col]] <- interpolated
  }
  
  return(result)
}

# ============================================================================
# FUNCIÓN PRINCIPAL DE PROCESAMIENTO DE DATOS DE ENTRADA
# ============================================================================

#' Procesar datos de entrada para el modelo
#'
#' @param temperature_data Data frame con columnas Day y Temperature
#' @param diet_data Data frame con proporciones de dieta
#' @param prey_energy_data Data frame con energías de presas
#' @param first_day Primer día de simulación
#' @param last_day Último día de simulación
#' @param interp_method Método de interpolación (por defecto "linear")
#' @param fill_na_method Método para rellenar NA ("extend" o "zero")
#' @return Lista con datos procesados
#' @export
process_input_data <- function(temperature_data, diet_data, prey_energy_data, 
                               first_day = 1, last_day = NULL, interp_method = "linear",
                               fill_na_method = "extend") {
  
  if (is.null(last_day)) {
    last_day <- max(temperature_data$Day)
  }
  
  if (first_day >= last_day || first_day < 1) {
    stop("Rango de días inválido: first_day debe ser < last_day y >= 1")
  }
  
  validate_time_series_data(temperature_data, "temperature_data", c("Day", "Temperature"))
  validate_time_series_data(diet_data, "diet_data", "Day", min_cols = 2)
  validate_time_series_data(prey_energy_data, "prey_energy_data", "Day", min_cols = 2)
  validate_diet_consistency(diet_data, prey_energy_data)
  
  temp_filtered   <- temperature_data[temperature_data$Day >= first_day & temperature_data$Day <= last_day, ]
  diet_filtered   <- diet_data[diet_data$Day >= first_day & diet_data$Day <= last_day, ]
  energy_filtered <- prey_energy_data[prey_energy_data$Day >= first_day & prey_energy_data$Day <= last_day, ]
  
  if (nrow(temp_filtered) == 0) stop("No hay datos de temperatura en el rango especificado")
  if (nrow(diet_filtered) == 0) stop("No hay datos de dieta en el rango especificado")
  
  all_days <- seq(first_day, last_day, by = 1)
  
  temp_complete <- interpolate_time_series(
    data = temp_filtered,
    value_columns = "Temperature",
    target_days = all_days,
    method = interp_method,
    fill_na_method = fill_na_method,
    validate_input = TRUE
  )
  
  diet_complete <- interpolate_time_series(
    data = diet_filtered,
    value_columns = setdiff(names(diet_filtered), "Day"),
    target_days = all_days,
    method = interp_method,
    fill_na_method = fill_na_method,
    validate_input = TRUE
  )
  
  energy_complete <- interpolate_time_series(
    data = energy_filtered,
    value_columns = setdiff(names(energy_filtered), "Day"),
    target_days = all_days,
    method = interp_method,
    fill_na_method = fill_na_method,
    validate_input = TRUE
  )
  
  prey_names <- setdiff(names(diet_data), "Day")
  
  return(list(
    days = all_days,
    n_days = length(all_days),
    temperature = temp_complete,
    diet = diet_complete,
    prey_energy = energy_complete,
    prey_names = prey_names,
    first_day = first_day,
    last_day = last_day,
    duration = length(all_days)
  ))
}

# ============================================================================
# PROCESAMIENTO DE DATOS OPCIONALES
# ============================================================================

#' Procesar datos opcionales
#'
#' Procesa datos de reproducción, mortalidad, contaminantes y nutrientes
#'
#' @param environmental_data Resultado de process_input_data
#' @param reproduction_data Data frame con datos de reproducción (opcional)
#' @param mortality_data Data frame con datos de mortalidad (opcional)
#' @param contaminant_data Lista con datos de contaminantes (opcional)
#' @param nutrient_data Lista con datos de nutrientes (opcional)
#' @return Lista con datos opcionales procesados
#' @export
process_optional_data <- function(environmental_data, reproduction_data = NULL,
                                  mortality_data = NULL, contaminant_data = NULL,
                                  nutrient_data = NULL) {
  
  target_days <- environmental_data$days
  prey_names <- environmental_data$prey_names
  result <- list()
  
  # Procesar datos de reproducción
  if (!is.null(reproduction_data)) {
    repro_cols <- intersect(c("Reproduction", "Spawning", "Spawn_Fraction"), 
                            names(reproduction_data))
    
    if (length(repro_cols) > 0) {
      repro_result <- interpolate_time_series(
        data = reproduction_data,
        value_columns = repro_cols[1],
        target_days = target_days,
        method = "constant"
      )
      
      # Validar rango [0,1]
      spawn_values <- repro_result[[repro_cols[1]]]
      spawn_values <- clamp(spawn_values, 0, 1)
      repro_result[[repro_cols[1]]] <- spawn_values
      
      result$reproduction <- repro_result
    }
  }
  
  # Procesar datos de mortalidad
  if (!is.null(mortality_data)) {
    mort_cols <- setdiff(names(mortality_data), "Day")
    
    if (length(mort_cols) > 0) {
      mort_result <- interpolate_time_series(
        data = mortality_data,
        value_columns = mort_cols,
        target_days = target_days,
        method = "constant"
      )
      
      # Validar rangos [0,1] y calcular supervivencia
      for (col in mort_cols) {
        mort_values <- mort_result[[col]]
        mort_values <- clamp(mort_values, 0, 1)
        mort_result[[col]] <- mort_values
      }
      
      # Calcular supervivencia combinada
      mort_matrix <- as.matrix(mort_result[, mort_cols, drop = FALSE])
      survival_rates <- apply(mort_matrix, 1, function(x) prod(1 - x))
      mort_result$combined_survival <- survival_rates
      
      result$mortality <- mort_result
    }
  }
  
  # Procesar datos de contaminantes
  if (!is.null(contaminant_data)) {
    result$contaminant <- process_contaminant_data_simple(contaminant_data, target_days, prey_names)
  }
  
  # Procesar datos de nutrientes
  if (!is.null(nutrient_data)) {
    result$nutrient <- process_nutrient_data_simple(nutrient_data, target_days, prey_names)
  }
  
  return(result)
}

#' Procesar datos de contaminantes simplificado
#'
#' @param contaminant_data Lista con datos de contaminantes
#' @param target_days Vector de días objetivo
#' @param prey_names Vector con nombres de presas
#' @return Lista con datos de contaminantes procesados
#' @keywords internal
process_contaminant_data_simple <- function(contaminant_data, target_days, prey_names) {
  
  result <- list()
  
  # Procesar concentraciones en presas
  if ("prey_concentrations" %in% names(contaminant_data)) {
    prey_conc_data <- contaminant_data$prey_concentrations
    
    if (is.data.frame(prey_conc_data)) {
      available_prey <- intersect(prey_names, names(prey_conc_data))
      
      if (length(available_prey) > 0) {
        conc_result <- interpolate_time_series(
          data = prey_conc_data,
          value_columns = available_prey,
          target_days = target_days,
          method = "linear"
        )
        
        # Agregar valores por defecto para presas faltantes
        missing_prey <- setdiff(prey_names, available_prey)
        for (prey in missing_prey) {
          conc_result[[prey]] <- 1.0  # μg/g por defecto
        }
        
        result$prey_concentrations <- conc_result
      }
    } else if (is.numeric(contaminant_data$prey_concentrations)) {
      # Valor constante para todas las presas
      const_val <- contaminant_data$prey_concentrations
      conc_result <- data.frame(Day = target_days)
      for (prey in prey_names) {
        conc_result[[prey]] <- const_val
      }
      result$prey_concentrations <- conc_result
    }
  }
  
  # Procesar eficiencias de asimilación
  if ("assimilation_efficiency" %in% names(contaminant_data)) {
    assim_data <- contaminant_data$assimilation_efficiency
    
    if (is.numeric(assim_data) && length(assim_data) == 1) {
      # Valor constante
      assim_result <- data.frame(Day = target_days)
      for (prey in prey_names) {
        assim_result[[prey]] <- assim_data
      }
      result$assimilation_efficiency <- assim_result
    } else if (is.data.frame(assim_data)) {
      # Datos temporales
      available_prey <- intersect(prey_names, names(assim_data))
      
      if (length(available_prey) > 0) {
        assim_result <- interpolate_time_series(
          data = assim_data,
          value_columns = available_prey,
          target_days = target_days,
          method = "constant"
        )
        
        # Valores por defecto para presas faltantes
        missing_prey <- setdiff(prey_names, available_prey)
        for (prey in missing_prey) {
          assim_result[[prey]] <- 0.8  # 80% por defecto
        }
        
        result$assimilation_efficiency <- assim_result
      }
    }
  }
  
  # Parámetros del modelo de contaminantes
  if ("model_parameters" %in% names(contaminant_data)) {
    result$model_parameters <- contaminant_data$model_parameters
  } else {
    # Parámetros por defecto
    result$model_parameters <- list(
      CONTEQ = 2,  # Modelo con eliminación dependiente de T y peso
      predator_concentration = 1.0  # μg/g
    )
  }
  
  return(result)
}

#' Procesar datos de nutrientes simplificado
#'
#' @param nutrient_data Lista con datos de nutrientes
#' @param target_days Vector de días objetivo
#' @param prey_names Vector con nombres de presas
#' @return Lista con datos de nutrientes procesados
#' @keywords internal
process_nutrient_data_simple <- function(nutrient_data, target_days, prey_names) {
  
  result <- list()
  
  # Procesar datos de nitrógeno
  if ("nitrogen" %in% names(nutrient_data)) {
    n_data <- nutrient_data$nitrogen
    
    # Concentraciones en presas
    if ("prey_concentrations" %in% names(n_data)) {
      if (is.numeric(n_data$prey_concentrations) && length(n_data$prey_concentrations) == 1) {
        # Valor constante
        n_conc_result <- data.frame(Day = target_days)
        for (prey in prey_names) {
          n_conc_result[[prey]] <- n_data$prey_concentrations
        }
        result$nitrogen_prey_concentrations <- n_conc_result
      }
    }
    
    # Eficiencias de asimilación
    if ("assimilation_efficiency" %in% names(n_data)) {
      if (is.numeric(n_data$assimilation_efficiency) && length(n_data$assimilation_efficiency) == 1) {
        n_ae_result <- data.frame(Day = target_days)
        for (prey in prey_names) {
          n_ae_result[[prey]] <- n_data$assimilation_efficiency
        }
        result$nitrogen_assimilation_efficiency <- n_ae_result
      }
    }
    
    # Concentración en depredador
    if ("predator_concentration" %in% names(n_data)) {
      result$nitrogen_predator_concentration <- n_data$predator_concentration
    } else {
      result$nitrogen_predator_concentration <- 0.09  # 9% por defecto
    }
  }
  
  # Procesar datos de fósforo (similar a nitrógeno)
  if ("phosphorus" %in% names(nutrient_data)) {
    p_data <- nutrient_data$phosphorus
    
    # Concentraciones en presas
    if ("prey_concentrations" %in% names(p_data)) {
      if (is.numeric(p_data$prey_concentrations) && length(p_data$prey_concentrations) == 1) {
        p_conc_result <- data.frame(Day = target_days)
        for (prey in prey_names) {
          p_conc_result[[prey]] <- p_data$prey_concentrations
        }
        result$phosphorus_prey_concentrations <- p_conc_result
      }
    }
    
    # Eficiencias de asimilación
    if ("assimilation_efficiency" %in% names(p_data)) {
      if (is.numeric(p_data$assimilation_efficiency) && length(p_data$assimilation_efficiency) == 1) {
        p_ae_result <- data.frame(Day = target_days)
        for (prey in prey_names) {
          p_ae_result[[prey]] <- p_data$assimilation_efficiency
        }
        result$phosphorus_assimilation_efficiency <- p_ae_result
      }
    }
    
    # Concentración en depredador
    if ("predator_concentration" %in% names(p_data)) {
      result$phosphorus_predator_concentration <- p_data$predator_concentration
    } else {
      result$phosphorus_predator_concentration <- 0.015  # 1.5% por defecto
    }
  }
  
  return(result)
}

# ============================================================================
# FUNCIONES DE VALIDACIÓN DE DATOS PROCESADOS
# ============================================================================

#' Validar datos procesados
#'
#' Verifica la consistencia e integridad de los datos procesados
#'
#' @param processed_data Lista con datos procesados
#' @return Lista con resultados de validación
#' @export
validate_processed_data <- function(processed_data) {
  
  validation <- list(
    valid = TRUE,
    warnings = character(),
    errors = character()
  )
  
  # Verificar estructura básica
  required_fields <- c("days", "n_days", "temperature", "diet", "prey_energy")
  missing_fields <- setdiff(required_fields, names(processed_data))
  
  if (length(missing_fields) > 0) {
    validation$errors <- c(validation$errors,
                           paste("Campos requeridos faltantes:", paste(missing_fields, collapse = ", ")))
    validation$valid <- FALSE
  }
  
  # Validar temperatura
  if ("temperature" %in% names(processed_data)) {
    temps <- processed_data$temperature$Temperature
    
    if (any(is.na(temps))) {
      validation$errors <- c(validation$errors, "Valores de temperatura faltantes (NA)")
      validation$valid <- FALSE
    }
    
    if (any(temps < -10 | temps > 50)) {
      validation$warnings <- c(validation$warnings, "Temperaturas fuera de rango típico (-10 a 50°C)")
    }
  }
  
  # Validar proporciones de dieta
  if ("diet" %in% names(processed_data)) {
    diet_data <- processed_data$diet
    prey_cols <- setdiff(names(diet_data), "Day")
    
    if (length(prey_cols) > 0) {
      diet_matrix <- as.matrix(diet_data[, prey_cols, drop = FALSE])
      row_sums <- rowSums(diet_matrix)
      
      # Verificar que sumen aproximadamente 1
      if (any(abs(row_sums - 1) > 0.1)) {
        validation$warnings <- c(validation$warnings, "Proporciones de dieta no suman 1.0")
      }
      
      # Verificar valores no negativos
      if (any(diet_matrix < 0)) {
        validation$errors <- c(validation$errors, "Proporciones de dieta negativas")
        validation$valid <- FALSE
      }
    }
  }
  
  # Validar energías de presa
  if ("prey_energy" %in% names(processed_data)) {
    energy_data <- processed_data$prey_energy
    prey_cols <- setdiff(names(energy_data), "Day")
    
    if (length(prey_cols) > 0) {
      energy_matrix <- as.matrix(energy_data[, prey_cols, drop = FALSE])
      
      if (any(energy_matrix < 500 | energy_matrix > 15000)) {
        validation$warnings <- c(validation$warnings, 
                                 "Energías de presa fuera de rango típico (500-15000 J/g)")
      }
      
      if (any(energy_matrix <= 0)) {
        validation$errors <- c(validation$errors, "Energías de presa no positivas")
        validation$valid <- FALSE
      }
    }
  }
  
  # Validar datos de reproducción si existen
  if ("reproduction" %in% names(processed_data)) {
    repro_data <- processed_data$reproduction
    repro_cols <- setdiff(names(repro_data), "Day")
    
    if (length(repro_cols) > 0) {
      repro_values <- repro_data[[repro_cols[1]]]
      
      if (any(repro_values < 0 | repro_values > 1)) {
        validation$errors <- c(validation$errors, "Valores de reproducción fuera de rango [0,1]")
        validation$valid <- FALSE
      }
      
      if (sum(repro_values) > 2) {
        validation$warnings <- c(validation$warnings, "Reproducción anual total muy alta")
      }
    }
  }
  
  # Validar datos de mortalidad si existen
  if ("mortality" %in% names(processed_data)) {
    mort_data <- processed_data$mortality
    mort_cols <- setdiff(names(mort_data), c("Day", "combined_survival"))
    
    if (length(mort_cols) > 0) {
      mort_matrix <- as.matrix(mort_data[, mort_cols, drop = FALSE])
      
      if (any(mort_matrix < 0 | mort_matrix > 1)) {
        validation$errors <- c(validation$errors, "Tasas de mortalidad fuera de rango [0,1]")
        validation$valid <- FALSE
      }
      
      # Verificar mortalidad combinada excesiva
      if ("combined_survival" %in% names(mort_data)) {
        final_survival <- tail(mort_data$combined_survival, 1)
        if (final_survival < 0.01) {
          validation$warnings <- c(validation$warnings, "Supervivencia final muy baja (<1%)")
        }
      }
    }
  }
  
  return(validation)
}



# ============================================================================
# FUNCIONES PARA VALIDACIÓN DE BALANCE ENERGÉTICO
# ============================================================================

#' Validar balance energético como FB4 original
#'
#' @param simulation_result Resultado de la simulación
#' @param tolerance Tolerancia para considerar balance válido
#' @return Lista con información del balance energético
#' @export
validate_energy_balance_fb4 <- function(simulation_result, tolerance = 1e-6) {
  
  if (is.null(simulation_result$daily_output)) {
    return(list(
      balanced = TRUE,
      message = "Sin datos diarios para validar balance",
      relative_error = 0
    ))
  }
  
  daily_data <- simulation_result$daily_output
  
  # Calcular energías inicial y final
  initial_weight <- daily_data$Weight.g[1]
  final_weight <- tail(daily_data$Weight.g, 1)
  
  # Calcular densidades energéticas (asumiendo que están en los datos diarios)
  if ("Predator.Energy.Density.J.g" %in% names(daily_data)) {
    initial_ed <- daily_data$Predator.Energy.Density.J.g[1]
    final_ed <- tail(daily_data$Predator.Energy.Density.J.g, 1)
  } else {
    # Si no están disponibles, usar valores por defecto
    initial_ed <- 5000
    final_ed <- 5000
  }
  
  initial_body_energy <- initial_weight * initial_ed
  final_body_energy <- final_weight * final_ed
  
  # Calcular energías acumuladas
  total_consumption <- sum(daily_data$Consumption.J.g.d * daily_data$Weight.g, na.rm = TRUE)
  total_respiration <- sum(daily_data$Respiration.J.g.d * daily_data$Weight.g, na.rm = TRUE)
  total_egestion <- sum(daily_data$Egestion.J.g.d * daily_data$Weight.g, na.rm = TRUE)
  total_excretion <- sum(daily_data$Excretion.J.g.d * daily_data$Weight.g, na.rm = TRUE)
  total_sda <- sum(daily_data$SDA.J.g.d * daily_data$Weight.g, na.rm = TRUE)
  
  # Energía de reproducción si está disponible
  total_spawn <- 0
  if ("Spawn.Energy.J.g.d" %in% names(daily_data)) {
    total_spawn <- sum(daily_data$Spawn.Energy.J.g.d * daily_data$Weight.g, na.rm = TRUE)
  }
  
  # Balance energético: Energía inicial + Consumo = Energía final + Respiración + Egestión + Excreción + SDA + Reproducción
  energy_in <- initial_body_energy + total_consumption
  energy_out <- final_body_energy + total_respiration + total_egestion + total_excretion + total_sda + total_spawn
  
  energy_difference <- abs(energy_in - energy_out)
  relative_error <- if (energy_in > 0) energy_difference / energy_in else 0
  
  balanced <- relative_error < tolerance
  
  return(list(
    balanced = balanced,
    message = if (balanced) "Balance energético OK" else "Advertencia: Balance energético no válido",
    relative_error = relative_error,
    energy_difference = energy_difference,
    energy_in = energy_in,
    energy_out = energy_out,
    components = list(
      initial_body_energy = initial_body_energy,
      final_body_energy = final_body_energy,
      total_consumption = total_consumption,
      total_respiration = total_respiration,
      total_egestion = total_egestion,
      total_excretion = total_excretion,
      total_sda = total_sda,
      total_spawn = total_spawn
    )
  ))
}

# ============================================================================
# FUNCIONES PARA RESULTADOS DE AJUSTE FB4
# ============================================================================

#' Validar resultado de ajuste FB4
#'
#' @param fit_result Resultado de función de ajuste FB4
#' @return Lista con información de validación
#' @export
validate_fb4_fit_result <- function(fit_result) {
  
  validation <- list(
    valid = TRUE,
    warnings = character(),
    errors = character(),
    recommendations = character()
  )
  
  # Verificar estructura básica
  required_fields <- c("fit_successful", "p_value", "fit_iterations")
  missing_fields <- setdiff(required_fields, names(fit_result))
  
  if (length(missing_fields) > 0) {
    validation$errors <- c(validation$errors, 
                           paste("Campos faltantes:", paste(missing_fields, collapse = ", ")))
    validation$valid <- FALSE
  }
  
  # Verificar convergencia
  if ("fit_successful" %in% names(fit_result)) {
    if (!fit_result$fit_successful) {
      validation$warnings <- c(validation$warnings, "El ajuste no convergió")
      validation$recommendations <- c(validation$recommendations, 
                                      "Considerar aumentar max_iterations o ajustar tolerance")
    }
  }
  
  # Verificar p-value
  if ("p_value" %in% names(fit_result)) {
    p_val <- fit_result$p_value
    if (p_val <= 0.001) {
      validation$warnings <- c(validation$warnings, "p-value muy bajo (posible subalimentación)")
      validation$recommendations <- c(validation$recommendations, 
                                      "Revisar parámetros de consumo o condiciones ambientales")
    } else if (p_val >= 4.999) {
      validation$warnings <- c(validation$warnings, "p-value muy alto (posible sobrealimentación)")
      validation$recommendations <- c(validation$recommendations, 
                                      "Revisar objetivo de ajuste o parámetros metabólicos")
    }
  }
  
  # Verificar error final
  if ("fit_error" %in% names(fit_result)) {
    if (!is.na(fit_result$fit_error)) {
      if (fit_result$fit_error > 0.1) {
        validation$warnings <- c(validation$warnings, "Error de ajuste alto")
        validation$recommendations <- c(validation$recommendations, 
                                        "Considerar disminuir tolerance o revisar datos de entrada")
      }
    }
  }
  
  # Verificar balance energético si está disponible
  if ("energy_balance" %in% names(fit_result)) {
    if (!fit_result$energy_balance$balanced) {
      validation$warnings <- c(validation$warnings, "Balance energético no válido")
      validation$recommendations <- c(validation$recommendations, 
                                      "Revisar cálculos metabólicos y parámetros de especies")
    }
  }
  
  return(validation)
}

#' Generar resumen detallado de ajuste FB4
#'
#' @param fit_result Resultado de función de ajuste FB4
#' @return Data frame con resumen detallado
#' @export
summarize_fb4_fit_result <- function(fit_result) {
  
  summary_data <- data.frame(
    Parameter = character(),
    Value = character(),
    Status = character(),
    stringsAsFactors = FALSE
  )
  
  # Información básica del ajuste
  if ("p_value" %in% names(fit_result)) {
    status <- if (fit_result$p_value <= 0.001) "Bajo" else if (fit_result$p_value >= 4.999) "Alto" else "Normal"
    summary_data <- rbind(summary_data, 
                          data.frame(Parameter = "P-value", 
                                     Value = round(fit_result$p_value, 4),
                                     Status = status))
  }
  
  if ("fit_successful" %in% names(fit_result)) {
    status <- if (fit_result$fit_successful) "Éxito" else "Fallo"
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Convergencia",
                                     Value = fit_result$fit_successful,
                                     Status = status))
  }
  
  if ("fit_iterations" %in% names(fit_result)) {
    status <- if (fit_result$fit_iterations < 10) "Rápido" else if (fit_result$fit_iterations > 20) "Lento" else "Normal"
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Iteraciones",
                                     Value = fit_result$fit_iterations,
                                     Status = status))
  }
  
  if ("fit_error" %in% names(fit_result)) {
    status <- if (is.na(fit_result$fit_error)) "N/A" else if (fit_result$fit_error < 0.001) "Excelente" else if (fit_result$fit_error < 0.01) "Bueno" else "Regular"
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Error final",
                                     Value = if (is.na(fit_result$fit_error)) "N/A" else round(fit_result$fit_error, 6),
                                     Status = status))
  }
  
  # Información de la simulación
  if ("final_weight" %in% names(fit_result)) {
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Peso final (g)",
                                     Value = round(fit_result$final_weight, 2),
                                     Status = ""))
  }
  
  if ("total_consumption" %in% names(fit_result)) {
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Consumo total (g)",
                                     Value = round(fit_result$total_consumption, 2),
                                     Status = ""))
  }
  
  # Balance energético
  if ("energy_balance" %in% names(fit_result)) {
    status <- if (fit_result$energy_balance$balanced) "Válido" else "Advertencia"
    summary_data <- rbind(summary_data,
                          data.frame(Parameter = "Balance energético",
                                     Value = fit_result$energy_balance$message,
                                     Status = status))
  }
  
  return(summary_data)
}

# ============================================================================
# FUNCIONES PARA OBJETOS BIOENERGETIC
# ============================================================================

#' Validar entradas para FB4 con objeto Bioenergetic
#' @keywords internal
validate_fb4_bioenergetic_inputs <- function(bio_obj, fit_to, fit_value, 
                                             first_day, last_day) {
  
  if (first_day >= last_day) {
    stop("first_day debe ser menor que last_day")
  }
  
  if (!fit_to %in% c("Weight", "Consumption", "Ration", "Ration_prey", "p-value")) {
    stop("fit_to debe ser uno de: Weight, Consumption, Ration, Ration_prey, p-value")
  }
  
  # Verificar que los días solicitados estén disponibles en los datos
  temp_days <- bio_obj$environmental_data$temperature$Day
  if (first_day < min(temp_days) || last_day > max(temp_days)) {
    stop("Los días solicitados exceden el rango de datos de temperatura disponibles")
  }
  
  diet_days <- bio_obj$diet_data$proportions$Day
  if (first_day < min(diet_days) || last_day > max(diet_days)) {
    stop("Los días solicitados exceden el rango de datos de dieta disponibles")
  }
}

#' Procesar datos desde objeto Bioenergetic
#' @keywords internal
process_bioenergetic_data <- function(bio_obj, first_day, last_day) {
  
  target_days <- first_day:last_day
  
  # Procesar temperatura
  temp_df <- bio_obj$environmental_data$temperature
  temp_subset <- temp_df[temp_df$Day %in% target_days, ]
  temp_subset <- temp_subset[order(temp_subset$Day), ]
  
  if (nrow(temp_subset) != length(target_days)) {
    warning("Algunos días de temperatura faltantes, se usará interpolación")
    # Interpolación simple
    full_temp <- approx(temp_subset$Day, temp_subset$Temperature, 
                        xout = target_days, rule = 2)$y
  } else {
    full_temp <- temp_subset$Temperature
  }
  
  # Procesar dieta
  diet_props <- bio_obj$diet_data$proportions
  diet_energies <- bio_obj$diet_data$energies
  prey_names <- bio_obj$diet_data$prey_names
  prey_indigestible <- bio_obj$diet_data$indigestible
  
  # Subconjunto de días
  diet_subset <- diet_props[diet_props$Day %in% target_days, ]
  energy_subset <- diet_energies[diet_energies$Day %in% target_days, ]
  prey_indigestible_subset <- prey_indigestible[prey_indigestible$Day %in% target_days, ]
  
  # Ordenar por día
  diet_subset <- diet_subset[order(diet_subset$Day), ]
  energy_subset <- energy_subset[order(energy_subset$Day), ]
  prey_indigestible_subset <- prey_indigestible_subset[order(prey_indigestible_subset$Day), ]
  
  # Extraer solo las columnas de presas (sin Day)
  diet_matrix <- as.matrix(diet_subset[, prey_names, drop = FALSE])
  energy_matrix <- as.matrix(energy_subset[, prey_names, drop = FALSE])
  prey_indigestible_matrix <- as.matrix(prey_indigestible_subset[, prey_names, drop = FALSE])
  
  # Normalizar proporciones de dieta por día
  row_sums <- rowSums(diet_matrix, na.rm = TRUE)
  diet_matrix <- diet_matrix / ifelse(row_sums == 0, 1, row_sums)
  
  return(list(
    temperature = full_temp,
    diet_proportions = diet_matrix,
    prey_energies = energy_matrix,
    prey_indigestible = prey_indigestible_matrix,
    duration = length(target_days),
    prey_names = prey_names,
    first_day = first_day,
    last_day = last_day
  ))
}

#' Configurar opciones del modelo desde objeto Bioenergetic
#' @keywords internal
setup_model_options_from_bioenergetic <- function(bio_obj, ...) {
  
  # Opciones por defecto
  default_options <- list(
    calc_mortality = FALSE,
    calc_reproduction = FALSE,
    calc_contaminant = FALSE,
    calc_nutrient = FALSE
  )
  
  # Combinar con opciones del objeto si existen
  if (!is.null(bio_obj$model_options)) {
    default_options <- modifyList(default_options, bio_obj$model_options)
  }
  
  # Sobrescribir con argumentos adicionales
  extra_args <- list(...)
  if (length(extra_args) > 0) {
    valid_options <- names(default_options)
    extra_options <- extra_args[names(extra_args) %in% valid_options]
    default_options <- modifyList(default_options, extra_options)
  }
  
  return(default_options)
}

#' Validar objeto Bioenergetic para FB4
#' @keywords internal
validate_bioenergetic_object <- function(bio_obj) {
  
  # Verificar componentes esenciales
  required_components <- c("species_params", "environmental_data", 
                           "diet_data", "simulation_settings")
  missing <- setdiff(required_components, names(bio_obj))
  if (length(missing) > 0) {
    stop("Objeto Bioenergetic falta componentes: ", paste(missing, collapse = ", "))
  }
  
  # Verificar parámetros de especie
  required_param_categories <- c("consumption", "respiration", "egestion", 
                                 "excretion", "predator")
  missing_params <- setdiff(required_param_categories, names(bio_obj$species_params))
  if (length(missing_params) > 0) {
    stop("species_params falta categorías: ", paste(missing_params, collapse = ", "))
  }
  
  # Verificar datos ambientales
  if (is.null(bio_obj$environmental_data$temperature) || 
      !is.data.frame(bio_obj$environmental_data$temperature)) {
    stop("environmental_data debe contener un data.frame 'temperature'")
  }
  
  temp_df <- bio_obj$environmental_data$temperature
  if (!all(c("Day", "Temperature") %in% names(temp_df))) {
    stop("temperature data.frame debe tener columnas 'Day' y 'Temperature'")
  }
  
  # Verificar datos de dieta
  if (is.null(bio_obj$diet_data$proportions) || 
      !is.data.frame(bio_obj$diet_data$proportions)) {
    stop("diet_data debe contener un data.frame 'proportions'")
  }
  
  if (is.null(bio_obj$diet_data$energies) || 
      !is.data.frame(bio_obj$diet_data$energies)) {
    stop("diet_data debe contener un data.frame 'energies'")
  }
  
  # Verificar configuración de simulación
  if (is.null(bio_obj$simulation_settings$initial_weight) || 
      bio_obj$simulation_settings$initial_weight <= 0) {
    stop("simulation_settings debe contener initial_weight > 0")
  }
}

# ============================================================================
# FUNCIONES HEREDADAS PARA COMPATIBILIDAD (FUNCIONES SIMPLIFICADAS)
# ============================================================================

#' Procesar archivos de entrada para FB4 (versión simplificada)
#'
#' @param temperature_data Data frame con Day y Temperature
#' @param diet_data Data frame con Day y proporciones de presa
#' @param prey_energy_data Data frame con Day y energías de presa
#' @param first_day Primer día de simulación
#' @param last_day Último día de simulación
#' @return Lista con datos procesados para FB4
#' @export
process_fb4_input_files <- function(temperature_data, diet_data, prey_energy_data,
                                    first_day, last_day) {
  
  # Validar datos de temperatura
  if (is.null(temperature_data) || !all(c("Day", "Temperature") %in% names(temperature_data))) {
    stop("temperature_data debe tener columnas 'Day' y 'Temperature'")
  }
  
  # Filtrar por rango de días
  temp_filtered <- temperature_data[
    temperature_data$Day >= first_day & temperature_data$Day <= last_day, 
  ]
  
  if (nrow(temp_filtered) == 0) {
    stop("No hay datos de temperatura para el rango de días especificado")
  }
  
  # Crear secuencia completa de días
  all_days <- first_day:last_day
  duration <- length(all_days)
  
  # Interpolar temperatura para días faltantes
  temperature <- approx(temp_filtered$Day, temp_filtered$Temperature, 
                        xout = all_days, method = "linear", rule = 2)$y
  
  # Procesar datos de dieta
  if (!is.null(diet_data)) {
    diet_filtered <- diet_data[diet_data$Day >= first_day & diet_data$Day <= last_day, ]
    
    # Obtener columnas de proporciones (excluyendo 'Day')
    prop_cols <- names(diet_filtered)[names(diet_filtered) != "Day"]
    
    # Crear matriz de proporciones interpoladas
    diet_proportions <- matrix(0, nrow = duration, ncol = length(prop_cols))
    colnames(diet_proportions) <- prop_cols
    
    for (i in seq_along(prop_cols)) {
      col_data <- diet_filtered[, prop_cols[i]]
      diet_proportions[, i] <- approx(diet_filtered$Day, col_data, 
                                      xout = all_days, method = "linear", rule = 2)$y
    }
    
    # Normalizar proporciones para que sumen 1
    row_sums <- rowSums(diet_proportions, na.rm = TRUE)
    diet_proportions <- diet_proportions / pmax(row_sums, 1e-10)
  } else {
    # Si no hay datos de dieta, asumir una presa con proporción 1
    diet_proportions <- matrix(1, nrow = duration, ncol = 1)
    colnames(diet_proportions) <- "Prey1"
  }
  
  # Procesar datos de energía de presa
  if (!is.null(prey_energy_data)) {
    energy_filtered <- prey_energy_data[
      prey_energy_data$Day >= first_day & prey_energy_data$Day <= last_day, 
    ]
    
    # Obtener columnas de energía (excluyendo 'Day')
    energy_cols <- names(energy_filtered)[names(energy_filtered) != "Day"]
    
    # Crear matriz de energías interpoladas
    prey_energies <- matrix(0, nrow = duration, ncol = length(energy_cols))
    colnames(prey_energies) <- energy_cols
    
    for (i in seq_along(energy_cols)) {
      col_data <- energy_filtered[, energy_cols[i]]
      prey_energies[, i] <- approx(energy_filtered$Day, col_data, 
                                   xout = all_days, method = "linear", rule = 2)$y
    }
  } else {
    # Si no hay datos de energía, usar valor por defecto
    prey_energies <- matrix(4000, nrow = duration, ncol = ncol(diet_proportions))
    colnames(prey_energies) <- colnames(diet_proportions)
  }
  
  # Verificar consistencia entre dieta y energías
  if (ncol(diet_proportions) != ncol(prey_energies)) {
    warning("Número de presas en dieta y energías no coincide. Ajustando...")
    
    # Tomar el mínimo número de columnas
    min_cols <- min(ncol(diet_proportions), ncol(prey_energies))
    diet_proportions <- diet_proportions[, 1:min_cols, drop = FALSE]
    prey_energies <- prey_energies[, 1:min_cols, drop = FALSE]
  }
  
  return(list(
    temperature = temperature,
    diet_proportions = diet_proportions,
    prey_energies = prey_energies,
    duration = duration,
    first_day = first_day,
    last_day = last_day
  ))
}