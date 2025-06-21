#' Funciones de Procesamiento de Datos para FB4
#'
#' @name data-processing
#' @aliases data-processing
NULL

# ============================================================================
# FUNCIONES CORE DE INTERPOLACIÓN
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


#' Interpolar datos de dieta
#'
#' Especializada para datos de proporciones de dieta
#'
#' @param diet_data Data frame con proporciones de dieta por día
#' @param target_days Vector de días objetivo
#' @param normalize Normalizar proporciones para que sumen 1
#' @return Data frame con dieta interpolada
#' @keywords internal
interpolate_diet_data <- function(diet_data, target_days, normalize = TRUE) {
  
  # Identificar columnas de presa (todas excepto Day)
  prey_columns <- setdiff(names(diet_data), "Day")
  
  if (length(prey_columns) == 0) {
    stop("No se encontraron columnas de presa en diet_data")
  }
  
  # Interpolar proporciones
  interpolated <- interpolate_time_series(
    data = diet_data,
    value_columns = prey_columns,
    target_days = target_days,
    method = "linear",
    fill_na_method = "extend"
  )
  
  # Normalizar proporciones si se solicita
  if (normalize) {
    prop_matrix <- as.matrix(interpolated[, prey_columns, drop = FALSE])
    
    # Asegurar valores no negativos
    prop_matrix[prop_matrix < 0] <- 0
    
    # Normalizar filas para que sumen 1
    row_sums <- rowSums(prop_matrix)
    zero_sum_rows <- row_sums == 0
    
    if (any(zero_sum_rows)) {
      warning("Algunas filas tienen suma cero, asignando proporciones uniformes")
      prop_matrix[zero_sum_rows, ] <- 1 / ncol(prop_matrix)
      row_sums[zero_sum_rows] <- 1
    }
    
    prop_matrix <- prop_matrix / row_sums
    interpolated[, prey_columns] <- prop_matrix
  }
  
  return(interpolated)
}

# ============================================================================
# FUNCIONES DE PROCESAMIENTO PRINCIPAL
# ============================================================================

#' Procesar datos ambientales básicos
#'
#' Procesa temperatura, dieta y energías de presa
#'
#' @param temperature_data Data frame con datos de temperatura
#' @param diet_data Data frame con proporciones de dieta (opcional)
#' @param prey_energy_data Data frame con energías de presa (opcional)
#' @param target_days Vector de días para simulación
#' @return Lista con datos ambientales procesados
#' @export
process_environmental_data <- function(temperature_data, diet_data = NULL, 
                                       prey_energy_data = NULL, target_days) {
  
  # Validar datos de temperatura
  if (is.null(temperature_data)) {
    stop("temperature_data es requerido")
  }
  
  target_days <- sort(unique(target_days))
  n_days <- length(target_days)
  
  # Procesar temperatura
  temp_result <- interpolate_time_series(
    data = temperature_data,
    value_columns = "Temperature",
    target_days = target_days,
    method = "linear"
  )
  
  result <- list(
    days = target_days,
    n_days = n_days,
    temperature = temp_result
  )
  
  # Procesar datos de dieta si se proporcionan
  if (!is.null(diet_data)) {
    diet_result <- interpolate_diet_data(diet_data, target_days, normalize = TRUE)
    prey_names <- setdiff(names(diet_result), "Day")
    
    result$diet <- diet_result
    result$prey_names <- prey_names
    result$n_prey <- length(prey_names)
    
    # Procesar energías de presa
    if (!is.null(prey_energy_data)) {
      # Verificar que las columnas de presa coincidan
      energy_prey_cols <- intersect(prey_names, names(prey_energy_data))
      
      if (length(energy_prey_cols) == 0) {
        warning("No hay coincidencia entre presas en diet_data y prey_energy_data")
        # Usar energías por defecto
        energy_result <- data.frame(Day = target_days)
        for (prey in prey_names) {
          energy_result[[prey]] <- 4000  # J/g por defecto
        }
      } else {
        energy_result <- interpolate_time_series(
          data = prey_energy_data,
          value_columns = energy_prey_cols,
          target_days = target_days,
          method = "linear"
        )
        
        # Agregar energías por defecto para presas faltantes
        missing_prey <- setdiff(prey_names, energy_prey_cols)
        for (prey in missing_prey) {
          energy_result[[prey]] <- 4000  # J/g por defecto
        }
      }
      
      result$prey_energy <- energy_result
    } else {
      # Crear energías por defecto
      energy_result <- data.frame(Day = target_days)
      for (prey in prey_names) {
        energy_result[[prey]] <- 4000  # J/g por defecto
      }
      result$prey_energy <- energy_result
    }
  } else {
    # Configuración mínima: una presa genérica
    result$prey_names <- "generic_prey"
    result$n_prey <- 1
    
    diet_result <- data.frame(Day = target_days, generic_prey = 1.0)
    energy_result <- data.frame(Day = target_days, generic_prey = 4000)
    
    result$diet <- diet_result
    result$prey_energy <- energy_result
  }
  
  return(result)
}

#' Procesar datos opcionales
#'
#' Procesa datos de reproducción, mortalidad, contaminantes y nutrientes
#'
#' @param environmental_data Resultado de process_environmental_data
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
# FUNCIONES DE ALTO NIVEL
# ============================================================================

#' Procesar todos los datos de entrada
#'
#' Función principal para procesar todos los tipos de datos
#'
#' @param temperature_data Data frame con datos de temperatura
#' @param simulation_days Vector de días para simulación
#' @param diet_data Data frame con proporciones de dieta (opcional)
#' @param prey_energy_data Data frame con energías de presa (opcional)
#' @param reproduction_data Data frame con datos de reproducción (opcional)
#' @param mortality_data Data frame con datos de mortalidad (opcional)
#' @param contaminant_data Lista con datos de contaminantes (opcional)
#' @param nutrient_data Lista con datos de nutrientes (opcional)
#' @return Lista con todos los datos procesados
#' @export
process_all_input_data <- function(temperature_data, simulation_days,
                                   diet_data = NULL, prey_energy_data = NULL,
                                   reproduction_data = NULL, mortality_data = NULL,
                                   contaminant_data = NULL, nutrient_data = NULL) {
  
  # Procesar datos ambientales básicos
  env_data <- process_environmental_data(
    temperature_data = temperature_data,
    diet_data = diet_data,
    prey_energy_data = prey_energy_data,
    target_days = simulation_days
  )
  
  # Procesar datos opcionales
  optional_data <- process_optional_data(
    environmental_data = env_data,
    reproduction_data = reproduction_data,
    mortality_data = mortality_data,
    contaminant_data = contaminant_data,
    nutrient_data = nutrient_data
  )
  
  # Combinar resultados
  result <- c(env_data, optional_data)
  
  return(result)
}

# ============================================================================
# FUNCIONES DE VALIDACIÓN
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

#' Generar resumen de datos procesados
#'
#' Crea un resumen descriptivo de los datos procesados
#'
#' @param processed_data Lista con datos procesados
#' @return Data frame con resumen
#' @export
summarize_processed_data <- function(processed_data) {
  
  summary_data <- data.frame(
    Component = character(),
    Description = character(),
    Value = character(),
    stringsAsFactors = FALSE
  )
  
  # Información básica
  if ("n_days" %in% names(processed_data)) {
    summary_data <- rbind(summary_data,
                          data.frame(Component = "Simulation",
                                     Description = "Number of days",
                                     Value = processed_data$n_days))
  }
  
  if ("days" %in% names(processed_data)) {
    day_range <- range(processed_data$days)
    summary_data <- rbind(summary_data,
                          data.frame(Component = "Simulation",
                                     Description = "Day range",
                                     Value = paste(day_range[1], "-", day_range[2])))
  }
  
  # Información de temperatura
  if ("temperature" %in% names(processed_data)) {
    temps <- processed_data$temperature$Temperature
    temp_range <- range(temps, na.rm = TRUE)
    temp_mean <- mean(temps, na.rm = TRUE)
    
    summary_data <- rbind(summary_data,
                          data.frame(Component = "Temperature",
                                     Description = "Range (°C)",
                                     Value = paste(round(temp_range[1], 1), "-", round(temp_range[2], 1))))
    
    summary_data <- rbind(summary_data,
                          data.frame(Component = "Temperature",
                                     Description = "Mean (°C)",
                                     Value = round(temp_mean, 1)))
  }
  
  # Información de dieta
  if ("prey_names" %in% names(processed_data)) {
    summary_data <- rbind(summary_data,
                          data.frame(Component = "Diet",
                                     Description = "Number of prey types",
                                     Value = length(processed_data$prey_names)))
    
    summary_data <- rbind(summary_data,
                          data.frame(Component = "Diet",
                                     Description = "Prey types",
                                     Value = paste(processed_data$prey_names, collapse = ", ")))
  }
  
  # Información de módulos opcionales
  optional_modules <- c("reproduction", "mortality", "contaminant", "nutrient")
  for (module in optional_modules) {
    if (module %in% names(processed_data)) {
      summary_data <- rbind(summary_data,
                            data.frame(Component = "Optional modules",
                                       Description = paste(module, "data"),
                                       Value = "Available"))
    }
  }
  
  return(summary_data)
}

# ============================================================================
# FUNCIONES DE UTILIDAD
# ============================================================================

#' Crear datos de temperatura de ejemplo
#'
#' Genera datos de temperatura sintéticos para pruebas
#'
#' @param days Vector de días
#' @param mean_temp Temperatura media (°C)
#' @param amplitude Amplitud de variación estacional (°C)
#' @param noise Nivel de ruido diario (°C)
#' @return Data frame con datos de temperatura
#' @export
create_example_temperature_data <- function(days, mean_temp = 20, amplitude = 10, noise = 2) {
  
  n_days <- length(days)
  
  # Patrón estacional (sinusoidal)
  seasonal_pattern <- mean_temp + amplitude * sin(2 * pi * (days - 80) / 365)
  
  # Agregar ruido diario
  if (noise > 0) {
    daily_noise <- rnorm(n_days, mean = 0, sd = noise)
    temperatures <- seasonal_pattern + daily_noise
  } else {
    temperatures <- seasonal_pattern
  }
  
  # Limitar a rangos realistas
  temperatures <- clamp(temperatures, -5, 45)
  
  return(data.frame(
    Day = days,
    Temperature = temperatures
  ))
}

#' Crear datos de dieta de ejemplo
#'
#' Genera datos de dieta sintéticos para pruebas
#'
#' @param days Vector de días
#' @param prey_names Vector con nombres de presas
#' @param seasonal_variation Incluir variación estacional
#' @return Data frame con proporciones de dieta
#' @export
create_example_diet_data <- function(days, prey_names = c("zooplankton", "insects"), 
                                     seasonal_variation = TRUE) {
  
  n_days <- length(days)
  n_prey <- length(prey_names)
  
  diet_data <- data.frame(Day = days)
  
  if (seasonal_variation && n_prey >= 2) {
    # Variación estacional entre presas
    for (i in seq_along(prey_names)) {
      # Cada presa tiene un pico estacional diferente
      peak_day <- 90 + (i - 1) * (365 / n_prey)
      seasonal_component <- 0.3 + 0.4 * exp(-((days - peak_day) / 60)^2)
      
      # Normalizar y agregar ruido
      base_proportion <- 1 / n_prey
      variation <- seasonal_component * 0.3
      proportions <- base_proportion + variation + rnorm(n_days, 0, 0.05)
      
      diet_data[[prey_names[i]]] <- pmax(0.01, proportions)
    }
    
    # Normalizar proporciones
    prop_matrix <- as.matrix(diet_data[, prey_names])
    row_sums <- rowSums(prop_matrix)
    diet_data[, prey_names] <- prop_matrix / row_sums
    
  } else {
    # Proporciones uniformes
    for (prey in prey_names) {
      diet_data[[prey]] <- 1 / n_prey
    }
  }
  
  return(diet_data)
}

#' Crear datos de energía de presa de ejemplo
#'
#' Genera datos de energía de presa sintéticos
#'
#' @param days Vector de días
#' @param prey_names Vector con nombres de presas
#' @param base_energies Vector con energías base por presa (J/g)
#' @param seasonal_variation Incluir variación estacional
#' @return Data frame con energías de presa
#' @export
create_example_prey_energy_data <- function(days, prey_names, 
                                            base_energies = NULL, seasonal_variation = FALSE) {
  
  n_days <- length(days)
  n_prey <- length(prey_names)
  
  # Energías base por defecto
  if (is.null(base_energies)) {
    base_energies <- c(3500, 4500, 4000, 5000)[1:n_prey]  # J/g típicas
  } else if (length(base_energies) != n_prey) {
    stop("Longitud de base_energies debe coincidir con prey_names")
  }
  
  energy_data <- data.frame(Day = days)
  
  for (i in seq_along(prey_names)) {
    base_energy <- base_energies[i]
    
    if (seasonal_variation) {
      # Variación estacional (típicamente mayor energía en verano/otoño)
      seasonal_factor <- 1 + 0.2 * sin(2 * pi * (days - 200) / 365)
      daily_noise <- rnorm(n_days, 1, 0.05)
      energies <- base_energy * seasonal_factor * daily_noise
    } else {
      # Energía constante con pequeño ruido
      energies <- rnorm(n_days, base_energy, base_energy * 0.02)
    }
    
    # Limitar a rangos realistas
    energies <- clamp(energies, 1000, 8000)
    energy_data[[prey_names[i]]] <- energies
  }
  
  return(energy_data)
}


#' Función de alto nivel para procesamiento completo
#'
#' Procesa y valida todos los datos de entrada de forma integrada
#'
#' @param input_data Lista con todos los datos de entrada
#' @param simulation_days Vector de días para simulación
#' @param validate_data Ejecutar validaciones automáticas
#' @return Lista con datos procesados y validación
#' @export
process_and_validate_data <- function(input_data, simulation_days, validate_data = TRUE) {
  
  # Verificar datos mínimos requeridos
  if (!"temperature" %in% names(input_data)) {
    stop("Datos de temperatura requeridos")
  }
  
  # Extraer componentes
  temperature_data <- input_data$temperature
  diet_data <- input_data$diet %||% NULL
  prey_energy_data <- input_data$prey_energy %||% NULL
  reproduction_data <- input_data$reproduction %||% NULL
  mortality_data <- input_data$mortality %||% NULL
  contaminant_data <- input_data$contaminant %||% NULL
  nutrient_data <- input_data$nutrient %||% NULL
  
  # Verificar compatibilidad si se solicita
  if (validate_data) {
    compatibility <- check_data_compatibility(temperature_data, diet_data, prey_energy_data)
    if (!compatibility$compatible) {
      stop("Datos incompatibles: ", paste(compatibility$errors, collapse = "; "))
    }
    if (length(compatibility$warnings) > 0) {
      warning("Advertencias de compatibilidad: ", paste(compatibility$warnings, collapse = "; "))
    }
  }
  
  # Procesar todos los datos
  processed_data <- process_all_input_data(
    temperature_data = temperature_data,
    simulation_days = simulation_days,
    diet_data = diet_data,
    prey_energy_data = prey_energy_data,
    reproduction_data = reproduction_data,
    mortality_data = mortality_data,
    contaminant_data = contaminant_data,
    nutrient_data = nutrient_data
  )
  
  # Validar datos procesados si se solicita
  validation_result <- NULL
  if (validate_data) {
    validation_result <- validate_processed_data(processed_data)
    if (!validation_result$valid) {
      stop("Errores en datos procesados: ", paste(validation_result$errors, collapse = "; "))
    }
    if (length(validation_result$warnings) > 0) {
      warning("Advertencias en datos procesados: ", paste(validation_result$warnings, collapse = "; "))
    }
  }
  
  # Crear resumen
  data_summary <- summarize_processed_data(processed_data)
  
  return(list(
    processed_data = processed_data,
    validation = validation_result,
    summary = data_summary,
    input_compatibility = if (validate_data) compatibility else NULL
  ))
}


#' Análisis de sensibilidad simple
#'
#' Evalúa cómo cambios en un parámetro afectan el resultado
#'
#' @param bioenergetic_obj Objeto de clase Bioenergetic
#' @param parameter_name Nombre del parámetro a analizar
#' @param parameter_range Vector con valores a probar
#' @param output_metrics Métricas a evaluar
#' @return Data frame con resultados del análisis
#' @export
analyze_parameter_sensitivity <- function(bioenergetic_obj, parameter_name, 
                                          parameter_range, 
                                          output_metrics = c("final_weight", "total_consumption")) {
  
  if (!is.Bioenergetic(bioenergetic_obj)) {
    stop("Objeto debe ser de clase Bioenergetic")
  }
  
  # Inicializar resultados
  results <- data.frame(
    parameter_value = parameter_range,
    stringsAsFactors = FALSE
  )
  
  # Añadir columnas para métricas
  for (metric in output_metrics) {
    results[[metric]] <- NA
  }
  
  # Valor base del parámetro
  baseline_value <- get_parameter_value(bioenergetic_obj$species_parameters, parameter_name)
  
  # Ejecutar simulaciones
  for (i in seq_along(parameter_range)) {
    param_value <- parameter_range[i]
    
    # Crear objeto temporal con parámetro modificado
    temp_obj <- bioenergetic_obj
    temp_obj$species_parameters <- set_parameter_value(
      temp_obj$species_parameters, parameter_name, param_value
    )
    
    # Ejecutar simulación
    sim_result <- tryCatch({
      run_bioenergetic_simulation_core(
        initial_weight = temp_obj$simulation_settings$initial_weight,
        p_value = 0.5,
        species_params = temp_obj$species_parameters,
        environmental_data = temp_obj$environmental_data,
        return_daily = FALSE
      )
    }, error = function(e) NULL)
    
    # Guardar resultados
    if (!is.null(sim_result)) {
      if ("final_weight" %in% output_metrics) {
        results$final_weight[i] <- sim_result$final_weight
      }
      if ("total_consumption" %in% output_metrics) {
        results$total_consumption[i] <- sim_result$total_consumption
      }
    }
  }
  
  # Calcular sensibilidades relativas
  baseline_idx <- which.min(abs(parameter_range - baseline_value))
  if (length(baseline_idx) > 0) {
    for (metric in output_metrics) {
      if (metric %in% names(results)) {
        baseline_metric <- results[[metric]][baseline_idx]
        if (!is.na(baseline_metric) && baseline_metric != 0) {
          sensitivity_col <- paste0(metric, "_relative_change")
          results[[sensitivity_col]] <- ((results[[metric]] - baseline_metric) / baseline_metric) * 100
        }
      }
    }
  }
  
  return(results)
}



#' Verificar compatibilidad entre datasets
#' @keywords internal
check_data_compatibility <- function(temperature_data, diet_data = NULL, prey_energy_data = NULL) {
  
  compatibility <- list(
    compatible = TRUE,
    warnings = character(),
    errors = character()
  )
  
  if (!"Day" %in% names(temperature_data) || !"Temperature" %in% names(temperature_data)) {
    compatibility$errors <- c(compatibility$errors, "temperature_data debe tener columnas 'Day' y 'Temperature'")
    compatibility$compatible <- FALSE
  }
  
  if (!is.null(diet_data) && "Day" %in% names(diet_data)) {
    temp_days <- range(temperature_data$Day, na.rm = TRUE)
    diet_days <- range(diet_data$Day, na.rm = TRUE)
    
    if (diet_days[1] > temp_days[2] || diet_days[2] < temp_days[1]) {
      compatibility$warnings <- c(compatibility$warnings, "Rangos de días no se superponen entre temperatura y dieta")
    }
  }
  
  if (!is.null(diet_data) && !is.null(prey_energy_data)) {
    diet_prey <- setdiff(names(diet_data), "Day")
    energy_prey <- setdiff(names(prey_energy_data), "Day")
    
    if (!identical(sort(diet_prey), sort(energy_prey))) {
      compatibility$errors <- c(compatibility$errors, "Columnas de presas no coinciden entre diet_data y prey_energy_data")
      compatibility$compatible <- FALSE
    }
  }
  
  return(compatibility)
}

#' Obtener valor diario de datos temporales
#' @keywords internal
get_daily_value <- function(data_vector, day) {
  if (is.data.frame(data_vector)) {
    if ("Day" %in% names(data_vector)) {
      idx <- which(data_vector$Day == day)
      if (length(idx) > 0) {
        value_cols <- setdiff(names(data_vector), "Day")
        return(data_vector[idx[1], value_cols[1]])
      }
    }
    return(NA)
  } else if (is.vector(data_vector)) {
    if (day <= length(data_vector)) {
      return(data_vector[day])
    }
  }
  return(NA)
}