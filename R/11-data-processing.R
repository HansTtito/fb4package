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
# FUNCIONES PARA OBJETOS BIOENERGETIC
# ============================================================================

#' Process Bioenergetic object data for simulation
#'
#' @description
#' Converts a Bioenergetic object into the data structures required for FB4 simulation.
#' Performs interpolation, normalization, and formatting of all temporal data.
#'
#' @param bio_obj Bioenergetic object (must be pre-validated)
#' @param first_day First simulation day
#' @param last_day Last simulation day
#' @return List with processed data ready for simulation
#' @export
process_bioenergetic_data <- function(bio_obj, first_day, last_day) {
  
  # Basic input validation
  if (first_day >= last_day) {
    stop("first_day must be less than last_day")
  }
  
  target_days <- first_day:last_day
  n_days <- length(target_days)
  
  # Process temperature data
  temp_data <- tryCatch({
    interpolate_time_series(
      data = bio_obj$environmental_data$temperature,
      value_columns = "Temperature",
      target_days = target_days,
      method = "linear"
    )
  }, error = function(e) {
    stop("Failed to process temperature data: ", e$message)
  })
  
  # Get prey information
  prey_names <- bio_obj$diet_data$prey_names
  if (is.null(prey_names) || length(prey_names) == 0) {
    stop("No prey species found in diet data")
  }
  
  # Process diet proportions
  diet_props <- tryCatch({
    interpolate_time_series(
      data = bio_obj$diet_data$proportions,
      value_columns = prey_names,
      target_days = target_days,
      method = "linear"
    )
  }, error = function(e) {
    stop("Failed to process diet proportions: ", e$message)
  })
  
  # Process prey energies
  diet_energies <- tryCatch({
    interpolate_time_series(
      data = bio_obj$diet_data$energies,
      value_columns = prey_names,
      target_days = target_days,
      method = "linear"
    )
  }, error = function(e) {
    stop("Failed to process prey energies: ", e$message)
  })
  
  # Process indigestible fractions
  prey_indigestible <- tryCatch({
    interpolate_time_series(
      data = bio_obj$diet_data$indigestible,
      value_columns = prey_names,
      target_days = target_days,
      method = "linear"
    )
  }, error = function(e) {
    stop("Failed to process indigestible fractions: ", e$message)
  })
  
  # Convert to matrices for efficient computation
  diet_matrix <- as.matrix(diet_props[, prey_names, drop = FALSE])
  energy_matrix <- as.matrix(diet_energies[, prey_names, drop = FALSE])
  indigestible_matrix <- as.matrix(prey_indigestible[, prey_names, drop = FALSE])
  
  # Normalize diet proportions to sum to 1
  row_sums <- rowSums(diet_matrix, na.rm = TRUE)
  # Avoid division by zero
  row_sums[row_sums == 0] <- 1
  diet_matrix <- diet_matrix / row_sums
  
  # Validate normalized diet
  final_sums <- rowSums(diet_matrix)
  if (any(abs(final_sums - 1) > 0.01)) {
    warning("Some diet proportions could not be properly normalized")
  }
  
  # Process predator energy density using simplified functions
  predator_ed_vector <- tryCatch({
    
    # Use initial weight as reference for weight-dependent calculations
    reference_weight <- bio_obj$simulation_settings$initial_weight %||% 100
    
    # Calculate energy density for each day
    sapply(target_days, function(day) {
      calculate_predator_energy_density(
        weight = reference_weight,
        day = day,
        predator_params = bio_obj$species_params$predator
      )
    })
    
  }, error = function(e) {
    stop("Failed to process predator energy density: ", e$message)
  })
  
  # Final validation of processed data
  if (any(is.na(temp_data$Temperature))) {
    stop("Temperature data contains NA values after processing")
  }
  if (any(is.na(diet_matrix))) {
    stop("Diet proportions contain NA values after processing")
  }
  if (any(energy_matrix <= 0, na.rm = TRUE)) {
    stop("Prey energies contain non-positive values after processing")
  }
  if (any(is.na(predator_ed_vector))) {
    stop("Predator energy density contains NA values after processing")
  }
  
  # Return processed data structure
  return(list(
    temperature = temp_data$Temperature,
    diet_proportions = diet_matrix,
    prey_energies = energy_matrix,
    prey_indigestible = indigestible_matrix,
    predator_energy_density = predator_ed_vector,
    duration = n_days,
    prey_names = prey_names,
    first_day = first_day,
    last_day = last_day
  ))
}

# ============================================================================
# FB4 VALIDATION SYSTEM - COMPLETE AND ROBUST
# ============================================================================

# ============================================================================
# EQUATION REQUIREMENTS MAP
# ============================================================================

#' Equation requirements for all FB4 components
#' @keywords internal
EQUATION_REQUIREMENTS <- list(
  consumption = list(
    "1" = list(required = c("CEQ", "CA", "CB", "CQ")),
    "2" = list(required = c("CEQ", "CA", "CB", "CTM", "CTO", "CQ"), optional = c("CX")),
    "3" = list(required = c("CEQ", "CA", "CB", "CQ", "CTO", "CTL", "CTM", "CK1", "CK4"), optional = c("CG1", "CG2")),
    "4" = list(required = c("CEQ", "CA", "CB", "CQ", "CK1", "CK4"))
  ),
  
  respiration = list(
    "1" = list(
      respiration_required = c("REQ", "RA", "RB", "RQ", "RTO", "RTM", "RTL", "RK1", "RK4", "RK5"),
      activity_required = c("ACT", "BACT"),
      sda_required = c("SDA")
    ),
    "2" = list(
      respiration_required = c("REQ", "RA", "RB", "RQ", "RTO", "RTM"),
      activity_required = c("ACT"),
      sda_required = c("SDA"),
      optional = c("RX")
    )
  ),
  
  egestion = list(
    "1" = list(required = c("EGEQ", "FA")),
    "2" = list(required = c("EGEQ", "FA", "FB", "FG")),
    "3" = list(required = c("EGEQ", "FA", "FB", "FG")),
    "4" = list(required = c("EGEQ", "FA", "FB"))
  ),
  
  excretion = list(
    "1" = list(required = c("EXEQ", "UA")),
    "2" = list(required = c("EXEQ", "UA", "UB", "UG")),
    "3" = list(required = c("EXEQ", "UA", "UB", "UG")),  # Same as 2
    "4" = list(required = c("EXEQ", "UA", "UB"))
  ),
  
  predator = list(
    "1" = list(
      required = c("PREDEDEQ"),
      data_options = "energy_data OR (ED_ini AND ED_end)"
    ),
    "2" = list(
      required = c("PREDEDEQ", "Alpha1", "Beta1", "Cutoff"),
      optional = c("Alpha2", "Beta2")
    ),
    "3" = list(
      required = c("PREDEDEQ", "Alpha1", "Beta1")
    )
  )
)

# ============================================================================
# CORE VALIDATION FUNCTIONS
# ============================================================================

#' Validate equation parameters for a specific category
#'
#' @param category_name Name of the category (consumption, respiration, etc.)
#' @param category_params Parameters for the category
#' @param species_params Full species parameters (for cross-category validation)
#' @return List with validation results
#' @keywords internal
validate_category_equation <- function(category_name, category_params, species_params = NULL) {
  
  validation <- list(
    valid = TRUE,
    errors = character(),
    warnings = character(),
    category = category_name
  )
  
  # Find equation parameter (CEQ, REQ, etc.)
  eq_param_name <- switch(category_name,
                          "consumption" = "CEQ",
                          "respiration" = "REQ",
                          "egestion" = "EGEQ", 
                          "excretion" = "EXEQ",
                          "predator" = "PREDEDEQ",
                          stop("Unknown category: ", category_name)
  )
  
  # Get equation number
  equation_num <- category_params[[eq_param_name]]
  if (is.null(equation_num) || is.na(equation_num)) {
    validation$errors <- c(validation$errors, 
                           paste("Missing equation parameter:", eq_param_name))
    validation$valid <- FALSE
    return(validation)
  }
  
  # Convert to character for lookup
  eq_key <- as.character(equation_num)
  
  # Get requirements for this equation
  category_reqs <- EQUATION_REQUIREMENTS[[category_name]]
  if (is.null(category_reqs) || !eq_key %in% names(category_reqs)) {
    validation$errors <- c(validation$errors, 
                           paste("Invalid equation number for", category_name, ":", equation_num))
    validation$valid <- FALSE
    return(validation)
  }
  
  eq_reqs <- category_reqs[[eq_key]]
  
  # Special handling for respiration (requires multiple categories)
  if (category_name == "respiration") {
    validation <- validate_respiration_requirements(eq_reqs, category_params, species_params, validation)
  } else if (category_name == "predator") {
    validation <- validate_predator_requirements(eq_reqs, category_params, validation)
  } else {
    # Standard validation for other categories
    validation <- validate_standard_requirements(eq_reqs, category_params, validation)
  }
  
  return(validation)
}

#' Validate standard parameter requirements
#' @keywords internal
validate_standard_requirements <- function(eq_reqs, category_params, validation) {
  
  # Check required parameters
  if ("required" %in% names(eq_reqs)) {
    missing_required <- setdiff(eq_reqs$required, names(category_params))
    if (length(missing_required) > 0) {
      validation$errors <- c(validation$errors, 
                             paste("Missing required parameters:", paste(missing_required, collapse = ", ")))
      validation$valid <- FALSE
    }
    
    # Check for NA values in required parameters
    for (param in eq_reqs$required) {
      if (param %in% names(category_params)) {
        value <- category_params[[param]]
        if (is.null(value) || (is.numeric(value) && is.na(value))) {
          validation$errors <- c(validation$errors, 
                                 paste("Required parameter", param, "is NULL or NA"))
          validation$valid <- FALSE
        }
      }
    }
  }
  
  # Check optional parameters (warnings only)
  if ("optional" %in% names(eq_reqs)) {
    missing_optional <- setdiff(eq_reqs$optional, names(category_params))
    if (length(missing_optional) > 0) {
      validation$warnings <- c(validation$warnings, 
                               paste("Missing optional parameters (will be calculated):", 
                                     paste(missing_optional, collapse = ", ")))
    }
  }
  
  return(validation)
}

#' Validate respiration requirements (multiple categories)
#' @keywords internal
validate_respiration_requirements <- function(eq_reqs, respiration_params, species_params, validation) {
  
  # Validate respiration parameters
  if ("respiration_required" %in% names(eq_reqs)) {
    missing_resp <- setdiff(eq_reqs$respiration_required, names(respiration_params))
    if (length(missing_resp) > 0) {
      validation$errors <- c(validation$errors, 
                             paste("Missing respiration parameters:", paste(missing_resp, collapse = ", ")))
      validation$valid <- FALSE
    }
  }
  
  # Validate activity parameters
  if ("activity_required" %in% names(eq_reqs) && !is.null(species_params)) {
    activity_params <- species_params$activity
    if (is.null(activity_params)) {
      validation$errors <- c(validation$errors, "Missing activity category for respiration equation")
      validation$valid <- FALSE
    } else {
      missing_act <- setdiff(eq_reqs$activity_required, names(activity_params))
      if (length(missing_act) > 0) {
        validation$errors <- c(validation$errors, 
                               paste("Missing activity parameters:", paste(missing_act, collapse = ", ")))
        validation$valid <- FALSE
      }
    }
  }
  
  # Validate SDA parameters
  if ("sda_required" %in% names(eq_reqs) && !is.null(species_params)) {
    sda_params <- species_params$sda
    if (is.null(sda_params)) {
      validation$errors <- c(validation$errors, "Missing sda category for respiration equation")
      validation$valid <- FALSE
    } else {
      missing_sda <- setdiff(eq_reqs$sda_required, names(sda_params))
      if (length(missing_sda) > 0) {
        validation$errors <- c(validation$errors, 
                               paste("Missing sda parameters:", paste(missing_sda, collapse = ", ")))
        validation$valid <- FALSE
      }
    }
  }
  
  return(validation)
}

#' Validate predator requirements (special data handling)
#' @keywords internal
validate_predator_requirements <- function(eq_reqs, predator_params, validation) {
  
  # Check standard required parameters
  if ("required" %in% names(eq_reqs)) {
    missing_required <- setdiff(eq_reqs$required, names(predator_params))
    if (length(missing_required) > 0) {
      validation$errors <- c(validation$errors, 
                             paste("Missing predator parameters:", paste(missing_required, collapse = ", ")))
      validation$valid <- FALSE
    }
  }
  
  # Special validation for PREDEDEQ = 1 (data requirements)
  if (!is.null(predator_params$PREDEDEQ) && predator_params$PREDEDEQ == 1) {
    has_ed_data <- !is.null(predator_params$ED_data) && !is.na(predator_params$ED_data)
    has_ed_ini_end <- !is.null(predator_params$ED_ini) && !is.null(predator_params$ED_end) &&
      !is.na(predator_params$ED_ini) && !is.na(predator_params$ED_end)
    
    if (!has_ed_data && !has_ed_ini_end) {
      validation$errors <- c(validation$errors, 
                             "PREDEDEQ=1 requires either ED_data OR both ED_ini and ED_end")
      validation$valid <- FALSE
    }
  }
  
  return(validation)
}

#' Main function to validate all species equations
#'
#' @param species_params List with all species parameters
#' @return List with comprehensive validation results
#' @export
validate_species_equations <- function(species_params) {
  
  overall_validation <- list(
    valid = TRUE,
    errors = character(),
    warnings = character(),
    category_results = list()
  )
  
  # Categories to validate
  categories_to_validate <- c("consumption", "respiration", "egestion", "excretion", "predator")
  
  for (category in categories_to_validate) {
    if (category %in% names(species_params)) {
      category_params <- species_params[[category]]
      
      # Validate this category
      cat_validation <- validate_category_equation(category, category_params, species_params)
      overall_validation$category_results[[category]] <- cat_validation
      
      # Accumulate errors and warnings
      if (!cat_validation$valid) {
        overall_validation$valid <- FALSE
        overall_validation$errors <- c(overall_validation$errors, cat_validation$errors)
      }
      overall_validation$warnings <- c(overall_validation$warnings, cat_validation$warnings)
      
    } else {
      overall_validation$errors <- c(overall_validation$errors, 
                                     paste("Missing category:", category))
      overall_validation$valid <- FALSE
    }
  }
  
  return(overall_validation)
}

# ============================================================================
# BIOENERGETIC OBJECT VALIDATION
# ============================================================================

#' Comprehensive validation for Bioenergetic objects
#'
#' @param bio_obj Bioenergetic object
#' @return List with validation results
#' @export
validate_bioenergetic_for_simulation <- function(bio_obj) {
  
  validation <- list(
    valid = TRUE,
    errors = character(),
    warnings = character(),
    ready_to_run = FALSE
  )
  
  # 1. Basic structure validation
  required_components <- c("species_params", "environmental_data", "diet_data", "simulation_settings")
  missing_components <- setdiff(required_components, names(bio_obj))
  
  if (length(missing_components) > 0) {
    validation$errors <- c(validation$errors, 
                           paste("Missing components:", paste(missing_components, collapse = ", ")))
    validation$valid <- FALSE
    return(validation)
  }
  
  # 2. Validate species equations
  equation_validation <- validate_species_equations(bio_obj$species_params)
  if (!equation_validation$valid) {
    validation$valid <- FALSE
    validation$errors <- c(validation$errors, equation_validation$errors)
  }
  validation$warnings <- c(validation$warnings, equation_validation$warnings)
  
  # 3. Validate environmental data
  if (is.null(bio_obj$environmental_data$temperature)) {
    validation$errors <- c(validation$errors, "Missing temperature data")
    validation$valid <- FALSE
  } else {
    temp_validation <- validate_time_series_data(
      bio_obj$environmental_data$temperature, 
      "temperature", 
      c("Day", "Temperature")
    )
  }
  
  # 4. Validate diet data
  if (is.null(bio_obj$diet_data$proportions) || is.null(bio_obj$diet_data$energies)) {
    validation$errors <- c(validation$errors, "Missing diet data (proportions or energies)")
    validation$valid <- FALSE
  } else {
    tryCatch({
      validate_diet_consistency(bio_obj$diet_data$proportions, bio_obj$diet_data$energies)
    }, error = function(e) {
      validation$errors <- c(validation$errors, paste("Diet data error:", e$message))
      validation$valid <<- FALSE
    })
  }
  
  # 5. Validate simulation settings
  if (is.null(bio_obj$simulation_settings$initial_weight) || 
      bio_obj$simulation_settings$initial_weight <= 0) {
    validation$errors <- c(validation$errors, "Invalid or missing initial_weight")
    validation$valid <- FALSE
  }
  
  # 6. Check if ready to run
  if (validation$valid) {
    validation$ready_to_run <- TRUE
    validation$warnings <- c(validation$warnings, "Object is ready for simulation")
  }
  
  return(validation)
}

# ============================================================================
# SIMPLIFIED INPUT VALIDATION
# ============================================================================

#' Validate inputs for FB4 simulation
#'
#' @param bio_obj Bioenergetic object
#' @param fit_to Fitting target
#' @param fit_value Fitting value
#' @param first_day First simulation day
#' @param last_day Last simulation day
#' @return NULL (throws error if invalid)
#' @export
validate_fb4_inputs <- function(bio_obj, fit_to = NULL, fit_value = NULL, 
                                first_day = 1, last_day = NULL) {
  
  # Basic parameter validation
  if (first_day < 1) {
    stop("first_day must be >= 1")
  }
  
  if (!is.null(last_day) && first_day >= last_day) {
    stop("first_day must be less than last_day")
  }
  
  if (!is.null(fit_to)) {
    valid_fit_options <- c("Weight", "Consumption", "Ration", "Ration_prey", "p-value")
    if (!fit_to %in% valid_fit_options) {
      stop("fit_to must be one of: ", paste(valid_fit_options, collapse = ", "))
    }
    
    if (is.null(fit_value) || !is.numeric(fit_value) || fit_value <= 0) {
      stop("fit_value must be a positive number when fit_to is specified")
    }
  }
  
  # Comprehensive object validation
  obj_validation <- validate_bioenergetic_for_simulation(bio_obj)
  
  if (!obj_validation$valid) {
    stop("Bioenergetic object validation failed:\n", 
         paste(obj_validation$errors, collapse = "\n"))
  }
  
  # Data range validation
  if (!is.null(bio_obj$environmental_data$temperature)) {
    temp_days <- range(bio_obj$environmental_data$temperature$Day)
    if (is.null(last_day)) {
      last_day <- temp_days[2]
    }
    
    if (first_day < temp_days[1] || last_day > temp_days[2]) {
      stop("Requested day range (", first_day, "-", last_day, 
           ") exceeds temperature data range (", temp_days[1], "-", temp_days[2], ")")
    }
  }
  
  if (!is.null(bio_obj$diet_data$proportions)) {
    diet_days <- range(bio_obj$diet_data$proportions$Day)
    if (first_day < diet_days[1] || last_day > diet_days[2]) {
      stop("Requested day range (", first_day, "-", last_day, 
           ") exceeds diet data range (", diet_days[1], "-", diet_days[2], ")")
    }
  }
  
  # Print validation summary if warnings exist
  if (length(obj_validation$warnings) > 0) {
    message("Validation warnings:\n", paste(obj_validation$warnings, collapse = "\n"))
  }
  
  return(invisible(TRUE))
}

# ============================================================================
# DATA PROCESSING FUNCTIONS
# ============================================================================

#' Process Bioenergetic object data for simulation
#'
#' @description
#' Converts a Bioenergetic object into the data structures required for FB4 simulation.
#' Performs interpolation, normalization, and formatting of all temporal data.
#'
#' @param bio_obj Bioenergetic object (must be pre-validated)
#' @param first_day First simulation day
#' @param last_day Last simulation day
#' @return List with processed data ready for simulation
#' @export
process_bioenergetic_data <- function(bio_obj, first_day, last_day) {
  
  # Basic input validation
  if (first_day >= last_day) {
    stop("first_day must be less than last_day")
  }
  
  target_days <- first_day:last_day
  n_days <- length(target_days)
  
  # Process temperature data
  temp_data <- tryCatch({
    interpolate_time_series(
      data = bio_obj$environmental_data$temperature,
      value_columns = "Temperature",
      target_days = target_days,
      method = "linear"
    )
  }, error = function(e) {
    stop("Failed to process temperature data: ", e$message)
  })
  
  # Get prey information
  prey_names <- bio_obj$diet_data$prey_names
  if (is.null(prey_names) || length(prey_names) == 0) {
    stop("No prey species found in diet data")
  }
  
  # Process diet proportions
  diet_props <- tryCatch({
    interpolate_time_series(
      data = bio_obj$diet_data$proportions,
      value_columns = prey_names,
      target_days = target_days,
      method = "linear"
    )
  }, error = function(e) {
    stop("Failed to process diet proportions: ", e$message)
  })
  
  # Process prey energies
  diet_energies <- tryCatch({
    interpolate_time_series(
      data = bio_obj$diet_data$energies,
      value_columns = prey_names,
      target_days = target_days,
      method = "linear"
    )
  }, error = function(e) {
    stop("Failed to process prey energies: ", e$message)
  })
  
  # Process indigestible fractions
  prey_indigestible <- tryCatch({
    interpolate_time_series(
      data = bio_obj$diet_data$indigestible,
      value_columns = prey_names,
      target_days = target_days,
      method = "linear"
    )
  }, error = function(e) {
    stop("Failed to process indigestible fractions: ", e$message)
  })
  
  # Convert to matrices for efficient computation
  diet_matrix <- as.matrix(diet_props[, prey_names, drop = FALSE])
  energy_matrix <- as.matrix(diet_energies[, prey_names, drop = FALSE])
  indigestible_matrix <- as.matrix(prey_indigestible[, prey_names, drop = FALSE])
  
  # Normalize diet proportions to sum to 1
  row_sums <- rowSums(diet_matrix, na.rm = TRUE)
  # Avoid division by zero
  row_sums[row_sums == 0] <- 1
  diet_matrix <- diet_matrix / row_sums
  
  # Validate normalized diet
  final_sums <- rowSums(diet_matrix)
  if (any(abs(final_sums - 1) > 0.01)) {
    warning("Some diet proportions could not be properly normalized")
  }
  
  # Process predator energy density using simplified functions
  predator_ed_vector <- tryCatch({
    
    # Use initial weight as reference for weight-dependent calculations
    reference_weight <- bio_obj$simulation_settings$initial_weight %||% 100
    
    # Calculate energy density for each day
    sapply(target_days, function(day) {
      calculate_predator_energy_density(
        weight = reference_weight,
        day = day,
        predator_params = bio_obj$species_params$predator
      )
    })
    
  }, error = function(e) {
    stop("Failed to process predator energy density: ", e$message)
  })
  
  # Process reproduction data (if exists)
  reproduction_data <- NULL
  if (!is.null(bio_obj$reproduction_data)) {
    reproduction_data <- tryCatch({
      interpolated_repro <- interpolate_time_series(
        data = bio_obj$reproduction_data,
        value_columns = "proportion",
        target_days = target_days,
        method = "constant"  # Use constant for discrete spawning events
      )
      interpolated_repro$proportion
    }, error = function(e) {
      warning("Failed to process reproduction data: ", e$message, ". Using no reproduction.")
      rep(0, n_days)
    })
  } else {
    # Create vector of zeros if no reproduction data
    reproduction_data <- rep(0, n_days)
  }
  
  # Final validation of processed data
  if (any(is.na(temp_data$Temperature))) {
    stop("Temperature data contains NA values after processing")
  }
  if (any(is.na(diet_matrix))) {
    stop("Diet proportions contain NA values after processing")
  }
  if (any(energy_matrix <= 0, na.rm = TRUE)) {
    stop("Prey energies contain non-positive values after processing")
  }
  if (any(is.na(predator_ed_vector))) {
    stop("Predator energy density contains NA values after processing")
  }
  if (any(reproduction_data < 0, na.rm = TRUE) || any(reproduction_data > 1, na.rm = TRUE)) {
    stop("Reproduction proportions must be between 0 and 1")
  }
  
  # Return processed data structure
  return(list(
    temperature = temp_data$Temperature,
    diet_proportions = diet_matrix,
    prey_energies = energy_matrix,
    prey_indigestible = indigestible_matrix,
    predator_energy_density = predator_ed_vector,  # Vector, not data.frame
    reproduction = reproduction_data,  # Vector of reproduction proportions
    duration = n_days,
    prey_names = prey_names,
    first_day = first_day,
    last_day = last_day
  ))
}