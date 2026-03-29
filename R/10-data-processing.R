#' Data Processing Functions for FB4
#'
#' @name data-processing
#' @aliases data-processing
NULL

# ============================================================================
# MAIN DATA PROCESSING FUNCTIONS
# ============================================================================

#' Prepare all simulation data
#'
#' @description
#' Master function that processes and validates ALL data required for FB4 simulation.
#' Combines species parameter processing with temporal data processing.
#'
#' @param bio_obj Bioenergetic object (must be pre-validated)
#' @param strategy Strategy to use: "binary_search", "optim", "bootstrap", "mle", "hierarchical"
#' @param fit_to Target type for fitting (e.g., "Weight"); optional for direct strategy
#' @param fit_value Target value to fit to; optional for direct strategy
#' @param first_day First simulation day
#' @param last_day Last simulation day
#' @param validate_inputs Whether to perform comprehensive validation, default TRUE
#' @param oxycal Oxycalorific coefficient (J/g O2), default 13560
#' @param output_format Output format: "simulation", "tmb_basic", "tmb_hierarchical"
#' @param observed_weights Data frame with columns: individual_id, initial_weight and observed_weight
#' @param covariates Optional covariate matrix or data frame or choose a column of individual_data
#' @return List with complete processed data ready for simulation
#' @export
prepare_simulation_data <- function(bio_obj, strategy, fit_to = NULL, fit_value = NULL, first_day = 1, last_day = NULL,
                                   validate_inputs = TRUE, oxycal = 13560,
                                   output_format = "simulation", observed_weights = NULL, covariates = NULL) {

  # Infer last_day from bio object if not provided (same logic as run_fb4 orchestrator)
  if (is.null(last_day)) {
    if (!is.null(bio_obj$simulation_settings$duration)) {
      last_day <- bio_obj$simulation_settings$duration
    } else if (!is.null(bio_obj$environmental_data$temperature)) {
      last_day <- max(bio_obj$environmental_data$temperature$Day)
    } else {
      last_day <- 365
    }
  }

  # Comprehensive input validation
  if (validate_inputs) {
    validate_fb4_inputs(bio_obj, strategy, fit_to, fit_value, first_day, last_day, observed_weights, covariates)
  }

  # Process species parameters (from parameter-processing.R)
  message("Processing species parameters...")
  n_days_sim <- last_day - first_day + 1
  processed_species_params <- process_species_parameters(bio_obj$species_params, n_days = n_days_sim)

  # Process temporal data
  message("Processing temporal data...")
  processed_temporal_data <- process_bioenergetic_data(bio_obj, first_day, last_day)
  
  # Process simulation settings
  message("Processing simulation settings...")
  processed_settings <- process_simulation_settings(bio_obj$simulation_settings,
                                                    first_day, last_day, oxycal)
  
  # NUEVO: Handle observed_weights for different strategies
  if (!is.null(observed_weights) && strategy %in% c("bootstrap", "mle")) {
    # For bootstrap/MLE: observed_weights should be numeric vector
    if (is.data.frame(observed_weights)) {
      warning("observed_weights is data.frame but strategy needs numeric vector. Using first weight column.")
      weight_cols <- grep("weight", names(observed_weights), value = TRUE)
      if (length(weight_cols) > 0) {
        observed_weights <- observed_weights[[weight_cols[1]]]
      }
    }
    # Store in processed_settings for strategies to access
    processed_settings$observed_weights <- observed_weights
  }
  
  # Create complete simulation data structure
  simulation_data <- list(
    # Processed parameters (ready for calculations)
    species_params = processed_species_params,
    
    # Temporal data (vectors/matrices for each day)
    temporal_data = processed_temporal_data,
    
    # Simulation configuration
    simulation_settings = processed_settings,
    
    # Metadata
    metadata = list(
      processed_at = Sys.time(),
      duration = last_day - first_day + 1,
      first_day = first_day,
      last_day = last_day,
      prey_species = processed_temporal_data$prey_names,
      data_sources = extract_data_sources(bio_obj)
    )
  )
  
  # Final validation of complete simulation data
  if (validate_inputs) {
    validate_complete_simulation_data(simulation_data)
  }
  
  # Transform for TMB if requested
  if (output_format == "tmb_basic") {
    return(transform_to_tmb_basic(simulation_data, observed_weights))
  } else if (output_format == "tmb_hierarchical") {
    return(transform_to_tmb_hierarchical(simulation_data, observed_weights, covariates = covariates))
  }
  
  # Convenience top-level aliases expected by tests and direct callers
  simulation_data$n_days       <- simulation_data$temporal_data$duration
  simulation_data$temperatures <- simulation_data$temporal_data$temperature
  simulation_data$initial_weight <- simulation_data$simulation_settings$initial_weight

  message("Simulation data preparation complete. Ready for simulation.")
  return(simulation_data)
}


#' Process Bioenergetic object temporal data for simulation
#'
#' @description
#' Version that processes all temporal data required for FB4 simulation.
#' Includes better error handling and additional data types.
#'
#' @param bio_obj Bioenergetic object (must be pre-validated)
#' @param first_day First simulation day
#' @param last_day Last simulation day
#' @return List with processed temporal data ready for simulation
#' @export
process_bioenergetic_data <- function(bio_obj, first_day, last_day) {

  # Resolve last_day if not provided
  if (is.null(last_day)) {
    if (!is.null(bio_obj$simulation_settings$duration)) {
      last_day <- bio_obj$simulation_settings$duration
    } else if (!is.null(bio_obj$environmental_data$temperature)) {
      last_day <- max(bio_obj$environmental_data$temperature$Day)
    } else {
      last_day <- 365
    }
  }

  # Basic input validation
  if (first_day >= last_day) {
    stop("first_day must be less than last_day")
  }
  
  target_days <- first_day:last_day
  n_days <- length(target_days)
  
  # Process temperature data (REQUIRED)
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
  
  # Process diet proportions (REQUIRED)
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
  
  # Process prey energies (REQUIRED)
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
  
  # Process indigestible fractions (OPTIONAL)
  prey_indigestible <- tryCatch({
    if (!is.null(bio_obj$diet_data$indigestible)) {
      interpolate_time_series(
        data = bio_obj$diet_data$indigestible,
        value_columns = prey_names,
        target_days = target_days,
        method = "linear"
      )
    } else {
      # Create default indigestible fractions (0% for all prey, matching FB4 default)
      default_indigestible <- data.frame(Day = target_days)
      for (prey in prey_names) {
        default_indigestible[[prey]] <- 0.0
      }
      warning("No indigestible fraction data provided, using default 0% for all prey (FB4 default)")
      default_indigestible
    }
  }, error = function(e) {
    stop("Failed to process indigestible fractions: ", e$message)
  })
  
  # Convert diet data to matrices for efficient computation
  diet_matrix <- as.matrix(diet_props[, prey_names, drop = FALSE])
  energy_matrix <- as.matrix(diet_energies[, prey_names, drop = FALSE])
  indigestible_matrix <- as.matrix(prey_indigestible[, prey_names, drop = FALSE])
  
  # Normalize diet proportions to sum to 1
  diet_matrix <- normalize_diet_proportions(diet_matrix)
  
  # Process reproduction data (OPTIONAL)
  reproduction_data <- process_reproduction_data(bio_obj, target_days)
  
  # Final validation of processed temporal data
  validate_temporal_data(temp_data$Temperature, diet_matrix, energy_matrix, 
                         indigestible_matrix, reproduction_data)
  
  # Return comprehensive temporal data structure
  return(list(
    # Core required data
    temperature = temp_data$Temperature,
    diet_proportions = diet_matrix,
    prey_energies = energy_matrix,
    prey_indigestible = indigestible_matrix,
    
    # Optional temporal data
    reproduction = reproduction_data,
    # environmental = environmental_data,
    # contaminant = contaminant_data,
    # nutrient = nutrient_data,
    
    # Metadata
    duration = n_days,
    prey_names = prey_names,
    first_day = first_day,
    last_day = last_day,
    target_days = target_days
  ))
}

# ============================================================================
# SIMULATION SETTINGS PROCESSING
# ============================================================================

#' Process simulation settings
#'
#' @param settings Raw simulation settings
#' @param first_day First simulation day
#' @param last_day Last simulation day
#' @param oxycal Oxycalorific coefficient (J/g O2), default 13560
#' @return Processed simulation settings
#' @export
process_simulation_settings <- function(settings, first_day, last_day, oxycal = 13560) {
  
  processed <- list(
    # Basic simulation parameters
    initial_weight = check_numeric_value(settings$initial_weight, "initial_weight", min_val = 0.01),
    duration = last_day - first_day + 1,
    first_day = first_day,
    last_day = last_day,
    oxycal = oxycal,
    
    # Simulation options
    output_frequency = settings$output_frequency %||% 1,  # Every n days
    save_daily_details = settings$save_daily_details %||% TRUE,
    
    # Numerical options
    tolerance = settings$tolerance %||% 1e-6,
    max_iterations = settings$max_iterations %||% 1000,
    step_size = settings$step_size %||% 1.0,
    
    # Output options
    calculate_composition = settings$calculate_composition %||% FALSE,
    calculate_contaminants = settings$calculate_contaminants %||% FALSE,
    calculate_nutrients = settings$calculate_nutrients %||% FALSE,
    track_mortality = settings$track_mortality %||% FALSE
  )

  return(processed)
}

# ============================================================================
# OPTIONAL DATA PROCESSING FUNCTIONS
# ============================================================================

#' Process reproduction data
#'
#' @param bio_obj Bioenergetic object
#' @param target_days Target simulation days
#' @return Vector of reproduction fractions for each day
#' @keywords internal
process_reproduction_data <- function(bio_obj, target_days) {
  
  if (!is.null(bio_obj$reproduction_data)) {
    reproduction_data <- tryCatch({
      interpolated_repro <- interpolate_time_series(
        data = bio_obj$reproduction_data,
        value_columns = "proportion",
        target_days = target_days,
        method = "constant"  # Use constant for discrete spawning events
      )
      clamp(interpolated_repro$proportion, 0, 1, param_name = "reproduction_proportions")
    }, error = function(e) {
      warning("Failed to process reproduction data: ", e$message, ". Using no reproduction.")
      rep(0, length(target_days))
    })
  } else {
    # No reproduction data
    reproduction_data <- rep(0, length(target_days))
  }
  
  return(reproduction_data)
}

# ============================================================================
# UTILITY AND VALIDATION FUNCTIONS
# ============================================================================

#' Normalize diet proportions to sum to 1
#'
#' @param diet_matrix Matrix with diet proportions
#' @return Normalized diet matrix
#' @keywords internal
normalize_diet_proportions <- function(diet_matrix) {
  
  row_sums <- rowSums(diet_matrix, na.rm = TRUE)
  
  # Handle zero sums (no food)
  zero_rows <- which(row_sums == 0)
  if (length(zero_rows) > 0) {
    warning("Found ", length(zero_rows), " days with zero diet proportions. ",
            "Setting equal proportions for all prey.")
    diet_matrix[zero_rows, ] <- 1 / ncol(diet_matrix)
    row_sums[zero_rows] <- 1
  }
  
  # Normalize non-zero rows
  diet_matrix <- diet_matrix / row_sums
  
  # Final validation
  final_sums <- rowSums(diet_matrix)
  if (any(abs(final_sums - 1) > 0.01)) {
    warning("Some diet proportions could not be properly normalized")
  }
  
  return(diet_matrix)
}

#' Interpolate time series with error handling
#'
#' @param data Data frame with Day column and value columns
#' @param value_columns Vector with names of columns to interpolate
#' @param target_days Vector of target days
#' @param method Interpolation method ("linear", "constant", "spline")
#' @param fill_na_method Method to fill missing values ("extend", "zero", "mean")
#' @param validate_input Validate input structure, default TRUE
#' @return Data frame with interpolated data
#' @importFrom stats approx spline
#' @importFrom utils tail
#' @export
interpolate_time_series <- function(data, value_columns, target_days, 
                                    method = "linear", fill_na_method = "extend",
                                    validate_input = TRUE) {
  
  # Validations
  if (validate_input) {
    if (!"Day" %in% names(data)) {
      stop("Data frame must have 'Day' column")
    }
    
    if (nrow(data) == 0) {
      stop("Data frame cannot be empty")
    }
    
    missing_cols <- setdiff(value_columns, names(data))
    if (length(missing_cols) > 0) {
      stop("Missing columns: ", paste(missing_cols, collapse = ", "))
    }
    
    # Validate that target_days is numeric and sorted
    if (!is.numeric(target_days)) {
      stop("target_days must be numeric")
    }
    target_days <- sort(unique(target_days))
  }
  
  # Check data coverage
  data_range <- range(data$Day, na.rm = TRUE)
  target_range <- range(target_days)
  
  if (target_range[1] < data_range[1] || target_range[2] > data_range[2]) {
    warning("Target days (", target_range[1], "-", target_range[2], 
            ") extend beyond data range (", data_range[1], "-", data_range[2], 
            "). Extrapolation will be used.")
  }
  
  # Prepare result
  n_days <- length(target_days)
  result <- data.frame(Day = target_days)
  
  # Internal function to handle interpolation
  interpolate_column <- function(values, days, target_days, method) {
    # Remove NA values
    valid_idx <- !is.na(values) & !is.na(days)
    
    if (sum(valid_idx) < 1) {
      return(rep(NA_real_, length(target_days)))
    }
    
    if (sum(valid_idx) == 1) {
      return(rep(values[valid_idx][1], length(target_days)))
    }
    
    clean_days <- days[valid_idx]
    clean_values <- values[valid_idx]
    
    # Sort by days
    order_idx <- order(clean_days)
    clean_days <- clean_days[order_idx]
    clean_values <- clean_values[order_idx]
    
    # Perform interpolation according to method
    if (method == "linear") {
      interpolated <- stats::approx(x = clean_days, y = clean_values,
                                    xout = target_days, method = "linear", rule = 2)$y
    } else if (method == "constant") {
      interpolated <- stats::approx(x = clean_days, y = clean_values,
                                    xout = target_days, method = "constant", rule = 2)$y
    } else if (method == "spline") {
      if (length(clean_days) < 4) {
        interpolated <- stats::approx(x = clean_days, y = clean_values,
                                      xout = target_days, method = "linear", rule = 2)$y
      } else {
        tryCatch({
          interpolated <- stats::spline(x = clean_days, y = clean_values,
                                        xout = target_days, method = "natural")$y
        }, error = function(e) {
          warning("Spline interpolation failed, using linear method")
          interpolated <- stats::approx(x = clean_days, y = clean_values,
                                        xout = target_days, method = "linear", rule = 2)$y
        })
      }
    } else {
      stop("Invalid interpolation method: ", method)
    }
    
    return(interpolated)
  }
  
  # Interpolate each column
  for (col in value_columns) {
    interpolated <- interpolate_column(data[[col]], data$Day, target_days, method)
    
    # Handle resulting NA values
    if (any(is.na(interpolated))) {
      if (fill_na_method == "extend") {
        first_valid <- which(!is.na(interpolated))[1]
        last_valid <- utils::tail(which(!is.na(interpolated)), 1)
        
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
        if (!is.na(mean_val)) {
          interpolated[is.na(interpolated)] <- mean_val
        }
      }
    }
    
    result[[col]] <- interpolated
  }
  
  return(result)
}


#' Extract data sources from bioenergetic object
#'
#' @param bio_obj Bioenergetic object
#' @return List with data source information
#' @keywords internal
extract_data_sources <- function(bio_obj) {
  
  sources <- list()
  
  # Check what data sources were provided
  if (!is.null(bio_obj$environmental_data$temperature)) {
    sources$temperature <- "user_provided"
  }
  
  if (!is.null(bio_obj$diet_data$proportions)) {
    sources$diet_proportions <- "user_provided"
  }
  
  if (!is.null(bio_obj$diet_data$energies)) {
    sources$prey_energies <- "user_provided"
  }
  
  if (!is.null(bio_obj$diet_data$indigestible)) {
    sources$indigestible_fractions <- "user_provided"
  } else {
    sources$indigestible_fractions <- "default_0percent"
  }
  
  if (!is.null(bio_obj$reproduction_data)) {
    sources$reproduction <- "user_provided"
  } else {
    sources$reproduction <- "none"
  }

  return(sources)
}


#' Convert hierarchical observed_weights to individual_data format
#' @param observed_weights Hierarchical data (data.frame or named list)
#' @param verbose Show conversion details
#' @return data.frame in format expected by TMB hierarchical
#' @keywords internal
convert_to_individual_data <- function(observed_weights, verbose = FALSE) {
  
  if (verbose) {
    message("Converting hierarchical data to individual_data format")
  }
  
  if (is.data.frame(observed_weights)) {
    # Already in correct format, just validate
    validate_individual_data(observed_weights)
    
    if (verbose) {
      message("Data.frame format detected: ", nrow(observed_weights), " individuals")
    }
    
    return(observed_weights)
    
  } else if (is.list(observed_weights) && !is.null(names(observed_weights))) {
    # Convert named list to data.frame
    if (verbose) {
      message("Named list format detected, converting to data.frame")
    }
    
    # Extract individuals
    individual_ids <- names(observed_weights)
    n_individuals <- length(individual_ids)
    
    # Create base data.frame
    individual_data <- data.frame(
      individual_id = individual_ids,
      stringsAsFactors = FALSE
    )
    
    # Extract data for each individual
    for (i in 1:n_individuals) {
      ind_data <- observed_weights[[i]]
      
      if (is.list(ind_data)) {
        # Extract initial_weight
        individual_data$initial_weight[i] <- ind_data$initial_weight %||% 
          stop("Missing initial_weight for individual: ", individual_ids[i])
        
        # Extract final_weight (support multiple names for backward compatibility)
        final_weight <- ind_data$final_weight %||% 
                       ind_data$observed_weight %||% 
                       ind_data$observed_weights
        
        if (is.null(final_weight)) {
          stop("Missing final_weight for individual: ", individual_ids[i])
        }
        
        # If multiple final weights, take the last one or mean
        if (length(final_weight) > 1) {
          if (verbose) {
            message("Individual ", individual_ids[i], " has multiple final weights, using the last one")
          }
          final_weight <- final_weight[length(final_weight)]
        }
        
        individual_data$final_weight[i] <- final_weight
        
      } else {
        stop("Invalid format for individual: ", individual_ids[i])
      }
    }
    
    # Validate the created data.frame
    validate_individual_data(individual_data)
    
    if (verbose) {
      message("Converted ", n_individuals, " individuals to data.frame format")
    }
    
    return(individual_data)
    
  } else {
    stop("Hierarchical observed_weights must be data.frame or named list")
  }
}


#' Transform simulation data to TMB basic format
#' @keywords internal
transform_to_tmb_basic <- function(simulation_data, observed_weights) {
  
  # Extract temporal data
  temporal_data <- simulation_data$temporal_data
  n_days <- simulation_data$metadata$duration
  
  # Extract species parameters (already processed)
  sp <- simulation_data$species_params
  
  # Build TMB data structure (copying from original extract_tmb_data_basic)
  tmb_data <- list(
    # Model type identifier
    model_type = "basic",
    
    # Simulation settings
    n_days = as.integer(n_days),
    initial_weight = as.numeric(simulation_data$simulation_settings$initial_weight),
    observed_weights = as.numeric(observed_weights),
    oxycal = as.numeric(simulation_data$simulation_settings$oxycal),
    
    # Temporal data
    temperature = as.numeric(temporal_data$temperature),
    diet_proportions = as.matrix(temporal_data$diet_proportions),
    prey_energies = as.matrix(temporal_data$prey_energies),
    prey_indigestible = as.matrix(temporal_data$prey_indigestible),
    reproduction_data = as.numeric(temporal_data$reproduction %||% rep(0, n_days)),
    
    # Species parameters - Consumption
    CEQ = as.integer(sp$consumption$CEQ),
    CA = sp$consumption$CA, CB = sp$consumption$CB, CQ = sp$consumption$CQ,
    CTM = sp$consumption$CTM, CTO = sp$consumption$CTO, CTL = sp$consumption$CTL %||% 0.0,
    CK1 = sp$consumption$CK1 %||% 0.0, CK4 = sp$consumption$CK4 %||% 0.0,
    CG1 = sp$consumption$CG1 %||% 1.0, CG2 = sp$consumption$CG2 %||% 1.0, CX = sp$consumption$CX %||% 1.0,
    
    # Species parameters - Respiration  
    REQ = as.integer(sp$respiration$REQ),
    RA = sp$respiration$RA, RB = sp$respiration$RB, RQ = sp$respiration$RQ,
    RTO = sp$respiration$RTO, RTM = sp$respiration$RTM, RTL = sp$respiration$RTL %||% 5.0,
    RK1 = sp$respiration$RK1 %||% 0.1, RK4 = sp$respiration$RK4 %||% 0.1, RK5 = sp$respiration$RK5 %||% 0.1, RX = sp$respiration$RX %||% 0.0,
    ACT = sp$respiration$ACT %||% 1.0, BACT = sp$respiration$BACT %||% 0.0, SDA = sp$respiration$SDA %||% 0.15,
    
    # Species parameters - Egestion
    EGEQ = as.integer(sp$egestion$EGEQ),
    FA = sp$egestion$FA, FB = sp$egestion$FB %||% 0.0, FG = sp$egestion$FG %||% 1.0,
    
    # Species parameters - Excretion
    EXEQ = as.integer(sp$excretion$EXEQ),
    UA = sp$excretion$UA, UB = sp$excretion$UB %||% 0.0, UG = sp$excretion$UG %||% 1.0,
    
    # Species parameters - Predator energy density
    PREDEDEQ = as.integer(sp$predator$PREDEDEQ),
    Alpha1 = sp$predator$Alpha1 %||% 0.0, Beta1 = sp$predator$Beta1 %||% 0.0,
    Alpha2 = sp$predator$Alpha2 %||% 0.0, Beta2 = sp$predator$Beta2 %||% 0.0, Cutoff = sp$predator$Cutoff %||% 0.0,
    ED_data = if (!is.null(sp$predator$ED_data) && length(sp$predator$ED_data) > 1) {
      as.numeric(sp$predator$ED_data)
    } else {
      rep(as.numeric(sp$predator$ED_data %||% 7000.0), n_days)
    },
    
    # Body composition
    water_fraction = sp$composition$water_fraction %||% 0.75,
    fat_energy = sp$composition$fat_energy %||% 39500,
    protein_energy = sp$composition$protein_energy %||% 23600,
    max_fat_fraction = sp$composition$max_fat_fraction %||% 0.25
  )
  
  return(tmb_data)
}

#' Transform simulation data to TMB hierarchical format
#' @param simulation_data Processed simulation data
#' @param individual_data Data frame with columns: individual_id, initial_weight and observed_weight
#' @param covariates Optional covariate matrix or data frame or choose a column of individual_data
#' @keywords internal
transform_to_tmb_hierarchical <- function(simulation_data, individual_data = NULL, covariates = NULL) {
  
  # For hierarchical, we need individual_data as additional parameter
  if (is.null(individual_data)) {
    stop("individual_data required for hierarchical TMB format")
  }
  
  # Extract temporal data (shared across individuals)
  temporal_data <- simulation_data$temporal_data
  n_days <- simulation_data$metadata$duration
  
  # Extract species parameters (shared)
  sp <- simulation_data$species_params
  
  validate_individual_data(individual_data)
  # Process individual data
  n_individuals <- nrow(individual_data)
  initial_weights <- individual_data$initial_weight
  
  # Extract observed weights (using first observed weight column)
  weight_cols <- grep("^final_weight", names(individual_data), value = TRUE)
  if (length(weight_cols) == 0) {
    stop("No final_weight columns found in individual_data")
  }
  
  observed_weights_matrix <- individual_data[[weight_cols[1]]]
  observed_weights_matrix[is.na(observed_weights_matrix)] <- -1.0  # TMB convention for missing
  
  if (is.null(covariates)) {
    # Default: matriz vacía
    covariates_matrix <- matrix(0, nrow = n_individuals, ncol = 1)
    n_covariates <- 0
  } else if (is.character(covariates)) {
    if(!all(covariates %in% names(individual_data))){
      stop("covariates must be in individual_data")
    }
    covariate_data <- individual_data[covariates]
    covariates_matrix <- scale(as.matrix(covariate_data))
    n_covariates <- ncol(covariates_matrix)
  } else if (is.matrix(covariates) || is.data.frame(covariates)) {
    # Matriz directa
    covariates_matrix <- scale(as.matrix(covariates))
    n_covariates <- ncol(covariates_matrix)
  }
  
  # Build TMB hierarchical data structure
  tmb_data <- list(
    # Model type identifier
    model_type = "hierarchical",
    
    # Individual data
    observed_weights_matrix = observed_weights_matrix,
    initial_weights = initial_weights,
    n_individuals = as.integer(n_individuals),
    
    # Simulation settings
    n_days = as.integer(n_days),
    oxycal = as.numeric(simulation_data$simulation_settings$oxycal),
    
    # Temporal data (shared across individuals)
    temperature = as.numeric(temporal_data$temperature),
    diet_proportions = as.matrix(temporal_data$diet_proportions),
    prey_energies = as.matrix(temporal_data$prey_energies),
    prey_indigestible = as.matrix(temporal_data$prey_indigestible),
    reproduction_data = as.numeric(temporal_data$reproduction %||% rep(0, n_days)),
    
    # Covariates (default to none)
    covariates_matrix = covariates_matrix,
    n_covariates = n_covariates,
    
    # Species parameters - Consumption (same as basic)
    CEQ = as.integer(sp$consumption$CEQ),
    CA = sp$consumption$CA, CB = sp$consumption$CB, CQ = sp$consumption$CQ,
    CTM = sp$consumption$CTM, CTO = sp$consumption$CTO, CTL = sp$consumption$CTL %||% 0.0,
    CK1 = sp$consumption$CK1 %||% 0.0, CK4 = sp$consumption$CK4 %||% 0.0,
    CG1 = sp$consumption$CG1 %||% 1.0, CG2 = sp$consumption$CG2 %||% 1.0, CX = sp$consumption$CX %||% 1.0,
    
    # Species parameters - Respiration (same as basic)
    REQ = as.integer(sp$respiration$REQ),
    RA = sp$respiration$RA, RB = sp$respiration$RB, RQ = sp$respiration$RQ,
    RTO = sp$respiration$RTO, RTM = sp$respiration$RTM, RTL = sp$respiration$RTL %||% 5.0,
    RK1 = sp$respiration$RK1 %||% 0.1, RK4 = sp$respiration$RK4 %||% 0.1, RK5 = sp$respiration$RK5 %||% 0.1, RX = sp$respiration$RX %||% 0.0,
    ACT = sp$respiration$ACT %||% 1.0, BACT = sp$respiration$BACT %||% 0.0, SDA = sp$respiration$SDA %||% 0.15,
    
    # Species parameters - Egestion (same as basic)
    EGEQ = as.integer(sp$egestion$EGEQ),
    FA = sp$egestion$FA, FB = sp$egestion$FB %||% 0.0, FG = sp$egestion$FG %||% 1.0,
    
    # Species parameters - Excretion (same as basic)
    EXEQ = as.integer(sp$excretion$EXEQ),
    UA = sp$excretion$UA, UB = sp$excretion$UB %||% 0.0, UG = sp$excretion$UG %||% 1.0,
    
    # Species parameters - Predator energy density (same as basic)
    PREDEDEQ = as.integer(sp$predator$PREDEDEQ),
    Alpha1 = sp$predator$Alpha1 %||% 0.0, Beta1 = sp$predator$Beta1 %||% 0.0,
    Alpha2 = sp$predator$Alpha2 %||% 0.0, Beta2 = sp$predator$Beta2 %||% 0.0, Cutoff = sp$predator$Cutoff %||% 0.0,
    ED_data = if (!is.null(sp$predator$ED_data) && length(sp$predator$ED_data) > 1) {
      as.numeric(sp$predator$ED_data)
    } else {
      rep(as.numeric(sp$predator$ED_data %||% 7000.0), n_days)
    },
    
    # Body composition (same as basic)
    water_fraction = sp$composition$water_fraction %||% 0.75,
    fat_energy = sp$composition$fat_energy %||% 39500,
    protein_energy = sp$composition$protein_energy %||% 23600,
    max_fat_fraction = sp$composition$max_fat_fraction %||% 0.25
  )
  
  return(tmb_data)
}
