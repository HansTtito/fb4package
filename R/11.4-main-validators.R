#' Main Validation Functions for FB4
#'
#' @description
#' Main integration validation functions that orchestrate all other validators.
#' These are your primary validation entry points, optimized with core validators
#' but maintaining original interfaces and behavior.
#'
#' @name main-validators
#' @aliases main-validators
NULL

# ============================================================================
# BIOENERGETIC OBJECT VALIDATION (your original function, optimized)
# ============================================================================

#' Comprehensive validation for Bioenergetic objects
#'
#' @description
#' Enhanced version of your validate_bioenergetic_for_simulation() with improved
#' organization and better error accumulation.
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
  
  # 1. Basic structure validation using core validators
  structure_result <- validate_structure_core(
    data = bio_obj,
    data_name = "bio_obj",
    required_class = "list",
    allow_empty = FALSE
  )
  
  if (!structure_result$valid) {
    validation$valid <- FALSE
    validation$errors <- c(validation$errors, structure_result$errors)
    return(validation)
  }
  
  required_components <- c("species_params", "environmental_data", "diet_data", "simulation_settings")
  missing_components <- setdiff(required_components, names(bio_obj))
  
  if (length(missing_components) > 0) {
    validation$errors <- c(validation$errors, 
                           paste("Missing components:", paste(missing_components, collapse = ", ")))
    validation$valid <- FALSE
    return(validation)
  }
  
  # 2. Validate species equations using optimized function
  tryCatch({
    equation_validation <- validate_species_equations(bio_obj$species_params)
    if (!equation_validation$valid) {
      validation$valid <- FALSE
      validation$errors <- c(validation$errors, equation_validation$errors)
    }
    validation$warnings <- c(validation$warnings, equation_validation$warnings)
  }, error = function(e) {
    validation$valid <- FALSE
    validation$errors <- c(validation$errors, paste("Species parameter validation error:", e$message))
  })
  
  # 3. Validate environmental data using optimized function
  if (is.null(bio_obj$environmental_data$temperature)) {
    validation$errors <- c(validation$errors, "Missing temperature data")
    validation$valid <- FALSE
  } else {
    tryCatch({
      validate_time_series_data(
        bio_obj$environmental_data$temperature, 
        "temperature", 
        c("Day", "Temperature")
      )
    }, error = function(e) {
      validation$errors <- c(validation$errors, paste("Temperature data error:", e$message))
      validation$valid <- FALSE
    })
  }
  
  # 4. Validate diet data using optimized function
  if (is.null(bio_obj$diet_data$proportions) || is.null(bio_obj$diet_data$energies)) {
    validation$errors <- c(validation$errors, "Missing diet data (proportions or energies)")
    validation$valid <- FALSE
  } else {
    tryCatch({
      validate_diet_consistency(bio_obj$diet_data$proportions, bio_obj$diet_data$energies)
    }, error = function(e) {
      validation$errors <- c(validation$errors, paste("Diet data error:", e$message))
      validation$valid <- FALSE
    })
  }
  
  # 5. Validate simulation settings using core validators
  if (is.null(bio_obj$simulation_settings$initial_weight)) {
    validation$errors <- c(validation$errors, "Missing initial_weight in simulation_settings")
    validation$valid <- FALSE
  } else {
    weight_result <- validate_positive(bio_obj$simulation_settings$initial_weight, 
                                      "initial_weight", strategy = "strict")
    if (!weight_result$valid) {
      validation$errors <- c(validation$errors, weight_result$errors)
      validation$valid <- FALSE
    }
  }
  
  # 6. Check if ready to run
  if (validation$valid) {
    validation$ready_to_run <- TRUE
    validation$warnings <- c(validation$warnings, "Object is ready for simulation")
  }
  
  return(validation)
}

# ============================================================================
# FB4 INPUT VALIDATION (your original function, optimized)
# ============================================================================

#' Validate inputs for FB4 simulation
#'
#' @description
#' Your original validate_fb4_inputs() function, optimized with core validators
#' while maintaining exact original interface and behavior.
#'
#' @param bio_obj Bioenergetic object
#' @param strategy Strategy to use: "binary_search", "optim", "bootstrap", "mle", "hierarchical"
#' @param fit_to Fitting target (for traditional strategies)
#' @param fit_value Fitting value (for traditional strategies)
#' @param first_day First simulation day
#' @param last_day Last simulation day
#' @param observed_weights Vector of observed weights (for statistical strategies)
#' @param covariates Covariates (for hierarchical strategy)
#' @return NULL (throws error if invalid)
#' @export
validate_fb4_inputs <- function(bio_obj, strategy, fit_to = NULL, fit_value = NULL,
                         first_day = 1, last_day = NULL, observed_weights = NULL, covariates = NULL) {
  
  # ============================================================================
  # BASIC PARAMETER VALIDATION
  # ============================================================================
  
  # Validate first_day using core validators
  first_day_result <- validate_numeric_core(
    value = first_day,
    param_name = "first_day",
    integer_only = TRUE,
    min_val = 1,
    strategy = "strict"
  )
  
  if (!first_day_result$valid) {
    stop(paste(first_day_result$errors, collapse = "; "))
  }
  
  # Validate last_day if provided
  if (!is.null(last_day)) {
    last_day_result <- validate_numeric_core(
      value = last_day,
      param_name = "last_day",
      integer_only = TRUE,
      min_val = first_day + 1,
      strategy = "strict"
    )
    
    if (!last_day_result$valid) {
      stop(paste(last_day_result$errors, collapse = "; "))
    }
  }
  
  # ============================================================================
  # STRATEGY-SPECIFIC VALIDATION
  # ============================================================================
  
  # Strategy-specific validation (delegated to optimized functions)
  if (strategy %in% c("mle", "bootstrap", "hierarchical")) {
    validate_statistical_method_inputs(strategy, observed_weights, covariates)
  } else if (strategy %in% c("direct", "optim")) {
    # direct/optim use p_value directly — fit_to/fit_value not required
    invisible(NULL)
  } else {
    # Remaining traditional strategies (binary_search, etc.) need fit_to/fit_value
    if (is.null(fit_to) || is.null(fit_value)) {
      stop("fit_to and fit_value required for traditional strategies")
    } else {
      valid_fit_options <- c("Weight", "Consumption", "Ration", "Ration_prey", "p_value")
      if (!fit_to %in% valid_fit_options) {
        stop("fit_to must be one of: ", paste(valid_fit_options, collapse = ", "))
      }
      
      # Validate fit_value using core validators
      fit_value_result <- validate_positive(fit_value, "fit_value", strategy = "strict")
      if (!fit_value_result$valid) {
        stop("fit_value must be a positive number when fit_to is specified")
      }
    }
  }
  
  # ============================================================================
  # BIOENERGETIC OBJECT VALIDATION
  # ============================================================================
  
  obj_validation <- validate_bioenergetic_for_simulation(bio_obj)
  
  if (!obj_validation$valid) {
    stop("Bioenergetic object validation failed:\n", 
         paste(obj_validation$errors, collapse = "\n"))
  }
  
  # ============================================================================
  # DATA RANGE VALIDATION
  # ============================================================================
  
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
# STATISTICAL METHOD VALIDATION (your original functions, optimized)
# ============================================================================

#' Validate statistical strategy inputs
#' @keywords internal
validate_statistical_method_inputs <- function(strategy, observed_weights, covariates = NULL) {
 
  if (is.null(observed_weights)) {
    stop("observed_weights must be provided for ", strategy, " strategy")
  }
 
  # Strategy-specific validation
  if (strategy == "hierarchical") {
    # observed_weights is the individual_data in this case
    validate_individual_data(observed_weights)
    
    if (nrow(observed_weights) < 3) {
      stop("Hierarchical strategy requires at least 3 individuals")
    }
    
    # Validate covariates for hierarchical strategy
    if (!is.null(covariates)) {
      if (is.character(covariates)) {
        if (!all(covariates %in% names(observed_weights))) {
          stop("All covariates must be present in observed_weights")
        }
      } else if (!is.matrix(covariates) && !is.data.frame(covariates)) {
        stop("covariates must be NULL, character vector, matrix, or data.frame")
      }
      
      # Validate dimensions if covariates is matrix/data.frame
      if (is.matrix(covariates) || is.data.frame(covariates)) {
        if (nrow(covariates) != nrow(observed_weights)) {
          stop("Number of rows in covariates must match number of individuals")
        }
      }
    }
  } else if (strategy %in% c("mle", "bootstrap")) {
    # MLE/Bootstrap: expects numeric vector, validate using core validators
    weights_result <- validate_positive(observed_weights, "observed_weights", strategy = "strict")
    if (!weights_result$valid) {
      stop("observed_weights must be positive numeric values for ", strategy, " strategy")
    }
   
    min_obs <- if (strategy == "bootstrap") 5 else 3
    if (length(observed_weights) < min_obs) {
      stop("At least ", min_obs, " observations required for ", strategy, " strategy")
    }
    
    # For non-hierarchical strategies, covariates should not be used
    if (!is.null(covariates)) {
      warning("covariates are only supported for hierarchical strategy and will be ignored")
    }
  }
  
  return(invisible(TRUE))
}

#' Validate backend compatibility
#' @keywords internal
validate_backend_compatibility <- function(strategy, backend, verbose = FALSE) {

  if(!any(backend %in% c("tmb","r"))){
    stop("backend only can be r or tmb")
  }
 
  if (backend == "tmb" && !strategy %in% c("mle", "hierarchical")) {
    if (verbose) {
      message("TMB backend only supports 'mle' and 'hierarchical' strategies, using R backend")
    }
    backend <- "r"
  }
 
  if (backend == "tmb" && !check_tmb_compilation("fb4package", verbose = FALSE)) {
    if (verbose) {
      message("TMB not available, falling back to R backend")
    }
    backend <- "r"
  }
 
  return(backend)
}

# ============================================================================
# ADVANCED VALIDATION FUNCTIONS
# ============================================================================

#' Validate complete FB4 system ready for simulation
#'
#' @description
#' Comprehensive validation that combines all validation layers.
#' This is the ultimate validation function for production use.
#'
#' @param bio_obj Bioenergetic object
#' @param strategy Strategy to use: "binary_search", "optim", "bootstrap", "mle", "hierarchical"
#' @param first_day First simulation day
#' @param last_day Last simulation day
#' @param validation_level Validation strictness ("basic", "standard", "comprehensive")
#' @param ... Additional arguments for strategy-specific validation
#'
#' @return List with comprehensive validation results
#' @export
validate_fb4_system <- function(bio_obj, strategy, first_day = 1, last_day = NULL,
                                validation_level = "standard", ...) {
  
  validation_result <- list(
    system_valid = TRUE,
    validation_level = validation_level,
    errors = character(),
    warnings = character(),
    info = character(),
    component_results = list(),
    timestamp = Sys.time()
  )
  
  # Level 1: Basic validation (always performed)
  tryCatch({
    validate_basic_params(bio_obj$simulation_settings$initial_weight %||% 1, 
                         last_day - first_day + 1)
    validation_result$component_results$basic <- "PASSED"
  }, error = function(e) {
    validation_result$system_valid <<- FALSE
    validation_result$errors <<- c(validation_result$errors, paste("Basic validation:", e$message))
    validation_result$component_results$basic <<- "FAILED"
  })
  
  # Level 2: Structure validation
  if (validation_level %in% c("standard", "comprehensive")) {
    tryCatch({
      bio_validation <- validate_bioenergetic_for_simulation(bio_obj)
      if (!bio_validation$valid) {
        validation_result$system_valid <- FALSE
        validation_result$errors <- c(validation_result$errors, bio_validation$errors)
      }
      validation_result$warnings <- c(validation_result$warnings, bio_validation$warnings)
      validation_result$component_results$bioenergetic <- if (bio_validation$valid) "PASSED" else "FAILED"
    }, error = function(e) {
      validation_result$system_valid <<- FALSE
      validation_result$errors <<- c(validation_result$errors, paste("Structure validation:", e$message))
      validation_result$component_results$bioenergetic <<- "FAILED"
    })
  }
  
  # Level 3: Comprehensive validation (includes integration tests)
  if (validation_level == "comprehensive") {
    
    # Test data processing pipeline
    tryCatch({
      test_data <- prepare_simulation_data(bio_obj, first_day, 
                                          last_day %||% (first_day + 10), 
                                          validate_inputs = TRUE, 
                                          output_format = "simulation")
      validation_result$component_results$data_processing <- "PASSED"
      validation_result$info <- c(validation_result$info, "Data processing pipeline validated")
    }, error = function(e) {
      validation_result$warnings <- c(validation_result$warnings, 
                                     paste("Data processing test:", e$message))
      validation_result$component_results$data_processing <- "WARNING"
    })
    
    # Test parameter validation
    tryCatch({
      species_validation <- validate_species_equations(bio_obj$species_params)
      validation_result$component_results$species_parameters <- if (species_validation$valid) "PASSED" else "FAILED"
      if (!species_validation$valid) {
        validation_result$system_valid <- FALSE
        validation_result$errors <- c(validation_result$errors, species_validation$errors)
      }
      validation_result$warnings <- c(validation_result$warnings, species_validation$warnings)
    }, error = function(e) {
      validation_result$system_valid <<- FALSE
      validation_result$errors <<- c(validation_result$errors, paste("Species parameter validation:", e$message))
      validation_result$component_results$species_parameters <<- "FAILED"
    })
  }
  
  # Strategy-specific validation
  dots <- list(...)
  if (strategy %in% c("mle", "bootstrap", "hierarchical") && "observed_weights" %in% names(dots)) {
    tryCatch({
      validate_statistical_method_inputs(strategy, dots$observed_weights, dots$covariates)
      validation_result$component_results$strategy_validation <- "PASSED"
    }, error = function(e) {
      validation_result$system_valid <<- FALSE
      validation_result$errors <<- c(validation_result$errors, paste("Strategy validation:", e$message))
      validation_result$component_results$strategy_validation <<- "FAILED"
    })
  }
  
  # Summary
  validation_result$summary <- list(
    total_components = length(validation_result$component_results),
    passed = sum(validation_result$component_results == "PASSED"),
    failed = sum(validation_result$component_results == "FAILED"),
    warnings = sum(validation_result$component_results == "WARNING"),
    overall_status = if (validation_result$system_valid) "READY" else "NOT_READY"
  )
  
  return(validation_result)
}
