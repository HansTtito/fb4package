#' Parameter Validation Functions for FB4
#'
#' @description
#' Reorganized and optimized versions of your parameter validation functions.
#' Maintains original interfaces while using core validators internally
#' to eliminate code duplication.
#'
#' @name parameter-validators
#' @aliases parameter-validators
NULL

# ============================================================================
# EQUATION REQUIREMENTS
# ============================================================================

#' Equation requirements for all FB4 components
#' @keywords internal
EQUATION_REQUIREMENTS <- list(
  
  # ========== CONSUMPTION ==========
  consumption = list(
    "1" = list(
      required = c("CEQ", "CA", "CB", "CQ"),
      validations = list(
        CEQ = list(range = 1:4, type = "integer"),
        CA = list(range = c(0, Inf), type = "positive"),
        CB = list(range = c(-Inf, Inf), type = "numeric"),
        CQ = list(range = c(0, Inf), type = "positive")
      )
    ),
    
    "2" = list(
      required = c("CEQ", "CA", "CB", "CTM", "CTO", "CQ"),
      optional = c("CX"),
      validations = list(
        CEQ = list(range = 1:4, type = "integer"),
        CA = list(range = c(0, Inf), type = "positive"),
        CB = list(range = c(-Inf, Inf), type = "numeric"),
        CQ = list(range = c(0, Inf), type = "positive"),
        CTM = list(range = c(-50, 50), type = "numeric"),
        CTO = list(range = c(-50, 50), type = "numeric"),
        CX = list(range = c(0, 10), type = "positive")
      ),
      parameter_relationships = list(
        "temperature_hierarchy" = "CTM > CTO > CQ"
      )
    ),
    
    "3" = list(
      required = c("CEQ", "CA", "CB", "CQ", "CTO", "CTL", "CTM", "CK1", "CK4"),
      optional = c("CG1", "CG2"),
      validations = list(
        CEQ = list(range = 1:4, type = "integer"),
        CA = list(range = c(0, Inf), type = "positive"),
        CB = list(range = c(-Inf, Inf), type = "numeric"),  
        CQ = list(range = c(0, Inf), type = "positive"),
        CTO = list(range = c(-50, 50), type = "numeric"),
        CTL = list(range = c(-50, 50), type = "numeric"),
        CTM = list(range = c(-50, 50), type = "numeric"),
        CK1 = list(range = c(0.000001, 0.99), type = "positive"),
        CK4 = list(range = c(0.000001, 0.99), type = "positive"),
        CG1 = list(range = c(0, Inf), type = "positive"),
        CG2 = list(range = c(0, Inf), type = "positive")
      ),
      parameter_relationships = list(
        "temperature_hierarchy" = "CTL > CTM > CTO > CQ"
      )
    ),
    
    "4" = list(
      required = c("CEQ", "CA", "CB", "CQ", "CK1", "CK4"),
      validations = list(
        CEQ = list(range = 1:4, type = "integer"),
        CA = list(range = c(0, Inf), type = "positive"),
        CB = list(range = c(0, Inf), type = "positive"),
        CQ = list(range = c(0, Inf), type = "positive"),
        CK1 = list(range = c(0.000001, 0.99), type = "positive"),
        CK4 = list(range = c(0.000001, 0.99), type = "positive")
      )
    )
  ),
  
  # ========== RESPIRATION ==========
  respiration = list(
    "1" = list(
      respiration_required = c("REQ", "RA", "RB", "RQ", "RTO", "RTM", "RTL", "RK1", "RK4", "RK5"),
      activity_required = c("ACT", "BACT"),
      sda_required = c("SDA"),
      validations = list(
        # Respiration parameters
        REQ = list(range = 1:2, type = "integer"),
        RA = list(range = c(0, Inf), type = "positive"),
        RB = list(range = c(-Inf, Inf), type = "numeric"),  
        RQ = list(range = c(0, Inf), type = "positive"),
        RTO = list(range = c(-50, 50), type = "numeric"),
        RTM = list(range = c(-50, 50), type = "numeric"),
        RTL = list(range = c(-50, 50), type = "numeric"),
        RK1 = list(range = c(0, 1), type = "positive"),        
        RK4 = list(range = c(0, 1), type = "positive"),        
        RK5 = list(range = c(0, 1), type = "positive"),        
        # Activity parameters
        ACT = list(range = c(0, 10), type = "positive"),
        BACT = list(range = c(0, 10), type = "positive"),
        # SDA parameters
        SDA = list(range = c(0, 1), type = "fraction")
      ),
      parameter_relationships = list(
        "temperature_hierarchy_conditional" = "RTM > RTO (if RTM > 0)"
      )
    ),
    
    "2" = list(
      respiration_required = c("REQ", "RA", "RB", "RQ", "RTO", "RTM"),
      activity_required = c("ACT"),
      sda_required = c("SDA"),
      optional = c("RX"),
      validations = list(
        REQ = list(range = 1:2, type = "integer"),
        RA = list(range = c(0, Inf), type = "positive"),
        RB = list(range = c(0, Inf), type = "positive"),
        RQ = list(range = c(0, Inf), type = "positive"),
        RTO = list(range = c(-50, 50), type = "numeric"),
        RTM = list(range = c(-50, 50), type = "numeric"),
        RX = list(range = c(0, 10), type = "positive"),
        ACT = list(range = c(0, 10), type = "positive"),
        SDA = list(range = c(0, 1), type = "fraction")
      ),
      parameter_relationships = list(
        "temperature_hierarchy" = "RTM > RTO"
      )
    )
  ),
  
  # ========== EGESTION ==========
  egestion = list(
    "1" = list(
      required = c("EGEQ", "FA"),
      validations = list(
        EGEQ = list(range = 1:4, type = "integer"),
        FA = list(range = c(0, 1), type = "fraction")
      )
    ),
    
    "2" = list(
      required = c("EGEQ", "FA", "FB", "FG"),
      validations = list(
        EGEQ = list(range = 1:4, type = "integer"),
        FA = list(range = c(0, 1), type = "fraction"),
        FB = list(range = c(-5, 5), type = "numeric"),
        FG = list(range = c(0, Inf), type = "positive")
      )
    ),
    
    "3" = list(
      required = c("EGEQ", "FA", "FB", "FG"),
      validations = list(
        EGEQ = list(range = 1:4, type = "integer"),
        FA = list(range = c(0, 1), type = "fraction"),
        FB = list(range = c(-5, 5), type = "numeric"),
        FG = list(range = c(0, Inf), type = "positive")
      )
    ),
    
    "4" = list(
      required = c("EGEQ", "FA", "FB"),
      validations = list(
        EGEQ = list(range = 1:4, type = "integer"),
        FA = list(range = c(0, 1), type = "fraction"),
        FB = list(range = c(-5, 5), type = "numeric")
      )
    )
  ),
  
  # ========== EXCRETION ==========
  excretion = list(
    "1" = list(
      required = c("EXEQ", "UA"),
      validations = list(
        EXEQ = list(range = 1:4, type = "integer"),
        UA = list(range = c(0, Inf), type = "positive")
      )
    ),
    
    "2" = list(
      required = c("EXEQ", "UA", "UB", "UG"),
      validations = list(
        EXEQ = list(range = 1:4, type = "integer"),
        UA = list(range = c(0, Inf), type = "positive"),
        UB = list(range = c(0, Inf), type = "positive"),
        UG = list(range = c(-Inf, Inf), type = "numeric")
      )
    ),
    
    "3" = list(
      required = c("EXEQ", "UA", "UB", "UG"),
      validations = list(
        EXEQ = list(range = 1:4, type = "integer"),
        UA = list(range = c(0, Inf), type = "positive"),
        UB = list(range = c(0, Inf), type = "positive"),
        UG = list(range = c(-Inf, Inf), type = "numeric")
      )
    ),
    
    "4" = list(
      required = c("EXEQ", "UA", "UB"),
      validations = list(
        EXEQ = list(range = 1:4, type = "integer"),
        UA = list(range = c(0, Inf), type = "positive"),
        UB = list(range = c(0, Inf), type = "positive")
      )
    )
  ),
  
  # ========== PREDATOR ==========
  predator = list(
    "1" = list(
      required = c("PREDEDEQ"),
      validations = list(
        PREDEDEQ = list(range = 1:3, type = "integer")
      ),
      data_requirements = list(
        energy_data_or_range = "ED_data OR (ED_ini AND ED_end)"
      )
    ),
    
    "2" = list(
      required = c("PREDEDEQ", "Alpha1", "Beta1", "Cutoff"),
      optional = c("Alpha2", "Beta2"),
      validations = list(
        PREDEDEQ = list(range = 1:3, type = "integer"),
        Alpha1 = list(range = c(0, Inf), type = "positive"),
        Beta1 = list(range = c(-2, 2), type = "numeric"),
        Cutoff = list(range = c(0, Inf), type = "positive"),
        Alpha2 = list(range = c(0, Inf), type = "positive"),
        Beta2 = list(range = c(-2, 2), type = "numeric")
      ),
      energy_density_constraints = list(
        min = 1000,
        max = 15000
      )
    ),
    
    "3" = list(
      required = c("PREDEDEQ", "Alpha1", "Beta1"),
      validations = list(
        PREDEDEQ = list(range = 1:3, type = "integer"),
        Alpha1 = list(range = c(0, Inf), type = "positive"),
        Beta1 = list(range = c(-2, 2), type = "numeric")
      ),
      energy_density_constraints = list(
        min = 1000,
        max = 15000
      )
    )
  )
)

# ============================================================================
# MAIN SPECIES VALIDATION
# ============================================================================

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
      
      # Validate this category using optimized function
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

# ============================================================================
# REQUIREMENT VALIDATION
# ============================================================================

#' @noRd
validate_standard_requirements <- function(eq_reqs, category_params, validation) {
  
  # Check required parameters
  if ("required" %in% names(eq_reqs)) {
    missing_required <- setdiff(eq_reqs$required, names(category_params))
    if (length(missing_required) > 0) {
      validation$errors <- c(validation$errors, 
                             paste("Missing required parameters:", paste(missing_required, collapse = ", ")))
      validation$valid <- FALSE
    }
    
    # Check for NA values in required parameters using core validation
    for (param in eq_reqs$required) {
      if (param %in% names(category_params)) {
        value <- category_params[[param]]
        if (is.null(value) || (is.numeric(value) && any(is.na(value)))) {
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
  
  # Validate parameter ranges using core validators
  if ("validations" %in% names(eq_reqs)) {
    range_validation <- validate_parameter_ranges(category_params, eq_reqs$validations, validation$category)
    if (!range_validation$valid) {
      validation$valid <- FALSE
      validation$errors <- c(validation$errors, range_validation$errors)
    }
    validation$warnings <- c(validation$warnings, range_validation$warnings)
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
    has_ed_data <- !is.null(predator_params$ED_data) && !anyNA(predator_params$ED_data)
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

#' Validate parameter ranges
#'
#' @param params Parameter list
#' @param validations Validation specifications
#' @param category Category name for error messages
#' @keywords internal
validate_parameter_ranges <- function(params, validations, category) {
  
  validation <- list(
    valid = TRUE,
    errors = character(),
    warnings = character()
  )
  
  for (param_name in names(validations)) {
    if (param_name %in% names(params)) {
      
      value <- params[[param_name]]
      validation_spec <- validations[[param_name]]
      
      # Use core validators based on type
      type_spec <- validation_spec$type %||% "numeric"
      
      if (type_spec == "positive") {
        param_result <- validate_positive(value, param_name, strategy = "strict")
      } else if (type_spec == "fraction") {
        param_result <- validate_fraction(value, param_name, strategy = "strict")
      } else if (type_spec == "integer") {
        param_result <- validate_numeric_core(value, param_name, integer_only = TRUE, 
                                             min_val = min(validation_spec$range), 
                                             max_val = max(validation_spec$range),
                                             strategy = "strict")
      } else {
        # Generic numeric validation with range
        range_spec <- validation_spec$range
        min_val <- if (length(range_spec) >= 1) min(range_spec) else NULL
        max_val <- if (length(range_spec) >= 2) max(range_spec) else NULL
        
        param_result <- validate_numeric_core(value, param_name, 
                                             min_val = min_val, max_val = max_val,
                                             strategy = "strict")
      }
      
      # Accumulate results
      if (!param_result$valid) {
        validation$valid <- FALSE
        validation$errors <- c(validation$errors, 
                              paste(category, "parameter", param_name, "validation failed:", 
                                    paste(param_result$errors, collapse = "; ")))
      }
      validation$warnings <- c(validation$warnings, param_result$warnings)
    }
  }
  
  return(validation)
}

# ============================================================================
# SPECIALIZED PARAMETER VALIDATORS
# ============================================================================

#' Validate predator energy density parameters
#'
#' @param predator_params List with parameters
#' @param weight_range Weight range for testing
#' @return List with validation results
#' @export
validate_predator_energy_params <- function(predator_params, weight_range = c(1, 1000)) {
  
  validation <- list(
    valid = TRUE,
    errors = character(),
    warnings = character()
  )
  
  PREDEDEQ <- predator_params$PREDEDEQ %||% 1
  
  if (!PREDEDEQ %in% 1:3) {
    validation$errors <- c(validation$errors, "PREDEDEQ must be 1, 2, or 3")
    validation$valid <- FALSE
    return(validation)
  }
  
  # Test calculations across weight range
  test_weights <- seq(weight_range[1], weight_range[2], length.out = 10)
  
  for (weight in test_weights) {
    tryCatch({
      # This would use your existing calculate_predator_energy_density function
      ed <- if (PREDEDEQ == 1) {
        predator_params$ED_data %||% 7000  # Default
      } else if (PREDEDEQ %in% 2:3) {
        predator_params$Alpha1 * (weight ^ predator_params$Beta1)
      } else {
        7000  # Default
      }
      
      if (ed < 1000) {
        validation$warnings <- c(validation$warnings, 
                                 paste("Low energy density for weight", weight, "g:", round(ed)))
      }
      
      if (ed > 15000) {
        validation$warnings <- c(validation$warnings,
                                 paste("High energy density for weight", weight, "g:", round(ed)))
      }
      
    }, error = function(e) {
      validation$errors <- c(validation$errors, 
                             paste("Error calculating density for weight", weight, "g:", e$message))
      validation$valid <- FALSE
    })
  }
  
  # Equation-specific validations using core validators
  if (PREDEDEQ == 2) {
    required_params <- c("Alpha1", "Beta1", "Cutoff")
    for (param in required_params) {
      if (param %in% names(predator_params)) {
        param_result <- validate_positive(predator_params[[param]], param, strategy = "warn")
        validation$warnings <- c(validation$warnings, param_result$warnings)
      }
    }
  } else if (PREDEDEQ == 3) {
    required_params <- c("Alpha1", "Beta1")
    for (param in required_params) {
      if (param %in% names(predator_params)) {
        param_result <- validate_positive(predator_params[[param]], param, strategy = "warn")
        validation$warnings <- c(validation$warnings, param_result$warnings)
      }
    }
  }
  
  return(validation)
}

#' Validate contaminant parameters
#'
#' @param contaminant_params List with parameters
#' @return List with validation results
#' @export
validate_contaminant_params <- function(contaminant_params) {
  
  validation <- list(
    valid = TRUE,
    warnings = character(),
    errors = character()
  )
  
  CONTEQ <- contaminant_params$CONTEQ %||% 1
  
  if (!CONTEQ %in% 1:3) {
    validation$errors <- c(validation$errors, "CONTEQ must be 1, 2, or 3")
    validation$valid <- FALSE
    return(validation)
  }
  
  # Validate prey concentrations using core validators
  if ("prey_concentrations" %in% names(contaminant_params)) {
    conc_result <- validate_numeric_core(
      value = contaminant_params$prey_concentrations,
      param_name = "prey_concentrations",
      min_val = 0,
      max_val = 1000,
      strategy = "warn"
    )
    if (!conc_result$valid) {
      validation$valid <- FALSE
      validation$errors <- c(validation$errors, conc_result$errors)
    }
    validation$warnings <- c(validation$warnings, conc_result$warnings)
  }
  
  # Validate efficiencies using core validators
  efficiency_params <- c("transfer_efficiency", "assimilation_efficiency")
  for (param in efficiency_params) {
    if (param %in% names(contaminant_params)) {
      eff_result <- validate_fraction(contaminant_params[[param]], param, strategy = "warn")
      if (!eff_result$valid) {
        validation$valid <- FALSE
        validation$errors <- c(validation$errors, eff_result$errors)
      }
      validation$warnings <- c(validation$warnings, eff_result$warnings)
    }
  }
  
  # Model 3 specific validations
  if (CONTEQ == 3) {
    model3_params <- c("gill_efficiency", "dissolved_fraction")
    for (param in model3_params) {
      if (param %in% names(contaminant_params)) {
        param_result <- validate_fraction(contaminant_params[[param]], param, strategy = "warn")
        if (!param_result$valid) {
          validation$valid <- FALSE
          validation$errors <- c(validation$errors, param_result$errors)
        }
        validation$warnings <- c(validation$warnings, param_result$warnings)
      }
    }
    
    if ("fish_water_partition" %in% names(contaminant_params)) {
      partition_result <- validate_positive(contaminant_params$fish_water_partition,
                                           "fish_water_partition", strategy = "warn")
      if (!partition_result$valid) {
        validation$valid <- FALSE
        validation$errors <- c(validation$errors, partition_result$errors)
      }
      validation$warnings <- c(validation$warnings, partition_result$warnings)
    }
  }
  
  return(validation)
}

#' Validate nutrient concentrations
#'
#' @param nutrient_concentrations List with N and P concentrations
#' @param organism_type Organism type for validation
#' @return List with validation results
#' @export
validate_nutrient_concentrations <- function(nutrient_concentrations, organism_type = "fish") {
  
  validation <- list(
    valid = TRUE,
    warnings = character(),
    errors = character()
  )
  
  # Typical ranges (g/g wet weight)
  typical_ranges <- list(
    fish = list(nitrogen = c(0.08, 0.12), phosphorus = c(0.01, 0.02)),
    zooplankton = list(nitrogen = c(0.07, 0.11), phosphorus = c(0.008, 0.015)),
    invertebrates = list(nitrogen = c(0.06, 0.10), phosphorus = c(0.006, 0.012))
  )
  
  if (!organism_type %in% names(typical_ranges)) {
    validation$warnings <- c(validation$warnings, 
                             paste("Unrecognized organism type:", organism_type))
    organism_type <- "fish"
  }
  
  ranges <- typical_ranges[[organism_type]]
  
  # Validate nitrogen using core validators
  if ("nitrogen" %in% names(nutrient_concentrations)) {
    n_result <- validate_numeric_core(
      value = nutrient_concentrations$nitrogen,
      param_name = "nitrogen_concentrations",
      min_val = 0,
      strategy = "warn"
    )
    if (!n_result$valid) {
      validation$valid <- FALSE
      validation$errors <- c(validation$errors, n_result$errors)
    }
    validation$warnings <- c(validation$warnings, n_result$warnings)
    
    # Check typical ranges
    n_values <- nutrient_concentrations$nitrogen
    if (any(n_values < ranges$nitrogen[1] | n_values > ranges$nitrogen[2], na.rm = TRUE)) {
      validation$warnings <- c(validation$warnings, 
                               paste("N concentrations outside typical range for", organism_type))
    }
  }
  
  # Validate phosphorus using core validators
  if ("phosphorus" %in% names(nutrient_concentrations)) {
    p_result <- validate_numeric_core(
      value = nutrient_concentrations$phosphorus,
      param_name = "phosphorus_concentrations",
      min_val = 0,
      strategy = "warn"
    )
    if (!p_result$valid) {
      validation$valid <- FALSE
      validation$errors <- c(validation$errors, p_result$errors)
    }
    validation$warnings <- c(validation$warnings, p_result$warnings)
    
    # Check typical ranges
    p_values <- nutrient_concentrations$phosphorus
    if (any(p_values < ranges$phosphorus[1] | p_values > ranges$phosphorus[2], na.rm = TRUE)) {
      validation$warnings <- c(validation$warnings, 
                               paste("P concentrations outside typical range for", organism_type))
    }
  }
  
  # Validate N:P ratio
  if ("nitrogen" %in% names(nutrient_concentrations) && 
      "phosphorus" %in% names(nutrient_concentrations)) {
    
    n_values <- nutrient_concentrations$nitrogen
    p_values <- nutrient_concentrations$phosphorus
    
    # Avoid division by zero
    valid_indices <- !is.na(p_values) & p_values > 0
    if (any(valid_indices)) {
      np_ratios <- n_values[valid_indices] / p_values[valid_indices]
      
      # Typical range for N:P ratios (mass): 4-15
      if (any(np_ratios < 2 | np_ratios > 20, na.rm = TRUE)) {
        validation$warnings <- c(validation$warnings, 
                                 "N:P ratios outside typical range (2-20)")
      }
    }
  }
  
  return(validation)
}

#' Validate body composition
#'
#' @param composition Body composition list
#' @return List with validation results
#' @export
validate_body_composition <- function(composition) {
  
  validation <- list(
    valid = TRUE,
    warnings = character(),
    errors = character()
  )
  
  # Typical ranges for fish (fractions)
  typical_ranges <- list(
    water = c(0.65, 0.85),
    protein = c(0.10, 0.25),
    ash = c(0.02, 0.08),
    fat = c(0.02, 0.25),
    energy_density = c(2000, 8000)
  )
  
  # Validate ranges using core validators
  if (!is.null(composition$water_fraction)) {
    water_result <- validate_fraction(composition$water_fraction, "water_fraction", strategy = "warn")
    if (!water_result$valid) {
      validation$valid <- FALSE
      validation$errors <- c(validation$errors, water_result$errors)
    }
    validation$warnings <- c(validation$warnings, water_result$warnings)
    
    if (composition$water_fraction < typical_ranges$water[1] || 
        composition$water_fraction > typical_ranges$water[2]) {
      validation$warnings <- c(validation$warnings,
                               paste("Water fraction outside typical range:",
                                     round(composition$water_fraction, 3)))
    }
  }
  
  if (!is.null(composition$protein_fraction)) {
    protein_result <- validate_fraction(composition$protein_fraction, "protein_fraction", strategy = "warn")
    if (!protein_result$valid) {
      validation$valid <- FALSE
      validation$errors <- c(validation$errors, protein_result$errors)
    }
    validation$warnings <- c(validation$warnings, protein_result$warnings)
    
    if (composition$protein_fraction < typical_ranges$protein[1] || 
        composition$protein_fraction > typical_ranges$protein[2]) {
      validation$warnings <- c(validation$warnings,
                               paste("Protein fraction outside typical range:",
                                     round(composition$protein_fraction, 3)))
    }
  }
  
  if (!is.null(composition$energy_density)) {
    energy_result <- validate_numeric_core(
      value = composition$energy_density,
      param_name = "energy_density",
      min_val = typical_ranges$energy_density[1],
      max_val = typical_ranges$energy_density[2],
      strategy = "warn"
    )
    validation$warnings <- c(validation$warnings, energy_result$warnings)
    
    if (composition$energy_density < typical_ranges$energy_density[1] || 
        composition$energy_density > typical_ranges$energy_density[2]) {
      validation$warnings <- c(validation$warnings,
                               paste("Energy density outside typical range:",
                                     round(composition$energy_density, 0), "J/g"))
    }
  }
  
  # Validate fraction balance
  if (!is.null(composition$balanced) && !composition$balanced) {
    if (!is.null(composition$total_fraction)) {
      validation$errors <- c(validation$errors, 
                             paste("Fractions do not sum to ~1.0:",
                                   round(composition$total_fraction, 3)))
      validation$valid <- FALSE
    }
  }
  
  # Validate negative values
  numeric_components <- c("water_g", "protein_g", "ash_g", "fat_g")
  for (component in numeric_components) {
    if (!is.null(composition[[component]]) && composition[[component]] < 0) {
      validation$errors <- c(validation$errors, paste("Negative", component, "detected"))
      validation$valid <- FALSE
    }
  }
  
  return(validation)
}