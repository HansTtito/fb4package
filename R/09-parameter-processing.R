#' Parameter Processing Functions for FB4
#'
#' @description
#' Functions for validating, transforming, and enriching raw user-supplied
#' species parameters before they are passed to the simulation engine.
#' Each major bioenergetic category (consumption, respiration, egestion,
#' excretion, predator energy density, contaminant, nutrient, mortality,
#' body composition) has a dedicated processor that (i) checks that the
#' required parameters for the chosen equation are present, (ii) computes
#' derived values (e.g., \code{CX}/\code{CY}/\code{CZ} for CEQ 2, gill
#' efficiency for CONTEQ 3), and (iii) fills missing optional parameters
#' with documented defaults. The top-level entry point is
#' \code{\link{process_species_parameters}}.
#'
#' @references
#' Hanson, P.C., Johnson, T.B., Schindler, D.E. and Kitchell, J.F. (1997).
#' \emph{Fish Bioenergetics 3.0}. University of Wisconsin Sea Grant Institute,
#' Madison, WI.
#'
#' Deslauriers, D., Chipps, S.R., Breck, J.E., Rice, J.A. and Madenjian, C.P.
#' (2017). Fish Bioenergetics 4.0: An R-based modeling application.
#' \emph{Fisheries}, 42(11), 586–596. \doi{10.1080/03632415.2017.1377558}
#'
#' @return No return value; this page documents the parameter processing functions module. See individual function documentation for return values.
#' @name parameter-processing
#' @aliases parameter-processing
NULL

# ============================================================================
# MAIN PARAMETER PROCESSING FUNCTIONS
# ============================================================================

#' Process all species parameters for simulation
#'
#' @description
#' Main function that processes and validates all species parameters,
#' calculating derived parameters and preparing them for simulation.
#'
#' @param species_params Raw species parameters from user
#' @param n_days Integer. Number of simulation days, used to build the
#'   predator energy density vector when `ED_ini`/`ED_end` are supplied
#'   (PREDEDEQ = 1). If `NULL` the vector length is inferred later.
#' @return A named list containing one processed sub-list for each category
#'   present in \code{species_params} (e.g. \code{consumption},
#'   \code{respiration}, \code{egestion}, \code{excretion}, \code{predator},
#'   and optionally \code{contaminant}, \code{nutrient}, \code{mortality},
#'   \code{composition}). Each sub-list holds the validated raw parameters
#'   plus any derived values required by the chosen equation. A
#'   \code{processing_info} element is always appended containing
#'   \code{processed_at} (POSIXct timestamp), \code{validation_warnings}
#'   (character vector), and \code{categories_processed} (character vector
#'   of processed category names).
#' @examples
#' sp <- list(
#'   consumption = list(CEQ = 1, CA = 0.303, CB = -0.275, CQ = 0.06),
#'   egestion    = list(EGEQ = 1, FA = 0.16),
#'   excretion   = list(EXEQ = 1, UA = 0.10),
#'   predator    = list(PREDEDEQ = 3, Alpha1 = 4800, Beta1 = 0.1),
#'   respiration = list(REQ = 2, RA = 0.0033, RB = -0.227,
#'                      RQ = 0.025, RTM = 30, RTO = 18),
#'   activity    = list(ACT = 1.5),
#'   sda         = list(SDA = 0.15)
#' )
#' process_species_parameters(sp)
#' @export
process_species_parameters <- function(species_params, n_days = NULL) {
  
  # Validate overall structure first
  validation <- validate_species_equations(species_params)
  if (!validation$valid) {
    stop("Species parameter validation failed:\n", 
         paste(validation$errors, collapse = "\n"))
  }
  
  processed_params <- list()
  
  # Process each category
  if ("consumption" %in% names(species_params)) {
    processed_params$consumption <- process_consumption_params(species_params$consumption)
  }
  
  if ("respiration" %in% names(species_params)) {
    processed_params$respiration <- process_respiration_params(
      species_params$respiration,
      activity_params = species_params$activity,
      sda_params = species_params$sda
    )
  }
  
  if ("egestion" %in% names(species_params)) {
    processed_params$egestion <- process_egestion_params(species_params$egestion)
  }
  
  if ("excretion" %in% names(species_params)) {
    processed_params$excretion <- process_excretion_params(species_params$excretion)
  }
  
  if ("predator" %in% names(species_params)) {
    processed_params$predator <- process_predator_params(species_params$predator, n_days = n_days)
  }
  
  # Process optional categories
  if ("contaminant" %in% names(species_params)) {
    processed_params$contaminant <- process_contaminant_params(species_params$contaminant)
  }
  
  if ("nutrient" %in% names(species_params)) {
    processed_params$nutrient <- process_nutrient_params(species_params$nutrient)
  }
  
  if ("mortality" %in% names(species_params)) {
    processed_params$mortality <- process_mortality_params(species_params$mortality)
  }
  
  if ("composition" %in% names(species_params)) {
    processed_params$composition <- process_composition_params(species_params$composition)
  }
  
  # Add processing metadata
  processed_params$processing_info <- list(
    processed_at = Sys.time(),
    validation_warnings = validation$warnings,
    categories_processed = names(processed_params)[names(processed_params) != "processing_info"]
  )
  
  return(processed_params)
}

# ============================================================================
# CONSUMPTION PARAMETER PROCESSING
# ============================================================================

#' Process consumption parameters
#'
#' @param consumption_params Raw consumption parameters
#' @return A list containing all elements of \code{consumption_params} plus
#'   derived values required by the selected equation: \code{CX}, \code{CY},
#'   \code{CZ} for CEQ 2 (Kitchell et al. 1977); \code{CG1}, \code{CG2} for
#'   CEQ 3 (Thornton and Lessem 1978). For CEQ 1 and CEQ 4 the input list is
#'   returned unchanged after validation.
#' @examples
#' process_consumption_params(list(CEQ = 1, CA = 0.303, CB = -0.275, CQ = 0.06))
#' @export
process_consumption_params <- function(consumption_params) {
  
  # Validate using equation requirements
  CEQ <- consumption_params$CEQ %||% 1
  validate_equation_params("consumption", as.character(CEQ), consumption_params)
  
  # Start with input parameters
  processed <- consumption_params
  
  # Calculate derived parameters based on equation
  if (CEQ == 2) {
    derived_params <- calculate_consumption_params_eq2(
      CQ = consumption_params$CQ,
      CTM = consumption_params$CTM,
      CTO = consumption_params$CTO,
      warn = TRUE
    )
    processed <- c(processed, derived_params)
    
  } else if (CEQ == 3) {
    derived_params <- calculate_consumption_params_eq3(
      CTO = consumption_params$CTO,
      CQ = consumption_params$CQ,
      CTL = consumption_params$CTL,
      CTM = consumption_params$CTM,
      CK1 = consumption_params$CK1,
      CK4 = consumption_params$CK4
    )
    processed <- c(processed, derived_params)
  }
  
  return(processed)
}

# ============================================================================
# RESPIRATION PARAMETER PROCESSING
# ============================================================================

#' Process respiration parameters
#'
#' @param respiration_params Raw respiration parameters
#' @param activity_params Activity parameters (required for REQ=1)
#' @param sda_params SDA parameters
#' @return A list containing all elements of \code{respiration_params},
#'   the activity parameters (merged from \code{activity_params}), the SDA
#'   coefficient (\code{SDA}), and — for REQ 2 — the derived values
#'   \code{RX}, \code{RY}, \code{RZ} (Kitchell et al. 1977). For REQ 1 no
#'   additional derived values are added.
#' @examples
#' process_respiration_params(
#'   respiration_params = list(REQ = 2, RA = 0.0033, RB = -0.227,
#'                             RQ = 0.025, RTM = 30, RTO = 18),
#'   activity_params = list(ACT = 1.5),
#'   sda_params = list(SDA = 0.15)
#' )
#' @export
process_respiration_params <- function(respiration_params, activity_params = NULL, 
                                       sda_params = NULL) {
  
  REQ <- respiration_params$REQ %||% 1
  
  # Validate main respiration parameters
  validate_equation_params("respiration", as.character(REQ), respiration_params)
  
  # Start with input parameters
  processed <- respiration_params
  
  # Add activity parameters (required for all equations)
  if (is.null(activity_params)) {
    stop("Activity parameters required for respiration calculations")
  }
  processed <- c(processed, activity_params)
  
  # Add SDA parameters
  if (is.null(sda_params)) {
    warning("SDA parameters not provided, using default SDA = 0.15")
    processed$SDA <- 0.15
  } else {
    processed <- c(processed, sda_params)
  }
  
  # Calculate derived parameters for equation 2
  if (REQ == 2) {
    derived_params <- calculate_respiration_params_eq2(
      RQ = respiration_params$RQ,
      RTM = respiration_params$RTM,
      RTO = respiration_params$RTO
    )
    processed <- c(processed, derived_params)
  }
  
  return(processed)
}

# ============================================================================
# EGESTION AND EXCRETION PARAMETER PROCESSING
# ============================================================================

#' Process egestion parameters
#'
#' @param egestion_params Raw egestion parameters
#' @return A list identical to \code{egestion_params} after validation. No
#'   additional derived values are computed; the function ensures the
#'   required parameters for the selected EGEQ are present and valid.
#' @examples
#' process_egestion_params(list(EGEQ = 1, FA = 0.16))
#' @export
process_egestion_params <- function(egestion_params) {
  
  EGEQ <- egestion_params$EGEQ %||% 1
  validate_equation_params("egestion", as.character(EGEQ), egestion_params)
  
  processed <- egestion_params
  
  return(processed)
}

#' Process excretion parameters
#'
#' @param excretion_params Raw excretion parameters
#' @return A list identical to \code{excretion_params} after validation. No
#'   additional derived values are computed; the function ensures the
#'   required parameters for the selected EXEQ are present and valid.
#' @examples
#' process_excretion_params(list(EXEQ = 1, UA = 0.10))
#' @export
process_excretion_params <- function(excretion_params) {
  
  EXEQ <- excretion_params$EXEQ %||% 1
  validate_equation_params("excretion", as.character(EXEQ), excretion_params)
  
  processed <- excretion_params
  
  return(processed)
}

# ============================================================================
# PREDATOR PARAMETER PROCESSING
# ============================================================================

#' Process predator energy density parameters
#'
#' @param predator_params Raw predator parameters
#' @param n_days Integer. Number of simulation days, used to build the
#'   energy density vector from `ED_ini`/`ED_end` when PREDEDEQ = 1.
#'   If `NULL` the vector is built lazily when the simulation starts.
#' @return A list containing all elements of \code{predator_params}. For
#'   PREDEDEQ 1, an \code{ED_data} numeric vector of length
#'   \code{n_days + 1} is added (or validated if already present). For
#'   PREDEDEQ 2 and 3 the list is returned after validation with no
#'   additional derived values.
#' @examples
#' process_predator_params(list(PREDEDEQ = 3, Alpha1 = 4800, Beta1 = 0.1))
#' @export
process_predator_params <- function(predator_params, n_days = NULL) {

  PREDEDEQ <- predator_params$PREDEDEQ %||% 1
  validate_equation_params("predator", as.character(PREDEDEQ), predator_params)

  processed <- predator_params

  # Pass simulation duration so process_predator_energy_data can build the
  # correct-length ED_data vector when ED_ini/ED_end are supplied
  if (!is.null(n_days)) {
    processed$simulation_days <- n_days
  }

  # Special processing for equation 1 (energy data)
  if (PREDEDEQ == 1) {
    processed <- process_predator_energy_data(processed)
  }
  
  # Validate energy density ranges
  validate_predator_energy_params(processed, c(1, 10000))
  
  return(processed)
}

#' Process predator energy data for equation 1 (PREDEDEQ = 1)
#'
#' Ensures \code{ED_data} is available as a numeric vector of length
#' \code{n_days + 1}. Accepts either a pre-built vector via \code{ED_data}
#' or a pair of scalars via \code{ED_ini}/\code{ED_end} (which are linearly
#' interpolated to produce the full vector internally).
#'
#' @param predator_params List of predator parameters. Must include either:
#'   \describe{
#'     \item{\code{ED_data}}{Numeric vector of length \code{n_days + 1}
#'       (e.g., 366 for 365 days). Element \code{[i]} is the energy density
#'       at the boundary of day \code{i-1}. Generate with
#'       \code{approx(..., xout = 0:n_days)$y} or
#'       \code{seq(ED_ini, ED_end, length.out = n_days + 1)}.}
#'     \item{\code{ED_ini} + \code{ED_end}}{Scalar start/end values; the
#'       function creates the full vector via linear interpolation.}
#'   }
#' @return Predator parameters list with \code{ED_data} populated.
#' @keywords internal
process_predator_energy_data <- function(predator_params) {
  
  # Check if user provided ED_data directly
  if (!is.null(predator_params$ED_data) && !anyNA(predator_params$ED_data)) {
    return(predator_params)
  }
  
  # Check if user provided ED_ini and ED_end
  if (!is.null(predator_params$ED_ini) && !is.null(predator_params$ED_end)) {
    # Create linear interpolation between initial and final
    n_days <- predator_params$simulation_days %||% 365
    predator_params$ED_data <- seq(from = predator_params$ED_ini, 
                                   to = predator_params$ED_end, 
                                   length.out = (n_days + 1))
    return(predator_params)
  }
  
  stop("PREDEDEQ=1 requires either ED_data OR both ED_ini and ED_end")
}

# ============================================================================
# ADDITIONAL PARAMETER PROCESSING
# ============================================================================

#' Process contaminant parameters
#'
#' @param contaminant_params Raw contaminant parameters
#' @return A list containing all elements of \code{contaminant_params}. For
#'   CONTEQ 3 (Arnot and Gobas 2004), three additional elements are computed
#'   when not already present: \code{gill_efficiency} (dimensionless),
#'   \code{fish_water_partition} (dimensionless), and
#'   \code{dissolved_fraction} (dimensionless). For CONTEQ 1 and 2 the list
#'   is returned after validation unchanged.
#' @examples
#' process_contaminant_params(list(
#'   CONTEQ = 1,
#'   prey_concentrations  = c(0.05, 0.08),
#'   transfer_efficiency  = c(0.80, 0.80)
#' ))
#' @export
process_contaminant_params <- function(contaminant_params) {
  
  CONTEQ <- contaminant_params$CONTEQ %||% 1
  
  processed <- contaminant_params
  
  # Calculate derived parameters for equation 3 (Arnot & Gobas)
  if (CONTEQ == 3) {
    
    # Calculate gill efficiency if not provided
    if (is.null(processed$gill_efficiency) && !is.null(processed$kow)) {
      processed$gill_efficiency <- calculate_gill_efficiency(processed$kow)
    }
    
    # Calculate fish:water partition coefficient
    if (is.null(processed$fish_water_partition) && 
        all(c("fat_fraction", "protein_ash_fraction", "water_fraction", "kow") %in% names(processed))) {
      processed$fish_water_partition <- calculate_fish_water_partition(
        processed$fat_fraction, processed$protein_ash_fraction, 
        processed$water_fraction, processed$kow
      )
    }
    
    # Calculate dissolved fraction
    if (is.null(processed$dissolved_fraction) && 
        all(c("poc_concentration", "doc_concentration", "kow") %in% names(processed))) {
      processed$dissolved_fraction <- calculate_dissolved_fraction(
        processed$poc_concentration, processed$doc_concentration, processed$kow
      )
    }
  }
  
  # Validate final parameters
  contaminant_validation <- validate_contaminant_params(processed)
  if (!contaminant_validation$valid) {
    stop("Contaminant parameter validation failed:\n", 
         paste(contaminant_validation$errors, collapse = "\n"))
  }
  
  return(processed)
}

#' Process nutrient parameters
#'
#' @param nutrient_params Raw nutrient parameters
#' @return A list containing all elements of \code{nutrient_params}. Default
#'   assimilation efficiencies are inserted when absent:
#'   \code{n_assimilation_efficiency = 0.85} and
#'   \code{p_assimilation_efficiency = 0.80} (with a warning in each case).
#' @examples
#' process_nutrient_params(list(
#'   prey_n_concentrations    = c(0.025, 0.030),
#'   prey_p_concentrations    = c(0.004, 0.005),
#'   predator_n_concentration = 0.030,
#'   predator_p_concentration = 0.004
#' ))
#' @export
process_nutrient_params <- function(nutrient_params) {
  
  processed <- nutrient_params
  
  # Set default assimilation efficiencies if not provided
  if (is.null(processed$n_assimilation_efficiency)) {
    processed$n_assimilation_efficiency <- 0.85  # Default 85%
    warning("Using default N assimilation efficiency = 0.85")
  }
  
  if (is.null(processed$p_assimilation_efficiency)) {
    processed$p_assimilation_efficiency <- 0.80  # Default 80%
    warning("Using default P assimilation efficiency = 0.80")
  }
  
  # Validate concentrations
  nutrient_validation <- validate_nutrient_concentrations(processed)
  if (!nutrient_validation$valid) {
    stop("Nutrient parameter validation failed:\n", 
         paste(nutrient_validation$errors, collapse = "\n"))
  }
  
  return(processed)
}

#' Process mortality parameters
#'
#' @param mortality_params Raw mortality parameters
#' @return A list containing all elements of \code{mortality_params} with
#'   missing entries filled with defaults: \code{base_mortality = 0.001},
#'   \code{natural_mortality} (copied from \code{base_mortality}),
#'   \code{fishing_mortality = 0}, \code{predation_mortality = 0},
#'   \code{optimal_temp = 15}, \code{thermal_tolerance = 5}, and
#'   \code{stress_factor = 2}. If a \code{reproduction} sub-list is present,
#'   a \code{spawn_pattern} numeric vector (length 365) is appended.
#' @examples
#' process_mortality_params(list(base_mortality = 0.001,
#'                               natural_mortality = 0.001))
#' @export
process_mortality_params <- function(mortality_params) {
  
  processed <- mortality_params
  
  # Set default values for missing parameters
  if (is.null(processed$base_mortality)) {
    processed$base_mortality <- 0.001  # 0.1% daily
    warning("Using default base_mortality = 0.001")
  }
  
  if (is.null(processed$natural_mortality)) {
    processed$natural_mortality <- processed$base_mortality
  }
  
  if (is.null(processed$fishing_mortality)) {
    processed$fishing_mortality <- 0
  }
  
  if (is.null(processed$predation_mortality)) {
    processed$predation_mortality <- 0
  }
  
  # Set default thermal tolerance parameters
  if (is.null(processed$optimal_temp)) {
    processed$optimal_temp <- 15  # Default 15\u00b0C
    warning("Using default optimal_temp = 15\u00b0C")
  }
  
  if (is.null(processed$thermal_tolerance)) {
    processed$thermal_tolerance <- 5  # Default \u00b15\u00b0C
    warning("Using default thermal_tolerance = 5\u00b0C")
  }
  
  if (is.null(processed$stress_factor)) {
    processed$stress_factor <- 2  # Default 2x mortality increase
  }
  
  # Process reproduction pattern if provided
  if (!is.null(processed$reproduction)) {
    processed <- process_reproduction_pattern(processed)
  }
  
  return(processed)
}

#' Process body composition parameters
#'
#' @param composition_params Raw composition parameters
#' @return A list containing all elements of \code{composition_params} with
#'   missing entries filled with defaults: \code{water_fraction = 0.75},
#'   \code{fat_energy = 39500} (J/g), \code{protein_energy = 23600} (J/g),
#'   and \code{max_fat_fraction = 0.25}.
#' @examples
#' process_composition_params(list(water_fraction = 0.72))
#' @export
process_composition_params <- function(composition_params) {
  
  processed <- composition_params
  
  # Set default water fraction if not provided
  if (is.null(processed$water_fraction)) {
    processed$water_fraction <- 0.75  # Default 75%
    warning("Using default water_fraction = 0.75")
  }
  
  # Set default energy densities
  if (is.null(processed$fat_energy)) {
    processed$fat_energy <- 39500  # J/g fat
  }
  
  if (is.null(processed$protein_energy)) {
    processed$protein_energy <- 23600  # J/g protein
  }
  
  # Set maximum fat fraction
  if (is.null(processed$max_fat_fraction)) {
    processed$max_fat_fraction <- 0.25  # Maximum 25% fat
  }
  
  return(processed)
}


# ============================================================================
# RANGE VALIDATION FUNCTIONS
# ============================================================================

#' Process reproduction pattern
#' @keywords internal
process_reproduction_pattern <- function(mortality_params) {
  
  repro <- mortality_params$reproduction
  
  # If spawn_pattern is already provided, use it
  if (!is.null(repro$spawn_pattern)) {
    mortality_params$spawn_pattern <- repro$spawn_pattern
    return(mortality_params)
  }
  
  # Generate pattern from parameters
  if (all(c("peak_day", "duration", "max_spawn_fraction") %in% names(repro))) {
    
    pattern_type <- repro$pattern_type %||% "gaussian"
    days <- 1:365
    
    spawn_pattern <- generate_reproduction_pattern(
      days = days,
      peak_day = repro$peak_day,
      duration = repro$duration,
      max_spawn_fraction = repro$max_spawn_fraction,
      pattern_type = pattern_type
    )
    
    mortality_params$spawn_pattern <- spawn_pattern
  }
  
  return(mortality_params)
}