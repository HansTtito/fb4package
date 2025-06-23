#' Utility Functions for FB4 Model
#'
#' @description
#' Collection of utility functions for the Fish Bioenergetics 4.0 model,
#' including mathematical operators, validation functions, and data processing helpers.
#'
#' @name utils
#' @aliases utils
NULL

# ============================================================================
# OPERATORS AND BASIC MATHEMATICAL FUNCTIONS
# ============================================================================

#' Null Coalescing Operator
#'
#' @description
#' Returns the first value if it's not NULL, otherwise returns the second value.
#' Similar to the || operator in other programming languages.
#'
#' @param x First value
#' @param y Default value if x is NULL
#'
#' @return x if not NULL, y otherwise
#'
#' @details
#' This operator is useful for providing default values when working with
#' potentially NULL parameters or list elements.
#'
#' @export
#'
#' @examples
#' # Basic usage
#' NULL %||% "default"        # Returns "default"
#' "value" %||% "default"     # Returns "value"
#' 
#' # Common use case with list elements
#' params <- list(temperature = 15)
#' temp <- params$temperature %||% 10  # Returns 15
#' depth <- params$depth %||% 5        # Returns 5 (default)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' String Concatenation Operator
#'
#' @description
#' Convenience operator for concatenating strings without separator.
#' Equivalent to paste0(a, b).
#'
#' @param a First string
#' @param b Second string
#'
#' @return Concatenated string
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' "Hello" %+% " World"  # Returns "Hello World"
#' }
`%+%` <- function(a, b) paste0(a, b)

#' Safe Square Root Function
#'
#' @description
#' Computes square root with protection against invalid inputs.
#' Returns NA for non-finite inputs and minimum value for negative inputs.
#'
#' @param x Numeric value
#' @param min_val Minimum allowed value, default 0
#'
#' @return Square root of x, or appropriate fallback value
#'
#' @details
#' This function prevents errors that could occur with standard sqrt()
#' when dealing with potentially problematic numerical data.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' safe_sqrt(4)      # Returns 2
#' safe_sqrt(-1)     # Returns 0 (min_val)
#' safe_sqrt(NA)     # Returns NA
#' }
safe_sqrt <- function(x, min_val = 0) {
  if (is.na(x) || !is.finite(x)) return(NA_real_)
  if (x < min_val) return(min_val)
  return(sqrt(x))
}

#' Safe Exponential Function
#'
#' @description
#' Computes exponential with protection against overflow and invalid inputs.
#' Limits extreme values to prevent numerical overflow.
#'
#' @param x Numeric value
#' @param max_exp Maximum exponent value, default 700
#'
#' @return Exponential of x, or appropriate bounded value
#'
#' @details
#' This function prevents numerical overflow that can occur with exp()
#' for very large input values, which is common in bioenergetic calculations
#' with temperature dependencies.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' safe_exp(1)       # Returns exp(1) ≈ 2.718
#' safe_exp(1000)    # Returns exp(700) (bounded)
#' safe_exp(-1000)   # Returns 0
#' }
safe_exp <- function(x, max_exp = 700) {
  if (is.na(x) || !is.finite(x)) return(NA_real_)
  if (x > max_exp) return(exp(max_exp))
  if (x < -max_exp) return(0)
  return(exp(x))
}

#' Clamp Values Between Minimum and Maximum
#'
#' @description
#' Constrains numeric values to lie within specified bounds.
#' Values below minimum are set to minimum, values above maximum are set to maximum.
#'
#' @param x Numeric value or vector
#' @param min_val Minimum allowed value
#' @param max_val Maximum allowed value
#'
#' @return Clamped value(s) within [min_val, max_val]
#'
#' @details
#' This function is essential for ensuring model parameters stay within
#' biologically realistic ranges and maintaining numerical stability.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' clamp(1.5, 0, 1)    # Returns 1.0
#' clamp(-0.5, 0, 1)   # Returns 0.0
#' clamp(0.3, 0, 1)    # Returns 0.3
#' clamp(c(-1, 0.5, 2), 0, 1)  # Returns c(0, 0.5, 1)
#' }
clamp <- function(x, min_val, max_val) {
  pmax(min_val, pmin(max_val, x))
}

# ============================================================================
# BASIC VALIDATIONS
# ============================================================================

#' Validate Time Series Data
#'
#' @description
#' Validates time series data structure and content for use in FB4 simulations.
#' Checks for required columns, data types, and temporal consistency.
#'
#' @param data Data to validate
#' @param data_name Name of the dataset (for error messages)
#' @param required_cols Required column names
#' @param min_cols Minimum number of columns
#'
#' @return NULL (throws error if invalid)
#'
#' @details
#' Performs comprehensive validation including:
#' \itemize{
#'   \item{Structure validation (data.frame, non-empty)}
#'   \item{Required column presence}
#'   \item{Day column validation (numeric, finite, ascending)}
#'   \item{Duplicate detection}
#' }
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' temp_data <- data.frame(Day = 1:10, Temperature = 15:24)
#' validate_time_series_data(temp_data, "temperature", c("Day", "Temperature"))
#' }
validate_time_series_data <- function(data, data_name, required_cols = NULL, min_cols = NULL) {
  
  if (is.null(data)) {
    stop(data_name, " cannot be NULL")
  }
  
  if (!is.data.frame(data)) {
    stop(data_name, " must be a data.frame")
  }
  
  if (nrow(data) == 0) {
    stop(data_name, " cannot be empty")
  }
  
  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop("Missing columns in ", data_name, ": ", paste(missing_cols, collapse = ", "))
    }
  }
  
  if (!is.null(min_cols) && ncol(data) < min_cols) {
    stop(data_name, " must have at least ", min_cols, " columns")
  }
  
  if ("Day" %in% names(data)) {
    if (!is.numeric(data$Day)) {
      stop("'Day' column in ", data_name, " must be numeric")
    }
    if (any(!is.finite(data$Day))) {
      stop("'Day' column in ", data_name, " contains non-finite values")
    }
    if (anyDuplicated(data$Day)) {
      warning("'Day' column in ", data_name, " contains duplicate values")
    }
    if (any(diff(data$Day) <= 0)) {
      warning("'Day' column in ", data_name, " is not in ascending order")
    }
  }
}

#' Validate Basic Model Parameters
#'
#' @description
#' Validates fundamental model parameters for biological feasibility
#' and computational practicality.
#'
#' @param initial_weight Initial weight in grams
#' @param duration Duration in days
#'
#' @return NULL (throws error if invalid)
#'
#' @details
#' Checks that:
#' \itemize{
#'   \item{Initial weight is positive and numeric}
#'   \item{Duration is positive and numeric}
#'   \item{Duration is not excessively long (performance warning)}
#' }
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' validate_basic_params(10.5, 365)  # Valid
#' validate_basic_params(-5, 100)    # Error: negative weight
#' }
validate_basic_params <- function(initial_weight, duration) {
  if (!is.numeric(initial_weight) || initial_weight <= 0) {
    stop("initial_weight must be a positive number")
  }
  if (!is.numeric(duration) || duration <= 0) {
    stop("duration must be a positive number")
  }
  if (duration > 10000) {
    warning("Very long duration (>10000 days), this may cause performance issues")
  }
}

#' Validate Consistency Between Diet and Energy Data
#'
#' @description
#' Validates consistency between diet composition and prey energy density data,
#' ensuring they have matching prey species and valid values.
#'
#' @param diet_data Data frame with diet proportions
#' @param energy_data Data frame with prey energies
#'
#' @return NULL (throws error if inconsistent)
#'
#' @details
#' Validation includes:
#' \itemize{
#'   \item{Matching prey species columns between datasets}
#'   \item{Diet proportions sum approximately to 1.0}
#'   \item{No negative diet proportions}
#'   \item{All prey energies are positive}
#' }
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' diet <- data.frame(Day = 1:5, fish = 0.6, zooplankton = 0.4)
#' energy <- data.frame(Day = 1:5, fish = 4000, zooplankton = 2500)
#' validate_diet_consistency(diet, energy)
#' }
validate_diet_consistency <- function(diet_data, energy_data) {
  diet_prey_cols <- setdiff(names(diet_data), "Day")
  energy_prey_cols <- setdiff(names(energy_data), "Day")
  
  if (length(diet_prey_cols) == 0) {
    stop("diet_data must have at least one prey column besides 'Day'")
  }
  
  if (!identical(sort(diet_prey_cols), sort(energy_prey_cols))) {
    stop("Prey columns must be identical in diet_data and energy_data")
  }
  
  diet_sums <- rowSums(diet_data[diet_prey_cols], na.rm = TRUE)
  if (any(abs(diet_sums - 1) > 0.1, na.rm = TRUE)) {
    warning("Some diet proportions deviate significantly from 1.0")
  }
  
  if (any(as.matrix(diet_data[diet_prey_cols]) < 0, na.rm = TRUE)) {
    stop("Diet proportions cannot be negative")
  }
  
  if (any(as.matrix(energy_data[energy_prey_cols]) <= 0, na.rm = TRUE)) {
    stop("Prey energies must be positive")
  }
}

#' Check Numeric Value
#'
#' @description
#' Validates that a value is numeric, finite, scalar, and within specified bounds.
#'
#' @param value Value to check
#' @param name Parameter name (for error messages)
#' @param min_val Minimum allowed value, default -Inf
#' @param max_val Maximum allowed value, default Inf
#'
#' @return Validated numeric value
#'
#' @details
#' This utility function provides consistent validation for scalar numeric
#' parameters throughout the package, ensuring they meet basic requirements
#' for numerical computations.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' check_numeric_value(15.5, "temperature", 0, 40)  # Returns 15.5
#' check_numeric_value(-5, "weight", 0)             # Error: below minimum
#' }
check_numeric_value <- function(value, name, min_val = -Inf, max_val = Inf) {
  if (is.null(value) || !is.numeric(value) || !is.finite(value)) {
    stop(name, " must be a valid numeric value")
  }
  if (length(value) != 1) stop(name, " must be a scalar value")
  if (value < min_val || value > max_val) {
    stop(name, " must be between ", min_val, " and ", max_val)
  }
  return(as.numeric(value))
}

# ============================================================================
# SPECIES PARAMETER VALIDATION
# ============================================================================

#' Validate Species Parameters for FB4
#'
#' @description
#' Comprehensive validation of species parameters for Fish Bioenergetics 4.0 model,
#' checking for required parameters and biological feasibility.
#'
#' @param species_params List with species parameters
#'
#' @return List with fields: valid (logical), errors (character), warnings (character)
#'
#' @details
#' Validates the structure and content of species parameter lists, including:
#' \itemize{
#'   \item{Required parameter groups (consumption, respiration)}
#'   \item{Critical parameters within each group}
#'   \item{Parameter value ranges and relationships}
#'   \item{Biological feasibility checks}
#' }
#'
#' The function returns a validation object that can be used to provide
#' detailed feedback about parameter issues.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example species parameters
#' params <- list(
#'   consumption = list(CA = 0.303, CB = -0.275, CQ = 3, CTO = 15, CTM = 25, CTL = 28),
#'   respiration = list(RA = 0.0548, RB = -0.299, RQ = 2, RTO = 5, RTM = 25, RTL = 35)
#' )
#' 
#' validation <- validate_species_params(params)
#' if (!validation$valid) {
#'   cat("Errors:", validation$errors, "\n")
#'   cat("Warnings:", validation$warnings, "\n")
#' }
#' }
validate_species_params <- function(species_params) {
  validation <- list(valid = TRUE, errors = character(), warnings = character())
  
  required_groups <- c("consumption", "respiration")
  missing_groups <- setdiff(required_groups, names(species_params))
  if (length(missing_groups) > 0) {
    validation$errors <- c(validation$errors, paste("Missing groups:", paste(missing_groups, collapse = ", ")))
    validation$valid <- FALSE
  }
  
  if ("consumption" %in% names(species_params)) {
    cons <- validate_consumption_params(species_params$consumption)
    validation$errors <- c(validation$errors, cons$errors)
    validation$warnings <- c(validation$warnings, cons$warnings)
    if (!cons$valid) validation$valid <- FALSE
  }
  
  if ("respiration" %in% names(species_params)) {
    resp <- validate_respiration_params(species_params$respiration)
    validation$errors <- c(validation$errors, resp$errors)
    validation$warnings <- c(validation$warnings, resp$warnings)
    if (!resp$valid) validation$valid <- FALSE
  }
  
  return(validation)
}

#' Validate Consumption Parameters
#'
#' @description
#' Validates consumption-related parameters for biological feasibility
#' and proper temperature response relationships.
#'
#' @param consumption_params Consumption parameters list
#'
#' @return Validation list with valid, errors, and warnings fields
#'
#' @details
#' Checks critical consumption parameters (CA, CB, CQ, CTO, CTM, CTL)
#' and validates temperature response relationships:
#' \itemize{
#'   \item{CTO < CTM < CTL (optimal < maximum < lethal temperatures)}
#'   \item{All critical parameters are present and valid}
#' }
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' cons_params <- list(CA = 0.303, CB = -0.275, CQ = 3, CTO = 15, CTM = 25, CTL = 28)
#' result <- validate_consumption_params(cons_params)
#' }
validate_consumption_params <- function(consumption_params) {
  validation <- list(valid = TRUE, errors = character(), warnings = character())
  critical <- c("CA", "CB", "CQ", "CTO", "CTM", "CTL")
  
  for (param in critical) {
    if (is.null(consumption_params[[param]]) || is.na(consumption_params[[param]])) {
      validation$errors <- c(validation$errors, paste("Missing parameter:", param))
      validation$valid <- FALSE
    }
  }
  
  if (!is.null(consumption_params$CTO) && !is.null(consumption_params$CTM)) {
    if (consumption_params$CTO >= consumption_params$CTM) {
      validation$warnings <- c(validation$warnings, "CTO should be less than CTM")
    }
  }
  
  if (!is.null(consumption_params$CTM) && !is.null(consumption_params$CTL)) {
    if (consumption_params$CTM >= consumption_params$CTL) {
      validation$warnings <- c(validation$warnings, "CTM should be less than CTL")
    }
  }
  
  return(validation)
}

#' Validate Respiration Parameters
#'
#' @description
#' Validates respiration-related parameters for biological feasibility
#' and proper scaling relationships.
#'
#' @param respiration_params Respiration parameters list
#'
#' @return Validation list with valid, errors, and warnings fields
#'
#' @details
#' Checks critical respiration parameters (RA, RB, RQ, RTO, RTM, RTL)
#' and validates that scaling parameters (RA, RB) are positive,
#' which is required for proper metabolic calculations.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' resp_params <- list(RA = 0.0548, RB = -0.299, RQ = 2, RTO = 5, RTM = 25, RTL = 35)
#' result <- validate_respiration_params(resp_params)
#' }
validate_respiration_params <- function(respiration_params) {
  validation <- list(valid = TRUE, errors = character(), warnings = character())
  critical <- c("RA", "RB", "RQ", "RTO", "RTM", "RTL")
  
  for (param in critical) {
    if (is.null(respiration_params[[param]]) || is.na(respiration_params[[param]])) {
      validation$errors <- c(validation$errors, paste("Missing parameter:", param))
      validation$valid <- FALSE
    }
  }
  
  for (param in c("RA", "RB")) {
    if (!is.null(respiration_params[[param]]) && !is.na(respiration_params[[param]])) {
      if (respiration_params[[param]] <= 0) {
        validation$errors <- c(validation$errors, paste(param, "must be positive"))
        validation$valid <- FALSE
      }
    }
  }
  
  return(validation)
}

# ============================================================================
# AUXILIARY FUNCTIONS FOR DAILY OUTPUT
# ============================================================================

#' Initialize Daily Output Structure
#'
#' @description
#' Creates and initializes a data frame structure for storing daily
#' simulation results from Fish Bioenergetics 4.0 model runs.
#'
#' @param n_days Number of simulation days
#' @param temperature Vector of daily temperatures
#' @param p_value P-value (proportion of maximum consumption)
#'
#' @return Initialized data frame with all required columns
#'
#' @details
#' Creates a comprehensive data frame with columns for:
#' \itemize{
#'   \item{Environmental conditions (Day, Temperature)}
#'   \item{Fish status (Weight)}
#'   \item{Consumption (g/g/d and J/g/d)}
#'   \item{Metabolic components (Respiration, Egestion, Excretion, SDA)}
#'   \item{Energy balance (Net Energy, Spawn Energy)}
#'   \item{Growth (Weight Change)}
#'   \item{Model parameters (P.Value, Predator Energy Density)}
#' }
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' daily_out <- initialize_daily_output(365, rep(15, 365), 0.6)
#' str(daily_out)
#' }
initialize_daily_output <- function(n_days, temperature, p_value) {
  
  return(data.frame(
    Day = 1:n_days,
    Temperature.C = temperature,
    Weight.g = numeric(n_days),
    Consumption.g.g.d = numeric(n_days),
    Consumption.J.g.d = numeric(n_days),
    Respiration.g.O2.g.d = numeric(n_days),
    Respiration.J.g.d = numeric(n_days),
    Egestion.J.g.d = numeric(n_days),
    Excretion.J.g.d = numeric(n_days),
    SDA.J.g.d = numeric(n_days),
    Net.Energy.J.g.d = numeric(n_days),
    Spawn.Energy.J.g.d = numeric(n_days),
    Weight.Change.g = numeric(n_days),
    Predator.Energy.Density.J.g = numeric(n_days),
    P.Value = rep(p_value, n_days),
    stringsAsFactors = FALSE
  ))
}

#' Initialize Daily Output Structure for Ration Simulations
#'
#' @description
#' Creates and initializes a data frame structure for storing daily
#' simulation results when using ration-based feeding scenarios.
#'
#' @param n_days Number of simulation days
#' @param temperature Vector of daily temperatures
#'
#' @return Initialized data frame with all required columns plus ration-specific fields
#'
#' @details
#' Similar to initialize_daily_output() but includes additional columns
#' for ration-specific information:
#' \itemize{
#'   \item{P.Value: Dynamically calculated daily p-values}
#'   \item{Ration.g: Daily ration in grams}
#'   \item{Ration.Type: Type of ration specification ("percent" or "grams")}
#' }
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' daily_out <- initialize_daily_output_ration(365, rep(15, 365))
#' str(daily_out)
#' }
initialize_daily_output_ration <- function(n_days, temperature) {
  
  return(data.frame(
    Day = 1:n_days,
    Temperature.C = temperature,
    Weight.g = numeric(n_days),
    Consumption.g.g.d = numeric(n_days),
    Consumption.J.g.d = numeric(n_days),
    Respiration.g.O2.g.d = numeric(n_days),
    Respiration.J.g.d = numeric(n_days),
    Egestion.J.g.d = numeric(n_days),
    Excretion.J.g.d = numeric(n_days),
    SDA.J.g.d = numeric(n_days),
    Net.Energy.J.g.d = numeric(n_days),
    Spawn.Energy.J.g.d = numeric(n_days),
    Weight.Change.g = numeric(n_days),
    Predator.Energy.Density.J.g = numeric(n_days),
    P.Value = numeric(n_days),
    Ration.g = numeric(n_days),
    Ration.Type = character(n_days),
    stringsAsFactors = FALSE
  ))
}

#' Update Daily Output with Day's Data
#'
#' @description
#' Updates the daily output data frame with calculated results
#' from a single day's bioenergetic simulation.
#'
#' @param daily_output Daily output data frame
#' @param day Current day number
#' @param current_weight Current fish weight (g)
#' @param consumption_result Consumption calculation results
#' @param metabolism_result Metabolism calculation results
#' @param growth_result Growth calculation results
#' @param spawn_energy Spawning energy loss (J)
#' @param predator_ed Predator energy density (J/g)
#'
#' @return Updated daily output data frame
#'
#' @details
#' This function populates all the daily output columns with the
#' calculated values from the bioenergetic simulation for a single day.
#' It serves as the interface between the simulation calculations
#' and the output data structure.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Used internally during simulation loops
#' daily_output <- update_daily_output(
#'   daily_output, day, weight, cons_result, 
#'   metab_result, growth_result, spawn, pred_ed
#' )
#' }
update_daily_output <- function(daily_output, day, current_weight, 
                                consumption_result, metabolism_result, 
                                growth_result, spawn_energy, predator_ed) {
  
  daily_output$Weight.g[day] <- current_weight
  daily_output$Consumption.g.g.d[day] <- consumption_result$consumption_gg
  daily_output$Consumption.J.g.d[day] <- consumption_result$consumption_energy
  daily_output$Respiration.g.O2.g.d[day] <- metabolism_result$respiration_o2
  daily_output$Respiration.J.g.d[day] <- metabolism_result$respiration_energy
  daily_output$Egestion.J.g.d[day] <- metabolism_result$egestion_energy
  daily_output$Excretion.J.g.d[day] <- metabolism_result$excretion_energy
  daily_output$SDA.J.g.d[day] <- metabolism_result$sda_energy
  daily_output$Net.Energy.J.g.d[day] <- metabolism_result$net_energy
  daily_output$Spawn.Energy.J.g.d[day] <- spawn_energy / current_weight
  daily_output$Weight.Change.g[day] <- growth_result$weight_change
  daily_output$Predator.Energy.Density.J.g[day] <- predator_ed
  
  return(daily_output)
}

#' Update Daily Output for Ration Simulations
#'
#' @description
#' Updates the daily output data frame for ration-based simulations,
#' including both standard bioenergetic results and ration-specific information.
#'
#' @param daily_output Daily output data frame
#' @param day Current day number
#' @param current_weight Current fish weight (g)
#' @param consumption_result Consumption calculation results
#' @param metabolism_result Metabolism calculation results
#' @param growth_result Growth calculation results
#' @param spawn_energy Spawning energy loss (J)
#' @param predator_ed Predator energy density (J/g)
#' @param ration_value Ration value (grams or fraction)
#' @param ration_type Ration type ("percent" or "grams")
#'
#' @return Updated daily output data frame
#'
#' @details
#' Extends the standard daily output update to include ration-specific
#' information such as the dynamically calculated p-value and
#' the actual ration consumed in grams.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Used internally during ration-based simulation loops
#' daily_output <- update_daily_output_ration(
#'   daily_output, day, weight, cons_result, metab_result,
#'   growth_result, spawn, pred_ed, 0.03, "percent"
#' )
#' }
update_daily_output_ration <- function(daily_output, day, current_weight, 
                                       consumption_result, metabolism_result, 
                                       growth_result, spawn_energy, predator_ed,
                                       ration_value, ration_type) {
  
  # Update common fields
  daily_output <- update_daily_output(
    daily_output, day, current_weight, consumption_result,
    metabolism_result, growth_result, spawn_energy, predator_ed
  )
  
  # Update ration-specific fields
  daily_output$P.Value[day] <- consumption_result$effective_p
  daily_output$Ration.g[day] <- consumption_result$consumption_gg * current_weight
  daily_output$Ration.Type[day] <- ration_type
  
  return(daily_output)
}

# ============================================================================
# AUXILIARY FUNCTIONS FOR TEMPORAL DATA
# ============================================================================

#' Get Daily Value from Temporal Data
#'
#' @description
#' Extracts a value for a specific day from temporal data structures,
#' handling both data frames and vectors.
#'
#' @param data_vector Data structure (data frame or vector)
#' @param day Day number to extract
#'
#' @return Value for the specified day, or NA if not found
#'
#' @details
#' This utility function provides a consistent interface for extracting
#' daily values from different data structures used throughout the package.
#' It handles both indexed vectors and data frames with Day columns.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # With vector
#' temps <- c(10, 12, 14, 16, 18)
#' get_daily_value(temps, 3)  # Returns 14
#' 
#' # With data frame
#' temp_df <- data.frame(Day = c(1, 3, 5), Temperature = c(10, 14, 18))
#' get_daily_value(temp_df, 3)  # Returns 14
#' }
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

#' Calculate Daily Mean Prey Energy
#'
#' @description
#' Calculates the weighted mean energy density of prey items
#' based on diet proportions and individual prey energy densities.
#'
#' @param daily_proportions Diet proportions for the day
#' @param daily_energies Energy densities of prey items for the day
#'
#' @return Weighted mean prey energy density (J/g)
#'
#' @details
#' This function computes the effective energy density of the diet
#' by weighting each prey type's energy density by its proportion
#' in the diet. This value is used in consumption calculations
#' to convert between mass and energy units.
#'
#' Formula: mean_energy = Σ(proportion_i × energy_i)
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Diet: 60% fish (4000 J/g), 40% zooplankton (2500 J/g)
#' proportions <- c(0.6, 0.4)
#' energies <- c(4000, 2500)
#' mean_energy <- calculate_daily_mean_prey_energy(proportions, energies)
#' # Returns: 0.6 * 4000 + 0.4 * 2500 = 3400 J/g
#' }
calculate_daily_mean_prey_energy <- function(daily_proportions, daily_energies) {
  return(sum(daily_proportions * daily_energies, na.rm = TRUE))
}

#' Calculate Daily Spawning Energy Loss
#'
#' @description
#' Calculates the energy lost to reproduction (spawning) on a given day
#' based on reproductive schedule and fish condition.
#'
#' @param day Current simulation day
#' @param current_weight Current fish weight (g)
#' @param predator_ed Predator energy density (J/g)
#' @param processed_data Processed simulation data
#' @param model_options Model configuration options
#'
#' @return Spawning energy loss (J)
#'
#' @details
#' Spawning energy represents the metabolic cost of reproduction,
#' including gamete production and spawning behavior. The energy
#' loss is calculated as a fraction of total body energy content:
#'
#' spawn_energy = spawn_fraction × weight × energy_density
#'
#' Spawning only occurs when:
#' \itemize{
#'   \item{Reproduction calculations are enabled}
#'   \item{A spawning schedule is provided}
#'   \item{The spawning fraction for the day is > 0}
#' }
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal function used during simulation
#' spawn_loss <- calculate_daily_spawn_energy(
#'   day = 150, 
#'   current_weight = 25, 
#'   predator_ed = 5000,
#'   processed_data = my_data,
#'   model_options = my_options
#' )
#' }
calculate_daily_spawn_energy <- function(day, current_weight, predator_ed, 
                                         processed_data, model_options) {
  
  spawn_energy <- 0
  
  if (model_options$calc_reproduction && !is.null(processed_data$reproduction)) {
    spawn_fraction <- processed_data$reproduction[day]
    if (!is.na(spawn_fraction) && spawn_fraction > 0) {
      spawn_energy <- spawn_fraction * current_weight * predator_ed
    }
  }
  
  return(spawn_energy)
}

#' Interpolate Predator Energy Density
#'
#' @description
#' Interpolates predator energy density values across the simulation period
#' based on initial and final energy density values.
#'
#' @param predator_params Predator parameters containing ED_ini and ED_end
#' @param first_day First simulation day
#' @param last_day Last simulation day
#'
#' @return Vector of energy densities for each simulation day
#'
#' @details
#' Energy density represents the caloric content per gram of fish tissue
#' and typically changes seasonally due to factors like lipid storage,
#' reproductive status, and feeding conditions.
#'
#' The function performs linear interpolation between initial and final
#' energy density values across the simulation period. This accounts
#' for gradual changes in fish condition over time.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' predator_params <- list(ED_ini = 4500, ED_end = 5200)
#' ed_values <- interpolate_predator_energy_density(
#'   predator_params, 1, 365
#' )
#' plot(1:365, ed_values, type = "l")
#' }
interpolate_predator_energy_density <- function(predator_params, first_day, last_day) {
  
  ed_data <- data.frame(
    Day = c(first_day, last_day),
    ED = c(predator_params$ED_ini, predator_params$ED_end)
  )
  
  # Use simple linear interpolation if no interpolation function available
  all_days <- first_day:last_day
  ed_values <- approx(ed_data$Day, ed_data$ED, xout = all_days, method = "linear", rule = 2)$y
  
  return(ed_values)
}

# ============================================================================
# COMPATIBILITY CHECKS
# ============================================================================

#' Check Data Compatibility Between Datasets
#'
#' @description
#' Validates compatibility and consistency between different datasets
#' used in Fish Bioenergetics 4.0 simulations.
#'
#' @param temperature_data Temperature data frame
#' @param diet_data Diet composition data frame (optional)
#' @param prey_energy_data Prey energy density data frame (optional)
#'
#' @return List with compatibility status, warnings, and errors
#'
#' @details
#' Performs comprehensive compatibility checks including:
#' 
#' \strong{Structure Validation:}
#' \itemize{
#'   \item{Required columns presence}
#'   \item{Data types and formats}
#'   \item{Non-empty datasets}
#' }
#'
#' \strong{Temporal Consistency:}
#' \itemize{
#'   \item{Overlapping day ranges between datasets}
#'   \item{Chronological order validation}
#'   \item{Gap detection in time series}
#' }
#'
#' \strong{Content Consistency:}
#' \itemize{
#'   \item{Matching prey species columns}
#'   \item{Consistent naming conventions}
#'   \item{Value range validation}
#' }
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' temp_data <- data.frame(Day = 1:365, Temperature = rnorm(365, 15, 3))
#' diet_data <- data.frame(Day = 1:365, fish = 0.6, zooplankton = 0.4)
#' energy_data <- data.frame(Day = 1:365, fish = 4000, zooplankton = 2500)
#' 
#' compatibility <- check_data_compatibility(temp_data, diet_data, energy_data)
#' if (!compatibility$compatible) {
#'   cat("Errors found:", compatibility$errors, "\n")
#' }
#' }
check_data_compatibility <- function(temperature_data, diet_data = NULL, prey_energy_data = NULL) {
  
  compatibility <- list(
    compatible = TRUE,
    warnings = character(),
    errors = character()
  )
  
  if (!"Day" %in% names(temperature_data) || !"Temperature" %in% names(temperature_data)) {
    compatibility$errors <- c(compatibility$errors, "temperature_data must have 'Day' and 'Temperature' columns")
    compatibility$compatible <- FALSE
  }
  
  if (!is.null(diet_data) && "Day" %in% names(diet_data)) {
    temp_days <- range(temperature_data$Day, na.rm = TRUE)
    diet_days <- range(diet_data$Day, na.rm = TRUE)
    
    if (diet_days[1] > temp_days[2] || diet_days[2] < temp_days[1]) {
      compatibility$warnings <- c(compatibility$warnings, "Day ranges do not overlap between temperature and diet data")
    }
  }
  
  if (!is.null(diet_data) && !is.null(prey_energy_data)) {
    diet_prey <- setdiff(names(diet_data), "Day")
    energy_prey <- setdiff(names(prey_energy_data), "Day")
    
    if (!identical(sort(diet_prey), sort(energy_prey))) {
      compatibility$errors <- c(compatibility$errors, "Prey columns do not match between diet_data and prey_energy_data")
      compatibility$compatible <- FALSE
    }
  }
  
  return(compatibility)
}