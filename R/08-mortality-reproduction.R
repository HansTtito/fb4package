#' Mortality and Reproduction Functions for FB4 Model
#'
#' @name mortality-reproduction
#' @aliases mortality-reproduction
NULL

# ============================================================================
# LOW-LEVEL FUNCTIONS
# ============================================================================

#' Calculate combined daily survival (Low-level)
#'
#' Calculates survival considering multiple mortality sources
#'
#' @param mortality_rates Vector of mortality rates by source (daily fraction 0-1)
#' @param method Combination method ("independent", "additive")
#' @return Combined survival rate
#' @keywords internal
calculate_combined_survival <- function(mortality_rates, method = "independent") {
  
  # Validate inputs
  mortality_rates <- mortality_rates[!is.na(mortality_rates)]
  
  if (any(mortality_rates < 0 | mortality_rates > 1)) {
    warning("Mortality rates outside range [0,1], correcting")
    mortality_rates <- clamp(mortality_rates, 0, 1)
  }
  
  if (method == "independent") {
    # Independent survival: S = Π(1 - mi)
    survival_rate <- prod(1 - mortality_rates)
  } else if (method == "additive") {
    # Additive mortality: M = Σmi, S = 1 - M
    combined_mortality <- sum(mortality_rates)
    survival_rate <- max(0, 1 - combined_mortality)
  } else {
    stop("method must be 'independent' or 'additive'")
  }
  
  return(clamp(survival_rate, 0, 1))
}

#' Calculate weight-dependent mortality (Low-level)
#'
#' Adjusts mortality based on current fish weight
#'
#' @param current_weight Current weight (g)
#' @param base_mortality Base daily mortality rate
#' @param weight_threshold Weight threshold below which mortality increases
#' @param starvation_factor Multiplication factor for starvation
#' @param initial_weight Initial weight for comparison (optional)
#' @return Adjusted mortality rate
#' @keywords internal
calculate_weight_dependent_mortality <- function(current_weight, base_mortality,
                                                 weight_threshold = 0,
                                                 starvation_factor = 5,
                                                 initial_weight = NULL) {
  
  mortality_rate <- base_mortality
  
  # Mortality due to low absolute weight
  if (weight_threshold > 0 && current_weight < weight_threshold) {
    weight_ratio <- current_weight / weight_threshold
    starvation_effect <- starvation_factor * (1 - weight_ratio)
    mortality_rate <- mortality_rate + starvation_effect * base_mortality
  }
  
  # Mortality due to relative weight loss
  if (!is.null(initial_weight) && initial_weight > 0) {
    weight_loss_fraction <- 1 - (current_weight / initial_weight)
    if (weight_loss_fraction > 0.5) {  # More than 50% weight loss
      severe_loss_effect <- 2 * (weight_loss_fraction - 0.5)
      mortality_rate <- mortality_rate + severe_loss_effect * base_mortality
    }
  }
  
  return(clamp(mortality_rate, 0, 0.99))
}

#' Calculate temperature-dependent mortality (Low-level)
#'
#' Adjusts mortality based on thermal stress
#'
#' @param temperature Current temperature (°C)
#' @param base_mortality Base daily mortality rate
#' @param optimal_temp Optimal temperature (°C)
#' @param thermal_tolerance Thermal tolerance range (°C)
#' @param stress_factor Multiplication factor for thermal stress
#' @return Adjusted mortality rate
#' @keywords internal
calculate_temperature_dependent_mortality <- function(temperature, base_mortality,
                                                      optimal_temp = 20,
                                                      thermal_tolerance = 10,
                                                      stress_factor = 2) {
  
  temp_deviation <- abs(temperature - optimal_temp)
  
  if (temp_deviation > thermal_tolerance) {
    thermal_stress <- (temp_deviation - thermal_tolerance) / thermal_tolerance
    stress_mortality <- stress_factor * thermal_stress * base_mortality
    total_mortality <- base_mortality + stress_mortality
  } else {
    total_mortality <- base_mortality
  }
  
  return(clamp(total_mortality, 0, 0.99))
}

#' Calculate reproductive weight loss (Low-level)
#'
#' Calculates weight and energy loss during reproductive events
#'
#' @param spawn_fraction Fraction of weight lost in reproduction (0-1)
#' @param current_weight Current fish weight (g)
#' @param energy_density Energy density of reproductive tissue (J/g)
#' @return List with weight and energy losses
#' @keywords internal
calculate_reproductive_loss <- function(spawn_fraction, current_weight, energy_density = 5000) {
  
  # Validate inputs
  spawn_fraction <- clamp(spawn_fraction, 0, 1)
  current_weight <- check_numeric_value(current_weight, "current_weight", min_val = 0.001)
  energy_density <- check_numeric_value(energy_density, "energy_density", min_val = 100)
  
  # Calculations
  weight_loss <- spawn_fraction * current_weight
  energy_loss <- weight_loss * energy_density
  
  return(list(
    weight_loss = weight_loss,
    energy_loss = energy_loss,
    spawn_fraction = spawn_fraction,
    remaining_weight = current_weight - weight_loss
  ))
}

#' Generate seasonal reproduction pattern (Low-level)
#'
#' Creates a simplified seasonal reproduction pattern
#'
#' @param days Vector of days of year
#' @param peak_day Day of reproductive peak
#' @param duration Duration of reproductive period (days)
#' @param max_spawn_fraction Maximum fraction of weight lost
#' @param pattern_type Pattern type ("gaussian", "uniform", "pulse")
#' @return Vector with reproductive fractions by day
#' @keywords internal
generate_reproduction_pattern <- function(days, peak_day, duration,
                                          max_spawn_fraction = 0.15,
                                          pattern_type = "gaussian") {
  
  n_days <- length(days)
  spawn_fractions <- rep(0, n_days)
  
  if (pattern_type == "gaussian") {
    # Gaussian pattern centered on peak_day
    for (i in seq_along(days)) {
      day <- days[i]
      
      # Distance to peak (considering year circularity)
      dist_to_peak <- min(abs(day - peak_day), 365 - abs(day - peak_day))
      
      if (dist_to_peak <= duration/2) {
        # Gaussian function
        sigma <- duration / 4  # Standard deviation
        spawn_fractions[i] <- max_spawn_fraction * safe_exp(-0.5 * (dist_to_peak / sigma)^2)
      }
    }
    
  } else if (pattern_type == "uniform") {
    # Uniform pattern during period
    start_day <- peak_day - duration/2
    end_day <- peak_day + duration/2
    
    for (i in seq_along(days)) {
      day <- days[i]
      
      # Check if in reproductive period (considering circularity)
      in_season <- FALSE
      if (start_day >= 1 && end_day <= 365) {
        in_season <- day >= start_day && day <= end_day
      } else {
        # Handle cases where period crosses year
        if (start_day < 1) {
          in_season <- day >= (start_day + 365) || day <= end_day
        } else if (end_day > 365) {
          in_season <- day >= start_day || day <= (end_day - 365)
        }
      }
      
      if (in_season) {
        spawn_fractions[i] <- max_spawn_fraction / duration
      }
    }
    
  } else if (pattern_type == "pulse") {
    # Punctual spawning event
    closest_day_index <- which.min(abs(days - peak_day))
    if (length(closest_day_index) > 0) {
      spawn_fractions[closest_day_index] <- max_spawn_fraction
    }
  }
  
  return(pmax(0, spawn_fractions))
}

# ============================================================================
# MID-LEVEL FUNCTIONS: Coordination and Business Logic
# ============================================================================

#' Calculate daily mortality and reproduction (Mid-level - Main function)
#'
#' Main function for calculating mortality and reproduction effects
#'
#' @param current_weight Current fish weight (g)
#' @param temperature Water temperature (°C)
#' @param day_of_year Day of year (1-365)
#' @param mortality_params List with mortality parameters
#' @param reproduction_params List with reproduction parameters (optional)
#' @param initial_weight Initial weight for relative calculations (optional)
#' @return List with mortality and reproduction results
#' @export
calculate_mortality_reproduction <- function(current_weight, temperature, day_of_year,
                                             mortality_params, reproduction_params = NULL,
                                             initial_weight = NULL) {
  
  # Basic validations
  current_weight <- check_numeric_value(current_weight, "current_weight", min_val = 0.001)
  temperature <- check_numeric_value(temperature, "temperature", min_val = -5, max_val = 50)
  day_of_year <- check_numeric_value(day_of_year, "day_of_year", min_val = 1, max_val = 365)
  
  if (is.null(mortality_params)) {
    stop("mortality_params cannot be NULL")
  }
  
  # Extract mortality parameters with default values
  base_mortality <- mortality_params$base_mortality %||% 0.001
  natural_mortality <- mortality_params$natural_mortality %||% base_mortality
  fishing_mortality <- mortality_params$fishing_mortality %||% 0
  predation_mortality <- mortality_params$predation_mortality %||% 0
  
  # Dependency parameters
  weight_threshold <- mortality_params$weight_threshold %||% 0
  starvation_factor <- mortality_params$starvation_factor %||% 5
  optimal_temp <- mortality_params$optimal_temp %||% 20
  thermal_tolerance <- mortality_params$thermal_tolerance %||% 10
  stress_factor <- mortality_params$stress_factor %||% 2
  
  # Calculate weight-dependent mortality
  weight_adjusted_mortality <- calculate_weight_dependent_mortality(
    current_weight = current_weight,
    base_mortality = natural_mortality,
    weight_threshold = weight_threshold,
    starvation_factor = starvation_factor,
    initial_weight = initial_weight
  )
  
  # Calculate temperature-dependent mortality
  temp_adjusted_mortality <- calculate_temperature_dependent_mortality(
    temperature = temperature,
    base_mortality = weight_adjusted_mortality,
    optimal_temp = optimal_temp,
    thermal_tolerance = thermal_tolerance,
    stress_factor = stress_factor
  )
  
  # Combine all mortality sources
  all_mortality_rates <- c(
    natural = temp_adjusted_mortality,
    fishing = fishing_mortality,
    predation = predation_mortality
  )
  
  # Calculate combined survival
  survival_rate <- calculate_combined_survival(all_mortality_rates, method = "independent")
  combined_mortality <- 1 - survival_rate
  
  # Mortality result
  mortality_result <- list(
    survival_rate = survival_rate,
    combined_mortality = combined_mortality,
    natural_mortality = temp_adjusted_mortality,
    fishing_mortality = fishing_mortality,
    predation_mortality = predation_mortality,
    weight_effect = weight_adjusted_mortality - natural_mortality,
    temperature_effect = temp_adjusted_mortality - weight_adjusted_mortality
  )
  
  # Calculate reproduction if specified
  reproduction_result <- process_reproduction(
    reproduction_params = reproduction_params,
    current_weight = current_weight,
    day_of_year = day_of_year
  )
  
  return(list(
    mortality = mortality_result,
    reproduction = reproduction_result,
    day_of_year = day_of_year,
    current_weight = current_weight,
    temperature = temperature
  ))
}

# ============================================================================
# UTILITY FUNCTIONS: Parameter Calculations and Validation
# ============================================================================

#' Process reproduction parameters (Utility)
#'
#' Handles reproduction calculations if parameters are provided
#'
#' @param reproduction_params List with reproduction parameters (optional)
#' @param current_weight Current fish weight (g)
#' @param day_of_year Day of year (1-365)
#' @return List with reproduction results or NULL
#' @keywords internal
process_reproduction <- function(reproduction_params, current_weight, day_of_year) {
  
  if (is.null(reproduction_params)) {
    return(NULL)
  }
  
  # Extract reproductive parameters
  spawn_pattern <- reproduction_params$spawn_pattern %||% rep(0, 365)
  energy_density <- reproduction_params$energy_density %||% 5000
  
  # Get reproductive fraction for current day
  if (length(spawn_pattern) >= day_of_year) {
    spawn_fraction <- spawn_pattern[day_of_year]
  } else {
    spawn_fraction <- 0
  }
  
  # Calculate reproductive losses if spawning occurs
  if (spawn_fraction > 0) {
    reproduction_result <- calculate_reproductive_loss(
      spawn_fraction = spawn_fraction,
      current_weight = current_weight,
      energy_density = energy_density
    )
  } else {
    reproduction_result <- list(
      weight_loss = 0,
      energy_loss = 0,
      spawn_fraction = 0,
      remaining_weight = current_weight
    )
  }
  
  return(reproduction_result)
}

#' Calculate cumulative survival (Utility)
#'
#' Calculates survival across multiple periods
#'
#' @param survival_rates Vector of daily survival rates
#' @param initial_population Initial population
#' @return Vector with surviving population by day
#' @export
calculate_cumulative_survival <- function(survival_rates, initial_population = 1) {
  
  n_days <- length(survival_rates)
  population <- numeric(n_days)
  population[1] <- initial_population
  
  for (i in 2:n_days) {
    population[i] <- population[i-1] * survival_rates[i-1]
  }
  
  return(population)
}

#' Validate mortality and reproduction parameters (Utility)
#'
#' Verifies that parameters are within realistic ranges
#'
#' @param mortality_params List with mortality parameters
#' @param reproduction_params List with reproduction parameters (optional)
#' @param species_type Species type for validation
#' @return List with validation results
#' @export
validate_mortality_reproduction_params <- function(mortality_params, reproduction_params = NULL,
                                                   species_type = "general") {
  
  validation <- list(
    valid = TRUE,
    warnings = character(),
    errors = character()
  )
  
  # Typical ranges by species type (daily mortality)
  typical_mortality_range <- switch(species_type,
                                    "small_fish" = c(0.0001, 0.01),     # 0.01% - 1% daily
                                    "large_fish" = c(0.00001, 0.005),   # 0.001% - 0.5% daily
                                    "general" = c(0.00001, 0.01)        # 0.001% - 1% daily
  )
  
  # Validate mortality parameters
  validation <- validate_mortality_parameters(mortality_params, typical_mortality_range, 
                                              species_type, validation)
  
  # Validate reproduction parameters
  if (!is.null(reproduction_params)) {
    validation <- validate_reproduction_parameters(reproduction_params, validation)
  }
  
  return(validation)
}

#' Validate mortality parameters (Utility)
#'
#' Internal function to validate mortality-specific parameters
#'
#' @param mortality_params List with mortality parameters
#' @param typical_mortality_range Typical mortality range for species
#' @param species_type Species type
#' @param validation Current validation state
#' @return Updated validation list
#' @keywords internal
validate_mortality_parameters <- function(mortality_params, typical_mortality_range, 
                                          species_type, validation) {
  
  if (!is.null(mortality_params)) {
    
    # Validate base mortality
    if ("base_mortality" %in% names(mortality_params)) {
      base_mort <- mortality_params$base_mortality
      if (base_mort < 0 || base_mort > 1) {
        validation$errors <- c(validation$errors, "base_mortality must be between 0 and 1")
        validation$valid <- FALSE
      }
      
      if (base_mort < typical_mortality_range[1] || base_mort > typical_mortality_range[2]) {
        validation$warnings <- c(validation$warnings, 
                                 paste("base_mortality outside typical range for", species_type))
      }
    }
    
    # Validate other mortality types
    mortality_types <- c("natural_mortality", "fishing_mortality", "predation_mortality")
    for (mort_type in mortality_types) {
      if (mort_type %in% names(mortality_params)) {
        mort_value <- mortality_params[[mort_type]]
        if (mort_value < 0 || mort_value > 1) {
          validation$errors <- c(validation$errors, 
                                 paste(mort_type, "must be between 0 and 1"))
          validation$valid <- FALSE
        }
      }
    }
    
    # Validate temperature parameters
    if ("optimal_temp" %in% names(mortality_params)) {
      opt_temp <- mortality_params$optimal_temp
      if (opt_temp < -5 || opt_temp > 40) {
        validation$warnings <- c(validation$warnings, 
                                 "optimal_temp outside typical range (-5 to 40°C)")
      }
    }
    
    if ("thermal_tolerance" %in% names(mortality_params)) {
      tolerance <- mortality_params$thermal_tolerance
      if (tolerance <= 0) {
        validation$errors <- c(validation$errors, "thermal_tolerance must be positive")
        validation$valid <- FALSE
      }
    }
  }
  
  return(validation)
}

#' Validate reproduction parameters (Utility)
#'
#' Internal function to validate reproduction-specific parameters
#'
#' @param reproduction_params List with reproduction parameters
#' @param validation Current validation state
#' @return Updated validation list
#' @keywords internal
validate_reproduction_parameters <- function(reproduction_params, validation) {
  
  if ("spawn_pattern" %in% names(reproduction_params)) {
    spawn_pattern <- reproduction_params$spawn_pattern
    
    if (any(spawn_pattern < 0 | spawn_pattern > 1, na.rm = TRUE)) {
      validation$errors <- c(validation$errors, 
                             "spawn_pattern must be between 0 and 1")
      validation$valid <- FALSE
    }
    
    # Check total annual reproduction
    total_annual_spawn <- sum(spawn_pattern, na.rm = TRUE)
    if (total_annual_spawn > 0.5) {
      validation$warnings <- c(validation$warnings, 
                               "Total annual reproduction very high (>50% of weight)")
    }
  }
  
  if ("energy_density" %in% names(reproduction_params)) {
    energy_density <- reproduction_params$energy_density
    if (energy_density <= 0) {
      validation$errors <- c(validation$errors, "energy_density must be positive")
      validation$valid <- FALSE
    }
    
    if (energy_density < 1000 || energy_density > 20000) {
      validation$warnings <- c(validation$warnings, 
                               "energy_density outside typical range (1000-20000 J/g)")
    }
  }
  
  return(validation)
}

