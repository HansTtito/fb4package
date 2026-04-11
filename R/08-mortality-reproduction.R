#' Mortality and Reproduction Functions for FB4 Model
#'
#' @description
#' Experimental functions for computing daily fish mortality and reproductive
#' energy costs in the FB4 framework. Mortality is decomposed into natural,
#' fishing, and predation components, with optional adjustments for thermal
#' stress and starvation (weight-dependent). Reproduction is modelled as a
#' seasonal spawn-fraction pattern that removes a fraction of body weight
#' (and its associated energy) on each spawning day.
#'
#' \strong{Note}: Mortality rate tracking is not yet integrated into the main
#' \code{run_fb4()} loop; spawning energy loss is.
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
#' @return No return value; this page documents the mortality and reproduction functions module. See individual function documentation for return values.
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
  
  if (method == "independent") {
    # Independent survival computed as the product of (1 - mortality values)
    survival_rate <- prod(1 - mortality_rates, na.rm = TRUE)
  } else {
    # Additive mortality: total mortality is sum of mortalities; survival is 1 minus total mortality
    combined_mortality <- sum(mortality_rates, na.rm = TRUE)
    survival_rate <- 1 - combined_mortality
  }
  
  return(survival_rate)
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
                                                 weight_threshold,
                                                 starvation_factor,
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
  
  return(mortality_rate)
}


#' Calculate temperature-dependent mortality (Low-level)
#'
#' Adjusts mortality based on thermal stress
#'
#' @param temperature Current temperature (deg C)
#' @param base_mortality Base daily mortality rate
#' @param optimal_temp Optimal temperature (deg C)
#' @param thermal_tolerance Thermal tolerance range (deg C)
#' @param stress_factor Multiplication factor for thermal stress
#' @return Adjusted mortality rate
#' @keywords internal
calculate_temperature_dependent_mortality <- function(temperature, base_mortality,
                                                      optimal_temp,
                                                      thermal_tolerance,
                                                      stress_factor) {
  
  temp_deviation <- abs(temperature - optimal_temp)
  
  if (temp_deviation > thermal_tolerance) {
    thermal_stress <- (temp_deviation - thermal_tolerance) / thermal_tolerance
    stress_mortality <- stress_factor * thermal_stress * base_mortality
    total_mortality <- base_mortality + stress_mortality
  } else {
    total_mortality <- base_mortality
  }
  
  return(total_mortality)
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
calculate_reproductive_loss <- function(spawn_fraction, current_weight, energy_density) {
  
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
                                          max_spawn_fraction,
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
        spawn_fractions[i] <- max_spawn_fraction * safe_exp(-0.5 * (dist_to_peak / sigma)^2, 
                                                            param_name = "Reproduction pattern")
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
  } else {
    stop("generate_reproduction_pattern: unrecognized pattern_type '", pattern_type,
         "'. Must be \"gaussian\", \"uniform\", or \"pulse\".", call. = FALSE)
  }
  
  return(spawn_fractions)
}

#' Calculate spawning energy loss (Low-level)
#'
#' Calculates energy lost to reproduction on a given day
#'
#' @param spawn_fraction Fraction of weight lost in reproduction (0-1)
#' @param current_weight Current fish weight (g)
#' @param energy_density Energy density of reproductive tissue (J/g)
#' @return Spawning energy loss (J)
#' @keywords internal
calculate_spawn_energy <- function(spawn_fraction, current_weight, energy_density) {
  spawn_energy <- spawn_fraction * current_weight * energy_density
  return(spawn_energy)
}


# ============================================================================
# MID-LEVEL FUNCTIONS: Coordination and Business Logic
# ============================================================================

#' Calculate daily mortality and reproduction (Mid-level - Main function)
#'
#' @description
#' Calculates daily mortality probability and reproductive energy loss
#' (natural mortality, fishing mortality, predation mortality, starvation,
#' and weight-dependent survival) for a single time step.
#'
#' @param current_weight Current fish weight (g)
#' @param temperature Water temperature (deg C)
#' @param day_of_year Day of year (1-365)
#' @param processed_mortality_params List with processed mortality parameters
#' @param initial_weight Initial weight for relative calculations (optional)
#' @return A named list with five elements:
#'   \describe{
#'     \item{mortality}{Named list with seven numeric scalars (all daily
#'       rates, 0--1): \code{survival_rate}, \code{combined_mortality},
#'       \code{natural_mortality}, \code{fishing_mortality},
#'       \code{predation_mortality}, \code{weight_effect} (increment to
#'       mortality due to low body weight), and \code{temperature_effect}
#'       (increment due to thermal stress).}
#'     \item{reproduction}{\code{NULL} if no spawn pattern is defined in
#'       \code{processed_mortality_params}; otherwise a named list with
#'       \code{weight_loss} (g), \code{energy_loss} (J),
#'       \code{spawn_fraction} (0--1), and \code{remaining_weight} (g).}
#'     \item{day_of_year}{Integer. Day of year, as supplied.}
#'     \item{current_weight}{Numeric. Fish weight (g), as supplied.}
#'     \item{temperature}{Numeric. Water temperature (\eqn{^\circ}C), as
#'       supplied.}
#'   }
#'
#' @section Experimental:
#' Mortality rate modelling is an **experimental feature** under active
#' development. This function can be called directly to compute daily
#' mortality probability for a single time step, but **mortality rates
#' are not yet integrated** into the main `run_fb4()` simulation loop.
#' Full integration (automatic daily mortality application, population
#' survival tracking, and result reporting) is planned for a future
#' release. The API may change.
#'
#' Note: **spawning energy loss** (reproductive cost) *is* already
#' integrated into `run_fb4()` and applies automatically when
#' `reproduction_data` is supplied to the `Bioenergetic` object.
#'
#' @examples
#' params <- list(
#'   base_mortality      = 0.001,
#'   natural_mortality   = 0.001,
#'   fishing_mortality   = 0.0005,
#'   predation_mortality = 0.0002,
#'   weight_threshold    = 10,
#'   starvation_factor   = 2,
#'   optimal_temp        = 18,
#'   thermal_tolerance   = 8,
#'   stress_factor       = 1.5,
#'   spawn_pattern       = NULL
#' )
#' calculate_mortality_reproduction(current_weight = 100, temperature = 15,
#'                                  day_of_year = 180,
#'                                  processed_mortality_params = params)
#' @export
calculate_mortality_reproduction <- function(current_weight, temperature, day_of_year,
                                             processed_mortality_params, 
                                             initial_weight = NULL) {
  
  # Extract processed mortality parameters
  # base_mortality <- processed_mortality_params$base_mortality
  natural_mortality <- processed_mortality_params$natural_mortality
  fishing_mortality <- processed_mortality_params$fishing_mortality
  predation_mortality <- processed_mortality_params$predation_mortality
  
  # Weight dependency parameters
  weight_threshold <- processed_mortality_params$weight_threshold
  starvation_factor <- processed_mortality_params$starvation_factor
  
  # Temperature dependency parameters
  optimal_temp <- processed_mortality_params$optimal_temp
  thermal_tolerance <- processed_mortality_params$thermal_tolerance
  stress_factor <- processed_mortality_params$stress_factor
  
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
  
  # Calculate reproduction if parameters exist
  reproduction_result <- NULL
  if (!is.null(processed_mortality_params$spawn_pattern)) {
    spawn_fraction <- if (length(processed_mortality_params$spawn_pattern) >= day_of_year) {
      processed_mortality_params$spawn_pattern[day_of_year]
    } else {
      0
    }
    
    if (spawn_fraction > 0) {
      reproduction_result <- calculate_reproductive_loss(
        spawn_fraction = spawn_fraction,
        current_weight = current_weight,
        energy_density = processed_mortality_params$energy_density
      )
    } else {
      reproduction_result <- list(
        weight_loss = 0,
        energy_loss = 0,
        spawn_fraction = 0,
        remaining_weight = current_weight
      )
    }
  }
  
  return(list(
    mortality = mortality_result,
    reproduction = reproduction_result,
    day_of_year = day_of_year,
    current_weight = current_weight,
    temperature = temperature
  ))
}
