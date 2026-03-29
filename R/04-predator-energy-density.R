#' Predator Energy Density Functions for FB4 Model
#'
#' @name predator-energy-density
#' @aliases predator-energy-density
NULL

# ============================================================================
# LOW-LEVEL FUNCTIONS
# ============================================================================

#' Energy density from interpolated data - Equation 1 (Low-level)
#'
#' Retrieves energy density from pre-calculated daily data.
#' The function accesses both \code{energy_data[day]} (start of day) and
#' \code{energy_data[day + 1]} (end of day) during weight calculations,
#' so the vector must have \strong{n_days + 1} elements (e.g., 366 for a
#' 365-day simulation: indices 1 to 366 represent days 0 to 365).
#'
#' @param weight Fish weight (g) - not used in this equation
#' @param day Simulation day (integer, 1-based)
#' @param energy_data Numeric vector of energy densities (J/g). Must have
#'   length \code{n_days + 1} — one value per day boundary, from the start
#'   of day 1 through the end of the last day. For a 365-day simulation,
#'   provide 366 values. Use \code{approx(..., xout = 0:n_days)} or
#'   \code{seq(ED_ini, ED_end, length.out = n_days + 1)} to generate this
#'   vector. If only initial and final values are known, use
#'   \code{ED_ini}/\code{ED_end} in the predator parameters instead.
#' @return Energy density (J/g)
#' @keywords internal
predator_energy_eq1 <- function(weight, day, energy_data) {
  day_index <- round(day)
  return(energy_data[day_index])
}

#' Linear piecewise energy density - Equation 2 (Low-level)
#'
#' Two-segment linear relationship between weight and energy density
#'
#' @param weight Fish weight (g)
#' @param Alpha1 Intercept for first segment
#' @param Beta1 Slope for first segment
#' @param Alpha2 Intercept for second segment
#' @param Beta2 Slope for second segment
#' @param Cutoff Weight cutoff between segments
#' @return Energy density (J/g)
#' @keywords internal
predator_energy_eq2 <- function(weight, Alpha1, Beta1, Alpha2, Beta2, Cutoff) {
  if (weight < Cutoff) {
    energy_density <- Alpha1 + Beta1 * weight
  } else {
    energy_density <- Alpha2 + Beta2 * weight
  }
  
  return(energy_density)
}

#' Power function energy density - Equation 3 (Low-level)
#'
#' Power relationship between weight and energy density
#'
#' @param weight Fish weight (g)
#' @param Alpha1 Multiplicative coefficient
#' @param Beta1 Exponent
#' @return Energy density (J/g)
#' @keywords internal
predator_energy_eq3 <- function(weight, Alpha1, Beta1) {
  energy_density <- Alpha1 * (weight^Beta1)
  return(energy_density)
}

#' Solve weight for power function (Low-level)
#'
#' Solves final weight for power function relationship (PREDEDEQ = 3)
#'
#' @param initial_weight Initial weight
#' @param net_energy Net energy
#' @param Alpha1 Coefficient
#' @param Beta1 Exponent
#' @return Final weight
#' @keywords internal
solve_weight_power_function <- function(initial_weight, net_energy, Alpha1, Beta1) {
  initial_body_energy <- initial_weight * Alpha1 * (initial_weight^Beta1)
  
  if (abs(Beta1) < 1e-6) {
    # Beta1 ~= 0: constant energy density
    final_weight <- (initial_body_energy + net_energy) / Alpha1
  } else {
    target_energy <- initial_body_energy + net_energy
    
    if (target_energy <= 0) {
      return(0)  # Mathematical result
    }
    
    exponent <- Beta1 + 1
    if (exponent <= 0) {
      # Approximation for problematic case
      final_weight <- initial_weight * (1 + net_energy / initial_body_energy)
    } else {
      final_weight <- (target_energy / Alpha1)^(1/exponent)
    }
  }
  
  return(final_weight)
}

#' Solve weight for linear segments (Low-level)
#'
#' Solves final weight for piecewise linear function (PREDEDEQ = 2)
#'
#' @param available_energy Available energy (J)
#' @param Alpha1 Intercept for first size segment
#' @param Beta1 Slope for first size segment
#' @param Alpha2 Intercept for second size segment
#' @param Beta2 Slope for second size segment
#' @param Cutoff Weight cutoff between segments (g)
#' @return Final weight (g)
#' @keywords internal
solve_weight_linear_segments <- function(available_energy, Alpha1, Beta1, Alpha2, Beta2, Cutoff) {
  # Energy at cutoff point
  cutoff_energy <- Cutoff * (Alpha1 + Beta1 * Cutoff)
  
  if (available_energy <= cutoff_energy) {
    # First segment
    if (abs(Beta1) < 1e-10) {
      final_weight <- available_energy / Alpha1
    } else {
      # Solve quadratic equation
      discriminant <- Alpha1^2 + 4 * Beta1 * available_energy
      if (discriminant < 0) return(0)  # Mathematical result
      final_weight <- (-Alpha1 + safe_sqrt(discriminant)) / (2 * Beta1)
    }
  } else {
    # Second segment
    if (abs(Beta2) < 1e-10) {
      additional_energy <- available_energy - cutoff_energy
      final_weight <- Cutoff + additional_energy / Alpha2
    } else {
      adjusted_energy <- available_energy - cutoff_energy + Cutoff * (Alpha2 + Beta2 * Cutoff)
      discriminant <- Alpha2^2 + 4 * Beta2 * adjusted_energy
      if (discriminant < 0) return(0)  # Mathematical result
      final_weight <- (-Alpha2 + safe_sqrt(discriminant)) / (2 * Beta2)
    }
  }
  
  return(final_weight)
}

# ============================================================================
# MID-LEVEL FUNCTIONS: Coordination and Business Logic
# ============================================================================

#' Calculate predator energy density (Mid-level - Main function)
#'
#' Main energy density calculation function called from simulation loop
#'
#' @param weight Fish weight (g)
#' @param day Simulation day (for equation 1)
#' @param processed_predator_params List with processed predator parameters
#' @return Energy density (J/g)
#' @export
calculate_predator_energy_density <- function(weight, day = 1, processed_predator_params) {
  
  PREDEDEQ <- processed_predator_params$PREDEDEQ
  
  if (PREDEDEQ == 1) {
    return(predator_energy_eq1(weight, day, processed_predator_params$ED_data))
    
  } else if (PREDEDEQ == 2) {
    return(predator_energy_eq2(weight, processed_predator_params$Alpha1, processed_predator_params$Beta1, 
                               processed_predator_params$Alpha2, processed_predator_params$Beta2, 
                               processed_predator_params$Cutoff))
    
  } else if (PREDEDEQ == 3) {
    return(predator_energy_eq3(weight, processed_predator_params$Alpha1, processed_predator_params$Beta1))
  }
}

#' Solve weight using iterative method (Mid-level)
#'
#' General safe method for solving final weight when analytical solutions fail
#' Used as fallback for complex cases
#'
#' @param target_energy Target body energy
#' @param processed_predator_params Processed predator parameters
#' @param day Current day
#' @param initial_guess Initial weight estimate
#' @return Final weight
#' @keywords internal
#' @importFrom stats optimize
solve_weight_iterative <- function(target_energy, processed_predator_params, day = 1, initial_guess = 1000) {
  
  # Objective function: find weight where energy_density * weight = target_energy
  objective_function <- function(weight) {
    if (weight <= 0) return(Inf)
    ed <- calculate_predator_energy_density(weight, day, processed_predator_params)
    return(abs(ed * weight - target_energy))
  }
  
  # Use optimization to find weight
  tryCatch({
    result <- optimize(objective_function, 
                       interval = c(0.001, 100000), 
                       tol = 1e-6)
    return(result$minimum)
  }, error = function(e) {
    return(initial_guess)  # Silent fallback
  })
}

#' Calculate final weight using FB4 equations (Mid-level)
#'
#' Main function for calculating final weight from energy balance
#'
#' @param initial_weight Initial weight (g)
#' @param net_energy Net available energy (J)
#' @param spawn_energy Energy lost to reproduction (J)
#' @param processed_predator_params Processed predator parameters
#' @param day Current day
#' @return List with final weight and change
#' @export
calculate_final_weight_fb4 <- function(initial_weight, net_energy, spawn_energy = 0, 
                                       processed_predator_params, day = 1) {
  
  # Net energy after reproduction
  net_energy_after_spawn <- net_energy - spawn_energy
  
  # Calculate initial body energy
  initial_ed <- calculate_predator_energy_density(initial_weight, day, processed_predator_params)
  initial_body_energy <- initial_weight * initial_ed
  target_energy <- initial_body_energy + net_energy_after_spawn
  
  # If target energy is very low, return minimum weight
  if (target_energy <= 0) {
    return(list(
      final_weight = 0.01,
      final_energy_density = initial_ed,
      weight_change = 0.01 - initial_weight
    ))
  }
  
  # Calculate final weight based on PREDEDEQ
  PREDEDEQ <- processed_predator_params$PREDEDEQ
  
  if (PREDEDEQ == 1) {
    # For PREDEDEQ=1, energy density may change day to day
    final_energy_density <- calculate_predator_energy_density(initial_weight, day + 1, processed_predator_params)
    final_weight <- target_energy / final_energy_density
    
  } else if (PREDEDEQ == 2) {
    # Piecewise linear energy density
    final_weight <- solve_weight_linear_segments(target_energy, processed_predator_params$Alpha1, 
                                                 processed_predator_params$Beta1, processed_predator_params$Alpha2, 
                                                 processed_predator_params$Beta2, processed_predator_params$Cutoff)
    
  } else if (PREDEDEQ == 3) {
    # Power function energy density
    final_weight <- solve_weight_power_function(initial_weight, net_energy_after_spawn, 
                                                processed_predator_params$Alpha1, processed_predator_params$Beta1)
  }
  
  # Final validation and safety check
  if (!is.finite(final_weight) || final_weight <= 0) {
    final_weight <- 0.01
  }
  
  # Calculate final energy density
  final_ed <- calculate_predator_energy_density(final_weight, day, processed_predator_params)
  
  return(list(
    final_weight = final_weight,
    final_energy_density = final_ed,
    weight_change = final_weight - initial_weight
  ))
}


