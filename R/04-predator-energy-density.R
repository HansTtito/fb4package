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
#' Retrieves energy density from pre-calculated daily data
#'
#' @param weight Fish weight (g) - not used in this equation
#' @param day Simulation day
#' @param energy_data Vector with energy densities by day
#' @return Energy density (J/g)
#' @keywords internal
predator_energy_eq1 <- function(weight, day, energy_data) {
  if (is.null(energy_data) || length(energy_data) == 0) {
    return(4500)  # Typical default value
  }
  
  day_index <- clamp(round(day), 1, length(energy_data))
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
  cutoff_value <- as.numeric(Cutoff)
  
  if (weight < cutoff_value) {
    # First segment
    energy_density <- Alpha1 + Beta1 * weight
  } else {
    # Second segment
    if (is.na(Alpha2) || is.na(Beta2)) {
      # Use first segment if second segment parameters missing
      energy_density <- Alpha1 + Beta1 * weight
    } else {
      energy_density <- Alpha2 + Beta2 * weight
    }
  }
  
  return(clamp(energy_density, 1000, 15000))
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
  if (Alpha1 <= 0) {
    return(4500)  # Default value for invalid Alpha1
  }
  
  safe_beta <- clamp(Beta1, -2, 2)
  energy_density <- Alpha1 * (weight^safe_beta)
  
  return(clamp(energy_density, 1000, 15000))
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
    # Beta1 ≈ 0: constant energy density
    final_weight <- (initial_body_energy + net_energy) / Alpha1
  } else {
    # Solve: Alpha1 * W^(Beta1+1) = initial_body_energy + net_energy
    target_energy <- initial_body_energy + net_energy
    
    if (target_energy <= 0) {
      return(0.001)  # Minimum weight
    }
    
    exponent <- Beta1 + 1
    if (exponent <= 0) {
      # Problematic case, use approximation
      final_weight <- initial_weight * (1 + net_energy / initial_body_energy)
    } else {
      final_weight <- (target_energy / Alpha1)^(1/exponent)
    }
  }
  
  return(pmax(0.001, final_weight))
}

#' Solve weight for linear segments (Low-level)
#'
#' Solves final weight for piecewise linear function (PREDEDEQ = 2)
#'
#' @param available_energy Available energy (J)
#' @param predator_params Predator parameters
#' @return Final weight (g)
#' @keywords internal
solve_weight_linear_segments <- function(available_energy, predator_params) {
  Alpha1 <- as.numeric(predator_params$Alpha1)
  Beta1 <- as.numeric(predator_params$Beta1)
  Alpha2 <- as.numeric(predator_params$Alpha2)
  Beta2 <- as.numeric(predator_params$Beta2)
  Cutoff <- as.numeric(predator_params$Cutoff)
  
  # Energy at cutoff point
  cutoff_energy <- Cutoff * (Alpha1 + Beta1 * Cutoff)
  
  if (available_energy <= cutoff_energy) {
    # Use first segment
    if (abs(Beta1) < 1e-10) {
      # Beta1 ≈ 0: constant ED = Alpha1
      final_weight <- available_energy / Alpha1
    } else {
      # Solve quadratic equation: Beta1*W^2 + Alpha1*W - available_energy = 0
      discriminant <- Alpha1^2 + 4 * Beta1 * available_energy
      if (discriminant < 0) {
        return(0.01)  # Fallback for negative discriminant
      }
      final_weight <- (-Alpha1 + safe_sqrt(discriminant)) / (2 * Beta1)
    }
  } else {
    # Use second segment
    if (abs(Beta2) < 1e-10) {
      # Beta2 ≈ 0: constant ED = Alpha2
      energy_to_cutoff <- cutoff_energy
      additional_energy <- available_energy - energy_to_cutoff
      final_weight <- Cutoff + additional_energy / Alpha2
    } else {
      # Solve quadratic equation for second segment
      adjusted_energy <- available_energy - cutoff_energy + Cutoff * (Alpha2 + Beta2 * Cutoff)
      discriminant <- Alpha2^2 + 4 * Beta2 * adjusted_energy
      if (discriminant < 0) {
        return(0.01)  # Fallback for negative discriminant
      }
      final_weight <- (-Alpha2 + safe_sqrt(discriminant)) / (2 * Beta2)
    }
  }
  
  return(pmax(0.01, final_weight))
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
#' @param predator_params List with energy density parameters
#' @return Energy density (J/g)
#' @export
calculate_predator_energy_density <- function(weight, day = 1, predator_params) {

  # Extract equation type (assume it exists and is valid)
  PREDEDEQ <- predator_params$PREDEDEQ
  
  # Calculate energy density based on equation
  if (PREDEDEQ == 1) {
    ED_ini <- predator_params$ED_ini
    ED_end <- predator_params$ED_end
    energy_data <- predator_params$ED_data
    
    # Option 1: Use ED_data if available
    if (!is.null(energy_data) && length(energy_data) > 0 && !all(is.na(energy_data))) {
      return(predator_energy_eq1(weight, day, energy_data))
    }
    
    # Option 2: Use ED_ini/ED_end for interpolation
    if (!is.null(ED_ini) && !is.null(ED_end) && !is.na(ED_ini) && !is.na(ED_end)) {
      duration <- predator_params$duration %||% 365
      proportion <- pmin(1, pmax(0, (day - 1) / max(1, duration - 1)))
      energy_density <- ED_ini + (ED_end - ED_ini) * proportion
      return(clamp(energy_density, 1000, 15000))
    }
    
    # Fallback to default value
    return(4500)
    
  } else if (PREDEDEQ == 2) {
    # Piecewise linear function
    return(predator_energy_eq2(weight, predator_params$Alpha1, predator_params$Beta1, 
                               predator_params$Alpha2, predator_params$Beta2, predator_params$Cutoff))
    
  } else if (PREDEDEQ == 3) {
    # Power function
    return(predator_energy_eq3(weight, predator_params$Alpha1, predator_params$Beta1))
    
  } else {
    # Fallback to default
    return(4500)
  }
}

#' Solve weight using iterative method (Mid-level)
#'
#' General safe method for solving final weight when analytical solutions fail
#' Used as fallback for complex cases
#'
#' @param target_energy Target body energy
#' @param predator_params Predator parameters
#' @param day Current day
#' @param initial_guess Initial weight estimate
#' @return Final weight
#' @keywords internal
solve_weight_iterative <- function(target_energy, predator_params, day = 1, initial_guess = 1000) {

  # Objective function: find weight where energy_density * weight = target_energy
  objective_function <- function(weight) {
    if (weight <= 0) return(Inf)
    ed <- calculate_predator_energy_density(weight, day, predator_params)
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
#' @param predator_params Predator parameters
#' @param day Current day
#' @return List with final weight and change
#' @export
calculate_final_weight_fb4 <- function(initial_weight, net_energy, spawn_energy = 0, 
                                       predator_params, day = 1) {

  # Net energy after reproduction
  net_energy_after_spawn <- net_energy - spawn_energy
  
  # Calculate initial body energy
  initial_ed <- calculate_predator_energy_density(initial_weight, day, predator_params)
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
  PREDEDEQ <- predator_params$PREDEDEQ
  
  if (PREDEDEQ == 1) {
    # For PREDEDEQ=1, energy density may change day to day
    final_energy_density <- calculate_predator_energy_density(initial_weight, day + 1, predator_params)
    final_weight <- target_energy / final_energy_density
    
  } else if (PREDEDEQ == 2) {
    # Piecewise linear energy density
    final_weight <- solve_weight_linear_segments(target_energy, predator_params)
    
  } else if (PREDEDEQ == 3) {
    # Power function energy density
    final_weight <- solve_weight_power_function(initial_weight, net_energy_after_spawn, 
                                                predator_params$Alpha1, predator_params$Beta1)
    
  } else {
    # Fallback to iterative method
    final_weight <- solve_weight_iterative(target_energy, predator_params, day, initial_weight)
  }
  
  # Final validation and safety check
  if (!is.finite(final_weight) || final_weight <= 0) {
    final_weight <- 0.01
  }
  
  # Calculate final energy density
  final_ed <- calculate_predator_energy_density(final_weight, day, predator_params)
  
  return(list(
    final_weight = final_weight,
    final_energy_density = final_ed,
    weight_change = final_weight - initial_weight
  ))
}

# ============================================================================
# UTILITY FUNCTIONS: Validation and Analysis
# ============================================================================

#' Validate predator energy density parameters (Utility)
#'
#' Comprehensive validation of predator parameters across weight ranges
#' Includes validation since this may be called independently
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
      ed <- calculate_predator_energy_density(weight, 1, predator_params)
      
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
  
  # Equation-specific validations
  if (PREDEDEQ == 2) {
    required_params <- c("Alpha1", "Beta1", "Cutoff")
    missing_params <- setdiff(required_params, names(predator_params))
    if (length(missing_params) > 0) {
      validation$warnings <- c(validation$warnings,
                               paste("Missing parameters for PREDEDEQ=2:", 
                                     paste(missing_params, collapse = ", ")))
    }
  } else if (PREDEDEQ == 3) {
    required_params <- c("Alpha1", "Beta1")
    missing_params <- setdiff(required_params, names(predator_params))
    if (length(missing_params) > 0) {
      validation$warnings <- c(validation$warnings,
                               paste("Missing parameters for PREDEDEQ=3:", 
                                     paste(missing_params, collapse = ", ")))
    }
  }
  
  return(validation)
}