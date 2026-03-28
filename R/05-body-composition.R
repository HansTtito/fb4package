#' Body Composition Functions for FB4 Model
#'
#' @name body-composition
#' @aliases body-composition
NULL

# ============================================================================
# LOW-LEVEL FUNCTIONS
# ============================================================================

#' Estimate composition from water using Breck (2014) regression (Low-level)
#'
#' Estimates grams of component based on water content using Breck regression
#'
#' @param water_content Water content (g)
#' @param component_type Type of component ("protein" or "ash")
#' @return Component content (g)
#' @keywords internal
#' @references Breck, J.E. 2014. Body composition in fishes: body size matters. Aquaculture 433:40-49.
calculate_component_from_water <- function(water_content, component_type) {
  if (water_content <= 0) return(0)
  
  # Breck (2014) regression coefficients
  if (component_type == "protein") {
    # N = 101, r² = 0.9917
    intercept <- -0.8068
    slope <- 1.0750
  } else if (component_type == "ash") {
    # N = 101, r² = 0.9932
    intercept <- -1.6765
    slope <- 1.0384
  }
  
  # log10(Component) = intercept + slope * log10(H2O)
  component <- 10^(intercept + slope * log10(water_content))
  
  return(component)
}

#' Calculate fat content by subtraction (Low-level)
#'
#' Calculates grams of fat by subtracting water, protein and ash from total weight
#'
#' @param total_weight Total wet weight (g)
#' @param water_content Water content (g)
#' @param protein_content Protein content (g)
#' @param ash_content Ash content (g)
#' @return Fat content (g)
#' @keywords internal
calculate_fat_by_subtraction <- function(total_weight, water_content, protein_content, ash_content) {
  fat_content <- total_weight - water_content - protein_content - ash_content
  return(fat_content)
}

#' Calculate energy density from fat and protein (Low-level)
#'
#' Calculates energy density based on fat and protein content
#'
#' @param fat_content Fat content (g)
#' @param protein_content Protein content (g)
#' @param total_weight Total weight (g)
#' @param fat_energy Energy per gram of fat (J/g)
#' @param protein_energy Energy per gram of protein (J/g)
#' @return Energy density (J/g wet weight)
#' @keywords internal
calculate_energy_density <- function(fat_content, protein_content, total_weight, fat_energy, protein_energy) {
  energy_density <- (fat_content * fat_energy + protein_content * protein_energy) / total_weight
  return(energy_density)
}

# ============================================================================
# MID-LEVEL FUNCTIONS: Coordination and Business Logic
# ============================================================================

#' Calculate complete body composition (Mid-level - Main function)
#'
#' Main function that calculates all body composition components
#' and energy density from weight and processed parameters
#'
#' @param weight Total wet weight (g)
#' @param processed_composition_params List with processed composition parameters
#' @return List with complete body composition
#' @export
calculate_body_composition <- function(weight, processed_composition_params) {
  
  # Early return for invalid weight
  if (weight <= 0) {
    return(create_empty_composition())
  }
  
  # Extract processed parameters
  water_fraction <- processed_composition_params$water_fraction
  fat_energy <- processed_composition_params$fat_energy
  protein_energy <- processed_composition_params$protein_energy
  max_fat_fraction <- processed_composition_params$max_fat_fraction
  
  # Calculate water content
  water_content <- water_fraction * weight
  
  # Calculate other components using Breck (2014) regressions
  protein_content <- calculate_component_from_water(water_content, "protein")
  ash_content <- calculate_component_from_water(water_content, "ash")
  
  # Check if components exceed total weight
  total_components <- water_content + protein_content + ash_content
  if (total_components > weight) {
    # Proportional adjustment if components exceed weight
    adjustment_factor <- weight / total_components
    water_content <- water_content * adjustment_factor
    protein_content <- protein_content * adjustment_factor
    ash_content <- ash_content * adjustment_factor
  }
  
  # Calculate fat by subtraction
  fat_content <- calculate_fat_by_subtraction(weight, water_content, protein_content, ash_content)
  
  # Apply biological limits to fat
  max_fat <- weight * max_fat_fraction
  if (fat_content < 0) {
    fat_content <- 0
  } else if (fat_content > max_fat) {
    fat_content <- max_fat
  }
  
  # Calculate energy density
  energy_density <- calculate_energy_density(fat_content, protein_content, weight, fat_energy, protein_energy)
  
  # Calculate fractions
  water_frac <- water_content / weight
  protein_frac <- protein_content / weight
  ash_frac <- ash_content / weight
  fat_frac <- fat_content / weight
  
  # Check balance
  total_fraction <- water_frac + protein_frac + ash_frac + fat_frac
  balanced <- abs(total_fraction - 1) < 0.05
  
  return(list(
    # Basic information
    total_weight = weight,
    
    # Absolute contents (g)
    water_g = water_content,
    protein_g = protein_content,
    ash_g = ash_content,
    fat_g = fat_content,
    
    # Fractions
    water_fraction = water_frac,
    protein_fraction = protein_frac,
    ash_fraction = ash_frac,
    fat_fraction = fat_frac,
    
    # Energy
    energy_density = energy_density,
    total_energy = energy_density * weight,
    
    # Validation
    total_fraction = total_fraction,
    balanced = balanced
  ))
}


#' Update body composition during simulation (Mid-level)
#'
#' Updates body composition as fish grows or changes condition
#' Used during simulation loops - assumes pre-validated inputs
#'
#' @param old_weight Previous weight (g)
#' @param new_weight New weight (g)
#' @param old_composition Previous composition (optional)
#' @param processed_composition_params List with processed composition parameters
#' @return New body composition
#' @export
update_body_composition <- function(old_weight, new_weight, old_composition = NULL,
                                    processed_composition_params) {
  # Calculate new composition
  new_composition <- calculate_body_composition(new_weight, processed_composition_params)
  
  # If we have previous composition, calculate changes
  if (!is.null(old_composition)) {
    changes <- list(
      weight_change = new_weight - old_weight,
      water_change = new_composition$water_g - old_composition$water_g,
      protein_change = new_composition$protein_g - old_composition$protein_g,
      fat_change = new_composition$fat_g - old_composition$fat_g,
      energy_density_change = new_composition$energy_density - old_composition$energy_density
    )
    
    new_composition$changes <- changes
  }
  
  return(new_composition)
}

