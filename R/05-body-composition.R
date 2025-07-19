#' Body Composition Functions for FB4 Model
#'
#' @name body-composition
#' @aliases body-composition
NULL

# ============================================================================
# LOW-LEVEL FUNCTIONS
# ============================================================================

#' Estimate protein content from water (Low-level)
#'
#' Estimates grams of protein based on water content using Breck (2014) regression
#'
#' @param water_content Water content (g)
#' @return Protein content (g)
#' @keywords internal
#' @references Breck, J.E. 2014. Body composition in fishes: body size matters. Aquaculture 433:40-49.
calculate_protein_from_water <- function(water_content) {
  if (water_content <= 0) return(0)
  
  # Breck (2014) regression, N = 101, r² = 0.9917
  # log10(Protein) = -0.8068 + 1.0750 * log10(H2O)
  protein <- 10^(-0.8068 + 1.0750 * log10(water_content))
  
  return(pmax(0, protein))
}

#' Estimate ash content from water (Low-level)
#'
#' Estimates grams of ash based on water content using Breck (2014) regression
#'
#' @param water_content Water content (g)
#' @return Ash content (g)
#' @keywords internal
#' @references Breck, J.E. 2014. Body composition in fishes: body size matters. Aquaculture 433:40-49.
calculate_ash_from_water <- function(water_content) {
  if (water_content <= 0) return(0)
  
  # Breck (2014) regression, N = 101, r² = 0.9932
  # log10(Ash) = -1.6765 + 1.0384 * log10(H2O)
  ash <- 10^(-1.6765 + 1.0384 * log10(water_content))
  
  return(pmax(0, ash))
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
  
  # Ensure fat is not negative
  fat_content <- pmax(0, fat_content)
  
  # Apply biological limits (maximum ~35% of weight)
  max_fat <- total_weight * 0.35
  fat_content <- pmin(fat_content, max_fat)
  
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
calculate_energy_density <- function(fat_content, protein_content, total_weight, 
                                     fat_energy = 36200, protein_energy = 23600) {
  if (total_weight <= 0) return(4500)  # Default value
  
  # Calculate energy density
  energy_density <- (fat_content * fat_energy + protein_content * protein_energy) / total_weight
  
  # Limit to typical biological range
  energy_density <- clamp(energy_density, 2000, 10000)
  
  return(energy_density)
}

# ============================================================================
# MID-LEVEL FUNCTIONS: Coordination and Business Logic
# ============================================================================

#' Calculate complete body composition (Mid-level - Main function)
#'
#' Main function that calculates all body composition components
#' and energy density from weight and water fraction
#'
#' @param weight Total wet weight (g)
#' @param water_fraction Water fraction (0-1), default 0.728
#' @param fat_energy Energy per gram of fat (J/g)
#' @param protein_energy Energy per gram of protein (J/g)
#' @return List with complete body composition
#' @export
calculate_body_composition <- function(weight, 
                                       water_fraction = 0.728, 
                                       fat_energy = 39300, 
                                       protein_energy = 23600) {

  # Calculate water content
  water_content <- water_fraction * weight
  
  # Calculate other components using Breck (2014) regressions
  protein_content <- calculate_protein_from_water(water_content)
  ash_content <- calculate_ash_from_water(water_content)
  fat_content <- calculate_fat_by_subtraction(weight, water_content, protein_content, ash_content)
  
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
#' @param water_fraction_new Water fraction for new weight
#' @return New body composition
#' @export
update_body_composition <- function(old_weight, new_weight, old_composition = NULL, 
                                    water_fraction_new = 0.728) {

  # Calculate new composition
  new_composition <- calculate_body_composition(new_weight, water_fraction_new)
  
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

# ============================================================================
# UTILITY FUNCTIONS: Analysis and Validation
# ============================================================================

#' Analyze body composition by size range (Utility)
#'
#' Analyzes body composition across a range of fish sizes
#'
#' @param weight_range Weight range to analyze (2-element vector)
#' @param n_points Number of points to analyze
#' @param water_fraction Water fraction (constant or function of weight)
#' @return Data frame with composition analysis by size
#' @export
analyze_composition_by_size <- function(weight_range = c(1, 500), 
                                        n_points = 50, 
                                        water_fraction = 0.728) {

  # Create weight sequence
  weights <- seq(weight_range[1], weight_range[2], length.out = n_points)
  
  # Allow variable water fraction if function
  if (is.function(water_fraction)) {
    water_fractions <- sapply(weights, water_fraction)
  } else {
    water_fractions <- rep(water_fraction, n_points)
  }
  
  # Calculate compositions
  compositions <- mapply(
    calculate_body_composition,
    weight = weights,
    water_fraction = water_fractions,
    SIMPLIFY = FALSE
  )
  
  # Convert to data frame
  result_df <- data.frame(
    Weight = weights,
    Water_g = sapply(compositions, function(x) x$water_g),
    Protein_g = sapply(compositions, function(x) x$protein_g),
    Ash_g = sapply(compositions, function(x) x$ash_g),
    Fat_g = sapply(compositions, function(x) x$fat_g),
    Water_fraction = sapply(compositions, function(x) x$water_fraction),
    Protein_fraction = sapply(compositions, function(x) x$protein_fraction),
    Ash_fraction = sapply(compositions, function(x) x$ash_fraction),
    Fat_fraction = sapply(compositions, function(x) x$fat_fraction),
    Energy_density = sapply(compositions, function(x) x$energy_density)
  )
  
  return(result_df)
}

#' Validate body composition (Utility)
#'
#' Validates body composition against typical biological ranges
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
  
  # Validate ranges
  if (composition$water_fraction < typical_ranges$water[1] || 
      composition$water_fraction > typical_ranges$water[2]) {
    validation$warnings <- c(validation$warnings,
                             paste("Water fraction outside typical range:",
                                   round(composition$water_fraction, 3)))
  }
  
  if (composition$protein_fraction < typical_ranges$protein[1] || 
      composition$protein_fraction > typical_ranges$protein[2]) {
    validation$warnings <- c(validation$warnings,
                             paste("Protein fraction outside typical range:",
                                   round(composition$protein_fraction, 3)))
  }
  
  if (composition$energy_density < typical_ranges$energy_density[1] || 
      composition$energy_density > typical_ranges$energy_density[2]) {
    validation$warnings <- c(validation$warnings,
                             paste("Energy density outside typical range:",
                                   round(composition$energy_density, 0), "J/g"))
  }
  
  # Validate fraction balance
  if (!composition$balanced) {
    validation$errors <- c(validation$errors, 
                           paste("Fractions do not sum to ~1.0:",
                                 round(composition$total_fraction, 3)))
    validation$valid <- FALSE
  }
  
  # Validate negative values
  if (any(c(composition$water_g, composition$protein_g, 
            composition$ash_g, composition$fat_g) < 0)) {
    validation$errors <- c(validation$errors, "Negative components detected")
    validation$valid <- FALSE
  }
  
  return(validation)
}