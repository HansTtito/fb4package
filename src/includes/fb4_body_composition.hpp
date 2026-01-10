#ifndef FB4_BODY_COMPOSITION_HPP
#define FB4_BODY_COMPOSITION_HPP


#include "fb4_utils.hpp"

/*
 * Body Composition Functions for FB4 Model
 * 
 * Based on Breck (2014) regressions for estimating protein and ash
 * from water content, with fat calculated by subtraction.
 * 
 * References:
 * Breck, J.E. 2014. Body composition in fishes: body size matters. 
 * Aquaculture 433:40-49.
 */

namespace fb4 {

// ============================================================================
// LOW-LEVEL FUNCTIONS
// ============================================================================

/**
 * Estimate component from water using Breck (2014) regression
 * 
 * @param water_content Water content (g)
 * @param component_type Component type: 0=protein, 1=ash
 * @return Component content (g)
 */
template<class Type>
Type calculate_component_from_water(Type water_content, int component_type) {
  if (water_content <= Type(0.0)) return Type(0.0);
  
  Type intercept, slope;
  
  if (component_type == 0) {  // protein
    // N = 101, r² = 0.9917
    intercept = Type(-0.8068);
    slope = Type(1.0750);
  } else {  // ash (component_type == 1)
    // N = 101, r² = 0.9932
    intercept = Type(-1.6765);
    slope = Type(1.0384);
  }
  
  // log10(Component) = intercept + slope * log10(H2O)
  Type log_water = log(water_content) / log(Type(10.0));
  Type log_component = intercept + slope * log_water;
  Type component = pow(Type(10.0), log_component);
  
  return component;
}

/**
 * Calculate fat content by subtraction
 * 
 * @param total_weight Total wet weight (g)
 * @param water_content Water content (g)
 * @param protein_content Protein content (g)
 * @param ash_content Ash content (g)
 * @return Fat content (g)
 */
template<class Type>
Type calculate_fat_by_subtraction(Type total_weight, Type water_content, 
                                  Type protein_content, Type ash_content) {
  Type fat_content = total_weight - water_content - protein_content - ash_content;
  return fat_content;
}

/**
 * Calculate energy density from fat and protein
 * 
 * @param fat_content Fat content (g)
 * @param protein_content Protein content (g)
 * @param total_weight Total weight (g)
 * @param fat_energy Energy per gram of fat (J/g)
 * @param protein_energy Energy per gram of protein (J/g)
 * @return Energy density (J/g wet weight)
 */
template<class Type>
Type calculate_energy_density(Type fat_content, Type protein_content, Type total_weight,
                              Type fat_energy, Type protein_energy) {
  if (total_weight <= Type(0.0)) return Type(0.0);
  
  Type energy_density = (fat_content * fat_energy + protein_content * protein_energy) / total_weight;
  return energy_density;
}

// ============================================================================
// COMPOSITION RESULT STRUCTURE
// ============================================================================

/**
 * Structure to hold complete body composition results
 */
template<class Type>
struct BodyComposition {
  // Basic information
  Type total_weight;
  
  // Absolute contents (g)
  Type water_g;
  Type protein_g;
  Type ash_g;
  Type fat_g;
  
  // Fractions
  Type water_fraction;
  Type protein_fraction;
  Type ash_fraction;
  Type fat_fraction;
  
  // Energy
  Type energy_density;
  Type total_energy;
  
  // Validation
  Type total_fraction;
  bool balanced;
  
  // Constructor
  BodyComposition() : total_weight(Type(0.0)), water_g(Type(0.0)), protein_g(Type(0.0)), 
  ash_g(Type(0.0)), fat_g(Type(0.0)), water_fraction(Type(0.0)), 
  protein_fraction(Type(0.0)), ash_fraction(Type(0.0)), fat_fraction(Type(0.0)),
  energy_density(Type(0.0)), total_energy(Type(0.0)), total_fraction(Type(0.0)), 
  balanced(false) {}
};

// ============================================================================
// MID-LEVEL FUNCTIONS
// ============================================================================

/**
 * Calculate complete body composition - main function
 * 
 * @param weight Total wet weight (g)
 * @param water_fraction Water fraction (0-1)
 * @param fat_energy Energy per gram of fat (J/g)
 * @param protein_energy Energy per gram of protein (J/g)
 * @param max_fat_fraction Maximum fat fraction (0-1)
 * @return Complete body composition
 */
template<class Type>
BodyComposition<Type> calculate_body_composition(Type weight, Type water_fraction,
                                                 Type fat_energy, Type protein_energy,
                                                 Type max_fat_fraction) {
  
  BodyComposition<Type> comp;
  
  // Early return for invalid weight
  if (weight <= Type(0.0)) {
    return comp;
  }
  
  comp.total_weight = weight;
  
  // Calculate water content
  Type water_content = water_fraction * weight;
  
  // Calculate other components using Breck (2014) regressions
  Type protein_content = calculate_component_from_water(water_content, 0);  // protein
  Type ash_content = calculate_component_from_water(water_content, 1);      // ash
  
  // Check if components exceed total weight
  Type total_components = water_content + protein_content + ash_content;
  if (total_components > weight) {
    // Proportional adjustment if components exceed weight
    Type adjustment_factor = weight / total_components;
    water_content *= adjustment_factor;
    protein_content *= adjustment_factor;
    ash_content *= adjustment_factor;
  }
  
  // Calculate fat by subtraction
  Type fat_content = calculate_fat_by_subtraction(weight, water_content, 
                                                  protein_content, ash_content);
  
  // Apply biological limits to fat
  Type max_fat = weight * max_fat_fraction;
  if (fat_content < Type(0.0)) {
    fat_content = Type(0.0);
  } else if (fat_content > max_fat) {
    fat_content = max_fat;
  }
  
  // Calculate energy density
  Type energy_density = calculate_energy_density(fat_content, protein_content, weight,
                                                 fat_energy, protein_energy);
  
  // Fill structure
  comp.water_g = water_content;
  comp.protein_g = protein_content;
  comp.ash_g = ash_content;
  comp.fat_g = fat_content;
  
  // Calculate fractions
  comp.water_fraction = water_content / weight;
  comp.protein_fraction = protein_content / weight;
  comp.ash_fraction = ash_content / weight;
  comp.fat_fraction = fat_content / weight;
  
  // Energy
  comp.energy_density = energy_density;
  comp.total_energy = energy_density * weight;
  
  // Validation
  comp.total_fraction = comp.water_fraction + comp.protein_fraction + 
    comp.ash_fraction + comp.fat_fraction;
  comp.balanced = abs(comp.total_fraction - Type(1.0)) < Type(0.05);
  
  return comp;
}

/**
 * Update body composition during simulation
 * 
 * @param old_weight Previous weight (g)
 * @param new_weight New weight (g)
 * @param water_fraction Water fraction for new weight
 * @param fat_energy Energy per gram of fat (J/g)
 * @param protein_energy Energy per gram of protein (J/g)
 * @param max_fat_fraction Maximum fat fraction (0-1)
 * @return New body composition
 */
template<class Type>
BodyComposition<Type> update_body_composition(Type old_weight, Type new_weight,
                                              Type water_fraction, Type fat_energy,
                                              Type protein_energy, Type max_fat_fraction) {
  
  // Calculate new composition
  BodyComposition<Type> new_composition = calculate_body_composition(new_weight, water_fraction,
                                                                     fat_energy, protein_energy,
                                                                     max_fat_fraction);
  
  return new_composition;
}

} // namespace fb4

#endif // FB4_BODY_COMPOSITION_HPP