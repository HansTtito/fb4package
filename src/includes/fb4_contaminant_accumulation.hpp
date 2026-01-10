#ifndef FB4_CONTAMINANT_HPP
#define FB4_CONTAMINANT_HPP


#include "fb4_utils.hpp"

/*
 * Contaminant Accumulation Functions for FB4 Model
 * 
 * Three contaminant models:
 * 1. Food uptake only (no elimination)
 * 2. Temperature and weight dependent elimination (Trudel & Rasmussen 1997)
 * 3. Arnot & Gobas (2004) - uptake from water and food, elimination via respiration
 * 
 * References:
 * Trudel, M. & Rasmussen, J.B. 1997. Modeling the elimination of mercury by fish.
 * Arnot, J.A. & Gobas, F.A.P.C. 2004. A food web bioaccumulation model for organic chemicals in aquatic ecosystems.
 */

namespace fb4 {

// ============================================================================
// CONTAMINANT RESULT STRUCTURE
// ============================================================================

/**
 * Structure to hold contaminant accumulation results
 */
template<class Type>
struct ContaminantResult {
  Type clearance;           // Elimination rate (ug/day)
  Type uptake;             // Total uptake rate (ug/day)
  Type uptake_water;       // Uptake from water (ug/day) - only for model 3
  Type uptake_food;        // Uptake from food (ug/day) - only for model 3
  Type new_burden;         // New body burden (ug)
  Type new_concentration;  // New concentration (ug/g)
  Type weight;             // Fish weight (g)
  int model_used;          // Model number used
  
  // Constructor
  ContaminantResult() : clearance(Type(0.0)), uptake(Type(0.0)), uptake_water(Type(0.0)),
  uptake_food(Type(0.0)), new_burden(Type(0.0)), new_concentration(Type(0.0)),
  weight(Type(0.0)), model_used(1) {}
};

// ============================================================================
// LOW-LEVEL FUNCTIONS
// ============================================================================

/**
 * Contaminant model 1 - Food uptake only
 * Simple model without elimination, only accumulation from food
 * 
 * @param consumption Vector of consumption by prey (g/day)
 * @param prey_concentrations Vector of concentrations in prey (ug/g)
 * @param transfer_efficiency Vector of transfer efficiencies
 * @param current_burden Current body burden (ug)
 * @return Contaminant result
 */
template<class Type>
ContaminantResult<Type> contaminant_model_1(const vector<Type>& consumption,
                                            const vector<Type>& prey_concentrations,
                                            const vector<Type>& transfer_efficiency,
                                            Type current_burden) {
  
  ContaminantResult<Type> result;
  result.model_used = 1;
  
  // Uptake from food (ug/day)
  Type uptake = Type(0.0);
  for (int i = 0; i < consumption.size(); i++) {
    uptake += consumption[i] * prey_concentrations[i] * transfer_efficiency[i];
  }
  
  // No elimination in this model
  result.clearance = Type(0.0);
  result.uptake = uptake;
  result.new_burden = current_burden + uptake;
  
  return result;
}

/**
 * Contaminant model 2 - With temperature and weight dependent elimination
 * Model with food uptake and elimination dependent on temperature and weight
 * Based on Trudel & Rasmussen (1997) for MeHg
 * 
 * @param consumption Vector of consumption by prey (g/day)
 * @param weight Fish weight (g)
 * @param temperature Water temperature (deg C)
 * @param prey_concentrations Vector of concentrations in prey (ug/g)
 * @param assimilation_efficiency Vector of assimilation efficiencies
 * @param current_burden Current body burden (ug)
 * @return Contaminant result
 */
template<class Type>
ContaminantResult<Type> contaminant_model_2(const vector<Type>& consumption,
                                            Type weight, Type temperature,
                                            const vector<Type>& prey_concentrations,
                                            const vector<Type>& assimilation_efficiency,
                                            Type current_burden) {
  
  ContaminantResult<Type> result;
  result.model_used = 2;
  result.weight = weight;
  
  // Uptake from food (ug/day)
  Type uptake = Type(0.0);
  for (int i = 0; i < consumption.size(); i++) {
    uptake += consumption[i] * prey_concentrations[i] * assimilation_efficiency[i];
  }
  
  // MeHg elimination coefficient (Trudel & Rasmussen 1997)
  // Kx = exp(0.066*T - 0.2*log(W) - 6.56)
  Type log_weight = log(weight);
  Type Kx = exp(Type(0.066) * temperature - Type(0.2) * log_weight - Type(6.56));
  
  // Elimination (ug/day)
  Type clearance = Kx * current_burden;
  
  result.uptake = uptake;
  result.clearance = clearance;
  result.new_burden = current_burden + uptake - clearance;
  
  return result;
}

/**
 * Contaminant model 3 - Arnot & Gobas (2004)
 * Complete model with uptake from water and food, elimination proportional to respiration
 * 
 * @param respiration_o2 Respiration (g O2/g/day)
 * @param consumption Vector of consumption by prey (g/day)
 * @param weight Fish weight (g)
 * @param temperature Water temperature (deg C)
 * @param prey_concentrations Vector of concentrations in prey (ug/g)
 * @param assimilation_efficiency Vector of assimilation efficiencies
 * @param current_burden Current body burden (ug)
 * @param gill_efficiency Gill uptake efficiency
 * @param fish_water_partition Fish:water partition coefficient
 * @param water_concentration Total concentration in water (mg/L)
 * @param dissolved_fraction Dissolved fraction
 * @param do_saturation Dissolved oxygen saturation (fraction)
 * @return Contaminant result
 */
template<class Type>
ContaminantResult<Type> contaminant_model_3(Type respiration_o2,
                                            const vector<Type>& consumption,
                                            Type weight, Type temperature,
                                            const vector<Type>& prey_concentrations,
                                            const vector<Type>& assimilation_efficiency,
                                            Type current_burden,
                                            Type gill_efficiency,
                                            Type fish_water_partition,
                                            Type water_concentration,
                                            Type dissolved_fraction,
                                            Type do_saturation) {
  
  ContaminantResult<Type> result;
  result.model_used = 3;
  result.weight = weight;
  
  // Convert respiration to mg O2/g/day
  Type VOx = Type(1000.0) * respiration_o2;
  
  // Dissolved oxygen concentration (mg O2/L)
  // Arnot & Gobas (2004) equation
  Type COx = (-Type(0.24) * temperature + Type(14.04)) * do_saturation;
  
  // Water elimination rate (L/g/day)
  Type K1 = gill_efficiency * VOx / COx;
  
  // Uptake from water (ug/day)
  Type uptake_water = weight * K1 * dissolved_fraction * water_concentration * Type(1000.0);
  
  // Uptake from food (ug/day)
  Type uptake_food = Type(0.0);
  for (int i = 0; i < consumption.size(); i++) {
    uptake_food += consumption[i] * prey_concentrations[i] * assimilation_efficiency[i];
  }
  
  // Total uptake
  Type uptake = uptake_water + uptake_food;
  
  // Elimination coefficient
  Type Kx = K1 / fish_water_partition;
  
  // Elimination (ug/day)
  Type clearance = Kx * current_burden;
  
  result.uptake = uptake;
  result.uptake_water = uptake_water;
  result.uptake_food = uptake_food;
  result.clearance = clearance;
  result.new_burden = current_burden + uptake - clearance;
  
  return result;
}

// ============================================================================
// PARAMETER CALCULATION FUNCTIONS
// ============================================================================

/**
 * Calculate gill uptake efficiency based on Kow
 * According to Arnot & Gobas (2004)
 * 
 * @param kow Octanol:water partition coefficient
 * @return Gill uptake efficiency
 */
template<class Type>
Type calculate_gill_efficiency(Type kow) {
  // Arnot & Gobas (2004) equation
  Type efficiency = Type(1.0) / (Type(1.85) + (Type(155.0) / kow));
  return efficiency;
}

/**
 * Calculate fish:water partition coefficient
 * Based on body composition and Kow according to Arnot & Gobas (2004)
 * 
 * @param fat_fraction Fat fraction in fish
 * @param protein_ash_fraction Protein + ash fraction
 * @param water_fraction Water fraction
 * @param kow Octanol:water partition coefficient
 * @return Fish:water partition coefficient
 */
template<class Type>
Type calculate_fish_water_partition(Type fat_fraction, Type protein_ash_fraction,
                                    Type water_fraction, Type kow) {
  // Arnot & Gobas (2004) equation
  Type partition_coeff = fat_fraction * kow + 
    protein_ash_fraction * Type(0.035) * kow + 
    water_fraction;
  return partition_coeff;
}

/**
 * Calculate dissolved fraction of contaminant
 * Based on organic carbon according to Arnot & Gobas (2004)
 * 
 * @param poc_concentration Particulate organic carbon concentration (kg/L)
 * @param doc_concentration Dissolved organic carbon concentration (kg/L)
 * @param kow Octanol:water partition coefficient
 * @return Dissolved fraction
 */
template<class Type>
Type calculate_dissolved_fraction(Type poc_concentration, Type doc_concentration, Type kow) {
  // Arnot & Gobas (2004) constants
  Type a_poc = Type(0.35);
  Type a_doc = Type(0.08);
  
  // Arnot & Gobas (2004) equation
  Type denominator = Type(1.0) + poc_concentration * a_poc * kow + 
    doc_concentration * a_doc * kow;
  Type dissolved_fraction = Type(1.0) / denominator;
  
  return dissolved_fraction;
}

// ============================================================================
// MID-LEVEL FUNCTIONS
// ============================================================================

/**
 * Calculate contaminant accumulation - main function
 * 
 * @param respiration_o2 Respiration in g O2/g/day
 * @param consumption Vector of consumption by prey type (g/day)
 * @param weight Fish weight (g)
 * @param temperature Water temperature (deg C)
 * @param current_concentration Current concentration in predator (ug/g)
 * @param CONTEQ Contaminant equation number (1, 2, or 3)
 * @param prey_concentrations Vector of concentrations in prey (ug/g)
 * @param transfer_efficiency Vector of transfer efficiencies (for model 1)
 * @param assimilation_efficiency Vector of assimilation efficiencies (for models 2,3)
 * @param gill_efficiency Gill efficiency (for model 3)
 * @param fish_water_partition Fish:water partition coefficient (for model 3)
 * @param water_concentration Water concentration (mg/L) (for model 3)
 * @param dissolved_fraction Dissolved fraction (for model 3)
 * @param do_saturation DO saturation fraction (for model 3)
 * @return Contaminant result
 */
template<class Type>
ContaminantResult<Type> calculate_contaminant_accumulation(Type respiration_o2,
                                                           const vector<Type>& consumption,
                                                           Type weight, Type temperature,
                                                           Type current_concentration,
                                                           int CONTEQ,
                                                           const vector<Type>& prey_concentrations,
                                                           const vector<Type>& transfer_efficiency,
                                                           const vector<Type>& assimilation_efficiency,
                                                           Type gill_efficiency,
                                                           Type fish_water_partition,
                                                           Type water_concentration,
                                                           Type dissolved_fraction,
                                                           Type do_saturation) {
  
  // Calculate initial body burden (ug)
  Type current_burden = current_concentration * weight;
  
  ContaminantResult<Type> result;
  
  // Calculate based on model
  if (CONTEQ == 1) {
    result = contaminant_model_1(consumption, prey_concentrations, 
                                 transfer_efficiency, current_burden);
  } else if (CONTEQ == 2) {
    result = contaminant_model_2(consumption, weight, temperature, prey_concentrations,
                                 assimilation_efficiency, current_burden);
  } else if (CONTEQ == 3) {
    result = contaminant_model_3(respiration_o2, consumption, weight, temperature,
                                 prey_concentrations, assimilation_efficiency,
                                 current_burden, gill_efficiency, fish_water_partition,
                                 water_concentration, dissolved_fraction, do_saturation);
  }
  
  // Calculate new concentration
  result.new_concentration = (weight > Type(0.0)) ? result.new_burden / weight : Type(0.0);
  result.weight = weight;
  
  // Ensure non-negative values
  result.new_burden = fb4::clamp(result.new_burden, Type(0.0), Type(1e6), "new_burden");
  result.new_concentration = fb4::clamp(result.new_concentration, Type(0.0), Type(1e6), "new_concentration");
  
  return result;
}

} // namespace fb4

#endif // FB4_CONTAMINANT_HPP