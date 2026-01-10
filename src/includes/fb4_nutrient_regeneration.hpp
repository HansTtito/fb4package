#ifndef FB4_NUTRIENT_HPP
#define FB4_NUTRIENT_HPP


#include "fb4_utils.hpp"

/*
 * Nutrient Regeneration Functions for FB4 Model
 * 
 * Calculates nitrogen and phosphorus fluxes through:
 * - Consumption from prey
 * - Growth incorporation 
 * - Excretion (dissolved)
 * - Egestion (particulate)
 * 
 * Used for understanding stoichiometric ecology and ecosystem impacts.
 */

namespace fb4 {

// ============================================================================
// NUTRIENT RESULT STRUCTURE
// ============================================================================

/**
 * Structure to hold nutrient flux results for a single nutrient
 */
template<class Type>
struct NutrientFlux {
  Type consumed;                    // Total consumed (g nutrient/day)
  Type growth;                     // Incorporated in growth (g nutrient/day)
  Type excretion;                  // Excreted (dissolved) (g nutrient/day)
  Type egestion;                   // Egested (particulate) (g nutrient/day)
  Type assimilated;                // Total assimilated (g nutrient/day)
  Type assimilation_efficiency;    // Assimilation efficiency (fraction)
  
  // Constructor
  NutrientFlux() : consumed(Type(0.0)), growth(Type(0.0)), excretion(Type(0.0)),
  egestion(Type(0.0)), assimilated(Type(0.0)), 
  assimilation_efficiency(Type(0.0)) {}
};

/**
 * Structure to hold complete nutrient balance results
 */
template<class Type>
struct NutrientBalance {
  NutrientFlux<Type> nitrogen;     // Nitrogen fluxes
  NutrientFlux<Type> phosphorus;   // Phosphorus fluxes
  Type weight_gain;                // Weight gain used for calculations (g/day)
  
  // Constructor
  NutrientBalance() : weight_gain(Type(0.0)) {}
};

// ============================================================================
// LOW-LEVEL FUNCTIONS
// ============================================================================

/**
 * Generic nutrient allocation in bioenergetic model
 * Calculates nutrient balance in consumption, growth, excretion and egestion
 * 
 * @param consumption Vector of consumption by prey type (g/day)
 * @param prey_nutrient_concentrations Vector of nutrient concentrations in prey (g nutrient/g wet weight)
 * @param nutrient_assimilation_efficiency Vector of nutrient assimilation efficiencies (fraction 0-1)
 * @param weight_gain Predator weight gain (g/day)
 * @param predator_nutrient_concentration Nutrient concentration in predator (g nutrient/g wet weight)
 * @return Nutrient flux structure
 */
template<class Type>
NutrientFlux<Type> calculate_nutrient_allocation(const vector<Type>& consumption,
                                                 const vector<Type>& prey_nutrient_concentrations,
                                                 const vector<Type>& nutrient_assimilation_efficiency,
                                                 Type weight_gain,
                                                 Type predator_nutrient_concentration) {
  
  NutrientFlux<Type> flux;
  
  // 1. Nutrient consumed by prey type (g nutrient/day)
  vector<Type> nutrient_consumption_by_prey(consumption.size());
  for (int i = 0; i < consumption.size(); i++) {
    nutrient_consumption_by_prey[i] = consumption[i] * prey_nutrient_concentrations[i];
  }
  
  // 2. Total nutrient consumed (g nutrient/day)
  flux.consumed = Type(0.0);
  for (int i = 0; i < nutrient_consumption_by_prey.size(); i++) {
    flux.consumed += nutrient_consumption_by_prey[i];
  }
  
  // 3. Nutrient incorporated in growth (g nutrient/day)
  flux.growth = weight_gain * predator_nutrient_concentration;
  
  // 4. Total assimilated nutrient (g nutrient/day)
  flux.assimilated = Type(0.0);
  for (int i = 0; i < consumption.size(); i++) {
    flux.assimilated += nutrient_assimilation_efficiency[i] * nutrient_consumption_by_prey[i];
  }
  
  // 5. Nutrient excretion (g nutrient/day)
  // Excreted = Assimilated - Growth
  flux.excretion = flux.assimilated - flux.growth;
  
  // 6. Nutrient egestion (g nutrient/day)
  // Egestion = Consumed - Assimilated
  flux.egestion = flux.consumed - flux.assimilated;
  
  // 7. Calculate assimilation efficiency
  flux.assimilation_efficiency = (flux.consumed > Type(0.0)) ? 
  flux.assimilated / flux.consumed : Type(0.0);
  
  return flux;
}

// ============================================================================
// MID-LEVEL FUNCTIONS
// ============================================================================

/**
 * Calculate nutrient balance - main function
 * Calculates nitrogen and phosphorus fluxes
 * 
 * @param consumption Vector of consumption by prey type (g/day)
 * @param weight_gain Predator weight gain (g/day)
 * @param prey_n_concentrations Vector of N concentrations in prey (g N/g wet weight)
 * @param prey_p_concentrations Vector of P concentrations in prey (g P/g wet weight)
 * @param predator_n_concentration N concentration in predator (g N/g wet weight)
 * @param predator_p_concentration P concentration in predator (g P/g wet weight)
 * @param n_assimilation_efficiency Vector of N assimilation efficiencies
 * @param p_assimilation_efficiency Vector of P assimilation efficiencies
 * @return Complete nutrient balance
 */
template<class Type>
NutrientBalance<Type> calculate_nutrient_balance(const vector<Type>& consumption,
                                                 Type weight_gain,
                                                 const vector<Type>& prey_n_concentrations,
                                                 const vector<Type>& prey_p_concentrations,
                                                 Type predator_n_concentration,
                                                 Type predator_p_concentration,
                                                 const vector<Type>& n_assimilation_efficiency,
                                                 const vector<Type>& p_assimilation_efficiency) {
  
  NutrientBalance<Type> balance;
  balance.weight_gain = weight_gain;
  
  // Calculate nitrogen fluxes
  balance.nitrogen = calculate_nutrient_allocation(consumption,
                                                   prey_n_concentrations,
                                                   n_assimilation_efficiency,
                                                   weight_gain,
                                                   predator_n_concentration);
  
  // Calculate phosphorus fluxes
  balance.phosphorus = calculate_nutrient_allocation(consumption,
                                                     prey_p_concentrations,
                                                     p_assimilation_efficiency,
                                                     weight_gain,
                                                     predator_p_concentration);
  
  return balance;
}

// ============================================================================
// ANALYSIS FUNCTIONS
// ============================================================================

/**
 * Calculate N:P ratios for all processes
 * 
 * @param nitrogen_flux Nitrogen flux results
 * @param phosphorus_flux Phosphorus flux results
 * @param ratio_type 0=mass ratio, 1=molar ratio
 * @return Vector of N:P ratios [consumed, growth, excretion, egestion]
 */
template<class Type>
vector<Type> calculate_np_ratios(const NutrientFlux<Type>& nitrogen_flux,
                                 const NutrientFlux<Type>& phosphorus_flux,
                                 int ratio_type = 0) {
  
  vector<Type> ratios(4);
  
  // Conversion factors for molar ratios
  Type atomic_weight_N = Type(14.007);
  Type atomic_weight_P = Type(30.974);
  
  // Process fluxes: consumed, growth, excretion, egestion
  vector<Type> n_fluxes = {nitrogen_flux.consumed, nitrogen_flux.growth, 
                           nitrogen_flux.excretion, nitrogen_flux.egestion};
  vector<Type> p_fluxes = {phosphorus_flux.consumed, phosphorus_flux.growth,
                           phosphorus_flux.excretion, phosphorus_flux.egestion};
  
  for (int i = 0; i < 4; i++) {
    Type n_flux = n_fluxes[i];
    Type p_flux = p_fluxes[i];
    
    if (p_flux == Type(0.0)) {
      ratios[i] = (n_flux == Type(0.0)) ? Type(0.0) : Type(1e6);  // Large number for Inf
    } else {
      if (ratio_type == 0) {  // mass ratio
        ratios[i] = n_flux / p_flux;
      } else {  // molar ratio
        Type mol_n = n_flux / atomic_weight_N;
        Type mol_p = p_flux / atomic_weight_P;
        ratios[i] = mol_n / mol_p;
      }
    }
  }
  
  return ratios;
}

/**
 * Calculate nutrient retention efficiencies
 * 
 * @param nitrogen_flux Nitrogen flux results
 * @param phosphorus_flux Phosphorus flux results
 * @return Vector of efficiencies [N_retention, P_retention, N_excretion_rate, P_excretion_rate]
 */
template<class Type>
vector<Type> calculate_nutrient_efficiencies(const NutrientFlux<Type>& nitrogen_flux,
                                             const NutrientFlux<Type>& phosphorus_flux) {
  
  vector<Type> efficiencies(4);
  
  // Nitrogen efficiencies
  Type n_retention_efficiency = (nitrogen_flux.consumed > Type(0.0)) ? 
  nitrogen_flux.growth / nitrogen_flux.consumed : Type(0.0);
  Type n_excretion_rate = (nitrogen_flux.consumed > Type(0.0)) ? 
  nitrogen_flux.excretion / nitrogen_flux.consumed : Type(0.0);
  
  // Phosphorus efficiencies
  Type p_retention_efficiency = (phosphorus_flux.consumed > Type(0.0)) ? 
  phosphorus_flux.growth / phosphorus_flux.consumed : Type(0.0);
  Type p_excretion_rate = (phosphorus_flux.consumed > Type(0.0)) ? 
  phosphorus_flux.excretion / phosphorus_flux.consumed : Type(0.0);
  
  efficiencies[0] = n_retention_efficiency;
  efficiencies[1] = p_retention_efficiency;
  efficiencies[2] = n_excretion_rate;
  efficiencies[3] = p_excretion_rate;
  
  return efficiencies;
}

/**
 * Calculate ecosystem impact of nutrient excretion
 * 
 * @param daily_n_excretion Daily nitrogen excretion per fish (g N/g fish/day)
 * @param daily_p_excretion Daily phosphorus excretion per fish (g P/g fish/day)
 * @param fish_biomass Total fish biomass in system (g)
 * @param water_volume Water volume of system (L)
 * @param simulation_days Number of simulation days
 * @return Vector of impact metrics [total_N_g, total_P_g, N_conc_mgL, P_conc_mgL, NP_ratio]
 */
template<class Type>
vector<Type> calculate_ecosystem_impact(Type daily_n_excretion, Type daily_p_excretion,
                                        Type fish_biomass, Type water_volume,
                                        Type simulation_days) {
  
  vector<Type> impact(5);
  
  // Total excretion for entire fish population
  Type total_n_excretion_per_day = daily_n_excretion * fish_biomass;  // g N/day
  Type total_p_excretion_per_day = daily_p_excretion * fish_biomass;  // g P/day
  
  // Cumulative excretion during simulation period
  Type total_n_excretion = total_n_excretion_per_day * simulation_days;  // g N
  Type total_p_excretion = total_p_excretion_per_day * simulation_days;  // g P
  
  // Potential water concentrations (mg/L)
  // Assuming complete mixing and no losses
  Type n_concentration_mgL = (total_n_excretion / water_volume) * Type(1000.0);
  Type p_concentration_mgL = (total_p_excretion / water_volume) * Type(1000.0);
  
  // N:P ratio in excretion
  Type excretion_np_ratio = (total_p_excretion > Type(0.0)) ? 
  total_n_excretion / total_p_excretion : Type(1e6);
  
  impact[0] = total_n_excretion;
  impact[1] = total_p_excretion;
  impact[2] = n_concentration_mgL;
  impact[3] = p_concentration_mgL;
  impact[4] = excretion_np_ratio;
  
  return impact;
}

} // namespace fb4

#endif // FB4_NUTRIENT_HPP