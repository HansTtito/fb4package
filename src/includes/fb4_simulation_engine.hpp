// #ifndef FB4_SIMULATION_ENGINE_HPP
// #define FB4_SIMULATION_ENGINE_HPP

// #include "fb4_utils.hpp"
// #include "fb4_consumption.hpp"
// #include "fb4_respiration.hpp"
// #include "fb4_egestion_excretion.hpp"
// #include "fb4_predator_energy.hpp"
// #include "fb4_body_composition.hpp"
// #include "fb4_mortality_reproduction.hpp"

// /*
//  * Complete Simulation Engine for FB4 TMB Implementation
//  * 
//  * Provides daily simulation functions that orchestrate all biological processes:
//  * consumption, metabolism, growth, and reproduction.
//  */

// namespace fb4 {

// // ============================================================================
// // DAILY CALCULATION STRUCTURES
// // ============================================================================

// /**
//  * Structure to hold daily consumption results
//  */
// template<class Type>
// struct DailyConsumption {
//   Type consumption_gg;        // Consumption rate (g prey/g fish/day)
//   Type consumption_energy;    // Energy consumption (J/g fish/day)
//   Type effective_p;          // Effective p_value used
//   Type max_consumption_gg;   // Maximum possible consumption rate
  
//   DailyConsumption() : consumption_gg(Type(0.0)), consumption_energy(Type(0.0)),
//   effective_p(Type(0.0)), max_consumption_gg(Type(0.0)) {}
// };

// /**
//  * Structure to hold daily metabolism results
//  */
// template<class Type>
// struct DailyMetabolism {
//   Type egestion_energy;      // Egestion energy (J/g fish/day)
//   Type excretion_energy;     // Excretion energy (J/g fish/day)
//   Type respiration_energy;   // Respiration energy (J/g fish/day)
//   Type respiration_o2;       // Respiration rate (g O2/g fish/day)
//   Type sda_energy;           // SDA energy (J/g fish/day)
//   Type net_energy;           // Net energy for growth (J/g fish/day)
//   Type total_indigestible;   // Total indigestible fraction
  
//   DailyMetabolism() : egestion_energy(Type(0.0)), excretion_energy(Type(0.0)),
//   respiration_energy(Type(0.0)), respiration_o2(Type(0.0)),
//   sda_energy(Type(0.0)), net_energy(Type(0.0)),
//   total_indigestible(Type(0.0)) {}
// };

// /**
//  * Structure to hold daily growth results
//  */
// template<class Type>
// struct DailyGrowth {
//   Type final_weight;         // Final weight after growth (g)
//   Type weight_change;        // Weight change (g)
//   Type energy_density;       // Final energy density (J/g)
//   Type spawn_energy;         // Energy lost to reproduction (J)
  
//   DailyGrowth() : final_weight(Type(0.0)), weight_change(Type(0.0)),
//   energy_density(Type(0.0)), spawn_energy(Type(0.0)) {}
// };

// // ============================================================================
// // LOW-LEVEL DAILY CALCULATION FUNCTIONS
// // ============================================================================

// /**
//  * Calculate daily consumption with multiple methods
//  * 
//  * @param current_weight Current fish weight (g)
//  * @param temperature Water temperature (°C)
//  * @param p_value p_value for consumption (0-5)
//  * @param ration_percent Ration as percentage of body weight
//  * @param ration_grams Ration in absolute grams per day
//  * @param method Consumption method: 0=p_value, 1=ration_percent, 2=ration_grams
//  * @param mean_prey_energy Mean prey energy density (J/g)
//  * @param consumption_params Consumption parameters
//  * @return Daily consumption results
//  */
// template<class Type>
// DailyConsumption<Type> calculate_daily_consumption_tmb(
//     Type current_weight, Type temperature, Type p_value, Type ration_percent, 
//     Type ration_grams, int method, Type mean_prey_energy,
//     // Consumption parameters
//     int CEQ, Type CA, Type CB, Type CQ, Type CTM, Type CTO, Type CTL,
//     Type CK1, Type CK4, Type CG1, Type CG2, Type CX) {
  
//   DailyConsumption<Type> result;
  
//   // Calculate maximum consumption rate
//   result.max_consumption_gg = calculate_consumption_rate(
//     temperature, current_weight, Type(1.0),
//     CEQ, CA, CB, CQ, CTM, CTO, CTL, CK1, CK4, CG1, CG2, CX
//   );
  
//   // Calculate consumption based on method
//   if (method == 1) {
//     // Ration as % of body weight
//     result.consumption_gg = ration_percent / Type(100.0);
//     result.effective_p = (result.max_consumption_gg > Type(0.0)) ? 
//     result.consumption_gg / result.max_consumption_gg : Type(0.0);
//     result.effective_p = clamp(result.effective_p, Type(0.0), Type(5.0), "effective_p");
    
//   } else if (method == 2) {
//     // Ration as absolute grams per day
//     result.consumption_gg = ration_grams / current_weight;
//     result.effective_p = (result.max_consumption_gg > Type(0.0)) ? 
//     result.consumption_gg / result.max_consumption_gg : Type(0.0);
//     result.effective_p = clamp(result.effective_p, Type(0.0), Type(5.0), "effective_p");
    
//   } else {
//     // Use p_value directly (method == 0)
//     result.effective_p = clamp(p_value, Type(0.0), Type(5.0), "p_value");
//     result.consumption_gg = calculate_consumption_rate(
//       temperature, current_weight, result.effective_p,
//       CEQ, CA, CB, CQ, CTM, CTO, CTL, CK1, CK4, CG1, CG2, CX
//     );
//   }
  
//   // Calculate energy consumption
//   result.consumption_energy = result.consumption_gg * mean_prey_energy;
  
//   // Ensure non-negative values
//   result.consumption_gg = CppAD::CondExpGe(result.consumption_gg, Type(0.0), result.consumption_gg, Type(0.0));
//   result.consumption_energy = CppAD::CondExpGe(result.consumption_energy, Type(0.0), result.consumption_energy, Type(0.0));
  
//   return result;
// }

// /**
//  * Calculate daily metabolic processes
//  * 
//  * @param consumption_energy Energy consumption (J/g fish/day)
//  * @param current_weight Current weight (g)
//  * @param temperature Temperature (°C)
//  * @param effective_p Effective p_value
//  * @param total_indigestible Total indigestible fraction
//  * @param oxycal Oxycalorific coefficient (J/g O2)
//  * @param egestion_params Egestion parameters
//  * @param excretion_params Excretion parameters
//  * @param respiration_params Respiration parameters
//  * @param SDA_coeff SDA coefficient
//  * @return Daily metabolism results
//  */
// template<class Type>
// DailyMetabolism<Type> calculate_daily_metabolism_tmb(
//     Type consumption_energy, Type current_weight, Type temperature, Type effective_p,
//     Type total_indigestible, Type oxycal,
//     // Egestion parameters
//     int EGEQ, Type FA, Type FB, Type FG,
//     // Excretion parameters
//     int EXEQ, Type UA, Type UB, Type UG,
//     // Respiration parameters
//     int REQ, Type RA, Type RB, Type RQ, Type RTO, Type RTM, Type RTL,
//     Type RK1, Type RK4, Type RK5, Type RX, Type ACT, Type BACT,
//     // SDA parameter
//     Type SDA_coeff) {
  
//   DailyMetabolism<Type> result;
//   result.total_indigestible = total_indigestible;
  
//   // Calculate egestion
//   result.egestion_energy = calculate_egestion_rate(
//     consumption_energy, temperature, effective_p, total_indigestible,
//     EGEQ, FA, FB, FG
//   );
  
//   // Calculate excretion
//   result.excretion_energy = calculate_excretion_rate(
//     consumption_energy, result.egestion_energy, temperature, effective_p, current_weight,
//     EXEQ, UA, UB, UG
//   );
  
//   // Calculate respiration
//   result.respiration_o2 = calculate_respiration_rate(
//     temperature, current_weight,
//     REQ, RA, RB, RQ, RTO, RTM, RTL, RK1, RK4, RK5, RX, ACT, BACT
//   );
  
//   // Convert respiration to energy units
//   result.respiration_energy = convert_respiration_to_energy(result.respiration_o2, oxycal);
  
//   // Calculate SDA
//   result.sda_energy = calculate_sda_energy(consumption_energy, result.egestion_energy, SDA_coeff);
  
//   // Calculate net energy for growth
//   result.net_energy = consumption_energy - result.egestion_energy - result.excretion_energy - 
//     result.respiration_energy - result.sda_energy;
  
//   return result;
// }

// /**
//  * Calculate daily growth and reproduction
//  * 
//  * @param current_weight Current weight (g)
//  * @param net_energy Net energy available (J/g fish/day)
//  * @param day Current simulation day
//  * @param reproduction_data Reproduction data vector
//  * @param predator_params Predator energy density parameters
//  * @return Daily growth results
//  */
// template<class Type>
// DailyGrowth<Type> calculate_daily_growth_tmb(
//     Type current_weight, Type net_energy, int day, 
//     const vector<Type>& reproduction_data,
//     // Predator energy density parameters
//     int PREDEDEQ, Type Alpha1, Type Beta1, Type Alpha2, Type Beta2, 
//     Type Cutoff, const vector<Type>& ED_data) {
  
//   DailyGrowth<Type> result;
  
//   // Calculate current energy density
//   result.energy_density = calculate_predator_energy_density(
//     current_weight, day, PREDEDEQ, Alpha1, Beta1, Alpha2, Beta2, Cutoff, ED_data
//   );
  
//   // Calculate spawning energy loss
//   result.spawn_energy = calculate_daily_reproduction_energy(
//     day, current_weight, result.energy_density, reproduction_data
//   );
  
//   // Calculate weight change
//   Type total_energy_gain = net_energy * current_weight;  // Convert to total energy (J)
  
//   WeightChangeResult<Type> weight_result = calculate_final_weight_fb4(
//     current_weight, total_energy_gain, result.spawn_energy, day,
//     PREDEDEQ, Alpha1, Beta1, Alpha2, Beta2, Cutoff, ED_data
//   );
  
//   result.final_weight = weight_result.final_weight;
//   result.weight_change = weight_result.weight_change;
//   result.energy_density = weight_result.final_energy_density;
  
//   // Ensure minimum viable weight
//   result.final_weight = CppAD::CondExpGe(result.final_weight, Type(0.01), result.final_weight, Type(0.01));
  
//   return result;
// }

// // ============================================================================
// // MID-LEVEL SIMULATION FUNCTIONS
// // ============================================================================

// /**
//  * Execute single day simulation
//  * 
//  * @param day Current simulation day (1-based)
//  * @param current_weight Current fish weight (g)
//  * @param p_value p_value for consumption
//  * @param ration_percent Ration as percentage (if using)
//  * @param ration_grams Ration in grams (if using)
//  * @param method Consumption method
//  * @param temporal_data All temporal environmental data
//  * @param species_params All species parameters
//  * @param oxycal Oxycalorific coefficient
//  * @return Final weight after daily simulation
//  */
// template<class Type>
// Type execute_daily_simulation_tmb(
//     int day, Type current_weight, Type p_value, Type ration_percent, Type ration_grams, int method,
//     // Temporal data
//     const vector<Type>& temperature_data, const matrix<Type>& diet_proportions,
//     const matrix<Type>& prey_energies, const matrix<Type>& prey_indigestible,
//     const vector<Type>& reproduction_data, Type oxycal,
//     // Species parameters - Consumption
//     int CEQ, Type CA, Type CB, Type CQ, Type CTM, Type CTO, Type CTL,
//     Type CK1, Type CK4, Type CG1, Type CG2, Type CX,
//     // Egestion parameters
//     int EGEQ, Type FA, Type FB, Type FG,
//     // Excretion parameters
//     int EXEQ, Type UA, Type UB, Type UG,
//     // Respiration parameters
//     int REQ, Type RA, Type RB, Type RQ, Type RTO, Type RTM, Type RTL,
//     Type RK1, Type RK4, Type RK5, Type RX, Type ACT, Type BACT,
//     // Predator energy parameters
//     int PREDEDEQ, Type Alpha1, Type Beta1, Type Alpha2, Type Beta2, 
//     Type Cutoff, const vector<Type>& ED_data,
//     // SDA parameter
//     Type SDA_coeff) {
  
//   // Convert to 0-based indexing for data access
//   int day_idx = day - 1;
  
//   // Extract daily environmental data
//   Type temperature = temperature_data[day_idx];
  
//   // Calculate mean prey energy for this day
//   Type mean_prey_energy = Type(0.0);
//   for (int i = 0; i < diet_proportions.cols(); i++) {
//     mean_prey_energy += diet_proportions(day_idx, i) * prey_energies(day_idx, i);
//   }
  
//   // Calculate total indigestible fraction
//   Type total_indigestible = Type(0.0);
//   for (int i = 0; i < diet_proportions.cols(); i++) {
//     total_indigestible += diet_proportions(day_idx, i) * prey_indigestible(day_idx, i);
//   }
  
//   // Calculate daily consumption
//   DailyConsumption<Type> consumption_result = calculate_daily_consumption_tmb(
//     current_weight, temperature, p_value, ration_percent, ration_grams, method, mean_prey_energy,
//     CEQ, CA, CB, CQ, CTM, CTO, CTL, CK1, CK4, CG1, CG2, CX
//   );
  
//   // Calculate daily metabolism
//   DailyMetabolism<Type> metabolism_result = calculate_daily_metabolism_tmb(
//     consumption_result.consumption_energy, current_weight, temperature, 
//     consumption_result.effective_p, total_indigestible, oxycal,
//     EGEQ, FA, FB, FG,
//     EXEQ, UA, UB, UG,
//     REQ, RA, RB, RQ, RTO, RTM, RTL, RK1, RK4, RK5, RX, ACT, BACT,
//     SDA_coeff
//   );
  
//   // Calculate daily growth
//   DailyGrowth<Type> growth_result = calculate_daily_growth_tmb(
//     current_weight, metabolism_result.net_energy, day, reproduction_data,
//     PREDEDEQ, Alpha1, Beta1, Alpha2, Beta2, Cutoff, ED_data
//   );
  
//   return growth_result.final_weight;
// }

// // ============================================================================
// // HIGH-LEVEL SIMULATION FUNCTION
// // ============================================================================

// /**
//  * Run complete FB4 simulation
//  * 
//  * @param initial_weight Initial fish weight (g)
//  * @param n_days Number of simulation days
//  * @param p_value p_value for consumption (if using)
//  * @param ration_percent Ration as percentage (if using)
//  * @param ration_grams Ration in grams (if using)
//  * @param method Consumption method
//  * @param temporal_data All temporal environmental data
//  * @param species_params All species parameters
//  * @param oxycal Oxycalorific coefficient
//  * @return Vector of daily weights [initial_weight, weight_day1, weight_day2, ...]
//  */
// template<class Type>
// vector<Type> run_fb4_simulation_tmb(
//     Type initial_weight, int n_days, Type p_value, Type ration_percent, Type ration_grams, int method,
//     // Temporal data
//     const vector<Type>& temperature_data, const matrix<Type>& diet_proportions,
//     const matrix<Type>& prey_energies, const matrix<Type>& prey_indigestible,
//     const vector<Type>& reproduction_data, Type oxycal,
//     // Species parameters - Consumption
//     int CEQ, Type CA, Type CB, Type CQ, Type CTM, Type CTO, Type CTL,
//     Type CK1, Type CK4, Type CG1, Type CG2, Type CX,
//     // Egestion parameters
//     int EGEQ, Type FA, Type FB, Type FG,
//     // Excretion parameters
//     int EXEQ, Type UA, Type UB, Type UG,
//     // Respiration parameters
//     int REQ, Type RA, Type RB, Type RQ, Type RTO, Type RTM, Type RTL,
//     Type RK1, Type RK4, Type RK5, Type RX, Type ACT, Type BACT,
//     // Predator energy parameters
//     int PREDEDEQ, Type Alpha1, Type Beta1, Type Alpha2, Type Beta2, 
//     Type Cutoff, const vector<Type>& ED_data,
//     // SDA parameter
//     Type SDA_coeff) {
  
//   // Initialize results vector (include initial weight)
//   vector<Type> weights(n_days + 1);
//   weights[0] = initial_weight;
  
//   Type current_weight = initial_weight;
  
//   // Main simulation loop
//   for (int day = 1; day <= n_days; day++) {
    
//     // Execute daily simulation
//     Type final_weight = execute_daily_simulation_tmb(
//       day, current_weight, p_value, ration_percent, ration_grams, method,
//       temperature_data, diet_proportions, prey_energies, prey_indigestible, 
//       reproduction_data, oxycal,
//       CEQ, CA, CB, CQ, CTM, CTO, CTL, CK1, CK4, CG1, CG2, CX,
//       EGEQ, FA, FB, FG,
//       EXEQ, UA, UB, UG,
//       REQ, RA, RB, RQ, RTO, RTM, RTL, RK1, RK4, RK5, RX, ACT, BACT,
//       PREDEDEQ, Alpha1, Beta1, Alpha2, Beta2, Cutoff, ED_data,
//       SDA_coeff
//     );
    
//     // Update state for next day
//     current_weight = CppAD::CondExpGe(final_weight, Type(0.01), final_weight, Type(0.01));
//     weights[day] = current_weight;
    
//     // Check for fish mortality
//     if (current_weight <= Type(0.01)) {
//       // Fill remaining days with minimum weight
//       for (int remaining_day = day + 1; remaining_day <= n_days; remaining_day++) {
//         weights[remaining_day] = Type(0.01);
//       }
//       break;
//     }
    
//     // Check for unrealistic growth (optional safeguard)
//     if (current_weight > initial_weight * Type(50.0)) {
//       // Cap unrealistic growth
//       current_weight = initial_weight * Type(50.0);
//       weights[day] = current_weight;
//     }
//   }
  
//   return weights;
// }

// } // namespace fb4

// #endif // FB4_SIMULATION_ENGINE_HPP