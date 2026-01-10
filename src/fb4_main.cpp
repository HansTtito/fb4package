#include <TMB.hpp>

// Include all FB4 headers
#include "includes/fb4_utils.hpp"
#include "includes/fb4_consumption.hpp"
#include "includes/fb4_respiration.hpp"
#include "includes/fb4_egestion_excretion.hpp"
#include "includes/fb4_predator_energy.hpp"
#include "includes/fb4_body_composition.hpp"
#include "includes/fb4_contaminant_accumulation.hpp"
#include "includes/fb4_nutrient_regeneration.hpp"
#include "includes/fb4_mortality_reproduction.hpp"
#include "includes/fb4_validation.hpp"

/*
 * FB4 TMB Implementation - Basic and Hierarchical Models
 * 
 * Supports both traditional single p_value estimation and 
 * hierarchical modeling with individual variation in p_values.
 * Includes comprehensive uncertainty propagation via delta method.
 */

template<class Type>
Type objective_function<Type>::operator() () {  
  
  // ========================================================================
  // MODEL TYPE CONTROL
  // ========================================================================
  DATA_STRING(model_type);  // "basic" or "hierarchical"
  
  if (model_type == "basic") {
    
    // ====================================================================
    // BASIC MODEL (UPDATED IMPLEMENTATION)
    // ====================================================================
    
    // Parameter declarations
    PARAMETER(log_p_value);
    PARAMETER(log_sigma);
    
    Type p_value = exp(log_p_value);
    Type sigma = exp(log_sigma);
    
    // Data declarations - single individual
    DATA_VECTOR(observed_weights);
    DATA_SCALAR(initial_weight);
    DATA_INTEGER(n_days);
    DATA_SCALAR(oxycal);
    
    // Temporal data
    DATA_VECTOR(temperature);
    DATA_MATRIX(diet_proportions);
    DATA_MATRIX(prey_energies);
    DATA_MATRIX(prey_indigestible);
    DATA_VECTOR(reproduction_data);
    
    // Species parameters - Consumption
    DATA_INTEGER(CEQ);
    DATA_SCALAR(CA); DATA_SCALAR(CB); DATA_SCALAR(CQ);
    DATA_SCALAR(CTM); DATA_SCALAR(CTO); DATA_SCALAR(CTL);
    DATA_SCALAR(CK1); DATA_SCALAR(CK4);
    DATA_SCALAR(CG1); DATA_SCALAR(CG2); DATA_SCALAR(CX);
    
    // Species parameters - Respiration
    DATA_INTEGER(REQ);
    DATA_SCALAR(RA); DATA_SCALAR(RB); DATA_SCALAR(RQ);
    DATA_SCALAR(RTO); DATA_SCALAR(RTM); DATA_SCALAR(RTL);
    DATA_SCALAR(RK1); DATA_SCALAR(RK4); DATA_SCALAR(RK5); DATA_SCALAR(RX);
    
    // Activity and SDA
    DATA_SCALAR(ACT); DATA_SCALAR(BACT); DATA_SCALAR(SDA);
    
    // Egestion
    DATA_INTEGER(EGEQ);
    DATA_SCALAR(FA); DATA_SCALAR(FB); DATA_SCALAR(FG);
    
    // Excretion
    DATA_INTEGER(EXEQ);
    DATA_SCALAR(UA); DATA_SCALAR(UB); DATA_SCALAR(UG);
    
    // Predator Energy Density
    DATA_INTEGER(PREDEDEQ);
    DATA_SCALAR(Alpha1); DATA_SCALAR(Beta1);
    DATA_SCALAR(Alpha2); DATA_SCALAR(Beta2);
    DATA_SCALAR(Cutoff);
    DATA_VECTOR(ED_data);
    
    // Optional parameters
    DATA_SCALAR(water_fraction);
    DATA_SCALAR(fat_energy);
    DATA_SCALAR(protein_energy);
    DATA_SCALAR(max_fat_fraction);
    
    // Parameter validation
    if (initial_weight <= Type(0.0) || initial_weight > Type(1e6) || 
        p_value <= Type(0.0) || p_value > Type(5.0) ||
        temperature[0] < Type(-5.0) || temperature[0] > Type(45.0)) {
      return Type(1e10);
    }
    
    // ========================================================================
    // SIMULATION PREPARATION
    // ========================================================================
    
    // Initialize simulation variables
    Type current_weight = initial_weight;
    Type total_consumption_g = Type(0.0);
    Type total_consumption_energy = Type(0.0);
    Type total_respiration_energy = Type(0.0);
    Type total_egestion_energy = Type(0.0);
    Type total_excretion_energy = Type(0.0);
    Type total_sda_energy = Type(0.0);
    Type total_net_energy = Type(0.0);
    Type total_spawn_energy = Type(0.0);
    
    // Daily tracking vectors for detailed output
    vector<Type> daily_weights(n_days + 1);
    vector<Type> daily_consumption(n_days);
    vector<Type> daily_growth_rates(n_days);
    vector<Type> daily_p_effective(n_days);
    
    daily_weights[0] = current_weight;
    
    // ========================================================================
    // MAIN SIMULATION LOOP
    // ========================================================================
    
    for (int day = 0; day < n_days; day++) {
      
      // Extract daily data
      Type temp_day = temperature[day];
      vector<Type> diet_props = diet_proportions.row(day);
      vector<Type> prey_energies_day = prey_energies.row(day);
      vector<Type> indigestible_day = prey_indigestible.row(day);
      Type reproduction_fraction = reproduction_data[day];
      
      // Calculate mean prey energy
      Type mean_prey_energy = Type(0.0);
      for (int i = 0; i < diet_props.size(); i++) {
        mean_prey_energy += diet_props[i] * prey_energies_day[i];
      }
      
      // Calculate total indigestible fraction
      Type total_indigestible = Type(0.0);
      for (int i = 0; i < diet_props.size(); i++) {
        total_indigestible += diet_props[i] * indigestible_day[i];
      }
      
      // ====================================================================
      // DAILY CONSUMPTION CALCULATION
      // ====================================================================
      
      // Calculate maximum consumption
      Type max_consumption_gg = fb4::calculate_consumption_rate(
        temp_day, current_weight, Type(1.0), 
        CEQ, CA, CB, CQ, CTM, CTO, CTL, CK1, CK4, CG1, CG2, CX
      );
      
      // Calculate actual consumption with p_value
      Type consumption_gg = fb4::calculate_consumption_rate(
        temp_day, current_weight, p_value,
        CEQ, CA, CB, CQ, CTM, CTO, CTL, CK1, CK4, CG1, CG2, CX
      );
      
      Type consumption_energy = consumption_gg * mean_prey_energy;
      
      // Store daily values
      daily_consumption[day] = consumption_gg * current_weight;  // Total daily consumption (g)
      daily_p_effective[day] = (max_consumption_gg > Type(0.0)) ? 
      consumption_gg / max_consumption_gg : Type(0.0);
      
      // ====================================================================
      // DAILY METABOLISM CALCULATION
      // ====================================================================
      
      // Egestion
      Type egestion_energy = fb4::calculate_egestion_rate(
        consumption_energy, temp_day, p_value, total_indigestible,
        EGEQ, FA, FB, FG
      );
      
      // Excretion  
      Type excretion_energy = fb4::calculate_excretion_rate(
        consumption_energy, egestion_energy, temp_day, p_value, current_weight,
        EXEQ, UA, UB, UG
      );
      
      // Respiration
      Type respiration_o2 = fb4::calculate_respiration_rate(
        temp_day, current_weight,
        REQ, RA, RB, RQ, RTO, RTM, RTL, RK1, RK4, RK5, RX, ACT, BACT
      );
      
      Type respiration_energy = fb4::convert_respiration_to_energy(respiration_o2, oxycal);
      
      // SDA (Specific Dynamic Action)
      Type sda_energy = fb4::calculate_sda_energy(consumption_energy, egestion_energy, SDA);
      
      // Net energy for growth
      Type net_energy = consumption_energy - egestion_energy - excretion_energy - respiration_energy - sda_energy;
      
      // ====================================================================
      // REPRODUCTION AND GROWTH - UPDATED WITH NEW FUNCTION
      // ====================================================================
      
      // Calculate current energy density
      Type current_ed = fb4::calculate_predator_energy_density(
        current_weight, day, 
        PREDEDEQ, Alpha1, Beta1, Alpha2, Beta2, Cutoff, ED_data
      );
      
      // Spawning energy loss
      Type spawn_energy = reproduction_fraction * current_weight * current_ed;
      
      // Calculate final weight using FB4 equations
      Type total_energy_gain = net_energy * current_weight;  // Convert to total energy (J)
      Type net_energy_for_growth = total_energy_gain - spawn_energy;
      
      Type new_weight = fb4::calculate_final_weight_simple(
        current_weight, net_energy_for_growth, current_ed, day,
        PREDEDEQ, Alpha1, Beta1, Alpha2, Beta2, Cutoff, ED_data
      );
      
      // Calculate daily growth rate
      Type weight_change = new_weight - current_weight;
      daily_growth_rates[day] = (current_weight > Type(0.0)) ? 
        weight_change / current_weight : Type(0.0);
      
      // ====================================================================
      // ACCUMULATE TOTALS
      // ====================================================================
      
      total_consumption_g += daily_consumption[day];
      total_consumption_energy += consumption_energy * current_weight;
      total_respiration_energy += respiration_energy * current_weight;
      total_egestion_energy += egestion_energy * current_weight;
      total_excretion_energy += excretion_energy * current_weight;
      total_sda_energy += sda_energy * current_weight;
      total_net_energy += net_energy * current_weight;
      total_spawn_energy += spawn_energy;
      
      // Update for next day
      current_weight = new_weight;
      daily_weights[day + 1] = current_weight;
    }
    
    // ========================================================================
    // DERIVED VARIABLES CALCULATION
    // ========================================================================
    
    Type final_weight = current_weight;
    Type total_growth = final_weight - initial_weight;
    Type relative_growth = (final_weight / initial_weight - Type(1.0)) * Type(100.0);
    
    // Efficiency calculations
    Type gross_growth_efficiency = (total_consumption_energy > Type(0.0)) ? 
    total_net_energy / total_consumption_energy : Type(0.0);
    
    Type metabolic_scope = (total_consumption_energy > Type(0.0)) ? 
    (total_respiration_energy + total_sda_energy) / total_consumption_energy : Type(0.0);
    
    Type mean_daily_consumption = total_consumption_g / Type(n_days);
    Type mean_specific_consumption = (initial_weight > Type(0.0)) ? 
    mean_daily_consumption / initial_weight : Type(0.0);
    
    Type mean_growth_rate = Type(0.0);
    for (int i = 0; i < n_days; i++) {
      mean_growth_rate += daily_growth_rates[i];
    }
    mean_growth_rate /= Type(n_days);
    
    Type specific_growth_rate = mean_growth_rate * Type(100.0);  // % per day
    
    // Energy budget proportions
    Type prop_respiration = (total_consumption_energy > Type(0.0)) ? 
    total_respiration_energy / total_consumption_energy : Type(0.0);
    Type prop_egestion = (total_consumption_energy > Type(0.0)) ? 
    total_egestion_energy / total_consumption_energy : Type(0.0);
    Type prop_excretion = (total_consumption_energy > Type(0.0)) ? 
    total_excretion_energy / total_consumption_energy : Type(0.0);
    Type prop_sda = (total_consumption_energy > Type(0.0)) ? 
    total_sda_energy / total_consumption_energy : Type(0.0);
    Type prop_growth = (total_consumption_energy > Type(0.0)) ? 
    total_net_energy / total_consumption_energy : Type(0.0);
    
    // Final energy density
    Type final_energy_density = fb4::calculate_predator_energy_density(
      final_weight, n_days - 1,
      PREDEDEQ, Alpha1, Beta1, Alpha2, Beta2, Cutoff, ED_data
    );
    
    // ========================================================================
    // LIKELIHOOD CALCULATION
    // ========================================================================

    // Likelihood calculation
    Type negative_log_likelihood = Type(0.0);
    for (int i = 0; i < observed_weights.size(); i++) {
      Type log_obs = log(observed_weights[i]);
      Type log_pred = log(final_weight);
      Type log_density = -Type(0.5) * log(Type(2.0) * Type(M_PI)) 
        - log(sigma) - log(observed_weights[i])
        - Type(0.5) * pow((log_obs - log_pred) / sigma, Type(2.0));
        negative_log_likelihood -= log_density;
    }
    
    // Priors
    negative_log_likelihood -= dnorm(log_p_value, Type(0.0), Type(2.0), true);
    negative_log_likelihood -= dnorm(log_sigma, Type(-2.3), Type(1.0), true);
    
    // ========================================================================
    // REPORTS WITH UNCERTAINTY PROPAGATION - BASIC MODEL
    // ========================================================================
    
    // Primary parameters
    ADREPORT(p_value);
    ADREPORT(sigma);
    
    // Growth and weight variables
    ADREPORT(final_weight);
    ADREPORT(total_growth);
    ADREPORT(relative_growth);
    ADREPORT(mean_growth_rate);
    ADREPORT(specific_growth_rate);
    
    // Consumption variables
    ADREPORT(total_consumption_g);
    ADREPORT(mean_daily_consumption);
    ADREPORT(mean_specific_consumption);
    
    // Energy budget variables
    ADREPORT(total_consumption_energy);
    ADREPORT(total_respiration_energy);
    ADREPORT(total_egestion_energy);
    ADREPORT(total_excretion_energy);
    ADREPORT(total_sda_energy);
    ADREPORT(total_net_energy);
    ADREPORT(total_spawn_energy);
    
    // Efficiency and scope variables
    ADREPORT(gross_growth_efficiency);
    ADREPORT(metabolic_scope);
    
    // Energy budget proportions
    ADREPORT(prop_respiration);
    ADREPORT(prop_egestion);
    ADREPORT(prop_excretion);
    ADREPORT(prop_sda);
    ADREPORT(prop_growth);
    
    // Final energy density
    ADREPORT(final_energy_density);

    // Standard REPORT for direct access (without uncertainty)
    REPORT(p_value);
    REPORT(sigma);
    REPORT(final_weight);
    REPORT(total_consumption_g);
    REPORT(gross_growth_efficiency);
    REPORT(total_growth);
    REPORT(relative_growth);

    return negative_log_likelihood;
    
  } else if (model_type == "hierarchical") {
    
    // ====================================================================
    // HIERARCHICAL MODEL WITH INDIVIDUAL-LEVEL UNCERTAINTY - UPDATED
    // ====================================================================
    
    // Generic structure for any number of covariates
    DATA_MATRIX(covariates_matrix);  
    DATA_INTEGER(n_covariates);      // Number of covariates (can be 0)
    PARAMETER_VECTOR(betas);         // Covariate effects
    
    // Hierarchical parameter declarations
    PARAMETER(log_mu_p);              // Population mean of log(p_value)
    PARAMETER(log_sigma_p);           // Population SD of log(p_value)
    PARAMETER_VECTOR(log_p_individual); // Individual log(p_values)
    PARAMETER(log_sigma_obs);         // Observation error
    
    // Transform to natural scale
    Type sigma_p = exp(log_sigma_p);
    Type sigma_obs = exp(log_sigma_obs);
    
    // Data declarations - multiple individuals
    DATA_VECTOR(observed_weights_matrix);  // One final weight per individual
    DATA_VECTOR(initial_weights);          // Vector of initial weights
    DATA_INTEGER(n_individuals);
    DATA_INTEGER(n_days);
    DATA_SCALAR(oxycal);
    
    // Environmental data (same for all individuals)
    DATA_VECTOR(temperature);
    DATA_MATRIX(diet_proportions);
    DATA_MATRIX(prey_energies);
    DATA_MATRIX(prey_indigestible);
    DATA_VECTOR(reproduction_data);
    
    // Species parameters (same as basic model)
    DATA_INTEGER(CEQ);
    DATA_SCALAR(CA); DATA_SCALAR(CB); DATA_SCALAR(CQ);
    DATA_SCALAR(CTM); DATA_SCALAR(CTO); DATA_SCALAR(CTL);
    DATA_SCALAR(CK1); DATA_SCALAR(CK4);
    DATA_SCALAR(CG1); DATA_SCALAR(CG2); DATA_SCALAR(CX);
    
    DATA_INTEGER(REQ);
    DATA_SCALAR(RA); DATA_SCALAR(RB); DATA_SCALAR(RQ);
    DATA_SCALAR(RTO); DATA_SCALAR(RTM); DATA_SCALAR(RTL);
    DATA_SCALAR(RK1); DATA_SCALAR(RK4); DATA_SCALAR(RK5); DATA_SCALAR(RX);
    
    DATA_SCALAR(ACT); DATA_SCALAR(BACT); DATA_SCALAR(SDA);
    
    DATA_INTEGER(EGEQ);
    DATA_SCALAR(FA); DATA_SCALAR(FB); DATA_SCALAR(FG);
    
    DATA_INTEGER(EXEQ);
    DATA_SCALAR(UA); DATA_SCALAR(UB); DATA_SCALAR(UG);
    
    DATA_INTEGER(PREDEDEQ);
    DATA_SCALAR(Alpha1); DATA_SCALAR(Beta1);
    DATA_SCALAR(Alpha2); DATA_SCALAR(Beta2);
    DATA_SCALAR(Cutoff);
    DATA_VECTOR(ED_data);
    
    DATA_SCALAR(water_fraction);
    DATA_SCALAR(fat_energy);
    DATA_SCALAR(protein_energy);
    DATA_SCALAR(max_fat_fraction);
    
    // Dimension validation
    if (log_p_individual.size() != n_individuals ||
        initial_weights.size() != n_individuals ||
        observed_weights_matrix.size() != n_individuals ||
        temperature[0] < Type(-5.0) || temperature[0] > Type(45.0)) {
      return Type(1e10);
    }
    
    if (n_covariates > 0) {
      if (betas.size() != n_covariates ||
          covariates_matrix.rows() != n_individuals ||
          covariates_matrix.cols() != n_covariates) {
        return Type(1e10);
      }
    } else {
      // No covariates: betas should be empty
      if (betas.size() != 0) {
        return Type(1e10);
      }
    }

    Type total_negative_log_likelihood = Type(0.0);
    
    // Storage for individual results - vectors for uncertainty propagation
    vector<Type> final_weights(n_individuals);
    vector<Type> individual_p_values(n_individuals);
    vector<Type> individual_total_consumption(n_individuals);
    vector<Type> individual_total_growth(n_individuals);
    vector<Type> individual_relative_growth(n_individuals);
    vector<Type> individual_gross_efficiency(n_individuals);
    vector<Type> individual_metabolic_scope(n_individuals);
    vector<Type> individual_final_energy_density(n_individuals);
    
    // Individual energy budget components
    vector<Type> individual_respiration_energy(n_individuals);
    vector<Type> individual_egestion_energy(n_individuals);
    vector<Type> individual_excretion_energy(n_individuals);
    vector<Type> individual_sda_energy(n_individuals);
    vector<Type> individual_net_energy(n_individuals);
    vector<Type> individual_spawn_energy(n_individuals);

    // ====================================================================
    // LOOP OVER INDIVIDUALS
    // ====================================================================
    
    for (int ind = 0; ind < n_individuals; ind++) {
      
      // Linear predictor for this individual
      Type linear_predictor = log_mu_p;
      for(int k = 0; k < n_covariates; k++) {
        linear_predictor += betas[k] * covariates_matrix(ind, k);
      }
      
      Type p_value_ind = exp(log_p_individual[ind]);
      
      // Individual-specific parameters
      Type initial_weight_ind = initial_weights[ind];
      individual_p_values[ind] = p_value_ind;
      
      // Basic validation for this individual
      if (initial_weight_ind <= Type(0.0) || initial_weight_ind > Type(1e6) || 
          p_value_ind <= Type(0.0) || p_value_ind > Type(5.0)) {
        return Type(1e10);
      }
      
      // ================================================================
      // INDIVIDUAL SIMULATION - UPDATED WITH NEW FUNCTION
      // ================================================================
      
      // Initialize simulation for this individual
      Type current_weight = initial_weight_ind;
      Type total_consumption_g = Type(0.0);
      Type total_consumption_energy = Type(0.0);
      Type total_respiration_energy = Type(0.0);
      Type total_egestion_energy = Type(0.0);
      Type total_excretion_energy = Type(0.0);
      Type total_sda_energy = Type(0.0);
      Type total_net_energy = Type(0.0);
      Type total_spawn_energy = Type(0.0);
      
      // Daily simulation loop for this individual
      for (int day = 0; day < n_days; day++) {
        
        Type temp_day = temperature[day];
        vector<Type> diet_props = diet_proportions.row(day);
        vector<Type> prey_energies_day = prey_energies.row(day);
        vector<Type> indigestible_day = prey_indigestible.row(day);
        Type reproduction_fraction = reproduction_data[day];
        
        // Calculate mean prey energy
        Type mean_prey_energy = Type(0.0);
        for (int i = 0; i < diet_props.size(); i++) {
          mean_prey_energy += diet_props[i] * prey_energies_day[i];
        }
        
        // Calculate total indigestible fraction
        Type total_indigestible = Type(0.0);
        for (int i = 0; i < diet_props.size(); i++) {
          total_indigestible += diet_props[i] * indigestible_day[i];
        }
        
        // Daily consumption calculation
        Type consumption_gg = fb4::calculate_consumption_rate(
          temp_day, current_weight, p_value_ind,
          CEQ, CA, CB, CQ, CTM, CTO, CTL, CK1, CK4, CG1, CG2, CX
        );
        
        Type consumption_energy = consumption_gg * mean_prey_energy;
        
        // Daily metabolism calculation
        Type egestion_energy = fb4::calculate_egestion_rate(
          consumption_energy, temp_day, p_value_ind, total_indigestible,
          EGEQ, FA, FB, FG
        );
        
        Type excretion_energy = fb4::calculate_excretion_rate(
          consumption_energy, egestion_energy, temp_day, p_value_ind, current_weight,
          EXEQ, UA, UB, UG
        );
        
        Type respiration_o2 = fb4::calculate_respiration_rate(
          temp_day, current_weight,
          REQ, RA, RB, RQ, RTO, RTM, RTL, RK1, RK4, RK5, RX, ACT, BACT
        );
        
        Type respiration_energy = fb4::convert_respiration_to_energy(respiration_o2, oxycal);
        Type sda_energy = fb4::calculate_sda_energy(consumption_energy, egestion_energy, SDA);
        Type net_energy = consumption_energy - egestion_energy - excretion_energy - 
          respiration_energy - sda_energy;
        
        // Growth calculation - UPDATED WITH NEW FUNCTION
        Type current_ed = fb4::calculate_predator_energy_density(
          current_weight, day, 
          PREDEDEQ, Alpha1, Beta1, Alpha2, Beta2, Cutoff, ED_data
        );
        
        Type spawn_energy = reproduction_fraction * current_weight * current_ed;
        Type total_energy_gain = net_energy * current_weight;
        Type net_energy_for_growth = total_energy_gain - spawn_energy;
        
        Type new_weight = fb4::calculate_final_weight_simple(
          current_weight, net_energy_for_growth, current_ed, day,
          PREDEDEQ, Alpha1, Beta1, Alpha2, Beta2, Cutoff, ED_data
        );
        
        // Update for next day
        current_weight = new_weight;
        
        // Accumulate totals for this individual
        total_consumption_g += consumption_gg * current_weight;
        total_consumption_energy += consumption_energy * current_weight;
        total_respiration_energy += respiration_energy * current_weight;
        total_egestion_energy += egestion_energy * current_weight;
        total_excretion_energy += excretion_energy * current_weight;
        total_sda_energy += sda_energy * current_weight;
        total_net_energy += net_energy * current_weight;
        total_spawn_energy += spawn_energy;
      }
      
      // Final calculations for this individual
      Type final_weight_ind = current_weight;
      final_weights[ind] = final_weight_ind;
      
      // Store individual-level derived variables
      individual_total_consumption[ind] = total_consumption_g;
      individual_total_growth[ind] = final_weight_ind - initial_weight_ind;
      individual_relative_growth[ind] = (final_weight_ind / initial_weight_ind - Type(1.0)) * Type(100.0);
      
      // Individual efficiency calculations
      individual_gross_efficiency[ind] = (total_consumption_energy > Type(0.0)) ? 
        total_net_energy / total_consumption_energy : Type(0.0);
      individual_metabolic_scope[ind] = (total_consumption_energy > Type(0.0)) ? 
        (total_respiration_energy + total_sda_energy) / total_consumption_energy : Type(0.0);
      
      // Individual energy budget components
      individual_respiration_energy[ind] = total_respiration_energy;
      individual_egestion_energy[ind] = total_egestion_energy;
      individual_excretion_energy[ind] = total_excretion_energy;
      individual_sda_energy[ind] = total_sda_energy;
      individual_net_energy[ind] = total_net_energy;
      individual_spawn_energy[ind] = total_spawn_energy;
      
      // Final energy density for this individual
      individual_final_energy_density[ind] = fb4::calculate_predator_energy_density(
        final_weight_ind, n_days - 1,
        PREDEDEQ, Alpha1, Beta1, Alpha2, Beta2, Cutoff, ED_data
      );
      
      // ================================================================
      // LIKELIHOOD FOR THIS INDIVIDUAL
      // ================================================================
      
      Type obs_weight_ind = observed_weights_matrix[ind];
      
      if (obs_weight_ind > Type(0.0)) {
        Type log_obs = log(obs_weight_ind);
        Type log_pred = log(final_weight_ind);
        Type log_density = -Type(0.5) * log(Type(2.0) * Type(M_PI)) 
          - log(sigma_obs) - log(obs_weight_ind)
          - Type(0.5) * pow((log_obs - log_pred) / sigma_obs, Type(2.0));
          total_negative_log_likelihood -= log_density;
      }
      
      // Random effect likelihood for this individual
      total_negative_log_likelihood -= dnorm(log_p_individual[ind], linear_predictor, sigma_p, true);
    }
    
    // ====================================================================
    // POPULATION-LEVEL DERIVED VARIABLES
    // ====================================================================
    
    // Calculate population means and uncertainties
    Type mean_final_weight = Type(0.0);
    Type mean_total_consumption = Type(0.0);
    Type mean_total_growth = Type(0.0);
    Type mean_relative_growth = Type(0.0);
    Type mean_gross_efficiency = Type(0.0);
    Type mean_metabolic_scope = Type(0.0);
    Type mean_final_energy_density = Type(0.0);
    
    Type mean_respiration_energy = Type(0.0);
    Type mean_egestion_energy = Type(0.0);
    Type mean_excretion_energy = Type(0.0);
    Type mean_sda_energy = Type(0.0);
    Type mean_net_energy = Type(0.0);
    Type mean_spawn_energy = Type(0.0);
    
    for (int i = 0; i < n_individuals; i++) {
      mean_final_weight += final_weights[i];
      mean_total_consumption += individual_total_consumption[i];
      mean_total_growth += individual_total_growth[i];
      mean_relative_growth += individual_relative_growth[i];
      mean_gross_efficiency += individual_gross_efficiency[i];
      mean_metabolic_scope += individual_metabolic_scope[i];
      mean_final_energy_density += individual_final_energy_density[i];
      
      mean_respiration_energy += individual_respiration_energy[i];
      mean_egestion_energy += individual_egestion_energy[i];
      mean_excretion_energy += individual_excretion_energy[i];
      mean_sda_energy += individual_sda_energy[i];
      mean_net_energy += individual_net_energy[i];
      mean_spawn_energy += individual_spawn_energy[i];
    }
    
    // Convert to means
    mean_final_weight /= Type(n_individuals);
    mean_total_consumption /= Type(n_individuals);
    mean_total_growth /= Type(n_individuals);
    mean_relative_growth /= Type(n_individuals);
    mean_gross_efficiency /= Type(n_individuals);
    mean_metabolic_scope /= Type(n_individuals);
    mean_final_energy_density /= Type(n_individuals);
    
    mean_respiration_energy /= Type(n_individuals);
    mean_egestion_energy /= Type(n_individuals);
    mean_excretion_energy /= Type(n_individuals);
    mean_sda_energy /= Type(n_individuals);
    mean_net_energy /= Type(n_individuals);
    mean_spawn_energy /= Type(n_individuals);
    
    // ====================================================================
    // POPULATION-LEVEL PRIORS
    // ====================================================================
    
    total_negative_log_likelihood -= dnorm(log_mu_p, Type(0.0), Type(1.0), true);      
    total_negative_log_likelihood -= dnorm(log_sigma_p, Type(-1.6), Type(0.5), true);  
    total_negative_log_likelihood -= dnorm(log_sigma_obs, Type(-2.3), Type(1.0), true); 
    
    for(int k = 0; k < n_covariates; k++) {
      total_negative_log_likelihood -= dnorm(betas[k], Type(0.0), Type(1.0), true);
    }

    // ====================================================================
    // REPORTS WITH UNCERTAINTY PROPAGATION - HIERARCHICAL MODEL
    // ====================================================================
    
    // Population-level parameters (transformed)
    Type mu_p = exp(log_mu_p);  // For reporting only
    ADREPORT(mu_p);
    ADREPORT(sigma_p);
    ADREPORT(sigma_obs);
    ADREPORT(betas);
    
    // Population-level parameters (log scale)
    ADREPORT(log_mu_p);
    ADREPORT(log_sigma_p);
    ADREPORT(log_sigma_obs);
    
    // Individual-level variables (vectors with uncertainty)
    ADREPORT(individual_p_values);
    ADREPORT(final_weights);
    ADREPORT(individual_total_consumption);
    ADREPORT(individual_total_growth);
    ADREPORT(individual_relative_growth);
    ADREPORT(individual_gross_efficiency);
    ADREPORT(individual_metabolic_scope);
    ADREPORT(individual_final_energy_density);
    
    // Individual energy budget components
    ADREPORT(individual_respiration_energy);
    ADREPORT(individual_egestion_energy);
    ADREPORT(individual_excretion_energy);
    ADREPORT(individual_sda_energy);
    ADREPORT(individual_net_energy);
    ADREPORT(individual_spawn_energy);
    
    // Population-level means (with uncertainty propagated from individuals)
    ADREPORT(mean_final_weight);
    ADREPORT(mean_total_consumption);
    ADREPORT(mean_total_growth);
    ADREPORT(mean_relative_growth);
    ADREPORT(mean_gross_efficiency);
    ADREPORT(mean_metabolic_scope);
    ADREPORT(mean_final_energy_density);
    
    // Population-level mean energy budgets
    ADREPORT(mean_respiration_energy);
    ADREPORT(mean_egestion_energy);
    ADREPORT(mean_excretion_energy);
    ADREPORT(mean_sda_energy);
    ADREPORT(mean_net_energy);
    ADREPORT(mean_spawn_energy);
    
    // Standard REPORT for direct access (without uncertainty)
    REPORT(mu_p);
    REPORT(sigma_p);
    REPORT(sigma_obs);
    REPORT(individual_p_values);
    REPORT(final_weights);
    REPORT(mean_final_weight);
    REPORT(betas);
    REPORT(individual_total_consumption);
    REPORT(mean_total_consumption);
    
    return total_negative_log_likelihood;
    
  } else {
    // Invalid model type
    Rf_error("Invalid model_type. Must be 'basic' or 'hierarchical'");
    return Type(1e10);
  }
}
