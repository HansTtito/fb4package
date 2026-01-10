#ifndef FB4_MORTALITY_REPRODUCTION_HPP
#define FB4_MORTALITY_REPRODUCTION_HPP


#include "fb4_utils.hpp"

// ============================================================================
// MORTALITY AND REPRODUCTION FUNCTIONS FOR FB4 TMB
// ============================================================================

// Calculate combined daily survival (Low-level)
template<class Type>
Type calculate_combined_survival(vector<Type> mortality_rates, int method = 0) {
  // method: 0 = independent, 1 = additive
  
  Type survival_rate;
  
  if (method == 0) {
    // Independent survival: product of (1 - mortality)
    survival_rate = Type(1.0);
    for (int i = 0; i < mortality_rates.size(); i++) {
      survival_rate *= (Type(1.0) - mortality_rates[i]);
    }
  } else {
    // Additive mortality
    Type combined_mortality = Type(0.0);
    for (int i = 0; i < mortality_rates.size(); i++) {
      combined_mortality += mortality_rates[i];
    }
    survival_rate = Type(1.0) - combined_mortality;
  }
  
  return CppAD::CondExpGe(survival_rate, Type(0.0), survival_rate, Type(0.0));
}

// Calculate weight-dependent mortality (Low-level)
template<class Type>
Type calculate_weight_dependent_mortality(Type current_weight, Type base_mortality,
                                          Type weight_threshold, Type starvation_factor,
                                          Type initial_weight = Type(0.0)) {
  
  Type mortality_rate = base_mortality;
  
  // Mortality due to low absolute weight
  if (weight_threshold > Type(0.0) && current_weight < weight_threshold) {
    Type weight_ratio = current_weight / weight_threshold;
    Type starvation_effect = starvation_factor * (Type(1.0) - weight_ratio);
    mortality_rate = mortality_rate + starvation_effect * base_mortality;
  }
  
  // Mortality due to relative weight loss
  if (initial_weight > Type(0.0)) {
    Type weight_loss_fraction = Type(1.0) - (current_weight / initial_weight);
    if (weight_loss_fraction > Type(0.5)) {  // More than 50% weight loss
      Type severe_loss_effect = Type(2.0) * (weight_loss_fraction - Type(0.5));
      mortality_rate = mortality_rate + severe_loss_effect * base_mortality;
    }
  }
  
  return mortality_rate;
}

// Calculate temperature-dependent mortality (Low-level)
template<class Type>
Type calculate_temperature_dependent_mortality(Type temperature, Type base_mortality,
                                               Type optimal_temp, Type thermal_tolerance,
                                               Type stress_factor) {
  
  Type temp_deviation = abs(temperature - optimal_temp);
  Type total_mortality;
  
  if (temp_deviation > thermal_tolerance) {
    Type thermal_stress = (temp_deviation - thermal_tolerance) / thermal_tolerance;
    Type stress_mortality = stress_factor * thermal_stress * base_mortality;
    total_mortality = base_mortality + stress_mortality;
  } else {
    total_mortality = base_mortality;
  }
  
  return total_mortality;
}

// Calculate reproductive weight loss (Low-level)
template<class Type>
Type calculate_reproductive_loss(Type spawn_fraction, Type current_weight, Type energy_density) {
  Type weight_loss = spawn_fraction * current_weight;
  Type energy_loss = weight_loss * energy_density;
  
  return energy_loss;
}

// Generate seasonal reproduction pattern (Low-level)
template<class Type>
Type generate_reproduction_pattern_value(int day, int peak_day, Type duration,
                                         Type max_spawn_fraction, int pattern_type = 0) {
  // pattern_type: 0 = gaussian, 1 = uniform, 2 = pulse
  
  Type spawn_fraction = Type(0.0);
  
  if (pattern_type == 0) {
    // Gaussian pattern
    // Distance to peak (considering year circularity)
    Type arg1 = abs(Type(day) - Type(peak_day));
    Type arg2 = Type(365.0) - abs(Type(day) - Type(peak_day));
    Type dist_to_peak = CppAD::CondExpLe(arg1, arg2, arg1, arg2);
    
    if (dist_to_peak <= duration / Type(2.0)) {
      Type sigma = duration / Type(4.0);  // Standard deviation
      Type exponent = -Type(0.5) * pow(dist_to_peak / sigma, Type(2.0));
      spawn_fraction = max_spawn_fraction * safe_exp(exponent);
    }
    
  } else if (pattern_type == 1) {
    // Uniform pattern
    Type start_day = Type(peak_day) - duration / Type(2.0);
    Type end_day = Type(peak_day) + duration / Type(2.0);
    
    bool in_season = false;
    if (start_day >= Type(1.0) && end_day <= Type(365.0)) {
      in_season = (Type(day) >= start_day && Type(day) <= end_day);
    } else {
      // Handle cases where period crosses year
      if (start_day < Type(1.0)) {
        in_season = (Type(day) >= (start_day + Type(365.0)) || Type(day) <= end_day);
      } else if (end_day > Type(365.0)) {
        in_season = (Type(day) >= start_day || Type(day) <= (end_day - Type(365.0)));
      }
    }
    
    if (in_season) {
      spawn_fraction = max_spawn_fraction / duration;
    }
    
  } else if (pattern_type == 2) {
    // Pulse spawning
    if (day == peak_day) {
      spawn_fraction = max_spawn_fraction;
    }
  }
  
  return spawn_fraction;
}

// Calculate spawning energy loss (Low-level)
template<class Type>
Type calculate_spawn_energy(Type spawn_fraction, Type current_weight, Type energy_density) {
  Type spawn_energy = spawn_fraction * current_weight * energy_density;
  return spawn_energy;
}

// Calculate daily mortality and reproduction (Mid-level - Main function)
template<class Type>
Type calculate_mortality_reproduction(Type current_weight, Type temperature, int day_of_year,
                                      Type base_mortality, Type natural_mortality,
                                      Type fishing_mortality, Type predation_mortality,
                                      Type weight_threshold = Type(0.0), Type starvation_factor = Type(1.0),
                                      Type optimal_temp = Type(15.0), Type thermal_tolerance = Type(5.0),
                                      Type stress_factor = Type(2.0), Type initial_weight = Type(0.0),
                                      vector<Type> spawn_pattern = vector<Type>(0)) {
  
  // Calculate weight-dependent mortality
  Type weight_adjusted_mortality = calculate_weight_dependent_mortality(
    current_weight, natural_mortality, weight_threshold, starvation_factor, initial_weight
  );
  
  // Calculate temperature-dependent mortality
  Type temp_adjusted_mortality = calculate_temperature_dependent_mortality(
    temperature, weight_adjusted_mortality, optimal_temp, thermal_tolerance, stress_factor
  );
  
  // Combine all mortality sources
  vector<Type> all_mortality_rates(3);
  all_mortality_rates[0] = temp_adjusted_mortality;  // natural
  all_mortality_rates[1] = fishing_mortality;
  all_mortality_rates[2] = predation_mortality;
  
  // Calculate combined survival
  Type survival_rate = calculate_combined_survival(all_mortality_rates, 0);  // independent method
  
  return survival_rate;
}

// Calculate reproduction energy loss for a given day
template<class Type>
Type calculate_daily_reproduction_energy(int day_of_year, Type current_weight, Type energy_density,
                                         vector<Type> spawn_pattern) {
  
  if (spawn_pattern.size() == 0 || day_of_year <= 0 || day_of_year > spawn_pattern.size()) {
    return Type(0.0);
  }
  
  Type spawn_fraction = spawn_pattern[day_of_year - 1];  // Convert to 0-based indexing
  
  if (spawn_fraction <= Type(0.0)) {
    return Type(0.0);
  }
  
  Type spawn_energy = calculate_spawn_energy(spawn_fraction, current_weight, energy_density);
  
  return spawn_energy;
}

#endif // FB4_MORTALITY_REPRODUCTION_HPP