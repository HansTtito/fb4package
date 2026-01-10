#ifndef FB4_PREDATOR_ENERGY_HPP
#define FB4_PREDATOR_ENERGY_HPP

#include "fb4_utils.hpp"

namespace fb4 {

// ============================================================================
// PREDATOR ENERGY DENSITY FUNCTIONS FOR FB4 TMB
// ============================================================================

// Energy density from interpolated data - Equation 1 (Low-level)
template<class Type>
Type predator_energy_eq1(Type weight, int day, const vector<Type>& energy_data) {
  int day_index = day - 1;  // Convert to 0-based indexing
  if (day_index < 0) day_index = 0;
  if (day_index >= energy_data.size()) day_index = energy_data.size() - 1;
  
  return energy_data[day_index];
}

// Linear piecewise energy density - Equation 2 (Low-level)
template<class Type>
Type predator_energy_eq2(Type weight, Type Alpha1, Type Beta1, Type Alpha2, Type Beta2, Type Cutoff) {
  if (weight < Cutoff) {
    return Alpha1 + Beta1 * weight;
  } else {
    return Alpha2 + Beta2 * weight;
  }
}

// Power function energy density - Equation 3 (Low-level)
template<class Type>
Type predator_energy_eq3(Type weight, Type Alpha1, Type Beta1) {
  Type energy_density = Alpha1 * pow(weight, Beta1);
  return energy_density;
}

// Calculate predator energy density (Mid-level - Main function)
template<class Type>
Type calculate_predator_energy_density(Type weight, int day, int PREDEDEQ, 
                                       Type Alpha1 = Type(0.0), Type Beta1 = Type(0.0), 
                                       Type Alpha2 = Type(0.0), Type Beta2 = Type(0.0), 
                                       Type Cutoff = Type(0.0), 
                                       const vector<Type>& energy_data = vector<Type>(0)) {
  
  if (PREDEDEQ == 1) {
    return predator_energy_eq1(weight, day, energy_data);
  } else if (PREDEDEQ == 2) {
    return predator_energy_eq2(weight, Alpha1, Beta1, Alpha2, Beta2, Cutoff);
  } else if (PREDEDEQ == 3) {
    return predator_energy_eq3(weight, Alpha1, Beta1);
  }
  
  return Type(4000.0);  // Default energy density fallback
}

// ============================================================================
// WEIGHT CALCULATION FUNCTION - SAFE IMPLEMENTATION
// ============================================================================

/**
 * Calculate final weight after energy balance - Simple and safe version
 * Implements all three PREDEDEQ equations following R logic exactly
 * 
 * @param current_weight Current fish weight (g)
 * @param net_energy_for_growth Net energy available for growth (J total)
 * @param current_ed Current energy density (J/g)
 * @param day Current simulation day
 * @param PREDEDEQ Predator energy density equation number (1, 2, or 3)
 * @param Alpha1, Beta1, Alpha2, Beta2, Cutoff Energy density parameters
 * @param ED_data Energy density data vector (for PREDEDEQ=1)
 * @return Final weight (g)
 */
template<class Type>
Type calculate_final_weight_simple(Type current_weight, Type net_energy_for_growth, 
                                   Type current_ed, int day, int PREDEDEQ,
                                   Type Alpha1, Type Beta1, Type Alpha2, Type Beta2, 
                                   Type Cutoff, const vector<Type>& ED_data) {
  
  // Current body energy
  Type current_body_energy = current_weight * current_ed;
  Type target_energy = current_body_energy + net_energy_for_growth;
  
  // Safety check
  if (current_weight <= Type(0.01)) {
    return Type(0.01);
  }
  
  Type new_weight = current_weight; // Default fallback
  
  // ========================================================================
  // PREDEDEQ = 1: Interpolated data
  // ========================================================================
  if (PREDEDEQ == 1) {
    // R logic: finalwt <- (egain + (Pred_E_i*W))/Pred_E_iplusone
    Type pred_e_next_day = calculate_predator_energy_density(
      current_weight, day + 1, PREDEDEQ, Alpha1, Beta1, Alpha2, Beta2, Cutoff, ED_data
    );
    
    if (pred_e_next_day > Type(0.0)) {
      new_weight = target_energy / pred_e_next_day;
    } else {
      new_weight = Type(0.01);
    }
  }
  
  // ========================================================================
  // PREDEDEQ = 2: Linear piecewise
  // ========================================================================
  else if (PREDEDEQ == 2) {
    // R logic: Quadratic formula depending on segment
    
    if (current_weight < Cutoff) {
      // First segment: Alpha1 + Beta1*W
      if (CppAD::CondExpGt(Beta1, Type(0.0), Beta1, -Beta1) > Type(1e-10)) {
        // Quadratic equation: Beta1*W^2 + Alpha1*W - target_energy = 0
        Type discriminant = Alpha1 * Alpha1 + Type(4.0) * Beta1 * target_energy;
        if (discriminant >= Type(0.0)) {
          new_weight = (-Alpha1 + safe_sqrt(discriminant)) / (Type(2.0) * Beta1);
        } else {
          new_weight = Type(0.01);
        }
      } else {
        // Linear case: Alpha1*W = target_energy
        if (Alpha1 > Type(0.0)) {
          new_weight = target_energy / Alpha1;
        } else {
          new_weight = Type(0.01);
        }
      }
    } else {
      // Second segment: Alpha2 + Beta2*W
      if (CppAD::CondExpGt(Beta2, Type(0.0), Beta2, -Beta2) > Type(1e-10)) {
        // Quadratic equation: Beta2*W^2 + Alpha2*W - target_energy = 0
        Type discriminant = Alpha2 * Alpha2 + Type(4.0) * Beta2 * target_energy;
        if (discriminant >= Type(0.0)) {
          new_weight = (-Alpha2 + safe_sqrt(discriminant)) / (Type(2.0) * Beta2);
        } else {
          new_weight = Type(0.01);
        }
      } else {
        // Linear case: Alpha2*W = target_energy
        if (Alpha2 > Type(0.0)) {
          new_weight = target_energy / Alpha2;
        } else {
          new_weight = Type(0.01);
        }
      }
    }
  }
  
  // ========================================================================
  // PREDEDEQ = 3: Power function
  // ========================================================================
  else if (PREDEDEQ == 3) {
    // R logic: finalwt <- ((egain + (Pred_E_i*W))/alpha1)^(1/(beta1+1))
    
    if (target_energy > Type(0.0) && Alpha1 > Type(0.0)) {
      Type exponent = Type(1.0) / (Beta1 + Type(1.0));
      
      // Safety check for exponent
      if (CppAD::CondExpGt(exponent, Type(0.0), exponent, -exponent) < Type(10.0)) { // Reasonable exponent range
        new_weight = pow(target_energy / Alpha1, exponent);
      } else {
        new_weight = Type(0.01);
      }
    } else {
      new_weight = Type(0.01);
    }
  }
  
  // ========================================================================
  // SAFETY CHECKS AND RETURN
  // ========================================================================
  
  // Final safety checks
  if (!R_finite(asDouble(new_weight)) || new_weight <= Type(0.0)) {
    new_weight = Type(0.01);
  }
  
  // Clamp to reasonable range
  new_weight = clamp(new_weight, Type(0.01), Type(1e6), "final_weight");
  
  return new_weight;
}

} // namespace fb4

#endif // FB4_PREDATOR_ENERGY_HPP