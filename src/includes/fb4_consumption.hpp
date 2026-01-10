#ifndef FB4_CONSUMPTION_HPP
#define FB4_CONSUMPTION_HPP


#include "fb4_utils.hpp"

namespace fb4 {

// ============================================================================
// CONSUMPTION FUNCTIONS FOR FB4 TMB
// ============================================================================

// Temperature function for consumption - Equation 1 (Low-level)
template<class Type>
Type consumption_temp_eq1(Type temperature, Type CQ) {
  Type ft = safe_exp(CQ * temperature);
  return ft;
}

// Temperature function for consumption - Equation 2 (Low-level)
template<class Type>
Type consumption_temp_eq2(Type temperature, Type CTM, Type CTO, Type CX) {
  if (temperature >= CTM) {
    return Type(0.0);
  }
  
  Type V = (CTM - temperature) / (CTM - CTO);
  Type ft = pow(V, CX) * safe_exp(CX * (Type(1.0) - V));
  
  return CppAD::CondExpGe(ft, Type(0.0), ft, Type(0.0));
}

// Temperature function for consumption - Equation 3 (Low-level)
template<class Type>
Type consumption_temp_eq3(Type temperature, Type CQ, Type CG1, Type CK1, Type CG2, Type CTL, Type CK4) {
  // Calculate first component
  Type L1 = safe_exp(CG1 * (temperature - CQ));
  Type KA = (CK1 * L1) / (Type(1.0) + CK1 * (L1 - Type(1.0)));
  
  // Calculate second component
  Type L2 = safe_exp(CG2 * (CTL - temperature));
  Type KB = (CK4 * L2) / (Type(1.0) + CK4 * (L2 - Type(1.0)));
  
  Type ft = KA * KB;
  return ft;
}

// Temperature function for consumption - Equation 4 (Low-level)
template<class Type>
Type consumption_temp_eq4(Type temperature, Type CQ, Type CK1, Type CK4) {
  Type exponent = CQ * temperature + CK1 * pow(temperature, Type(2.0)) + CK4 * pow(temperature, Type(3.0));
  Type ft = safe_exp(exponent);
  return ft;
}

// Calculate temperature factor for consumption (Mid-level)
template<class Type>
Type calculate_temperature_factor_consumption(Type temperature, int CEQ, Type CQ, Type CTM = Type(0.0), 
                                              Type CTO = Type(0.0), Type CX = Type(0.0), Type CG1 = Type(0.0), 
                                              Type CK1 = Type(0.0), Type CG2 = Type(0.0), Type CTL = Type(0.0), 
                                              Type CK4 = Type(0.0)) {
  
  if (CEQ == 1) {
    return consumption_temp_eq1(temperature, CQ);
  } else if (CEQ == 2) {
    return consumption_temp_eq2(temperature, CTM, CTO, CX);
  } else if (CEQ == 3) {
    return consumption_temp_eq3(temperature, CQ, CG1, CK1, CG2, CTL, CK4);
  } else if (CEQ == 4) {
    return consumption_temp_eq4(temperature, CQ, CK1, CK4);
  }
  
  return Type(1.0);  // Default fallback
}

// Calculate daily consumption rate (Mid-level - Main function)
template<class Type>
Type calculate_consumption_rate(Type temperature, Type weight, Type p_value, 
                                int CEQ, Type CA, Type CB, Type CQ, Type CTM = Type(0.0), 
                                Type CTO = Type(0.0), Type CTL = Type(0.0), Type CK1 = Type(0.0), 
                                Type CK4 = Type(0.0), Type CG1 = Type(0.0), Type CG2 = Type(0.0), 
                                Type CX = Type(0.0)) {
  
  // Calculate maximum consumption
  Type Cmax = CA * pow(weight, CB);
  
  // Calculate temperature factor
  Type ft = calculate_temperature_factor_consumption(temperature, CEQ, CQ, CTM, CTO, CX, 
                                                     CG1, CK1, CG2, CTL, CK4);
  
  // Calculate consumption rate
  Type consumption_rate = Cmax * p_value * ft;
  
  return CppAD::CondExpGe(consumption_rate, Type(0.0), consumption_rate, Type(0.0));
}

} // namespace fb4

#endif // FB4_CONSUMPTION_HPP