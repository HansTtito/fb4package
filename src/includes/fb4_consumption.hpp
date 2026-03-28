#ifndef FB4_CONSUMPTION_HPP
#define FB4_CONSUMPTION_HPP

/*
 * fb4_consumption.hpp — Consumption sub-model (TMB/C++ backend)
 *
 * Implements the four temperature-dependence equations for consumption (CEQ 1-4)
 * and the allometric maximum-consumption function used in Fish Bioenergetics 4.0.
 *
 * Energy balance:  C = Cmax * p * F(T)
 *   Cmax = CA * W^CB   (allometric maximum consumption)
 *   F(T) = temperature multiplier (one of CEQ 1-4 below)
 *   p    = P-value, proportion of maximum consumption (0-1)
 *
 * CEQ 1 — simple Q10 exponential:
 *   F(T) = exp(CQ * T)
 *
 * CEQ 2 — Kitchell et al. (1977):
 *   V    = (CTM - T) / (CTM - CTO)
 *   F(T) = V^CX * exp(CX * (1 - V))
 *
 * CEQ 3 — Thornton and Lessem (1978), two-part sigmoid:
 *   uses CQ, CTO, CTL, CTM, CK1, CK4 via calculated CG1, CG2
 *
 * CEQ 4 — polynomial:
 *   F(T) = exp(CQ*T + CK1*T^2 + CK4*T^3)
 *
 * References:
 *   Kitchell, J.F., Stewart, D.J. and Weininger, D. (1977). Applications of a bioenergetics model to yellow perch and walleye. J. Fish. Res. Board Can., 34(10), 1922-1935.
 *
 *   Thornton, K.W. and Lessem, A.S. (1978). A temperature algorithm for modifying biological rates. Trans. Am. Fish. Soc., 107(2), 284-287.
 *
 *   Hartman, K.J. and Hayward, R.S. (2007). Bioenergetics. In Guy & Brown (eds.), Analysis and Interpretation of Freshwater Fisheries Data. AFS, Bethesda, MD.
 *
 *   Deslauriers, D. et al. (2017). Fish Bioenergetics 4.0: An R-based modeling application. Fisheries, 42(11), 586-596. doi:10.1080/03632415.2017.1377558
 *
 *   Bevelhimer, M.S., Stein, R.A. and Carline, R.F. (1985). Assessing significance of physiological differences among three esocids with a bioenergetics model. Can. J. Fish. Aquat. Sci., 42(1), 57-69.
 *
 *   Hanson, P.C. et al. (1997). Fish Bioenergetics 3.0. UW Sea Grant, WISCU-T-97-001.
 */

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