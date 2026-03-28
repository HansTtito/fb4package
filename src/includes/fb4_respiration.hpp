#ifndef FB4_RESPIRATION_HPP
#define FB4_RESPIRATION_HPP

/*
 * fb4_respiration.hpp — Respiration sub-model (TMB/C++ backend)
 *
 * Implements the two temperature-dependence equations for respiration (REQ 1-2),
 * the velocity-based activity correction, and conversion of O2 consumption to
 * energy units used in Fish Bioenergetics 4.0.
 *
 * R = RA * W^RB * F(T) * ACT
 *   RA, RB = allometric intercept and slope
 *   F(T)   = temperature multiplier (REQ 1 or REQ 2)
 *   ACT    = activity multiplier
 *
 * REQ 1 — simple Q10 exponential with velocity-based activity:
 *   F(T) = exp(RQ * T)
 *   Velocity model: VEL = ACT * W^RK4 * exp(BACT * T)  for T <= RTL
 *                   VEL = RK1 * W^RK4 * exp(RK5 * T)   for T >  RTL
 *   ACT  = max(1, exp(RTO * VEL))
 *
 * REQ 2 — Kitchell et al. (1977):
 *   V    = (RTM - T) / (RTM - RTO)
 *   F(T) = V^RX * exp(RX * (1 - V))
 *
 * Oxygen consumption is converted to energy via the oxycalorific coefficient (default 13 560 J g-1 O2; Elliott and Davison 1975).
 *
 * References:
 *   Kitchell, J.F., Stewart, D.J. and Weininger, D. (1977). Applications of a bioenergetics model to yellow perch and walleye. J. Fish. Res. Board Can., 34(10), 1922-1935.
 *
 *   Elliott, J.M. and Davison, W. (1975). Energy equivalents of oxygen consumption in animal energetics. Oecologia, 19(3), 195-201.
 *
 *   Deslauriers, D. et al. (2017). Fish Bioenergetics 4.0: An R-based modeling application. Fisheries, 42(11), 586-596. doi:10.1080/03632415.2017.1377558
 *
 *   Hanson, P.C. et al. (1997). Fish Bioenergetics 3.0. UW Sea Grant, WISCU-T-97-001.
 */

#include "fb4_utils.hpp"

namespace fb4 {

// ============================================================================
// RESPIRATION FUNCTIONS FOR FB4 TMB
// ============================================================================

// Temperature function for respiration - Equation 1 (Low-level)
template<class Type>
Type respiration_temp_eq1(Type temperature, Type RQ) {
  Type ft = safe_exp(RQ * temperature);
  return ft;
}

// Temperature function for respiration - Equation 2 (Low-level)
template<class Type>
Type respiration_temp_eq2(Type temperature, Type RTM, Type RTO, Type RX) {
  if (temperature >= RTM) {
    return Type(0.000001);
  }
  
  Type V = (RTM - temperature) / (RTM - RTO);
  Type ft = pow(V, RX) * safe_exp(RX * (Type(1.0) - V));
  
  return CppAD::CondExpGe(ft, Type(0.000001), ft, Type(0.000001));
}

// Calculate temperature factor for respiration (Mid-level)
template<class Type>
Type calculate_temperature_factor_respiration(Type temperature, int REQ, Type RQ, 
                                              Type RTM = Type(0.0), Type RTO = Type(0.0), 
                                              Type RX = Type(0.0)) {
  
  if (REQ == 1) {
    return respiration_temp_eq1(temperature, RQ);
  } else if (REQ == 2) {
    return respiration_temp_eq2(temperature, RTM, RTO, RX);
  }
  
  return Type(1.0);  // Default fallback
}

// Calculate activity factor for respiration (Mid-level)
template<class Type>
Type calculate_activity_factor_respiration(Type weight, Type temperature, int REQ, Type ACT, 
                                           Type RTL = Type(0.0), Type RK4 = Type(0.0), 
                                           Type RK1 = Type(0.0), Type RK5 = Type(0.0), 
                                           Type RTO = Type(0.0), Type BACT = Type(0.0)) {
  
  if (REQ != 1) {
    return ACT;
  }
  
  // Complex activity calculation (REQ == 1)
  Type VEL;
  if (temperature <= RTL) {
    VEL = ACT * pow(weight, RK4) * safe_exp(BACT * temperature);
  } else {
    VEL = RK1 * pow(weight, RK4) * safe_exp(RK5 * temperature);
  }
  
  VEL = clamp(VEL, Type(0.0), Type(100.0), "VEL");
  Type ACTIVITY = safe_exp(RTO * VEL);
  
  return CppAD::CondExpGe(ACTIVITY, Type(1.0), ACTIVITY, Type(1.0));
}

// Calculate daily respiration rate (Mid-level - Main function)
template<class Type>
Type calculate_respiration_rate(Type temperature, Type weight, 
                                int REQ, Type RA, Type RB, Type RQ, Type RTO, Type RTM, 
                                Type RTL, Type RK1, Type RK4, Type RK5, Type RX, 
                                Type ACT, Type BACT) {
  
  // Calculate maximum respiration
  Type Rmax = RA * pow(weight, RB);
  
  // Calculate temperature factor
  Type ft = calculate_temperature_factor_respiration(temperature, REQ, RQ, RTM, RTO, RX);
  
  // Calculate activity factor
  Type activity_factor = calculate_activity_factor_respiration(weight, temperature, REQ, ACT, 
                                                               RTL, RK4, RK1, RK5, RTO, BACT);
  
  // Total respiration
  Type total_respiration = Rmax * ft * activity_factor;
  
  // Ensure valid result
  if (!R_finite(asDouble(total_respiration)) || total_respiration <= Type(0.0)) {
    return Type(0.000001);
  }
  
  return total_respiration;
}

// Calculate SDA (Specific Dynamic Action)
template<class Type>
Type calculate_sda_energy(Type consumption_energy, Type egestion_energy, Type SDA_coeff) {
  // Ensure egestion is not greater than consumption
  egestion_energy = CppAD::CondExpLe(egestion_energy, consumption_energy, egestion_energy, consumption_energy);
  
  Type sda = SDA_coeff * (consumption_energy - egestion_energy);
  
  return sda;
}

// Convert respiration from O2 to energy units
template<class Type>
Type convert_respiration_to_energy(Type respiration_o2, Type oxycal = Type(13560.0)) {
  return respiration_o2 * oxycal;
}

} // namespace fb4

#endif // FB4_RESPIRATION_HPP