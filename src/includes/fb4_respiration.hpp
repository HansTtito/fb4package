#ifndef FB4_RESPIRATION_HPP
#define FB4_RESPIRATION_HPP


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