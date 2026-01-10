#ifndef FB4_EGESTION_EXCRETION_HPP
#define FB4_EGESTION_EXCRETION_HPP


#include "fb4_utils.hpp"

namespace fb4 {

// ============================================================================
// EGESTION AND EXCRETION FUNCTIONS FOR FB4 TMB
// ============================================================================

// Egestion model 1 - Basic (Low-level)
template<class Type>
Type egestion_model_1(Type consumption, Type FA) {
  Type egestion = FA * consumption;
  return CppAD::CondExpLe(egestion, consumption, egestion, consumption);
}

// Egestion model 2 - Elliott (1976) (Low-level)
template<class Type>
Type egestion_model_2(Type consumption, Type temperature, Type p_value, Type FA, Type FB, Type FG) {
  Type egestion = FA * pow(temperature, FB) * safe_exp(FG * p_value) * consumption;
  return CppAD::CondExpLe(egestion, consumption, egestion, consumption);
}

// Egestion model 3 - Stewart et al. (1983) (Low-level)
template<class Type>
Type egestion_model_3(Type consumption, Type temperature, Type p_value, Type FA, Type FB, Type FG, Type indigestible_fraction) {
  Type PE = FA * pow(temperature, FB) * safe_exp(FG * p_value);
  Type PF = ((PE - Type(0.1)) / Type(0.9)) * (Type(1.0) - indigestible_fraction) + indigestible_fraction;
  Type temp_min = CppAD::CondExpLe(PF, Type(1.0), PF, Type(1.0));  
  PF = CppAD::CondExpGe(temp_min, Type(0.0), temp_min, Type(0.0)); 
  Type egestion = PF * consumption;
  return egestion;
}

// Egestion model 4 - Elliott (1976) without p_value (Low-level)
template<class Type>
Type egestion_model_4(Type consumption, Type temperature, Type FA, Type FB) {
  Type egestion = FA * pow(temperature, FB) * consumption;
  return CppAD::CondExpLe(egestion, consumption, egestion, consumption);
}

// Calculate daily egestion rate (Mid-level - Main function)
template<class Type>
Type calculate_egestion_rate(Type consumption, Type temperature, Type p_value, 
                             Type indigestible_fraction, int EGEQ, Type FA, 
                             Type FB = Type(0.0), Type FG = Type(0.0)) {
  
  if (consumption == Type(0.0)) return Type(0.0);
  
  if (EGEQ == 1) {
    return egestion_model_1(consumption, FA);
  } else if (EGEQ == 2) {
    return egestion_model_2(consumption, temperature, p_value, FA, FB, FG);
  } else if (EGEQ == 3) {
    return egestion_model_3(consumption, temperature, p_value, FA, FB, FG, indigestible_fraction);
  } else if (EGEQ == 4) {
    return egestion_model_4(consumption, temperature, FA, FB);
  }
  
  return Type(0.0);  // Default fallback
}

// Excretion model 1 - Basic (Low-level)
template<class Type>
Type excretion_model_1(Type consumption, Type egestion, Type UA) {
  Type assimilated = consumption - egestion;
  Type excretion = UA * assimilated;
  return excretion;
}

// Excretion model 2 - With temperature and feeding dependence (Low-level)
template<class Type>
Type excretion_model_2(Type consumption, Type egestion, Type temperature, Type p_value, Type UA, Type UB, Type UG) {
  Type assimilated = consumption - egestion;
  Type excretion = UA * pow(temperature, UB) * safe_exp(UG * p_value) * assimilated;
  return excretion;
}

// Excretion model 3 - Variant of model 2 (Low-level)
template<class Type>
Type excretion_model_3(Type consumption, Type egestion, Type temperature, Type p_value, Type UA, Type UB, Type UG) {
  return excretion_model_2(consumption, egestion, temperature, p_value, UA, UB, UG);
}

// Excretion model 4 - Without feeding dependence (Low-level)
template<class Type>
Type excretion_model_4(Type consumption, Type egestion, Type temperature, Type UA, Type UB) {
  Type assimilated = consumption - egestion;
  Type excretion = UA * pow(temperature, UB) * assimilated;
  return excretion;
}

// Calculate daily excretion rate (Mid-level - Main function)
template<class Type>
Type calculate_excretion_rate(Type consumption, Type egestion, Type temperature, 
                              Type p_value, Type weight, int EXEQ, Type UA, 
                              Type UB = Type(0.0), Type UG = Type(0.0)) {
  
  if (consumption == Type(0.0)) return Type(0.0);
  
  if (EXEQ == 1) {
    return excretion_model_1(consumption, egestion, UA);
  } else if (EXEQ == 2) {
    return excretion_model_2(consumption, egestion, temperature, p_value, UA, UB, UG);
  } else if (EXEQ == 3) {
    return excretion_model_3(consumption, egestion, temperature, p_value, UA, UB, UG);
  } else if (EXEQ == 4) {
    return excretion_model_4(consumption, egestion, temperature, UA, UB);
  }
  
  return Type(0.0);  // Default fallback
}

} // namespace fb4

#endif // FB4_EGESTION_EXCRETION_HPP