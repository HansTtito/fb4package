#ifndef FB4_EGESTION_EXCRETION_HPP
#define FB4_EGESTION_EXCRETION_HPP

/*
 * fb4_egestion_excretion.hpp — Egestion and excretion sub-models (TMB/C++ backend)
 *
 * Implements four egestion models (EGEQ 1-4) and four excretion models
 * (EXEQ 1-4) used in Fish Bioenergetics 4.0.
 *
 * EGESTION MODELS:
 *   EGEQ 1 — constant fraction:
 *     F = FA * C
 *
 *   EGEQ 2 — Elliott (1976), temperature- and feeding-dependent:
 *     F = FA * T^FB * exp(FG * p) * C
 *
 *   EGEQ 3 — Stewart et al. (1983), includes indigestible prey fraction:
 *     PE = FA * T^FB * exp(FG * p)
 *     PF = ((PE - 0.1) / 0.9) * (1 - indigest) + indigest   [clamped 0-1]
 *     F  = PF * C
 *
 *   EGEQ 4 — Luo and Brandt (1993), temperature-dependent only (no ration effect):
 *     F = FA * T^FB * C
 *
 * EXCRETION MODELS:
 *   EXEQ 1 — constant fraction of assimilated energy:
 *     U = UA * (C - F)
 *
 *   EXEQ 2 — Elliott (1976), temperature- and feeding-dependent:
 *     U = UA * T^UB * exp(UG * p) * (C - F)
 *
 *   EXEQ 3 — same form as EXEQ 2 (alternate parameterisation)
 *
 *   EXEQ 4 — Luo and Brandt (1993), temperature-dependent only (no ration effect):
 *     U = UA * T^UB * (C - F)
 *
 * References:
 *   Elliott, J.M. (1976). Energy losses in the waste products of brown trout
 *   (Salmo trutta L.). Journal of Animal Ecology, 45(2), 561-580.
 *
 *   Stewart, D.J., Weininger, D., Rottiers, D.V. and Edsall, T.A. (1983). An
 *   energetics model for lake trout, Salvelinus namaycush: application to the
 *   Lake Michigan population. Can. J. Fish. Aquat. Sci., 40(6), 681-698.
 *
 *   Deslauriers, D. et al. (2017). Fish Bioenergetics 4.0: An R-based modeling
 *   application. Fisheries, 42(11), 586-596. doi:10.1080/03632415.2017.1377558
 *
 *   Luo, J. and Brandt, S.B. (1993). Bay anchovy Anchoa mitchilli production and
 *   consumption in mid-Chesapeake Bay based on a bioenergetics model and acoustic
 *   measures of fish abundance. Mar. Ecol. Prog. Ser., 98(3), 223-236.
 *
 *   Hanson, P.C. et al. (1997). Fish Bioenergetics 3.0. UW Sea Grant, WISCU-T-97-001.
 */

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