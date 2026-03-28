#ifndef FB4_VALIDATION_HPP
#define FB4_VALIDATION_HPP


#include "fb4_utils.hpp"

/*
 * Validation Functions for FB4 Model
 * 
 * Provides parameter validation, range checking, and biological constraint enforcement for TMB implementation.
 * 
 * Note: This is a simplified version focusing on critical validations that can be performed efficiently in TMB. Full validation is done in R preprocessing.
 */

namespace fb4 {

// ============================================================================
// VALIDATION RESULT STRUCTURE
// ============================================================================

/**
 * Structure to hold validation results
 */
struct ValidationResult {
  bool valid;           // Overall validation status
  int error_count;     // Number of errors found
  int warning_count;   // Number of warnings found
  
  // Constructor
  ValidationResult() : valid(true), error_count(0), warning_count(0) {}
};

// ============================================================================
// PARAMETER RANGE VALIDATION
// ============================================================================

/**
 * Validate parameter within specified range
 * 
 * @param value Parameter value to validate
 * @param min_val Minimum allowed value
 * @param max_val Maximum allowed value
 * @param param_name Parameter name for error reporting
 * @return True if valid, false otherwise
 */
template<class Type>
bool validate_parameter_range(Type value, Type min_val, Type max_val, 
                              const std::string& param_name = "parameter") {
  if (value < min_val || value > max_val) {
    return false;
  }
  return true;
}

/**
 * Validate that parameter is positive
 * 
 * @param value Parameter value to validate
 * @param param_name Parameter name for error reporting
 * @return True if positive, false otherwise
 */
template<class Type>
bool validate_positive(Type value, const std::string& param_name = "parameter") {
  return value > Type(0.0);
}

/**
 * Validate that parameter is a valid fraction (0-1)
 * 
 * @param value Parameter value to validate
 * @param param_name Parameter name for error reporting
 * @return True if valid fraction, false otherwise
 */
template<class Type>
bool validate_fraction(Type value, const std::string& param_name = "parameter") {
  return (value >= Type(0.0) && value <= Type(1.0));
}

/**
 * Validate temperature within biological range
 * 
 * @param temperature Temperature value (°C)
 * @return True if valid, false otherwise
 */
template<class Type>
bool validate_temperature(Type temperature) {
  return validate_parameter_range(temperature, Type(-5.0), Type(45.0), "temperature");
}

/**
 * Validate weight within reasonable range
 * 
 * @param weight Weight value (g)
 * @return True if valid, false otherwise
 */
template<class Type>
bool validate_weight(Type weight) {
  return validate_parameter_range(weight, Type(0.001), Type(1e6), "weight");
}

/**
 * Validate p_value within FB4 range
 * 
 * @param p_value p_value for consumption
 * @return True if valid, false otherwise
 */
template<class Type>
bool validate_p_value(Type p_value) {
  return validate_parameter_range(p_value, Type(0.001), Type(5.0), "p_value");
}

// ============================================================================
// CONSUMPTION PARAMETER VALIDATION
// ============================================================================

/**
 * Validate consumption parameters for specified equation
 * 
 * @param CEQ Consumption equation number
 * @param CA Consumption parameter A
 * @param CB Consumption parameter B
 * @param CQ Q10 value
 * @param CTM Maximum temperature (for equations 2,3)
 * @param CTO Optimum temperature (for equations 2,3)
 * @return Validation result
 */
template<class Type>
ValidationResult validate_consumption_params(int CEQ, Type CA, Type CB, Type CQ,
                                             Type CTM = Type(0.0), Type CTO = Type(0.0)) {
  ValidationResult result;
  
  // Basic parameter validation
  if (!validate_positive(CA, "CA")) {
    result.valid = false;
    result.error_count++;
  }
  
  if (!validate_positive(CQ, "CQ")) {
    result.valid = false;
    result.error_count++;
  }
  
  // Equation-specific validation
  if (CEQ == 2 || CEQ == 3) {
    if (!validate_temperature(CTM)) {
      result.valid = false;
      result.error_count++;
    }
    
    if (!validate_temperature(CTO)) {
      result.valid = false;
      result.error_count++;
    }
    
    // Temperature hierarchy check
    if (CTM <= CTO) {
      result.valid = false;
      result.error_count++;
    }
  }
  
  return result;
}

// ============================================================================
// RESPIRATION PARAMETER VALIDATION
// ============================================================================

/**
 * Validate respiration parameters for specified equation
 * 
 * @param REQ Respiration equation number
 * @param RA Respiration parameter A
 * @param RB Respiration parameter B
 * @param RQ Q10 value
 * @param RTO Optimum temperature
 * @param RTM Maximum temperature
 * @param ACT Activity multiplier
 * @param SDA Specific Dynamic Action coefficient
 * @return Validation result
 */
template<class Type>
ValidationResult validate_respiration_params(int REQ, Type RA, Type RB, Type RQ,
                                             Type RTO, Type RTM, Type ACT, Type SDA) {
  ValidationResult result;
  
  // Basic parameter validation
  if (!validate_positive(RA, "RA")) {
    result.valid = false;
    result.error_count++;
  }
  
  if (!validate_positive(RQ, "RQ")) {
    result.valid = false;
    result.error_count++;
  }
  
  if (!validate_temperature(RTO)) {
    result.valid = false;
    result.error_count++;
  }
  
  if (!validate_temperature(RTM)) {
    result.valid = false;
    result.error_count++;
  }
  
  if (!validate_positive(ACT, "ACT")) {
    result.valid = false;
    result.error_count++;
  }
  
  if (!validate_fraction(SDA, "SDA")) {
    result.valid = false;
    result.error_count++;
  }
  
  // Temperature hierarchy check for equation 1
  if (REQ == 1 && RTM > Type(0.0) && RTM <= RTO) {
    result.valid = false;
    result.error_count++;
  }
  
  return result;
}

// ============================================================================
// EGESTION AND EXCRETION VALIDATION
// ============================================================================

/**
 * Validate egestion parameters
 * 
 * @param EGEQ Egestion equation number
 * @param FA Base egestion fraction
 * @param FB Temperature coefficient (for equations 2,3,4)
 * @return Validation result
 */
template<class Type>
ValidationResult validate_egestion_params(int EGEQ, Type FA, Type FB = Type(0.0)) {
  ValidationResult result;
  
  if (!validate_fraction(FA, "FA")) {
    result.valid = false;
    result.error_count++;
  }
  
  // For equations with temperature dependence
  if (EGEQ >= 2) {
    if (!validate_parameter_range(FB, Type(-5.0), Type(5.0), "FB")) {
      result.warning_count++;
    }
  }
  
  return result;
}

/**
 * Validate excretion parameters
 * 
 * @param EXEQ Excretion equation number
 * @param UA Base excretion coefficient
 * @param UB Weight coefficient (for equations 2,3,4)
 * @return Validation result
 */
template<class Type>
ValidationResult validate_excretion_params(int EXEQ, Type UA, Type UB = Type(0.0)) {
  ValidationResult result;
  
  if (!validate_positive(UA, "UA")) {
    result.valid = false;
    result.error_count++;
  }
  
  // For equations with weight dependence
  if (EXEQ >= 2) {
    if (!validate_positive(UB, "UB")) {
      result.valid = false;
      result.error_count++;
    }
  }
  
  return result;
}

// ============================================================================
// PREDATOR ENERGY DENSITY VALIDATION
// ============================================================================

/**
 * Validate predator energy density parameters
 * 
 * @param PREDEDEQ Predator energy density equation number
 * @param Alpha1 Alpha parameter 1
 * @param Beta1 Beta parameter 1
 * @param Cutoff Cutoff weight (for equation 2)
 * @return Validation result
 */
template<class Type>
ValidationResult validate_predator_params(int PREDEDEQ, Type Alpha1, Type Beta1,
                                          Type Cutoff = Type(0.0)) {
  ValidationResult result;
  
  if (PREDEDEQ >= 2) {
    if (!validate_positive(Alpha1, "Alpha1")) {
      result.valid = false;
      result.error_count++;
    }
    
    if (!validate_parameter_range(Beta1, Type(-2.0), Type(2.0), "Beta1")) {
      result.valid = false;
      result.error_count++;
    }
    
    if (PREDEDEQ == 2 && !validate_positive(Cutoff, "Cutoff")) {
      result.valid = false;
      result.error_count++;
    }
  }
  
  return result;
}

// ============================================================================
// BIOLOGICAL CONSTRAINT VALIDATION
// ============================================================================

/**
 * Validate that energy density is within biological range
 * 
 * @param energy_density Energy density (J/g)
 * @return True if valid, false otherwise
 */
template<class Type>
bool validate_energy_density(Type energy_density) {
  return validate_parameter_range(energy_density, Type(1000.0), Type(15000.0), "energy_density");
}

/**
 * Validate that consumption rate is reasonable
 * 
 * @param consumption_rate Consumption rate (g/g/day)
 * @return True if valid, false otherwise
 */
template<class Type>
bool validate_consumption_rate(Type consumption_rate) {
  return validate_parameter_range(consumption_rate, Type(0.0), Type(1.0), "consumption_rate");
}

/**
 * Validate that respiration rate is reasonable
 * 
 * @param respiration_rate Respiration rate (g O2/g/day)
 * @return True if valid, false otherwise
 */
template<class Type>
bool validate_respiration_rate(Type respiration_rate) {
  return validate_parameter_range(respiration_rate, Type(0.0), Type(0.1), "respiration_rate");
}

/**
 * Validate diet proportions sum to approximately 1
 * 
 * @param diet_proportions Vector of diet proportions
 * @param tolerance Tolerance for sum deviation from 1.0
 * @return True if valid, false otherwise
 */
template<class Type>
bool validate_diet_proportions(const vector<Type>& diet_proportions, Type tolerance = Type(0.1)) {
  Type sum = Type(0.0);
  for (int i = 0; i < diet_proportions.size(); i++) {
    if (!validate_fraction(diet_proportions[i], "diet_proportion")) {
      return false;
    }
    sum += diet_proportions[i];
  }
  
  return abs(sum - Type(1.0)) <= tolerance;
}

// ============================================================================
// COMPREHENSIVE VALIDATION FUNCTIONS
// ============================================================================

/**
 * Validate all core FB4 parameters
 * Comprehensive validation for main simulation parameters
 * 
 * @param weight Fish weight (g)
 * @param temperature Water temperature (°C)
 * @param p_value p_value for consumption
 * @param diet_proportions Vector of diet proportions
 * @return Validation result
 */
template<class Type>
ValidationResult validate_core_simulation_params(Type weight, Type temperature, Type p_value,
                                                 const vector<Type>& diet_proportions) {
  ValidationResult result;
  
  if (!validate_weight(weight)) {
    result.valid = false;
    result.error_count++;
  }
  
  if (!validate_temperature(temperature)) {
    result.valid = false;
    result.error_count++;
  }
  
  if (!validate_p_value(p_value)) {
    result.valid = false;
    result.error_count++;
  }
  
  if (!validate_diet_proportions(diet_proportions)) {
    result.valid = false;
    result.error_count++;
  }
  
  return result;
}

/**
 * Validate simulation results for biological realism
 * 
 * @param consumption_rate Daily consumption rate (g/g/day)
 * @param respiration_rate Daily respiration rate (g O2/g/day)
 * @param energy_density Predator energy density (J/g)
 * @param growth_rate Daily growth rate (g/g/day)
 * @return Validation result
 */
template<class Type>
ValidationResult validate_simulation_results(Type consumption_rate, Type respiration_rate,
                                             Type energy_density, Type growth_rate) {
  ValidationResult result;
  
  if (!validate_consumption_rate(consumption_rate)) {
    result.warning_count++;
  }
  
  if (!validate_respiration_rate(respiration_rate)) {
    result.warning_count++;
  }
  
  if (!validate_energy_density(energy_density)) {
    result.warning_count++;
  }
  
  // Check for unrealistic growth
  if (!validate_parameter_range(growth_rate, Type(-0.1), Type(0.2), "growth_rate")) {
    result.warning_count++;
  }
  
  // Check energy balance (consumption should exceed respiration for growth)
  if (growth_rate > Type(0.0) && consumption_rate < respiration_rate) {
    result.warning_count++;
  }
  
  return result;
}

} // namespace fb4

#endif // FB4_VALIDATION_HPP