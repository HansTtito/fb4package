#ifndef FB4_UTILS_HPP
#define FB4_UTILS_HPP


namespace fb4 {

// ============================================================================
// BASIC MATHEMATICAL FUNCTIONS FOR FB4 TMB
// ============================================================================

// Safe Square Root Function
template<class Type>
Type safe_sqrt(Type x, Type min_val = Type(0.0)) {
  // Handle non-finite values
  if (!R_finite(asDouble(x))) {
    return Type(NAN);
  }
  
  // Handle negative values
  if (x < min_val) {
    return sqrt(min_val);
  }
  
  return sqrt(x);
}

// Safe Exponential Function
template<class Type>
Type safe_exp(Type x, Type max_exp = Type(700.0)) {
  // Handle non-finite values
  if (!R_finite(asDouble(x))) {
    return Type(NAN);
  }
  
  // Handle overflow (large positive values)
  if (x > max_exp) {
    return exp(max_exp);
  }
  
  // Handle underflow (large negative values)
  if (x < -max_exp) {
    return exp(-max_exp);  // Will be close to 0
  }
  
  return exp(x);
}

// Clamp Values Between Minimum and Maximum
template<class Type>
Type clamp(Type x, Type min_val, Type max_val, const char* name = "value") {
  // Handle non-finite values
  if (!R_finite(asDouble(x))) {
    return Type(NAN);
  }
  
  // Clamp to bounds
  if (x < min_val) {
    return min_val;
  }
  if (x > max_val) {
    return max_val;
  }
  
  return x;
}

// Basic validation function
template<class Type>
bool check_numeric_value(Type value, Type min_val = Type(-INFINITY), Type max_val = Type(INFINITY)) {
  if (!isFinite(value)) {
    return false;
  }
  
  if (value < min_val || value > max_val) {
    return false;
  }
  
  return true;
}

} // namespace fb4

#endif // FB4_UTILS_HPP