########################################################################
### Body Composition
###   as Protein, Lipid, Ash and Water
###   and energy density
########################################################################
# Fish proximate body composition, based on Breck (2014)
# Breck, J.E. 2014. Body composition in fishes: body size matters. Aquaculture 433:40-49.
#
# Estimate g Protein from g Water
Protein = function(H2O) {
  # Regression from Breck (2014), N = 101, r2 = 0.9917
  Pro = 10**(-0.8068 + 1.0750 * log10(H2O))
  return(Pro)
}
# Estimate g Ash from g Water
Ash = function(H2O){
  # Regression from Breck (2014), N = 101, r2 = 0.9932
  A.g = 10**(-1.6765 + 1.0384 * log10(H2O))
  return(A.g)
}
#
# Estimate g Fat from W and g water, g Protein, g Ash, by subtraction
Fat = function(W, H2O, Pro, Ash){
  F.g = W - H2O - Pro - Ash
  return(F.g)
}
# Estimate energy density from Fat and Protein content
EnDen = function(Fat, Pro, W){
  ED = (Fat.g*36200 + Pro.g*23600)/W  # J/g wet weight
  return(ED)
}
