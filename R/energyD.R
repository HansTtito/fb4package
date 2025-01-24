########################################################################
### Predator Energy Density function
########################################################################

# Define function for obtaining predator energy density
pred_En_D <- function(W,day,PREDEDEQ) {    # Find Energy Density (ED, J/g) for this day and W
  if(PREDEDEQ == 1) {return(Pred_E[day])   # Use daily interpolated values from csv file; ignore weight
  } else if(PREDEDEQ == 3) {return(alpha1*W^beta1)  # ED is power function of weight; ignore day
  } else if(PREDEDEQ == 2) {  # Using two line segments, ED is linear function of weight; ignore day
    Wco = as.numeric(cutoff)  # Wco is weight at cutoff, where the line breaks
    if(W <Wco) {return((as.numeric(alpha1) + as.numeric(beta1)*W))}
    if(W>=Wco) {return((as.numeric(alpha2) + as.numeric(beta2)*W))}
    if(W <Wco && as.numeric(beta1) == 0) {return((as.numeric(alpha1)))}
    if(W>=Wco && as.numeric(beta2) == 0) {return((as.numeric(alpha2)))}
  }  # restructured using "else if" to reduce if-tests; JEB
}  # end of predator energy density section


