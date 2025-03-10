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





########################################################################
### Predator energy density
########################################################################

# We only need to read the Predator_E_File and set up daily vectors if PREDEDEQ == 1;
FB4.Param = append(FB4.Param, "PREDEDEQ")
FB4.Value = append(FB4.Value,  PREDEDEQ)  # record Predator Energy Density Equation used

if(PREDEDEQ == 1) {
  if(file.exists(Predator_E_File)){
    Predator_E <- read.csv(Predator_E_File,head=TRUE, stringsAsFactors = FALSE)
  } else {
    print(paste("Cannot find Predator_E_File: ",Predator_E_File))
    exit()
  }
  FB4.Param = append(FB4.Param, "Predator_ED")
  FB4.Value = append(FB4.Value, as.character(Predator_E_File))  # record file used
  Day_pred <- Predator_E[,1] # Days
  Pred_E <- Predator_E[,2]  # Just use the predator energy values, which are in column 2
  last_day_pred <- tail(Day_pred, n = 1)  # get the total number of days + 1
  Dayz_pred <- approx(Day_pred,Pred_E, n = last_day_pred, method="linear")$x
  Dayz_pred <- Dayz_pred[First_day:Last_day]
  Pred_E <- approx(Day_pred,Pred_E, n = last_day_pred, method="linear")$y # interpolate temperature data
  Pred_model <- data.frame(Y_Pred=Pred_E[c(last_day_pred-1,last_day_pred)],
                           X_Pred=c(last_day_pred-1,last_day_pred))
  predict_Pred_Eplusone <- lm(Y_Pred ~ X_Pred,data=Pred_model)
  new <- data.frame(X_Pred=last_day_pred+1)
  last_Pred_E <- predict(predict_Pred_Eplusone,new)
  Pred_E <- c(Pred_E,last_Pred_E)
  Pred_E <- Pred_E[First_day:(Last_day+1)]  ## added by JEB
} else if(PREDEDEQ == 2) {
  FB4.Param = append(FB4.Param, "Predator_ED")
  FB4.Value = append(FB4.Value, "ED = a+b*W")  # record function used
} else if(PREDEDEQ == 3) {
  FB4.Param = append(FB4.Param, "Predator_ED")
  FB4.Value = append(FB4.Value, "ED = a*W**b")  # record function used
}



