########################################################################
### Specific dynamic action
########################################################################

SpDynAct <- function(C,Eg) { ### Specific dynamic action function (Hanson et al. 1997)
  S <- SDA *(C-Eg)
  return(S)
}

