########################################################################
### Egestion and Excretion models
########################################################################

egestion3 <- function(C,Temperature,p) { ### Egestion model from Stewart et al. (1983)
  PE = FA*(Temperature^FB)*exp(FG*p)
  PFF = sum(globalout_Ind_Prey[i,]*globalout_Prey[i,]) # allows specification of indigestible prey, as proportions
  PF = ((PE-0.1)/0.9)*(1-PFF)+PFF
  Eg = PF*C
  return(Eg)
}

egestion <- function(C, Temperature, p, EGEQ) {
  if(EGEQ == 1) {Eg <- egestion1(C)
  } else if(EGEQ == 2) {Eg <- egestion2(C,Temperature,p)
  } else if(EGEQ == 3) {Eg <- egestion3(C,Temperature,p)
  } else if(EGEQ == 4) {Eg <- egestion4(C,Temperature)
  }  # reformatted to minimize if-tests; JEB
  return(Eg)
}

excretion3 <- function(C,Temperature,p,Eg) {
  U = UA*(Temperature^UB)*exp(UG*p)*(C-Eg)
  return(U)
}

excretion <- function(C, Eg, Temperature, p, EXEQ) {
  if(EXEQ == 1) {U <- excretion1(C,Eg)
  } else if(EXEQ == 2) {U <- excretion2(C,Temperature,p,Eg)
  } else if(EXEQ == 3) {U <- excretion3(C,Temperature,p,Eg)
  } else if(EXEQ == 4) {U <- excretion4(C,Temperature,Eg)
  }  # reformatted to minimize if-checks; JEB
  return(U)
}

