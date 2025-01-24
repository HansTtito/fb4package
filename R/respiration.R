
########################################################################
### Respiration models
########################################################################

Rf1T <- function(Temperature) { ### Temperature function equation 1 (Hanson et al. 1997; Stewart et al. 1983)
  ft <- exp(RQ*Temperature)
  return(ft)
}

RACTf1T <- function(W,Temperature) { ### Temperature function equation 1 with activity component (Hanson et al. 1997; Stewart et al. 1983)
  if(Temperature <= RTL) {VEL <- ACT * W ^ RK4 * exp(BACT * Temperature)
  } else if(Temperature >  RTL) {VEL <- RK1 * W ^ RK4 * exp(RK5 * Temperature)}
  ACTIVITY <- exp(RTO * VEL)
  return(ACTIVITY)
}

respiration <- function(Temperature, W, REQ) { ### Respiration function
  Rmax <- RA * W ^ RB
  if(REQ == 1) {
    ft <- Rf1T(Temperature)
    ACTIVITY <- RACTf1T(W,Temperature)
  } else if(REQ == 2) {
    ft <- Rf2T(Temperature)
    ACTIVITY <- ACT
  }
  R <- (Rmax * ft * ACTIVITY)
  return(R)
}

