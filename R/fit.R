########################################################################
### Binary search algorithm for p-value
########################################################################

fit.p <- function(p, IW, FW, W.tol, max.iter) {
  W      <- IW    # Initial weight
  n.iter <- 0     # Counter for number of iterations
  p.max  <- 5.00  # current max
  p.min  <- 0.00  # current min
  outpt <- "End"  # desire only ending weight or consumption value, not full vector; revised by JEB
  fit_p_Flag <- FALSE # at start, no fit has been found
  # withProgress(message = 'Calculating ...', min=0, max=max.iter, value = 0, {  # revised by JEB
  # initialize W.p
  W.p <- grow(Temperature, W, p, outpt,globalout_Prey, globalout_Prey_E)
  while((n.iter <= max.iter) & (abs(W.p-FW) > W.tol)) {
    n.iter <- n.iter + 1
    # incProgress(1, detail = paste("Doing iteration", n.iter))  # added by JEB
    if(W.p > FW) {p.max <- p} else {p.min <- p}
    p <- (p.min + p.max)/2 #p.min + (p.max - p.min)/2
    W.p <- grow(Temperature, W, p, outpt,globalout_Prey, globalout_Prey_E)
  }
  # })  # end of "withProgress" function; added by JEB
  if(abs(W.p-FW) <= W.tol) {fit_p_Flag <- TRUE} # adequate fit has been reached
  return(c(p, fit_p_Flag)) (g)  ## fit_p_Flag added by JEB
}  # end of fit.p function

