#' Fish Bioenergetics 4.0 Package
#'
#' @description
#' An R implementation of fish bioenergetics modeling with TMB backend
#' for high-performance maximum likelihood estimation.
#'
#' @keywords internal
#' @useDynLib fb4package, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom TMB MakeADFun dynlib
#' @importFrom stats median nlminb optim quantile rnorm sd
"_PACKAGE"