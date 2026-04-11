#' Fish Bioenergetics 4.0 Package
#'
#' @description
#' An R package implementation of Fish Bioenergetics 4.0 (FB4), a widely used
#' approach for modelling energy allocation in fish. The model partitions consumed
#' energy into three fundamental components: metabolism (respiration + activity +
#' specific dynamic action), waste losses (egestion + excretion), and growth
#' (somatic + gonadal). The package provides multiple estimation strategies
#' (binary search, direct, bootstrap, maximum likelihood) and an optional
#' C++ backend via TMB for high-performance MLE.
#'
#' @details
#' The bioenergetics energy balance implemented here follows Kitchell et al. (1977):
#'
#' \deqn{C = R + A + \mathrm{SDA} + F + U + G}
#'
#' where \eqn{C} = consumption, \eqn{R} = standard respiration, \eqn{A} = active
#' metabolism, \eqn{\mathrm{SDA}} = specific dynamic action, \eqn{F} = egestion,
#' \eqn{U} = excretion, and \eqn{G} = growth (somatic + gonadal).
#'
#' \strong{Consumption} is modelled as \eqn{C = C_{\max} \cdot p \cdot F(T)},
#' where \eqn{C_{\max} = CA \cdot W^{CB}} (Hartman and Hayward 2007) and \eqn{F(T)}
#' is one of four temperature-dependence functions (CEQ 1–4; Kitchell et al. 1977;
#' Thornton and Lessem 1978).
#'
#' \strong{Respiration} is modelled as
#' \eqn{R = RA \cdot W^{RB} \cdot F(T) \cdot \mathrm{ACT}}, with \eqn{F(T)}
#' following Kitchell et al. (1977) or a simple Q10 exponential (REQ 1–2).
#'
#' \strong{Egestion and excretion} follow Elliott (1976) or Stewart et al. (1983)
#' (EGEQ/EXEQ 1–4).
#'
#' \strong{Predator energy density} can be modelled as weight-dependent or constant
#' (PREDEDEQ 1–2; Hanson et al. 1997).
#'
#' This package extends Fish Bioenergetics 4.0 (Deslauriers et al. 2017), itself
#' an R-based re-implementation of Fish Bioenergetics 3.0 (Hanson et al. 1997)
#' and its predecessors (Hewett and Johnson 1987, 1992).
#'
#' @references
#' Deslauriers, D., Chipps, S.R., Breck, J.E., Rice, J.A. and Madenjian, C.P.
#' (2017). Fish Bioenergetics 4.0: An R-based modeling application.
#' \emph{Fisheries}, 42(11), 586–596.
#' \doi{10.1080/03632415.2017.1377558}
#'
#' Hanson, P.C., Johnson, T.B., Schindler, D.E. and Kitchell, J.F. (1997).
#' \emph{Fish Bioenergetics 3.0}. University of Wisconsin Sea Grant Institute,
#' Madison, WI. WISCU-T-97-001.
#'
#' Kitchell, J.F., Stewart, D.J. and Weininger, D. (1977). Applications of a
#' bioenergetics model to yellow perch (\emph{Perca flavescens}) and walleye
#' (\emph{Stizostedion vitreum vitreum}).
#' \emph{Journal of the Fisheries Research Board of Canada}, 34(10), 1922–1935.
#' \doi{10.1139/f77-258}
#'
#' Thornton, K.W. and Lessem, A.S. (1978). A temperature algorithm for modifying
#' biological rates.
#' \emph{Transactions of the American Fisheries Society}, 107(2), 284–287.
#'
#' Elliott, J.M. (1976). Energy losses in the waste products of brown trout
#' (\emph{Salmo trutta} L.).
#' \emph{Journal of Animal Ecology}, 45(2), 561–580.
#'
#' Stewart, D.J., Weininger, D., Rottiers, D.V. and Edsall, T.A. (1983). An
#' energetics model for lake trout, \emph{Salvelinus namaycush}: application to
#' the Lake Michigan population.
#' \emph{Canadian Journal of Fisheries and Aquatic Sciences}, 40(6), 681–698.
#'
#' Hartman, K.J. and Hayward, R.S. (2007). Bioenergetics. In C.S. Guy and
#' M.L. Brown (eds.), \emph{Analysis and Interpretation of Freshwater Fisheries
#' Data}. American Fisheries Society, Bethesda, MD.
#'
#' Hewett, S.W. and Johnson, B.L. (1987). \emph{A Generalized Bioenergetics
#' Model of Fish Growth for Microcomputers}. University of Wisconsin Sea Grant
#' Institute, Madison, WI.
#'
#' Hewett, S.W. and Johnson, B.L. (1992). \emph{Fish Bioenergetics 2.0}.
#' University of Wisconsin Sea Grant Institute, Madison, WI.
#'
#' Winberg, G.G. (1956). Rate of metabolism and food requirements of fishes.
#' \emph{Fisheries Research Board of Canada Translation Series} No. 194.
#'
#' Breck, J.E. (2014). Body composition in fishes: body size matters.
#' \emph{Aquaculture}, 433, 40–49.
#' \doi{10.1016/j.aquaculture.2014.05.049}
#'
#' Arnot, J.A. and Gobas, F.A.P.C. (2004). A food web bioaccumulation model
#' for organic chemicals in aquatic ecosystems.
#' \emph{Environmental Toxicology and Chemistry}, 23(10), 2343–2355.
#' \doi{10.1897/03-438}
#'
#' @seealso
#' \code{\link{Bioenergetic}}, \code{\link{run_fb4}}, \code{\link{fish4_parameters}}
#'
#' @keywords internal
#' @useDynLib fb4package, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom TMB MakeADFun dynlib
#' @importFrom stats median nlminb optim quantile rnorm sd dlnorm runif var
#'   density lowess cor qnorm
#' @importFrom utils head tail
#' @importFrom tools file_ext
#' @importFrom graphics par plot lines points abline text legend grid hist
#'   barplot polygon arrows rug mtext plot.new plot.window title axis box
#' @importFrom grDevices png pdf dev.off rgb col2rgb rainbow gray.colors
"_PACKAGE"
