% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00-utils.R
\name{calculate_daily_mean_prey_energy}
\alias{calculate_daily_mean_prey_energy}
\title{Calculate Daily Mean Prey Energy}
\usage{
calculate_daily_mean_prey_energy(daily_proportions, daily_energies)
}
\arguments{
\item{daily_proportions}{Diet proportions for the day}

\item{daily_energies}{Energy densities of prey items for the day}
}
\value{
Weighted mean prey energy density (J/g)
}
\description{
Calculates the weighted mean energy density of prey items
based on diet proportions and individual prey energy densities.
}
\details{
This function computes the effective energy density of the diet
by weighting each prey type's energy density by its proportion
in the diet. This value is used in consumption calculations
to convert between mass and energy units.

Formula: mean_energy = Σ(proportion_i × energy_i)
}
\examples{
\dontrun{
# Diet: 60\% fish (4000 J/g), 40\% zooplankton (2500 J/g)
proportions <- c(0.6, 0.4)
energies <- c(4000, 2500)
mean_energy <- calculate_daily_mean_prey_energy(proportions, energies)
# Returns: 0.6 * 4000 + 0.4 * 2500 = 3400 J/g
}
}
\keyword{internal}
