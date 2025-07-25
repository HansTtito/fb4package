% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05-body-composition.R
\name{analyze_composition_by_size}
\alias{analyze_composition_by_size}
\title{Analyze body composition by size range (Utility)}
\usage{
analyze_composition_by_size(
  weight_range = c(1, 500),
  n_points = 50,
  water_fraction = 0.728
)
}
\arguments{
\item{weight_range}{Weight range to analyze (2-element vector)}

\item{n_points}{Number of points to analyze}

\item{water_fraction}{Water fraction (constant or function of weight)}
}
\value{
Data frame with composition analysis by size
}
\description{
Analyzes body composition across a range of fish sizes
}
