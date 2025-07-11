% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/07-nutrient-regeneration.R
\name{calculate_nitrogen_allocation}
\alias{calculate_nitrogen_allocation}
\title{Asignación de nitrógeno en el modelo bioenergético}
\usage{
calculate_nitrogen_allocation(
  consumption,
  prey_n_concentrations,
  n_assimilation_efficiency,
  weight_gain,
  predator_n_concentration
)
}
\arguments{
\item{consumption}{Vector de consumo por tipo de presa (g/día)}

\item{prey_n_concentrations}{Vector de concentraciones de N en presas (g N/g peso húmedo)}

\item{n_assimilation_efficiency}{Vector de eficiencias de asimilación de N (fracción 0-1)}

\item{weight_gain}{Ganancia de peso del depredador (g/día)}

\item{predator_n_concentration}{Concentración de N en depredador (g N/g peso húmedo)}
}
\value{
Lista con flujos de nitrógeno
}
\description{
Calcula el balance de nitrógeno en consumo, crecimiento, excreción y egestion
}
\keyword{internal}
