% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-egestion-excretion.R
\name{excretion_model_4}
\alias{excretion_model_4}
\title{Excretion model 4 - Without feeding dependence (Low-level)}
\usage{
excretion_model_4(consumption, egestion, temperature, UA, UB)
}
\arguments{
\item{consumption}{Consumption (J/g)}

\item{egestion}{Egestion (J/g)}

\item{temperature}{Water temperature (°C)}

\item{UA}{Base excretion parameter}

\item{UB}{Temperature dependence coefficient}
}
\value{
Excretion (J/g)
}
\description{
Excretion model with only temperature dependence
}
\keyword{internal}
