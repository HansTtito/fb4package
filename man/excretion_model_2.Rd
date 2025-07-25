% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-egestion-excretion.R
\name{excretion_model_2}
\alias{excretion_model_2}
\title{Excretion model 2 - With temperature and feeding dependence (Low-level)}
\usage{
excretion_model_2(consumption, egestion, temperature, p_value, UA, UB, UG)
}
\arguments{
\item{consumption}{Consumption (J/g)}

\item{egestion}{Egestion (J/g)}

\item{temperature}{Water temperature (°C)}

\item{p_value}{Proportion of maximum consumption (p-value)}

\item{UA}{Base excretion parameter}

\item{UB}{Temperature dependence coefficient}

\item{UG}{Feeding level dependence coefficient}
}
\value{
Excretion (J/g)
}
\description{
Excretion model dependent on temperature and feeding level
}
\keyword{internal}
