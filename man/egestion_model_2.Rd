% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-egestion-excretion.R
\name{egestion_model_2}
\alias{egestion_model_2}
\title{Egestion model 2 - Elliott (1976) (Low-level)}
\usage{
egestion_model_2(consumption, temperature, p_value, FA, FB, FG)
}
\arguments{
\item{consumption}{Consumption (J/g)}

\item{temperature}{Water temperature (°C)}

\item{p_value}{Proportion of maximum consumption (p-value)}

\item{FA}{Base egestion parameter}

\item{FB}{Temperature dependence coefficient}

\item{FG}{Feeding level dependence coefficient}
}
\value{
Egestion (J/g)
}
\description{
Egestion model dependent on temperature and feeding level
}
\keyword{internal}
