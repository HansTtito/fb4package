% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01-consumption.R
\name{calculate_consumption}
\alias{calculate_consumption}
\title{Calculate daily consumption (Mid-level - Main function)}
\usage{
calculate_consumption(
  temperature,
  weight,
  p_value,
  consumption_params,
  method = "rate"
)
}
\arguments{
\item{temperature}{Water temperature (°C)}

\item{weight}{Fish weight (g)}

\item{p_value}{Proportion of maximum consumption (0-5)}

\item{consumption_params}{List with consumption parameters}

\item{method}{Calculation method ("maximum", "rate", "specific")}
}
\value{
Specific consumption (g prey/g fish/day)
}
\description{
Main consumption calculation function called from simulation loop
}
