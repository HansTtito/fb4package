% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/08-mortality-reproduction.R
\name{calculate_mortality_reproduction}
\alias{calculate_mortality_reproduction}
\title{Calcular mortalidad y reproducción diaria}
\usage{
calculate_mortality_reproduction(
  current_weight,
  temperature,
  day_of_year,
  mortality_params,
  reproduction_params = NULL,
  initial_weight = NULL
)
}
\arguments{
\item{current_weight}{Peso actual del pez (g)}

\item{temperature}{Temperatura del agua (°C)}

\item{day_of_year}{Día del año (1-365)}

\item{mortality_params}{Lista con parámetros de mortalidad}

\item{reproduction_params}{Lista con parámetros de reproducción (opcional)}

\item{initial_weight}{Peso inicial para cálculos relativos (opcional)}
}
\value{
Lista con resultados de mortalidad y reproducción
}
\description{
Función principal para calcular efectos de mortalidad y reproducción
}
