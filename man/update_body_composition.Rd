% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05-body-composition.R
\name{update_body_composition}
\alias{update_body_composition}
\title{Update body composition during simulation (Mid-level)}
\usage{
update_body_composition(
  old_weight,
  new_weight,
  old_composition = NULL,
  water_fraction_new = 0.728
)
}
\arguments{
\item{old_weight}{Previous weight (g)}

\item{new_weight}{New weight (g)}

\item{old_composition}{Previous composition (optional)}

\item{water_fraction_new}{Water fraction for new weight}
}
\value{
New body composition
}
\description{
Updates body composition as fish grows or changes condition
Used during simulation loops - assumes pre-validated inputs
}
