% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/14-bioenergetic-classes.R
\name{list_species}
\alias{list_species}
\title{List Available Species}
\usage{
list_species(fish4_db = NULL, family = NULL)
}
\arguments{
\item{fish4_db}{Fish bioenergetics database (optional)}

\item{family}{Taxonomic family filter (optional)}
}
\value{
Character vector of available species names
}
\description{
Lists available species in the fish bioenergetics parameter database,
with optional filtering by taxonomic family.
}
\details{
If no database is provided, the function searches for the default
fish4_parameters database in the environment or loads it from file.
Family filtering allows users to focus on related species with
similar bioenergetic characteristics.
}
\examples{
\dontrun{
# List all species
all_species <- list_species()

# List only salmon species
salmon_species <- list_species(family = "Salmonidae")
}

}
\seealso{
\code{\link{species_info}}
}
