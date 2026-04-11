#' Fish Bioenergetics 4.0 Official Parameters Database
#'
#' @description
#' Comprehensive database containing species-specific bioenergetic parameters for the 
#' Fish Bioenergetics 4.0 model. This database includes consumption, respiration, 
#' egestion, excretion, and predator energy density parameters for multiple fish 
#' species across different life stages.
#'
#' @format A list containing bioenergetic parameters for fish species with the following structure:
#' \describe{
#'   \item{species_name}{List for each species containing:}
#'   \item{species_info}{Basic taxonomic information (scientific name, common name, family, order)}
#'   \item{life_stages}{Named list of life stages (e.g., "juvenile", "adult", "larval") containing:}
#'   \item{consumption}{Consumption parameters (CA, CB, CQ, CTO, CTM, CTL, CK1, CK4, CEQ)}
#'   \item{respiration}{Respiration parameters (RA, RB, RQ, RTO, RTM, RTL, RK1, RK4, RK5, REQ)}
#'   \item{activity}{Activity multipliers (ACT, BACT)}
#'   \item{sda}{Specific Dynamic Action coefficient (SDA)}
#'   \item{egestion}{Egestion parameters (FA, FB, FG, EGEQ)}
#'   \item{excretion}{Excretion parameters (UA, UB, UG, EXEQ)}
#'   \item{predator}{Predator energy density parameters (Alpha1, Beta1, Alpha2, Beta2, Cutoff, ED_data, PREDEDEQ)}
#'   \item{source}{Literature source reference}
#'   \item{notes}{Additional notes about the parameters}
#'   \item{sources}{Vector of literature sources for the species}
#' }
#'
#' @details
#' The database contains parameters for fish species from multiple families including 
#' Salmonidae, Percidae, Centrarchidae, Cyprinidae, and others. Each species entry 
#' includes one or more life stages with complete or partial parameter sets.
#'
#' \strong{Parameter Categories:}
#' \describe{
#'   \item{\strong{Consumption:} }{Temperature-dependent consumption model parameters}
#'   \item{\strong{Respiration:} }{Metabolic rate and activity parameters}
#'   \item{\strong{Egestion:} }{Waste production and defecation parameters}
#'   \item{\strong{Excretion:} }{Nitrogenous waste excretion parameters}
#'   \item{\strong{Predator Energy Density:} }{Weight-dependent energy content parameters}
#' }
#'
#' \strong{Temperature Parameters:}
#' \describe{
#'   \item{CTO, RTO: }{Optimum temperature for consumption/respiration}
#'   \item{CTM, RTM: }{Maximum temperature for consumption/respiration}
#'   \item{CTL, RTL: }{Lethal temperature for consumption/respiration}
#' }
#'
#' \strong{Usage:}
#' This database is primarily used with the \code{\link{Bioenergetic}} constructor 
#' to create species-specific bioenergetic model objects. Parameters can be extracted 
#' using utility functions or accessed directly by species and life stage.
#'
#' @source
#' Parameters extracted from \code{Parameters_official.csv}, the official
#' species database bundled with the Fish Bioenergetics 4.0 ('FB4') Shiny
#' application (<https://github.com/biofish/FishBioenergetics>).
#' Converted to R list format via \code{generate_fish4_parameters()}.
#' Original parameter values compiled from peer-reviewed literature; see
#' the \code{source} field within each species entry for individual
#' references.
#'
#' @examples
#' data(fish4_parameters)
#' head(names(fish4_parameters))
#' salmon_info <- fish4_parameters[["Oncorhynchus mykiss"]]
#' names(salmon_info$life_stages)
#' juvenile_params <- salmon_info$life_stages$juvenile
#' names(juvenile_params$consumption)
#'
#' @seealso 
#' \code{\link{Bioenergetic}}, \code{\link{run_fb4}}
#'
#' @references
#' Deslauriers, D., Chipps, S.R., Breck, J.E., Rice, J.A., Madenjian, C.P. (2017). 
#' Fish Bioenergetics 4.0: An R-based modeling application. 
#' \emph{Fisheries}, 42(11), 586–596.
#' \doi{10.1080/03632415.2017.1377558}
#'
#' @keywords datasets
"fish4_parameters"

#' Fish Bioenergetics 4.0 Parameters Database Metadata
#'
#' @description
#' Metadata information about the Fish Bioenergetics 4.0 parameters database, 
#' including version information, creation details, and structural specifications.
#'
#' @format A list containing metadata about the fish4_parameters database:
#' \describe{
#'   \item{version}{Version number of the database}
#'   \item{creation_date}{Date when the database was generated}
#'   \item{source_file}{Original CSV file used to generate the database}
#'   \item{description}{Brief description of the database contents}
#'   \item{n_species}{Total number of species in the database}
#'   \item{n_total_records}{Total number of parameter records}
#'   \item{families_included}{Number of taxonomic families represented}
#'   \item{parameter_groups}{Vector of parameter category names}
#'   \item{required_parameters}{List of essential parameters for FB4 simulations}
#'   \item{units}{Description of parameter units and measurements}
#' }
#'
#' @details
#' This metadata object provides essential information about the structure, 
#' content, and requirements of the fish4_parameters database. It includes 
#' quality control information and parameter specifications necessary for 
#' proper use of the bioenergetic model.
#'
#' \strong{Required Parameters:}
#' The metadata specifies which parameters are essential for running 
#' Fish Bioenergetics 4.0 simulations:
#' \describe{
#'   \item{\strong{Consumption:} }{CA, CB, CQ, CTO, CTM, CTL}
#'   \item{\strong{Respiration:} }{RA, RB, RQ, RTO, RTM, RTL}
#' }
#'
#' \strong{Units:}
#' \describe{
#'   \item{Temperature: }{Degrees Celsius}
#'   \item{Energy Density: }{Joules per gram (J/g)}
#'   \item{Weight: }{Grams (g)}
#'   \item{Rates: }{Proportions or model coefficients}
#' }
#'
#' @source
#' Generated automatically when converting \code{Parameters_official.csv}
#' (from the Fish Bioenergetics 4.0 Shiny application,
#' <https://github.com/biofish/FishBioenergetics>) into an R list object.
#'
#' @examples
#' data(fish4_parameters_metadata)
#' print(fish4_parameters_metadata$description)
#' print(paste("Species count:", fish4_parameters_metadata$n_species))
#'
#' @seealso 
#' \code{\link{fish4_parameters}}, \code{\link{Bioenergetic}}
#'
#' @keywords datasets
"fish4_parameters_metadata"