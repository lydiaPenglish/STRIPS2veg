#' Useful information about the plants identified
#'
#' A dataset containing life history information about plants identified at strips sites
#'
#'
#' @format A data frame with 218 rows and 8 columns:
#' \describe{
#'   \item{full_name}{Scientific name - the genus and species}
#'   \item{common_name}{Common name of a species}
#'   \item{family}{Taxanomic family to which the species belongs}
#'   \item{native}{"Native" or "introduced" indicating origin of species}
#'   \item{life_cycle}{"Annual" "biennial" or "perennial" indicating the longevity of a species}
#'   \item{group}{Life-history group: forb, C3 grass, C4 grass, weedy forb, woody}
#'   \item{code}{5 letter code unique to each species. First three letters of genus and first two letters of species}
#'   \item{alternate name}{Secondary common name}
#' }
#'
"species_list"

