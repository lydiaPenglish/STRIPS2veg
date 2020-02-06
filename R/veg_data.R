#' Useful information about plants identified
#'
#' A dataset containing life history and taxonomic information about plants identified in vegetation surveys.
#'
#'
#' @format A data frame with 268 rows and 10 columns:
#' \describe{
#'   \item{full_name}{Scientific name - the genus and species}
#'   \item{common_name}{Common name of a species}
#'   \item{family}{Taxanomic family to which the species belongs}
#'   \item{native}{"Native" or "introduced" indicating origin of species}
#'   \item{life_cycle}{"Annual" "biennial" or "perennial" indicating the longevity of a species}
#'   \item{group}{Life-history group: prairie/weedy forb, C3 grass, C4 grass, sedge , rush, fern, woody}
#'   \item{code}{5 letter code unique to each species. First three letters of genus and first two letters of species}
#'   \item{alternate name}{Secondary common name}
#'   \item{seeds_per_oz}{Number of seeds per ounce, gathered from \href{https://www.prairiemoon.com/}{Prairie Moon Nursery}}
#'   \item{group_simple}{Simplified version of life history group. Consolidates C3, C4 grasses and sedges into just grasses 
#'   and creates "other" group for rushes, ferns, and wetland forbs}
#' }
#'
"species_list"

#' Vegetation data for every quadrat 
#'
#' A dataset containing the visual percent cover estimates for all species identified in every quadrat of every strip.
#'
#'
#' @format A data frame with 10416 rows and 7 columns:
#' \describe{
#'   \item{year}{The year the data was collected}
#'   \item{quadratID}{A unique code each quadrat. Reads like "siteID_stripID_quadratNumber"}
#'   \item{siteID}{A unique 3 letter code for the site}
#'   \item{speciesID}{A unique 5 letter code for each species - consists of first 3 letters of genus, followed by 
#'   first two letters of species. Ex: \emph{Ratibida pinnata} = ratpi}
#'   \item{cover}{Visual percent cover of vegetation. Has 7 distinct catagories. }
#'   \item{notes}{Any notes taken during data collection}
#'   \item{flowering}{Whether or not a species was flowering - This was data we thought we'd collect throughout sampling
#'   for another project, but we really didn't end up consistently recording it. Therefore it can largely be ignored.}
#' }
#'
"vegetation"
