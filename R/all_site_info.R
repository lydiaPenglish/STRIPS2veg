#' Useful information for every site
#'
#' A dataset containing information about every site visited for vegetation data collection
#'
#'
#' @format A data frame with 21 rows and 8 columns:
#' \describe{
#'   \item{siteID}{A unique code for the site}
#'   \item{season_seeded}{The time of year the strip was planted}
#'   \item{year_seeded}{The year the strip was planted}
#'   \item{area_in_strips}{The area of a field in prairie strips (in m2)}
#'   \item{seedling_method}{What kind of equipment was used to seed the strip}
#'   \item{species_seeded}{The number of species in the seed mix}
#'   \item{management}{Management done on a site. "Burn" indicates the site was burned}
#'   \item{nurse_crop}{The type of crop seeded with the prairie mix, usually oats}
#' }
#'
"all_site_info"
