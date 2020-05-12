#' Useful information for every site
#'
#' A dataset containing information about every site visited for vegetation data collection
#'
#'
#' @format A data frame with 26 rows and 12 columns:
#' \describe{
#'   \item{siteID}{A unique 3 letter code for the site}
#'   \item{season_seeded}{The time of year the strip was planted}
#'   \item{date_seeded}{The exact planting day of a site}
#'   \item{year_seeded}{The year the strip was planted}
#'   \item{hectares_in_strips}{The area of a field in prairie strips (ha)}
#'   \item{seeding_method}{What kind of equipment was used to seed the strip}
#'   \item{seed_source}{Where the seed mix was purchased}
#'   \item{species_seeded}{The number of species in the seed mix}
#'   \item{management}{Management done on a site. "Burn" indicates the site was burned}
#'   \item{burn_year}{The year that a site was burned}
#'   \item{burn_notes}{Any other relevant information about how a site is burned}
#'   \item{nurse_crop}{The type of crop seeded with the prairie mix, usually oats}
#' }
#'
"all_site_info"


#' Useful information about strips
#'
#' A dataset containing the area, perimeters, and number of quadrats for each
#' strip within a site.
#'
#' @format A data frame with 141 rows and 5 variables:
#' \describe{
#'   \item{year}{The year that the stripID was generated.}
#'   \item{stripID}{A numerical identifier for the strip. Each identifier begins at 1 for a given site and can be read as 
#'   "siteID_stripID"}
#'   \item{siteID}{A unique 3 letter code for the site}
#'   \item{area}{Area of the strip (m^2^)}
#'   \item{perimeter}{Perimeter of the strip (m)}
#' }
#'
"strips"

#' Useful information about quadrats
#'
#' A dataset containing identifiers and anonymized GPS coordinates for each quadrat visited at each site
#' 
#'
#' @format A data frame with 669 rows and 5 variables:
#' \describe{
#'   \item{year}{The year the quadratID was generated. This column was added because 5 sites were visited during 
#'   just the second field season (2019) and some sites had to have some new quadrats generated between field seasons.}
#'   \item{quadratID}{A unique code for the quadrat (sampling location). Each site has 24 quadrats. 
#'   The number of quadrats per strip is proportional to the area of the strip. QuadratID can be read as 
#'   "siteID_stripNumber_quadratNumber"}
#'   \item{siteID}{A unique 3 letter code for the site}
#'   \item{easting}{Anonymized easting coordinate for quadrat location}
#'   \item{northing}{Anonymized northing coordate for quadrat location}
#' }
#'
"quadrats"
