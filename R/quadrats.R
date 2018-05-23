#' Quadrat locations and unique identifiers
#'
#' A dataset containing anonymized GPS coordinates for each quadrat measured at each site
#' 
#'
#' @format A data frame with 505 rows and 4 variables:
#' \describe{
#'   \item{quadratID}{A unique code for the quadrat. Each site has 24 quadrats. The number of quadrats per strip
#'   is proportional to the area of the strip. QuadratID can be read as "siteID_stripNumber_quadratNumber"}
#'   \item{siteID}{A unique code for the site}
#'   \item{easting}{Anonymized easting coordinate for quadrat location}
#'   \item{northing}{Anonymized northing coordate for quadrat location}
#' }
#'
"quadrats"
