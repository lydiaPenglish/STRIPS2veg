#' Vegetation data for every quadrat 
#'
#' A dataset containing the visual percent cover estimates for all species identified in every quadrat of every strip.
#'
#'
#' @format A data frame with 4597 rows and 4 columns:
#' \describe{
#'   \item{quadratID}{A unique code each quadrat. Reads like "siteID_stripID_quadratNumber"}
#'   \item{siteID}{A unique code for the site}
#'   \item{speciesID}{5 letter code unique to each species - consists of first 3 letters of genus, followed by first two letters of species}
#'   \item{cover}{Visual percent cover of vegetation. Has 7 distinct catagories}
#'   \item{notes}{Notes taken during data collection}
#'   \item{flowering}{Whether or not a species was flowering - only some sites}
#' }
#'
"vegetation"
