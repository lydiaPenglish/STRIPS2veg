library("STRIPS2PRIVATE")
library("dplyr")
library("tidyr")
library("readr")
library("stringr")

my_read_csv = function(f, into) {
  readr::read_csv(
    file = f,
    col_types = cols(quadratID = readr::col_character(),
                     easting   = readr::col_double(),
                     northing  = readr::col_double())) %>%
    dplyr::mutate(file=f) %>%
    tidyr::separate(file, into)
}

read_dir = function(path, pattern, into) {
  files = list.files(path = path,
                     pattern = pattern,
                     recursive = TRUE,
                     full.names = TRUE)
  plyr::ldply(files, my_read_csv, into = into)
}

# Above modified from https://gist.github.com/jarad/8f3b79b33489828ab8244e82a4a0c5b3
#######################################################################

quadrats <- read_dir(path = "quadrat",
                  pattern = "*.csv",
                  into = c("quadrat","year", "siteID","quadrats","csv")) %>%
  
  mutate(
    siteID    = factor(toupper(siteID)),
    quadratID = str_to_upper(quadratID),
    easting   = anonymizeGPS(easting, siteID, "easting"),
    northing  = anonymizeGPS(northing, siteID, "northing")) %>%
  
  select(year, quadratID, siteID, easting, northing)

usethis::use_data(quadrats, overwrite = TRUE)
