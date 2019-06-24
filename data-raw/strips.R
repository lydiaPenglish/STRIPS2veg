library("dplyr")
library("tidyr")
library("readr")

my_read_csv = function(f, into) {
  readr::read_csv(
    file = f,
    col_types = readr::cols(stripID         = readr::col_integer(),
                            area            = readr::col_double(),
                            perimeter       = readr::col_double(),
                            number_quadrats = readr::col_integer())) %>%
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

strips <- read_dir(path = "strips",
                   pattern = "*.csv",
                   into = c("site","year","siteID","csv")) %>%
  
  # Make unique stripID
  mutate(stripID = formatC(stripID, width = 2, format = "d", flag = "0"), # add preceeding zeros for proper ordering
         stripID = paste(siteID, stripID, sep="_"),
         stripID = toupper(stripID),
         
         siteID = toupper(siteID)) %>%
  
  select(year, stripID, siteID, area, perimeter)

usethis::use_data(strips, overwrite = TRUE)
