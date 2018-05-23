library(STRIPS2PRIVATE)
library(dplyr)
library(tidyr)

my_read_csv = function(f, into) {
  readr::read_csv(f,
                  col_types = cols(quadratID        = col_integer(),
                                   easting            = col_double(),
                                   northing       = col_double())) %>%
    mutate(file=f) %>%
    separate(file, into)
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
                  into = c("siteID","quadrats","csv")) %>%
  
  # Make unique quadratID
  mutate(quadratID = formatC(quadratID, width = 2, format = "d", flag = "0"), # add preceeding zeros for proper ordering
         quadratID = paste(siteID, quadratID, sep="")) %>%
  
  select(quadratID, easting, northing)

## Error here that object "quadratID" not found in mutate function

devtools::use_data(quadrats, overwrite = TRUE)

## an example of how I would anonymize GPS for an individual site...

bue <- read.csv("data-raw/quadrat/bue_quadrats.csv", header = T)
bue$eastingA <- anonymizeGPS(bue$easting, "BUE", direction = "easting")
bue$northingA <- anonymizeGPS(bue$northing, "BUE", direction = "northing")
bue <- select(bue, quadratID, eastingA, northingA)
# And then I can write over the original csv file in the folder with this new one?
