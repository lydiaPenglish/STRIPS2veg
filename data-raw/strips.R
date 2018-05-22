library("dplyr")
library("tidyr")

my_read_csv = function(f, into) {
  readr::read_csv(f,
                  col_types = cols(stripID         = col_integer(),
                                        area            = col_double(),
                                        perimeter       = col_double(),
                                        number_quadrats = col_integer())) %>%
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

strips <- read_dir(path = "strips",
         pattern = "*.csv",
         into = c("site","siteID","csv")) %>%

  # Make unique stripID
  mutate(stripID = formatC(stripID, width = 2, format = "d", flag = "0"), # add preceeding zeros for proper ordering
         stripID = paste(siteID, stripID, sep="")) %>%

  select(stripID, siteID, number_quadrats, area, perimeter)

devtools::use_data(strips, overwrite = TRUE)
