library("dplyr")
library("tidyr")
library("readr")

all_site_info <- read_csv("all_site_info.csv") %>%  
mutate(
    siteID = factor(toupper(siteID))) %>%
  select(siteID, season_seeded, year_seeded, area_in_strips, seeding_method,
         species_seeded, management, nurse_crop)
devtools::use_data(all_site_info, overwrite = TRUE)

species_list <- read_csv("species_list.csv")
devtools::use_data(species_list, overwrite = TRUE)
