library("dplyr")
library("tidyr")
library("readr")
library("stringr")
library("lubridate")


all_site_info <- read_csv("all_site_info.csv") %>%  
    mutate(siteID = factor(toupper(siteID)),
           # parsing date seeded
           date_seeded = lubridate::mdy(date_seeded)) %>%
    # -- no longer just selecting a few columns -- # 
    # select(siteID, season_seeded, year_seeded, area_in_strips, seeding_method,
    #     species_seeded, management, nurse_crop)%>%
  arrange(siteID)

usethis::use_data(all_site_info, overwrite = TRUE)

species_list <- read_csv("species_list.csv")%>%
  # capilatizing first letters 
    mutate(full_name   = str_to_sentence(full_name),
           common_name = str_to_sentence(common_name))

usethis::use_data(species_list, overwrite = TRUE)
