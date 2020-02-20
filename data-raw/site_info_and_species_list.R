library("dplyr")
library("tidyr")
library("readr")
library("stringr")
library("lubridate")


all_site_info <- read_csv("all_site_info.csv") %>%  
  mutate(siteID = factor(toupper(siteID)),
         # parsing date seeded
         date_seeded = lubridate::mdy(date_seeded),
         # adding age based on age at January 1st 2020
         age_days = interval(date_seeded, lubridate::dmy("01-01-2020")),
         age_yrs  = round(time_length(age_days, unit = "year"), 1),
         # convert area to acres
         acres_in_strips = area_in_strips*0.000247105) %>%
  select(-age_days)%>%
  arrange(siteID)

usethis::use_data(all_site_info, overwrite = TRUE)

species_list <- read_csv("species_list.csv")%>%
  # capilatizing first letters 
    mutate(full_name    = str_to_sentence(full_name),
           common_name  = str_to_sentence(common_name),
  # making a new variable with even simpler group names         
           group_simple = dplyr::recode(group, "prairie C3 grass" = "prairie grass",
                                        "prairie C4 grass" = "prairie grass",
                                        "prairie sedge"    = "prairie grass",
                                        "weedy C3 grass"   = "weedy grass",
                                        "weedy C4 grass"   = "weedy grass",
                                        "weedy sedge"      = "weedy grass",
                                        "fern"             = "other",
                                        "rush"             = "other",
                                        "wetland forb"     = "other"))

usethis::use_data(species_list, overwrite = TRUE)
