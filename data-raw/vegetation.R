library("dplyr")
library("tidyr")
library("readr")

my_read_csv = function(f, into) {
  readr::read_csv(
    file = f,
    col_types = readr::cols(quadratID         = readr::col_character(),
                            speciesID         = readr::col_character(),
                            cover             = readr::col_character(),
                            notes             = readr::col_character(),
                            flowering         = readr::col_character())) %>%
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

vegetation <- read_dir(path = "vegetation",
                       pattern = "*.csv",
                       
                       # into provides the directory and filename structure
                       # where every non-alphanumeric character splits the
                       # directory/filename
                       into = c("vegetation",
                                "year","month","day",
                                "siteID", "csv")) %>%
  mutate(
    siteID = toupper(siteID), 
    
    # Order cover by amount of cover
    cover = factor(cover, levels = c("<1", "1-5","5-25","25-50","50-75","75-95",">95")),
    
    # speciesID = factor(speciesID), # not sure I want speciesID to be a factor...
  
    # fixing common mistakes where species were recorded with the wrong code
    speciesID = replace(speciesID, speciesID == "setlu", "setpu"),       # yellow foxtail
    speciesID = replace(speciesID, speciesID == "amata", "amatu"),       # waterhemp
    speciesID = replace(speciesID, speciesID == "viola", "vioso"),       # common violet
    speciesID = replace(speciesID, speciesID == "gaipu", "gaiar"),       # blanket flower
    speciesID = replace(speciesID, speciesID == "spoas", "spoco"),       # rough/tall dropseed, considering to be one spp. 
  
    # Fixing errors that have to do with species recording
    
    # 1. changing liaas to liapy in STN's site. He seeded liaas, but the photos I took show it's actually liapy. 
    speciesID = if_else(siteID == "STN" & speciesID == "liaas", "liapy", speciesID),
    # 2. changing spocr to spoas in MUG's site. He seeded spoas and spohe but I wrote down "spocr." Since the grass was not 
    # flowering I'm assuming I just guessed wrong.
    speciesID = if_else(siteID == "MUG" & speciesID == "spocr", "spoas", speciesID),
    # 3. changing carvu to caran at RDM, since carvu wasn't seeded in RDM and caran was.
    speciesID = if_else(siteID == "RDM" & speciesID == "carvu", "caran", speciesID)) %>%
  
  select(year, quadratID, siteID, speciesID, cover, notes, flowering)

usethis::use_data(vegetation, overwrite = TRUE)
