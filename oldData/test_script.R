library(tidyverse)
library(iNEXT)

SH <- read.csv("oldData/practiceSHData.csv", header = T)
str(SH)

## clean up datafram

SH[is.na(SH)]<-0
SH.1 <- SH[, colSums(SH != 0) > 0]
SH.2 <- SH.1 %>%
  mutate(id = str_c(Treatment, Year, Quadrat, sep = "_")) %>%
  set_tidy_names(syntactic = T, quiet = F) %>%
  select(id, Acalypha.virginica.rhomboidea:Helianthus.spp.) %>%
  gather(species, abundance, -id) %>%
  spread(id, abundance)
Bass1_2008 <- select(SH.2, species, starts_with("Bass1_2008"))
Bass1_2009 <- select(SH.2, species, starts_with("Bass1_2009"))
Bass2_2008 <- select(SH.2, species, starts_with("Bass2_2008"))
Bass2_2009 <- select(SH.2, species, starts_with("Bass2_2009"))
Bass3_2008 <- select(SH.2, species, starts_with("Bass3_2008"))
Bass3_2009 <- select(SH.2, species, starts_with("Bass3_2009"))
Bass4_2008 <- select(SH.2, species, starts_with("Bass4_2008"))
Bass4_2009 <- select(SH.2, species, starts_with("Bass4_2009"))
Bass5_2008 <- select(SH.2, species, starts_with("Bass5_2008"))
Bass5_2009 <- select(SH.2, species, starts_with("Bass5_2009"))
Int1_2008 <- select(SH.2, species, starts_with("Int1_2008"))
Int1_2009 <- select(SH.2, species, starts_with("Int1_2009"))
Int2_2008 <- select(SH.2, species, starts_with("Int2_2008"))
Int2_2009 <- select(SH.2, species, starts_with("Int2_2009"))
Orb1_2008 <- select(SH.2, species, starts_with("Orb1_2008"))
Orb1_2009 <- select(SH.2, species, starts_with("Orb1_2009"))
Orb2_2008 <- select(SH.2, species, starts_with("Orb2_2008"))
Orb2_2009 <- select(SH.2, species, starts_with("Orb2_2009"))
veg_2008_2009 <- list(Bass1_2008, Bass1_2009, Bass2_2008, Bass2_2009, Bass3_2008,
                      Bass3_2009, Bass4_2008, Bass4_2009, Bass5_2008, Bass5_2009,
                      Int1_2008, Int1_2009, Int2_2008, Int2_2009, Orb1_2008, Orb1_2009,
                      Orb2_2008, Orb2_2009)
## STOPPED HERE, need help
# changing all non-zero values to 1 (aka presence/absence)
veg_08_09_PA <- lapply(veg_2008_2009, function(x) if 0 else 1))


# taking row sums for all species across all 12 quadrats
lapply(veg_2008_2009, function(x) rowSums(x[, -1]))
head(veg_2008_2009)
