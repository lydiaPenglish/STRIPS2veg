library(tidyverse)
library(iNEXT)

SH <- read.csv("oldData/practiceSHData.csv", header = T)
str(SH)

### clean up datafram
# Make NAs 0
SH[is.na(SH)]<-0
# getting rid of species with 0 observations
SH.1 <- SH[, colSums(SH != 0) > 0]
# transposing the dataframe to long instead of wide
SH.2 <- SH.1 %>%
  mutate(id = str_c(Treatment, Year, Quadrat, sep = "_")) %>%
  set_tidy_names(syntactic = T, quiet = F) %>%
  select(id, Acalypha.virginica.rhomboidea:Helianthus.spp.) %>%
  gather(species, abundance, -id) %>%
  spread(id, abundance)
#### making dataframe for iNEXT: only 0 and 1s - prensence absence
x <- SH.2[, 2:217]
species <- SH.2[,1]

x[x > 0.1] <- 1
SH.3 <- cbind(species, x)
str(SH.3)

### splitting data frame into list
Bass1_2008 <- select(SH.3, species, starts_with("Bass1_2008"))
# str_sub("Bass1_2008_1", 12)
# str_replace(names(Bass1_2008), "Bass1_2008_\\d", str_sub(names(Bass1_2008), -2, -1))
# str_sub(names(Bass1_2008), "//d$")
# names(Bass1_2008)
matrix.please<-function(x) {
  m<-as.matrix(x[,-1])
  rownames(m)<-x[,1]
  m
}
mBass1_2008 <- matrix.please(Bass1_2008)
Bass1_2009 <- select(SH.3, species, starts_with("Bass1_2009"))
mBass1_2009 <- matrix.please(Bass1_2009)
Bass2_2008 <- select(SH.3, species, starts_with("Bass2_2008"))
mBass2_2008 <- matrix.please(Bass2_2008)
Bass2_2009 <- select(SH.3, species, starts_with("Bass2_2009"))
mBass2_2009 <- matrix.please(Bass2_2009)
Bass3_2008 <- select(SH.3, species, starts_with("Bass3_2008"))
mBass3_2008 <- matrix.please(Bass3_2008)
Bass3_2009 <- select(SH.3, species, starts_with("Bass3_2009"))
mBass3_2009 <- matrix.please(Bass3_2009)
Bass4_2008 <- select(SH.3, species, starts_with("Bass4_2008"))
mBass4_2008 <- matrix.please(Bass4_2008)
Bass4_2009 <- select(SH.3, species, starts_with("Bass4_2009"))
mBass4_2009 <- matrix.please(Bass4_2009)
Bass5_2008 <- select(SH.3, species, starts_with("Bass5_2008"))
mBass5_2008 <- matrix.please(Bass5_2008)
Bass5_2009 <- select(SH.3, species, starts_with("Bass5_2009"))
mBass5_2009 <- matrix.please(Bass5_2009)
Int1_2008 <- select(SH.3, species, starts_with("Int1_2008"))
mInt1_2008 <- matrix.please(Int1_2008)
Int1_2009 <- select(SH.3, species, starts_with("Int1_2009"))
mInt1_2009 <- matrix.please(Int1_2009)
Int2_2008 <- select(SH.3, species, starts_with("Int2_2008"))
mInt2_2008 <- matrix.please(Int2_2008)
Int2_2009 <- select(SH.3, species, starts_with("Int2_2009"))
mInt2_2009  <- matrix.please(Int2_2009 )
Orb1_2008 <- select(SH.3, species, starts_with("Orb1_2008"))
mOrb1_2008 <- matrix.please(Orb1_2008)
Orb1_2009 <- select(SH.3, species, starts_with("Orb1_2009"))
mOrb1_2009 <- matrix.please(Orb1_2009)
Orb2_2008 <- select(SH.3, species, starts_with("Orb2_2008"))
mOrb2_2008<- matrix.please(Orb2_2008)
Orb2_2009 <- select(SH.3, species, starts_with("Orb2_2009"))
mOrb2_2009 <- matrix.please(Orb2_2009)
mveg_2008_2009 <- list(mBass1_2008, mBass1_2009, mBass2_2008, mBass2_2009, mBass3_2008,
                      mBass3_2009, mBass4_2008, mBass4_2009, mBass5_2008, mBass5_2009,
                      mInt1_2008, mInt1_2009, mInt2_2008, mInt2_2009, mOrb1_2008, mOrb1_2009,
                      mOrb2_2008, mOrb2_2009)
names(mveg_2008_2009) <- c("Bass1_2008", "Bass1_2009", "Bass2_2008", "Bass2_2009", "Bass3_2008",
                           "Bass3_2009", "Bass4_2008", "Bass4_2009", "Bass5_2008", "Bass5_2009",
                           "Int1_2008", "Int1_2009", "Int2_2008", "Int2_2009", "Orb1_2008", "Orb1_2009",
                           "Orb2_2008", "Orb2_2009")
lessM <- list(mBass1_2008, mBass2_2008, mBass3_2008)
names(lessM) <- c("Bass1_2008", "Bass2_2008", "Bass3_2008")
names(lessM)
# This didn't work!
# changing all non-zero values to 1 (aka presence/absence), made new list
# veg_08_09_PA <- lapply(veg_2008_2009[1:18][2:13], function(x) replace (x, x > 0.1, 1))

# taking row sums for all species across all 12 quadrats

# veg_08_09 <- lapply(veg_2008_2009, function(x) {
#   mutate(x,
#          row_sum = rowSums(x[, -1]))
# })
# veg_08_09[[1]]

### iNEXT
iNEX <- lapply(lessM, as.incfreq)
allSites <- lapply(mveg_2008_2009, as.incfreq)
test1 <- as.incfreq(mOrb1_2008) ## works with one but doesn't work as list
all <- iNEXT(allSites, q=0, datatype="incidence_freq")
outC <- iNEXT(cil, q=0, datatype="incidence_freq")
ggiNEXT(all)
