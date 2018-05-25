library(tidyverse)
library(iNEXT)

SH <- read.csv("data-raw/oldData/practiceSHData.csv", header = T)
str(SH)

### clean up datafram
# Make NAs 0
SH[is.na(SH)]<-0
SH[, 4:170] <- sapply(SH[, 4:170], as.numeric)
str(SH)
# getting rid of species with 0 observations
# SH.1 <- SH[, colSums(SH != 0) > 0] # no longer any dummy species
# transposing the dataframe to long instead of wide
SH.1 <- SH %>%
  mutate(id = str_c(Treatment, Year, Quadrat, sep = "_")) %>%
  set_tidy_names(syntactic = T, quiet = F) %>%
  select(id, Acalypha.virginica.rhomboidea:Helianthus.spp.) %>%
  gather(species, abundance, -id) %>%
  spread(id, abundance)
str(SH.1)

#### making dataframe for iNEXT: only 0 and 1s - prensence absence
x <- SH.1[, 2:433]
str(x)
species <- SH.1[,1]

x[x > 0.1] <- 1
str(x)
# x[,] <- sapply(x[,], as.integer) # makes characters into integers
SH.2 <- cbind(species, x)
str(SH.2)

### have presence/absence info for all 4 years, now splitting up

# function to turn dataframes into matrices
matrix.please<-function(x) {
  m<-as.matrix(x[,-1])
  rownames(m)<-x[,1]
  m
}

## 2008

Bass1_2008 <- select(SH.2, species, starts_with("Bass1_2008"))
mBass1_2008 <- matrix.please(Bass1_2008)
Bass2_2008 <- select(SH.2, species, starts_with("Bass2_2008"))
mBass2_2008 <- matrix.please(Bass2_2008)
Bass3_2008 <- select(SH.2, species, starts_with("Bass3_2008"))
mBass3_2008 <- matrix.please(Bass3_2008)
Bass4_2008 <- select(SH.2, species, starts_with("Bass4_2008"))
mBass4_2008 <- matrix.please(Bass4_2008)
Bass5_2008 <- select(SH.2, species, starts_with("Bass5_2008"))
mBass5_2008 <- matrix.please(Bass5_2008)
Int1_2008 <- select(SH.2, species, starts_with("Int1_2008"))
mInt1_2008 <- matrix.please(Int1_2008)
Int2_2008 <- select(SH.2, species, starts_with("Int2_2008"))
mInt2_2008 <- matrix.please(Int2_2008)
Orb1_2008 <- select(SH.2, species, starts_with("Orb1_2008"))
mOrb1_2008 <- matrix.please(Orb1_2008)
Orb2_2008 <- select(SH.2, species, starts_with("Orb2_2008"))
mOrb2_2008<- matrix.please(Orb2_2008)

mat_2008 <- list(mBass1_2008, mBass2_2008, mBass3_2008, mBass4_2008, mBass5_2008,
                       mInt1_2008, mInt2_2008, mOrb1_2008, mOrb2_2008)
names(mat_2008) <- c("Bass1_2008", "Bass2_2008", "Bass3_2008", "Bass4_2008", "Bass5_2008",
                           "Int1_2008", "Int2_2008",  "Orb1_2008", "Orb2_2008")

mat_2008[[2]]

## 2009

Bass1_2009 <- select(SH.2, species, starts_with("Bass1_2009"))
mBass1_2009 <- matrix.please(Bass1_2009)
Bass2_2009 <- select(SH.2, species, starts_with("Bass2_2009"))
mBass2_2009 <- matrix.please(Bass2_2009)
Bass3_2009 <- select(SH.2, species, starts_with("Bass3_2009"))
mBass3_2009 <- matrix.please(Bass3_2009)
Bass4_2009 <- select(SH.2, species, starts_with("Bass4_2009"))
mBass4_2009 <- matrix.please(Bass4_2009)
Bass5_2009 <- select(SH.2, species, starts_with("Bass5_2009"))
mBass5_2009 <- matrix.please(Bass5_2009)
Int1_2009 <- select(SH.2, species, starts_with("Int1_2009"))
mInt1_2009 <- matrix.please(Int1_2009)
Int2_2009 <- select(SH.2, species, starts_with("Int2_2009"))
mInt2_2009  <- matrix.please(Int2_2009 )
Orb1_2009 <- select(SH.2, species, starts_with("Orb1_2009"))
mOrb1_2009 <- matrix.please(Orb1_2009)
Orb2_2009 <- select(SH.2, species, starts_with("Orb2_2009"))
mOrb2_2009 <- matrix.please(Orb2_2009)

mat_2009 <- list(mBass1_2009, mBass2_2009, mBass3_2009, mBass4_2009, mBass5_2009,
                 mInt1_2009, mInt2_2009, mOrb1_2009, mOrb2_2009)
names(mat_2009) <- c("Bass1_2009", "Bass2_2009", "Bass3_2009", "Bass4_2009", "Bass5_2009",
                     "Int1_2009", "Int2_2009",  "Orb1_2009", "Orb2_2009")

## 2010

Bass1_2010 <- select(SH.2, species, starts_with("Bass1_2010"))
mBass1_2010 <- matrix.please(Bass1_2010)
Bass2_2010 <- select(SH.2, species, starts_with("Bass2_2010"))
mBass2_2010 <- matrix.please(Bass2_2010)
Bass3_2010 <- select(SH.2, species, starts_with("Bass3_2010"))
mBass3_2010 <- matrix.please(Bass3_2010)
Bass4_2010 <- select(SH.2, species, starts_with("Bass4_2010"))
mBass4_2010 <- matrix.please(Bass4_2010)
Bass5_2010 <- select(SH.2, species, starts_with("Bass5_2010"))
mBass5_2010 <- matrix.please(Bass5_2010)
Int1_2010 <- select(SH.2, species, starts_with("Int1_2010"))
mInt1_2010 <- matrix.please(Int1_2010)
Int2_2010 <- select(SH.2, species, starts_with("Int2_2010"))
mInt2_2010  <- matrix.please(Int2_2010 )
Orb1_2010 <- select(SH.2, species, starts_with("Orb1_2010"))
mOrb1_2010 <- matrix.please(Orb1_2010)
Orb2_2010 <- select(SH.2, species, starts_with("Orb2_2010"))
mOrb2_2010 <- matrix.please(Orb2_2010)

mat_2010 <- list(mBass1_2010, mBass2_2010, mBass3_2010, mBass4_2010, mBass5_2010,
                 mInt1_2010, mInt2_2010, mOrb1_2010, mOrb2_2010)
names(mat_2010) <- c("Bass1_2010", "Bass2_2010", "Bass3_2010", "Bass4_2010", "Bass5_2010",
                     "Int1_2010", "Int2_2010",  "Orb1_2010", "Orb2_2010")

## 2011

Bass1_2011 <- select(SH.2, species, starts_with("Bass1_2011"))
mBass1_2011 <- matrix.please(Bass1_2011)
Bass2_2011 <- select(SH.2, species, starts_with("Bass2_2011"))
mBass2_2011 <- matrix.please(Bass2_2011)
Bass3_2011 <- select(SH.2, species, starts_with("Bass3_2011"))
mBass3_2011 <- matrix.please(Bass3_2011)
Bass4_2011 <- select(SH.2, species, starts_with("Bass4_2011"))
mBass4_2011 <- matrix.please(Bass4_2011)
Bass5_2011 <- select(SH.2, species, starts_with("Bass5_2011"))
mBass5_2011 <- matrix.please(Bass5_2011)
Int1_2011 <- select(SH.2, species, starts_with("Int1_2011"))
mInt1_2011 <- matrix.please(Int1_2011)
Int2_2011 <- select(SH.2, species, starts_with("Int2_2011"))
mInt2_2011  <- matrix.please(Int2_2011 )
Orb1_2011 <- select(SH.2, species, starts_with("Orb1_2011"))
mOrb1_2011 <- matrix.please(Orb1_2011)
Orb2_2011 <- select(SH.2, species, starts_with("Orb2_2011"))
mOrb2_2011 <- matrix.please(Orb2_2011)

mat_2011 <- list(mBass1_2011, mBass2_2011, mBass3_2011, mBass4_2011, mBass5_2011,
                 mInt1_2011, mInt2_2011, mOrb1_2011, mOrb2_2011)
names(mat_2011) <- c("Bass1_2011", "Bass2_2011", "Bass3_2011", "Bass4_2011", "Bass5_2011",
                     "Int1_2011", "Int2_2011",  "Orb1_2011", "Orb2_2011")

### iNEXT

m2008 <- lapply(mat_2008, as.incfreq)
i2008q0 <- iNEXT(m2008, q=0, datatype = "incidence_freq")
i2008q1 <- iNEXT(m2008, q=1, datatype = "incidence_freq")
i2008q2 <- iNEXT(m2008, q=2, datatype = "incidence_freq")
ggiNEXT(i2008q2)

m2009 <- lapply(mat_2009, as.incfreq)
i2009q0 <- iNEXT(m2009, q=0, datatype = "incidence_freq")
i2009q1 <- iNEXT(m2009, q=1, datatype = "incidence_freq")
i2009q2 <- iNEXT(m2009, q=2, datatype = "incidence_freq")
ggiNEXT(i2009q0)

m2010 <- lapply(mat_2010, as.incfreq)
i2010q0 <- iNEXT(m2010, q=0, datatype = "incidence_freq")
i2010q1 <- iNEXT(m2010, q=1, datatype = "incidence_freq")
i2010q2 <- iNEXT(m2010, q=2, datatype = "incidence_freq")
ggiNEXT(i2010q0)

m2011 <- lapply(mat_2011, as.incfreq)
i2011q0 <- iNEXT(m2011, q=0, datatype = "incidence_freq")
i2011q1 <- iNEXT(m2011, q=1, datatype = "incidence_freq")
i2011q2 <- iNEXT(m2011, q=2, datatype = "incidence_freq")
ggiNEXT(i2011q0)

### testing out other package functions
ChaoRichness(m2011, datatype = "incidence_freq", conf = 0.95)
ChaoShannon(m2011, datatype = "incidence_freq", transform = T, conf= 0.95)
ChaoSimpson(m2011, datatype="incidence_freq", transform = T, conf = 0.95, B = 200)
DataInfo(m2011, datatype="incidence_freq")
estimateD(m2011, datatype = "incidence_freq", base = "size", level = NULL, conf = 0.95)

### adding data for 24 quadrats vs 12 ####

SH24 <- read.csv("data-raw/oldData/practiceSHData_24.csv", header = T)
SH24[is.na(SH24)]<-0
SH24[, 4:194] <- sapply(SH24[, 4:194], as.numeric)
str(SH24)
unique24 <- setdiff(names(SH24), names(SH))
# most of these are unknown species, for the purposes of this test I am going to delete them
SH24.1 <- SH24[, !names(SH24) %in% unique24]

SH24.2 <- SH24 %>%
  mutate(id = str_c(Treatment, Year, Quadrat, sep = "_")) %>%
  set_tidy_names(syntactic = T, quiet = F) %>%
  select(id, Abutilon.theophrasti:Stickseed) %>%
  gather(species, abundance, -id) %>%
  spread(id, abundance)
str(SH24.2)

x24 <- SH24.2[, 2:144]
str(x24)
species24 <- SH24.2[,1]

x24[x24 > 0.1] <- 1
str(x24)
# x[,] <- sapply(x[,], as.integer) # makes characters into integers
SH24.3 <- cbind(species24, x24)
str(SH24.3)

### getting each site/year into a matrix
Bass1_2010_24 <- select(SH24.3, species24, starts_with("Bass1_2010"))
mBass1_2010_24 <- matrix.please(Bass1_2010_24)
Bass1_2011_24 <- select(SH24.3, species24, starts_with("Bass1_2011"))
mBass1_2011_24 <- matrix.please(Bass1_2011_24)
Orb1_2010_24 <- select(SH24.3, species24, starts_with("Orb1_2010"))
mOrb1_2010_24 <- matrix.please(Orb1_2010_24)
Orb1_2011_24 <- select(SH24.3, species24, starts_with("Orb1_2011"))
mOrb1_2011_24 <- matrix.please(Orb1_2011_24)
Int2_2010_24 <- select(SH24.3, species24, starts_with("Int2_2010"))
mInt2_2010_24 <- matrix.please(Int2_2010_24)
Int2_2011_24 <- select(SH24.3, species24, starts_with("Int2_2011"))
mInt2_2011_24 <- matrix.please(Int2_2011_24)

# 2010

mat_2010_24 <- list(mBass1_2010, mBass1_2010_24, mOrb1_2010, mOrb1_2010_24, mInt2_2010,
                 mInt2_2010_24)
names(mat_2010_24) <- c("Bass1", "Bass1_24", "Orb1", "Orb1_24", "Int2",
                     "Int2_24")

# 2011

mat_2011_24 <- list(mBass1_2011, mBass1_2011_24, mOrb1_2011, mOrb1_2011_24, mInt2_2011,
                    mInt2_2011_24)
names(mat_2011_24) <-  c("Bass1", "Bass1_24", "Orb1", "Orb1_24", "Int2",
                         "Int2_24")

m2010_24 <- lapply(mat_2010_24, as.incfreq)
i2010q0 <- iNEXT(m2010_24, q=0, datatype = "incidence_freq")
ggiNEXT(i2010q0)+
  scale_shape_manual(values = c(19,19,19,19,19,19))+ 
  ggtitle("2010 iNEXT curves for species richness q=0")+
  theme(plot.title = element_text(hjust = 0.5))

m2011_24 <- lapply(mat_2011_24, as.incfreq)
i2011q0 <- iNEXT(m2011_24, q=0, datatype = "incidence_freq")
i2011q <- iNEXT(m2011_24, q=c(0,1,2), datatype = "incidence_freq")
ggiNEXT(i2011q0)+
  scale_shape_manual(values = c(19,19,19,19,19,19))+
  ggtitle("2011 iNEXT curves for species richness q=0")+
  theme(plot.title = element_text(hjust = 0.5))

ChaoRichness(m2010_24, datatype = "incidence_freq", conf = 0.95)
ChaoRichness(m2011_24, datatype = "incidence_freq", conf = 0.95)
ChaoShannon(m2010_24, datatype = "incidence_freq", transform = T, conf = 0.95)
ChaoShannon(m2011_24, datatype = "incidence_freq", transform = T, conf = 0.95)
estimateD(m2010_24, datatype="incidence_freq")



# attempting to facet by something else

mat_2011_24[[1]]

i2011q %>%
  mutate(site = fct_recode(i2011q[[1]][[1]],
           "Bass1" = "Bass",
           "Bass1_24" = "Bass", 
           "Int2" ="Int",
           "Int2_24" = "Int",
           "Orb1" = "Orb",
           "Orb1_24" = "Orb"))

class(i2011q)

         i2011q[[1]]

i2011q[[1]][[1]] 

ggiNEXT(i2011q, type=1, facet.var="site") +
  facet_wrap(~site)

i2011q0[[2]][[1]]
print.iNEXT(i2011q0)
DataInfo(mat_2011, datatype = "incidence_freq")
