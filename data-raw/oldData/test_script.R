library(tidyverse)
library(iNEXT)

SH <- read.csv("data-raw/oldData/practiceSHData.csv", header = T)

### clean up datafram
# Make NAs 0
SH[is.na(SH)]<-0
SH[, 4:170] <- sapply(SH[, 4:170], as.numeric)
# transposing the dataframe to long instead of wide
SH.1 <- SH %>%
  mutate(id = str_c(Treatment, Year, Quadrat, sep = "_")) %>%
  set_tidy_names(syntactic = T, quiet = F) %>%
  select(id, Acalypha.virginica.rhomboidea:Helianthus.spp.) %>%
  gather(species, abundance, -id) %>%
  spread(id, abundance)

### making dataframe for iNEXT: only 0 and 1s - prensence absence
x <- SH.1[, 2:433]
species <- SH.1[,1]

x[x > 0.1] <- 1
# x[,] <- sapply(x[,], as.integer) # makes characters into integers
SH.2 <- cbind(species, x)

### have presence/absence info for all 4 years, now splitting up

# function to generate matrix for each watershed in every year
make.matrix <- function(x){
  z <- select(SH.2, species, starts_with(x))
  m <- as.matrix(z[,-1])
  rownames(m) <- z[,1]
  m
}

# this generates a list of all treatment/years
trts <- word(names(SH.2), 1, -2,  sep = "\\_") %>%
  unique() %>%
  na.omit() %>%
  as.character()

# making a large list of matrices for each site
all_trts <- lapply(trts, make.matrix) 
names(all_trts) <- trts

### checking to make sure names assigned correctly, looks good! 
# mInt2_2009 <- make.matrix("Int2_2009") 
# lInt2_2009 <- all_trts[["Int2_2009"]]
# mInt2_2010 <- make.matrix("Int2_2010") 
# lInt2_2010 <- all_trts[["Int2_2010"]]
# identical(mInt2_2009, lInt2_2009) # true
# identical(mInt2_2009, lInt2_2010) # false

# subset by year

mat_2008 <- all_trts[str_subset(trts, "2008$")]
mat_2009 <- all_trts[str_subset(trts, "2009$")]
mat_2010 <- all_trts[str_subset(trts, "2010$")]
mat_2011 <- all_trts[str_subset(trts, "2011$")]

### iNEXT

m2008 <- lapply(mat_2008, as.incfreq)
i2008q0 <- iNEXT(m2008, q=0, datatype = "incidence_freq")
i2008q1 <- iNEXT(m2008, q=1, datatype = "incidence_freq")
i2008q2 <- iNEXT(m2008, q=2, datatype = "incidence_freq")
ggiNEXT(i2008q0)

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
#unique24 <- setdiff(names(SH24), names(SH))
# most of these are unknown species, for the purposes of this test I am going to delete them
#SH24.1 <- SH24[, !names(SH24) %in% unique24]

SH24.2 <- SH24 %>%
  mutate(id = str_c(Treatment, Year, Quadrat, sep = "_")) %>%
  set_tidy_names(syntactic = T, quiet = F) %>%
  select(id, Abutilon.theophrasti:Stickseed) %>%
  gather(species, abundance, -id) %>%
  spread(id, abundance)

x24 <- SH24.2[, 2:145]
species24 <- SH24.2[,1]

x24[x24 > 0.1] <- 1
# x[,] <- sapply(x[,], as.integer) # makes characters into integers
SH24.3 <- cbind(species24, x24)

### transforming into matrix

make.matrix24 <- function(x){
  z <- select(SH24.3, species24, starts_with(x))
  m <- as.matrix(z[,-1])
  rownames(m) <- z[,1]
  m
}

trts24 <- word(names(SH24.3), 1, -2,  sep = "\\_") %>%
  unique() %>%
  na.omit() %>%
  as.character()

# making a large list of matrices for each site
all_trts24 <- lapply(trts24, make.matrix24) 
names(all_trts24) <- trts24

### subset for each year and combine with quadrat data for 12 quadrats

mat_2010_24 <- do.call(c, list(all_trts24[str_subset(trts24, "2010$")], 
                                mat_2010[c(1, 7,8)]))
mat_2011_24 <- do.call(c, list(all_trts24[str_subset(trts24, "2011$")],
                                mat_2011[c(1,7,8)]))
names(mat_2010_24) <- c("Bass1_24", "Int2_24", "Orb1_24", "Bass1", "Int2", "Orb1")
names(mat_2011_24) <- c("Bass1_24", "Int2_24", "Orb1_24", "Bass1", "Int2", "Orb1")


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

#### attempting to analyze

a08 <- ChaoRichness(m2008, datatype = "incidence_freq", conf = 0.95)
a08 <- tibble::rownames_to_column(a08, var = "site")
a08$site <- as.factor(a08$site)
a09 <- ChaoRichness(m2009, datatype = "incidence_freq", conf = 0.95)
a09 <- tibble::rownames_to_column(a09, var = "site")
a09$site <- as.factor(a09$site)
a10 <- ChaoRichness(m2010, datatype = "incidence_freq", conf = 0.95)
a10 <- tibble::rownames_to_column(a10, var = "site")
a10$site <- as.factor(a10$site)
a11 <- ChaoRichness(m2011, datatype = "incidence_freq", conf = 0.95)
a11 <- tibble::rownames_to_column(a11, var = "site")
a11$site <- as.factor(a11$site)

at <- bind_rows(a08, a09, a10, a11)
str(at)

at <- separate(at, col = site, into = c("site", "year"), sep = "\\_", remove = T)

aov.test <- aov(Estimator ~ site, data = at)
summary.aov(aov.test)
aov.year <- aov(Estimator ~ year, data = at)
summary.aov(aov.year)
aov.both <- aov(Estimator ~ site*year, data = at)
summary.aov(aov.both)

treatment <- as_tibble(rep(c(1,2,3,3,2,2,1,1,3),4))
names(treatment) <- "treatment"
treatment
at<- bind_cols(at, treatment)
at$treatment <- as.factor(at$treatment)
str(at)

aov.trt <- aov(Estimator ~ treatment, data = at)
summary.aov(aov.trt)
coefficients(aov.trt)
lsmeans(aov.trt, "treatment")

aov.trtyr <- aov(Estimator ~ treatment*year, data = at)
summary(aov.trtyr)

block <- as_tibble(rep(c("c", "c", "c", "d", "d", "a", "a", "b", "b"), 4))
names(block) <- "block"
at <- bind_cols(at, block)
at$block <- as.factor(at$block)
str(at)

aov.ibd(Estimator ~ site+block, data = at, specs = site)
library(ibd)
data(ibddata)
aov.ibd(Estimator ~ site+block, data = at, specs = site)

# trying to fit curves to estimate a slope parameter
# gather a set of points 

curveTest <- i2011q0[["iNextEst"]][["Orb1_24"]]
plot(qD ~ t, data = curveTest)
plot(log(qD) ~ log(t), data = curveTest)
fit <- lm(formula = log(qD) ~ log(t), data = curveTest)
summary(fit)
curveTest2 <- i2011q0[["iNextEst"]][["Orb1"]]
fit2 <- lm(formula = log(qD) ~ log(t), data = curveTest2)
plot(fit, which = 1)
plot(fit2, which = 1)
fit2$coefficients
fit$coefficients


summary(fit2)plot(fit, which = 1)
coef(fit)


lm(formula = qD ~ t, data = curveTest) %>%
  summary()
# attempting to facet by something else

# mat_2011_24[[1]]
# i2011q %>%
#   mutate(site = fct_recode(i2011q[[1]][[1]],
#            "Bass1" = "Bass",
#            "Bass1_24" = "Bass", 
#            "Int2" ="Int",
#            "Int2_24" = "Int",
#            "Orb1" = "Orb",
#            "Orb1_24" = "Orb"))
# class(i2011q)
#          i2011q[[1]]
# i2011q[[1]][[1]] 
# ggiNEXT(i2011q, type=1, facet.var="site") +
#   facet_wrap(~site)


