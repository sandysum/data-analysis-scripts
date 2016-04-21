library(dplyr)
library(ggplot2)
library(magrittr)

getwd()
ecoli <- read.csv("GoletaEcoli.csv")

ecoli.melt <- stack(ecoli, colnames(ecoli))

ggplot(ecoli.melt, aes(values))

ecoli.melt %>% ggplot(aes(values)) +
  geom_histogram(bins = 20) +
  facet_wrap(~ind)

# plots boxplot.
ecoli.melt %>% ggplot(aes(ind, values)) +
  geom_boxplot() +
  labs(list(x = "Location of Goleta Watershed", 
                y = "E .coli concentration / (MPN/100ml)")) +
  theme(
    axis.text.x = element_text(colour="grey20",size=14),
    axis.text.y = element_text(colour="grey20",size=14),
    axis.title.x = element_text(colour="grey20",size=16),
    axis.title.y = element_text(colour="grey20",size=16)
  )
# function for single imputation with random substitution based on observed data.
a <- ecoli$GSWAT1

random.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample(a.obs, n.missing, replace=TRUE)
  return (imputed)
}

# for loop for imputation iterating over each column.
for (i in 1:4) {
  ecoli2[i] <- random.imp(ecoli[i])
}

# explore data post the imputation.
ecoli2.melt <- stack(ecoli2, colnames(ecoli2))

ecoli2.melt %>% ggplot(aes(values)) +
  geom_histogram(bins = 20) +
  facet_grid(~ind) +
  labs(list(title = "Distribution of E. coli concentration across Goleta watersheds",
            x = "E. coli concentration (MPN/100ml)", 
            y = "Frequency"))

# data is very skewed to the right, therefore data is non parametric. Use kruskal.wallis.
test.ecoli <- kruskal.test(values ~ ind, data = ecoli2.melt)
summary(test.ecoli)

######### PART 2 ###########

state <-  read.csv("StateInfo.csv")
juststate <- juststate[-52,]
StateNames <- as.character(state$State)[1:51]
Variables <- colnames(juststate)


library(corrplot)

statePC <- prcomp(juststate, scale = TRUE)

quartz()
plot(statePC)

quartz()
screeplot(statePC, type = "lines")

# stupid labels got overlaid.
title(main="Scree plot of state variables", 
      xlab="Principal component", ylab="Variance")


quartz()
biplot(statePC, xlabs = StateNames, ylabs = Variables, cex = .5, xlim=c(-0.35, 0.30))

######### PART 3 ##########

library(sp)
library(rgdal)
library(gstat)
library(spatstat)
library(maptools)

setwd("/Users/sandys/Dropbox/UCSB WINTER 2015/ESM 244/Data/Bay")
BayArea <- readOGR(dsn = getwd(), layer = "bay_area_cities")
SF <- BayArea[107,] # Only use spatial data from row 107
CarTheft <- readOGR(dsn = getwd(), layer = "sf_cartheft")

# plotting car theft on the map of san francisco.
quartz()
plot(SF) # This should look like the city (without other areas)
points(CarTheft, pch = 20, col = "navyblue", cex = .8) 
title(main="Distribution of Car Theft in San Francisco")


CarTheftPoints <- as(CarTheft, "ppp")
# R should recognise the border as the window that we are using
SFBorder <- as(SF, "owin")

class(CarTheftPoints)
class(SFBorder)

CarPPP <- ppp(CarTheftPoints$x, CarTheftPoints$y, window = SFBorder)
class(CarPPP)
quartz()
plot(CarPPP)

######### Quardrat Test ########

CarQT <- quadrat.test(CarPPP, nx = 5, ny = 5)
quartz()
plot(CarPPP)
plot(CarQT, add = T, cex = .5)