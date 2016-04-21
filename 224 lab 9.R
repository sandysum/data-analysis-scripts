install.packages()
pac <- c("vegan","rattle","NbClust", "cluster", "qgraph", "reshape", "corrplot") 
#this for loop does not work
# please do not be lazy and install.packages() and library() all of them manually....

library(reshape)
library(rattle)
for (i in 1:length(pac)) {
  install.packages(pac[i])
  library(pac[i], character.only=TRUE)
}

setwd("~/Dropbox/UCSB WINTER 2015/ESM 244/Data")

WB <- read.csv("WBDevelopment.csv")
colnames(WB) <-c("Country", "Variable", "Code", "Value")
WB$code <- NULL
unique(WB$var)

WBdatapivot <- cast(WB, Country~Variable) 

NewData <- WBdatapivot[c(1,2,18,28,30,35,38,40,46,61)]

colnames(NewData) <- c("Country", "AdFert", "GDPGrowth", "Immunization", "Sanitation", "Internet", "Military", "Mortality", "PopDensity", "UrbanPop")

CountryName <- as.character(NewData$Country)

JustData <- NewData[-1]
FinalData <- data.frame(JustData)

quartz()
corrplot(cor(FinalData), method = "ellipse") # SO COOL! Visualise strength of correlation. 
# It is practically a visual var-cov matrix

quartz()
corrplot(cor(FinalData), method = "square", type = "upper")


# j principal component

WBPC <- prcomp(FinalData, scale = T)

summary(WBPC)

# shows how much the variance is explained with each PC.
quartz()
plot(WBPC)

quartz()
screeplot(WBPC, type = "lines")

# l. using the biplot function

Variables <- c("AdFert", "GDPGrowth", "Immunization", "Sanitation", "Internet", "Military", "Mortality", "PopDensity", "UrbanPop")

quartz()
biplot(WBPC, xlabs = CountryName, ylabs = Variables, cex = .6)

# it looks like JP, DEU, CHE, GBR have greater internet use while MEX, BRA, ARG seems to be associated with higher AdFertility rate.

quartz()
qgraph(cor(FinalData))

quartz()
qgraph(cor(FinalData), layout = "spring", posCol = "green", negCol = "red")

###### Part 2. 

data(dune)
data(dune.env)

duneRDA <- rda(dune ~ A1 + Management, data = dune.env)

quartz()
plot(duneRDA)
plot(envfit(duneRDA ~.,dune), add = TRUE, cex = .8, col = "red")


######## Part 3. 
?wine

Winedata <- scale(wine[-1])

ClusterNumber <- NbClust(Winedata, min.nc = 2, max.nc = 10, method = "kmeans")

WineKMeans <- kmeans(Winedata, 3)

quartz()
clusplot(Winedata, WineKMeans$cluster, color = TRUE, shade = TRUE, lines = 0)

# can you plot by type - use the type of wine to represents the point, so we can see if certain type
# of wine belong in some clusters or the other!