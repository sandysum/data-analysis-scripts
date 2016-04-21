library(forecast)
library(ggplot2)
library(rgdal)
library(maptools)
library(plyr)
library(sp)

setwd("/Users/sandys/Dropbox/UCSB WINTER 2015/ESM 244/data")

####################
# Part 1. Time Series Analysis ARIMA
####################

euse <- read.csv("EnergyUse.csv")
ResTS <- ts(euse$Residential,
            frequency = 12, 
            start = c(1973, 1))

IndTS <- ts(euse$Industrial,
            frequency = 12,
            start = c(1973, 1))

# plots 2 in a row
par(mfrow = c(2,1))
plot(ResTS, type = "l")
plot(IndTS, type = "l")

DecompRes <- decompose(ResTS)
quartz()
plot(DecompRes)

DecompInd <- decompose(IndTS) #!! This is the decompose function I was talking about
quartz()
plot(DecompInd)

quartz()
plot(DecompRes$figure)

Resstationary <- DecompRes$x - DecompRes$trend
quartz()
# why is there a weird peak in the middle of the season? Perhaps people are getting warm in summer and using a lot of AC ?

plot(Resstationary)

ResACF <- acf(Resstationary[7:499])
quartz()
plot(ResACF)

ResOpt <- auto.arima(ResTS)
IndOpt <- auto.arima(IndTS, 
                     seasonal = FALSE) # IF DECOMPOSE TREND PLOT SHOWS NO SEASONALITY.

# USE OUTPUT OF ABOVE auto.arima() FUNCTION TO SET PARAMETERS FOR ARIMA MODEL.
# Residential: ARIMA(1,0,1)(0,1,1) 
# Industrial: ARIMA(4,1,0)(2,0,0) 
# these are our model parameters

ResARIMA <- arima(ResTS, 
                  order = c(1,0,1), 
                  seasonal = list(order = c(0,1,1))
                  ) # do you want the seasonal component?

IndARIMA <- arima(IndTS,
                  order = c(5,1,2))

# is it a good plot? Look at the residuals!

hist(ResARIMA$residuals)
qqnorm(ResARIMA$residuals)

hist(IndARIMA$residuals)
qqnorm(IndARIMA$residuals)

#h forecast 

ForecastRes <- forecast(ResARIMA, h = 72) # h denotes how many more months you want to look ahead - in terms of the same unit you specified at the start
ForecastInd <- forecast(IndARIMA, h = 72) 

quartz()
plot(ForecastRes) # this is what ARIMA does, it looks and forecast the future values placing greater # weight on the later and more recent values. The values are much closer to those of later years.
plot(ForecastInd)

####################
# Part 2. Spatial Data! 
####################

QuakeData <- data.frame(latitude = quakes$lat, 
                        longitude = quakes$lon,
                        magnitude = quakes$mag)

# tell R that it is a spatial object use the SP package

coordinates(QuakeData) <- ~ latitude + longitude # assigned coordinates to our model

proj4string(QuakeData) <- CRS("+init=epsg:3143") # this epsg number is the standard system for 
# projection data - as # the earth is not flat, so we have to re-project the data?? 
# READ UP ON THIS!

quartz()
spplot(QuakeData, xlab = "Longitude", ylab = "Latitude")

####################
# Loading in data from california!

# Making maps of California with attributes

CountyData <- readOGR(dsn = getwd(), layer = "california_county_shape_file")
proj4string(CountyData) <- CRS("+init=epsg:3310")

quartz()
plot(CountyData)
plot(CountyData[CountyData$AREA <.3,], 
     add = T, 
     col = "yellow2")
plot(CountyData[CountyData$AREA >= .3 & CountyData$AREA < .8,], 
     add = T, 
     col = "springgreen")
plot(CountyData[CountyData$AREA >= .8 & CountyData$AREA < 1.4,], 
     add = T, 
     col = "steelblue")
plot(CountyData[CountyData$AREA >= 1.4,], 
     add = T, 
     col = "slateblue")

CAIncome <- read.csv("/Users/sandys/Dropbox/UCSB WINTER 2015/ESM 244/Data/CAIncome.csv")

CountyData$NAME %in% CAIncome$County
# this matches the county names

CountyData$NAME[!CountyData$NAME %in% CAIncome$County]

CAIncome <- rename(CAIncome, replace = c(County = "NAME"))

CountyData@data <- join(CountyData@data, CAIncome)
View(CountyData@data)

CountyFortify <- fortify(CountyData, region = "NAME")

CountyFortify <- merge(CountyFortify, CountyData@data, by.x = "id", by.y = "NAME")

quartz()
map <- 
