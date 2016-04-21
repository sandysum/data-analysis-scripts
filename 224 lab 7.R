install.packages('rgdal')
install.packages("sp")
install.packages("gstat")

library(rgdal)
library(sp)
library(gstat)

CountyData <- readOGR(dsn = getwd(), layer = "ks_counties_shapefile")

plot(CountyData)

class(CountyData)

bbox(CountyData) # this tells the min and max lat and lon of the spatial data - the bounds!

# e

CountyPoly <- list("sp.lines", as(CountyData, "SpatialLines"), col = "black")

# KRIGING

KSrain <- read.csv("/Users/sandys/Dropbox/UCSB WINTER 2015/ESM 244/Data/KSRain2.csv")

KSrainData <- data.frame(Longitude = KSrain$LON, 
                         Latitude = KSrain$LAT, 
                         Amount = KSrain$AMT)

# the order matters here, so remember to put lon, lat followed by amt

coordinates(KSrainData) <- ~ Longitude + Latitude

class(KSrainData)

quartz()
BubbleRain <- bubble(KSrainData, "Amount", maxsize = 3, xlab = "Longitude", ylab = "Latitude", main = "Kansas Precipitation", col = "blue", alpha = .3)

# Variogram

?variogram

RainVar <- variogram(Amount ~ 1, KSrainData) # function of 1 for ordinary kriging, unknown but constant trend that exists

# run the RainVar to see the different values of the variogram. As dist ^, the gamma ^.

plot(RainVar)

# nugget = .18, range = 1.5, sill = .9 

Fit.Var <- fit.variogram(RainVar, 
                         model = vgm(nugget = .18, psill = .9, range = 1.5, model = "Sph")
                         )

Fit.Var.Gau <- fit.variogram(RainVar, 
                         model = vgm(nugget = .18, psill = .9, range = 1.5, model = "Gau")
)

Fit.Var.Exp <- fit.variogram(RainVar, 
                             model = vgm(nugget = .18, psill = .9, range = 1.5, model = "Exp")
)

# this fit looks good!
quartz()
plot(RainVar, Fit.Var)
# this fit is not very accurate
quartz()
plot(RainVar, Fit.Var.Gau)
quartz()
# even though the exp model looks to fit really well, the exp model never flattens out! the spherical model would fit better, with the slight flattening out
plot(RainVar, Fit.Var.Exp)

attr(Fit.Var, "SSErr") # check for the sum of squared residuals - the lower the better
attr(Fit.Var.Gau, "SSErr")
attr(Fit.Var.Exp, "SSErr")

# using conceptual understanding the "Gau" model fits better as the variance seems to flatter out around 1.8

####################################
# using spherical model for variogram,
# KRIGING
####################################

# use bbox to find the bounds for making the grid

Long <- seq(-102.1, -94.4, length = 100)
Lat <- seq(36.9, 40.1, length = 100)

Grid <- expand.grid(Long, Lat)
colnames(Grid) <- c("Lon", "Lat")

class(Grid) # it is still a data.frame! let R knw it is spatial data

coordinates(Grid) <- ~ Lon + Lat

class(Grid) #[1] "SpatialPoints"

# change it to a pixel 

gridded(Grid) = TRUE

plot(Grid)

RainKrige <- krige(Amount ~ 1, KSrainData, newdata = Grid, model = Fit.Var)

head(RainKrige)

# compare to "BubbleRain" plot and it makes sense, the purple / blue region represents the high rainfall region
quartz()
image(RainKrige, col = rainbow(100), xlim = c(-102.1,-94.4), ylim = c(36.9, 40.1))
contour(RainKrige, levels = seq(.1, by = .4), add = T, col = "black")

# you can play with names, axis and stuff

quartz()

KSrainPlotBorder <- spplot(RainKrige["var1.pred"], 
                           sp.layout = list(CountyPoly), 
                           scales = list(draw = T),
                           xlab = "Longitude",
                           ylab = "Latitude", 
                           col.regions = colorRampPalette(c("blueviolet", "dodgerblue", "skyblue", "white")(20)))


