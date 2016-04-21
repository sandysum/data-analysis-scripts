################ START TEST SCRIPT ############################
# ESM 244
# LAB WEEK 8 
############################################

library(sp)
library(rgdal)
library(gstat)
library(spatstat)
library(maptools)

VoleData <- readOGR(dsn = getwd(), layer = "ds033")
CountyData <- readOGR(dsn = getwd(), layer = "california_county_shape_file")
VolesHumboldt <- VoleData[VoleData$COUNTY == "HUM",]
Humboldt <- CountyData[CountyData$NAME == "Humboldt",]

(proj4string(Humboldt) <- CRS("+init=epsg:4326")

VoleData@proj4string #copy and paste infomation from here and insert into CSR.

ProjHumboldt <- spTransform(Humboldt, CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

quartz()
plot(ProjHumboldt)
points(VolesHumboldt, pch = 20, col = "coral")

VolePoints <- as(VolesHumboldt, "ppp")
# R should recognise the border as the window that we are using
HumboldtBorder <- as(ProjHumboldt, "owin")
class(HumboldtBorder)
class(VolePoints)

VolePPP <- ppp(VolePoints$x, VolePoints$y, window = HumboldtBorder)
quartz()
plot(VolePPP)
# Now it is stored as a single piece of information with the points and border. 

######## QUADRAT TEST #############

VoleQT <- quadrat.test(VolePPP, nx = 5, ny = 10)

# p-value < 0.00000000000000022 Definitely reject the null hypothesis of CSR. There is an uneven distribution of voles...

quartz()
plot(VolePPP)
plot(VoleQT, add = T, cex = .5) #cex changes the font size. The number in the top right hand side in the
# cell is the expected number of points you'd expect in that cell if this data follow true CSR. 
# The number in the left side is the actual number of events in the cell and the bottom number is the standardised residual. If it is positive the actual count is more than what was expected, and if it is negative, it is LESS than what was expected.

########## KERNEL DENSITY #############

quartz()
plot(density(VolePPP, sigma = 10000)) # Shows how many hot spots there are. Experiment with sigma = 3000, 10000, 1000)

########## NEAREST NEIGHBOUR G-FUNCTION #############

r <- seq(0,5000, by = 500) # seq of breaks where we can calculate how far apart we want to calculate the G # -function for

Gfunc <- envelope(VolePPP, fun = Gest, r = r, nsim = 100, nrank = 2) # nrank means that the G-function is picking the second (not the max or min data points), we skip the outliers.
# Significance level of pointwise Monte Carlo test: 4/101 = 0.0396 (the more simulations you run, the more significant your MC test will be)

plot(Gfunc$obs ~ Gfunc$r, type = "l", col = "red", lty = 11)
lines(Gfunc$theo ~ Gfunc$r, type = "l", col = "green", lty = 6)
# My red line representing the observed g-function shows that the actual data is more clustered!
lines(Gfunc$lo ~ Gfunc$r, type = "l", col = "gray", lty = 8)
lines(Gfunc$hi ~ Gfunc$r, type = "l", col = "black", lty = 4)
legend(500, .8, c("Obs", "High", "CSR", "Low"), lty = c(11, 4, 6, 8), col = c("red", "black", "green", "gray")) 

########## K/L Function #############

r2 <- seq(0, 30000, by = 3000)
Lfunc <- envelope(VolePPP, fun = Lest, r = r2, nsim = 100, rank = 2, global = TRUE)
plot(Lfunc$obs ~ Lfunc$r, type = "l", col = "red", lty = 11)
lines(Lfunc$theo ~ Lfunc$r, type = "l", col = "green", lty = 6)
# My red line representing the observed g-function shows that the actual data is more clustered!
lines(Lfunc$lo ~ Lfunc$r, type = "l", col = "gray", lty = 8)
lines(Lfunc$hi ~ Lfunc$r, type = "l", col = "black", lty = 4)
legend(500, 30000, c("Obs", "High", "CSR", "Low"), lty = c(11, 4, 6, 8), col = c("red", "black", "green", "gray")) 

############ DCLF TEst for CSR ############

DCLFtest <- dclf.test(VolePPP, nsim = 100, rank = 2)
# p-value = 0.009901. Very significant results! 
  