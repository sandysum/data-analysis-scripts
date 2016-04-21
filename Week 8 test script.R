################ START TEST SCRIPT ############################
# ESM 244
# LAB WEEK 8 TEST RUN
# COMPLETE TUESDAY/WEDNESDAY
# ALLISON WILL HAVE EXTRA OFFICE HOURS WEDNESDAY 12 - 1:30pm
############################################

######## 1. INSTALL AND LOAD THE FOLLOWING PACKAGES #########

# The only new one should be maptools...

install.packages("maptools")
install.packages("spatstat")
library(sp)
library(rgdal)
library(gstat)
library(spatstat)
library(maptools)

######## 2. GET DATA ########

# You were emailed spatial data via GauchoSpace. Download simultaneously and save ALL OF THEM into a single folder called 'Lab8Data.' Notice that there are two "sets" of spatial data - one for the california county polygons (layer = "california_county_shape_file") and one for red tree vole locations in the state (layer = "ds033")

# Set the working directory to that folder using the 'Session >> Set Working Directory >> Choose Directory' OR the manual pathway option (see previous labs)

######### 3. GET SPATIAL DATA FROM THE WORKING DIRECTORY ##########

# Recall: If you used the 'Session >> Set Working Directory >> Choose Directory' option, you'll need to just use 'dsn = getwd() in the code below (that's the method I'll use here). If you manually set the pathway, use 'dsn = PathwayName' (the name you assigned for the pathway)

VoleData <- readOGR(dsn = getwd(), layer = "ds033")
CountyData <- readOGR(dsn = getwd(), layer = "california_county_shape_file")

# Make sure both load successfully

######### 4. RUN THIS PART LINE-BY-LINE (we'll cover in class) ####

# You do NOT need to understand this code yet. We'll go through it in lab Thursday. Just make sure it works without error and you get a graph at the end. 

VolesHumboldt <- VoleData[VoleData$COUNTY == "HUM",]
Humboldt <- CountyData[CountyData$NAME == "Humboldt",]
proj4string(Humboldt) <- CRS("+init=epsg:4326")
ProjHumboldt <- spTransform(Humboldt, CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

plot(ProjHumboldt)
points(VolesHumboldt, pch = 20, col = "blue") # Successful plot?

# You should get a plot of Humboldt County outline, with locations of voles plotted as blue points throughout.

########## 5. NOW RUN THIS LINE BY LINE #########

VolePoints <- as(VolesHumboldt, "ppp")
HumboldtBorder <- as(ProjHumboldt, "owin")
VolePPP <- ppp(VolePoints$x, VolePoints$y, window = HumboldtBorder) # Don't panic, warning message is OK

plot(VolePPP) # Successful plot?

# Should get a graph of Humboldt with points plotted (again)

# That's it for now! Thanks for doing the run-through.

############# END TEST SCRIPT ###############