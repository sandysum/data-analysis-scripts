#################################
# ESM 244 Lab Week 7 Prep
# Operation R Error Preemptive Strike
#################################

library(rgdal)
library(sp)
library(gstat)

#################################
# Preemptive Strike Part 1. Install and load packages
#################################

# Necessary packages: rgdal, sp, gstat
# Make sure that above packages install load successfully.
# Run the following to make sure they load without issue once they're installed: 

library(rgdal)
library(sp)
library(gstat)

#################################
# Preemptive Strike Part 2. Get spatial data for Kansas Counties
#################################

# Save the Kansas counties spatial data from the email you were sent. Download the four files all at once (zipped), and save them in a folder on your computer called 'KSCounties'

# MAKE SURE THAT YOU CAN SUCCESSFULLY GET THE SHAPEFILE DATA. 

######### OPTION 1 TO GET SPATIAL DATA #########

# Go up to 'Session' in the R menu bar. Choose 'Set Working Directory' >> 'Choose Directory', then maneuver to the KSCounties folder you've created containing the spatial files. That sets the working directory. THEN, use this code to access the spatial data (calling it CountyData): 

CountyData <- readOGR(dsn = getwd(), layer = "ks_counties_shapefile")

# IF THAT WORKED, information about the spatial data should come up in the console window. IF THAT WORKED, you don't need to do the next option too (unless you want to)

########## OPTION 2 TO GET SPATIAL DATA ########### 

# Find the pathway to the KSCounties file, and assign that pathway to a name. For example, my code looks like this (but your pathway would be different): 

KSDir <- "/Users/allisonhorst/Documents/Bren Courses/244/244 Spring 2015/ESM 244 Labs/Lab Week 8/KansasRain/KSCounties/"

# Set the working directory to that pathway (example code below)

setwd(KSDir)

# Then load the shapefile data using readOGR, like this: 

CountyData <- readOGR(dsn = KSDir, layer = "ks_counties_shapefile")

# Again, if the data loads correctly then the spatial information summary should appear in the console window as above. 

# If you still cannot load the spatial data for the Kansas Counties, please come see us before lab (additional office hours this week listed on GauchoSpace). 

# A good check to see if the spatial data loaded: 

plot(CountyData) # Should be Kansas counties map

###############################################
# Preemptive Strike Part 3. Test a few functions for spatial plotting
###############################################

# Run the following five lines to create a  fabricated data frame for pixie dust concentrations in LaLa Land: 

Concentration <- c(130, 240, 172, 90)
Latitude <- c(38.71, 38.39, 39.15, 39.64)
Longitude <- c(94.16, 93.22, 94.05, 94.51)
DustData <- data.frame(Concentration, Latitude, Longitude)
coordinates(DustData) <- ~ Longitude + Latitude

# Just test the following two lines by running separately (making sure you get a weird graphic for each, without error messages):

plot(DustData) # Four plus signs 
bubble(DustData, "Concentration") # Four circles of differing size

# For the following, make sure the code runs successfully (no error), then use plot(UnicornVariogram) to make sure you can visualize (you are NOT expected to understand what you're doing...we're going over variograms in class this week) 

UnicornVariogram <- variogram(Concentration ~ 1, DustData)
plot(UnicornVariogram) # Don't worry, there should only be one point...just make sure the plot window shows up successfully with no error

######## JUST RUN THE FOLLOWING CODE TO MAKE SURE YOU DON'T GET ERRORS. A GRAPHIC SHOULD SHOW UP WHEN YOU RUN THE LAST LINE. YOU DO NOT NEED TO UNDERSTAND THE CODE - WE'RE GOING TO GO OVER IT IN LAB THURSDAY. **NOTE: this should run successfully if you run it all at once. IF your R session aborts when you do that, just run each line sequentially and it shouldn't be an issue. ########

Test <- fit.variogram(UnicornVariogram, model = vgm(nugget = 0, psill = 700, model = "Exp", range = 0.5))
Long <- seq(93,95,length = 50)
Lat <- seq(38,40,length = 50)
Grid <- expand.grid(Long, Lat)
colnames(Grid) <- c("Long","Lat")
coordinates(Grid) <- ~ Long + Lat
gridded(Grid) = TRUE
Dust <- krige(Concentration ~ 1, DustData, newdata = Grid, model = Test)
image(Dust)

######### MAKE SURE THAT A KIND OF COOL GRAPHIC SHOWS UP AS A RESULT...COME SEE US WITH ANY ERROR MESSAGES DURING OFFICE HOURS - ALLISON HAS EXTRA OFFICE HOURS 1 - 3pm WEDNESDAY #########

#########
# THAT'S IT! THANKS FOR CHECKING!
#########