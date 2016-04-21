## FINAL REPORT R SCRIPT, ESM 244 WINTER 2016
## SANDY SUM

install.packages("lmtest")
library(lmtest)
library(ggplot2)

# Using whale abundance data from GOA.
whale <- read.csv("whaleabundance.csv", header = T)
# Remove redundant columns and NA values.
whaleAbundance <- whale[,c(1,3)]
whaleAbundance <- whaleAbundance[complete.cases(whaleAbundance),]

# Plot shows quadratic growth pattern.
ggplot(whaleAbundance, aes(Year, Abundance.Estimate)) +
  geom_point(size = 4, shape = 1)

whaleAbundance$Sightings <- whaleAbundance$Abundance.Estimate

# running least squares on whale estimate against year.
whalels <- lm(Sightings ~ Year, data = whaleAbundance)
summary(whalels)

plot(whaleAbundance$Year, whaleres)

# Get the residuals
whaleres <- whalels$residuals

# manual Breusch Godfrey test: don't need to do this if you have lmtest package! Just use the bgtest() func.
n = length(whaleres)
plot(whaleres[-n], whaleres[-1])

whaleresmod <- lm(whaleres[-1] ~ whaleres[-n] + whaleAbundance$Year[-1]) 
summary(whaleresmod)

# using the bgtest function from lmtest package in R
bgtestwhale <- bgtest(Sightings ~ Year, order = 2, data = whaleAbundance)
dwtestwhale <- dwtest(Sightings ~ Year, data = whaleAbundance)

# compare with DW test outcome
coeftest(bgtestwhale)
coeftest(dwtestwhale)

# correlogram
whaleACF <- acf(whaleres, type = "correlation", main = "Plot of residual against lags")

library(nlme)
#this will give same results as OLS
whale <- gls(Sightings ~ Year, data = whaleAbundance, method="ML")
hist(whaleGLS$residuals)

# AR(1), q=0 so it is technically an AR(1)
mod3 = gls(Sightings ~ Year, data = whaleAbundance, correlation = corARMA(form=~Year, p = 1, q = 0))
qqnorm(mod2$residuals)
summary(mod2) 
plot(mod2)

# compare with AR(2)
mod3 = gls(Sightings ~ Year, data = whaleAbundance, correlation = corARMA(form=~Year, p = 2, q = 0))
summary(mod3)
plot(mod3)

# I saved it as a ts and used auto.arima on it just to check. Same results!
auto.arima(whalets)

whaleARIMA <- arima(whalets, 
                  order = c(1,1,0))

hist(whaleARIMA$residuals)
