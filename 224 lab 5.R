library(forecast)
library(ggplot2)
CellGrowth <- read.csv("CellGrowth.csv")

###########################################
# Part 1. Nonlinear Model (Logistic Growth)
###########################################

Graph1 <- ggplot(CellGrowth, aes(Time, CellCount)) +
  geom_point()

# K = A = 3700
# N0: 2000
# B = .85
# r = .1035

BEstimate <- (3700 - 2000) / 2000
REstimate <- lm(log(CellGrowth$CellCount[1:5]) ~ CellGrowth$Time[1:5]) 

# my y values are simply the nat log of the first 5 value of cell count 

#d. 

# Manually typing out the exponential model: telling R that this is the formula that I want to find the best fit model for 

ModelFit <- nls(CellCount ~ A/(1 + B*exp(-r*Time)), start = list(A=3700, B=.85,r=.1035), data = CellGrowth, trace = TRUE)

# new values are stored sequentially in the model. It seems to have iterated like 10 times, before finding the minimum SSR which is 87187.7

# to get the best fit parameter --

#e.
A <- coef(ModelFit)[1]
B <- coef(ModelFit)[2]
r <- coef(ModelFit)[3]

#f. use this model to predict values of y based on best estimate of parameter.

TimeSeq <- seq(0, 20, length = 100)

#g. 

CountPredict <- A/(1+B*exp(-r*TimeSeq)) 

# this is the model predicted data

#h.

PredictFrame <- data.frame(TimeSeq, CountPredict)

#i. Predict frame will be your data source

Graph2 <- ggplot(CellGrowth, aes(Time, CellCount)) +
  geom_point(colour = "blue", size = 3) +
  theme_bw() + # this is the original data and I am plotting it as the base layer
  geom_line(data = PredictFrame, aes(TimeSeq, CountPredict), colour = "orange", size = 1) +
  labs(title = "Predicted Cell Growth vs Actual Cell Growth (points)")

###########################################
# Part 2. Non-linear Model Parameter for Sinusoidal Data
###########################################

pop <- read.csv("PopData.csv")

graph.pop <- ggplot(pop, aes(Year, Population)) + 
  geom_point()

# A2 = 7
# B2 = (2*pi)/T = (2*pi)/12 = B2 = .5236
# D2 = 25

ModelFitPop <- nls(Population ~ A2*sin(B2*Year) + D2, data = pop, 
                   start = list(A2 = 7, B2 = .48, D2 = 25), 
                   trace = TRUE)

A2 <- coef(ModelFitPop)[1]
B2 <- coef(ModelFitPop)[2]
D2 <- coef(ModelFitPop)[3]

TimeFrame <- seq(0,40, length = 120)

PopPredict <- A2*sin(B2*TimeFrame) + D2

PopTable <- data.frame(TimeFrame, PopPredict)

Graph.pop <- ggplot(pop, aes(Year, Population)) +
  geom_point(colour = "red", size = 3) +
  theme_bw() + 
  geom_line(data = PopTable, aes(TimeFrame, PopPredict)
            , colour = "blue", size = 1)

# Part 3. Intro to TS

Arctic<-read.csv("ArcticTemp.csv")

GraphTS<- plot(Arctic$MonthElapse, Arctic$Temp, type = "l")

TempData <- Arctic$Temp

TempTS <- ts(data = TempData, frequency = 12, start = c(1975,1))

Graph3 <- plot.ts(TempTS)

DecompTemp <- decompose(TempTS)

plot(DecompTemp) #coooool

forecast1 <- HoltWinters(TempTS)
plot(forecast1) # red is forecast graph and black is original

# Computes Holt-Winters Filtering of a given time series. Unknown parameters 
# are determined by minimizing the squared prediction error.

forecast2 <- forecast.HoltWinters(forecast1, h = 48) 
#use same unit as the plot, which is in months

plot(forecast2)

plot.ts(forecast2$residuals) # my residual looks random over time, 
# the distribution should be normal and close to mean

hist(forecast2$residuals)




