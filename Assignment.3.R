library(reshape2) 
library(magrittr)
library(dplyr)
library(ggplot2)

MPA <- read.csv("MPAData.csv")

#Q1

colnames(MPA) <- c("Country", "Code", "in2000", "in2010")
par <- par(mfrow=c(1, 2))
hist(MPA$in2000)
hist(MPA$in2010)

median(MPA$in2000)
median(MPA$in2010)

test.MPA <- wilcox.test(MPA$in2010, MPA$in2000, paired = T, alternative = "greater")
qnorm(3.715e-05, mean = 0, sd = 1, lower.tail = FALSE)

#Q2

rownames(MPAbelize) <- "Belize"

MPAbelize <- MPA[MPA$Country=="Belize", 8:24]

MPAbelize <- t(MPAbelize)
MPAbel <- data_frame(Year = c(0:16), MPA = as.vector(MPAbelize))

Bel.plot <- ggplot(MPAbel, aes(Year, MPA)) +
  geom_point(size = 3, colour = "green") +
  ylab("MPA of Belize") +
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.text=element_text(size = 15)
  )


# K = A = 12
# N0: 0.2
# B = 59
# r = .618

B <- (12-.2) / .2
R <- lm(log(MPAbel$MPA[1:5]) ~ MPAbel$Year[1:5]) 

BelFit <- nls(MPA ~ A/(1 + B*exp(-r*Year)), 
              start = list(A=12, B=59,r=.7418), 
              data = MPAbel, 
              trace = TRUE)

A <- coef(BelFit)[1]
B <- coef(BelFit)[2]
r <- coef(BelFit)[3]

YearSeq <- c(0:16)
BelPredict <- A/(1+B*exp(-r*YearSeq)) 

Predict <- data.frame(YearSeq, BelPredict)

Bel.plot +
  geom_line(data = Predict, aes(YearSeq, BelPredict), colour = "purple", size = 1) +
  labs(title = "Predicted MPA vs Actual MPA of Belize") +
  scale_x_discrete(labels = c(1995:2010,2012))

# Part 3

handsplot <- ggplot(hands, aes(BirthYear, fill = Region, colour = Region)) +
  facet_grid(ThumbStyle~.) +
  geom_density(alpha = .4) 

handsplot +
  xlab("Birth Year of Guitarist") +
  ylab("Count")

# Multinorm model

levels(hands$Region)
levels(hands$ThumbStyle)

handsMLR <- multinom(ThumbStyle ~ Region + BirthYear, data = hands)
oddshands <- coef(summary(handsMLR)) 
table <- exp(oddshands)
handsprob <- fitted(handsMLR)

z <- summary(handsMLR)$coefficients/summary(handsMLR)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2

Frame <- data.frame(Region = rep(levels(hands$Region), each=67), BirthYear = rep(seq(1874,1940,1),3))

Predprob <- cbind(Frame, predict(handsMLR, newdata = Frame, type = "probs"))

Predprobmelt <- melt(Predprob, id.vars = c("Region", "BirthYear"), value.name = "Probability")

quartz()
blues <- ggplot(Predprobmelt, aes(x = BirthYear, y = Probability, colour = Region)) +
  geom_line(size = .8) +
  facet_grid(variable ~., scales = "free") +
  xlab("Birth Year of Guitarist") +
  ylab("Predicted Probabilities")

# Part 4

truckee <- read.csv("TruckeeData.csv")
TStruck <- ts(truckee$RiverFlow,
               frequency = 12,
               start = c(2000, 1))

Graph3 <- plot.ts(TStruck)

DecompTruck <- decompose(TStruck)

plot(DecompTruck)

auto.arima(TStruck)

# ARIMA(2,0,1)(1,0,0)

truckeeARIMA <- arima(TStruck, 
                      order = c(2,0,1), 
                      seasonal = list(order = c(1,0,0))
                      ) 


hist(truckeeARIMA$residuals)

ForecastT <- forecast(truckeeARIMA, h = 60) 
quartz()
plot(ForecastT, ylab = "Monthly Averaged Discharge (cubic feet per second)", xlab = "Year", main = "ARIMA forecast model of the Truckee River discharge")

