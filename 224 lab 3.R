##############################
# ESM 244 Lab 3
##############################

install.packages("boot")
install.packages("alr3")

library(boot)
library(alr3)
library(ggplot2)

##############################
# Part 1 Jittle/boxplots in ggplot
##############################

quartz()
ggplot(chickwts, aes(x = feed, y = weight)) +
  geom_jitter(aes(color = feed)) +
  geom_boxplot(outlier.size = 0, aes(color = feed))

##############################
# Part 2 Bootstrapping
##############################

weights <- PlantGrowth$weight[1:10]

FunMean <- function(x,i) {
  mean(x[i])
}

FunMedian <- function(x,i) {
  median(x[i])
}

# boot only take a single vector of values, note that a hundred simulations is really low.
BootMean100 <- boot(weights, FunMean, 100)

plot(BootMean100)

# the bootstrapping values are saved in a column called t
hist(BootMean100$t)
BootMean1000 <- boot(weights, FunMean, 1000)

quartz()
plot(BootMean1000)

# calculating bias manually
mean(weights) - mean(BootMean1000$t)

summary(BootMean1000)

MeanCI <- boot.ci(boot.out = BootMean1000, type = "all")

# Mean plant weight was found to be 5.035 g (n = 10), with a bootstrapped CI of 4.68 to 5.38 g ( 95% CI, n = # 1000 ), use the percentile or BCa CI, that's in terms of probability. If you have skewed or bias data
# you might use BCa CI instead. READ.

##############################
# Part 3
##############################

DonnerBLR <- glm(Outcome ~ Age + Sex, data = donner, family = "binomial")

summary(DonnerBlR) # gives test stat

LogOdds5girls <- 1.6218 - .03561*5 + 0
Odds5girls <- exp(LogOdds5girls) # 4.24

Odds25man <- exp(1.6218 - .03561*25 - 1.06798) # .71

confint(DonnerBLR)

##############################
# GRAPH IT
##############################

# making a sequence of ages, 0-100 repeated twice

SequenceAge <- rep(seq(from = 0, to = 100), 2)

# create variable called sex

Sex <- c("Male", "Female")
Reps <- 101
SequenceSex <- sapply(Sex, function(x) rep(x, Reps))
SexVector <- as.vector(SequenceSex)

DonnerFrame <- data.frame(Age = SequenceAge, Sex = SexVector)

# use cbind to joint the original data frame with a new column that is the predicted data
PredictData <- cbind(DonnerFrame, predict(DonnerBLR, newdata = DonnerFrame, type = "link", se = TRUE))

View(PredictData)

# Have created a vector and finding the upper and lower limit of prob of survival (95% CI) and mean prob of # survival. 1.96 is 5% sig fig level. 
PredictData2 <- within(PredictData, {
  PredictedProb <- plogis(fit)
  LowLimit <- plogis(fit - 1.96*se.fit)
  UpLimit <- plogis(fit + 1.96*se.fit)
  }
) 

quartz()
ggplot(PredictData2, aes(x = Age, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LowLimit, ymax = UpLimit, fill = Sex ), alpha = .2) +
  geom_line(aes(colour = Sex), size = 1)




