##############################
# ESM 244 Lab 4 / 28.1.16
##############################

install.packages("nnet")
install.packages("MASS")
install.packages("reshape2")

library(nnet)
library(MASS)
library(reshape2)
library(ggplot2)
library(alr3)

setwd("~/Dropbox/UCSB WINTER 2015/ESM 244/Data")

K <- read.csv("KermitAttitudes.csv")
P <- read.csv("Purchase.csv")

##############################
# 1.Multinomial Logistic Regression
##############################

LevelsPurchase <- levels(P$Purchase)

#c change reference level for education level

P$EdLevel <- relevel(P$EdLevel, ref = "HS")

#d Multinomial Logistic Regression

PurchaseMLR <- multinom(Purchase ~ EdLevel + Income, data = P)

## as I switch from a HS to College level grad, there is increased likihood that I will switch from generic to local organic. (base is HS and generic purchase)

#f view exponentiated coefficients / relatively risk coefficients

Risk <- exp(coef(PurchaseMLR))

# As Income increases by 1,000 dollar, the odds of buying local organic vs generic fall from 1 to .9717 / is decreased by a factor of .9717

#g these are already in probabilities
ObsProb <- fitted(PurchaseMLR) 
# different from plogis function from last week's lab - this fitted function works well for Multinom regression ... should try using plogis = good for binary logistic regression

#h 

Frame <- data.frame(EdLevel = rep(c("HS", "College", "Graduate"), each=121), Income = rep(seq(0,120),3))

#predict function , if you put in type = probs, you want to make predictions based on probs, not #in terms of log odds or odds. you can also use fitted but there are more steps.

#i remember that predict function will assign column names based on your original MLR model, 
# your new data frame must have the same column names !!!

Predprob <- cbind(Frame, predict(PurchaseMLR, newdata = Frame, type = "probs"))
View(Predprob)
# you will see predicted probabilities for the combination of characteristics that each person i # has from the new fake data

#melt is like stacking data, outcome var in one column and probs in another column
#tell melt what is your independent variables... id.vars
Predprobmelt <- melt(Predprob, id.vars = c("EdLevel", "Income"), value.name = "Probability")

#k
quartz()
cheese <- ggplot(Predprobmelt, aes(x = Income, y = Probability, colour = EdLevel)) +
  geom_line() +
  facet_grid(variable ~., scales = "free") # scales set to free means that they don't have to # # stick to the same axis level for each facet


##############################
# 2. Ordinal Logistic Regression
##############################

class(K$Score.pre)

# need to change this variable to an factor.

Rankasfactor <- factor(K$Score.pre, levels = c("1", "2", "3", "4", "5"))
class(Rankasfactor)
Kermitnew <- data.frame(Rankasfactor, K$Age, K$Gender)
head(Kermitnew)

colnames(Kermitnew) <- c("Rank", "Age", "Gender")

#e. Ordinal Logistic Regression

KermitOLR <- polr(Rank ~ Age + Gender, data = Kermitnew, Hess = T)

#f. create summary table of the log odds coef and CI

Table1 <- coef(summary(KermitOLR)) # this table is in terms of log-odds
Table1CIs <- cbind(Coef = coef(KermitOLR), confint(KermitOLR))
Table1CIs2 <- exp(Table1CIs) # this gives us the effects of the coefficients in terms of odds

#j. 
NewData <- data.frame(Age = rep(seq(0,60),2), Gender = rep(c("M", "F"), each = 61))

#k.
Predictkermit <- cbind(NewData, predict(KermitOLR, newdata = NewData, type = "probs"))

#i.

Kermitmelt <- melt(Predictkermit, id.vars = c("Age", "Gender"), value.name = "Probability")

#m. 

quartz()

kerm <- ggplot(Kermitmelt, aes(x = Age, y = Probability, colour = Gender)) + 
  geom_line() +
  facet_grid(variable ~., scales = "free")
