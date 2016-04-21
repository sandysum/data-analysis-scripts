# ESM 244 LAB
# SANDY SUM
#####################
library(ggplot2)
library(agricolae)
install.packages("agricolae")
setwd("/Users/sandys/Dropbox/UCSB WINTER 2015/ESM 244/Data")

apples1 <- read.csv("apples1.csv")
apples2 <- read.csv("apples2.csv")
Butterflies <- read.csv("Butterflies.csv")
Cars <- read.csv("Cars.csv")

# Explore the dataset by using boxplot function

boxplot(apples1)

# Stacking creates a new catogorical variable from all the different
# independent variables and stacking them together.
newapples1 <- stack(apples1)

# quartz() prints the graphic into a new window
quartz()
ggplot(newapples1, aes(x = ind, y = values)) +
  geom_boxplot()

# Part 1.3.a
Test1 <- t.test(apples1$A, apples1$D)

data:  apples1$A and apples1$D
# t = -0.2567, df = 37.965, p-value = 0.7988
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -19.73493  15.29340
# sample estimates:
#   mean of x mean of y 
# 101.7605  103.9812

# default in t.test is two.sided, hence we want to find if apples in soil F is greater in mass than apples in soil C. We can reject.... therefore it is significant.

Test2 <- t.test(apples1$F, apples1$C, alternative = "greater")

# Part 1.4

Test3 <- aov(values ~ ind, data = newapples1)
summary(Test3) 

# AT LEAST TWO GROUPS DIFFERS SIGNIFICANTLY /

Test3PH <- TukeyHSD(Test3)
# post-hoc HSD to check if there are significant differences across all 
# treatments
Test3PHNew <- HSD.test(Test3, "ind", group = T)

quartz()
pairs(apples2) 

# nothing much to be worried about - corr bet IV and DV is desirable. Assumptions are not violated.

# based on my conceptual understanding - all four of these variable SHOULD be included.

LM1 <- lm(Mass ~ TreeAge + SoilType + SoilMoisture + Species, data = apples2)
summary(LM1)

plot(LM1)

summary(step(LM1))

#Part 3.1 MANOVA

CarCor <- cor(Cars$MPG, Cars$Emissions)

# Use column bind function to create x vector
Perf <- manova(cbind(Cars$MPG, Cars$Emissions) ~ Cars$Car, data = Cars)

Perfsummary <- summary(Perf)

# look at contribution of both individually
summary.aov(Perf)

# Part 4 
##########################

# reasonable to believe that there are some interaction / correlation between 
# the different catogorical variables

View(Butterflies)

ButterflyANOVA <- aov(Lifespan ~ Species*Size, data = Butterflies)
summary(ButterflyANOVA)

# we want to force this to be a type 3 anova
ButterflyTypeIII <- drop1(ButterflyANOVA, ~., test = "F")
summary(ButterflyTypeIII)

# Lifespan ~ Species * Size
# Df Sum of Sq     RSS    AIC F value  Pr(>F)  
# <none>                     8617.5 88.920                  
# Species       2    3552.6 12170.1 89.062  1.4429 0.29874  
# Size          1    5002.1 13619.5 92.412  4.0632 0.08365 . No sig result
# Species:Size  1    4970.0 13587.5 92.384  4.0372 0.08446 . No sig result
# we cannot reject the null hypothesis.

