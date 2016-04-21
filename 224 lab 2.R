setwd("/Users/sandys/Dropbox/UCSB WINTER 2015/ESM 244/Data")

########################################
# Sandy Sum
# Lab week 2
#######################################

install.packages("car")
library('car')
install.packages('pgirmess')
library(pgirmess)

#######################################
# PART 1 #
#######################################

# subsetting data based on column

Cars1 <- mtcars[c(1:4)]

Cars2 <- mtcars[c(3,4,8)]

Cars3 <- mtcars[c("mpg", "cyl", "hp")]

# looking for the column number of a specific column name

which(colnames(mtcars)=="carb")

CarsNew <- mtcars[c("mpg", "cyl", "carb")]

# two-way ANOVA

# use the short hand version where you just type in the interaction term
# R assumes that you want to look at the effects of the other two variables
# remember to drop / overwrite the default type 1 sequential ANOVA - make it 
# into an orthogonal non-sequential function (fixed)

CarsAnova <- aov(mpg ~ cyl*carb, data = CarsNew)
CarsAnovaFixed <- drop1(CarsAnova, ~., test = "F")

CarsAnovaFixed

#######################################
# PART 2 # Irises (ANCOVA)
#######################################

attach(iris)

# add the comma to include order for all column

Order1 <- iris[order(Sepal.Length),]

View(Order1)

Order2 <- iris[order(Species),]

Order3 <- iris[order(Sepal.Width, -Petal.Length),]

quartz()

# the pch will correspond to the species
SeriesPlot <- plot(iris$Petal.Length ~ iris$Petal.Width, pch = c(15,16,17)[iris$Species], col = c("blue", "pink", "green")[iris$Species])


IrisANCOVA1 <- aov(Petal.Length ~ Species + Petal.Width, data = iris)
drop1(IrisANCOVA1, ~., test = "F")

quartz()
Interaction <- with(iris, {interaction.plot(Petal.Length,Species,Petal.Width)})

#######################################
# PART 3 # MLR with VIF
#######################################

CarLM <- lm(mpg ~ cyl + disp + hp + drat + wt, mtcars)
CarVIF <- vif(CarLM)

#######################################
# PART 4 # Mann-Whitney
#######################################

View(InsectSprays) 

#InsectSpray is a stacked data

Insect <- unstack(InsectSprays)
# check if they are normal
hist(Insect$A)

TestA <- wilcox.test(Insect$A,Insect$D)

# retain null hyp, it is different

# order matters a lot

TestB <- wilcox.test(Insect$B,Insect$E, alternative = "greater")

# retain null hyp, it is greater

TestC <- wilcox.test(Insect$A, Insect$B, alternative = "greater", paired = T)

# There is no increase from A to B if data is paired

#######################################
# PART 5
#######################################

?shapiro.test 
# this is the formal test for normality
# testing for null hyp that the data truly is normal
hist(faithful$eruptions)
shapiro.test(faithful$eruptions)

# p-value = 9.036e-16; reject null hyp that it is normal

hist(chickwts[[1]])

# assume that chickwt is not normal

KWtest <- kruskal.test(weight ~ feed, data = chickwts)

# p-value = 5.113e-07; they are significantly different, BUT WHICH ONE?
# post-hoc test needed

# is there is sig different? T / F
KWposthoc <- kruskalmc(chickwts$weight, chickwts$feed)