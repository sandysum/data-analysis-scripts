library(ggplot2)
library(dplyr)
setwd("/Users/sandys/Dropbox/UCSB WINTER 2015/ESM 244/Data")

Fish <- read.csv("Fishermen.csv", header = TRUE)
boxplot(Abundance~Type,data=Pot, main="Abundance by Type", 
        xlab="Type of Artwork", ylab="Abundance")

boxplot(Abundance~Region,data=Pot, main="Abundance by Region", 
        xlab="Region", ylab="Abundance")

ggplot(Pot, aes(x = Region, y = Abundance, size = Type)) +
  geom_point()

hist(Pot$Abundance, breaks = 24, main = "Abundance", xlab = "Abundance")
interaction.plot(Pot$Type,Pot$Region,Pot$Abundance, xlab = "Type", ylab = "Abundance", trace.label = "Region") 

I.P <- ggplot(Pot, aes(x=Type, y=Abundance, colour=factor(Region), group = 4)) 
I.P + geom_point() + stat_smooth(method="lm")

Abundance <- aov(Abundance ~ Type*Region, data = Pot)
Abundance2 <- drop1(Abundance, ~., test = "F")

Pot %>% group_by(Type, Region) %>%
  summarise(Sample = n())

mercury.reg <- lm(TotHg ~ fisherman + age + weight + fishmlwk + fishpart, data = Fish)

ggplot(Fish, aes(x=age, y=TotHg, col=fishpart)) +
  geom_point()

boxplot(TotHg~fisherman,data=Fish, xlab = 'Fisherman or not?', ylab = 'Total Mercury Concentration')

man <- aov(TotHg ~ fisherman, data = Fish)
man.aov <- drop1(man, ~., test = "F")

man <- lm(TotHg ~ fisherman + age + weight + fishmlwk, data = Fish)
summary(man)
man.ANCOVA <- anova(man)

weight <- lm(TotHg ~ fisherman + weight, data = Fish)
hist(Fish$TotHg, breaks = 30)

reef <- read.csv("ReefPerch.csv", header = TRUE)

loc <- aov(FISHSIZE ~ SITE, data = reef)
summary(loc)

boxplot(reef$FISHSIZE, reef$SITE)

hist(reef$FISHSIZE, breaks = 8, xlab = "Fish size", main = " ")

wilcox.test(w,m)

median(w) median(m)

ggplot(Fish, aes(y = TotHg, x = fishmlwk, col = factor(fisherman))) +
  geom_point() +
  geom_smooth(method='lm')

ggplot(Fish, aes(y = TotHg, x = weight, col = factor(fisherman))) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x = "Weight (kg)", 
       y = "Total mercury concentration (mg/g)", 
       colour = "fisherman")

weight <- lm(TotHg ~ fisherman + weight, data = Fish)
w.ANCOVA <- anova(weight)