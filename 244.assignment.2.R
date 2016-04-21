library(ggplot2)
lob <- read.csv("Lobster.csv")
lobl <- glm(Survival ~ Size, family = "binomial", data = lob)
summary(lobl)
plot(lob$Size, lob$Survival, ylab = "Survival (Yes = 1)", xlab = "Lobster Carapace Length/mm")


lobframe <- data.frame(Size = seq(0,70,1))

PredictData <- cbind(lobframe, predict(lobl, newdata = lobframe, type = "link", se = TRUE))

Predictlob <- within(PredictData, {
  PredictedProb <- plogis(fit)
  LowLimit <- plogis(fit - 1.96*se.fit)
  UpLimit <- plogis(fit + 1.96*se.fit)
}
) 

quartz()
ggplot(Predictlob, aes(x = Size, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LowLimit, ymax = UpLimit), fill = "pink", alpha = .4) +
  geom_line(size = .8, colour = "red") + 
  ylab("Predicted Probablity of Survival\n") +
  xlab("\nLobster Carapace Length (mm)") +
  scale_x_continuous(breaks=seq(0, 70, 5)) +
  theme(
    axis.text.x = element_text(colour="grey20",size=18),
    axis.text.y = element_text(colour="grey20",size=18),
    axis.title.x = element_text(colour="grey20",size=26),
    axis.title.y = element_text(colour="grey20",size=26)
  )

exp(.19586)
    

########

row1 <- c(255,330,313,431) 
row2 <- c(218,452,252,452)
row3 <- c(41,260,138,286)

count <- rbind(row1, row2, row3)
count <- data.frame(count)
colnames(count.props) <- c(2011:2014, "Total")
rownames(count.props) <- c("NOAA SW Fisheries Science Center", "SB Gray Whale Counts", "American Cetarean Society - LA Chapter", "Total")

count.props <- prop.table(count)
count.props <- rbind(count.props,colSums(count.props))
count.props <- cbind(count.props,rowSums(count.props))

write.table(count, file = "whale count.csv")

whale <- melt(count2, variable.name = "Year")

count <- cbind(Source = rownames(count), count) 
  
# plot actual counts
ggplot(whale, aes(Year, value, fill = Source)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Actual Count for Gray Whales") +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12)) 
  
# plots prop counts
ggplot(whale, aes(Year, Count, fill = Source)) +
  geom_bar(stat = "identity", position = "fill") +
  xlab("Year") +
  ylab("Count Proportion for Gray Whales") +
  theme(axis.text.x = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.text=element_text(size = 15)
        )

ggplot(whale, aes(Source, Count, fill = Year)) +
  geom_bar(stat = "identity", position = "fill") +
  xlab("Source") +
  ylab("Count Proportion for Gray Whales") +
  theme(axis.text.x = element_text(size = 10, angle = 40),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.text=element_text(size = 15)
  )

whale.test <- chisq.test(count)

whale.test$observed
write.csv(whale.test$stdres, file = "stdres.csv")
summary(whale.test)

####

# part 3

ggplot(fish, aes(O2, fill = Environment)) +
  geom_histogram(binwidth=2) +
  facet_grid(~Environment) +
  ylab("Frequency") + 
  xlab("Oxygen Consumption(mg/h)")

shapiro.test(fish[1:8,2])
shapiro.test(fish[9:22,2])

mean(fish[1:8,3])
mean(fish[9:22,3])

ggplot(fish, aes(Weight, O2, colour = Environment)) +
  geom_point(size = 3) +
  stat_smooth(method = "lm", size = 1) +
  ylab("Oxygen Consumption(mg/h)") +
  xlab("Weight(g)") +
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        legend.text=element_text(size = 15)
  )
fishmodel <- lm(O2 ~ Weight + Environment, data = fish)  
fishanc <- anova(fishmodel)

summary(fishmodel)
posthocfish <- TukeyHSD(fishanc)
summary(fishmodel)
###

kermit <- read.csv("KermitAttitudes.csv")

colnames(kermitplot) <- c("values", "ind")

ggplot(kermitplot, aes(values, fill = ind)) + 
  geom_histogram(alpha = 0.4) +
  xlab("Rank Scores") +
  ylab("Frequency") +
  scale_fill_discrete(name="Time Survey Was Taken", 
                      breaks=c("Score.post", "Score.pre"),
                      labels=c("Post-ceremony", "Pre-Ceremony"))

ggplot(kermitplot, aes(x=ind, y=values, fill=ind)) +
  geom_boxplot() +
  xlab("Time Survey Was Taken") +
  ylab("Rank Scores") +
  guides(fill=FALSE)

mean(kermit$Score.pre)
mean(kermit$Score.post)

median(kermit$Score.pre)
median(kermit$Score.post)
    

ggplot(kermitplot, aes(values, fill = ind)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')

kermitplot <- stack(kermit, select = c("Score.pre", "Score.post"))

kermitWSR <- wilcox.test(kermit$Score.post, kermit$Score.pre, paired = TRUE, alternative = "greater")

plot(fishanc)



