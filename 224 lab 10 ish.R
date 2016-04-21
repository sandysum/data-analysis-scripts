ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("tm","RColorBrewer","wordcloud","igraph","stargazer","dendextend","gplots","hexbin")
ipak(packages)

repub <- file.path("~", "Desktop","texts")
dir(repub)
doc <- Corpus(DirSource(repub))

class(doc)
?Corpus # Corpora are collections of documents containing (natural language) text. 

doc.nopunc <- tm_map(doc, removePunctuation)
doclower <- tm_map(doc.nopunc, tolower)
stopwords("english") # common words that can be removed, does not affect. But beware of the order in which you clean the doc, if you remove punctuation, she's = shes and may notbe removed. 

docSim <- tm_map(doclower, removeWords, stopwords("english"))
docSim <- tm_map(docSim, removeWords, c("applause", "booing", "bell"))

for (j in seq(docSim)) {
  docSim[[j]] <- gsub("north korea", "north_korea", docSim[[j]])
}

docSim3 <- tm_map(docSim, stripWhitespace)
docSim4 <- tm_map(docSim3, PlainTextDocument)

dtm <- DocumentTermMatrix(docSim4)

freq <- colSums(as.matrix(dtm))
ord <- order(freq)
hist(freq)

freq[head(ord)] # look at frequency of words least occuring
freq[tail(ord)] # look at frequency of words most occuring

FreqDec <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
head(FreqDec, 60) # top 60 words and not top 6.
class(FreqDec)
wf <- data.frame(word = names(FreqDec), freq = FreqDec)

quartz()
FreqGraph <- ggplot(subset(wf, freq > 50), aes(word, freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

quartz()
wordcloud(names(freq), freq, max.words = 100, scale = c(4, .5), colors = brewer.pal(8, "Spectral"))


############### MAKE NICE TABLE IN R ##################

stargazer(faithful, type = "html", covariate.labels = c("Eruption Duration (min)", "Waiting Time (min)"), out = "faithfultable.htm")

LM1 <- lm(mpg ~ cyl + disp, data = mtcars)
LM2 <- lm(mpg ~ cyl + disp + hp, data = mtcars)
LM3 <- lm(mpg ~ cyl + disp + hp + qsec, data = mtcars)

stargazer(LM1, LM2, LM3, type = "html", dep.var.labels = c("Miles per Gallon"), 
          covariate.labels = c("Cylinders", "Displacement (cubic cm)","Horsepower", "Quarter Mile Time (s)"),
          out = "CarModels.htm")

# exporting good version of graphic

Graph1 <- ggplot(cars, aes(speed, dist, color = speed, size = dist)) +
  geom_point() + 
  theme_bw() +
  labs(x = expression(Example1^superscript), y = expression(Example2[subscript])) +
  ggtitle("This is a graph \n with a second line") +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0, 30), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 130), expand = c(0,0)) + 
  theme(text = element_text(family = "Times New Roman"))

tiff("Plot1.tiff", width = 4, height = 3, units = "in", res = 300)
Graph1
dev.off()

jpeg("Plot1.jpeg", width = 4, height = 3, units = "in", res = 200)
Graph1
dev.off()

########## hexagon binning (bivariate histograms)
## hexbin

x <- rnorm(1000, mean = 5, sd = .5)
y <- rnorm(1000, mean = 8, sd = .3)


head(x)
head(y)

df <- cbind(x,y)
df <- data.frame(df)

HexPlot <- hexbin(df$x, df$y, xbins = 30)

quartz()
plot(HexPlot)

describe(iris)
