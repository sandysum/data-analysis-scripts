fi<-list.files(getwd(),full.names=T)

dat <- lapply(fi,read.csv)

dat$Year
library(plyr)
df <- ldply(dat, data.frame)

library(dplyr)

library(magrittr)
daily <- temp %>% group_by(Year, Month, Day) %>%
  summarise(pmm25 = mean(Value))

psi <- ts(daily$pmm25, 
          frequency = 7,
          start = c(1,1))

plot(psi)
pm25 <- decompose(psi)
plot(pm25)
