### libraries and such
library(ggplot2)
library(tibble)
library(tidyr)
library(readr)
library(purrr)
library(dplyr)
library(stringr)
library(forcats)
library(lubridate)
install.packages("FSA")
library(FSA)
library(magrittr)

###  Q1
fish_data = tibble(read.csv("/Users/Syrena/Desktop/UH_classes/MBIO612/YERockfish.txt"))
fish2 = fish_data %>% 
  select(date, length, age, maturity, stage) %>% 
  mutate(date = NULL, date2 = as.POSIXct(x=fish_data$date, format= "%m/%d/%Y"))

### Q2
head(fish2)

### Q3
count(fish2)

### Q4
fish2$year = year(fish2$date2)
str(fish2$year)
counts_year = fish2 %>% group_by(year) %>% tally()
ggplot(counts_year, aes(x= year, y= n)) + geom_bar(colour = "springgreen2", stat = "identity")

### Q5
fish3=counts_year[c(1:4),]

### Q6
fishv2 = fish2[-which(is.na(fish2$maturity)),]
fishv2 %>% mutate(lcat = lencat(length, w=2))
head(fishv2)
### this is adding some really weird columns in my datdset and not sure how to prevent that.... so just going to roll with it   
frequency = xtabs(~lcat$length+maturity, data = fishv2)
proportions = prop.table(frequency, margin = 1)
round(proportions,3)
model1 = (glm(maturity~length, data = fishv2, family = binomial))
plot((as.numeric(maturity)-1)~length, data=fishv2, pch=19, xlab="length cm", ylab= "% mature")
points(proportions[,"Mature"]~as.numeric(rownames(proportions)))
length = seq(30, 70, length.out=99)
prediction = predict(model1, data.frame(length=length), type = "response")
lines(prediction~length, lwd=3)  


### Q7
#### seems to be between 38-42cm
