library(tidyverse)
library(dslabs)
install.packages(dplyr)
library(dplyr)
data(murders)
data(heights)
options(digits = 3)
str(heights)

#Q1
mean(heights$height)
ind<-heights$height > mean(heights$height)
sum(ind)

#Q2
avg_height <- mean(heights$height)
ind<- heights$height > mean(heights$height) & heights$sex == "Female"
sum(ind)

#Q3
mean(heights$sex == "Female")

#Q4
min(heights$height)
match(min(heights$height),heights$height)
heights$sex[match(min(heights$height),heights$height)]

#Q5
max_h <- max(heights$height)
max_h
x<-50:82
ind <- x%in%heights$height != "TRUE"
sum(ind)

#Q6
data(heights)
heights2 <- mutate(heights,ht_cm = height*2.54)
heights2$ht_cm[18]
avg_cm <- mean(heights2$ht_cm)

#Q7
females <- filter(heights2,sex == "Female")
nrow(females)
mean(females$ht_cm)

#Q8
data(olive)
head(olive)
names(olive)
plot(olive$palmitic, olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(palmitic~region, data = olive)
