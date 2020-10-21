library(tidyverse)
library(dslabs)
codes<-c(380,124,818)
country<-c("italy","canada","egypt")
names(codes)<-country
names(codes)

#Sorting
data(murders)
sort(murders$total)
murders$population[51]
murders["AL"]

#SECTION 2 ASSESSMENT
#Q1
x <- c(2, 43, 27, 96, 18)
sort(x)
order(x)
rank(x)
#Q2
min(x)
which.min(x)
max(x)
which.max(x)
#Q3
name <- c("Mandi","Amy","Nicole","Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
time <- time/60
time
speed <- distance/time
speed
name[which.max(speed)]