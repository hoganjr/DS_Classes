library(tidyverse)
library(dslabs)
install.packages(dplyr)
library(dplyr)
data(murders)
data(heights)

#Q1
sex_vec <- ifelse(heights$sex == "Female", 1, 2)
sum(sex_vec)

#Q2
new_ht <- ifelse(heights$height > 72, heights$height, 0)
mean(new_ht)

#Q3
inches_to_ft <- function(x){
  x/12
}
inches_to_ft(144)
fiveft <- ifelse(inches_to_ft(heights$height)<5,1,0)
sum(fiveft)

#Q5
m<-10
f_n <- vector(length = m)
for(n in 1:m){
  f_n[n] <- factorial(n)
}
f_n