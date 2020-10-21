library(tidyverse)
library(dslabs)
install.packages(dplyr)
library(dplyr)
data(murders)
murder_rate <- murders$total / murders$population * 100000
str(murders)

#mutate function used to add column, notice that mutate knows to look in the d
#data frame for the "total" and "population" columns
murders_new <- mutate(murders, rate = total/population * 100000)
head(murders)

filter(murders_new, rate <= 0.71)
new_table<- select(murders, state, region, rate)

#pipe function allows for elimnation of steps
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)

#by default data.frame() turns char in factors. To change that do the following...
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85),
                     exam_2 = c(90, 85, 85, 90), 
                     stringsAsFactors = FALSE)

#basic plotting
population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total
plot(population_in_millions,total_gun_murders)
hist(murders$rate)
#boxplot of murder rates by region
boxplot(rate~region, data = murders)
