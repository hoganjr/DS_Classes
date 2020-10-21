library(tidyverse)
library(dslabs)
library(ggrepel)
library(ggthemes)
data(stars)
options(digits = 3)   # report 3 significant digits

#~~~~~QUESTION 1~~~~~~
str(stars)
mean(stars$magnitude)
sd(stars$magnitude)

#~~~~QUESTION 2~~~~~~
stars %>% ggplot(aes(magnitude, y = ..count..)) + geom_density(alpha = 0.2, bw = 0.75)

#~~~~QUESTION 3~~~~~~
stars %>% ggplot(aes(temp, y = ..count..)) + geom_density(alpha = 0.2, bw = 0.75)

#~~~~QUESTION 4~~~~~
stars %>% ggplot(aes(temp, magnitude))+geom_point()

#~~~~~QUESTION 5 & 6 & 7~~~~~~
stars %>% ggplot(aes(log10(temp), magnitude))+geom_point()+scale_y_reverse()+
  scale_x_reverse()
#giants appear to be roughly log10 temp of 3.7
10^3.7
#~~~~QUESTION 8~~~~~
stars %>% ggplot(aes(log10(temp), magnitude, label = star))+geom_point()+scale_y_reverse()+
  scale_x_reverse()+geom_text_repel()

#~~~~~QUESTION 9~~~~~~
stars %>% ggplot(aes(log10(temp), magnitude, color = type))+geom_point()+scale_y_reverse()+
  scale_x_reverse()
