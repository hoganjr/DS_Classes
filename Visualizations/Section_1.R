x<-c(3,3,3,3,4,4,2)
table(x) #frequency of unique entries

library(dslabs)
data(heights)

prop.table(table(heights$sex))

#cumulative distribution function CDF
#a<- seq((min(my_data)),max(my_data), length=100) #define range of values spanning the dataset
#cdf_function <- function (x){
#  mean(my_data <= x)
#}
#cdf_values <- sapply(a, cdf_function)
#plot(a, cdf_values)

#~~~~NORMAL DISTRIBUTION~~~~~
#define x as vector of male heights
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex == "Male"
x <- heights$height[index]

#calculate the mean and std dev manually
average <- sum(x)/length(x)
SD <- sqrt(sum((x-average)^2)/length(x))

#built in mean and sd functions
average <- mean(x)
SD <- sd(x)
c(average = average, SD = SD)

#calculate standard units
z <- scale(x)

#calculate propration of values within 2 SD of mean
#taking the mean of a T/F array yield proportion of TRUE values
mean(abs(z)<2)

#~~~~~THE NORMAL CDF AND PNORM~~~~
x<-heights %>% filter(sex=="Male") %>% pull(height)
pnorm(70.5, mean(x), sd(x))
#plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x=a)")

#probabilities in actual data over length 1 ranges containing and integer
mean(x <= 68.5) - mean(x<=67.5)
#probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
#probabilities in actual data over other ranges don't match normal approx as well
mean(x<=70.9)-mean(x<=70.1)
pnorm(70.9,mean(x),sd(x))-pnorm(70.1, mean(x), sd(x))

#~~~~QUANTILES~~~~
summary(heights$height) #to find the quartiles
p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height,p)
percentiles[names(percentiles)=="25%"]
percentiles[names(percentiles)=="75%"]
#qnorm is the inverse of pnorm()
theoretical_quantiles <-qnorm(p,69,3)
#Quantile-Quantile Plots
index <- heights$sex=="Male"
x<-heights$height[index]
z<-scale(x)
p<-seq(0.05,0.95,0.05)
observed_quantiles<-quantile(x,p)
theoretical_quantiles<-qnorm(p, mean(x), sd(x))
plot(theoretical_quantiles,observed_quantiles)
abline(0,1)
#data points fall on the line therofore normal distrib is good assumption 

#make QQ-plot wiht scaled values
observed_quantiles <- quantile(z,p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles,observed_quantiles)
abline(0,1)



