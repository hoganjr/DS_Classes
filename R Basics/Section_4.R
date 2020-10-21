#an example showing the general structure of an if-else statement
a <- 0 
if(a!=0){print(1/a)}else{print("No reciprocal for 0")}

#an example that tells us which states, if any, have a murder rate less than 0.5
library(dslabs)
data(murders)
murder_rate<-murders$total/murders$population*100000
ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){print(murders$state[ind])}else{pring("No state has murder rate that low")}

#the ifelse() function work similarly to an if-else conditional
a<-0
ifelse(a>0,1/a,NA)

#the ifelse() function is particularly useful on vectors
a<- c(0,1,2,-4,5)
ifelse(a>0,1/a,NA)

#the ifelse() function is also helpful for replacing missing values
data(na_example)
sum(is.na(na_example))
no_nas <- ifelse(is.na(na_example),0,na_example)
sum(is.na(no_nas))

#the any() and all() functions evaluate logical vectors
z <- c(TRUE,FALSE,FALSE)
any(z)
all(z)

#example of defining a function to compute the average of a vector x
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}
x <- 1:100
identical(mean(x),avg(x))

#variables inside a function are not defined in the workspace
s <- 3
avg(1:10)
s

#functions can have multiple arguments as well as default values
avg <- function(x, arithmetic = TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

#creating a function that computes the sum of integers 1 thru n
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}

#calculate sum(1:n) when n varies from 1 to 25?
m<-25
s_n <- vector(lenght = m) #creates empy vector of length m
for(n in 1:m){
  s_n [n]<- compute_s_n(n)
}

#plotting the function
n<-1:m
plot(n,s_n)
lines(n, n*(n+1)/2)

