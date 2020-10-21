library(dslabs)
data("murders")
class(murders)
str(murders)
head(murders)
murders$population
names(murders)
pop<-murders$population
length(pop)
class(pop)
class(murders$state)
class(murders$region)
levels(murders$region)
#SECTION 1 ASSESMENT QUESTIONS
#QUESTION 1
a<-2
b<--1
c<--4
sol_1<-(-b+sqrt(b^2-4*a*c))/(2*a)
sol_2<-(-b-sqrt(b^2-4*a*c))/(2*a)
#QUESTION 2
log(1024,4)
4^5
#QUESTION 3
data("movielens")
str(movielens)
#QUESTION 4
nlevels(movielens$genres)
