

#~~~~~~~ASSESSMENT: PROGRAMMING SKILLS~~~~~~~~~
library(tidyverse)
library(dslabs)
data(heights)
heights

#Q1 OBJECT CLASSES

class(heights) #class of heights dataset
class(heights$sex) #class of sex column
class(heights$height) #class of heights col

#Q2 OBJECT DIMENSIONS

nrow(heights) #num rows of heights df

#Q3 INDEXING -1

heights[777,] #height in row 777

#Q5 MAX AND MIN

max(heights$height) #max height

which.min(heights$height) #row location of min height

#Q6 SUMMARY STATISTICS

mean(heights$height) #mean height

median(heights$height) #median height

#Q7 CONDITIONAL STATEMENTS - 1

mean(heights$sex == "Male") #proportion of males in the dataset

#Q8 CONDITIONAL STATEMENTS - 2

sum(heights$height > "78.0") #total individuals taller than 78"

#Q9 CONDITIONAL STATEMENTS - 3

sum(heights$sex == "Female" && heights$height > "78.0") #num of females taller than 78"

heights %>% filter(sex == "Female") %>% summarise(sum(height > "78.0"))

