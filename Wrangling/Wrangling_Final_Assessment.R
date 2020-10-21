library(tidyverse)
library(tidytext)
library(pdftools)
library(dslabs)
library(lubridate)
library(broom)
library(textdata)
library(tidyr)
library(scales)
library(ggplot2)
library(stringr)
library(rvest)
library("pdftools")
library(purrr)
library(readxl)
options(digits = 3)    # report 3 significant digits

# On September 20, 2017, Hurricane María made landfall on Puerto Rico. It was the worst natural
# disaster on record in Puerto Rico and the deadliest Atlantic hurricane since 2004. However, 
# Puerto Rico's official death statistics only tallied 64 deaths caused directly by the hurricane 
# (due to structural collapse, debris, floods and drownings), an undercount that slowed disaster 
# recovery funding. The majority of the deaths resulted from infrastructure damage that made it 
# difficult to access resources like clean food, water, power, healthcare and communications in 
# the months after the disaster, and although these deaths were due to effects of the hurricane, 
# they were not initially counted.
# 
# In order to correct the misconception that few lives were lost in Hurricane María, statisticians 
# analyzed how death rates in Puerto Rico changed after the hurricane and estimated the excess number 
# of deaths likely caused by the storm. This analysis suggested that the actual number of deaths in 
# Puerto Rico was 2,975 (95% CI: 2,658-3,290) over the 4 months following the hurricane, much higher 
# than the original count.
# 
# We will use your new data wrangling skills to extract actual daily mortality data from Puerto Rico 
# and investigate whether the Hurricane María had an immediate effect on daily mortality compared to 
# unaffected days in September 2015-2017.

#Q1

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system2("open", args = fn)

#Q2

txt <- pdf_text(fn)

#Q3

x <- str_split(txt[9], "\n")
class(x)

#Q4

s <- x[[1]]
class(s)
s

#Q5

s <- str_trim(s)
s

#Q6

header_index <- str_which(s, pattern = "SEP\\s+2015")
header_index

#Q7

header <- str_split(s[[header_index]],"\\s+", simplify = TRUE)
#header <- s[[header_index]]
month <- header[1]
header <- header[2:length(header)]
header


#Q8

tail_index <- str_which(s, pattern = "Total")
tail_index

#Q9

n <- str_count(s, pattern ="\\d+")
sum(n ==1)
n

#Q10

s1 <- s[(header_index+1):(tail_index-1)]
n1 <- str_count(s1, pattern ="\\d+") 
single_entry <- which(n1 == 1)
single_entry
s2 <- s1[-single_entry]
str_count(s2, pattern ="\\d+") 

#Q11

s3 <- str_remove_all(s2, "[^\\d\\s]")

#Q12

s4 <- str_split_fixed(s3, "\\s+", n = 5)[,1:5]

tab <- data.frame(s4)
head(tab)
tab$X1 <- as.numeric(as.character(tab$X1))
tab$X2 <- as.numeric(as.character(tab$X2))
tab$X3 <- as.numeric(as.character(tab$X3))
tab$X4 <- as.numeric(as.character(tab$X4))
tab$X5 <- as.numeric(as.character(tab$X5))
head(tab)
class(tab$X1)
tab <- tab %>% setNames(c("day", header)) %>% mutate(month = month) 
head(tab)

mean(tab$`2015`)

mean(tab$`2016`)

mean(tab$`2017`[1:19])

mean(tab$`2017`[20:30])

#Q13

tab3 <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
head(tab3)

#Q14

tab3 %>% ggplot(aes(day, deaths, color = year)) +
  geom_line()

