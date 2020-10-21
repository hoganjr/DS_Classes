library(rvest)
library(tidyverse)
library(ggrepel)
library(dslabs)
library(readxl)

#~~~~~STRING PARSING~~~~~~~

# read in raw murders data from Wikipedia
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))

# inspect data and column classes
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)

#~~~~~~DEFINING STRINGS: SINGLE AND DOUBLE QUOTES AND HOW TO ESCAPE~~~~~~

s <- "Hello!"    # double quotes define a string
s <- 'Hello!'    # single quotes define a string
s <- `Hello`    # backquotes do not

s <- "10""    # error - unclosed quotes
s <- '10"'    # correct

# cat shows what the string actually looks like inside R
cat(s)

s <- "5'"
cat(s)

# to include both single and double quotes in string, escape with \
s <- '5'10"'    # error
s <- "5'10""    # error
s <- '5\'10"'    # correct
cat(s)
s <- "5'10\""    # correct
cat(s)

#~~~~~~~STRINGR PACKAGE~~~~~~

# murders_raw defined in web scraping video

# direct conversion to numeric fails because of commas
murders_raw$population[1:3]
as.numeric(murders_raw$population[1:3])

library(tidyverse)    # includes stringr

#~~~~~~~CASE STUDY 1: US MURDERS DATA~~~~~~~~~

# murders_raw was defined in the web scraping section

# detect whether there are commas
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))

# replace commas with the empty string and convert to numeric
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)

# parse_number also removes commas and converts to numeric
test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2)

murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new %>% head

#~~~~~CASE STUDY 2: REPORTED HEIGHTS~~~~~~~~

# load raw heights data and inspect
library(dslabs)
data(reported_heights)
class(reported_heights$height)

# convert to numeric, inspect, count NAs
x <- as.numeric(reported_heights$height)
head(x)
sum(is.na(x))

# keep only entries that result in NAs
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% 
  head(n=10)

# calculate cutoffs that cover 99.999% of human population
alpha <- 1/10^6
qnorm(1-alpha/2, 69.1, 2.9)
qnorm(alpha/2, 63.7, 2.7)

# keep only entries that either result in NAs or are outside the plausible range of heights
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

# number of problematic entries
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
length(problems)

# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat

#~~~~~~~~~REGEX~~~~~~~~

# load stringr through tidyverse
library(tidyverse)

# detect whether a comma is present
pattern <- ","
str_detect(murders_raw$total, pattern) 

# show the subset of strings including "cm"
str_subset(reported_heights$height, "cm")

# use the "or" symbol inside a regex (|)
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches")

# highlight the first occurrence of a pattern
str_view(s, pattern)

# highlight all instances of a pattern
str_view_all(s, pattern)

#~~~~~CHARACTER CLASSES, ANCHORS, AND QUANTIFIERS~~~~~

# s was defined in the previous video
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"

# [56] means 5 or 6
str_view(s, "[56]")

# [4-7] means 4, 5, 6 or 7
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")

# ^ means start of string, $ means end of string
pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)

# curly braces define quantifiers: 1 or 2 digits 
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)

# combining character class, anchors and quantifier
pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)

#~~~~~~SEARCH AND REPLACE WITH REGEX~~~~~~~~~

# number of entries matching our desired pattern
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))

# inspect examples of entries with problems
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)
str_subset(problems, "inches")
str_subset(problems, "''")

# replace or remove feet/inches words before matching
pattern <- "^[4-7]'\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

# R does not ignore whitespace
identical("Hi", "Hi ")

# \\s represents whitespace
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)

# * means 0 or more instances of a character
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")

# test how *, ? and + differ
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           nore_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))

# update pattern by adding optional spaces before and after feet symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

#~~~~~~GROUPS WITH REGEX~~~~~~~~

# define regex with and without groups
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"

# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)

# demonstrate the effect of groups
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)

# demonstrate difference between str_match and str_extract
str_match(s, pattern_with_groups)
str_extract(s, pattern_with_groups)

# improve the pattern to recognize more events
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")

# final pattern
pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

# combine stringr commands with the pipe
str_subset(problems, pattern_with_groups) %>% head
str_subset(problems, pattern_with_groups) %>% 
  str_replace(pattern_with_groups, "\\1'\\2") %>% head

#~~~~~~~TESTING AND IMPROVING~~~~~~~

# function to detect entries with problems
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

# identify entries with problems
problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format

# find proportion of entries that fit the pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

converted[!index]    # show problems


#~~~~~~~~~SECTION 3.2 ASSESSMENT~~~~~~~~

#Q4
s <- c("70", "5 ft", "4'11", "", ".","Six feet")
pattern <- "\\d|ft"
str_view_all(s,pattern)

#Q5
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]"
str_detect(animals, pattern)

#Q6
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[A-Z]$"
str_detect(animals, pattern)

#Q7
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]{4,5}"
str_detect(animals, pattern)

#Q8
animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern <- "mo?"
sum(str_detect(animals, pattern))

#Q13
yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)
converted

#~~~~~~SEPARATE WITH REGEX~~~~~~~~

# first example - normally formatted heights
s <- c("5'10", "6'1")
tab <- data.frame(x = s)

# the separate and extract functions behave similarly
tab %>% separate(x, c("feet", "inches"), sep = "'")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# second example - some heights with unusual formats
s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)

# separate fails because it leaves in extra characters, but extract keeps only the digits because of regex groups
tab %>% separate(x, c("feet","inches"), sep = "'", fill = "right")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

#~~~~~~~USING GROUPS AND QUANTIFIERS~~~~~~~

#CASE 1
#if we add '0 to the end of cases like 6' then our pattern will match.
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")  #\\1 refers to group one in the second argument

#CASE 2 & 4
str_replace(s, "^([56])'?$", "\\1'0")

#CASE 3
pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"

#CASE 5
yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")

#TRIMMING
s <- "Hi "
cat(s)
identical(s, "Hi")
str_trim("5 ' 9 ")

#CONVERT UPPER TO LOWER CASE
s <- c("Five feet eight inches")
str_to_lower(s)

#PUTTING INTO A FUNCTION
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}
words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}
problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

#~~~~~~PUTTING IT ALL TOGETHER~~~~~~~~~~
#Let's start by writing a function that cleans up strings so 
#that all the feet and inches formats use the same x'y format when appropriate.

pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

#We can check all the entries we converted using the following code:

new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()

#Let's take a look at the shortest students in our dataset using the following code:
new_heights %>% arrange(height) %>% head(n=7)

#~~~~~~STRING SPLITTING~~~~~~~~

# read raw murders data line by line
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()

# split at commas with str_split function, remove row of column names
x <- str_split(lines, ",") 
x %>% head()
col_names <- x[[1]]
x <- x[-1]

# extract first element of each list entry
library(purrr)
map(x, function(y) y[1]) %>% head()
map(x, 1) %>% head()

# extract columns 1-5 as characters, then convert to proper format - NOTE: DIFFERENT FROM VIDEO
dat <- data.frame(parse_guess(map_chr(x, 1)),
                  parse_guess(map_chr(x, 2)),
                  parse_guess(map_chr(x, 3)),
                  parse_guess(map_chr(x, 4)),
                  parse_guess(map_chr(x, 5))) %>%
  setNames(col_names)

dat %>% head

# more efficient code for the same thing
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>% 
  as.data.frame() 

# the simplify argument makes str_split return a matrix instead of a list
x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)

#~~~~~~~CASE STUDY: EXTRACTING A TABLE FROM A PDF~~~~~~

library(dslabs)
data("research_funding_rates")
research_funding_rates 

#DOWNLOADING THE DATA
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

#txt is a char vector with an entry for each page.  We keep the page we want:
raw_data_research_funding_rates <- txt[2]

#or skip this step b/c the raw data is already in the package
data("raw_data_research_funding_rates")

#LOOKING AT THE DOWNLOAD

#examine the object
raw_data_research_funding_rates %>% head

#it's a long string with \n representing each newline
#create a list with the lines of the text as elements:
tab <- str_split(raw_data_research_funding_rates, "\n")

#we start off with just one element in the string and just one entry
tab <- tab[[1]]
tab %>% head

#we see that info for the column names is the 3rd and 4th entries:
the_names_1 <- tab[3]
the_names_2 <- tab[4]

#EXTRACTING THE TABLE DATA
the_names_1

#remove the leading space and everything after the comma.
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1

#second line
the_names_2
#trim the leading space and split by space as we did for the first line:
the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2
#now join these to generate one name for each col
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

#ready now to get the actual data.  The tab object shows data in lines 6-14:
new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()

#check the objects are identical
identical(research_funding_rates, new_research_funding_rates)

#~~~~~RECODING~~~~~~~

# life expectancy time series for Caribbean countries
library(dslabs)
data("gapminder")
gapminder %>% 
  filter(region=="Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# display long country names
gapminder %>% 
  filter(region=="Caribbean") %>%
  filter(str_length(country) >= 12) %>%
  distinct(country) 

# recode long country names and remake plot
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          'Antigua and Barbuda'="Barbuda",
                          'Dominican Republic' = "DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

#~~~~~~SECTION 3.3 ASSESSMENT~~~~~~~

#Q3
schedule <- tibble(day = c("Mon", "Tue"),staff = c("Man, Chris and Laura", "Steve, Ruth and Frank"))
tidy <- schedule %>% mutate(staff = str_split(staff, ", | and ")) %>% unnest()

#Q4
gapminder %>% filter(region=="Middle Africa") %>% filter(str_length(country) >= 12) %>%  distinct(country) 

#Q5
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

col_names <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
polls <- polls %>% setNames(col_names)
polls <- polls %>% filter(str_length(remain) <= 5 & str_length(remain) > 0)

#Q6
polls2 <- as.numeric(str_replace(polls$remain, "%", ""))/100


#Q7
polls3 <- str_replace_all(polls$undecided, "N/A", "0")

#Q8
temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]{3,5}")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~SECTION 4~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~DATES AND TIMES~~~~~~~~~~

# inspect the startdate column of 2016 polls data, a Date type
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head

# ggplot is aware of dates
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

# lubridate: the tidyverse date package
library(lubridate)

# select some random dates from polls
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

# extract month, day, year from date strings
data.frame(date = dates, 
           month = month(dates),
           day = day(dates),
           year = year(dates))

month(dates, label = TRUE)    # extract month label

# ymd works on mixed date styles
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

# different parsers extract year, month and day in different orders
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)

now()    # current time in your time zone
now("GMT")    # current time in GMT
now() %>% hour()    # current hour
now() %>% minute()    # current minute
now() %>% second()    # current second

# parse time
x <- c("12:34:56")
hms(x)

#parse datetime
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)

#~~~~~~~~~TEXT MINING~~~~~~~

#CASE STUDY: TRUMP TWEETS

# During the 2016 US presidential election, then-candidate Donald J. Trump used 
# his Twitter account as a way to communicate with potential voters. On August 6, 2016 
# Todd Vaziri tweeted about Trump that "Every non-hyperbolic tweet is from iPhone (his staff). 
# Every hyperbolic tweet is from Android (from him)." Data scientist David Robinson 
# conducted an analysis to determine if data supported this assertion. Here we go through
# David's analysis to learn some of the basics of text mining. To learn more about text 
# mining in R we recommend this book.

# We will use the following libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

# In general, we can extract data directly from Twitter using the rtweet package. However, 
# in this case, a group has already compiled data for us and made it available 
# at http://www.trumptwitterarchive.com.

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST"))

#For convenience we include the result of the code above in the dslabs package:
library(dslabs)
data("trump_tweets")

#This is data frame with information about the tweet:
head(trump_tweets)

#The variables that are included are:
names(trump_tweets)

#The help file ?trump_tweets provides details on what each variable represents. 
#The tweets are represented by the text variable:
trump_tweets %>% select(text) %>% head

#and the source variable tells us the device that was used to compose and upload each tweet:
trump_tweets %>% count(source) %>% arrange(desc(n))

#We can use extract to remove the Twitter for part of the source and filter out retweets.
trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source)

#We are interested in what happened during the campaign, so for the analysis here we will 
#focus on what was tweeted between the day Trump announced his campaign and election day. 
#So we define the following table:
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

#We can now use data visualization to explore the possibility that two different groups 
#were tweeting from these devices. For each tweet, we will extract the hour, in the east
#coast (EST), it was tweeted then compute the proportion of tweets tweeted at each hour for each device.
ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

# We notice a big peak for the Android in early hours of the morning, between 6 and 8 AM. 
# There seems to be a clear different in these patterns. We will therefore assume that two 
# different entities are using these two devices. Now we will study how their tweets differ. 
# To do this we introduce the tidytext package.
library(tidytext)

# The main function needed to achieve this is unnest_tokens(). A token refers to the units 
# that we are considering to be a data point. The most common tokens will be words, but they 
# can also be single characters, ngrams, sentences, lines or a pattern defined by a regex. The 
# functions will take a vector of strings and extract the tokens so that each one gets a row in 
# the new table. Here is a simple example:
example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

#Now let's look at a quick example with a tweet number 3008:
i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

# Note that the function tries to convert tokens into words and strips characters important to twitter such 
# as # and @. A token in twitter is not the same as in regular English. For this reason, instead of using the 
# default token, words, we define a regex that captures twitter character. The pattern appears complex but 
# all we are defining is a patter that starts with @, # or neither and is followed by any combination of 
# letters or digits:
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

#We can now use the unnest_tokens() function with the regex option and appropriately extract the hashtags and mentions:
campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#Another minor adjustment we want to make is remove the links to pictures:
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#Now we are ready to extract the words for all our tweets.
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern)

#And we can now answer questions such as "what are the most commonly used words?"
tweet_words %>% count(word) %>%  arrange(desc(n))

#It is not surprising that these are the top words. The top words are not informative. The tidytext package has database 
#of these commonly used words, referred to as stop words, in text mining:
stop_words

#If we filter out rows representing stop words with filter(!word %in% stop_words$word):
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word ) 

#We end up with a much more informative set of top 10 tweeted words:
tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

#Some exploration of the resulting words (not show here) reveals a couple of unwanted 
#characteristics in our tokens. First, some of our tokens are just numbers (years for example).
#We want to remove these and we can find them using the regex ^\d+$. Second, some of our tokens come from 
#a quote and they start with '. We want to remove the ' when it's at the start of a word, so we will 
#use str_replace(). We add these two lines to the code above to generate our final table:
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

# For each word we want to know if it is more likely to come from an Android tweet or an iPhone tweet. 
# We previously introduced the odds ratio, a summary statistic useful for quantifying these differences. 
# For each device and a given word, let's call it y, we compute the odds or the ratio between the proportion 
# of words that are y and not y and compute the ratio of those odds. Here we will have many proportions that 
# are 0 so we use the 0.5 correction.
android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

#Given that several of these words are overall low frequency words we can impose a filter based on the 
#total frequency like this:
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)

#The first step in sentiment analysis is to assign a sentiment to each word. The tidytext package 
#includes several maps or lexicons in the object sentiments:
sentiments 

#There are several lexicons in the tidytext package that give different sentiments. 
# For example, the bing lexicon divides words into positive and negative. We can see this using the 
# tidytext function get_sentiments():
get_sentiments("bing")

#The AFINN lexicon assigns a score between -5 and 5, with -5 the most negative and 5 the most positive.
library(textdata)
get_sentiments("afinn")

#The loughran and nrc lexicons provide several different sentiments:
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

#For the analysis here we are interested in exploring the different sentiments of each tweet, so we will 
#use the nrc lexicon:
nrc <- get_sentiments("nrc") %>%
select(word, sentiment)

#We can combine the words and sentiments using inner_join(), which will only keep words associated with 
#a sentiment. Here are 10 random words extracted from the tweets:
tweet_words %>% inner_join(nrc, by = "word") %>% 
select(source, word, sentiment) %>% sample_n(10)

#Now we are ready to perform a quantitative analysis comparing Android and iPhone by comparing 
#the sentiments of the tweets posted from each device. Here we could perform a tweet by tweet analysis,
#assigning a sentiment to each tweet. However, this somewhat complex since each tweet will have several 
#sentiments attached to it, one for each word appearing in the lexicon. For illustrative purposes, we will 
#perform a much simpler analysis: we will count and compare the frequencies of each sentiment appears for each device.
sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

#Because more words were used on the Android than on the phone:
tweet_words %>% group_by(source) %>% summarize(n = n())

#for each sentiment we can compute the odds of being in the device: proportion of words with sentiment 
#versus proportion of words without and then compute the odds ratio comparing the two devices:
sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))

#So we do see some difference and the order is interesting: the largest three sentiments 
#are disgust, anger, and negative! But are they statistically significant? How does this compare if 
#we are just assigning sentiments at random?
  
#To answer that question we can compute, for each sentiment, an odds ratio and confidence interval. 
#We will add the two values we need to form a two-by-two table and the odds ratio:
library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or

#A graphical visualization shows some sentiments that are clearly overrepresented:
  
  log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 

#We see that the disgust, anger, negative sadness and fear sentiments are associated with the 
#Android in a way that is hard to explain by chance alone. Words not associated to a sentiment 
#were strongly associated with the iPhone source, which is in agreement with the original claim about hyperbolic tweets.

#If we are interested in exploring which specific words are driving these differences, we can back 
#to our android_iphone_or object:
  
  android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))
#We can make a graph:
  
  android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

  
#~~~~~~~~SECTION 4 ASSESSMENT~~~~~~~
library(dslabs)
library(lubridate)
options(digits = 3)    # 3 significant digits

#Q1
#ISO 8601 format for dates is YYYY-MM-DD

#Q2
#Q: which function would convert dates below to the correct format?
dates <- c("09-01-02", "01-12-07", "02-03-04")
#A: It is impossible to know which format is correct without more info

#Q3
data(brexit_polls)
brexit_polls %>% count(month(startdate))

sum(round_date(brexit_polls$enddate, unit = "week") == "2016-06-12")

#Q4
brexit_polls %>% count(weekdays(enddate)) %>% arrange(desc(n))

#Q5
data(movielens)
head(movielens)
movielens %>% mutate(year_reviewed = year(as_datetime(movielens$timestamp,origin = lubridate::origin, tz = "UTC"))) %>% 
  count(year_reviewed) %>% arrange(desc(n))
movielens %>% mutate(hour_reviewed = hour(as_datetime(movielens$timestamp,origin = lubridate::origin, tz = "UTC"))) %>% 
  count(hour_reviewed) %>% arrange(desc(n))

#Assessment Part 2

library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
gutenberg_metadata

#Q6
sum(str_detect(gutenberg_metadata$title , pattern = "Pride and Prejudice"), na.rm = TRUE)

#Q7
real_book <- gutenberg_works(title == "Pride and Prejudice")
id_book <- real_book[1]
id_book

#Q8
words <- gutenberg_download(id_book) 
word_count <- words %>% unnest_tokens(word, text) %>% count(word)
sum(word_count$n)

#Q9
words <- gutenberg_download(id_book) 
word_count <- words %>% unnest_tokens(word, text) %>% filter(!word %in% stop_words$word ) %>% count(word)
sum(word_count$n)

#Q10
word_count_no_digits <- words %>% unnest_tokens(word, text) %>% 
  filter(!word %in% stop_words$word & !str_detect(word, "\\d+")) %>% count(word)
sum(word_count_no_digits$n)

#Q11
word_count_no_digits %>% mutate(word = reorder(word, n)) %>%  arrange(desc(n))
sum(word_count_no_digits$n > 100)

#Q12
afinn <- get_sentiments("afinn")

afinn_sentiments <- word_count_no_digits %>% inner_join(afinn, by = "word")
sum(afinn_sentiments$n)
sum(afinn_sentiments$n[1:4])/sum(afinn_sentiments$n)

afinn_sentiments %>% filter( value == 4) %>% select(n) %>% sum()
