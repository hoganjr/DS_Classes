##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

#substite "::" for tab in ratings set
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

#split the moves file into 3 cols using "::" as the splitter
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
#create movies df and join with ratings 
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
#delete temp files used to create edx and validation sets
rm(dl, ratings, movies, test_index, temp, movielens, removed)

##########################################################
# QUIZ: MOVIELENS DATASET
##########################################################

#Q1
#How many rows and columns are there in the edx dataset?
dim(edx)

#Q2
#How many zeros were given as ratings in the edx dataset?
sum(edx$rating == "0")
#How many threes were given as ratings in the edx dataset?
sum(edx$rating == "3")

#Q3
#How many different movies are in the edx dataset?
n_distinct(edx$movieId)

#Q4
#How many different users are in the edx dataset?
n_distinct(edx$userId)

#Q5
#How many movie ratings are in each of the following genres in the edx dataset?
sum(stringr::str_detect(edx$genres,"Drama"))
sum(stringr::str_detect(edx$genres,"Comedy"))
sum(stringr::str_detect(edx$genres,"Thriller"))
sum(stringr::str_detect(edx$genres,"Romance"))

#Q6
#Which movie has the greatest number of ratings?
edx %>% group_by(title) %>% summarize(n = n()) %>% arrange(desc(n))

#Q7
#What are the five most given ratings in order from most to least?
edx %>% group_by(rating) %>% summarize(n = n()) %>% arrange(desc(n))

#Q8
#True or False: In general, half star ratings are less common than whole 
#star ratings (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.).
edx %>% group_by(rating) %>% summarize(n = n()) %>% arrange(desc(n))

##########################################################
# MOVIE RECOMMENDATION SYSTEM 
##########################################################

#~~~~~~~~DATA CLEANING/PROCESSING/VISUALIZATION~~~~~~~~~~~~
library(dslabs)

#looking for NAs
sum(is.na(edx)) #shows no NAs in the data


#How many different movies are in the edx dataset?
n_distinct(edx$movieId) #10677

#How many different users are in the edx dataset?
n_distinct(edx$userId) #69878

#visualizing some of the data to gain insight
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies") #most movies have less than 1k reviews

edx %>%
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users") #most users review less than 100 movies


#most common movie ratings
qplot(edx$rating, data = edx, bins = 10, color = I("black")) #4 is the most common rating
edx %>% group_by(rating) %>% summarize(n = n()) %>% arrange(desc(n)) #4 is most common

#number of ratings by movie
edx %>% group_by(title) %>% summarize(n = n()) %>% arrange(desc(n))
#the top 10 seem to be mostly blockbusters/action 

#highest ratings
edx %>% group_by(title) %>% summarize (n = n(), avg_rating = mean(rating)) %>% arrange(desc(avg_rating))
#the top ratings appear to be small, independent films with very few ratings

#based on intuition genre and movie age could also have an impact on rating.  
#splitting the each movie into separate entries by genre.
edx_genres <- edx %>% separate_rows(genres, sep = "\\|")
edx_genres %>% group_by(genres) %>% 
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#The age of a movie can also have an impact on ratings. Contemporary society views change
#and older movies can reflect views of different generations.  Let's see if
#release year has an effect on ratings.
library(stringr)
edx_genres <- edx_genres %>% 
  mutate(release_year = as.numeric(str_extract_all(title, "(?<=\\()\\d{4}(?=\\))")))
#look at average moving rating by release year
edx <- edx %>% mutate(release_year = as.numeric(str_extract_all(title, "(?<=\\()\\d{4}(?=\\))")))
edx %>% group_by(release_year) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(release_year, avg_rating)) + geom_point() + geom_smooth()

#look at number of movies per genre by release year
edx_genres %>% 
  filter(genres == c("Action", "Comedy", "Drama", "Romance")) %>%
  group_by(release_year, genres) %>%
  summarize(avg_rating = mean(rating), count = n()) %>%
  ggplot(aes(release_year, count, col = genres)) + geom_line()

#~~~~~~~~~~BUILDING A MODEL~~~~~~~~~

#first we need to divide the edx set in to train/test sets.  Giving 20% to test
edx_test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, 
                                  list = FALSE)
edx_train_set <- edx[-edx_test_index,]
edx_test_set <- edx[edx_test_index,]

#To make sure we don’t include users and movies in the test set that do 
#not appear in the training set, we remove these entries using the semi_join function:
edx_test_set <- edx_test_set %>% 
  semi_join(edx_train_set, by = "movieId") %>%
  semi_join(edx_train_set, by = "userId")

#making genre/yr version of the test/train sets for later on
edx_train_genres <- edx_train_set %>% separate_rows(genres, sep = "\\|")
edx_train_genres <- edx_train_genres %>% 
  mutate(release_year = as.numeric(str_extract_all(title, "(?<=\\()\\d{4}(?=\\))")))
edx_test_genres <- edx_test_set %>% separate_rows(genres, sep = "\\|")
edx_test_genres <- edx_test_genres %>%
  mutate(release_year = as.numeric(str_extract_all(title, "(?<=\\()\\d{4}(?=\\))")))

dim(validation %>% filter(movieId %in% edx_train_set$movieId))
#it appears that we can create a training set which doesn't
#match the validation set for users/movies. We'll need to retrain
#the entire edx set at the end so that all movies/users are included.

#define the RMSE function for testing losses
RMSE <- function(actual_ratings, pred_ratings){
  sqrt(mean((actual_ratings - pred_ratings)^2))
}


#start simple with predicting based off the mean movie rating. Y = mu + eps
mean_rating <- mean(edx_train_set$rating)
rmse_mean_rating <- RMSE(edx_test_set$rating, mean_rating)
rmse_mean_rating #got a not so good RMSE of 1.05
sd(edx_train_set$rating)
mean_rating #comparing the sd to the mean shows this model is 1 standard dev off
rmse_results <- data_frame(method = "Just the average", RMSE = rmse_mean_rating)

#from the data inspection it's clear that some movies get more ratings and higher ratings than others
#trying to account for the movie effect. Y = mu + b_i + eps
movie_avgs <- edx_train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mean_rating)) #calculate the LSE b_i
qplot(b_i, data = movie_avgs, bins = 10, color = I("black")) #showing the variation of b_i
#-3 b_i means a 0.5 star rating because the mean is 3.5 stars

#make new prediction with the movie effect
pred_movie_eff <- mean_rating + edx_test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i) #add a b_i column to the test set that's organized by movieId
rmse_mov_only <- RMSE(pred_movie_eff, edx_test_set$rating) #improved RMSE of 0.9428
rmse_mov_only
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = rmse_mov_only))
rmse_results %>% knitr::kable()

#People generally have movie preferences so the model should account for user differences.
#This is clear from plotting the average user rating (minimum 100 ratings). 
edx_train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black") #some love everything and some hate everything.

#Create a new linear model of Y = mu + b_i + b_u + eps
#b_u calculated similarly to b_i
user_avgs <- edx_train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mean_rating - b_i))

#make new prediction with the user effect added
pred_user_eff <- edx_test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mean_rating + b_i + b_u) %>%
  pull(pred)
rmse_mov_user <- RMSE(pred_user_eff, edx_test_set$rating) #improved RMSE of 0.8655
rmse_mov_user
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effect Model",
                                     RMSE = rmse_mov_user ))
rmse_results %>% knitr::kable()
#Create a new linear model of Y = mu + b_i + b_u + b_y + eps
#f_gen calculated similarly to b_i
rel_year_avgs <- edx_train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  group_by(release_year) %>%
  summarize(b_y = mean(rating - mean_rating - b_i - b_u))

#make new prediction with the release year effect added
pred_rel_year_eff <- edx_test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(rel_year_avgs, by = 'release_year') %>%
  mutate(pred = mean_rating + b_i + b_u + b_y) %>%
  pull(pred) 
rmse_mov_user_yr <- RMSE(pred_rel_year_eff, edx_test_set$rating) #improved RMSE of 0.8655
rmse_mov_user_yr
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Year Effect Model",
                                     RMSE = rmse_mov_user_yr ))
rmse_results %>% knitr::kable()

#Create a new linear model of Y = mu + b_i + b_u + b_y + b_g + eps
#f_gen calculated similarly to b_i
genre_avgs <- edx_train_genres %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(rel_year_avgs, by = "release_year") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mean_rating - b_i - b_u - b_y))

#make new prediction with the release year effect added
pred_genre_eff <- edx_test_genres %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(rel_year_avgs, by = 'release_year') %>%
  left_join(genre_avgs, by = "genres") %>%
  mutate(pred = mean_rating + b_i + b_u + b_y + b_g) %>%
  pull(pred) 
rmse_mov_user_yr_gen <- RMSE(pred_genre_eff, edx_test_genres$rating) #improved RMSE of 0.8655
rmse_mov_user_yr_gen
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Year + Genre Effect Model",
                                     RMSE = rmse_mov_user_yr_gen ))
rmse_results %>% knitr::kable()
#The RMSE has improved from 1.06 to 0.8655. To see if we can improve upon this let's see 
#where the movie effect model is wrong and the user effect model is wrong. This is done
#by calculating the residuals of each prediction.
edx_train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(resid = rating - (mean_rating + b_i)) %>%
  arrange(desc(abs(resid))) %>% 
  slice(1:10) %>%
  pull(title)
#top 10 best and worst movies based on estimates of the movie effect
movie_titles <- edx_train_set %>% 
  select(movieId, title) %>%
  distinct()

#look at top 10 best and worst movies
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10)  %>% 
  pull(title)

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10)  %>% 
  pull(title)
#these movies are all quite obscure. Looking to see how often they were rated.
edx_train_set %>% count(movieId) %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(n)
#>  [1] 1 2 1 1 1 1 1 6 4 4

edx_train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10) %>% 
  pull(n)
#> Joining, by = "movieId"
#>  [1] 2   1   1   1   2   2   3  43 149  28

#let's repeat this process for users
users_uniq <- edx_train_set %>% 
  select(userId, title) %>%
  distinct()
#look at top 10 best and worst user effects
user_avgs %>% left_join(users_uniq, by="userId") %>%
  arrange(desc(b_u)) %>% 
  slice(1:10)  %>% 
  pull(title)

user_avgs %>% left_join(users_uniq, by="userId") %>%
  arrange(b_u) %>% 
  slice(1:10)  %>% 
  pull(title)

#regularizing for on each effect
lambdas <- seq(1, 7, 0.25)
rmses <- sapply(lambdas, function(l){
  b_i <- edx_train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mean_rating)/(n()+l))
  b_u <- edx_train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mean_rating)/(n()+l))
  b_y <- edx_train_set %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(release_year) %>%
    summarize(b_y = sum(rating - mean_rating - b_i - b_u)/(n()+l))
  b_g <- edx_train_genres %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_y, by = 'release_year') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - b_y - mean_rating)/(n()+l))
  predicted_ratings <- 
    edx_test_genres %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_y, by = 'release_year') %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mean_rating + b_i + b_u + b_y + b_g) %>%
    pull(pred)
  return(RMSE(predicted_ratings, edx_test_genres$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

#running regularized prediction with optimized lambda
movie_reg_avgs <- edx_train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mean_rating)/(n()+lambda), n_i = n()) 
user_reg_avgs <- edx_train_set %>%  
  left_join(movie_reg_avgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mean_rating - b_i)/(n()+lambda), n_u = n())
rel_year_reg_avgs <- edx_train_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_reg_avgs, by = 'userId') %>%
  group_by(release_year) %>%
  summarize(b_y = sum(rating - mean_rating - b_i - b_u)/(n()+lambda), n_y = n())
genre_reg_avgs <- edx_train_genres %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_reg_avgs, by = 'userId') %>%
  left_join(rel_year_reg_avgs, by = "release_year") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mean_rating - b_i - b_u - b_y)/(n()+lambda), n_g = n())
predicted_full_reg_ratings <- edx_test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_reg_avgs, by = 'userId') %>%
  left_join(rel_year_reg_avgs, by = "release_year") %>%
  left_join(genre_reg_avgs, by = 'genres')
  mutate(pred = mean_rating + b_i + b_u + b_y + b_g) %>%
  pull(pred)
rmse_full_reg <- RMSE(predicted_full_reg_ratings, edx_test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User + Year + Genre Effect Model",  
                                     RMSE = rmse_full_reg))
rmse_results %>% knitr::kable()
##########################################################
# PREDICTION & RMSE ON VALIDATION SET
##########################################################
#using the entire edx set
#lambda <- 4.0
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mean_rating)/(n()+lambda))
b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mean_rating)/(n()+lambda))
b_y <- edx %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(release_year) %>%
  summarize(b_y = sum(rating - b_i - b_u - mean_rating)/(n()+lambda))
b_g <- edx_genres %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by = "release_year") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i - b_u - b_y - mean_rating)/(n()+lambda))

validation_genres <- validation %>% separate_rows(genres, sep = "\\|")
validation_genres <- validation_genres %>% 
  mutate(release_year = as.numeric(str_extract_all(title, "(?<=\\()\\d{4}(?=\\))"))) 
  

pred_val_set <- validation_genres %>%
  left_join(b_i, by = 'movieId') %>%
  left_join(b_u, by = 'userId') %>%
  left_join(b_y, by = 'release_year') %>%
  left_join(b_g, by = 'genres') %>%
  mutate(predic = mean_rating + b_i + b_u + b_y + b_g) %>%
  pull(predic)

RMSE(pred_val_set, validation_genres$rating)
dim(pred_val_set)
head(pred_val_set)
length(pred_val_set)
sum(is.na(pred_val_set))
sum(is.na(validation_genres$rating))

