#install.packages(c("tidyverse","dslabs","caret","matrixStats","randomForest","lubridate"))
library(tidyverse)
library(caret)
library(dslabs)
library(matrixStats)
library(rpart)
library(randomForest)
library(lubridate)

#~~~~~~~~CASE STUDY: MNIST~~~~~~~~
#We will apply what we have learned in the course on the Modified National 
#Institute of Standards and Technology database (MNIST) digits, a popular 
#dataset used in machine learning competitions. 

library(dslabs)
mnist <- read_mnist()

names(mnist)
dim(mnist$train$images) #60k images of 784 pixels each

class(mnist$train$labels)
table(mnist$train$labels)

# sample 10k rows from training set, 1k rows from test set
set.seed(123, sample.kind = "Rounding")
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
#note that the line above is the corrected code - code in video at 0:52 is incorrect
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])


#~~~~~~~~PRE-PROCESSING MNIST DATA~~~~~~~~
library(matrixStats)
sds <- colSds(x) #calculate the std dev of each col in x
qplot(sds, bins = 256, color = I("black")) #plot the std dev. We see that many pixels 
#have almost no variation (they're always blank).

library(caret)
nzv <- nearZeroVar(x) #caret fxn that selects features that have near zero variance and
#should be removed
image(matrix(1:784 %in% nzv, 28, 28)) #red areas are recommended for removal

col_index <- setdiff(1:ncol(x), nzv) #identify cols for removal
length(col_index)

#~~~~~~~~MODEL FITTING FOR MNIST DATA~~~~~~~~~~
colnames(x) <- 1:ncol(mnist$train$images) #set col names to col num
colnames(x_test) <- colnames(x) #set col names to col num

#start with kNN. However calculating distance for all observations would
#take a long time.  Therefore you k-fold cross validation
control <- trainControl(method = "cv", number = 10, p = .9) #define cross validation control
#10 splits of 10% each
train_knn <- train(x[,col_index], y,
                   method = "knn", 
                   tuneGrid = data.frame(k = c(1,3,5,7)),
                   trControl = control) #train the knn model with the removed features index
ggplot(train_knn)

#the code above took a while (several minutes) to run.  Let's take a smaller sample
#to test our code first.  
n <- 1000 #number of rows of x we'll start with
b <- 2 #number of cross validation folds we'll use.
index <- sample(nrow(x), n) #randomly select n rows of x
control <- trainControl(method = "cv", number = b, p = .9) #control for b folds
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control) #this runs much faster and we can slowly increase n and b
#now we can fit the entire data set
fit_knn <- knn3(x[ ,col_index], y,  k = 3)
#now we test the fit
y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

cm$byClass[,1:2] #show how we performed for each digit (class).

#try using random forest for better accuracy
library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
train_rf <-  train(x[, col_index], y,
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune

fit_rf <- Rborist(x[, col_index], y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

rafalib::mypar(3,4)
for(i in 1:12){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste("Our prediction:", y_hat_rf[i]),
        xaxt="n", yaxt="n")
}

#~~~~~~~~~~~VARIABLE IMPORATANCE~~~~~~~~~

library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y,  ntree = 50) #run random forest with all features and 50 trees
imp <- importance(rf) #lookup importance of each pixel
imp

image(matrix(imp, 28, 28))

p_max <- predict(fit_knn, x_test[,col_index]) #test our knn fit
p_max <- apply(p_max, 1, max) #find the max of each row in p_max
ind  <- which(y_hat_knn != y_test) #find where we got it wrong
ind <- ind[order(p_max[ind], decreasing = TRUE)]
#plot where we were wrong
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1],
        main = paste0("Pr(",y_hat_knn[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

#repeat with rf
p_max <- predict(fit_rf, x_test[,col_index])$census  
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_rf != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

#~~~~~~~~~~~~ENSEMBLES~~~~~~~~~~

p_rf <- predict(fit_rf, x_test[,col_index])$census
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)

#~~~~~~~~SECTION 6.1 COMPREHENSION CHECK~~~~~~~~

#Q1
#test out the 10 most commonly used algorithms
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

#use train() for all the models using the code below
library(caret)
library(dslabs)
library(tidyverse)

set.seed(1, sample.kind = "Rounding")
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

#Q2
#use sapply() or map() to create a matrix of predictions for the test set. 

pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))#, simplify = "matrix")
dim(pred)

#Q3
#compute the accuracy for each model, then the average of all
acc <- colMeans(pred == mnist_27$test$y) #determine the accuracy of model (column)
acc
mean(acc)

#Q4
#build an ensemble of all models. Vote 7 if more than half the models predict 7.

votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

#Q5
#which individual models are more accurate than the model?
ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]

#Q6
#consider dropping some of the lower performing models. You have to do this based on
#training and not test accuracy.  Look at the fit accuracy of each training model.

acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

#Q7
#consider only the methods with an estimated accuracy>0.8 and repeat Q4 with only those models.
ind <- acc_hat >= 0.8 #determine which are greater than 0.8 accurate
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)

#~~~~~~~~RECOMMENDATION SYSTEMS~~~~~~~~

library(tidyverse)
library(dslabs)
data("movielens")
head(movielens)

movielens %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId)) #unique users and unique movies
keep <- movielens %>%
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)
tab <- movielens %>%
  filter(userId %in% c(13:20)) %>% 
  filter(movieId %in% keep) %>% 
  select(userId, title, rating) %>% 
  spread(title, rating)
tab %>% knitr::kable() #table showing reviews for 5 movies by 7 users

users <- sample(unique(movielens$userId), 100)
rafalib::mypar()
movielens %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey") #yellow squares represent unique reviews for each movie


movielens %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies") #most movies are review once

movielens %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users") #some users review far more movies than others

library(caret)
set.seed(755, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, 
                                  list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

#To make sure we don’t include users and movies in the test set that do 
#not appear in the training set, we remove these entries using the semi_join function:
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
#test the losses using the RMSE funciton
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#~~~~~~~~BUILDING THE RECOMMENDATION SYSTEM~~~~~~~~~~

mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

predictions <- rep(3, nrow(test_set))
RMSE(test_set$rating, predictions)

rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse) #create a 
#table to store different RMSE results

#because there are 1000s of b's we'll skip running this for time/crashing R threats.
#fit <- lm(rating ~ as.factor(movieId), data = movielens)
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu)) #b_i is the avg of the rating - the avg

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))
#b_i = 1.5 means a 5 star rating because the average is 3.5
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

rmse_results %>% knitr::kable()

train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black") #avg rating from user u that has >100 ratings logged

# lm(rating ~ as.factor(movieId) + as.factor(userId))
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i)) #user effect is counter balenced by average rating

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
RMSE(predicted_ratings, test_set$rating)

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()


#~~~~~~~~SECTION 6.2 COMPREHENSION CHECK~~~~~~~~~~
library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

#Q1
#Compute the number of ratings for each movie and then plot it 
#against the year the movie came out. Use the square 
#root transformation on the counts.
#What year has the highest median number of ratings?
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Q2
#Among movies that came out in 1993 or later, select the top 25 movies 
#with the highest average number of ratings per year (n/year), and caculate 
#the average rating of each of them. To calculate number of ratings per year, 
#use 2018 as the end year.
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))

#Q3
#stratify the post-1993 movies by ratings per year and compute their average ratings
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

#Q5
#create a new column in movielens called date that pulls the date from the timestamp
movielens <- mutate(movielens, date = as_datetime(timestamp))

#Q6
#Compute the average rating for each week and plot this average against date.
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

#Q8
#determine which movie genre has the lowest average rating.
movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#~~~~~~~~~~REGULARIZATION~~~~~~~~~~~

library(dslabs)
library(tidyverse)
library(caret)
data("movielens")
set.seed(755, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]


test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
mu <- mean(train_set$rating) 

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
       left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
#we were only able to improve the RMSE 5% w/ the movie to movie model
#here are the 10 biggest mistakes
test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  slice(1:10) %>% 
  pull(title)
#top 10 best and worst movies based on estimates of the movie effect
movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10)  %>% 
  pull(title)

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10)  %>% 
  pull(title)
#same as above but listing how often each movie was rated
train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(title, n)

train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10) %>% 
  pull(title, n)

#computing the regularized estimates with lambda = 3
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

tibble(original = movie_avgs$b_i, 
       regularlized = movie_reg_avgs$b_i, 
       n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

#look at best/worst movies based on regularization results
train_set %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(title)

train_set %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  pull(title)

#calculate new RMSE with regularization
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
RMSE(predicted_ratings, test_set$rating)

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

#lambda is tunable and we can use cross validation to choose it
lambdas <- seq(0, 10, 0.25)
mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

#we can also take this approach on the user effect
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

#~~~~~~~COMPREHENSION CHECK: REGULARIZATION~~~~~~~~~

options(digits=7)
#An education expert is advocating for smaller schools. The expert bases this 
#recommendation on the fact that among the best performing schools, many are small 
#schools. Let's simulate a dataset for 1000 schools. First, let's simulate the 
#number of students in each school, using the following code:
set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))
#Now let's assign a true quality for each school that is completely independent from 
#size. This is the parameter we want to estimate in our analysis. The true quality 
#can be assigned using the following code:
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
#the top schools:
schools %>% top_n(10, quality) %>% arrange(desc(quality))
#Now let's have the students in the school take a test. There is random variability in
#test taking, so we will simulate the test scores as normally distributed with the 
#average determined by the school quality with a standard deviation of 30 percentage 
#points. This code will simulate the test scores:
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#Q1
#What are the top schools based on the average score? Show just the ID, size, and 
#the average score.
schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)

#Q2
#Compare the median school size to the median school size of the top 10 schools 
#based on the score.
median(schools$size)
schools %>% top_n(10, score) %>% .$size %>% median()

#Q3
#it appears that smaller schools have better scores BUT we created a data set
#that makes size and score independant.  Check the median size of the bottom 10
schools %>% top_n(-10, score) %>% .$size %>% median()

#Q4
#Plot the average score versus school size to see what's going on. Highlight the 
#top 10 schools based on the true quality.
schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)

#Q5
#lets regularize to pick the best schools

#first we need to define the overall average for all schools
overall <- mean(sapply(scores, mean))
#Write code that estimates the score above the average for each school but dividing by
#n+alpha instead of n, where n is school size and alpha is regularizatio param. 
#try alpha = 25
alpha <- 25
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

#Q6
#Using values of alpha  from 10 to 250, find the  𝛼  that minimizes the RMSE.

alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]

#Q7
#Rank the schools based on the average obtained with the best alpha from Q6
alpha <- alphas[which.min(rmse)]  
score_reg <- sapply(scores, function(x)
  overall+sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

#Q8
#A common mistake made when using regularization is shrinking values towards 0 
#that are not centered around 0. For example, if we don't subtract the overall 
#average before shrinking, we actually obtain a very similar result. Confirm this 
#by re-running the code from the exercise in Q6 but without removing the overall mean.
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
  })
plot(alphas, rmse)
alphas[which.min(rmse)]

#~~~~~~~~~MATRIX FACTORIZATION~~~~~~~~~~~
#the model until now leaves out an important characteristic. Groups of movies have similar
#ratings patterns and groups of users have similar ratings patterns.
#we will select a small subset of data and convert to a matrix so that each user gets a col
#an each movie gets a row
train_small <- movielens %>% 
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% #3252 is Scent of a Woman used in example
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>% 
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

#add row and column names
rownames(y)<- y[,1]
y <- y[,-1]

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])
#convert to residuals by removing the col and row means
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))

#compare residuals of godfather movies, strong correlation
m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)
#similar correlation between godfather and goodfellas
m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)
#same pattern between these 2 similar films
m_4 <- "You've Got Mail" 
m_5 <- "Sleepless in Seattle" 
qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)
#correlation between each title shows positive between gangster films, neg between gangster and rom/com
cor(y[, c(m_1, m_2, m_3, m_4, m_5)], use="pairwise.complete") %>% 
  knitr::kable()

#we can create a q and p vector summarizing the correlation characteristics
set.seed(1,sample.kind = "Rounding")
options(digits = 2)
#q vector is 1 for gangster movies and -1 for  rom/com
Q <- matrix(c(1 , 1, 1, -1, -1), ncol=1)
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
#p vector is 2 for likes gangster movies, -2 for doesn't like, 0 for doesn't care
P <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol=1)
rownames(P) <- 1:nrow(P)
#table of factors?
X <- jitter(P%*%t(Q))
X %>% knitr::kable(align = "c")
#even though this P and Q vector was generate by us, the correlation is similar
cor(X)

t(Q) %>% knitr::kable(aling="c")

P
#now let's add Scent of a Woman film
set.seed(1, sample.kind = "Rounding")
options(digits = 2)
m_6 <- "Scent of a Woman"
#now we have to account for does or does not like Al Pacino in Q with a second row
Q <- cbind(c(1 , 1, 1, -1, -1, -1), 
           c(1 , 1, -1, -1, -1, 1))
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)

P <- cbind(rep(c(2,0,-2), c(3,5,4)), 
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
rownames(P) <- 1:nrow(X)

X <- jitter(P%*%t(Q), factor=1)
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(align="c")

P

six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
tmp <- y[,six_movies]
cor(tmp, use="pairwise.complete")

#~~~~~~~~~SVD AND PCA~~~~~~~~~~~~~~~

#to compute the decomposition make the residuals with NAs = 0
y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)

#q vectors are called principal component vectors they are stored in this matrix
dim(pca$rotation)
#While the p, or the user effects, are here
dim(pca$x)
#we can see the variability of each of these vectors
qplot(1:nrow(pca$x), pca$sdev, xlab = "PC")
plot(pca$sdev)
#the variability decreases with the terms
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

library(ggrepel)
#make the principal component matrix
pcs <- data.frame(pca$rotation, name = colnames(y))
#plot tjust the first 2 principal components.
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))
#we see from the plot that, in the extremes PC1 seems to have blockbusters on one side and critically
#acclaimed movies on the other
pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)

pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)

#from the plot we can also see that PC2 has "structure" in the data. One side seems to have indepdendent
#films while the other has "nerd" favorites.
pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)

#~~~~~~~~~~~COMPREHENSION CHECK: MATRIX FACTORIZATION~~~~~~~~~~~

#we will construct a dataset that represents grade scores for 100 students in 24 different subjects.
#The overall average has been removed so this data represents the percentage point each student received 
#above or below the average test score. So a 0 represents an average grade (C), a 25 is a high grade 
#(A+), and a -25 represents a low grade (F). You can simulate the data like this:
set.seed(1987, sample.kind = "Rounding")
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

#Q1
#you can visualize the scores like this:
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

#Q2
#you can examine the correlation between the test scores directly like this:
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#Q3
#Use the function svd() to compute the SVD of y. This function will return 
#U, V and the diagonal entries of D
s <- svd(y)
names(s)
#check the svd works:
y_svd <- s$u %*% diag(s$d) %*% t(s$v) # this is UDt(V) 
max(abs(y - y_svd)) #should get zero or nearly zero

#Compute the sum of squares of the columns of Y and store them in ss_y. Then
#compute the sum of squares of columns of the transformed YV  and store them in ss_yv
#. Confirm that sum(ss_y) is equal to sum(ss_yv).
ss_y <- apply(y^2, 2, sum)
ss_yv <- apply((y%*%s$v)^2, 2, sum)
sum(ss_y)
sum(ss_yv)

#Q4
#plot ss_y vs col num and ss_yv vs col num
plot(ss_y)
plot(ss_yv)

#Q5
#Now notice that we didn't have to compute ss_yv because we already have the answer. How? 
#Remember that YV = UD and because U is orthogonal, we know that the sum of squares of the columns 
#of UD are the diagonal entries of D squared. Confirm this by plotting the square root of ss_yv 
#versus the diagonal entries of D.

#plot the sqrt of ss_yv vs the diagonal entries of D
data.frame(x = sqrt(ss_yv), y = s$d) %>%
  ggplot(aes(x,y)) +
  geom_point()
s$u

#Q6
#From the above we know that the sum of squares of the columns of Y (the total sum of squares) add 
#up to the sum of s$d^2 and that the transformation YV gives us columns with sums of squares equal 
#to s$d^2. Now compute what percent of the total variability is explained by just the first three 
#columns of YV
sum(s$d[1:3]^2) / sum(s$d^2)

#Q7
#Use the sweep function to compute UD without constructing diag(s$d) or using matrix multiplication.
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

#Q8
#Compute the average score for each student, plot it against U_1*d_1_1.
plot(s$u[,1]*s$d[1], rowMeans(y))

#Q9
#Make an image plot of V and describe the first column relative to others and how this relates to 
#taking an average.
my_image(s$v)

#Q10
#plot U_1, then plot t(V_1) using the same y-axis lims.  Then make an image of U_1*d_1_1*t(V_1)
plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
my_image(y)

#Q11
#In the exercise in Q6, we saw how to calculate the percent of total variability explained. However, 
#our approximation only explains the observation that good students tend to be good in all subjects. 
#Another aspect of the original data that our approximation does not explain was the higher similarity 
#we observed within subjects. We can see this by computing the difference between our approximation 
#and original data and then computing the correlations. You can see this by running this code:
resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#Now that we have removed the overall student effect, the correlation plot reveals that we have not 
#yet explained the within subject correlation nor the fact that math and science are closer to each 
#other than to the arts. So let's explore the second column of the SVD.

#remake the plots from q10 but with column 2 instead of column 1 and compare to resid image
plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))
my_image(resid)

#Q12

#The second column clearly relates to a student's difference in ability in math/science versus the arts. 
#We can see this most clearly from the plot of s$v[,2]. Adding the matrix we obtain with these two 
#columns will help with our approximation:

#We know it will explain sum(s$d[1:2]^2)/sum(s$d^2) * 100 percent of the total variability. We can 
#compute new residuals like this:
resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#and see that the structure that remains is driven by the difference b/w math and science. confirm this 
#by plotting the 3rd col and comparing to the resid image
plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])))
my_image(resid)

#Q13
#The third column clearly relates to a student's difference in ability in math and science. We can see 
#this most clearly from the plot of s$v[,3]. Adding the matrix we obtain with these two columns will help
#with our approximation.We know it will explain: sum(s$d[1:3]^2)/sum(s$d^2) * 100 percent of the 
#total variability. We can compute new residuals like this
resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
#We no longer see structure in the residuals: they seem to be independent of each other. This implies 
#that we can describe the data with the 3 columns plus an additional independent identically 
#distributed errors to account for the last variability.

#This model is useful because we summarize of  100×24 observations with  3×(100+24+1)=375  numbers.
#Furthermore, the three components of the model have useful interpretations:
#1 - the overall ability of a student
#2 - the difference in ability between the math/sciences and arts
#3 - the remaining differences between the three subjects.

#plot an image of Y, an image of the 3 principal compnent model, and an image of the residuals
y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(y, zlim = range(y))
my_image(y_hat, zlim = range(y))
my_image(y - y_hat, zlim = range(y))

#~~~~~~~~~~~COMPREHENSION CHECK: DIMENSION REDUCTION~~~~~~~~~~~
#We want to explore the tissue_gene_expression predictors by plotting them
data("tissue_gene_expression")
dim(tissue_gene_expression$x)

#Q1
#We want to get an idea of which observations are close to each other, but, as you can see from 
#the dimensions, the predictors are 500-dimensional, making plotting difficult. Plot the first 
#two principal components with color representing tissue type.
pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#Q2
#The predictors for each observation are measured using the same device and experimental procedure. 
#This introduces biases that can affect all the predictors from one observation. For each observation, 
#compute the average across all predictors, and then plot this against the first PC with color 
#representing tissue. Report the correlation.
avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()
cor(avgs, pc$x[,1])

#Q3
#We see an association with the first PC and the observation averages. Redo the PCA but only after 
#removing the center.
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#Q4
#For the first 10 PCs, make a boxplot showing the values for each tissue.
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

#Q5
#Plot the percent variance explained by PC number. Hint: use the summary function.
plot(summary(pc)$importance[3,])

#~~~~~~~~~COMPREHENSION CHECK: CLUSTERING~~~~~~~~~~

data("tissue_gene_expression")
#Q1
#Load the tissue_gene_expression dataset. Remove the row means and compute the 
#distance between each observation. Store the result in d
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))   

#Q2
#Make a hierarchical clustering plot and add the tissue types as labels.
h <- hclust(d)
plot(h)

#Q3
#Select the 50 most variable genes. Make sure the observations show up in the columns, 
#that the predictor are centered, and add a color bar to show the different tissue types. 
#Hint: use the ColSideColors argument to assign colors. Also, use 
#col = RColorBrewer::brewer.pal(11, "RdBu") for a better use of colors.
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = rev(colors))

