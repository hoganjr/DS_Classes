library(tidyverse)
library(caret)
library(dslabs)
library(matrixStats)

#~~~~~~DISTANCE~~~~~~

if(!exists("mnist")) mnist <- read_mnist()
set.seed(0, sample.kind = "Rounding") # if using R 3.6 or later
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

y[1:3]

x_1 <- x[1,] #this is 7
x_2 <- x[2,] #this is 7
x_3 <- x[3,] #this is 2

#distance between two numbers
sqrt(sum((x_1 - x_2)^2)) #these are closer because they're both a 7
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

#compute distance using matrix algebra
sqrt(crossprod(x_1 - x_2)) #these are closer because they're both a 7
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

#compute distance between each row
d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]

#visualize these distances
image(as.matrix(d))

#order the distance by labels
image(as.matrix(d)[order(y), order(y)]) #upper right and lower left show 2s & 7s are closer and that 2's are closer than 7s

#compute distance between predictors
d <- dist(t(x))
dim(as.matrix(d))

d_492 <- as.matrix(d)[492,] #selec pixel 492 and look at distance to all other predictors

image(1:28, 1:28, matrix(d_492, 28, 28)) #visualize distance.

#~~~~~~SECTION 4.1 ASSESMENT PART 1~~~~~~~~

library(dslabs)
data(tissue_gene_expression)
#gene expression levels of 500 genes from 189 biological samples 
#representing seven different tissues
dim(tissue_gene_expression$x)
#tissue type is stored in y
table(tissue_gene_expression$y)

#Q1
#compute the Euclidean distance between each observation and stores it in d
d <- dist(tissue_gene_expression$x)

#Q2
dim(d)
dim(as.matrix(d))
#compute distances between obs 1 & 2, 39 & 40, and 73 & 74
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

#Q3
#plot q2 result
image(as.matrix(d))

#~~~~~~~~K-NEAREST NEIGHBOR (KNN)~~~~~~~~

library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$test %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

#logistic regression
library(caret)
fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

#fit knn model
knn_fit <- knn3(y ~ ., data = mnist_27$train)

x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)

knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#~~~~~~OVERTRAINING AND OVERSMOOTHING~~~~~~~~

y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class") 
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")  
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#fit knn with k=1
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall[["Accuracy"]]

y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall[["Accuracy"]]

#fit knn with k=401
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]

#pick the k in knn
ks <- seq(3, 251, 2)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})


#pick the k that maximizes accuracy using the estimates built on the test data
ks[which.max(accuracy$test)]
max(accuracy$test)

#~~~~~~SECTION 4.1 ASSESSMENT PART 2~~~~~~~~

#Q1
data("heights")
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]     

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)
ks[which.max(F_1)]

#Q2
library(dslabs)
library(caret)
data("tissue_gene_expression")

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})

#~~~~~~~~SECTION 4.2 ASSESSMENT PART 1~~~~~~~~~~

library(tidyverse)
library(caret)

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p) #1000 X 10,000 matrix of indep random variables
colnames(x) <- paste("x", 1:ncol(x), sep = "_") #label each col as "x_#" 
y <- rbinom(n, 1, 0.5) %>% factor() #binomial distrib of n length prob=0.5, factorized

x_subset <- x[ ,sample(p, 100)] #take a random sample of 100 cols as a subset

#Q1
#use x_subset to train the model with cross validation
library(randomForest)
fit <- train(x_subset,y, method = "glm")
fit$results


#Q2
install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y) #performs a t-test on x using the y param (1 or 0)
#helps to test if predictors are better for y=0 or y=1

pvals <- tt$p.value # get p values for each column (predictor) of x

#Q3
ind <- which(pvals <= 0.01) #give the index of which cols have p-val less than 0.01
length(ind)
ind

#Q4
#redifine x_subset with index from Q3 and retest accuracy
x_subset <- x[, ind]
fit <- train(x_subset,y, method = "glm")
fit$results

#Q5
#re-run cross validation but using knn and a k sequence
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

#Q7
library(dslabs)
library(caret)
data("tissue_gene_expression")

#use the same approach as q5 but on gene expression data. Don't split the data to
#test/train sets. This is a hint that k=1 will have the best accuracy
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
fit <- train(x, y, method = "knn", tuneGrid = data.frame(k = seq(1, 7, 2)))
ggplot(fit) 


#~~~~~~~BOOTSTRAP~~~~~~~~~

n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3))) #Create a normal income distrib
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income)
m

set.seed(1, sample.kind="Rounding") 
N <- 250
X <- sample(income, N) #take N random sample from the income distrib
M<- median(X) #check the median of the sample
M #the sample median is similar to the true median

library(gridExtra)
B <- 10^5
#Monte Carlo simulation of sample distribution
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)

mean(M)
sd(M)


#creating a bootstrap sample
B <- 10^5
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

#compare the monte carlo and bootsrap sample medians
tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) + 
  geom_abline()

quantile(M, c(0.05, 0.95)) #true confidence interval
quantile(M_star, c(0.05, 0.95)) #bootstrap confidence interval

median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1) #Using CLT confidence interval is wrong

mean(M) + 1.96 * sd(M) * c(-1,1) 

mean(M_star) + 1.96 * sd(M_star) * c(-1, 1)

#~~~~~~~SECTION 4.2 ASSESSMENT PART 2~~~~~~~~~~

library(dslabs)
library(caret)
data(mnist_27)
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
#use creatResample() to create 10 bootstrap samples of mnist
indexes <- createResample(mnist_27$train$y, 10)

#Q1
#how many times do 3, 4, and 7 appear in the first index
sum(indexes$Resample01 == 3)
sum(indexes$Resample01 == 4)
sum(indexes$Resample01 == 7)

#Q2
#how many times does 3 appear in all indexes
find3 <- sapply(seq(1:10),function(x){
  sum(indexes[[x]] == 3)
})
sum(find3)

#Q3
y <- rnorm(100, 0, 1)
quantile(y, 0.75)
set.seed(1, sample.kind="Rounding") # if R 3.6 or later
B <- 10^5
#Monte Carlo simulation y distribution
M <- replicate(B, {
  X <- rnorm(100, 0, 1)
  quantile(X, 0.75)
})
mean(M) #expected value
sd(M) #standard error

#Q4
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
indexes <- createResample(y, 10)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

#Q5
#repeat Q4 but with 10,000 bootstraps vs 10
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

#~~~~~~~NAIVE BAYES'~~~~~~~

# Generating train and test set
library("caret")
data("heights")
y <- heights$height
set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# Estimating averages and standard deviations
params <- train_set %>%
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params

# Estimating the prevalence
pi <- train_set %>% summarize(pi=mean(sex=="Female")) %>% pull(pi)
pi

# Getting an actual rule
x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])
p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))

#~~~~~~~CONTROLLING PREVALENCE~~~~~~~~~~~~~~~

# Computing sensitivity
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Computing specificity
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Changing the cutoff of the decision rule
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))

# Draw plot
qplot(x, p_hat_bayes_unbiased, geom = "line") +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_vline(xintercept = 67, lty = 2)
#The inflection point of the line crosses right about at the cutoff height 66"

#~~~~~~~~~QDA AND LDA~~~~~~~~~~~~~

#QDA

# Load data
data("mnist_27")

# Estimate parameters from the data
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))

# Contour plots
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm", lwd = 1.5)

# Fit model
library(caret)
train_qda <- train(y ~., method = "qda", data = mnist_27$train)
# Obtain predictors and accuracy
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

# Draw separate plots for 2s and 7s
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm") +
  facet_wrap(~y)

#LDA

params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))
params <- params %>% mutate(sd_1 = mean(sd_1), sd_2 = mean(sd_2), r = mean(r))
train_lda <- train(y ~., method = "lda", data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]


#~~~~~~~CASE STUDY: MORE THAN 3 CLASSES~~~~~~~~~

if(!exists("mnist"))mnist <- read_mnist()

set.seed(3456, sample.kind="Rounding")
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127] 
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)

# get the quadrants
# temporary object to help figure out the quadrants
row_column <- expand.grid(row=1:28, col=1:28)
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)

# binarize the values. Above 200 is ink, below is no ink
x <- x > 200 

# cbind proportion of pixels in upper right quadrant and proportion of pixels in lower right quadrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x),
           rowSums(x[ ,lower_right_ind])/rowSums(x)) 

train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1],
                        x_2 = x[index_train,2])

test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1],
                       x_2 = x[-index_train,2])

train_set %>%  ggplot(aes(x_1, x_2, color=y)) + geom_point()

train_qda <- train(y ~ ., method = "qda", data = train_set)
predict(train_qda, test_set, type = "prob") %>% head()
predict(train_qda, test_set) %>% head()
confusionMatrix(predict(train_qda, test_set), test_set$y)$table
confusionMatrix(predict(train_qda, test_set), test_set$y)$overall["Accuracy"]
train_lda <- train(y ~ ., method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$y)$overall["Accuracy"]
train_knn <- train(y ~ ., method = "knn", tuneGrid = data.frame(k = seq(15, 51, 2)),
                   data = train_set)
confusionMatrix(predict(train_knn, test_set), test_set$y)$overall["Accuracy"]
train_set %>% mutate(y = factor(y)) %>% ggplot(aes(x_1, x_2, fill = y, color=y)) + 
    geom_point(show.legend = FALSE) + stat_ellipse(type="norm")

#~~~~~~~SECTION 4.3 ASSESSMENT~~~~~~~~~


data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

#Q1
#run LDA on the data sets created above and report accuracy (no predicting)
fit_lda <- train(x, y, method = "lda")
fit_lda$results["Accuracy"]

#Q2
#which 2 genes seem to be driving the mean (e.g. which two genes have the highest mean)
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

#Q3
#repeat exercies 1 but with QDA
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding") #if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

fit_qda <- train(x, y, method = "qda")
fit_qda$results["Accuracy"]

#Q4
#which 2 genes drive the algorithm with QDA
t(fit_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

#Q5
#predictors seem to behave the same for both groups.  The gene is either
#a high predictor for both the cerebellum AND hippocampus or a low predictor.
#scale each gene for both using LDA
fit_lda_center <- train(x, y, method = "lda", preProcess = "center")
fit_lda_center$results["Accuracy"]
t(fit_lda_center$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(predictor_name, hippocampus)) +
  geom_point() +
  coord_flip()

#Q6
#perform the analysis from Q5 but using all tissue types
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
fit_lda_center <- train(x, y, method = "lda", preProcess = "center")
fit_lda_center$results["Accuracy"]
confusionMatrix(fit_lda_center)
