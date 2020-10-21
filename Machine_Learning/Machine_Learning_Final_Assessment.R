#install.packages(c("tidyverse","dslabs","caret","matrixStats","randomForest","lubridate"))
library(tidyverse)
library(caret)
library(dslabs)
library(matrixStats)
library(rpart)
library(randomForest)
library(lubridate)
library(RColorBrewer)

#The brca dataset from the dslabs package contains information about breast cancer diagnosis 
#biopsy samples for tumors that were determined to be either benign (not cancer) and malignant 
#(cancer). The brca object is a list consisting of

#1) brca$y: a vector of sample classifications ("B" = benign or "M" = malignant)
#2) brca$x: a matrix of numeric features describing properties of the shape and size of cell 
    #nuclei extracted from biopsy microscope images

options(digits = 3)

data(brca)

#Q1
#dimensions and properties of the dataset
#How many samples are in the dataset?
length(brca$y)
dim(brca$x)
nrow(brca$x)
#How many predictors are in the matrix?
ncol(brca$x)
#What proportion of the samples are malignant?
mean(brca$y == "M")
#Which column number has the highest mean?
which.max(colMeans(brca$x))
#Which column number has the lowest standard deviation?
which.min(colSds(brca$x))
view(brca$x)

#Q2
#Use sweep() two times to scale each column: subtract the column means of brca$x, then divide 
#by the column standard deviations of brca$x
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")

#what's the sd of scaled columns 1?
sd(x_scaled[,1])
#what's the median value of scaled col 1?
median(x_scaled[,1])

#Q3
#Calculate the distance between all samples using the scaled matrix.
d_samples <- dist(x_scaled)

#What is the average distance between the first sample, which is benign, and other benign samples?
dist_BtoB <- as.matrix(d_samples)[1, brca$y == "B"]
mean(dist_BtoB[2:length(dist_BtoB)])
#What is the average distance between the first sample and malignant samples?
dist_BtoM <- as.matrix(d_samples)[1, brca$y == "M"]
mean(dist_BtoM)

#Q4
#Make a heatmap of the relationship between features using the scaled matrix
d_features <- dist(t(x_scaled))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

#Q5
#Perform hierarchical clustering on the 30 features. Cut the tree into 5 groups.
h <- hclust(d_features)
groups <- cutree(h, k = 5)
split(names(groups), groups)

#Q6
#Perform a principal component analysis of the scaled matrix.
pca <- prcomp(x_scaled)
#What proportion of variance is explained by the first principal component?
summary(pca)    # see PC1 Cumulative Proportion
#How many principal components are required to explain at least 90% of the variance?
summary(pca)     # first value of Cumulative Proportion that exceeds 0.9: PC7

#Q7
#Plot the first two principal components with color representing tumor type (benign/malignant).

#plot tjust the first 2 principal components.
data.frame(pca$x[,1:2], type = brca$y) %>%
  ggplot(aes(PC1, PC2, color = type)) +
  geom_point()

#Q8
#Make a boxplot of the first 10 PCs grouped by tumor type.
data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()

#Q9
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

#Check that the training and test sets have similar proportions of benign and malignant 
#tumors.
mean(train_y == "B")
mean(test_y == "B")

#Q10a
#The predict_kmeans() function defined here takes two arguments - a matrix of observations 
#x and a k-means object k - and assigns each row of x to a cluster from k.
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}
#Set the seed to 3. Perform k-means clustering on the training set with 2 centers and
#assign the output to k. Then use the predict_kmeans() function to make predictions on 
#the test set.
set.seed(3, sample.kind = "Rounding")

k <- kmeans(train_x, centers = 2)
kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")
mean(kmeans_preds == test_y)

#Q10b
#What proportion of benign tumors are correctly identified?
sensitivity(factor(kmeans_preds), test_y, positive = "B")
#What proportion of malignant tumors are correctly identified?
sensitivity(factor(kmeans_preds), test_y, positive = "M")

#Q11
#Fit a logistic regression model on the training set with caret::train() 
#using all predictors. Ignore warnings about the algorithm not converging. 
#Make predictions on the test set.
train_glm <- train(train_x, train_y, method = "glm")
glm_preds <- predict(train_glm, test_x)
mean(glm_preds == test_y)

#Q12
#Train an LDA model and a QDA model on the training set. Make predictions 
#on the test set using each model.
train_lda <- train(train_x, train_y, method = "lda")
train_qda <- train(train_x, train_y, method = "qda")
lda_preds <- predict(train_lda, test_x)
qda_preds <- predict(train_qda, test_x)
mean(lda_preds == test_y)
mean(qda_preds == test_y)

#Q13
#Set the seed to 5, then fit a loess model on the training set with the caret package. 
#You will need to install the gam package if you have not yet done so. Use the default 
#tuning grid. This may take several minutes; ignore warnings. Generate predictions on 
#the test set.
set.seed(5, sample.kind = "Rounding")
#install.packages("gam")
train_loess <- train(train_x, train_y, method = "gamLoess")
loess_preds <- predict(train_loess, test_x)
mean(loess_preds == test_y)


#Q14
#
#Set the seed to 7, then train a k-nearest neighbors model on the training set using the 
#caret package. Try odd values of  𝑘  from 3 to 21. Use the final model to generate prediction
#s on the test set.
set.seed(7, sample.kind = "Rounding")
ks <- seq(3,21,2)
train_knn <- train(train_x, train_y,
                   method = "knn", 
                   tuneGrid = data.frame(k = ks)) 
train_knn$bestTune
knn_preds <- predict(train_knn, test_x)
mean(knn_preds == test_y)

#Q15
#Set the seed to 9, then train a random forest model on the training set using the caret 
#package. Test mtry values of c(3, 5, 7, 9). Use the argument importance = TRUE so that 
#feature importance can be extracted. Generate predictions on the test set.
set.seed(9, sample.kind = "Rounding")
library(Rborist)
train_rf <- train(train_x, train_y,
                  method = "rf", 
                  tuneGrid = data.frame(mtry = c(3, 5, 7, 9)),
                  importance = TRUE)
train_rf$bestTune
rf_preds <- predict(train_rf, test_x)
mean(rf_preds == test_y)
varImp(train_rf)

#Q16
#Create an ensemble using the predictions from the 7 models created in the previous 
#exercises: k-means, logistic regression, LDA, QDA, loess, k-nearest neighbors, and 
#random forest. Use the ensemble to generate a majority prediction of the tumor type 
#(if most models suggest the tumor is malignant, predict malignant).
ensemble <- cbind(glm = glm_preds == "B", lda = lda_preds == "B", qda = qda_preds == "B", loess = loess_preds == "B", rf = rf_preds == "B", knn = knn_preds == "B", kmeans = kmeans_preds == "B")

ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_preds == test_y)

models <- c("K means", "Logistic regression", "LDA", "QDA", "Loess", "K nearest neighbors", "Random forest", "Ensemble")
accuracy <- c(mean(kmeans_preds == test_y),
              mean(glm_preds == test_y),
              mean(lda_preds == test_y),
              mean(qda_preds == test_y),
              mean(loess_preds == test_y),
              mean(knn_preds == test_y),
              mean(rf_preds == test_y),
              mean(ensemble_preds == test_y))
data.frame(Model = models, Accuracy = accuracy)
