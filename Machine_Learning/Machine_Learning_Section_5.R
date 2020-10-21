#install.packages(c("tidyverse","dslabs","caret","matrixStats","randomForest"))
library(tidyverse)
library(caret)
library(dslabs)
library(matrixStats)
library(rpart)
library(randomForest)
#~~~~~~~~~~~CLASSIFICATION AND REGRESSION TREES (CART)~~~~~~~~~~~

# Load data
#library(tidyverse)
#library(dslabs)
data("olive")
olive %>% as_tibble()
table(olive$region)
olive <- select(olive, -area)

# Predict region using KNN
#library(caret)
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = olive)
ggplot(fit)

# Plot distribution of each predictor stratified by region
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank())
#plot shows that eicosenoic only appears in Southern Italy (good predictor then)
#and shows that linoleic can be good predictor for Sardinia/Northern Italy

# plot values for eicosenoic and linoleic
p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
p + geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)
#plot shows that we can select southern italy for eicosenoic > 0.065 and use linoleic = 10.54 for other 2 regions


# load data for regression tree
data("polls_2008")
qplot(day, margin, data = polls_2008)

library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)

# visualize the splits on the regression tree
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# change parameters
fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
#setting cp = 0 essentially means every observation becomes a partition
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# use cross validation to choose cp
#library(caret)
train_rpart <- train(margin ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = polls_2008)
ggplot(train_rpart)

# access the final model and plot it
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# prune the tree 
pruned_fit <- prune(fit, cp = 0.01)

#~~~~~~CLASSIFICATION (DECISION) TREES~~~~~~~~~~

# fit a classification tree and plot it
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
plot(train_rpart)

# compute accuracy
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]


#~~~~~~~~RANDOM FORESTS~~~~~~~~~~

library(randomForest)
fit <- randomForest(margin~., data = polls_2008) 
plot(fit)

polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")

library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# use cross validation to choose parameter
train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

#~~~~~~~~~~SECTION 5.1 COMPREHENSION CHECK~~~~~~~~~~

library(rpart)
n <- 1000
sigma <- 0.25

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

#Q1
#use rpart() to correctly fit a regression tree
fit <- rpart(y ~ ., data = dat)

#Q2
#plot the tree from Q1
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

#Q3
#make a scatter plot of y versus x along with the predicted values based on the fit.
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

#Q4
#make the plot from Q3 but using randomForests instead
library(randomForest)
fit <- randomForest(y ~ x, data = dat)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

#Q5
# Use the plot() function to see if the Random Forest from 
#Q4 has converged or if we need more trees.
plot(fit)

#Q6
# the Random Forest result in Q4/5 an estimate that is too flexible (unsmooth). 
#Re-run the Random Forest but this time with a node size of 50 and a
#maximum of 25 nodes. Remake the plot.

fit <- randomForest( y ~ x, data = dat, nodesize = 50, maxnodes = 25)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

#~~~~~~~~~~~~~CARET PACKAGE~~~~~~~~~
#https://topepo.github.io/caret/available-models.html
#https://topepo.github.io/caret/train-models-by-tag.html

library(tidyverse)
library(dslabs)
data("mnist_27")

library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

#~~~~~~~~TUNING PARAMETERS WITH CARET~~~~~~~~~~~

getModelInfo("knn")
modelLookup("knn")

train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
#25 bootstraps comprised of 25% of the observations is the default for cross-validation
#uses k of 5,7 and 9 for default optimization
ggplot(train_knn, highlight = TRUE)

train_knn <- train(y ~ ., method = "knn", 
                   data = mnist_27$train,
                   #use tuneGrid to change the k params (requires a df)
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune
train_knn$finalModel
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

#change the cross validation method to use 10 samples comprising 10% of the observations each
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn", 
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

train_knn$results %>% 
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, 
                    ymin = Accuracy - AccuracySD,
                    ymax = Accuracy + AccuracySD))
#function to plot the condition prob
plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])

install.packages("gam")
modelLookup("gamLoess")

#create tunable span params df
grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)
#use loess method for smoother prob curve
train_loess <- train(y ~ ., 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

#~~~~~~~~SECTION 5.2 ASSESSMENT~~~~~~~~~~

#Q1
library(rpart)

set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
data("tissue_gene_expression")

fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit)
fit$bestTune

#Q2
#by default, rpart requires 20 observations before splitting a node
#that means it will be difficult to have a node in which placentas are a  majority
#(there's only 6).  Rerun Q1 but allowing it to split any node minsplit = 0.
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
data("tissue_gene_expression")
fit_rpart <- with(tissue_gene_expression, 
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                        control = rpart.control(minsplit = 0)))
ggplot(fit_rpart)
confusionMatrix(fit_rpart)


#Q3
#plot the tree from Q2, which gene is at the first split
plot(fit_rpart$finalModel, margin = 0.1)
text(fit_rpart$finalModel, cex = 0.75)

#Q4
#Use the train() function and the rf method to train a Random Forest 
#model and save it to an object called fit. Try out values of mtry ranging 
#from seq(50, 200, 25) (you can also explore other values on your own). 
#What mtry value maximizes accuracy? To permit small nodesize to grow as we did 
#with the classification trees, use the following argument: nodesize = 1.
library(randomForest)
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
data("tissue_gene_expression")
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf", 
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))

ggplot(fit)

#Q5
#Use the function varImp() on the output of train() and save it to an object called imp:
imp <- varImp(fit)
imp

#Q6
#determin variable importance rank from Q4
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms
imp_frame <- data_frame(term = rownames(imp$importance),
                        importance = imp$importance$Overall)
ranks_q6 <- imp_frame %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)
ranks_q6

#~~~~~~~~~SECTION 5.3 TITANIC EXERCISES~~~~~~~~~~

library(titanic)    # loads titanic_train data frame
#library(caret)
#library(tidyverse)
#library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)


#Q1
#split titanic_clean into train/test set with 80/20 split based on survived

# generate training and test sets
set.seed(42, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
test_set <- titanic_clean[test_index, ]
train_set <- titanic_clean[-test_index, ]
dim(test_set)
dim(train_set)
mean(train_set$Survived == 1)

#Q2
#Set the seed to 3. For each individual in the test set, randomly guess whether
#that person survived or not by sampling from the vector c(0,1) 
#(Note: use the default argument setting of prob from the sample function).
set.seed(3, sample.kind = "Rounding")

# guess with equal probability of survival
guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
mean(guess == test_set$Survived)

#Q3
#use the training set to predict survival based on sex

#what proportion of training set females survived?
train_set %>% filter( Sex == "female") %>% summarize(survived = mean(Survived == 1))
#what proportion of training set males survived?
train_set %>% filter( Sex == "male") %>% summarize(survived = mean(Survived == 1)) 
#predict survival rate based on sex. If the sex has >0.5 rate of survival
#predict survive otherwise pick died.
y_hat_sex <- ifelse(test_set$Sex == "female", 1, 0)
mean(y_hat_sex == test_set$Survived)

#Q4
#in the training set which passenger class (pclass) was more likely to survive vs die?
train_set %>% group_by(Pclass) %>% summarize(suvived = mean(Survived == 1))

#predict survival based on passenger class. If the class survival rate in the training
#set is >0.5 then pick survive, otherwise pick death
y_hat_class <- ifelse(test_set$Pclass == 1, 1, 0)
mean(y_hat_class == test_set$Survived)

#group by sex and passanger class.  Which groups were more likely to suvive vs die?
train_set %>% group_by(Pclass, Sex) %>% summarize(survived = mean(Survived == 1))

#predict survival based on clas and sex. Greater than 0.5 prob from the training set 
#predict survive otherwise pick death
class_sex_model <- ifelse(test_set$Pclass != 3 & test_set$Sex == "female", 1, 0)
mean(class_sex_model == test_set$Survived)

#Q5
#use confusionmatrix to on each of the 3 models in Q3/4.  You'll need to convert each to factors.
y_hat_sex <- ifelse(test_set$Sex == "female", 1, 0) %>% 
    factor(levels = levels(test_set$Survived))
y_hat_class <- ifelse(test_set$Pclass == 1, 1, 0) %>% 
  factor(levels = levels(test_set$Survived))
class_sex_model <- ifelse(test_set$Pclass != 3 & test_set$Sex == "female", 1, 0) %>% 
  factor(levels = levels(test_set$Survived))

#what is the "positive class" in the matrices?
confusionMatrix(data = y_hat_sex, reference = test_set$Survived)
confusionMatrix(data = y_hat_class, reference = test_set$Survived)
confusionMatrix(data = class_sex_model, reference = test_set$Survived)

#which model has the highest sensitivity?
sensitivity(data = y_hat_sex, reference = test_set$Survived)
sensitivity(data = y_hat_class, reference = test_set$Survived)
sensitivity(data = class_sex_model, reference = test_set$Survived)

#which model has the highest specificity?
specificity(data = y_hat_sex, reference = test_set$Survived)
specificity(data = y_hat_class, reference = test_set$Survived)
specificity(data = class_sex_model, reference = test_set$Survived)

#which model has the best balanced accuracy?
confusionMatrix(data = factor(y_hat_sex), reference = factor(test_set$Survived))
confusionMatrix(data = factor(y_hat_class), reference = factor(test_set$Survived))
confusionMatrix(data = factor(class_sex_model), reference = factor(test_set$Survived))

#Q6
#calculate the F1 score for each model.
F_meas(data = y_hat_sex, reference = factor(test_set$Survived))
F_meas(data = y_hat_class, reference = factor(test_set$Survived))
F_meas(data = class_sex_model, reference = factor(test_set$Survived))


#Q7
#set the seed to 1. Train the model using linear discriminant analysis (LDA). Use
#fare as the only predictor.
set.seed(1, sample.kind = "Rounding")
#what's the accuracy of the LDA model?
train_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
lda_preds <- predict(train_lda, test_set)
mean(lda_preds == test_set$Survived)

#repeat with QDA
set.seed(1, sample.kind = "Rounding")
#what's the accuracy of the QDA model?
train_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
qda_preds <- predict(train_qda, test_set)
mean(qda_preds == test_set$Survived)

#Q8
#set the seed to 1. Train a logistical reg model glm using age as the predictor.
set.seed(1, sample.kind = "Rounding")
train_glm <- train(Survived ~ Age, method = "glm", data = train_set)
glm_preds <- predict(train_glm, test_set)
mean(glm_preds == test_set$Survived)

#set the seed to 1. Train the glm model using sex, class, fare, and age.
set.seed(1, sample.kind = "Rounding")
train_glm_4pred <- train(Survived ~ Sex + Pclass + Fare + Age, method = "glm", data = train_set)
glm_preds_4pred <- predict(train_glm_4pred, test_set)
mean(glm_preds_4pred == test_set$Survived)

#set the seed to 1. Train the glm model using all predictors
set.seed(1, sample.kind = "Rounding")
train_glm_all <- train(Survived ~ ., method = "glm", data = train_set)
glm_preds_all <- predict(train_glm_all, test_set)
mean(glm_preds_all == test_set$Survived)

#Q9
#Set the seed to 6. Train a kNN model on the training set using the caret train function. 
#Try tuning with k = seq(3, 51, 2).
#what's the optimal k value for nearest neighbors?
set.seed(6, sample.kind = "Rounding")
train_knn <- train(Survived ~., method = "knn", tuneGrid = data.frame(k = seq(3, 51, 2)),
                   data = train_set)
ggplot(train_knn)
train_knn$bestTune

#what's the accuracy vs the test set?
knn_preds <- predict(train_knn, test_set)
mean(knn_preds == test_set$Survived)

#Q10
#Set the seed to 8 and train a new kNN model. Instead of the default training 
#control, use 10-fold cross-validation where each partition consists of 10% of 
#the total. Try tuning with k = seq(3, 51, 2).
set.seed(8, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(Survived ~., method = "knn", 
                   tuneGrid = data.frame(k = seq(3, 51, 2)),
                   data = train_set,
                   trControl = control)
#what's the optimal k value?
ggplot(train_knn_cv)
train_knn_cv$bestTune
#what's the accuracy vs the test set?
knn_preds_cv <- predict(train_knn_cv, test_set)
mean(knn_preds_cv == test_set$Survived)

#Q11
#Set the seed to 10. Use caret to train a decision tree with the 
#rpart method. Tune the complexity parameter with cp = seq(0, 0.05, 0.002).
set.seed(10, sample.kind = "Rounding")
# fit a classification tree and plot it
train_rpart <- train(Survived ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.05, 0.002)),
                     data = train_set)
#what's the optimal value of the cp param?
plot(train_rpart)
train_rpart$bestTune

# compute accuracy
preds_rpart <- predict(train_rpart, test_set)
mean(preds_rpart == test_set$Survived)

#which variables are used in the decision tree?
#on decision trees, left = yes and right = no
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
library(rattle)
fancyRpartPlot(train_rpart$finalModel)

#Q12
#Set the seed to 14. Use the caret train() 
#function with the rf method to train a random forest. 
#Test values of mtry = seq(1:7). Set ntree to 100.
set.seed(14, sample.kind = "Rounding")
train_rf <- train(Survived ~ . ,
                  method = "rf", 
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(1:7)),
                  ntree = 100,
                  data = train_set)

#what value of mtry maximizes accuracy?
plot(train_rf)
train_rf$bestTune

#what's the accuracy vs the test set?
preds_rf <- predict(train_rf, test_set)
mean(preds_rf == test_set$Survived)

#what's the most important variable
varImp(train_rf)
