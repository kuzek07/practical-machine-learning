#question 1
#Set the variable y to be a factor variable in both the training and test set. 
#Then set the seed to 33833. Fit (1) a random forest predictor relating the factor variable y 
#to the remaining variables and (2) a boosted predictor using the "gbm" method. 
#Fit these both with the train() command in the caret package.

#What are the accuracies for the two approaches on the test data set? 
#What is the accuracy among the test set samples where the two methods agree?

library(caret)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
mod_rf <- train(y ~ ., data = vowel.train, method = "rf")
mod_gbm <- train(y ~ ., data = vowel.train, method = "gbm")

pred_rf <- predict(mod_rf, vowel.test)
pred_gbm <- predict(mod_gbm, vowel.test)

confusionMatrix(pred_rf, vowel.test$y)$overall[1]

confusionMatrix(pred_gbm, vowel.test$y)$overall[1]

idx_agreed <- (pred_rf == pred_gbm)

confusionMatrix(vowel.test$y[idx_agreed], pred_rf[idx_agreed])$overall['Accuracy']

#question 2
#Set the seed to 62433 and predict diagnosis with all the other variables using a random forest ("rf"), 
#boosted trees ("gbm") and linear discriminant analysis ("lda") model. 
#Stack the predictions together using random forests ("rf"). 
#What is the resulting accuracy on the test set? Is it better or worse than each of the individual predictions?

library(caret)

library(gbm)

set.seed(3433)


library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]
set.seed(62433)

mod_rf <- train(diagnosis ~ ., data = training, method = "rf")
mod_gbm <- train(diagnosis ~ ., data = training, method = "gbm")
mod_lda <- train(diagnosis ~ ., data = training, method = "lda")
pred_rf <- predict(mod_rf, testing)
pred_gbm <- predict(mod_gbm, testing)
pred_lda <- predict(mod_lda, testing)
predDF <- data.frame(pred_rf, pred_gbm, pred_lda, diagnosis = testing$diagnosis)
combModFit <- train(diagnosis ~ ., method = "rf", data = predDF)
combPred <- predict(combModFit, predDF)

confusionMatrix(pred_rf, testing$diagnosis)$overall[1]
confusionMatrix(pred_gbm, testing$diagnosis)$overall[1]
confusionMatrix(pred_lda, testing$diagnosis)$overall[1]
confusionMatrix(combPred, testing$diagnosis)$overall[1]


#question 3
#Set the seed to 233 and fit a lasso model to predict Compressive Strength. 
#Which variable is the last coefficient to be set to zero as the penalty increases? 
#(Hint: it may be useful to look up ?plot.enet).
#Solution: To use the function plot.enet(), we need to load the package elasticnet.

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(233)

mod_lasso <- train(CompressiveStrength ~ ., data = training, method = "lasso")
library(elasticnet)
plot.enet(mod_lasso$finalModel, xvar = "penalty", use.color = TRUE)

#question 4
#Fit a model using the bats() function in the forecast package to the training time series. 
#Then forecast this model for the remaining time points. 
#For how many of the testing points is the true value within the 95% prediction interval bounds?

library(lubridate) # For year() function below

dat = read.csv("gaData.csv")

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

library(forecast)
mod_ts <- bats(tstrain)
fcast <- forecast(mod_ts, level = 95, h = dim(testing)[1])
sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / dim(testing)[1]

#question 5
#Set the seed to 325 and fit a support vector machine using the e1071 package 
#to predict Compressive Strength using the default settings. Predict on the testing set. \
#What is the RMSE?


set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(325)
library(e1071)
mod_svm <- svm(CompressiveStrength ~ ., data = training)
pred_svm <- predict(mod_svm, testing)
accuracy(pred_svm, testing$CompressiveStrength)
               