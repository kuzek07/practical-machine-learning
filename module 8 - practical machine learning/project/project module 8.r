#1. Set Librarries
## r=free up memory amd load required libraries
rm(list=ls())
library(knitr)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(corrplot)
set.seed(12345)

#2. Download Dataset
## set the URL for the download
UrlTrain <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
UrlTest  <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

## download the datasets
train <- read.csv(url(UrlTrain))
test  <- read.csv(url(UrlTest))

## create a partition with the training dataset 
inTrain  <- createDataPartition(train$classe, p=0.7, list=FALSE)
TrainDS <- train[inTrain, ]
TestDS  <- train[-inTrain, ]
dim(TrainDS)
dim(TestDS)


#3. Clean Data
## remove variables with Nearly Zero Variance
nzv <- nearZeroVar(TrainDS)
TrainDS <- TrainSet[, -nzv]
TestDS  <- TestSet[, -nzv]
dim(TrainDS)
dim(TestDS)

## remove variables that are mostly NA
allNA    <- sapply(TrainDS, function(x) mean(is.na(x))) > 0.95
TrainDS <- TrainDS[, allNA==FALSE]
TestDS  <- TestDS[, allNA==FALSE]
dim(TrainDS)
dim(TestDS)

## remove identification only variables (columns 1 to 5)
TrainDS <- TrainDS[, -(1:5)]
TestDS  <- TestDS[, -(1:5)]
dim(TrainDS)
dim(TestDS)

#4. Correlation Analysis

corMatrix <- cor(TrainDS[, -54])
corrplot(corMatrix, order = "FPC", method = "color", type = "lower", 
         tl.cex = 0.8, tl.col = rgb(0, 0, 0))

#5. Building Prediction Model

##a.Random Forest

###Model Fit using train dataset
set.seed(12345)
controlRF <- trainControl(method="cv", number=3, verboseIter=FALSE)
modelRF <- train(classe ~ ., data=TrainDS, method="rf", trControl=controlRF)
modelRF$finalModel

###prediction on Test dataset
predictRF <- predict(modelRF, newdata=TestDS)
confidenceRF <- confusionMatrix(predictRF, TestDS$classe)
confidenceRF

##b. Decision Tree
###Model Fit using train dataset
set.seed(12345)
modelDT <- rpart(classe ~ ., data=TrainDS, method="class")
fancyRpartPlot(modelDT)

###prediction on Test dataset
predictDT <- predict(modelDT, newdata=TestDS, type="class")
confidenceDT <- confusionMatrix(predictDT, TestDS$classe)
confidenceDT


##c. Generalized Boosted Model
### model fit using train dataset
set.seed(12345)
controlGBM <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
modelGBM  <- train(classe ~ ., data=TrainDS, method = "gbm", trControl = controlGBM, verbose = FALSE)
modelGBM$finalModel

###prediction on Test dataset
predictGBM <- predict(modelGBM, newdata=TestDS)
confidenceGBM <- confusionMatrix(predictGBM, TestDS$classe)
confidenceGBM

#6. Applying to test data

#The accuracy of the 3 regression modeling methods above are:
#Random Forest : 0.9963
#Decision Tree : 0.7368
#GBM : 0.9839
#In that case, the Random Forest model will be applied to predict the 20 quiz results (testing dataset) as shown below.

predictTEST <- predict(modelRF, newdata=testing)
predictTEST