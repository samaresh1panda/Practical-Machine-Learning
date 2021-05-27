library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(corrplot)

library(randomForest)
library(RColorBrewer)
#install.packages("rattle")
require(rattle)


training <- read.csv(url(UrlTrain))
testing  <- read.csv(url(UrlTest))

# create a partition with the training dataset 
inTrain  <- createDataPartition(training$classe, p=0.7, list=FALSE)
TrainSet <- training[inTrain, ]
TestSet  <- training[-inTrain, ]
dim(TrainSet)
dim(TestSet)


# remove variables with Nearly Zero Variance
NZV <- nearZeroVar(TrainSet)
TrainSet <- TrainSet[, -NZV]
TestSet  <- TestSet[, -NZV]
dim(TrainSet)
dim(TestSet)


# remove variables that are mostly NA
AllNA    <- sapply(TrainSet, function(x) mean(is.na(x))) > 0.95
TrainSet <- TrainSet[, AllNA==FALSE]
TestSet  <- TestSet[, AllNA==FALSE]
dim(TrainSet)
dim(TestSet)

# remove identification only variables (columns 1 to 5)
TrainSet <- TrainSet[, -(1:5)]
TestSet  <- TestSet[, -(1:5)]
dim(TrainSet)
dim(TestSet)



corMatrix <- cor(TrainSet[, -54])
corrplot(corMatrix, order = "FPC", method = "color", type = "lower", tl.cex = 0.8, tl.col = rgb(0, 0, 0))
         

    
# model fit
set.seed(12345)
controlRF <- trainControl(method="cv", number=3, verboseIter=FALSE)
modFitRandForest <- train(classe ~ ., data=TrainSet, method="rf",trControl=controlRF)
modFitRandForest$finalModel


 predictRf <- predict(modFitRandForest, TestSet)

all_zero_colnames <- sapply(names(TrainSet), function(x) all(is.na(TrainSet[,x])==TRUE))
nznames <- names(all_zero_colnames)[all_zero_colnames==FALSE]
nznames <- nznames[-(1:7)]
nznames <- nznames[1:(length(nznames)-1)] 
nznames

set.seed(12345)
decisionTreeMod <- rpart(classe ~ ., data=TrainSet, method="class")
fancyRpartPlot(decisionTreeMod)

predict_decision_tree <- predict(decisionTreeMod, newdata = TrainSet, type="class")
conf_matrix_decision_tree <- confusionMatrix(predict_decision_tree, TrainSet$classe)
conf_matrix_decision_tree

plot(conf_matrix_decision_tree$table, col = conf_matrix_decision_tree$byClass, 
     main = paste("Decision Tree Model: Predictive Accuracy =",
                  round(conf_matrix_decision_tree$overall['Accuracy'], 4)))

set.seed(1813)
ctrl_GBM <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
fit_GBM  <- train(classe ~ ., data = TrainSet, method = "gbm",
                  trControl = ctrl_GBM, verbose = FALSE)
fit_GBM$finalModel

predict_GBM <- predict(fit_GBM, newdata = TrainSet)
conf_matrix_GBM <- confusionMatrix(predict_GBM, TrainSet$classe)
conf_matrix_GBM

set.seed(1813)
ctrl_RF <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
fit_RF  <- train(classe ~ ., data = TrainSet, method = "rf",
                  trControl = ctrl_RF, verbose = FALSE)
fit_RF$finalModel

predict_RF <- predict(fit_RF, newdata = TrainSet)
conf_matrix_RF <- confusionMatrix(predict_RF, TrainSet$classe)
conf_matrix_RF


