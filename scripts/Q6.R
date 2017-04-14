library(C50)
library(FNN)
library(rpart)
library(DMwR)
library(caret)
set.seed(1234)
data(GermanCredit)
table(GermanCredit$Class) #unbalanced - 300 / 700
#try to balance
GermanCredit$Class <- as.factor(GermanCredit$Class)
GermanCredit <- SMOTE(Class ~ ., GermanCredit, perc.over = 100, perc.under=200)
GermanCredit$Class <- as.numeric(GermanCredit$Class)
table(GermanCredit$Class)
dim(GermanCredit)
#split into training and test
#dataPartition <- sample(2,nrow(GermanCredit),replace=TRUE,prob=c(0.5,0.5))
dataPartition <- sample(2,nrow(GermanCredit),replace=TRUE,prob=c(0.7,0.3))
trainData <- GermanCredit[dataPartition ==1,]
testData <- GermanCredit[dataPartition ==2,]
trainData$Class <- as.factor(trainData$Class)
dim(trainData)
dim(testData)
treeModelPruned = C5.0(x = trainData[, -10], y = trainData$Class,
                       control = C5.0Control(noGlobalPruning=FALSE))
treeModelPruned
summary(treeModelPruned)
predictions <- predict(treeModelPruned, testData, type="class")
table(testData$Class, predictions)
#@50/50 - Size = 47
#TPR = 236 / (236 + 61) = 79%
#TNR = 246 / (246 + 63) = 80%
#@70/30 - Size = 67
#TPR = 134 / (134 + 39) = 77%
#TNR = 144 / (144 + 19) = 88%