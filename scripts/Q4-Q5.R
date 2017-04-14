library(C50)
data(churn)
#summary shows that the classes are highly unbalanced. 
#Most cases are non-churn ie 85.5% 
summary(churnTrain)
table(churnTrain$churn)
#Build + view the tree model
treeModel = C5.0(x = churnTrain[, -20], y = churnTrain$churn,
                 control = C5.0Control(noGlobalPruning=TRUE))
treeModel
summary(treeModel)
treeModelPrune = C5.0(x = churnTrain[, -20], y = churnTrain$churn,
                      control = C5.0Control(noGlobalPruning=FALSE))
treeModelPrune
summary(treeModelPrune)
