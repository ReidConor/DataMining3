library(DMwR)
library(ggplot2) 
library(C50)
setwd("/Users/Conor/Google\ Drive/MSc/2_DM/Assignment3")
college = read.csv(file="data/college.csv",head=TRUE,sep=",")
college = college[,-c(1,2)] #dont need the first column
View(college)
summary(college)
table(college$isElite) #78 / 699...need to balance
aggregate(college[, 1:4], list(college$isElite), mean)#averages split by elite or not
qplot(accept_rate, data=college, geom="density", fill=isElite, alpha=I(.5), 
      main="Distribution of Acceptance Rate by Status", xlab="Accept Rate", 
      ylab="Density")
qplot(isElite, Grad.Rate, data=college, geom=c("boxplot", "jitter"), 
      fill=isElite, main="Graduation Rate by Status",
      xlab="", ylab="Graduation Rate")
table(college$isElite,college$Private)

college$isElite = as.numeric(college$isElite)
college$Private = as.numeric(college$Private)
View(college)

#balance the classes
college$isElite = as.factor(college$isElite)
collegeBalanced = SMOTE(isElite ~ ., college, perc.over = 300, perc.under=300)
collegeBalanced$isElite = as.numeric(collegeBalanced$isElite)
table(collegeBalanced$isElite)

#split
dataPartition = sample(2,nrow(collegeBalanced),replace=TRUE,prob=c(0.7,0.3))
trainData = collegeBalanced[dataPartition ==1,]
testData = collegeBalanced[dataPartition ==2,]
trainData$isElite <- as.factor(trainData$isElite)
dim(trainData)
dim(testData)

#model - tree
treeModelPruned = C5.0(x = trainData[, -6], y = trainData$isElite,
                       control = C5.0Control(noGlobalPruning=FALSE))
treeModelPruned
summary(treeModelPruned)
predictions <- predict(treeModelPruned, testData, type="class")
table(testData$isElite, predictions)


library(MASS)
fit <- lda(isElite ~ ., data=collegeBalanced, 
           na.action="na.omit", CV=TRUE)
fit # show results
