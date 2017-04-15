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

#######################################################################################
#model - tree
treeModelPruned = C5.0(x = trainData[, -6], y = trainData$isElite,
                       control = C5.0Control(noGlobalPruning=FALSE))
treeModelPruned
summary(treeModelPruned)
predictions <- predict(treeModelPruned, testData, type="class")
table(testData$isElite, predictions)

#random forest
library(randomForest)
randomForestModel = randomForest(isElite ~ ., data=trainData, ntree=100, proximity=T )
table(predict(randomForestModel), trainData$isElite)
print(randomForestModel)
plot(randomForestModel, main= "")
importance(randomForestModel)
randomForestModelPred = predict (randomForestModel, newdata = testData)
table(testData$isElite,randomForestModelPred)

#Logistic Regression
library(glm2)
logRegressionModel = glm(isElite ~ ., family=binomial(link='logit'), data=trainData)
summary(logRegressionModel)
logRegressionModelPred = predict(logRegressionModel, newdata=testData)
table(testData$isElite,logRegressionModelPred)

#Clustering
View(collegeBalanced)
collegeBalancedClustering = collegeBalanced[,-6]
View(collegeBalancedClustering)
#Use K-means
#Find num of clusters
wss = (nrow(collegeBalancedClustering)-1)*sum(apply(collegeBalancedClustering,2,var))
for (i in 2:15) wss[i] = sum(kmeans(collegeBalancedClustering, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
#pick three?
kmeansCluster = kmeans(collegeBalancedClustering, 3) # 5 cluster solution
aggregate(collegeBalancedClustering,by=list(kmeansCluster$cluster),FUN=mean)

#plot a silhouette
dissE = daisy(collegeBalancedClustering)
dE2 = dissE^2
sk2 = silhouette(kmeansCluster$cluster, dE2)
plot(sk2)

#merge the actaul results (from the orginal dataset) with those found during clustering
collegeBalancedClustering = data.frame(collegeBalancedClustering, kmeansCluster$cluster)
collegeBalancedClustering$rowNames = row.names(collegeBalancedClustering)
collegeBalanced$rowNames = row.names(collegeBalanced)
actualResults = collegeBalanced[,c(6,7)]
collegeBalancedClustering = merge(collegeBalancedClustering, actualResults, by="rowNames" )
View(collegeBalancedClustering)

#analyse
library(sqldf)
#matches 
sqldf('select count(*) from collegeBalancedClustering 
                where "kmeansCluster.cluster" = isElite')
#nonmatches
sqldf('select count(*) from collegeBalancedClustering 
                where "kmeansCluster.cluster" != isElite')
#extraGroup
sqldf('select count(*) from collegeBalancedClustering 
                where "kmeansCluster.cluster" = 3')


#are those observations marked 3 by the clustering actaully 1's or 2's in the
#orginal dataset?
sqldf('select isElite, count(*) from collegeBalancedClustering
             where "kmeansCluster.cluster" = 3 
             group by isElite')
#mostly 2's are being put in the new 3 group
#suggests that the non-elite colleges can be broken into two groups perhaps?
#the fact that many records in the 1's group are also in the 3's, maybe the 3's
#group is the upper end of the original 2's group?