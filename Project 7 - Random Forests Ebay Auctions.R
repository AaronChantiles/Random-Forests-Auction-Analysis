mydata = read.csv("Auctions.csv")


#1
mydata$Competitive = as.factor(mydata$Competitive)
mydata$Category = as.factor(mydata$Category)
mydata$Weekend = as.factor(mydata$Weekend)
str(mydata)

#2
set.seed(100)
train.indx = sample(1:nrow(mydata), 1000)
train.df = mydata[train.indx,]
test.df = mydata[-train.indx,]

#3
set.seed(1000)
bag.res = randomForest(Competitive~., data=train.df, xtest=test.df[,1:5], ytest=test.df[,6], mtry=5, ntree=1000, importance=TRUE)
rf.res = randomForest(Competitive~., data=train.df, xtest=test.df[,1:5], ytest=test.df[,6], mtry=2, ntree=1000, importance=TRUE)

names(rf.res)
names(rf.res$test)

#4
conf.rf = rf.res$test$confusion
conf.rf
sum(diag(conf.rf[,1:2]))/sum(conf.rf[,1:2])

#5 & 6
rf.res$err.rate
rf.res$err.rate[1000:4]
rf.res$test$err.rate

errors = cbind(bag.res$err.rate[,1], rf.res$err.rate[,1], bag.res$test$err.rate[,1], rf.res$test$err.rate[,1])
matplot(errors, type = "l", col = 1:4, lty = 1:4, lwd = 1.5)
legend("topright", legend=c("OOB: Bagging", "OOB: Random Forest", "Test: Bagging", "Test: Random Forest"), col = 1:4, lty = 1:4, lwd = 1.5)

#7
importance(rf.res, type=2)
varImpPlot(rf.res, type=2)


