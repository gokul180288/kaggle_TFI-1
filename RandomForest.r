library(randomForest)
library(lubridate)
# read training data
train <- read.csv("train.csv")
revenue<-train$revenue # for calculating RMSE in training data
# read test data
test <- read.csv("test.csv")
# separate month/day/year
train$day<-as.factor(day(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))
train$month<-as.factor(month(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))
train$year<-as.factor(year(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))

test$day<-as.factor(day(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")))
test$month<-as.factor(month(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")))
test$year<-as.factor(year(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")))

test.normal <- data.frame(lapply(test,as.numeric))
train.normal <- data.frame(lapply(train,as.numeric))
x.normal <- train.normal[,c(-1,-2,-43)]
y.normal <- train.normal[,43]

train[,6:43] <- log(1 +train[,6:43]) # P1-P36, and revenue
test[,6:42] <- log(1 +test[,6:42]) # P1-P36

test.log <- data.frame(lapply(test,as.numeric))
train.log <- data.frame(lapply(train,as.numeric))
x.log <- train.log[,c(-1,-2,-43)]
y.log <- train.log[,43]
#hist(y.normal)
#hist(y.log)

# normal
set.seed(131)
normalRF <- randomForest(x.normal,y.normal,mtry=4,ntree=300,sampsize=round(0.50*nrow(x.normal)),
nodesize=3,do.trace=F,replace=T)
#varImpPlot(normalRF)
#plot(normalRF)
pred.normal <- predict(normalRF,x.normal,type="response")
print(sqrt(sum((revenue-pred.normal)^2/nrow(train))))
# RMSE 1698021; Public LB score 1724475.73387

# double logarithm 
set.seed(131)
logRF <- randomForest(x.log,y.log,mtry=8,ntree=500,sampsize=round(0.50*nrow(x.log)),
nodesize=1,do.trace=F,replace=T)
#varImpPlot(logRF)
#plot(logRF)
pred.log <- predict(logRF,x.log,type="response")
pred.log<-exp(pred.log)-1
print(sqrt(sum((revenue-pred.log)^2/nrow(x.log))))
# RMSE 1699385, Public LB socre 1744175.71009
for(i in 0:10){
pred<-pred.normal*(i*0.1)+pred.log*(10-i)*0.1
print(sqrt(sum((revenue-pred)^2/nrow(x.log))))
}
# determine the ratio of pre.normal and pre.log
# 0:10 1699385
# 1:9  1693414
# 2:8  1688725
# 3:7  1685328
# 4:6  1683232
# 5:5  1682441 <- this is the best
# 6:4  1682957
# 7:3  1684778
# 8:2  1687902
# 9:1  1692319
#10:0  1698021
for(i in -5:5){
pred<-pred.normal*(0.50+i*0.01)+pred.log*(0.50-i*0.01)
print(sqrt(sum((revenue-pred)^2/nrow(x.log)))) # more precise
}
pred<-pred.normal*(0.51)+pred.log*(0.49)
print(sqrt(sum((revenue-pred)^2/nrow(x.log)))) # 1682434

###
pred.normal <- predict(normalRF,test.normal[,c(-1,-2)],type="response")
pred.log <- predict(logRF,test.log[,c(-1,-2)],type="response")
pred.log <- exp(pred.log)-1
pred<-pred.normal*0.51+pred.log*0.49
submit <- data.frame(Id=test$Id,Prediction=pred)
submit <- round(submit*10)/10
write.table(submit,"submit.csv",sep=",",row.names=FALSE)
# Public LB score 1681465.41570, Private LB score 1846686.19350
