library(ROCR)
library(plyr)
library(gridExtra)
library(e1071)
library(ggplot2)
library(randomForest)
library(ggRandomForests)

data=read.csv("D:/journal_paper/ITSC2018/data/nearCrashSample.csv")
data$level=as.factor(data$level)

## get Train set & Testing set
# set.seed(1)
n<-nrow(data)
data<-data[order(runif(n)),]
index=sample(1:n,n*0.8)
train=data[index,]
test<-data[-index,]

## prepare ROC for random forest model 1 : all features
## ROC based on test data
# set.seed(121)
## -c(2,3,...) remove threshold features(accel,jerk) & DCNN related features
(rf0 <- randomForest (level ~ .,data=train[,-c(2,3,138,139,140,141)],proximity = TRUE,importance=T,ntree=1000,tree.err=T,mtry = 2))
varImpPlot(rf0)  # important features


p0 <-predict(rf0,newdata=test,type="prob")
pred0=prediction(p0[,c(2)],test$level)
perf0=performance(pred0,"tpr","fpr")

## prepare ROC for random forest model 2: only accelMin & jerkMin 2 features (optional)
## ROC based on test data
set.seed(123)
(rf1 <- randomForest (level ~ jerk+accel,data=train,proximity = TRUE,importance=T,ntree=1000,tree.err=T,mtry = 2))
p1 <-predict(rf1,newdata=test,type="prob")

pred1=prediction(p1[,c(2)],test$level)
perf1=performance(pred1,"tpr","fpr")

## prepare ROC for jerk threshold
## ROC based on test data
pred2=prediction(test$jerk,test$level)
perf2=performance(pred2,"tpr","fpr")

## prepare ROC for acceleration threshold
## ROC based on test data
pred3=prediction(test$accel,test$level)
perf3=performance(pred3,"tpr","fpr")

## prepare ROC for DCNN model
## ROC based on test data
dcnn_data=read.csv("E:/tf_models/DCNN_12_TEST_PRED.csv")
dcnn_data$label=as.factor(dcnn_data$label)
pred=prediction(dcnn_data$hazardous,dcnn_data$label)
perf=performance(pred,"tpr","fpr")

## AUC calculation
(AUC=unlist(performance(pred,"auc")@y.values))
(AUC0=unlist(performance(pred0,"auc")@y.values))
(AUC1=unlist(performance(pred1,"auc")@y.values))
(AUC2=unlist(performance(pred2,"auc")@y.values))
(AUC3=unlist(performance(pred3,"auc")@y.values))

### prepare legend of the ROCs figure
t=paste("Multi-modal DCNN Model(AUC:",sprintf("%.2f", AUC),")",sep="")
t0=paste("RF Model by All Features(AUC:",sprintf("%.2f", AUC0),")",sep="")
t1=paste("RF Model by Mininal Jerk & Accel Features(AUC:",sprintf("%.2f", AUC1),")",sep="")
t2=paste("Using Only Jerk Threshold(AUC:",sprintf("%.2f", AUC2),")",sep="")
t3=paste("Using Only Acceleration Threshold(AUC:",sprintf("%.2f", AUC3),")",sep="")
lgnd=c(t,t0,t1,t2,t3)

## plot ROC
plot(perf)  
lines(perf0@x.values[[1]],perf0@y.values[[1]],lty=2,col=2)
lines(perf1@x.values[[1]],perf1@y.values[[1]],lty=3,col=3)
lines(perf2@x.values[[1]],perf2@y.values[[1]],lty=4,col=4)
lines(perf3@x.values[[1]],perf3@y.values[[1]],lty=5,col=5)
legend("bottomright",lty=c(1,2,3,4,5),col=c(1,2,3,4,5),legend=lgnd,text.width=0.75)

prf0 <- performance(pred3, 'sens', 'spec')
sens = data.frame(prf0@x.values)
spec = data.frame(prf0@y.values)
max_youden = 0
max_sens = 0
max_spec = 0
for (i in 1:nrow(sens)) {
  if (sens[i, 1] + spec[i, 1] > max_youden) {
    max_youden = sens[i, 1] + spec[i, 1]
    max_sens = sens[i, 1]
    max_spec = spec[i, 1]
  }
}
