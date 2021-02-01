library("caret")  
crashData <- data.train[data.train$Crash == 1,]
nonCrashData <- data.train[data.train$Crash == 0,]
nonCrashData <- nonCrashData[ ,c("Crash","ASC1","AFC1","SSC1","SFC1","TSC1","TFC1","ASU1","AFU1","SSU1","SFU1","TSU1","TFU1",
                          "ASD1","AFD1","SSD1","SFD1","TSD1","TFD1",
                          "ASC2","AFC2","SSC2","SFC2","TSC2","TFC2","ASC3","AFC3","SSC3","SFC3","TSC3","TFC3",
                          "ASC4","AFC4","SSC4","SFC4","TSC4","TFC4","ASU2","AFU2","SSU2","SFU2","TSU2","TFU2",
                          "ASU3","AFU3","SSU3","SFU3","TSU3","TFU3","ASU4","AFU4","SSU4","SFU4","TSU4","TFU4",
                          "ASD2","AFD2","SSD2","SFD2","TSD2","TFD2","ASD3","AFD3","SSD3","SFD3","TSD3","TFD3",
                          "ASD4","AFD4","SSD4","SFD4","TSD4","TFD4")]


table(crashData$Crash)

require(caret)  
#将australian数据分成随机十等分
#固定folds函数的分组  
set.seed(7)  
library(DMwR)
train.random.data_9$Crash <- as.factor(train.random.data_9$Crash) 
train.random.data <-train.random.data_9[ ,c("ASC1","ASU2","TFC1","SFC1","SSC1","SSD1" ,"Crash")]
#train.random.data.smote <- SMOTE(Crash~.,train.random.data.smote,perc.over=300,perc.under= 133.3333)

table(train.random.data$Crash)

#test.data_5 <-test.data[ ,c("ASC1","ASU2","TFC1","SFC1","SSC1","SSD1" ,"Crash")]
datatrain_9 <-data.train[ ,c("ASC1","ASU2","TFC1","SFC1","SSC1","SSD1" ,"Crash")]

data.k <-rbind(train.random.data,datatrain_9)
folds <- createFolds(y=data.k$Crash, k=10)

#kfold to validate the precision of models
all=0
for (i in 1:10) {
  fold_test <- data.k[folds[[i]],]
  table(fold_test$Crash)
    #crashFolds[folds[[i]],]   #取folds[[i]]作为测试集  
  fold_train <-  data.k[-folds[[i]],]
  table(fold_train$Crash)
    #nonCrashFolds[-folds[[i]],]   # 剩下的数据作为训练集  
  print("***组号***")  
  print(i) 

  
  f <- Crash ~ ASC1 + ASU2 + TFC1 + SFC1 + SSC1 + SSD1
  mglm <- glm(f, fold_train, family = binomial(link = "logit"))
  pglm <- predict(mglm, fold_test, type = 'response')
  pred <- prediction(as.numeric(pglm), as.numeric( fold_test$Crash))
  perf <- performance(pred, "tpr", "fpr")
  print("***测试集精确度***") 
  fold_accuracy <- performance(pred, "auc")@y.values[[1]]
  print(fold_accuracy)
  all = all +fold_accuracy
  #jpeg(file="kFold.jpg")
  #plot(perf)
  #dev.off()
  print("***训练集精确度***")  
  tglm <- predict(mglm, fold_train, type = 'response')
  pred2 <- prediction(as.numeric(tglm), as.numeric( fold_train$Crash))
  perf2 <- performance(pred2, "tpr", "fpr")
  performance(pred2, "auc")@y.values[[1]]
  
  #if(fold_accuracy>max)  
  #{  
  #  max=fold_accuracy    
  #  num=i  
  #}  
}
average = all/10
print(average)



sum(0.8676303,0.8064035,0.8628297,0.8203205,0.8404298,0.8212793,0.8476344,0.8604974,0.8220935,0.8749271)/10
0.8424046


#将数据集A随机分为k个包，每次将其中一个包作为测试集，剩下k-1个包作为训练集进行训练。
