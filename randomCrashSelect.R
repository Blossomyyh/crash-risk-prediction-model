#data <- read.csv("modelData201405.csv")


crashData <- data.train[data.train$Crash == 1,]
nonCrashData <- data.train[data.train$Crash == 0,]
set.seed(22)
prctSample <- 4
numRowC <- nrow(crashData)
numRowN <- nrow(nonCrashData)
rowIndex <- sort(sample(numRowN,numRowC*prctSample))
#Error in sample.int(x, size, replace, prob) : 
#cannot take a sample larger than the population when 'replace = FALSE'
nonCrashData <- nonCrashData[rowIndex,]
data <- rbind(crashData,nonCrashData) 
train.random.data_4= data
#write.csv(data,"randomData201405.csv")
#train.random.data_5 <- train.random.data[ ,c("Crash","ASC1","ASU2","TFC1","SFC1","SSC1","SSD1")]
#write.csv(train.random.data_6,"modelDataRandom201405_6.csv",row.names = F)



#install.packages(ROCR)
library(ROCR)
#读数据
data.random.train <- read.csv("randomData201405.csv")
data.test <- read.csv("modelData201406.csv")
sampleWeek <- read.csv('sampleWeek.csv')

test.crashData <- data.test[data.test$Crash == 1,]
test.nonCrashData <- data.test[data.test$Crash == 0,]
set.seed(22)
prctSample <- 0.5
test.numRowC <- nrow(test.crashData)
test.numRowN <- nrow(test.nonCrashData)
test.rowIndex <- sort(sample(test.numRowN,test.numRowC*prctSample))
#Error in sample.int(x, size, replace, prob) : 
#cannot take a sample larger than the population when 'replace = FALSE'
test.nonCrashData <- test.nonCrashData[test.rowIndex,]
#test.data是4：1的random
test.random.data_.5 <- rbind(test.crashData,test.nonCrashData) 
table(test.random.data_.5$Crash)
#   0    1 
#4512 1128 
write.csv(test.data,"testrandomData201406.csv")
test_data_6 <- test.data[ ,c("Crash","ASC1","ASU2","TFC1","SFC1","SSC1","SSD1")]
write.csv(test_data_6,"testrandomData201406_6.csv", row.names = F)
#数据生成
table(data.train$Crash)
prop.table(table(data.train$Crash))
sampledata <-read.csv('sampleData201405.csv')
sampletest <-read.csv('sampleData201406.csv')

#建模
sampledata <- sampledata[ ,c("ASC1","AFC1","SSC1","SFC1","TSC1","TFC1","ASU1","AFU1","SSU1","SFU1","TSU1","TFU1",
                             "ASD1","AFD1","SSD1","SFD1","TSD1","TFD1",
                             "ASC2","AFC2","SSC2","SFC2","TSC2","TFC2","ASC3","AFC3","SSC3","SFC3","TSC3","TFC3",
                             "ASC4","AFC4","SSC4","SFC4","TSC4","TFC4","ASU2","AFU2","SSU2","SFU2","TSU2","TFU2",
                             "ASU3","AFU3","SSU3","SFU3","TSU3","TFU3","ASU4","AFU4","SSU4","SFU4","TSU4","TFU4",
                             "ASD2","AFD2","SSD2","SFD2","TSD2","TFD2","ASD3","AFD3","SSD3","SFD3","TSD3","TFD3",
                             "ASD4","AFD4","SSD4","SFD4","TSD4","TFD4","Crash")]
#Crash~.
f <- Crash ~ ASC1 + ASU2 + TFC1 + SFC1 + SSC1 + SSD1
mglm <- glm(f, sampledata, family = binomial(link = "logit"))
#mglm.step <- step(mglm)  test.random.data_100
pglm <- predict(mglm, sampledata06, type = 'response')
pred <- prediction(as.numeric(pglm), as.numeric( sampledata06$Crash))
perf <- performance(pred, "tpr", "fpr")
performance(pred, "auc")@y.values[[1]]
jpeg(file="ROC_1:1mat_mat.jpg")
plot(perf)
dev.off()
save.image(file = "randomSample06.RData")






#oneMonth
#case control 1:4 条件  [1] 0.5265872  [test]0.5214006
#0    1 
#1223  314
#case control 1:4 random  [1] 0.8621841（6个参数建模 allTest）） [1]0.8639141(random test)
#0    1 
#4840 1210



#test data 抽样
#0    1 
#4512 1128 

