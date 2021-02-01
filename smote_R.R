#install.packages(ROCR)
library(ROCR)
#读数据
data.train <- read.csv("modelData201405.csv")
data.test <- read.csv("modelData201406.csv")
table(data.train$Crash)
prop.table(table(data.train$Crash))



#建模
data.train <- data.train[ ,c("Crash","ASC1","AFC1","SSC1","SFC1","TSC1","TFC1","ASU1","AFU1","SSU1","SFU1","TSU1","TFU1",
                             "ASD1","AFD1","SSD1","SFD1","TSD1","TFD1",
                             "ASC2","AFC2","SSC2","SFC2","TSC2","TFC2","ASC3","AFC3","SSC3","SFC3","TSC3","TFC3",
                             "ASC4","AFC4","SSC4","SFC4","TSC4","TFC4","ASU2","AFU2","SSU2","SFU2","TSU2","TFU2",
                             "ASU3","AFU3","SSU3","SFU3","TSU3","TFU3","ASU4","AFU4","SSU4","SFU4","TSU4","TFU4",
                             "ASD2","AFD2","SSD2","SFD2","TSD2","TFD2","ASD3","AFD3","SSD3","SFD3","TSD3","TFD3",
                             "ASD4","AFD4","SSD4","SFD4","TSD4","TFD4")]

#全样本，SMOTE1：1-10
#使用SMOTE来平衡
library(DMwR) 
#trainSplit$target <- as.factor(trainSplit$target) 
data.train$Crash <- as.factor(data.train$Crash) 

#只用6个变量进行smote优化
data_sample <- data.train[ ,c("ASC1","ASU2","TFC1","SFC1","SSC1","SSD1" ,"Crash")]
#，perc.over=a,perc.under=b。
#首先增加少数派样本的数量，平均每个样本增加a/100个新样本，一共新增了aN/100个全新的少数派样本
#并把最初的少数派样本和新增的少数派样本都放入新的数据集中
#对多数派的样本进行采样，采样数量为(b/100) * aN/100，得到新的多数派样本
#少数派样本有(1+a/100)N个,多数派样本有(b/100) * aN/100个 perc.over 不能为0 
data_sample <- SMOTE(f,sampledata,perc.over=200,perc.under=100) #Crash~.
#datatrain$Crash <- as.numeric(datatrain$Crash) 
# 我们再次用prop.table()函数检查结果的平衡性，确定我们已经让阴性、阳性数据达到相同。 
table(data_sample$Crash)

mglm <- glm(f, data_sample, family = binomial(link = "logit"))
#mglm.step <- step(mglm)
pglm <- predict(mglm, data.test, type = 'response')
pred <- prediction(as.numeric(pglm), as.numeric(data.test$Crash))
perf <- performance(pred, "tpr", "fpr")
performance(pred, "auc")@y.values[[1]]

#作图分析
jpeg(file="ROC_smote_1_1_sam_allT.jpg")
plot(perf)
dev.off()

save.image(file = "SMOTE_1_all_.RData")
