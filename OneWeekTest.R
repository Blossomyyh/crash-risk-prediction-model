
#install.packages(ROCR)
library(ROCR)
#读数据
data.train <- read.csv("modelData201405.csv")
data.test <- read.csv("modelData201406.csv")
#modelData201406.csv
data_test_6 <- data.train[ ,c("Crash","ASC1","ASU2","TFC1","SFC1","SSC1","SSD1")]
write.csv(data_test_6,"modelData201406_6.csv",row.names = F)
#数据生成
data_sample_6 <- sampledata[ ,c("Crash","ASC1","ASU2","TFC1","SFC1","SSC1","SSD1")]
write.csv(data_sample_6,"sample201405_6.csv",row.names = F)
#SMOTE1:1-10
table(data.train$Crash)
prop.table(table(data.train$Crash))
data.train <- read.csv('modelData201405.csv')


#建模
sampledata <- sampledata[ ,c("Crash","ASC1","AFC1","SSC1","SFC1","TSC1","TFC1","ASU1","AFU1","SSU1","SFU1","TSU1","TFU1",
                             "ASD1","AFD1","SSD1","SFD1","TSD1","TFD1",
                              "ASC2","AFC2","SSC2","SFC2","TSC2","TFC2","ASC3","AFC3","SSC3","SFC3","TSC3","TFC3",
                             "ASC4","AFC4","SSC4","SFC4","TSC4","TFC4","ASU2","AFU2","SSU2","SFU2","TSU2","TFU2",
                             "ASU3","AFU3","SSU3","SFU3","TSU3","TFU3","ASU4","AFU4","SSU4","SFU4","TSU4","TFU4",
                             "ASD2","AFD2","SSD2","SFD2","TSD2","TFD2","ASD3","AFD3","SSD3","SFD3","TSD3","TFD3",
                             "ASD4","AFD4","SSD4","SFD4","TSD4","TFD4")]
write.csv(datatest,"datatest201406_73.csv",row.names = F)
#Crash~.
f <- Crash ~ ASC1 + ASU2 + TFC1 + SFC1 + SSC1 + SSD1
mglm <- glm(f, train.random.data_100, family = binomial(link = "logit"))
#mglm.step <- step(mglm)
pglm <- predict(mglm, data.test, type = 'response')
pred <- prediction(as.numeric(pglm), as.numeric(data.test$Crash))
perf <- performance(pred, "tpr", "fpr")
performance(pred, "auc")@y.values[[1]]
jpeg(file="ROC_ran100_ran.jpg")
plot(perf)
dev.off()
save.image(file = "allSample_model.RData")


#全样本，SMOTE1：1-10
#使用SMOTE来平衡
library(DMwR) 
#trainSplit$target <- as.factor(trainSplit$target) 
sample.data$Crash <- as.factor(sample.data$Crash) 

#只用6个变量进行smote优化
data_sample <- data.train[ ,c("ASC1","ASU2","TFC1","SFC1","SSC1","SSD1" ,"Crash")]
#，perc.over=a,perc.under=b。
#首先增加少数派样本的数量，平均每个样本增加a/100个新样本，一共新增了aN/100个全新的少数派样本
#并把最初的少数派样本和新增的少数派样本都放入新的数据集中
#对多数派的样本进行采样，采样数量为(b/100) * aN/100，得到新的多数派样本
#少数派样本有(1+a/100)N个,多数派样本有(b/100) * aN/100个 perc.over 不能为0 
data_sample <- SMOTE(f,sample.data,perc.over=532,perc.under=123) #Crash~.
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

#save.image(file = "SMOTE_1_all_.RData")



# 再次建立treebag模型 
#tbmodel<-train(target~.,data=trainSplit,method="treebag", trControl=ctrl) 
#predictors <- names(trainSplit)[names(trainSplit)!='target'] 
#pred <- predict(tbmodel$finalModel,testSplit[,predictors]) 
#auc <- roc(testSplit$target,pred) 
#print(auc) 

#plot(auc,ylim=c(0,1),print.thres=TRUE,main=paste('AUC',round(auc$auc[[1]],2))) 
#abline(h=1,col="blue",lwd=2) 
#abline(h=0,col="red",lwd=2)

SMOTE.train_1_all_ <-write.csv(datatrain,"SMOTETrain_1_all_.csv",row.names = F)




#全样本[1] 0.8427695
#1:1   (100:200)[1] 0.8458516(所有参数)  //0.8476126 0.8455901 0.8441716   [1] 0.8484737（仅用6个参数）  //0.8503296  0.8445346  0.8455683
#   0    1 
#556 556
#1:2  [1] 0.8475436     [1] 0.8464478  //0.8481881 0.8454405
#   0    1 
#1112  556
#1:3  [1] 0.8493252     [1] 0.8492533  (0.8468517  0.8485979  0.8481013) 
#   0    1 
#1668  556
#1:4  (100:800)[1] 0.8497385  // 0.8499995 0.8481422 0.8479856 0.8476718 0.8470059 0.849733  [1] 0.8464321   \\\ 0.8460563 0.8464971  0.8472805 0.8464518 0.8468217 0.8471747 0.8448132
#   0    1 
#2224  556 
#1:5  [1] 0.8477215     [1] 0.8480294   
#   0    1 
#2780  556
#1:6  [1] 0.8474429     [1] 0.84888
#   0    1 
#3336  556
#1:7  [1] 0.8493126     [1] 0.8489731
#   0    1 
#3892  556
#1:8  [1] 0.8480156     [1] 0.845547
#   0    1 
#4448  556
#1:9  [1] 0.8484889     [1] 0.8464634
#0    1 
#5004  556 
#1:10 [1] 0.8478453  //0.8476315 0.8465905  0.8462087  0.8462087 [1] 0.8448464 //0.8475005 0.8470386
#   0    1 
#5560  556


#11个AUC值对比
Percent <- c(1,2,3,4,10,20,50,100)
#AUC <- c(0.8628515,0.8598532, 0.8576861,0.8631142,0.8549751,0.8549026,0.8534909,0.852288)
#radom
#
#all
AUC <- c(0.8441303,0.8442143, 0.8441896,0.8442488,0.8441327,0.8439704,0.8441249,0.8441764)

data <-data.frame(Percent,AUC)

#plot(data$x,data$y,xlim=c(0,9),ylim = c(0.84,0.85),lwd=2,xlab="smote",ylab="AUC",type="l")
#jpeg(file="AUC_all_.jpg")
#plot(x,y,xlim=c(0,10),ylim = c(0.84,0.85),lwd=2,xlab="smote",ylab="AUC",type="b")
#dev.off()

library(ggplot2)
data$Percent = factor(data$Percent)
jpeg(file="Random_ran.jpg")
#plot(x,y,xlim=c(0,8),ylim = c(0.84,0.85),lwd=2,xlab="smote",ylab="AUC",type="l")
ggplot(data,aes(x=Percent,y = AUC,group = 1))+geom_line()+geom_point() +geom_text(label=paste(data$AUC*100,'%',sep=''),colour = "black", vjust=-0.5, size=2)
#text(data$,data$y,data$y,cex=1,pos=3,col="red")
dev.off()
#1.需要仅仅6个变量的smote优化
#2.需要将多数类smote优化达到接近全样本的水平
#对于smote函数——采用k（为5）临近值
# INPUTS:
# form a model formula
# data the original training set (with the unbalanced distribution)
# minCl  the minority class label
# per.over/100 is the number of new cases (smoted cases) generated
#              for each rare case. If perc.over < 100 a single case
#              is generated uniquely for a randomly selected perc.over
#              of the rare cases
# k is the number of neighbours to consider as the pool from where
#   the new examples are generated
# perc.under/100 is the number of "normal" cases that are randomly
#                selected for each smoted case
# learner the learning system to use.
# ...  any learning parameters to pass to learner

#使normal case 数量接近全样本的1951911(crash: 278)
#全样本[1] 0.8427695
#1:1  （perc.over=702000,perc.under=100）[1] 0.846487（6个参数）    [1] 0.8467681（所有参数）  [test]0.8636824(random作为测试数据)
#0       1 
#1951560 1951838 
#2:1    (perc.over=351000,perc.under=200) [1] 0.8472263  [1] 0.8474739    [test] 0.8636824
#0       1 
#1951560  976058  
#3:1    (perc.over=234000,perc.under=300) [1] 0.8472371 //0.8475223 0.847495 [1] 0.8474189 ////0.8475557 0.8474585 0.847546  [1] 0.8621841
#0       1 
#1951560  650798 
#4:1    (perc.over=175500,perc.under=400) [1] 0.8470878  [1] 0.8472895   [test]0.8616922
#0       1 
#1951560  488168
#5:1    (perc.over=140400,perc.under=500) [1] 0.8467612  [1] 0.8466822   [test]0.8619335
#0       1 
#1951560  390590 
#6:1   (perc.over=117000,perc.under=600)  [1] 0.8464553  [1] 0.8466822   [test] 0.8613272
#0       1 
#1951560  325538 
#7:1   (perc.over=100286,perc.under=700)  [1] 0.8463788  [1] 0.8465278   [test] 0.8597291
# 0       1 
# 1949892  278834
#1:8   (perc.over=87750,perc.under=800)   [1] 0.8461279  [1] 0.846199    [test] 0.8593637
#0       1 
#1950448  244084 
#9:1   (perc.over=78000,perc.under=900)   [1] 0.8459252  [1] 0.8461497   
#0       1 
#1951560  217118
#10:1  (perc.over=70200,perc.under=1000)  [1] 0.8456668  [1] 0.8456668  [test] 0.8588193
#0       1 
#1951560  195434
#20:1  (perc.over=35100,perc.under=2000)  [1]



#XXX1.XX 所有参数简单抽样
#XXX_1.XX 6个参数简单抽样
#XXX_1_all.XX 6个参数all抽样


#使用06数据作为test
#全样本测试结果 0.8525565 
#smote 1:1      0.8628891 0.8628451
#smote 1：2     0.8623069 0.8623005
#smote 1:3      0.8617211
#4:1 random     0.8599123 0.8599123     0.8621841
#4:1            0.5265872

#05为train使用06数据作为test
#全样本测试结果0.853695
#smote 优化 0.8636891
#


#使normal case 数量接近全样本的7558142(crash: 1210)
#全样本[1] 0.8621841  [random]:0.8639141
#1:1  （perc.over=624600,perc.under=100）[1] 0.8637553（6个参数 全样本test）      [test][1] 0.8659098(random作为测试数据)
#0       1 
#7557660 7558870
#2:1    (perc.over=312300,perc.under=200) [1] 0.8628845     [test] 0.8636824
#0       1 
#7557660 3780040  
#3:1    (perc.over=208200,perc.under=300) [1] 0.8622514   [1] 0.8621841
#0       1 
#7557660 2520430
#4:1    (perc.over=156100,perc.under=400) [1] 0.8617332    [test]0.8636141
#0       1 
#7555240 1890020 
#10:1  (perc.over=62460,perc.under=1000)  [1] 0.8610106   [test] 0.8617395
#0       1 
#7550400  756250 
#20:1  (perc.over=35100,perc.under=2000)  [1] 0.8599053                  0.8601069
#7554000 377700
#30:1    (perc.over=20820,perc.under=3000) [1] 0.8573324                 0.8607496            
#0       1 
#7550400  252890 
#50:1    (perc.over=12492,perc.under=5000) [1] 0.8582532                0.8597023            
#0       1 
#7502000  151250  
#100:1    (perc.over=6246,perc.under=10000) [1] 0.8570745                0.858449            
#0       1 
#7502000   76230

#使normal case control 4840(crash: 1210)
#no smote[1]0.8621841  [random]:0.8639141
#1:1  （prc.over=perc.over=300,perc.under=133.33333）[1] 0.8639759（6个参数 全样本test）      [test][1] 0.8657231(random作为测试数据)
#0       1 
#4839 4840


#使 条件case control 4840(crash: 1210)
#no smote[1]0.4362119  [random]:0.465677
#0       1 
#7280 1368 

#使 条件case control 4840(crash: 1210)
#smote
#1:1  （perc.over=532,perc.under=133.33333）[1] 0.4799393（6个参数 全样本test）      [test][1]0.5104396(random作为测试数据)
#0       1 
#7280 1368
#adasyn
#[all]0.5448539183154137       [random]0.526991655034958


#random case contral ---all data
#smote >>case control-----小样本数据1：4
#random 1:?

#matched case
#6566 6840


#test Data
#0       1 
#7869083    1128