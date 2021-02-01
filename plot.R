library(plyr)
library(gridExtra)
library(e1071)
library(ggplot2)
#install.packages(ROCR)
library(ROCR)
#读数据
data.train <- read.csv("modelData201405.csv")
data.test <- read.csv("modelData201406.csv")
sample.data <-read.csv('sampleData201405.csv')





library(plyr)
library(gridExtra)
library(e1071)
library(ggplot2)
#install.packages(ROCR)
library(ROCR)
#建模
data.train <- data.train[ ,c("Crash","ASC1","AFC1","SSC1","SFC1","TSC1","TFC1","ASU1","AFU1","SSU1","SFU1","TSU1","TFU1",
                             "ASD1","AFD1","SSD1","SFD1","TSD1","TFD1",
                             "ASC2","AFC2","SSC2","SFC2","TSC2","TFC2","ASC3","AFC3","SSC3","SFC3","TSC3","TFC3",
                             "ASC4","AFC4","SSC4","SFC4","TSC4","TFC4","ASU2","AFU2","SSU2","SFU2","TSU2","TFU2",
                             "ASU3","AFU3","SSU3","SFU3","TSU3","TFU3","ASU4","AFU4","SSU4","SFU4","TSU4","TFU4",
                             "ASD2","AFD2","SSD2","SFD2","TSD2","TFD2","ASD3","AFD3","SSD3","SFD3","TSD3","TFD3",
                             "ASD4","AFD4","SSD4","SFD4","TSD4","TFD4")]
data.test <- data.test[ ,c("Crash","ASC1","AFC1","SSC1","SFC1","TSC1","TFC1","ASU1","AFU1","SSU1","SFU1","TSU1","TFU1",
                             "ASD1","AFD1","SSD1","SFD1","TSD1","TFD1",
                             "ASC2","AFC2","SSC2","SFC2","TSC2","TFC2","ASC3","AFC3","SSC3","SFC3","TSC3","TFC3",
                             "ASC4","AFC4","SSC4","SFC4","TSC4","TFC4","ASU2","AFU2","SSU2","SFU2","TSU2","TFU2",
                             "ASU3","AFU3","SSU3","SFU3","TSU3","TFU3","ASU4","AFU4","SSU4","SFU4","TSU4","TFU4",
                             "ASD2","AFD2","SSD2","SFD2","TSD2","TFD2","ASD3","AFD3","SSD3","SFD3","TSD3","TFD3",
                             "ASD4","AFD4","SSD4","SFD4","TSD4","TFD4")]
#factors
f <- Crash ~ ASC1 + ASU2 + TFC1 + SFC1 + SSC1 + SSD1
g <- Crash ~ AFC1 + SSC1 + SFC1 + TSC1 + AFU1 + ASD1 +SSD1 + AFC2 + SFC2 + AFC3
sample.data$Crash <- as.factor(sample.data$Crash) 
data.train$Crash <- as.factor(data.train$Crash) 
train.random.data_4$Crash <- as.factor(train.random.data_4$Crash) 
#####################################
#全样本
mglm <- glm(f, data.train, family = binomial(link = "logit"))
pglm <- predict(mglm, data.test, type = 'response')
pred <- prediction(as.numeric(pglm), as.numeric(data.test$Crash))
perf <- performance(pred, "tpr", "fpr")
performance(pred, "auc")@y.values[[1]]
#####################################
#matched under-sampling
mglm.match <- glm(g, sample.data, family = binomial(link = "logit"))
#mglm.matched <- step(mglm.match)
pglm.matched <- predict(mglm.match, data.test, type = 'response')
pred.matched <- prediction(as.numeric(pglm.matched), as.numeric(data.test$Crash))
perf.matched <- performance(pred.matched, "tpr", "fpr")
performance(pred.matched, "auc")@y.values[[1]]
#####################################
#random under-sampling
mglm.random <- glm(f, train.random.data_4, family = binomial(link = "logit"))
pglm.random <- predict(mglm.random, data.test, type = 'response')
pred.random <- prediction(as.numeric(pglm.random), as.numeric(data.test$Crash))
perf.random <- performance(pred.random, "tpr", "fpr")
performance(pred.random, "auc")@y.values[[1]]
## AUC calculation
(AUC=unlist(performance(pred,"auc")@y.values))
(AUC.matched=unlist(performance(pred.matched,"auc")@y.values))
(AUC.random=unlist(performance(pred.random,"auc")@y.values))


### prepare legend of the ROCs figure   ,sprintf("%.4f", AUC),")"
t=paste("Full set of data (AUC:0.8427)",sep="")
t.matched=paste("Matched Case-control data(AUC:",sprintf("%.4f", AUC.matched),")",sep="")
t.random=paste("Random Under-sampling data(AUC:",sprintf("%.4f", AUC.random),")",sep="")
lgnd=c(t,t.matched,t.random)

jpeg(file="All_matched_random_all.jpg")
## plot ROC
plot(perf)  
lines(perf.matched@x.values[[1]],perf.matched@y.values[[1]],lty=2,col=2)
lines(perf.random@x.values[[1]],perf.random@y.values[[1]],lty=3,col=3)
legend("bottomright",lty=c(1,2,3),col=c(1,2,3),legend=lgnd,text.width=0.7)
dev.off()

#lines(perf@x.values[[1]],perf@y.values[[1]],lty=1,col=1)

#############################################################################
########smote
library(DMwR) 
sample.data$Crash <- as.factor(sample.data$Crash) 
data.train$Crash <- as.factor(data.train$Crash) 
train.random.data_4$Crash <- as.factor(train.random.data_4$Crash) 
f <- Crash ~ ASC1 + ASU2 + TFC1 + SFC1 + SSC1 + SSD1
#####################################
#全样本
data.train.smote <- SMOTE(f,data.train,perc.over=624600,perc.under=100)
table(data.train.smote$Crash)#7557660 7558870 
mglm.smote <- glm(f, data.train.smote, family = binomial(link = "logit"))
pglm.smote <- predict(mglm.smote, data.test, type = 'response')
pred.smote <- prediction(as.numeric(pglm.smote), as.numeric(data.test$Crash))
perf.smote <- performance(pred.smote, "tpr", "fpr")
performance(pred.smote, "auc")@y.values[[1]]
#####################################
#matched under-sampling
sample.data.smote<- SMOTE(Crash~.,sample.data,perc.over=400,perc.under=123)
mglm.matched.smote <- glm(g, sample.data.smote, family = binomial(link = "logit")) #Crash~.
#mglm.matched.smote <- step(mglm.match.smote)
pglm.matched.smote <- predict(mglm.matched.smote, data.test, type = 'response')
pred.matched.smote <- prediction(as.numeric(pglm.matched.smote), as.numeric(data.test$Crash))
perf.matched.smote <- performance(pred.matched.smote, "tpr", "fpr")
performance(pred.matched.smote, "auc")@y.values[[1]]

#####################################
#random under-sampling
train.random.data_4.smote <- SMOTE(f,train.random.data_4,perc.over=300,perc.under=133.33333)
mglm.random.smote <- glm(f, train.random.data_4.smote, family = binomial(link = "logit"))
pglm.random.smote <- predict(mglm.random.smote, data.test, type = 'response')
pred.random.smote <- prediction(as.numeric(pglm.random.smote), as.numeric(data.test$Crash))
perf.random.smote <- performance(pred.random.smote, "tpr", "fpr")
performance(pred.random.smote, "auc")@y.values[[1]]
## AUC calculation
(AUC.smote=unlist(performance(pred.smote,"auc")@y.values))
(AUC.matched.smote=unlist(performance(pred.matched.smote,"auc")@y.values))
(AUC.random.smote=unlist(performance(pred.random.smote,"auc")@y.values))


### prepare legend of the ROCs figure  #
t.smote=paste(" All Data with Smote Optimized(AUC:",sprintf("%.4f", AUC.smote),")",sep="") 
t.matched.smote=paste("Matched Under-sampling with Smote Optimized(AUC:",sprintf("%.4f", AUC.matched.smote),")",sep="")
t.random.smote=paste("Random Under-sampling with Smote Optimized(AUC:",sprintf("%.4f", AUC.random.smote),")",sep="")
lgnd=c(t.smote,t.matched.smote,t.random.smote)

jpeg(file="All_matched_random_smote_all.jpg")
## plot ROC
plot(perf.smote)  
lines(perf.matched.smote@x.values[[1]],perf.matched.smote@y.values[[1]],lty=2,col=2)
lines(perf.random.smote@x.values[[1]],perf.random.smote@y.values[[1]],lty=3,col=3)
legend("bottomright",lty=c(1,2,3),col=c(1,2,3),legend=lgnd,text.width=0.75)
dev.off()

#0.8427
#[1] 0.8558328
#> (AUC.matched=unlist(performance(pred.matched,"auc")@y.values))
#[1] 0.6936717
#> (AUC.random=unlist(performance(pred.random,"auc")@y.values))
#[1] 0.8629565
#smote
#[1] 0.8637551
#[1] 0.7116261
#[1] 0.8637442


###################################################3
#####random
library(ROCR)
#建模
sample.data <- sample.data[ ,c("Crash","ASC1","AFC1","SSC1","SFC1","TSC1","TFC1","ASU1","AFU1","SSU1","SFU1","TSU1","TFU1",
                               "ASD1","AFD1","SSD1","SFD1","TSD1","TFD1",
                               "ASC2","AFC2","SSC2","SFC2","TSC2","TFC2","ASC3","AFC3","SSC3","SFC3","TSC3","TFC3",
                               "ASC4","AFC4","SSC4","SFC4","TSC4","TFC4","ASU2","AFU2","SSU2","SFU2","TSU2","TFU2",
                               "ASU3","AFU3","SSU3","SFU3","TSU3","TFU3","ASU4","AFU4","SSU4","SFU4","TSU4","TFU4",
                               "ASD2","AFD2","SSD2","SFD2","TSD2","TFD2","ASD3","AFD3","SSD3","SFD3","TSD3","TFD3",
                               "ASD4","AFD4","SSD4","SFD4","TSD4","TFD4")]
data.test <- data.test[ ,c("Crash","ASC1","AFC1","SSC1","SFC1","TSC1","TFC1","ASU1","AFU1","SSU1","SFU1","TSU1","TFU1",
                           "ASD1","AFD1","SSD1","SFD1","TSD1","TFD1",
                           "ASC2","AFC2","SSC2","SFC2","TSC2","TFC2","ASC3","AFC3","SSC3","SFC3","TSC3","TFC3",
                           "ASC4","AFC4","SSC4","SFC4","TSC4","TFC4","ASU2","AFU2","SSU2","SFU2","TSU2","TFU2",
                           "ASU3","AFU3","SSU3","SFU3","TSU3","TFU3","ASU4","AFU4","SSU4","SFU4","TSU4","TFU4",
                           "ASD2","AFD2","SSD2","SFD2","TSD2","TFD2","ASD3","AFD3","SSD3","SFD3","TSD3","TFD3",
                           "ASD4","AFD4","SSD4","SFD4","TSD4","TFD4")]
#factors
f <- Crash ~ ASC1 + ASU2 + TFC1 + SFC1 + SSC1 + SSD1
g <- Crash ~ AFC1 + SSC1 + SFC1 + TSC1 + AFU1 + ASD1 +SSD1 + AFC2 + SFC2 + AFC3
sample.data$Crash <- as.factor(sample.data$Crash) 
data.train$Crash <- as.factor(data.train$Crash) 
train.random.data_50$Crash <- as.factor(train.random.data_50$Crash) 
#####################################
#全样本
mglm0.5 <- glm(f, train.random.data_.5, family = binomial(link = "logit"))
pglm <- predict(mglm, test.data, type = 'response')
pred <- prediction(as.numeric(pglm), as.numeric(test.data$Crash))
perf <- performance(pred, "tpr", "fpr")
performance(pred, "auc")@y.values[[1]]
#####################################
#matched under-sampling
mglm.match <- glm(g, sample.data, family = binomial(link = "logit"))
#mglm.matched <- step(mglm.match)
pglm.matched <- predict(mglm.match, data.test, type = 'response')
pred.matched <- prediction(as.numeric(pglm.matched), as.numeric(data.test$Crash))
perf.matched <- performance(pred.matched, "tpr", "fpr")
performance(pred.matched, "auc")@y.values[[1]]
#####################################
#random under-sampling
mglm.random <- glm(f, train.random.data_4, family = binomial(link = "logit"))
pglm.random <- predict(mglm.random, data.test, type = 'response')
pred.random <- prediction(as.numeric(pglm.random), as.numeric(data.test$Crash))
perf.random <- performance(pred.random, "tpr", "fpr")

## AUC calculation
(AUC=unlist(performance(pred,"auc")@y.values))
(AUC.matched=unlist(performance(pred.matched,"auc")@y.values))
(AUC.random=unlist(performance(pred.random,"auc")@y.values))





### prepare legend of the ROCs figure   ,sprintf("%.4f", AUC),")"
t=paste("Full set of data model(AUC:0.8427)",sep="")
t.matched=paste("Matched Case-control Under-sampling model(AUC:",sprintf("%.4f", AUC.matched),")",sep="")
t.random=paste("Random Under-sampling model(AUC:",sprintf("%.4f", AUC.random),")",sep="")
lgnd=c(t,t.matched,t.random)

jpeg(file="All_matched_random_all.jpg")
## plot ROC
plot(perf)  
plot(perf.matched@x.values[[1]],perf.matched@y.values[[1]],lty=2,col=2)
#lines
plot(perf.random@x.values[[1]],perf.random@y.values[[1]],lty=3,col=3)
legend("bottomright",lty=c(1,2,3),col=c(1,2,3),legend=lgnd,text.width=0.7)
dev.off()







#################################################################################
#对比图

#11个AUC值对比
Percent <- c(1,2,3,4,5,6,7,8,9,10)
#AUC <- c(0.8628515,0.8598532, 0.8576861,0.8631142,0.8549751,0.8549026,0.8534909,0.852288)
#radom
#
#all
AUC <- c(0.8441303,0.8442143, 0.8441896,0.8442488,0.8443442,0.8441673,0.844225,0.844235,0.8441673,0.8441327)

data <-data.frame(Percent,AUC)

#plot(data$x,data$y,xlim=c(0,9),ylim = c(0.84,0.85),lwd=2,xlab="smote",ylab="AUC",type="l")
#jpeg(file="AUC_all_.jpg")
#plot(x,y,xlim=c(0,10),ylim = c(0.84,0.85),lwd=2,xlab="smote",ylab="AUC",type="b")
#dev.off()

library(ggplot2)
data$Percent = factor(data$Percent)
jpeg(file="Random_all.jpg")
#plot(x,y,xlim=c(0,8),ylim = c(0.84,0.85),lwd=2,xlab="smote",ylab="AUC",type="l")
ggplot(data,aes(x=Percent,y = AUC,group = 1))+geom_line()+geom_point() +geom_text(label=paste(data$AUC*100,'%',sep=''),colour = "white", vjust=-0.5, size=2)
#text(data$,data$y,data$y,cex=1,pos=3,col="red")
dev.off()







