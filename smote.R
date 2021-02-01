#install.packages("DMwR")
#install.packages("caret")
#install.packages("pROC")

#############################
#---------------------------------------------------#
#程序说明：类别不平衡问题处理
#---------------------------------------------------#


# 加载数据，删除冒号和句号，并追加列名 
hyper <-read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.data', header=F) 
names <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.names', header=F, sep='\t')[[1]] 
# [1] hypothyroid, negative.     age:                       sex:                       on_thyroxine:              query_on_thyroxine:        on_antithyroid_medication: thyroid_surgery:          
#[8] query_hypothyroid:         query_hyperthyroid:        pregnant:                  sick:                      tumor:                     lithium:                   goitre:                   
#[15] TSH_measured:              TSH:                       T3_measured:               T3:                        TT4_measured:              TT4:                       T4U_measured:             
#[22] T4U:                       FTI_measured:              FTI:                       TBG_measured:              TBG:                      
#26 Levels: age: FTI_measured: FTI: goitre: hypothyroid, negative. lithium: on_antithyroid_medication: on_thyroxine: pregnant: query_hyperthyroid: query_hypothyroid: query_on_thyroxine: sex: ... tumor:
#
names <- gsub(pattern =":|[.]", replacement="", x = names) 
#gsub是用于字符串替换的函数
#[1] "hypothyroid, negative"     "age"                       "sex"                       "on_thyroxine"              "query_on_thyroxine"        "on_antithyroid_medication" "thyroid_surgery"          
#[8] "query_hypothyroid"         "query_hyperthyroid"        "pregnant"                  "sick"                      "tumor"                     "lithium"                   "goitre"                   
#[15] "TSH_measured"              "TSH"                       "T3_measured"               "T3"                        "TT4_measured"              "TT4"                       "T4U_measured"             
#[22] "T4U"                       "FTI_measured"              "FTI"                       "TBG_measured"              "TBG"                      

colnames(hyper)<-names 

# 我们将第一列的列名从 hypothyroid, negative改成target，并将negative变成0，其他值变成1. 
colnames(hyper)[1]<-"target" 
#colnames(x) <- value
colnames(hyper) 


hyper$target<-ifelse(hyper$target=="negative",0,1) 
# 检查下阳性和阴性的结果 
table(hyper$target) 
#   0    1 
#3012  151 
prop.table(table(hyper$target)) 
#x$y  等价于 x[["y", exact = FALSE]]，用于获取dataframe或者list里面的某个变量
#  @ 是R中，S4类的一个操作符，用于提取S4对象中的内容（slot）
#0          1 
#0.95226051 0.04773949 

head(hyper,2) 
# 可以发现这数据都是因子型变量（字符型的值），这些都需要转换成二值化的数字，以方便建模： 
ind<-sapply(hyper,is.factor) 
hyper[ind]<-lapply(hyper[ind],as.character) 
#sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
hyper[hyper=="?"]=NA 
#Not Available’ / Missing Values
hyper[hyper=="f"]=0 
hyper[hyper=="t"]=1 
hyper[hyper=="n"]=0 
hyper[hyper=="y"]=1 
hyper[hyper=="M"]=0 
hyper[hyper=="F"]=1 

hyper[ind]<-lapply(hyper[ind],as.numeric) 
#全部化成数值了

#用均值代替缺失值
replaceNAWithMean<-function(x) {replace(x,is.na(x),mean(x[!is.na(x)]))} 
hyper<-replaceNAWithMean(hyper)

#############################################################################
#模型研究 
#我们利用caret包中的createDataPartition（数据分割功能）函数将数据随机分成相同的两份。 
library(caret) 
## Loading required package: lattice 
## Loading required package: ggplot2 
set.seed(1234) 
#Random Number Generation  set.seed(seed, kind = NULL, normal.kind = NULL)
#在r中取sample时候，经常会有set.seed(某数)，经常看见取值很大，其实这里无论括号里取值是多少，想要上下两次取值一样，都需要在每次取值前输入同样的set.seed(某数)，才能保证两次取值相同。
splitIndex <- createDataPartition(hyper$target,time=1,p=0.5,list=FALSE) 
trainSplit <- hyper[splitIndex,] 
testSplit <- hyper[-splitIndex,] 
#分离成两半，一半做train 一半做test
prop.table(table(trainSplit$target)) 
#0          1 
#0.95006321 0.04993679 

#我们利用caret包中的treebag模型算法，对训练集数据建立模型，并对测试集数据进行预测。 
ctrl <- trainControl(method="cv",number=5) 
#Control parameters for train？？？？？？
tbmodel <- train(target~.,data=trainSplit,method="treebag", 
                 trControl=ctrl) 
#Warning message:
#In train.default(x, y, weights = w, ...) :
#You are trying to do regression and your outcome only has two possible values Are you trying to do classification? If so, use a 2 level factor as your outcome column.

predictors <- names(trainSplit)[names(trainSplit)!='target'] 
pred <- predict(tbmodel$finalModel,testSplit[,predictors]) 


#为了评估模型，我们用pROC包的roc函数算auc得分和画图 
library(pROC) 
auc<-roc(testSplit$target,pred) 
print(auc) 
#Call:
#  roc.default(response = testSplit$target, predictor = pred)

#Data: pred in 1509 controls (testSplit$target 0) < 72 cases (testSplit$target 1).
#Area under the curve: 0.9854

plot(auc,ylim=c(0,1),print.thres=TRUE,main=paste('AUC',round(auc$auc[[1]],2))) 
abline(h=1,col="blue",lwd=2) 
abline(h=0,col="red",lwd=2) 



#使用SMOTE来平衡
library(DMwR) 
trainSplit$target <- as.factor(trainSplit$target) 
trainSplit <- SMOTE(target~.,trainSplit,perc.over=100,perc.under=200) 
trainSplit$target <- as.numeric(trainSplit$target) 
# 我们再次用prop.table()函数检查结果的平衡性，确定我们已经让阴性、阳性数据达到相同。 
prop.table(table(trainSplit$target)) 


# 再次建立treebag模型 
tbmodel<-train(target~.,data=trainSplit,method="treebag", 
               trControl=ctrl) 
predictors <- names(trainSplit)[names(trainSplit)!='target'] 
pred <- predict(tbmodel$finalModel,testSplit[,predictors]) 
auc <- roc(testSplit$target,pred) 
print(auc) 

plot(auc,ylim=c(0,1),print.thres=TRUE,
     main=paste('AUC',round(auc$auc[[1]],2))) 


abline(h=1,col="blue",lwd=2) 
abline(h=0,col="red",lwd=2)