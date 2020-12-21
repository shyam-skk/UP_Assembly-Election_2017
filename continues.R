####################### correlation matrix
cordata <- subset(up[-c(1:6,8,12)])
library(mlbench)
library(caret)


correlationMatrix <- cor(cordata)
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
print(highlyCorrelated)


#install.packages('GGally')
library('GGally')
#ggcorr(cordata)


cor_up <- subset(cordata[-c(10,14,18)])
#ggcorr(cor_up)
dim(cor_up)
str(cor_up)

################## partitioning dataset
set.seed(1234)
pd<-sample(2,nrow(cor_up),replace=TRUE, prob=c(0.7,0.3))


######## continues dataset
train<-cor_up[pd==1,]
val<-cor_up[pd==2,]

train$N0_Votes<-as.numeric(train$N0_Votes)
val$N0_Votes<-as.numeric(val$N0_Votes)
######## complete dataset
train_up<-up[pd==1,]
val_up<-up[pd==2,]

############# spliting the dataset
str(train)
dim(val)
c(nrow(train), nrow(val))

table(train$Target)
prop.table(table(train$Target))
table(val$Target)
prop.table(table(val$Target))


str(cor_up)
str(train)
########################### regression #####################################

library(car)
library(caret)
library(class)
library(devtools)
library(e1071)
library(ggord)
library(ggplot2)
library(Hmisc)
library(klaR)
library(klaR)
library(MASS)
library(nnet)
library(plyr)
library(pROC)
library(psych)
library(scatterplot3d)
library(SDMTools)
library(dplyr)
library(ElemStatLearn)
library(rpart)
library(rpart.plot)
library(randomForest)
library(neuralnet)


###Multiple Regression with full Model

OLS.full<-lm(train$N0_Votes~., data=train[-c(7,3,2)])
summary(OLS.full)

######

Linear.final<-train$N0_Votes  ~train$Male+train$Female+train$BJP+train$BSP+train$OTHERS


OLS.final<-lm(Linear.final,data=train)
summary(OLS.final)
vif(OLS.final)

pred.reg<-predict(OLS.final,newdata=val, interval="predict")
pred.reg

View(val)
head(pred.reg)
head(val)
head(train)
mse1 <- mean((val$N0_Votes- pred.reg)^2)
print(mse1)

###########

###Now some basic commands

Linear.1<-train$N0_Votes  ~ train$POSITION



OLS.1<-lm(Linear.1, data=train)
summary (OLS.1)

# Fit regression line
train.plot<-train[,c(2,3)]
val.plot<-val[,c(2,3)]
OLS.plot<-lm(Linear.1, data=train.plot)
summary(OLS.plot)
par(mgp=c(2,1,0), mar=c(3,3,1,1))

require(stats)
#reg<-lm(dist ~ speed, data = cars)
coeff=coefficients(OLS.plot)
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
# plot
plot(train.plot, main=eq)
abline(OLS.plot, col="blue")
plot(val.plot, main=eq)
abline(OLS.plot, col="red")

