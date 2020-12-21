setwd("C:/Users/SHYAM KRISHNAN K/Desktop/project-5")
getwd()


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


Loan<-read.csv(file.choose(), header=T)
#na.omit(Loan)

summary(Loan)
str(Loan)

levels(Loan$Purpose)
levels(Loan$Job)
#Define some dummies
Loan$Default<-ifelse(Loan$Status=="Default",1,0)
Loan$Female<-ifelse(Loan$Gender=="Female",1,0)
Loan$Management<-ifelse(Loan$Job=="Management",1,0)
Loan$Skilled<-ifelse(Loan$Job=="skilled",1,0)
Loan$Unskilled<-ifelse(Loan$Job=="unskilled",1,0)


Loan$CH.Poor<-ifelse(Loan$Credit.History=="Poor",1,0)
Loan$CH.critical<-ifelse(Loan$Credit.History=="critical",1,0)
Loan$CH.good<-ifelse(Loan$Credit.History=="good",1,0)
Loan$CH.verygood<-ifelse(Loan$Credit.History=="very good",1,0)

Loan$Purpose.car<-ifelse(Loan$Purpose=="car",1,0)
Loan$Purpose.cd<-ifelse(Loan$Purpose=="consumer.durable",1,0)
Loan$Purpose.education<-ifelse(Loan$Purpose=="education",1,0)
Loan$Purpose.personal<-ifelse(Loan$Purpose=="personal",1,0)


#PLOTS

str(Loan)
# qplot()
qplot(Credit.Score,  EMI.Ratio, colour = Status, data=Loan)
qplot(Work.Exp,  EMI.Ratio, colour = Status, data=Loan)


#Scatter Plots and Correlation



Loan.Scatter<-subset(Loan[,c(2,4:6,9,11:24)])


cor(Loan.Scatter)
#install.packages("corrplot")
library(corrplot)

correlations<- cor(Loan.Scatter)

corrplot(correlations, method="circle")





####variable Selection
#Variables...does borrowing purpose explain
boxplot(Loan$EMI.Ratio  ~Loan$Purpose)
aov.Purpose<-aov(Loan$EMI.Ratio  ~Loan$Purpose)
summary(aov.Purpose)
tk.Purpose<-TukeyHSD(aov.Purpose)
tk.Purpose



#Variables...does Job type  explain
boxplot(Loan$EMI.Ratio  ~Loan$Job)
aov.Job<-aov(Loan$EMI.Ratio  ~Loan$Job)
summary(aov.Job)
tk.Job<-TukeyHSD(aov.Job)
tk.Job



#######Performing Regression

#Partitioning Data Sets
#Partition train and val
#We will use this throughout so that samples are comparable
set.seed(1234)
pd<-sample(2,nrow(Loan),replace=TRUE, prob=c(0.7,0.3))



train<-Loan[pd==1,]
val<-Loan[pd==2,]


sum(Loan$Default)
sum(val$Default)
sum(train$Default)


#Data Frame for Linear Regression

train.reg<-train[,c(2,4:7,9,11,13:24)]
val.reg<-val[,c(2,4:7,9,11,13:24)]

str(train.reg)


###Now some basic commands

Linear.1<-EMI.Ratio  ~ Credit.Score



OLS.1<-lm(Linear.1, data=train.reg)
summary (OLS.1)

# Fit regression line
train.plot<-train.reg[,c(3,4)]
val.plot<-val.reg[,c(3,4)]
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



#add one variable
Linear.2<-EMI.Ratio  ~ Credit.Score+Female

OLS.2<-lm(Linear.2, data=train.reg)
summary (OLS.2)
#try another
Linear.3<-EMI.Ratio  ~ Credit.Score+CH.Poor


OLS.3<-lm(Linear.3, data=train.reg)
summary (OLS.3)

#We can try adding.....
#Or Start from the full Model and try deleting

#Should we scale the features?

normalize<-function(x){
  +return((x-min(x))/(max(x)-min(x)))}
train.reg$norm.Credit.Score<-normalize(train.reg$Credit.Score)

OLS.norm<-lm(EMI.Ratio~norm.Credit.Score, data=train.reg)
summary(OLS.norm)

####As an exercise try other scaling


###Multiple Regression with full Model

OLS.full<-lm(EMI.Ratio~., data=train.reg)
summary(OLS.full)

######

Linear.4<-EMI.Ratio  ~Loan.Offered+ Work.Exp+Credit.Score+Own.house+ Dependents+Female+Skilled+CH.verygood+CH.good+CH.critical+Purpose.education+Purpose.car



OLS.4<-lm(Linear.4,data=train.reg)
summary(OLS.4)
vif(OLS.4)

Linear<-EMI.Ratio  ~Loan.Offered+ Credit.Score+Own.house+ Dependents+Female+Skilled+CH.verygood+CH.good+CH.critical+Purpose.education+Purpose.car

OLS<-lm(Linear,data=train.reg)
summary(OLS)
vif(OLS)


#Drop insignificant ones
Linear.final<-EMI.Ratio  ~Loan.Offered+ Credit.Score+Dependents+Skilled+Unskilled+CH.good+CH.critical+Purpose.car


OLS.final<-lm(Linear.final,data=train.reg)
summary(OLS.final)
vif(OLS.final)

pred.reg<-predict(OLS.final,newdata=val.reg, interval="predict")
pred.reg


head(pred.reg)
head(val.reg)
head(train.reg)
mse1 <- mean((val.reg$EMI.Ratio- pred.reg)^2)
print(mse1)

###########
#Linear Probability Model
train.logit<-train[,c(2,4:7,9,11:24)]
val.lpm<-val[,c(2,4:7,9,11:24)]

LPM.1<-Default  ~ Credit.Score


OLS.LPM.1<-lm(LPM.1,train.lpm)
summary(OLS.LPM.1)


# Fit regression line
train.plot.LPM<-train[,c(5,12)]
val.plot.LPM<-val[,c(5,12)]
OLS.plot.LPM<-lm(LPM.1, data=train.plot.LPM)
summary(OLS.plot.LPM)
par(mgp=c(2,1,0), mar=c(3,3,1,1))

require(stats)
#reg<-lm(dist ~ speed, data = cars)
coeff=coefficients(OLS.plot.LPM)
# equation of the line : 
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
# plot
plot(train.plot.LPM, main=eq)
abline(OLS.plot.LPM, col="blue")
plot(val.plot.LPM, main=eq)
abline(OLS.plot.LPM, col="red")



####LPM: Multiple Regression


LPM.2<-Default  ~Loan.Offered+ Work.Exp+Credit.Score+Own.house+Dependents+Female+Skilled+Unskilled+CH.verygood+CH.good+CH.critical+Purpose.personal+Purpose.education+Purpose.car
LPM.2<-lm(LPM.2,train.lpm)
summary(LPM.2)
vif(LPM.2)

#Drop variables

LPM.3<-Default  ~Credit.Score+ Own.house+ Dependents+Female+Skilled+Unskilled+CH.verygood+CH.good+CH.critical+Purpose.personal+Purpose.education+Purpose.car
LPM.3<-lm(LPM.3,train.lpm)
summary(LPM.3)
vif(LPM.3)

#Retain only the significant ones
LPM.4<-Default  ~Credit.Score+ Own.house+ Dependents+Female+Skilled
LPM.4<-lm(LPM.4,train.lpm)
summary(LPM.4)
vif(LPM.4)

#Now some Predictions
Pred_LPM <- predict(LPM.4,newdata=val.lpm)
Pred_LPM
#Confusionmatrix
tab.LPM<-table(val.lpm$Default, Pred_LPM > 0.5)
tab.LPM
sum(diag(tab.LPM))/sum(tab.LPM)

#What went right and what went wrong?


val.lpm$LPM<-Pred_LPM
val.lpm$pred.LPM<-ifelse(val.lpm$LPM>0.5,1,0)
val.lpm$correct.LPM<-ifelse(val.lpm$Default-val$pred.LPM==0,1,0)
summary(val.lpm$correct.LPM)



#Logistic Regression

install.packages(c("SDMTools","pROC", "Hmisc"))
library(SDMTools)
library(pROC)
library(Hmisc)

#data frame
train.logit<-train[,c(2,4:6,9,11:24)]
val.logit<-val[,c(2,4:6,9,11:24)]







# Fit the Sigmoid function
Logit.1<-Default  ~ Credit.Score
logit.plot<-glm(Logit.1, data=train.logit, family=binomial())
summary(logit.plot)

pred.plot.logit <- predict.glm(logit.plot, newdata=val.logit, type="response")
qplot( Credit.Score, pred.plot.logit,data=val.logit, color=Default )



#All Variables
Logit.eq<-Default  ~Loan.Offered+ Work.Exp+Credit.Score+Own.house+ Dependents+Female+Skilled+Unskilled+CH.verygood+CH.good+Purpose.personal+Purpose.education

Logit <- glm(Logit.eq   , train.logit, family = binomial)
summary(Logit)
vif(Logit)

Logit.eq.1<-Default  ~ Own.house+ Female+Skilled+Unskilled+CH.verygood+CH.good+CH.critical+Purpose.personal+Purpose.education
Logit.1 <- glm(Logit.eq.1   , train.logit, family = binomial)
summary(Logit.1)
vif(Logit.1)
#####Retain only significant ones
Logit.eq.final<-Default  ~ Own.house+ Skilled+CH.verygood+CH.good+CH.critical+Purpose.personal+Purpose.education
Logit.final <- glm(Logit.eq.final   , train.logit, family = binomial)
summary(Logit.final)
vif(Logit.final)
pred.logit.final <- predict.glm(Logit.final, newdata=val.logit, type="response")

#Classification



tab.logit<-confusion.matrix(val.logit$Default,pred.logit.final,threshold = 0.5)
tab.logit
accuracy.logit<-
roc.logit<-roc(val.logit$Default,pred.logit.final )
roc.logit
plot(roc.logit)


ROC<- data.frame( Default=val.logit$Default, Fitted = pred.logit.final)
write.csv(ROC, file = "ROC.csv")

val.logit$logit<-pred.logit
val.logit$pred.logit<-ifelse(val$logit>0.5,1,0)
val.logit$correct.logit<-ifelse(val$Car-val$pred.logit==0,1,0)
summary(val.logit$correct.logit)


####How did the two fair?
#LPM
accuracy.LPM<-sum(diag(tab.LPM))/sum(tab.LPM)
accuracy.LPM
loss.LPM<-tab.LPM[2,1]/(tab.LPM[2,1]+tab.LPM[1,1])
loss.LPM
opp.loss.LPM<-tab.LPM[1,2]/(tab.LPM[1,2]+tab.LPM[2,2])
opp.loss.LPM
tot.loss.LPM<-0.95*loss.LPM+0.05*opp.loss.LPM
tot.loss.LPM
#Logit
accuracy.logit<-sum(diag(tab.logit))/sum(tab.logit)
accuracy.logit
loss.logit<-tab.logit[1,2]/(tab.logit[1,2]+tab.logit[1,1])
loss.logit
opp.loss.logit<-tab.logit[2,1]/(tab.logit[2,1]+tab.logit[2,2])
opp.loss.logit
tot.loss.logit<-0.95*loss.logit+0.05*opp.loss.logit
tot.loss.logit
