setwd("C:/Users/SHYAM KRISHNAN K/Desktop/project-5")
getwd()
str(train)
train.logit<-train[-c(3)]
val.logit<-val[-c(3)]

train.logit$Target<-as.numeric(train.logit$Target)
val.logit$Target<-as.numeric(val.logit$Target)

dim(train.logit)
dim(val.logit)
str(train.logit)
str(val.logit)

#Logistic Regression

#install.packages(c("SDMTools","pROC", "Hmisc"))
library(SDMTools)
library(pROC)
library(Hmisc)





# Fit the Sigmoid function
Logit.1<-Target  ~ N0_Votes
logit.plot<-glm(Logit.1, data=train.logit, family=binomial())
summary(logit.plot)

pred.plot.logit <- predict.glm(logit.plot, newdata=val.logit, type="response")
qplot( N0_Votes, pred.plot.logit,data=val.logit, color=Target )



#All Variables
Logit.eq<-Target  ~N0_Votes+total_assest+BJP+OTHERS

Logit <- glm(Logit.eq   , train.logit, family = binomial)
summary(Logit)
vif(Logit)


#####Retain only significant ones
Logit.eq.final<-Target  ~ N0_Votes+BJP+OTHERS
Logit.final <- glm(Logit.eq.final   , train.logit, family = binomial)
summary(Logit.final)
vif(Logit.final)
pred.logit.final <- predict.glm(Logit.final, newdata=val.logit, type="response")

#Classification



tab.logit<-confusion.matrix(val.logit$Target,pred.logit.final,threshold = 0.5)
tab.logit
accuracy.logit<-
  roc.logit<-roc(val.logit$Target,pred.logit.final )
roc.logit
plot(roc.logit)


ROC<- data.frame( Target=val.logit$Target, Fitted = pred.logit.final)
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
