
####################################################### Linear Probability Model
train.lpm<-train
val.lpm<-val

train.lpm$Target<-as.numeric(train.lpm$Target)
dim(train.lpm)
dim(val.lpm)
str(train.lpm)





####LPM: Multiple Regression
LPM.1<-Target  ~.
LPM.1<-lm(LPM.1,train.lpm[-c(3,11,16,23)])
summary(LPM.1)
vif(LPM.1)

LPM.2<-Target  ~N0_Votes+ total_assest+BJP+BSP+INC+SP
LPM.2<-lm(LPM.2,train.lpm)
summary(LPM.2)
vif(LPM.2)



#Retain only the significant ones
LPM.4<-Target  ~N0_Votes+BJP+BSP+INC+SP
LPM.4<-lm(LPM.4,train.lpm)
summary(LPM.4)
vif(LPM.4)

#Now some Predictions
Pred_LPM <- predict(LPM.4,newdata=val.lpm)
Pred_LPM
#Confusionmatrix
tab.LPM<-table(val.lpm$Target, Pred_LPM > 0.5)
tab.LPM
sum(diag(tab.LPM))/sum(tab.LPM)

#What went right and what went wrong?


val.lpm$LPM<-Pred_LPM
val.lpm$pred.LPM<-ifelse(val.lpm$LPM>0.5,1,0)
val.lpm$correct.LPM<-ifelse(val.lpm$Target-val$pred.LPM==0,1,0)
summary(val.lpm$correct.LPM)


