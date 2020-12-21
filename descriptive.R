setwd("C:/Users/SHYAM KRISHNAN K/Desktop/project-5")
getwd()
library("readxl") 

#up<- read_excel("2017.xlsx")
#write.csv(up,"up2017.csv")
up2017<- read.csv("up.csv")

dim(up2017)
str(up2017)
colSums(is.na(up2017))

attach(up2017)
str(up2017)
summary(up2017)

DIST_NAME<-unique(DIST_NAME)
length(DIST_NAME)


table(Target)
prop.table(table(Target))

table(CAND_SEX)
prop.table(table(CAND_SEX, Target),1)

table(CAND_CATEGORY)
prop.table(table(CAND_CATEGORY, Target),1)

table(PARTY)
prop.table(table(PARTY, Target),1)

table(criminal_case)
prop.table(table(criminal_case, Target),1)

table(education)
prop.table(table(education, Target),1)

table(CAND_SEX,Target)
table(PARTY,Target)




par(mfrow=c(2, 2))

plot(Target ~ criminal_case,xlab='criminal case', ylab='position',col='blue')
plot(POSITION ~ education,xlab='education', ylab='position',col='yellow')
plot(POSITION ~ PARTY,xlab='party', ylab='position',col='brown')
barplot(table(Target,PARTY),main = 'PARTY')
dev.off()

par(mfrow=c(2, 2))
mosaicplot(table(Target,CAND_SEX), color=T)
hist(criminal_case,main="criminal_case ", xlab='criminal_case', ylab=NA,col='orange')
plot(education,main="education ", xlab='education', ylab=NA,col='green')
hist(CAND_AGE,main="candidate Age ", xlab='Age', ylab='frequency',col='gold')
dev.off()



######################################################
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

str(up2017)
qplot(total_assest,  N0_Votes, colour =Target, data=up2017)
qplot(liabilities,  N0_Votes, colour =Target, data=up2017)
ggplot(data=up2017, aes(x=PARTY, y=Target, fill=education)) +
  geom_bar(stat="identity")
#barplot(table(PARTY,education))



############################  variable Selection
#Variables...does gender  explain
boxplot(N0_Votes  ~CAND_SEX)
aov.gender<-aov(N0_Votes  ~CAND_SEX)
#summary(aov.gender)
tk.gender<-TukeyHSD(aov.gender)
tk.gender



#Variables...does category  explain
boxplot(N0_Votes  ~CAND_CATEGORY)
aov.category<-aov(N0_Votes  ~CAND_CATEGORY)
#summary(aov.gender)
tk.category<-TukeyHSD(aov.category)
tk.category

#Variables...does party  explain
boxplot(N0_Votes  ~PARTY)
aov.category<-aov(N0_Votes  ~PARTY)
#summary(aov.gender)
tk.category<-TukeyHSD(aov.category)
tk.category

#Variables...does education  explain
boxplot(N0_Votes  ~education)
aov.education<-aov(N0_Votes  ~education)
#summary(aov.gender)
tk.education<-TukeyHSD(aov.education)
tk.education



################################# Define some dummies
up<-up2017
dim(up)
str(up)
View(up)

levels(PARTY)
levels(education)

up$Male<-ifelse(up$CAND_SEX=="M",1,0)
up$Female<-ifelse(up$CAND_SEX=="F",1,0)
up$Gender_NOTA<-ifelse(up$CAND_SEX=="NULL",1,0)

up$GEN<-ifelse(up$CAND_CATEGORY=="GEN",1,0)
up$SC<-ifelse(up$CAND_CATEGORY=="SC",1,0)
up$ST<-ifelse(up$CAND_CATEGORY=="ST",1,0)
up$Category_NOTA<-ifelse(up$CAND_CATEGORY=="NULL",1,0)

up$BJP<-ifelse(up$PARTY=="BJP",1,0)
up$BSP<-ifelse(up$PARTY=="BSP",1,0)
up$INC<-ifelse(up$PARTY=="INC",1,0)
up$NOTA<-ifelse(up$PARTY=="NOTA",1,0)
up$OTHERS<-ifelse(up$PARTY=="OTHERS",1,0)
up$SP<-ifelse(up$PARTY=="SP",1,0)

up$TWelve_Pass<-ifelse(up$education=="12th Pass",1,0)
up$Doctorate<-ifelse(up$education=="Doctorate",1,0)
up$Graduate<-ifelse(up$education=="Graduate",1,0)
up$Illiterate<-ifelse(up$education=="Illiterate",1,0)
up$Literate<-ifelse(up$education=="Literate",1,0)
up$PG<-ifelse(up$education=="Post Graduate",1,0)

dim(up)
str(up)
colSums(is.na(up2017))

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
ggcorr(cordata)


cor_up <- subset(cordata[-c(10,14,18)])
ggcorr(cor_up)
dim(cor_up)
str(cor_up)

################## partitioning dataset
set.seed(1234)
pd<-sample(2,nrow(cor_up),replace=TRUE, prob=c(0.7,0.3))


######## continues dataset
train<-cor_up[pd==1,]
val<-cor_up[pd==2,]

######## complete dataset
train_up<-up[pd==1,]
val_up<-up[pd==2,]

############# spliting the dataset
dim(train)
dim(val)
c(nrow(train), nrow(val))

table(train$Target)
prop.table(table(train$Target))
table(val$Target)
prop.table(table(val$Target))


########################### CART #####################################

