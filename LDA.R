####################### correlation matrix
cordata <- subset(up[-c(1:6,8,12)])
library(mlbench)
library(caret)


correlationMatrix <- cor(cordata)
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
print(highlyCorrelated)


cor_up <- subset(cordata[-c(10,14,18)])

dim(cor_up)
str(cor_up)

################## partitioning dataset
set.seed(1234)
pd<-sample(2,nrow(cor_up),replace=TRUE, prob=c(0.7,0.3))


######## continues dataset
train<-cor_up[pd==1,]
val<-cor_up[pd==2,]

str(train)
dim(train)
statesf<-train[,c(2,3,4,5,13,16)]
str(statesf)
statesf$POSITION1<-factor(statesf$POSITION)


library("car")
library("rattle")
library("MASS")
library("gdata")
library("lawstat")
library("psych")
library("OpenMx")
library("compute.es")
library("effects")
library("ggplot2")
library("multcomp")
library("pastecs")
library("nlme")
library("biotools")
library("mvoutlier")
library("mvnormtest")
library("plyr")
library("reshape2")
library("expm")
library("Matrix")



boxM(statesf[,c(1,3:5)], statesf$POSITION1)
region.lda <- lda(POSITION1 ~N0_Votes+criminal_case+total_assest+BJP+OTHERS, data=statesf)
region.lda
region.lda$svd #

statesf$PO1<- as.numeric(ifelse ( ( statesf$POSITION1 == "1" ), 1 , 0 ) )
statesf$PO2<- as.numeric(ifelse ( ( statesf$POSITION1 == "2" ), 1 , 0 ))
statesf$PO3<- as.numeric(ifelse ( ( statesf$POSITION1 == "3" ), 1 , 0 ))
statesf$PO4<- as.numeric(ifelse ( ( statesf$POSITION1 == "4" ), 1 , 0 ))

str(statesf)

length(statesf$POSITION1)
levells<-nlevels(statesf$POSITION1)
levells
Z1<-c("PO1","PO2","PO3","PO4")
Z2<-c("N0_Votes","criminal_case", "total_assest","BJP","OTHERS")

H<-as.matrix(data.frame(statesf[,c(Z1)]))
X<-as.matrix(data.frame(statesf[,c(Z2)]))


###Group Centroids
xg<-ginv((t(H)%*%H))%*%t(H)%*%X
xg
###Grand Mean
oness<- matrix(1,35,1)
length(oness)
dim(oness)
grandmeanX<-(as.matrix(t(oness)%*%X))/nrow(oness)
grandmeanX
