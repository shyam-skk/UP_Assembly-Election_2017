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


cor_up <- subset(cordata[-c(10,14,18,2,3,11)])
#ggcorr(cor_up)
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


########################### RF #####################################
colSums(is.na(train))
str(train)


#install.packages("randomForest")
library(randomForest)
RF <- randomForest(as.factor(Target) ~ ., data = train, 
                   ntree=501, mtry = 5, nodesize =50,
                   importance=TRUE)
print(RF)

plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest train")


RF$err.rate

impVar <- round(randomForest::importance(RF), 2)
impVar[order(impVar[,3], decreasing=TRUE),]

tRF <- tuneRF(x = train[-c(5)], 
              y=as.factor(train$Target),
              mtryStart =5, 
              ntreeTry=50, 
              stepFactor = 1.5, 
              improve = 0.0001, 
              trace = TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 50, 
              importance= TRUE
)

tRF$importance
View(train)


train$predict.class <- predict(tRF, train, type="class")
train$predict.score <- predict(tRF, train, type="prob")
head(train)
class(train$predict.score)

decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}


train$deciles <- decile(train$predict.score[,2])


library(data.table)
tmp_DT = data.table(train)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);


library(scales)
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)

sum(train$Target) / nrow(train)


library(ROCR)
pred <- prediction(train$predict.score[,2], train$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS

## Area Under Curve
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc

## Gini Coefficient
library(ineq)
gini = ineq(train$predict.score[,2], type="Gini")
gini

## Classification Error
with(train, table(Target, predict.class))


## Scoring syntax
val$predict.class <- predict(tRF, val, type="class")
val$predict.score <- predict(tRF, val, type="prob")

val$deciles <- decile(val$predict.score[,2])

tmp_DT = data.table(val)
h_rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round (h_rank$cnt_resp / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),2);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);


library(scales)
#h_rank$rrate <- percent(h_rank$rrate)
#h_rank$cum_rel_resp <- percent(h_rank$cum_rel_resp)
#h_rank$cum_rel_non_resp <- percent(h_rank$cum_rel_non_resp)

View(h_rank)


decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}


val$deciles <- decile(val$predict.score[,2])


library(data.table)
tmp_DT = data.table(val)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);


library(scales)
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)

sum(val$Target) / nrow(val)


library(ROCR)
pred <- prediction(val$predict.score[,2], val$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS

## Area Under Curve
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc

## Gini Coefficient
library(ineq)
gini = ineq(val$predict.score[,2], type="Gini")
gini

## Classification Error
with(val, table(Target, predict.class))


