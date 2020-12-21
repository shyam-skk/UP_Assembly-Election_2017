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

library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

r.ctrl = rpart.control(minsplit=10, minbucket =5, cp = 0, xval = 10)
m3 <- rpart(formula = train$Target ~ ., 
            data =train[-c(2,3,7)], method = "class", 
            control = r.ctrl)
m3
fancyRpartPlot(m3)
printcp(m3)
plotcp(m3)
ptree<- prune(m3, cp= 0.0034,"CP")
printcp(ptree)
plotcp(ptree)
fancyRpartPlot(ptree, uniform=TRUE,  main="Pruned Classification Tree")
train$predict.class <- predict(ptree, train, type="class")
train$predict.score <- predict(ptree, train, type="prob")
View(head(train))


############################## deciling code
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
class(train$predict.score)
## deciling
train$deciles <- decile(train$predict.score[,2])
#View(train)

######################################## Ranking code
#install.packages("data.table")
#install.packages("scales")
library(data.table)
library(scales)
tmp_DT = data.table(train)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp / rank$cnt,4);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),4);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),4);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp) * 100;
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)
View(rank)
##install.packages("ROCR")
##install.packages("ineq")
library(ROCR)
library(ineq)
pred <- prediction(train$predict.score[,2], train$Target)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
gini = ineq(train$predict.score[,2], type="Gini")
with(train, table(Target, predict.class))
auc
KS
gini
## Syntax to get the node path
tree.path <- path.rpart(ptree, node = c(2, 12))
nrow(val)
##################################### Scoring Holdout sample 
val$predict.class <- predict(ptree, val, type="class")
val$predict.score <- predict(ptree, val, type="prob")
val$deciles <- decile(val$predict.score[,2])
## Ranking code
tmp_DT = data.table(val)
h_rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round(h_rank$cnt_resp / h_rank$cnt,4);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),4);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),4);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp)*100;
h_rank$rrate <- percent(h_rank$rrate)
h_rank$cum_rel_resp <- percent(h_rank$cum_rel_resp)
h_rank$cum_rel_non_resp <- percent(h_rank$cum_rel_non_resp)
View(h_rank)
pred <- prediction(val$predict.score[,2], val$Target)
perf <- performance(pred, "tpr", "fpr")
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
gini = ineq(val$predict.score[,2], type="Gini")
with(val, table(Target, predict.class))
auc
KS
gini

