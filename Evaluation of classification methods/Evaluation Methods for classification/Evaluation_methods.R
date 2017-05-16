## Step 3...
getwd()
setwd("E:/Spring 2017/Advanced BA/Assignment_2")
data <- read.csv("election_campaign_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?")) 
str(data)

## Step 5 ...
data$cand_id <- NULL
data$last_name <- NULL
data$first_name <- NULL
data$twitterbirth <- NULL
data$facebookdate <- NULL
data$facebookjan <- NULL
data$youtubebirth <- NULL

##Step 6....
data$twitter <- as.factor(data$twitter)
data$facebook <- as.factor(data$facebook)
data$youtube <- as.factor(data$youtube)
data$cand_ici <- as.factor(data$cand_ici)
data$gen_election <- as.factor(data$gen_election)

##Step7
set.seed(32)
datax <- data[complete.cases(data), ]

## Step 8
n = nrow(datax) # n will be ther number of obs. in data
trainIndex = sample(1:n, 
                    size = round(0.7*n), 
                    replace=FALSE)
train_data = datax[trainIndex,] 
test_data = datax[-trainIndex,]

##Step 9
library(randomForest)
rf <-randomForest(gen_election~., data=train_data, ntree=100, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

## Step 9.6

mtry <- tuneRF(train_data[-26], train_data$gen_election, ntreeTry=80,  stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE, na.action=na.exclude)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

## Step 9.7
set.seed(32)
rf <-randomForest(gen_election~., data=train_data, mtry = 4, ntree=80, na.action=na.exclude, importance=T,
                  proximity=T) 
print(rf)

##Step 9.8

library(caret)
install.packages("plyr")
library(plyr)
predicted_values <- predict(rf, test_data,type= "prob")
head(predicted_values)
threshold <- 0.5
pred <- factor( ifelse(predicted_values[,2] > threshold, "W","L") )
head(pred)
levels(test_data$gen_election)[2]
confusionMatrix(pred, test_data$gen_election,positive = levels(test_data$gen_election)[2])
install.packages("e1071")

##Step 9.9

install.packages("ROCR")
install.packages("plyr")
library(ROCR)
library(ggplot2)
library(randomForest)
predicted_values <- predict(rf, test_data,type= "prob")[,2]
pred <- prediction(as.vector( predicted_values), as.vector(test_data$gen_election))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="RF")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

##Step 9.10

getwd()
importance(rf)
varImpPlot(rf)

##Step 10
library(nnet)
set.seed(32)
ann <- nnet(gen_election ~ ., data=train_data, size=5, maxit=1000)
summary(ann)
predicted_values <- predict(ann, test_data,type= "raw")
head(predicted_values)
threshold <- 0.5 
pred <- factor( ifelse(predicted_values[,1] > threshold, "W", "L") )
head(pred)
library(ROCR)
confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2])

##Step 10.1.6

predicted_values <- predict(ann, test_data,type= "raw")
pred <- prediction(as.vector(predicted_values), as.vector(test_data$gen_election))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

##Step 10.2

set.seed(32)
ann <- nnet(gen_election ~ ., data=train_data, size=24, maxit=1000)
summary(ann)
predicted_values <- predict(ann, test_data,type= "raw")
head(predicted_values)
threshold <- 0.5 
pred <- factor( ifelse(predicted_values[,1] > threshold, "W", "L") )
head(pred)
library(caret)
confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2])


predicted_values <- predict(ann, test_data,type= "raw")
pred <- prediction(as.vector(predicted_values), as.vector(test_data$gen_election))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

## Step 11
set.seed(32)
gbm_caret <- train(as.factor(gen_election) ~ ., data = train_data, method = "gbm", trControl = trainControl(method = "repeatedcv", number = 4, repeats = 4),verbose = FALSE)
summary(gbm_caret)

## Step 11.2
predicted_values <- predict(gbm_caret, test_data,type= "prob")
head(predicted_values)
threshold <- 0.5
pred <- factor( ifelse(predicted_values[,2] > threshold, "W","L") )
head(pred)
levels(test_data$gen_election)[2]
confusionMatrix(pred, test_data$gen_election,positive = levels(test_data$gen_election)[2])

## Step 11.3
predicted_values <- predict(gbm_caret, test_data,type= "prob")[,2]
pred <- prediction(as.vector( predicted_values), as.vector(test_data$gen_election))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GBM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))


## Step 13

ftable(xtabs(~youtube+gen_election, data=datax))
