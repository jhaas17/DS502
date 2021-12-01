rm(list=ls())

library(ggplot2)
library(dplyr)
library('pROC')

news <- read.csv("final_data.csv")
nrow(news)
news <- na.omit(news)
news<- subset(news, select=-c(url, timedelta))


#change categorical variable to as factor
news$data_channel_is_lifestyle <- as.factor(news$data_channel_is_lifestyle)
news$data_channel_is_entertainment <- as.factor(news$data_channel_is_entertainment)
news$data_channel_is_bus <- as.factor(news$data_channel_is_bus)
news$data_channel_is_socmed <- as.factor(news$data_channel_is_socmed)
news$data_channel_is_tech <- as.factor(news$data_channel_is_tech)
news$data_channel_is_world <- as.factor(news$data_channel_is_world)

news$weekday_is_monday <- as.factor(news$weekday_is_monday)
news$weekday_is_tuesday <- as.factor(news$weekday_is_tuesday)
news$weekday_is_wednesday <- as.factor(news$weekday_is_wednesday)
news$weekday_is_thursday <- as.factor(news$weekday_is_thursday)
news$weekday_is_friday <- as.factor(news$weekday_is_friday)
news$weekday_is_saturday <- as.factor(news$weekday_is_saturday)
news$weekday_is_sunday <- as.factor(news$weekday_is_sunday)
news$is_weekend <- as.factor(news$is_weekend)

news$shares <- as.factor(news$shares)

news=rapply(news,scale,c("numeric","integer"),how="replace")
news.numeric <- news[, sapply(news, is.numeric)]

#split train and test
train = sample(dim(news)[1],dim(news)[1]*0.7)
news.train = news[train,]
news.test = news[-train,]

XTrain = model.matrix(shares~.,news.train)
XTest = model.matrix(shares~.,news.test)
YTrain = news.train$shares
YTest = news.test$shares

XTrain

#KNN
knnFit = knn(XTrain, XTest, YTrain, k=10)

tabknn <- table(knnFit,YTest)
tabknn

accuracyknn <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracyknn(tabknn)

#plot(roc(test$y, predict(mymodel, test, type = "prob"))
     
#Logistic Regression

logisticFit = glm(shares ~ ., data=news.train,family=binomial)
summary(logisticFit)

logisticFit1 = glm(shares ~ n_tokens_content+n_tokens_content+n_non_stop_unique_tokens
                   +num_self_hrefs+num_imgs+data_channel_is_entertainment
                   +data_channel_is_bus
                   +data_channel_is_socmed
                   +data_channel_is_tech
                   +kw_min_min+kw_avg_max+kw_min_avg+kw_max_avg
                   +kw_avg_avg
                   +weekday_is_monday
                   +weekday_is_tuesday
                   +weekday_is_wednesday
                   +weekday_is_thursday
                   +weekday_is_friday
                   +weekday_is_saturday
                   +global_subjectivity
                   +avg_negative_polarity
                   +min_negative_polarity
                   +abs_title_subjectivity, data=news.train,family=binomial
)
summary(logisticFit1)
logistic.predicted_prob = predict(logisticFit1,news.test,type = "response")
logistic.predicted_prob = ifelse(logistic.predicted_prob > 0.5,1,0)
tablog = table(logistic.predicted_prob,news.test$shares)
tablog
plot(roc(news.test$shares,predict(logisticFit1,news.test,type="response")))
#plot(roc(tablog)
accuracylog <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracylog(tablog)
#plot(roc(test$y, predict(mymodel, test, type = "prob"))


#LDA
library(MASS)
LDAFit = lda(shares ~ n_tokens_content+n_tokens_content+n_non_stop_unique_tokens
             +num_self_hrefs+num_imgs+data_channel_is_entertainment
             +data_channel_is_bus
             +data_channel_is_socmed
             +data_channel_is_tech
             +kw_min_min+kw_avg_max+kw_min_avg+kw_max_avg
             +kw_avg_avg
             +weekday_is_monday
             +weekday_is_tuesday
             +weekday_is_wednesday
             +weekday_is_thursday
             +weekday_is_friday
             +weekday_is_saturday
             +global_subjectivity
             +avg_negative_polarity
             +min_negative_polarity
             +abs_title_subjectivity,data=news.train)
LDAFit = lda(shares ~ .,data=news.train)

LDAFit
LDA.pred = predict(LDAFit,newdata = news.test)
tabLDA = table(LDA.pred$class,news.test$shares)
tabLDA
accuracyLDA <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracyLDA(tabLDA)



#QDA
library(MASS)
QDAFit = qda(shares ~ n_tokens_content+n_tokens_content+n_non_stop_unique_tokens
             +num_self_hrefs+num_imgs+data_channel_is_entertainment
             +data_channel_is_bus
             +data_channel_is_socmed
             +data_channel_is_tech
             +kw_min_min+kw_avg_max+kw_min_avg+kw_max_avg
             +kw_avg_avg
             +weekday_is_monday
             +weekday_is_tuesday
             +weekday_is_wednesday
             +weekday_is_thursday
             +weekday_is_friday
             +weekday_is_saturday
             +global_subjectivity
             +avg_negative_polarity
             +min_negative_polarity
             +abs_title_subjectivity,data=news.train)
QDAFit
QDA.pred = predict(QDAFit,newdata = news.test)
tabQDA = table(QDA.pred$class,news.test$shares)
tabQDA
accuracyQDA <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracyQDA(tabQDA)



#Random Forest
library(randomForest)
#rfFit = randomForest(shares ~.,data=news.train)
rfFit = randomForest(shares ~ n_tokens_content+n_tokens_content+n_non_stop_unique_tokens
                     +num_self_hrefs+num_imgs+data_channel_is_entertainment
                     +data_channel_is_bus
                     +data_channel_is_socmed
                     +data_channel_is_tech
                     +kw_min_min+kw_avg_max+kw_min_avg+kw_max_avg
                     +kw_avg_avg
                     +weekday_is_monday
                     +weekday_is_tuesday
                     +weekday_is_wednesday
                     +weekday_is_thursday
                     +weekday_is_friday
                     +weekday_is_saturday
                     +global_subjectivity
                     +avg_negative_polarity
                     +min_negative_polarity
                     +abs_title_subjectivity,data=news.train)
rfFit
rf.pred = predict(rfFit,newdata = news.test)
tabrf = table(rf.pred,news.test$shares)
tabrf
accuracyrf <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracyrf(tabrf)
          