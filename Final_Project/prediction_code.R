rm(list=ls())

library(ggplot2)
library(dplyr)

news <- read.csv("Final_dataset.csv")

#remove na
#remove url and timedelta
news <- na.omit(news)
news<- subset(news, select=-c(url, timedelta))



# remove  num_hrefs > 40. 
#news_content = news_content[!(news_content$num_hrefs > 40),]
#nrow(news_content)
#37545






#df1$Sp2[df1$Sp2 == 8] <- 800
news$shares = log(news$shares)
boxplot(news$shares)
hist(news$shares)

#remove where shares <6
#news = news[!(news$shares < 6),]
#news = news[!(news$shares > 10),]
#boxplot(news$shares)
#hist(news$shares)
#nrow(news) #34093 rows

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

news=rapply(news,scale,c("numeric","integer"),how="replace")
news.numeric <- news[, sapply(news, is.numeric)]

#split train and test
train = sample(dim(news)[1],dim(news)[1]*0.7)
news.train = news.numeric[train,]
news.test = news.numeric[-train,]
XTrain = model.matrix(shares~.,news.train)
XTest = model.matrix(shares~.,news.test)
YTrain = news.train$shares
YTest = news.test$shares



#linear regression
linear.fit = lm(shares ~ ., data=news.train)
summary(linear.fit)

linear.fit1 = lm(shares~num_hrefs
                 +kw_max_avg
                 +kw_avg_avg
                 +self_reference_min_shares
                 +self_reference_max_shares
                 +self_reference_avg_sharess
                 +max_positive_polarity, data=news.train
)
summary(linear.fit1)
linear.predicted = predict(linear.fit1,news.test)
mean((news.test$shares - linear.predicted)^2)


#ridge
alpha0.fit = cv.glmnet(XTrain,YTrain,type.measure = "mse",alpha=0,family="gaussian")
alpha0.predicted=predict(alpha0.fit, s=alpha0.fit$lambda.1se, newx=XTest)
##alpha0.predicted=predict(alpha0.fit, s=alpha0.fit$lambda.min, newx=XTest)
mean((YTest - alpha0.predicted)^2)
plot(alpha0.fit)
summary(alpha0.fit)

alpha0.fit
coef(alpha0.fit)



  #Lasso

alpha1.fit = cv.glmnet(XTrain,YTrain,type.measure = "mse",alpha=1,family="gaussian")
alpha1.predicted=predict(alpha1.fit, s=alpha1.fit$lambda.min, newx=XTest)
##alpha0.predicted=predict(alpha0.fit, s=alpha0.fit$lambda.min, newx=XTest)
mean((YTest - alpha1.predicted)^2)
plot(alpha1.fit)
summary(alpha0.fit)
alpha1.fit
coef(alpha1.fit)






# Good for Ridge Regression



myAlpha=0
myLambda = 10^(seq(4,-1,length=100))
# # Good for Lasso Regression
#myAlpha=1
#myLambda = 10^(seq(3,-1,length=100))

# The actual solver
myFit = glmnet(XTrain,YTrain,alpha=myAlpha,lambda=0.1)
coef(myFit)

myPredict = predict(myFit,newx=XTest)
library(Metrics)
mse(YTest,predict(myPredict,news.test))
myCoef






myLambda[1]



errors = NULL

for (i in 1:100) {
  errors[i] = mean( (YTest - myPredict[,i])^2 )
}

plot(myLambda,errors,type='l')



myCoef[,which.min(errors)]

myLambda[which.min(errors)]

library("class")
pred.knn <- knn(news.train[,!names(news.train) %in% c('shares')],
                news.test[,!names(news.test) %in% c('shares')],
                cl=news.train[,'shares'], k=3)

library("caret")
#pred.knn <- knnregTrain(train = news.train[,!names(news.train) %in% c('shares')],
#test =  news.test[,!names(news.test) %in% c('shares')],
#y=news.train[,'shares'], k=3)
#mean((pred.knn-news.test[,'shares'])^2)
mse = vector(length = 10, mode="numeric")
for (k in 1:10){
  mdl.knn[k]  <- knnreg(shares ~ ., news, train, k=k)
  pred.knn[k] <- predict(mdl.knn,news.test)
  mse[k] <- mean((pred.knn-news.test[,'shares'])^2)
}

plot(x=k,y=mse[k])



