news.original <- read.csv("./final_data.csv")
news<- news.original
summary(news)
#import.as <- as.factor
import.as <- as.logical

news <- na.omit(news)
news<- subset(news, select=-c(url, timedelta,popular))
summary(news.original)
str(news)
news$data_channel_is_lifestyle <- import.as(news$data_channel_is_lifestyle)
news$data_channel_is_entertainment <- import.as(news$data_channel_is_entertainment)
news$data_channel_is_bus <- import.as(news$data_channel_is_bus)
news$data_channel_is_socmed <- import.as(news$data_channel_is_socmed)
news$data_channel_is_tech <- import.as(news$data_channel_is_tech)
news$data_channel_is_world <- import.as(news$data_channel_is_world)

news$weekday_is_monday <- import.as(news$weekday_is_monday)
news$weekday_is_tuesday <- import.as(news$weekday_is_tuesday)
news$weekday_is_wednesday <- import.as(news$weekday_is_wednesday)
news$weekday_is_thursday <- import.as(news$weekday_is_thursday)
news$weekday_is_friday <- import.as(news$weekday_is_friday)
news$weekday_is_saturday <- import.as(news$weekday_is_saturday)
news$weekday_is_sunday <- import.as(news$weekday_is_sunday)
news$is_weekend <- import.as(news$is_weekend)

news$logshares <- log(news[,'shares'])

boxplot(news$logshares)
hist(news$logshares)

news5000=news$shares<5000
news <- subset(news,select = -c(shares))

news=rapply(news,scale,c("numeric","integer"),how="replace")
news5000 = news[news5000,]

set.seed(123456)
train <- sample(dim(news)[1],dim(news)[1]*.7)
news.train <- news[train,]
news.test <- news[-train,]

Q = quantile(news.train$logshares, probs = c(.25,.75), na.rm=FALSE)
iqr = IQR(news.train$logshares)

upper = Q[2]+1.5*iqr
lower = Q[1]-1.5*iqr

news.train <- subset(news.train, news.train$logshares>lower & news.train$logshares<upper) 
boxplot(news.train$logshares)
hist(news.train$logshares)
dim(news.train)

train5 = sample(dim(news5000)[1],dim(news5000)[1]*.7)
news5.train <-news5000[train5,]
news5.test <-news5000[-train5,]


# news.numeric <- news[, sapply(news, is.numeric)]
# dim(news.numeric)
# 
# library(corrplot)
# news.numeric <- news[, sapply(news, is.numeric)]
# for (i in 1:4){
#   corrplot.mixed(cor(news.numeric),lower.col="black")
# }


# # correlations with absolute value above 0.05: 
# news.corr <- news[,c('num_hrefs', 'kw_max_avg', 'kw_avg_avg', 'self_reference_min_shares', 'self_reference_max_shares', 'self_reference_avg_sharess', 'LDA_02', 'LDA_03','shares')]
# # correlations to logshares with absolute value above 
# news.logcorr <- news[,c('num_hrefs', 'kw_avg_max', 'kw_min_avg', 'kw_max_avg', 'kw_avg_avg', 'self_reference_min_shares', 'self_reference_max_shares', 'self_reference_avg_sharess','LDA_01','LDA_02','LDA_03','global_subjectivity','global_sentiment_polarity','rate_negative_words','title_subjectivity','title_sentiment_polarity','abs_title_sentiment_polarity','logshares')]
# 
# corrplot.mixed(cor(news.logcorr),lower.col="black")

lm.fit.all <- lm(logshares~., data = news.train)
summary(lm.fit.all)

#Linear Regression with predictors of p-value less than .01 for Full DataSet
lm.fit.p01 <- lm(logshares~n_tokens_content+num_self_hrefs+
               data_channel_is_entertainment+data_channel_is_bus+data_channel_is_world+kw_min_min+kw_max_min+
               kw_avg_min+kw_avg_max+kw_min_avg+kw_max_avg+kw_avg_avg+weekday_is_monday+weekday_is_tuesday+
               weekday_is_wednesday+weekday_is_thursday+weekday_is_friday+LDA_00+
               global_subjectivity+title_sentiment_polarity+abs_title_subjectivity, data=news.train)
summary(lm.fit.p01)
lm.pred <- predict(lm.fit.p01, news.test)
mean((lm.pred-news.test$logshares)^2)

#Linear Regression for shares less than 5000
lm.fit.5000 <-lm(logshares~.,data=news5.train)
summary(lm.fit.5000)

lm.fit.5.p01 <- lm(logshares~n_tokens_content+data_channel_is_entertainment+data_channel_is_socmed+kw_min_min+kw_max_min+
               kw_avg_min+kw_avg_max+kw_min_avg+kw_max_avg+kw_avg_avg+weekday_is_monday+weekday_is_tuesday+
               weekday_is_wednesday+weekday_is_thursday+weekday_is_friday+LDA_00+LDA_01+LDA_02+LDA_03+
               abs_title_subjectivity, data=news5.train)
summary(lm.fit.5.p01)
lm.pred <- predict(lm.fit.5.p01, news5.test)
mean((lm.pred-news5.test$logshares)^2)


#PCR
library(pls)
pcr <- pcr(logshares~., data=news.numeric, scale = TRUE, validation = "CV")
summary(pcr)


#Best Subset Regression 
library(leaps)

dim(news.train)

predict.regsubsets = function (model ,data ,id ,...){
  form=as.formula(model$call [[2]])
  mat=model.matrix(form,data)
  coefi=coef(model ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best = regsubsets(logshares~.,news.train,nvmax = 58, really.big = TRUE)
reg.best.summary= summary(regfit.best)

val.errors=rep(NA,55)
for(i in 1:55){
  pred=predict(regfit.best,news.test, id=i)
  val.errors[i]=mean((news.test$logshares-pred)^2)
}
best<-which.min(val.errors)
best
val.errors[best]
plot(val.errors, xlab = "Number of predictors", ylab = "Validation Error", type = "b", main="Best Subset Selecion")
points(best,val.errors[best],col="red",cex=2,pch=20)

plot(reg.best.summary$bic, xlab = "Number of predictors", ylab = "BIC", type = "b", main="Best Subset Selecion")
points(which.min(reg.best.summary$bic),reg.best.summary$bic[35],col="red",cex=2,pch=20)

reg.best.summary$adjr2[35]

#Forward Best Subset Selection
regfit.best.forward = regsubsets(logshares~.,news.train,nvmax = 58,  method = "forward")
reg.summary=summary(regfit.best.forward)

val.errors=rep(NA,55)
for(i in 1:55){
  pred=predict(regfit.best.forward,news.test, id=i)
  val.errors[i]=mean((news.test$logshares-pred)^2)
}
best<-which.min(val.errors)
best
val.errors[best]
plot(val.errors, xlab = "Number of predictors", ylab = "Validation Error", type = "b", main="Forward Subset Selecion")
points(best,val.errors[best],col="red",cex=2,pch=20)
reg.summary$adjr2[best]

which.min(reg.summary$bic)

plot(reg.summary$bic, xlab = "Number of predictors", ylab = "BIC", type = "b", main="Forward Subset Selecion")
points(which.min(reg.summary$bic),reg.summary$bic[21],col="red",cex=2,pch=20)

reg.summary$adjr2[21]

val.errors[21]
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)

reg.summary$adjr2[35]


#BACKWARD 
regfit.best.backward = regsubsets(logshares~.,news.train,nvmax = 58,  method = "backward")
reg.b.summary=summary(regfit.best.backward)


val.errors=rep(NA,55)
for(i in 1:55){
  pred=predict(regfit.best.backward, news.test, id=i)
  val.errors[i]=mean((news.test$logshares-pred)^2)
}
best.backwards<- which.min(val.errors)
best.backwards
val.errors[best.backwards]
reg.b.summary$adjr2[best.backwards]

plot(val.errors, xlab = "Number of predictors", ylab = "Validation Error", type = "b", main="Backward Subset Selecion")
points(best.backwards,val.errors[best.backwards],col="red",cex=2,pch=20)

which.min(reg.b.summary$bic)

plot(reg.b.summary$bic, xlab = "Number of predictors", ylab = "BIC", type = "b", main="Backwards Subset Selecion")
points(which.min(reg.b.summary$bic),reg.b.summary$bic[26],col="red",cex=2,pch=20)

val.errors[26]
reg.b.summary$adjr2[26]

which.max(reg.b.summary$adjr2)
which.min(reg.b.summary$cp)
which.min(reg.b.summary$bic)


