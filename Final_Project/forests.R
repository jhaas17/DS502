library(randomForest)
library(ISLR)
source("load.R")


forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=50, mtry=5)
mean((news.test$shares - predict(forest, newdata = news.test))^2)

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=100, mtry=5)
mean((news.test$shares - predict(forest, newdata = news.test))^2)

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=200, mtry=5)
mean((news.test$shares - predict(forest, newdata = news.test))^2)

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=300, mtry=5)
mean((news.test$shares - predict(forest, newdata = news.test))^2)

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=50, mtry=7)
mean((news.test$shares - predict(forest, newdata = news.test))^2)

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=100, mtry=7)
mean((news.test$shares - predict(forest, newdata = news.test))^2)

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=200, mtry=7)
mean((news.test$shares - predict(forest, newdata = news.test))^2)

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=300, mtry=7)
mean((news.test$shares - predict(forest, newdata = news.test))^2)

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=50, mtry=9)
mean((news.test$shares - predict(forest, newdata = news.test))^2)

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=100, mtry=9)
mean((news.test$shares - predict(forest, newdata = news.test))^2)

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=200, mtry=9)
mean((news.test$shares - predict(forest, newdata = news.test))^2)

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=300, mtry=9)
mean((news.test$shares - predict(forest, newdata = news.test))^2)


forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=100, mtry=5)
mean((news.test$shares - predict(forest, newdata = news.test))^2)

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=100, mtry=7)
mean((news.test$shares - predict(forest, newdata = news.test))^2)

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=100, mtry=9)
mean((news.test$shares - predict(forest, newdata = news.test))^2)

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=100, mtry=11)
mean((news.test$shares - predict(forest, newdata = news.test))^2)

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=100, mtry=13)
mean((news.test$shares - predict(forest, newdata = news.test))^2)

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=100, mtry=19)
mean((news.test$shares - predict(forest, newdata = news.test))^2)

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=100, mtry=29)
mean((news.test$shares - predict(forest, newdata = news.test))^2)


forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=50, mtry=2, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
# 1.181
# 1.178

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=100, mtry=2, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
#1.173

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=50, mtry=5, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
#1.192075

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=100, mtry=5, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
# 1.18

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=50, mtry=20, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
# 1.220

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=500, mtry=20, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
# 1.208

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=500, mtry=5, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
#1.172
#1.174

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=500, mtry=30, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
#1.220

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=500, mtry=2, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
# 1.167

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=500, mtry=3, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
# 1.169

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=500, mtry=4, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
# 1.172

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=1000, mtry=2, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
# 1.164

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=4000, mtry=2, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
# 1.165

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=1000, mtry=3, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
# 1.169

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=1000, mtry=4, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
# 1.171

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=8000, mtry=2, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
# 1.164

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=8000, mtry=4, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
# 1.0524

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=8000, mtry=10, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
# 1.0524

forest <- randomForest(formula = shares ~ ., data=news.train[,!(names(news) %in% 'logshares')], ntree=12000, mtry=4, importance=TRUE)
mean((news.test$shares - predict(forest, newdata = news.test))^2)
# 1.053