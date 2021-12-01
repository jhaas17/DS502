library("class")
source("load.R")
pred.knn <- knn(news.train[,!names(news.train) %in% c('shares')],
               news.test[,!names(news.test) %in% c('shares')],
               cl=news.train[,'shares'], k=100)
# mse: 1.15

library("caret")
library("CombMSC")
all.vars <-names(news.train)[1:58]
mse = vector(mode="numeric")
vars = list() 
ks = vector(mode="numeric")
for (n_vars in 5:5){
  s <- subsets(length(all.vars),n_vars,all.vars)
  for (r in 1:nrow(s)){
    for (k in c(1:10,100)){
      vars[[length(vars)+1]] <- s[r,]
      ks[length(ks)+1] <- k
      print(paste("vars=", vars[[length(vars)]], "ks=", ks[length(ks)]))
      mdl.knn  <- knnreg(x=news.train[,as.character(lapply(vars[[length(vars)]],as.name))], y=news.train[,'shares'], k=k)
      pred.knn <- predict(mdl.knn,news.test[,as.character(lapply(vars[[length(vars)]],as.name))])
      mse[length(mse)+1] <- mean((pred.knn-news.test[,'shares'])^2)
   } 
    
  }
}

for (k in seq(from=10,to=100,by=10)){
  mdl.knn  <- knnreg(x=news.train[,all.vars], y=news.train[,'shares'], k=k)
  pred.knn <- predict(mdl.knn,news.test[,all.vars])
  mse[length(mse)+1] <- mean((pred.knn-news.test[,'shares'])^2)
}

plot(seq(from=10, to=100, by=10),c(1.220011,1.190522,1.182185,1.178617,1.175435,1.174244,1.174364,1.173720,1.173326,1.172940),
     main="MSE with various k",
     xlab="k",ylab = "Normalized MSE",
     ylim = c(1.15, 1.25))