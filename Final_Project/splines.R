library(splines)
library(ggplot2)
source("load.R")

# fit and plot some 1-d splines
df <- c(3,5,7,9,11)
var <- c('num_hrefs', 'kw_avg_max', 'kw_min_avg', 'kw_max_avg', 'kw_avg_avg', 'self_reference_min_shares', 'self_reference_max_shares', 'self_reference_avg_sharess','LDA_01','LDA_02','LDA_03','global_subjectivity','global_sentiment_polarity','rate_negative_words','title_subjectivity','title_sentiment_polarity','abs_title_sentiment_polarity')
var.all <- names(subset(news, select=-c(shares, logshares)))
df_frame <- c(rep('3',17),rep('5',17),rep('7',17),rep('9',17),rep('11',17))
var_frame <- rep(c('num_hrefs', 'kw_avg_max', 'kw_min_avg', 'kw_max_avg', 'kw_avg_avg', 'self_reference_min_shares', 'self_reference_max_shares', 'self_reference_avg_sharess','LDA_01','LDA_02','LDA_03','global_subjectivity','global_sentiment_polarity','rate_negative_words','title_subjectivity','title_sentiment_polarity','abs_title_sentiment_polarity'),
            length(df))
mse <- vector()
#for (d in df)
#{
#  for (v in var)
#  { 
#    print(paste("running with v =", v, "; d =", d))
#    f <- as.formula(paste("logshares ~ns(" ,v, ", d =", d, ")"))
#    fit <- lm(f, data=news.train)
#    pred <- predict(fit, news.test)
#    ix <- sort(news.test[,v],index.return=TRUE)$ix
#    
#    plot(news.train[,v],news.train$logshares,xlab=paste("news: ", v))
#    lines(news.test[ix,v],pred[ix],col=2, lwd=2)
#    mse[length(mse)+1] <- mean((pred-news.test$logshares)^2)
#    print(paste(
#      "mse:",
#      tail(mse,n=1))
#    )
#  }
#}

#mse_frame <- data.frame(var_frame,df_frame,mse)

#ggplot(mse_frame, aes(fill=df_frame, y=mse, x=var_frame)) + 
  #geom_bar(position="dodge", stat="identity")

# picking a few variables now:
# the best kw_* : kw_avg_avg
# the best self_reference_* : self_reference_avg_shares
# the acceptable LDAs: LDA_02, LDA_03

#fit <- lm(logshares ~ 
#            ns(kw_avg_avg, d= 9)+
#            ns(self_reference_avg_shares, d= 9)+
#            ns(LDA_02, d= 9)+
#            ns(LDA_03, d= 9), data=news.train)
#pred <- predict(fit, news.test)
#print(paste(
#  "mse:",
#  mean((pred-news.test$logshares)^2)
#))

# with all variables
f <- "logshares ~"
for (v in var.all)
{ 
  if (sapply(news[v],typeof) == "logical"){
    f <-paste(f, v, " +")
  }
  if (sapply(news[v],typeof) == "double"){
    k <- seq( min( news.train[v]) , max(news.train[v]),length.out=5)
    settings <-paste("knots =c(", k[2], ",", k[3], ",", k[4], ")")
    f <- paste(f,"ns(", v, ",", settings, ") +")
  }
}
#print(f)
f <- strtrim(f,nchar(f, type = "c")-2)
print(f)

fit <- lm(f, data=news.train)
pred <- predict(fit, news.test)
ix <- sort(news.test[,v],index.return=TRUE)$ix

plot(news.train[,v],news.train$logshares,xlab=paste("news: ", v))
lines(news.test[ix,v],pred[ix],col=2, lwd=2)
mse[length(mse)+1] <- mean((pred-news.test$logshares)^2)
print(paste(
  "mse:",
  tail(mse,n=1))
)

# just significant variables 


# SETTINGS: MSE
# with 
# var.sig <-c("n_tokens_title", "n_tokens_content", "num_hrefs", "num_self_hrefs", "average_token_length", "num_keywords", "data_channel_is_lifestyle", "data_channel_is_entertainment", "data_channel_is_bus", "data_channel_is_socmed", "data_channel_is_tech", "kw_min_min", "kw_max_min", "kw_avg_min", "kw_min_max", "kw_avg_max", "kw_min_avg", "kw_min_avg", "kw_max_avg", "kw_avg_avg", "weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday", "LDA_00", "LDA_01", "LDA_02", "global_subjectivity", "global_rate_positive_words", "min_positive_polarity", "title_subjectivity", "title_sentiment_polarity", "abs_title_subjectivity")
# empty (no knots): 
#    .88
# paste("knots = mean(news[,", paste("'",v,"'",sep=""), "])"):
#    .87
# paste("knots = seq(min(ns),max(ns),length=4)[2:3]"):
#    .8695
# paste("knots = seq(min(ns),max(ns),length=5)[2:4]"):
#    .8621
# paste("knots = seq(min(ns),max(ns),length=6)[2:5]"):
#    .8617
# paste("knots = seq(min(ns),max(ns),length=10)[2:9]"):
#    did not converge


# with updated var.sig
# SETTInGS: 
#   MSE
#k <- seq( min( news.train[v]) , max(news.train[v]),length.out=5)
#settings <-paste("knots =c(", k[2], ",", k[3], ",", k[4], ")")
#      .9082
#k <- seq( min( news.train[v]) , max(news.train[v]),length.out=6)
#settings <-paste("knots =c(", k[2], ",", k[3], ",", k[4], ",", k[5], ")")
#     .9077
#k <- seq( min( news.train[v]) , max(news.train[v]),length.out=7)
#settings <-paste("knots =c(", k[2], ",", k[3], ",", k[4], ",", k[5], ",", k[6], ")")
#     .9073
#k <- seq( min( news.train[v]) , max(news.train[v]),length.out=4)
#settings <-paste("knots =c(", k[2], ",", k[3], ",", ")")
#     .9098
#k <- seq( min( news.train[v]) , max(news.train[v]),length.out=3)
#settings <-paste("knots =c(", k[2], ")")
#      .9141

# plot
plot(1:5,c(.9141,.9098, .9073, .9077, .9082),
     main="MSE with different numbers of evenly spaced knots",
     xlab="knots",ylab = "Normalized log MSE",
     ylim = c(0.9,1))
text(3, .915, "Lowest MSE: .9073")


f <- "logshares ~"
var.sig <-c( "num_hrefs",
  "num_keywords","data_channel_is_lifestyle", "data_channel_is_entertainment", "data_channel_is_bus", "data_channel_is_tech","kw_max_max", "kw_avg_max", 
  "kw_min_avg",
  "weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday", 
  "weekday_is_saturday", 
  "LDA_00")
for (v in var.sig)
{ 
  if (sapply(news[v],typeof) == "logical"){
    f <-paste(f, v, " +")
  }
  if (sapply(news[v],typeof) == "double"){
    k <- seq( min( news.train[v]) , max(news.train[v]),length.out=3)
    settings <-paste("knots =c(", k[2], ")")
    f <- paste(f,"ns(as.numeric(", v, "),", settings, ") +")
    #f <- paste(f,"ns(", v, ") +")
   
  }
}
#print(f)
f <- strtrim(f,nchar(f, type = "c")-2)
print(f)

fit <- glm(f, data=news.train)
pred <- predict(fit, news.test)
ix <- sort(news.test[,v],index.return=TRUE)$ix

plot(news.train[,v],news.train$logshares,xlab=paste("news: ", v))
lines(news.test[ix,v],pred[ix],col=2, lwd=2)
mse[length(mse)+1] <- mean((pred-news.test$logshares)^2)
print(paste(
  "mse:",
  tail(mse,n=1))
)

# handpicked knots
knots <- list(
  num_hrefs = c(-.6,0,.6,1.8),
  num_keywords = -1,
  kw_max_max = c(-2.7, -1.5),
  kw_avg_max = c(-1.4,2.4),
  LDA_00 = c(-.3,2.2))
f <- "logshares ~"
for (v in var.sig)
{ 
  if (sapply(news[v],typeof) == "logical"){
    f <-paste(f, v, " +")
  }
  if (sapply(news[v],typeof) == "double"){
    k <- seq( min( news.train[v]) , max(news.train[v]),length.out=3)
    settings <-paste("knots =", knots[v])
    f <- paste(f,"ns(as.numeric(", v, "),", settings, ") +")
    #f <- paste(f,"ns(", v, ") +")
   
  }
}
#print(f)
f <- strtrim(f,nchar(f, type = "c")-2)
print(f)

fit <- glm(f, data=news.train)
pred <- predict(fit, news.test)
ix <- sort(news.test[,v],index.return=TRUE)$ix

plot(news.train[,v],news.train$logshares,xlab=paste("news: ", v))
lines(news.test[ix,v],pred[ix],col=2, lwd=2)
mse[length(mse)+1] <- mean((pred-news.test$logshares)^2)
print(paste(
  "mse:",
  tail(mse,n=1))
)


