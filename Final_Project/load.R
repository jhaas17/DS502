#news <- read.csv("/Users/jannikhaas/Desktop/WPI_DS/Spring20/DS_502_Stats/Project/OnlineNewsPopularity.csv")

#import.as <- as.factor
import.as <- as.logical

news <- read.csv("./final_data.csv")
news <- na.omit(news)
news<- subset(news, select=-c(url, timedelta))

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

news=rapply(news,scale,c("numeric","integer"),how="replace")


train <- sample(dim(news)[1],dim(news)[1]/2)
news.train <- news[train,]
news.test <- news[-train,]

news.numeric <- news[, sapply(news, is.numeric)]
for (i in 1:4){
corrplot.mixed(cor(news.numeric[,c(((i-1)*11+1):(i*11),45,46)]),lower.col="black")
}

# correlations with absolute value above 0.05: 
news.corr <- news[,c('num_hrefs', 'kw_max_avg', 'kw_avg_avg', 'self_reference_min_shares', 'self_reference_max_shares', 'self_reference_avg_sharess', 'LDA_02', 'LDA_03','shares')]
# correlations to logshares with absolute value above 
news.logcorr <- news[,c('num_hrefs', 'kw_avg_max', 'kw_min_avg', 'kw_max_avg', 'kw_avg_avg', 'self_reference_min_shares', 'self_reference_max_shares', 'self_reference_avg_sharess','LDA_01','LDA_02','LDA_03','global_subjectivity','global_sentiment_polarity','rate_negative_words','title_subjectivity','title_sentiment_polarity','abs_title_sentiment_polarity','logshares')]

#corrplot.mixed(cor(news.logcorr),lower.col="black")
