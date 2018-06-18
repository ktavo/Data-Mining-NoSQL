rm(list=ls())
setwd("E:\UBA\Data Mining\TP1")

#Para matriz de correlaciones
library("corrplot")
library("ggplot2")
require("mongolite")

#Conexión colleción Tweets
tweetsConection <- mongo(collection = "FIFA-dump_tweets", db = "fifa", url = "mongodb://localhost")
fullTwwetsQuery <- tweetsConection$find('{}')
seleTwwetsQuery <- tweetsConection$find('{"screen_name" : "FCFSeleccionCol"}')

#Conexión colleción Users
userConection <- mongo(collection = 'FIFA-dump_users', db = "fifa", url = "mongodb://localhost")
fullUsersQuery <- userConection$find('{}')
seleTwwetsQuery <- userConection$find('{"screen_name" : "FCFSeleccionCol"}')

#correlaciones numéricas de tweets
tweetVariables = c("favorite_count", "retweet_count")
numericTweetData <- fullTwwetsQuery[tweetVariables]
numericTweetDataCorrelationMatrix = cor(numericTweetData)
corrplot.mixed(numericTweetDataCorrelationMatrix, lower = "number", upper = "shade", addshade = "all")

#Correlaciones numéricas de usuarios
userVariables = c("favourites_count", "followers_count", "friends_count",
                   "listed_count", "statuses_count")
numericUserData <- fullUsersQuery[userVariables]
numericUserDataCorrelationMatrix = cor(numericUserData)
corrplot.mixed(numericUserDataCorrelationMatrix, lower = "number", upper = "shade", addshade = "all")


favoriteCount <- fullTwwetsQuery["favorite_count"]
noOutliersFavoriteCount <- subset(favoriteCount, favorite_count > 1  & favorite_count < 20)

hashtags <- fullTwwetsQuery["hashtags"]
noNAHastags <- subset(hashtags, !is.na(hashtags))

symbols <- fullTwwetsQuery["symbols"]
noNASymbols <- subset(symbols, is.na(symbols))



#Consultas NOSQL
demoQuery <- tweetsConection$find('{"favorite_count" : "{ "$gt": "1" }"}')


#Merge two collections
tweetsAndUsers <- unique(merge(fullTwwetsQuery,fullUsersQuery,by="user_id", all = TRUE))

#Correlaciones numéricas de usuarios
tweetsAndUsersVariables = c("followers_count", "favorite_count", "retweet_count")
numericTweetsAndUsersData <- tweetsAndUsers[tweetsAndUsersVariables]
numericTweetsAndUsersDataCorrelationMatrix = cor(numericTweetsAndUsersData)
corrplot.mixed(numericTweetsAndUsersDataCorrelationMatrix, lower = "number", upper = "shade", addshade = "all")

selectedVariables = c("created_at","retweet_count", "favorite_count", "hashtags", "is_quote",
                      "reply_to_screen_name","source", "favourites_count", "followers_count", 
                      "friends_count","listed_count", "statuses_count", "verified")
treeDataFrame <- tweetsAndUsers[selectedVariables]

treeDataFrame$popular <- FALSE
treeDataFrame$popular[treeDataFrame$favorite_count > 0] <- TRUE
treeDataFrame$popular[treeDataFrame$retweet_count > 0] <- TRUE




## 70% of the sample size
smp_size <- floor(0.7 * nrow(treeDataFrame))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(treeDataFrame)), size = smp_size)

trainTreeDataFrame <- treeDataFrame[train_ind, ]
testTreeDataFrame <- treeDataFrame[-train_ind, ]





