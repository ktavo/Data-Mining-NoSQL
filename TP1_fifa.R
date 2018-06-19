rm(list=ls())
setwd("E:/UBA/Data Mining/TP1")



#Para matriz de correlaciones
library("corrplot")
library("ggplot2")
require("mongolite")

library("RWeka")
library("partykit")

#Conexión colleción Tweets
tweetsConection <- mongo(collection = "FIFA-dump_tweets", db = "fifa", url = "mongodb://localhost")
fullTwwetsQuery <- tweetsConection$find('{}')
#seleTwwetsQuery <- tweetsConection$find('{"screen_name" : "FCFSeleccionCol"}')

#Conexión colleción Users
userConection <- mongo(collection = 'FIFA-dump_users', db = "fifa", url = "mongodb://localhost")
fullUsersQuery <- userConection$find('{}')
#seleTwwetsQuery <- userConection$find('{"screen_name" : "FCFSeleccionCol"}')

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
#noOutliersFavoriteCount <- subset(favoriteCount, favorite_count > 1  & favorite_count < 20)

hashtags <- fullTwwetsQuery["hashtags"]
noNAHastags <- subset(hashtags, !is.na(hashtags))

symbols <- fullTwwetsQuery["symbols"]
noNASymbols <- subset(symbols, is.na(symbols))



#Consultas NOSQL
#demoQuery <- tweetsConection$find('{"favorite_count" : "{ "$gt": "1" }"}')


#Merge two collections
tweetsAndUsers <- unique(merge(fullTwwetsQuery,fullUsersQuery,by="user_id", all = TRUE))

#Correlaciones numéricas de usuarios
tweetsAndUsersVariables = c("followers_count", "favorite_count", "retweet_count")
numericTweetsAndUsersData <- tweetsAndUsers[tweetsAndUsersVariables]
numericTweetsAndUsersDataCorrelationMatrix = cor(numericTweetsAndUsersData)
corrplot.mixed(numericTweetsAndUsersDataCorrelationMatrix, lower = "number", upper = "shade", addshade = "all")


#J48 Tree
selectedVariables = c("created_at","retweet_count", "favorite_count", "is_quote",
                      "reply_to_screen_name","source", "favourites_count", "followers_count", 
                      "friends_count","listed_count", "statuses_count", "verified")
treeDataFrame <- tweetsAndUsers[preSelectedVariables]

treeDataFrame$popular <- FALSE
treeDataFrame$popular[treeDataFrame$favorite_count > favoriteCountMean] <- TRUE
treeDataFrame$popular[treeDataFrame$retweet_count > retwwetCountMean] <- TRUE

#toDeleteVariables <- c("retweet_count", "favorite_count")
#treeDataFrame[ , !(names(treeDataFrame) %in% toDeleteVariables)]

## 70% of the sample size
smp_size <- floor(0.7 * nrow(treeDataFrame))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(treeDataFrame)), size = smp_size)

trainTreeDataFrame <- treeDataFrame[train_ind, ]
testTreeDataFrame <- treeDataFrame[-train_ind, ]

trainTreeDataFrame$source<-as.factor(trainTreeDataFrame$source)
trainTreeDataFrame$reply_to_screen_name<-as.factor(trainTreeDataFrame$reply_to_screen_name)


#Genera árbol train
tree_train <- J48(as.factor(popular)~., trainTreeDataFrame,control=Weka_control(M=40,C=0.25))
summary(tree_train)
performance_train <- summary(tree_train)$details[1]

#Acá evalúa con la base de TEST
validacion_test<-evaluate_Weka_classifier(tree_train,newdata=testTreeDataFrame)
performance_test<-validacion_test$details[1]

#Dimensión del árbol
p <- as.party(tree_train)
leaves<-length(p)
## [1] 9
size<-width(p)
## [1] 5
prof<-depth(p)
## [1] 4

#Create CSV training and test
write.table(trainTreeDataFrame, file = "trainTreeData.csv",row.names=FALSE, na="",
            col.names=c("created_at", "retweet_count","favorite_count", "is_quote","reply_to_screen_name",
                        "source", "favourites_count", "followers_count","friends_count",
                        "listed_count", "statuses_count", "verified", "popular"), sep=",")
write.table(testTreeDataFrame, file = "testTreeData.csv",row.names=FALSE, na="",
            col.names=c("created_at", "retweet_count","favorite_count", "is_quote","reply_to_screen_name",
                        "source", "favourites_count", "followers_count","friends_count",
                        "listed_count", "statuses_count", "verified", "popular"), sep=",")

#Reevaluando definición de popular
retwwetCountMean <- mean(treeDataFrame$retweet_count)
favoriteCountMean <- mean(treeDataFrame$favorite_count)



