####################################
#  DYNAMIC RELATIONSHIP MARKETING  #
####################################
library(NLP)
library(tm)
library(stringr)
library(qdapRegex)
library(SentimentAnalysis)
library(SnowballC)
library(jmotif)
library(data.table)
library(rlist)
library(stringdist)
library(cluster)
library(sentimentr)

CleanData <- function(tweets) {
	# Cleans twitter's data 
	# Args:
	#   tweets: A vector of raw tweets
	# Returns:
	#   A vector of "cleaned" tweets
	tweets <- str_replace(tweets, "RT @[a-z,A-Z,0-9,_]*: ", " ")  # Remove retweet headers
	tweets <- str_replace(tweets, "RT:", "")
	tweets <- str_replace_all(tweets, "@[a-z,A-Z,0-9,_]*", " ")  # Remove @mentions
	tweets <- str_replace_all(tweets, "&amp;", " ")  # Remove ampersands encoding "&amp;"
	tweets <- str_replace_all(tweets, "&lt;", " ")  # Remove "<" symbols encoding "&lt;"
	tweets <- str_replace_all(tweets, "&gt;", " ")  # Remove ">" symbols encoding "&gt;"
	tweets <- rm_url(tweets)  # Remove URLs
	tweets <- str_replace_all(tweets, "<ed>", " ")  # Remove emoticons encoding <ed><U+00A0><U+00BC>
	tweets <- str_replace_all(tweets, "<[a-z,A-Z,0-9,+]*>", " ")
	tweets <- str_replace_all(tweets, "[[:punct:]]", " ")  # Remove punctuation marks
	tweets <- str_replace_all(tweets, "w/", " ")  # Remove "with" encoding "w/"
	stop_pattern <- paste0("\\b(", paste0(stopwords("en"), collapse="|"), ")\\b")
	tweets <- str_replace_all(tweets, stop_pattern, " ")
	tweets <- str_replace_all(tweets, "\\s+", " ")  # Remove unnecessary spaces
	tweets <- tolower(tweets)
	return(tweets)
}

RemOutliers <- function(tweets.df, users) {
	# Removes the outliers (users) based on the number of tweets for each user
	# Args:
	#   tweets: A dataframe of tweets
	#   users: A vector of users who tweeted at least once in each period
	# Returns:
	#   A vector of users 
	no.users <- length(users)
	vec <- matrix(0, nrow = no.users , ncol = 1)
	for (i in 1:no.users) {
		user.df <- tweets.df[which(tweets.df$screenName == users[i]), ]
		# user.df <- user.df[which(user.df$isRetweet == FALSE), ]
		vec[i] <- nrow(user.df)
	}
	outliers <- boxplot.stats(vec)$out
	min.outlier <- sort(outliers)[1]
	users <- users[-(which(vec >= min.outlier))]
	return(users)
}

GetTweets <- function(tweets.df, users) {
	# Calculates the number of tweets for each user
	# Args:
	#   tweets: A dataframe of tweets
	#   users: A vector of users who tweeted at least once in each period
	# Returns:
	#   A vector of the number of tweets for each user
	no.users <- length(users)
	vec <- matrix(0, nrow = no.users , ncol = 1)
	for (i in 1:no.users) {
		user.df <- tweets.df[which(tweets.df$screenName == users[i]), ]
		user.df <- user.df[which(user.df$isRetweet == FALSE), ] 
		vec[i] <- nrow(user.df)
	}
	return(vec)
}

GetSentiment <- function(tweets.df, users) {
	# Calculates the tweets' sentiment for each user
	# Args:
	#   tweets: A dataframe of tweets
	#   users: A vector of users who tweeted at least once in each period
	# Returns:
	#   A vector the tweets' sentiment for each user
	no.users <- length(users)
	vec <- matrix(0, nrow = no.users , ncol = 1)
	for (i in 1:no.users) {
		user.df <- tweets.df[which(tweets.df$screenName == users[i]), ] 		
		vec[i] <- mean(sentiment(user.df$text)$sentiment)  # -1 to +1
	}
	return(vec)
}

GetProductMix <- function(tweets.df, users) {
	# Calculates the tweets' product mix for each user
	# Args:
	#   tweets: A dataframe of tweets
	#   users: A vector of users who tweeted at least once in each period
	# Returns:
	#   A vector of the tweets' product mix for each user
	products <- c("coffee", "drink", "americano", "latte", "mocha", "cappuccino", "caramel macchiato", "cinnamon dolce latte", "espresso",
			"espresso con panna", "espresso macchiato", "flat white", "iced americano", "iced latte", "iced mocha",
			"iced caramel macchiato", "iced cinnamon dolce latte", "iced latte macchiato", "iced skinny cinnamon dolce latte",
			"iced skinny mocha", "iced vanilla latte", "iced white chocolate mocha", "latte macchiato", "skinny mocha",
			"doubleshot", "blonde doubleshot", "vanilla latte", "white chocolate mocha",
			"refreshers", "cool lime", "ombre pink drink", "pink drink", "strawberry acai", "very berry hibiscus", "violet drink",
			"evolution fresh", "cold-pressed apple berry juice", "orange juice", "organic ginger limeade", "super green", "sweet greens and lemon",
			"iced coffee", "iced coffee with milk", "caffe mocha", "cold brew", "cold brew with milk", "nitro cold brew",
			"caramel iced coffee", "low calorie iced coffee", "vanilla iced coffee", "vanilla sweet cream cold brew",
			"iced tea", "shaken sweet tea", "berry blossom white", "black mango", "black with lemon", "brambleberry", "giant peach",
			"iced passion", "lemon ginger", "organic black lemonade", "organic iced black tea", "organic iced green tea", "plum pomegranate",
			"tazoberry", "white cranberry", "iced black tea", "iced black tea lemonade", "iced green tea", "iced passion",
			"colada tea infusion", "peach citrus white tea infusion", "pineapple black tea infusion", "strawberry green tea infusion",
			"chocolate smoothie", "hot chocolate",
			"vanilla frappuccino", "vanilla light", "caramel cocoa cluster", "caramel frappuccino", "caramel light frappuccino",
			"coffee frappuccino", "chai creme", "cinnamon dolce", "cinnamon roll", "coffee light frappuccino", "cupcake creme",
			"double chocolaty")
	no.users <- length(users)
	vec <- matrix(0, nrow = no.users , ncol = 1)
	for (i in 1:no.users) {
		user.df <- tweets.df[which(tweets.df$screenName == users[i]), ]
		no.Products <- 0
		for (j in 1:nrow(user.df)) {
			temp <- strsplit(user.df$text[j]," ") 
			for (k in 1:length(temp[[1]])) {
				if (amatch(temp[[1]][k], products, maxDist = 2, nomatch=0) > 0) {
					no.Products = no.Products + 1
				}
			}	
		}	
		vec[i] <- no.Products
	}
	return(vec)
}

Normalize <- function(x, newMin, newMax) {
  # Scale data to [0-1]
  # Args:
  #   x: A vector to be normalized 
  #   newMin: Minimum value
  #   newMax: Maximum value
  # Returns:
  #   The normalized vector
  Min <- min(x)
  Max <- max(x)
  return((x - Min)*(newMax - newMin)/(Max - Min))+newMin
}

GetFeatures <- function(tweets.df, users) {
	# Extract all the features for each user
	# Args:
	#   tweets: A dataframe of tweets
	#   users: A vector of users who tweeted at least once in each period
	# Returns:
	#   A matrix of all the features for each user
	no.users <- length(users)
	diss <- matrix(0, nrow = no.users , ncol = 3)
	diss[ ,1] <- Normalize(GetTweets(tweets.df, users), 0 ,1)
	diss[ ,2] <- Normalize(GetSentiment(tweets.df, users), 0 ,1)
	diss[ ,3] <- Normalize(GetProductMix(tweets.df, users), 0 ,1)
	return(diss)
}	
	
# Load the datasets
setwd("C:/Dataset/4_Periods_7days/1st")
filelist = list.files(pattern = ".*.*")
tweets1.df <- do.call(rbind, lapply(filelist, read.csv, stringsAsFactors = FALSE))
setwd("C:/Dataset/4_Periods_7days/2nd")
filelist = list.files(pattern = ".*.*")
tweets2.df <- do.call(rbind, lapply(filelist, read.csv, stringsAsFactors = FALSE))
setwd("C:/Dataset/4_Periods_7days/3rd")
filelist = list.files(pattern = ".*.*")
tweets3.df <- do.call(rbind, lapply(filelist, read.csv, stringsAsFactors = FALSE))
setwd("C:/Dataset/4_Periods_7days/4th")
filelist = list.files(pattern = ".*.*")
tweets4.df <- do.call(rbind, lapply(filelist, read.csv, stringsAsFactors = FALSE))
# Extract users who tweeted at least once in each period
users1 <- unique(tweets1.df$screenName)
users2 <- unique(tweets2.df$screenName)
users3 <- unique(tweets3.df$screenName)
users4 <- unique(tweets4.df$screenName)
usersA <- intersect(users1, users2)
usersB <- intersect(users3, users4)
users <- intersect(usersA, usersB)
# Retrieve Starbucks tweets 
starbucks1.df <- tweets1.df[which(tweets1.df$screenName == 'Starbucks'), ]
starbucks2.df <- tweets2.df[which(tweets2.df$screenName == 'Starbucks'), ]
starbucks3.df <- tweets3.df[which(tweets3.df$screenName == 'Starbucks'), ]
starbucks4.df <- tweets4.df[which(tweets4.df$screenName == 'Starbucks'), ]
# Remove the user name of the Brand if exists
users <- users[-(which(users == 'Starbucks'))]
# Remove the outliers of users
users <- RemOutliers(tweets1.df, users)
users <- RemOutliers(tweets2.df, users)
users <- RemOutliers(tweets3.df, users)
users <- RemOutliers(tweets4.df, users)
no.users <- length(users)
# Get their tweets
tweets1.df <- tweets1.df[which(tweets1.df$screenName %in% users), ]
tweets2.df <- tweets2.df[which(tweets2.df$screenName %in% users), ]
tweets3.df <- tweets3.df[which(tweets3.df$screenName %in% users), ]
tweets4.df <- tweets4.df[which(tweets4.df$screenName %in% users), ]
# Data Cleansing
tweets1.df$text <- CleanData(tweets1.df$text)
tweets2.df$text <- CleanData(tweets2.df$text)
tweets3.df$text <- CleanData(tweets3.df$text)
tweets4.df$text <- CleanData(tweets4.df$text)
starbucks1.df$text <- CleanData(starbucks1.df$text)
starbucks2.df$text <- CleanData(starbucks2.df$text)
starbucks3.df$text <- CleanData(starbucks3.df$text)
starbucks4.df$text <- CleanData(starbucks4.df$text)
# Normalized dissimilarity matrices
features1.mat <- GetFeatures(tweets1.df, users) 
features2.mat <- GetFeatures(tweets2.df, users) 
features3.mat <- GetFeatures(tweets3.df, users) 
features4.mat <- GetFeatures(tweets4.df, users) 
# PAM Clustering
no.clusters <- 4
clust1 <- pam(features1.mat, no.clusters, diss = FALSE, metric = "euclidean")
clust2 <- pam(features2.mat, no.clusters, diss = FALSE, metric = "euclidean")
clust3 <- pam(features3.mat, no.clusters, diss = FALSE, metric = "euclidean")
clust4 <- pam(features4.mat, no.clusters, diss = FALSE, metric = "euclidean")	
	
	
# ------------------------ #
#    E V A L U A T I O N   #
# ------------------------ #
ClassUsers <- function(cluster) {
	# Classifies the users to negative/neutral/transitional/positive based on the clustering 
	# Args:
	#   cluster: The clustering result of one period 
	# Returns:
	#   A vector of 4 clusters (negative/neutral/transitional/positive) of users
	med1 <- mean(cluster$medoids[1,])
	med2 <- mean(cluster$medoids[2,])
	med3 <- mean(cluster$medoids[3,])
	med4 <- mean(cluster$medoids[4,])
	clusters <- matrix(c(seq(4),c(med1, med2 , med3, med4)), nrow = 4 , ncol = 2)
	clusters <- clusters[order(clusters[,2], decreasing = FALSE),]	
	negative <- users[which(cluster$clustering == clusters[1])]
	neutral <- users[which(cluster$clustering == clusters[2])]
	transitional <- users[which(cluster$clustering == clusters[3])]
	positive <- users[which(cluster$clustering == clusters[4])]	
	states <- list(negative, neutral, transitional, positive)
	return(states)
}

CalcCommitment <- function(users1, users2, tweets.df) {
	# Calculates the commitment of users based on the total number of their posts
	# Args:
	#   users1: A vector of users who tweeted at the first period
	#   users2: A vector of users who tweeted at the second period
	#   tweets: A dataframe of tweets
	# Returns:
	#   The sum of the commitment of all users
	users <- intersect(users1, users2)
	no.users <- length(users)
	commitment <- matrix(0, nrow = no.users , ncol = 1)
	for (i in 1:no.users) {
		user.df <- tweets.df[which(tweets.df$screenName == users[i]), ]
		# user.df <- user.df[which(user.df$isRetweet == TRUE), ]  
		# commitment[i] <- nrow(user.df)
		commitment[i] <- sum(user.df$favoriteCount)
	}
	return(sum(commitment))
}

Commitment <- function(states1, states2, tweets.df) {
	# Calculates the commitment of all the possible transitions between 2 periods
	# Args:
	#   statesi: The 4 clusters of users (negative/neutral/transitional/positive)
	#   tweets.df: A dataframe of tweets of the first period
	# Returns:
	#   A vector of the commitment of all the possible transitions
	vec <- matrix(0, nrow = 16 , ncol = 1)
	z <- 1
	for (i in 1:4) {
		for (j in 1:4) {
			vec[z] <- CalcCommitment(states1[[i]], states2[[j]], tweets.df)
			z <- z + 1
		}
	}	
	return(vec)
}

CalcTrustLex <- function(users1, users2, tweets.df, lexicon) {
	# Calculates the trust of users based on a trust NRC lexicon
	# Args:
	#   users1: A vector of users who tweeted at the first period
	#   users2: A vector of users who tweeted at the second period
	#   tweets: A dataframe of tweets
	#   lexicon: A trust lexicon
	# Returns:
	#   A vector of the number of tweets for each user
	users <- intersect(users1, users2)
	no.users <- length(users)
	trust <- matrix(0, nrow = no.users , ncol = 1)
	for (i in 1:no.users) {
		user.df <- tweets.df[which(tweets.df$screenName == users[i]), ]
		counter <- 0
		for (j in 1:nrow(user.df)) {
			temp <- strsplit(user.df$text[j]," ") 
			for (k in 1:length(temp[[1]])) {
				if (amatch(temp[[1]][k], lexicon, maxDist = 1, nomatch=0) > 0) {
					counter = counter + 1
				}
			}	
		}	
		trust[i] <- counter
	}
	return(sum(trust))
}

TrustLexicon <- function(states1, states2, tweets.df, lexicon) {
	# Calculates the trust based on the NRC lexicon of all the possible transitions between 2 periods
	# Args:
	#   statesi: The 4 clusters of users (negative/neutral/transitional/positive)
	#   tweets.df: A dataframe of tweets of the first period
	#   lexicon: A trust lexicon
	# Returns:
	#   A vector of the trust of all the possible transitions
	vec <- matrix(0, nrow = 16 , ncol = 1)
	z <- 1
	for (i in 1:4) {
		for (j in 1:4) {
			vec[z] <- CalcTrustLex(states1[[i]], states2[[j]], tweets.df, lexicon)
			z <- z + 1
		}
	}	
	return(vec)
}

TransRates <- function(states1, states2) {
	# Calculates the transition rates of users
	# Args:
	#   statesi: The 4 clusters of users (negative/neutral/transitional/positive)
	# Returns:
	#   A vector of the transition rates
	vec <- matrix(0, nrow = 16 , ncol = 1)
	z <- 1
	for (i in 1:4) {
		for (j in 1:4) {
			vec[z] <- length(intersect(states1[[i]], states2[[j]]))
			z <- z + 1
		}
	}	
	return(vec)
}

CalcSilhouette <- function(users1, users2, cluster, users_init) {
	# Calculates the clusters silhouette
	# Args:
	#   users1: A vector of users who tweeted at the first period
	#   users2: A vector of users who tweeted at the second period
	#   cluster: The clustering result of one period 
	#   users_init: A vector of the initial user-names
	# Returns:
	#   The mean value of a cluster's silhouette 
	users <- intersect(users1, users2)
	users <-  which(users_init %in% users)
	no.users <- length(users)
	sil <- matrix(0, nrow = no.users , ncol = 1)
	silhou <- silhouette(cluster) 
	mean.sil <- mean(silhou[users, 3])
	return(mean.sil)
}

Homophily <- function(states1, states2, cluster, users_init) {
	# Calculates the users' homophily in a cluster
	# Args:
	#   statesi: The 4 clusters of users (negative/neutral/transitional/positive)
	#   cluster: The clustering result of one period 
	#   users_init: A vector of the initial user-names
	# Returns:
	#   A vector of the homophily of all the possible transitions
	vec <- matrix(0, nrow = 16 , ncol = 1)
	z <- 1
	for (i in 1:4) {
		for (j in 1:4) {
			vec[z] <- CalcSilhouette(states1[[i]], states2[[j]], cluster, users_init)
			z <- z + 1
		}
	}	
	return(vec)
}

PercentageChange <- function(vec1, vec2) {
	# Calculates the percentage change between two vectors 
	# Args:
	#   veci: The input numeric vectors
	# Returns:
	#   A numeric value of the percentage change
	vec <- matrix(0, nrow = 16 , ncol = 1)
	vec <- (vec2-vec1)/abs(vec1)*100
	return(vec)
}

MeanChange <- function(vec1) {
	# Calculates the mean of the upper/lower triangular percentage changes 
	# Args:
	#   veci: The input numeric vectors
	# Returns:
	#   A vector of the upper and lower means
	vec <- matrix(0, nrow = 2 , ncol = 1)
	vec[1] <- mean(c(vec1[2], vec1[3], vec1[4], vec1[7], vec1[8], vec1[12]))
	vec[2] <- mean(c(vec1[5], vec1[9], vec1[10], vec1[13], vec1[14], vec1[15]))
	return(vec)
}

# Recognize the state of each cluster
states1 <- ClassUsers(clust1)
states2 <- ClassUsers(clust2)
states3 <- ClassUsers(clust3)
states4 <- ClassUsers(clust4)
# Calculate the commitment of all the possible transitions
commitment1A <- Commitment(states1, states2, tweets1.df)
commitment2A <- Commitment(states2, states3, tweets2.df)
commitment3A <- Commitment(states3, states4, tweets3.df)
commitment1B <- Commitment(states1, states2, tweets2.df)
commitment2B <- Commitment(states2, states3, tweets3.df)
commitment3B <- Commitment(states3, states4, tweets4.df)
# Calculate the trust of users based on a trust NRC lexicon
setwd("C:/NRC-Sentiment-Emotion-Lexicons/NRC-Sentiment-Emotion-Lexicons/NRC-Emotion-Lexicon-v0.92")
nrcLexicon <- read.csv(file="NRC-Emotion-Lexicon-v0.92-In105Languages-Nov2017Translations.csv", header=TRUE, sep=",")
trustLexicon = cbind(nrcLexicon[1], nrcLexicon[115])
trustLexicon <- trustLexicon[which(trustLexicon$Trust==1),1]
trustLex1A <- TrustLexicon(states1, states2, tweets1.df, trustLexicon)
trustLex2A <- TrustLexicon(states2, states3, tweets2.df, trustLexicon)
trustLex3A <- TrustLexicon(states3, states4, tweets3.df, trustLexicon)
trustLex1B <- TrustLexicon(states1, states2, tweets2.df, trustLexicon)
trustLex2B <- TrustLexicon(states2, states3, tweets3.df, trustLexicon)
trustLex3B <- TrustLexicon(states3, states4, tweets4.df, trustLexicon)
# Calculate the transition rates of users
transRat1 <- TransRates(states1, states2)
transRat2 <- TransRates(states2, states3)
transRat3 <- TransRates(states3, states4)
# Calculate the users' homophily before the transition
homoph1A <- Homophily(states1, states2, clust1, users)
homoph2A <- Homophily(states2, states3, clust2, users)
homoph3A <- Homophily(states3, states4, clust3, users)
homoph1B <- Homophily(states1, states2, clust2, users)
homoph2B <- Homophily(states2, states3, clust3, users)
homoph3B <- Homophily(states3, states4, clust4, users)

# Calculate the percentage changes
commitment1 <- PercentageChange(commitment1A, commitment1B)
commitment2 <- PercentageChange(commitment2A, commitment2B)
commitment3 <- PercentageChange(commitment3A, commitment3B)

trustLex1 <- PercentageChange(trustLex1A, trustLex1B)
trustLex2 <- PercentageChange(trustLex2A, trustLex2B)
trustLex3 <- PercentageChange(trustLex3A, trustLex3B)

homoph1 <- PercentageChange(homoph1A, homoph1B)
homoph2 <- PercentageChange(homoph2A, homoph2B)
homoph3 <- PercentageChange(homoph3A, homoph3B)

# Calculate the upper/lower mean changes
MeanChange(commitment1)
MeanChange(commitment2)
MeanChange(commitment3)
MeanChange(trustLex1)
MeanChange(trustLex2)
MeanChange(trustLex3)
MeanChange(homoph1)
MeanChange(homoph2)
MeanChange(homoph3)
