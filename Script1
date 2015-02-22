################################################################
## Workshop: Collecting and Analyzing Social Media Data with R
## February 2nd, 2015
## Script 1: Collecting Twitter data
## Author: Pablo Barbera, NYU, @p_barbera
################################################################

setwd("/Users/giebumchun/Google Drive/MIDP 1/4. Spring 2015/Data Science/Script/Pablo/social-media-workshop-master")

## INSTALLING PACKAGES
doInstall <- TRUE  # Change to FALSE if you don't want packages installed.
toInstall <- c("ROAuth", "twitteR", "streamR", "ggplot2", "stringr",
               "tm", "RCurl", "maps", "Rfacebook", "topicmodels", "devtools")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}


## AUTHENTICATION

api_key <- "EKDDTbiQM6N8PdYvG6rmmrFrw"
api_secret <- "GPvpYJVNlpfhQmIAbE82b5StCpSuR7MeNfXrqs1oImHC7lbz8r"
access_token <- "2535685303-u6VVovUJfGbidcrthA06VKkFhi2MX2GbY9V9gRt"
access_token_secret <- "DTzho1LCwknbheHDGgbyl89OHJ4TzZMgcsjfOv5qgBeBn"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret, requestURL=requestURL,
                             accessURL=accessURL, authURL=authURL)

## run this line and go to the URL that appears on screen
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

## get authorization code and paste to console


## testing that it works
library(twitteR)
library(streamR)
#registerTwitterOAuth(my_oauth)
install.packages("RJSONIO", repos = "http://www.omegahat.org/R", type="source")
install.packages("Rstem", repos = "http://www.omegahat.org/R", type="source")

#### EXTRACTING TWEETS FROM MEDIA SOURCES

source("functions.r")

# 1. WASHINGTON POST
getTimeline(filename="tweets_news.json", screen_name="washingtonpost", 
            n=500, oauth=my_oauth, trim_user="false")
# 2. NYTIMES
getTimeline(filename="tweets_news.json", screen_name="nytimes", 
            n=500, oauth=my_oauth, trim_user="false")
# 3. WALL STREET JOURNAL
getTimeline(filename="tweets_news.json", screen_name="wsj", 
            n=500, oauth=my_oauth, trim_user="false")
# 4. BBC NEWS WORLD
getTimeline(filename="tweets_news.json", screen_name="bbcworld", 
            n=500, oauth=my_oauth, trim_user="false")
# 5. CHICAGO TRIBUNE
getTimeline(filename="tweets_news.json", screen_name="chicagotribune", 
            n=500, oauth=my_oauth, trim_user="false")
# 6. LA TIMES
getTimeline(filename="tweets_news.json", screen_name="latimes", 
            n=500, oauth=my_oauth, trim_user="false")
# 7. ALJAZEERA
getTimeline(filename="tweets_news.json", screen_name="ajenglish", 
            n=500, oauth=my_oauth, trim_user="false")
# 8. CCTV
getTimeline(filename="tweets_news.json", screen_name="cctvnews", 
            n=500, oauth=my_oauth, trim_user="false")
# 9. CNN
getTimeline(filename="tweets_news.json", screen_name="cnn", 
            n=500, oauth=my_oauth, trim_user="false")
# 10. ABC
getTimeline(filename="tweets_news.json", screen_name="abc", 
            n=500, oauth=my_oauth, trim_user="false")

news <- parseTweets("tweets_news.json")

# loading lexicon of positive and negative words (from Neal Caren)
lexicon <- read.csv("lexicon.csv", stringsAsFactors=F)
pos.words <- lexicon$word[lexicon$polarity=="positive"]
neg.words <- lexicon$word[lexicon$polarity=="negative"]

# function to clean the text
clean_tweets <- function(text){
  # loading required packages
  lapply(c("tm", "Rstem", "stringr"), require, c=T, q=T)
  # avoid encoding issues by dropping non-unicode characters
  utf8text <- iconv(text, to='UTF-8-MAC', sub = "byte")
  # remove punctuation and convert to lower case
  words <- removePunctuation(utf8text)
  words <- tolower(words)
  # spliting in words
  words <- str_split(words, " ")
  return(words)
}

# now we clean the text

clean_news <- clean_tweets(news$text)

# but we want to aggregate over many tweets...
classifier <- function(text, pos.words, neg.words){
  # classifier
  scores <- unlist(lapply(text, classify, pos.words, neg.words))
  n <- length(scores)
  positive <- as.integer(length(which(scores>0))/n*100)
  negative <- as.integer(length(which(scores<0))/n*100)
  neutral <- 100 - positive - negative
  #cat(n, "tweets:", positive, "% positive,",
   #   negative, "% negative,", neutral, "% neutral")
  y <- c(positive, negative, neutral)
  table <- matrix(c(y), ncol=3, byrow=TRUE)
  colnames(table) <- c('Positive', 'Negative', 'Neutral')
  rownames(table) <- c('News Sentiment')
  table <- as.table(table)
  table
  barplot(y, main = "News Sentiment", names.arg=c('Positive', 'Negative', 'Neutral'), ylab="%")
}

# applying classifier function
classifier(clean_news, pos.words, neg.words)


#
