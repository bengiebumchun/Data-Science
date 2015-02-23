################################################################
## DATA SCIENCE
## Assignment #1
## A. Is news a downer?
## February 23rd, 2015
## Ben Chun
################################################################

setwd("/Users/giebumchun/Google Drive/MIDP 1/4. Spring 2015/Data Science/Assignment/Assignment 1")

## INSTALLING PACKAGES
doInstall <- TRUE  # Change to FALSE if you don't want packages installed.
toInstall <- c("ROAuth", "twitteR", "streamR", "ggplot2", "stringr",
               "tm", "RCurl", "maps", "Rfacebook", "topicmodels", "devtools")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}


## AUTHENTICATION
library(ROAuth)

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "EKDDTbiQM6N8PdYvG6rmmrFrw"
consumerSecret <- "GPvpYJVNlpfhQmIAbE82b5StCpSuR7MeNfXrqs1oImHC7lbz8r"

my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret, requestURL=requestURL,
                             accessURL=accessURL, authURL=authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

#Calling libraries and installing necessary packages
library(twitteR)
library(streamR)

install.packages("RJSONIO", repos = "http://www.omegahat.org/R", type="source")
install.packages("Rstem", repos = "http://www.omegahat.org/R", type="source")


#### EXTRACTING TWEETS FROM MEDIA SOURCES

# Function to get timelines <-- from Pablo's 'functions.r' code
getTimeline <- function(filename, n=3200, oauth, screen_name=NULL, 
                        id=NULL, since_id=NULL, trim_user="true", sleep=.5){
  
  require(rjson); require(ROAuth)
  
  ## while rate limit is 0, open a new one
  limit <- getLimitTimeline(my_oauth)
  cat(limit, " hits left\n")
  while (limit==0){
    Sys.sleep(sleep)
    # sleep for 5 minutes if limit rate is less than 100
    rate.limit <- getLimitRate(my_oauth)
    if (rate.limit<100){
      Sys.sleep(300)
    }
    limit <- getLimitTimeline(my_oauth)
    cat(limit, " hits left\n")
  }
  ## url to call
  url <- "https://api.twitter.com/1.1/statuses/user_timeline.json"
  
  ## first API call
  if (!is.null(screen_name)){
    params <- list(screen_name = screen_name, count=200, trim_user=trim_user)
  }
  if (!is.null(id)){
    params <- list(id=id, count=200, trim_user=trim_user)   
  }
  if (!is.null(since_id)){
    params[["since_id"]] <- since_id
  }
  
  url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl")) 
  Sys.sleep(sleep)
  ## one API call less
  limit <- limit - 1
  ## changing oauth token if we hit the limit
  cat(limit, " hits left\n")
  while (limit==0){
    Sys.sleep(sleep)
    # sleep for 5 minutes if limit rate is less than 100
    rate.limit <- getLimitRate(my_oauth)
    if (rate.limit<100){
      Sys.sleep(300)
    }
    limit <- getLimitTimeline(my_oauth)
    cat(limit, " hits left\n")
  }
  ## trying to parse JSON data
  ## json.data <- fromJSON(url.data, unexpected.escape = "skip")
  json.data <- RJSONIO::fromJSON(url.data)
  if (length(json.data$error)!=0){
    cat(url.data)
    stop("error! Last cursor: ", cursor)
  }
  ## writing to disk
  conn <- file(filename, "a")
  invisible(lapply(json.data, function(x) writeLines(rjson::toJSON(x), con=conn)))
  close(conn)
  ## max_id
  tweets <- length(json.data)
  max_id <- json.data[[tweets]]$id_str
  cat(tweets, "tweets. Max id: ", max_id, "\n")
  max_id_old <- "none"
  if (is.null(since_id)) {since_id <- 1}
  
  while (tweets < n & max_id != max_id_old & 
           as.numeric(max_id) > as.numeric(since_id)){
    max_id_old <- max_id
    if (!is.null(screen_name)){
      params <- list(screen_name = screen_name, count=200, max_id=max_id,
                     trim_user=trim_user)
    }
    if (!is.null(id)){
      params <- list(id=id, count=200, max_id=max_id, trim_user=trim_user)
    }
    if (!is.null(since_id)){
      params[['since_id']] <- since_id
    }
    url.data <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
                                      cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl")) 
    Sys.sleep(sleep)
    ## one API call less
    limit <- limit - 1
    ## changing oauth token if we hit the limit
    cat(limit, " hits left\n")
    while (limit==0){
      Sys.sleep(sleep)
      # sleep for 5 minutes if limit rate is less than 100
      rate.limit <- getLimitRate(my_oauth)
      if (rate.limit<100){
        Sys.sleep(300)
      }
      limit <- getLimitTimeline(my_oauth)
      cat(limit, " hits left\n")
    }
    ## trying to parse JSON data
    ## json.data <- fromJSON(url.data, unexpected.escape = "skip")
    json.data <- RJSONIO::fromJSON(url.data)
    if (length(json.data$error)!=0){
      cat(url.data)
      stop("error! Last cursor: ", cursor)
    }
    ## writing to disk
    conn <- file(filename, "a")
    invisible(lapply(json.data, function(x) writeLines(rjson::toJSON(x), con=conn)))
    close(conn)
    ## max_id
    tweets <- tweets + length(json.data)
    max_id <- json.data[[length(json.data)]]$id_str
    cat(tweets, "tweets. Max id: ", max_id, "\n")
  }
}
getLimitTimeline <- function(my_oauth){
  require(rjson); require(ROAuth)
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "statuses,application")
  response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(fromJSON(response)$resources$statuses$`/statuses/user_timeline`[['remaining']]))
  
}
getLimitRate <- function(my_oauth){
  require(rjson); require(ROAuth)
  url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
  params <- list(resources = "followers,application")
  response <- my_oauth$OAuthRequest(URL=url, params=params, method="GET", 
                                    cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  return(unlist(fromJSON(response)$resources$application$`/application/rate_limit_status`[['remaining']]))
}

##########Extracting tweets from 10 different media sources

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

# PARSING TWEETS
news <- parseTweets("tweets_news.json")

# SENTIMENT ANALYSIS

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

# cleaning tweets that were parsed

clean_news <- clean_tweets(news$text)

# Function to classify sentiment of words in the tweets
classify <- function(words, pos.words, neg.words){
  # count number of positive and negative word matches
  pos.matches <- sum(words %in% pos.words)
  neg.matches <- sum(words %in% neg.words)
  return(pos.matches - neg.matches)
}

classifier <- function(text, pos.words, neg.words){
  # classifier
  scores <- unlist(lapply(text, classify, pos.words, neg.words))
  n <- length(scores)
  positive <- as.integer(length(which(scores>0))/n*100)
  negative <- as.integer(length(which(scores<0))/n*100)
  neutral <- 100 - positive - negative
  # show sentiments
  cat(n, "tweets:", positive, "% positive,",
      negative, "% negative,", neutral, "% neutral")
  #storing results to variable
  y <- c(positive, negative, neutral)
  # plot sentiment results in bar graphs
  barplot(y, main = "News Sentiment", names.arg=c('Positive', 'Negative', 'Neutral'), ylab="%")
  # exporting bar graph to pdf
  dev.copy2pdf(file="Barplot.pdf", width = 7, height = 5)
}

# applying classifier function to show the sentiment analysis in text and graph format
classifier(clean_news, pos.words, neg.words)

##########################
# The End 
##########################
