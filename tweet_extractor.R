# Read in a file from https://github.com/echen102/COVID-19-TweetIDs and extract tweets

library(tidyverse)

# Download any particular file as a txt file and read it into a dataframe
tweet_ids <- read.delim("C:/Users/ddrsq/rscripts/coronavirus-tweet-id-2020-03-01-00.txt", header = FALSE, sep = "\n", colClasses = "character")
colnames(tweet_ids) <- "ids"
class(tweet_ids$ids)

# Builds a tibble of the data from the tweets we actually care about
tweet_tibble <- tibble(
  date = character(),
  language = character(),
  text = character()
)
for(i in 1:length(tweet_ids$ids)){
  # For debugging purposes only
  if(i%%100==0){
    print(i)
  }
  
  # Grabs the whole tweet with its metadata 
  curr_tweet <- lookup_tweets(tweet_ids$ids[i], parse = TRUE, token = NULL)
  # This is the part of the tweet we actually care about
  # Technically we don't need to store the text
  curr_tweet_tib <- tibble(
    # Could truncate to date for a coarser measure
    date = c(as.character(curr_tweet$created_at)),
    language = c(curr_tweet$lang),
    text = c(curr_tweet$text)
  )
  
  #Adds to database as long as tweet not empty
  if(!is.null(curr_tweet)){
      tweet_tibble <- rbind(tweet_tibble, curr_tweet_tib)
  }
}

View(rate_limit())

tweet_tibble %>% 
  group_by(language) %>%
  summarise(
    count = n()
  ) %>%
  arrange(desc(count))
