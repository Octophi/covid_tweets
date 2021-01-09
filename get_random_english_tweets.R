# Gets sample of 4000 random English tweets from a particular day

# List of Things you might have to change, in order of importance
# - the filepath in filepath_prefix on line 10
# - the filepath for where you want to save your stuff on line
# - the seed in set.seed, if you're not getting enough tweets on line 34


# Stores all the tweet ids
tweet_ids <- as.data.frame(NULL)
# Replace this with whatever file path you need. 
filepath_prefix = "C:/Users/ddrsq/rscripts/covid_tweets/03-02/coronavirus-tweet-id-2020-03-02-"

# This loop will autocomplete the file path with 00, 01, ..., 23 to grab and concatenate all the hours
for(i in 0:23){
  # Pads your number to make it two digits
  two_digit <- toString(i)
  if(str_length(two_digit)==1){
    two_digit <- paste("0",two_digit, sep="")
  }
  
  # Final filepath you want to look up
  filepath <- paste(filepath_prefix, two_digit, ".txt", sep="")
  
  # Unpack the txt file
  curr_tweet_ids <- read.delim(filepath, header = FALSE, sep = "\n", colClasses = "character")
  colnames(curr_tweet_ids) <- "ids"
  tweet_ids <- rbind(tweet_ids, curr_tweet_ids)
}

# We're going to grab 3400 random numbers in the range to get random tweets. 
# Ultimately, we'll want 2000 of these to be English tweets

# Don't ever change this seed value, it ensures the random indices are reproducible
set.seed(5)
distribution <- runif(n= 4000, min= 0.5, max = length(tweet_ids$ids)+0.499)
distribution <- unique(round(distribution))
distribution2 <- runif(n= 8000, min= 0.5, max = length(tweet_ids$ids)+0.499) 
distribution2 <- unique(round(distribution2))

# For whatever reason it turns our thing into a vector instead of a dataframe but whatever
sampled_tweets <- tweet_ids[distribution,]

# Now filter out the ones that aren't English
english_tweets <- as.data.frame(NULL)
all_tweets <- as.data.frame(NULL)
# This is just to make the loop more efficient
english_tweet_tally <- 0

for(i in 1:length(sampled_tweets)){
  # Just to track your progress
  if(english_tweet_tally%%100==0){
    print(english_tweet_tally)
    print(i)
  }
  
  # Grabs the whole tweet with its metadata 
  curr_tweet <- lookup_tweets(sampled_tweets[i], parse = TRUE, token = NULL)
  
  # This is the part of the tweet we actually care about
  curr_tweet_tib <- tibble(
    date = c(as.character(curr_tweet$created_at)),
    language = c(curr_tweet$lang),
    text = c(curr_tweet$text)
  )
  
  # Just if anything weird happens, so you can see what things you might've skipped over
  all_tweets <- rbind(all_tweets, curr_tweet_tib)
  
  # Check language
  if(nrow(curr_tweet)==0 || curr_tweet$lang != "en"){
    next
  }
  
  # Adds English tweets to database
  english_tweets <- rbind(english_tweets, curr_tweet_tib)
  
  # Once we've got 2000 tweets just stop
  english_tweet_tally <- english_tweet_tally + 1
  if(english_tweet_tally == 4000){
    break
  }
  
  # Deal with rate-limiting
  if(i%% 800 == 0){
    print("pausing...")
    Sys.sleep(15*60)
  }
}
View(english_tweets)
tweets_0308_extended <- unique(rbind(tweets_0308_extended, english_tweets))

save(tweets_0301, tweets_0303, tweets_0304, tweets_0306_incomplete, tweets_0308, file="C:/Users/ddrsq/rscripts/covid_tweets/english_tweets.Rdata")

save(tweets_0308_extended, file="C:/Users/ddrsq/rscripts/covid_tweets/0308_extended.Rdata")

View(tweets_0308_incomplete)
