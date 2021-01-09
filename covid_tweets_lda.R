# Perform LDA analysis on extracted tweets
# Should have written a function for this for long-term usability

library(tidytext)
library(dplyr)
library(ggplot2)

load("C:/Users/ddrsq/rscripts/covid_tweets/english_tweets.Rdata")
load("C:/Users/ddrsq/rscripts/covid_tweets/0308_extended.Rdata")

library(topicmodels)
library(digest)

# First we create a corpus from our tweets and clean it
library(tm)
corpus_0301 <- Corpus(VectorSource(as.vector(tweets_0301$text)))
corpus_0301 <- tm_map(corpus_0301, removeWords, stopwords("english"))
corpus_0301 <- tm_map(corpus_0301, content_transformer(removePunctuation))
corpus_0301 <- tm_map(corpus_0301, content_transformer(removeNumbers))
corpus_0301 <- tm_map(corpus_0301, content_transformer(tolower))
corpus_0301 <- tm_map(corpus_0301, content_transformer(stripWhitespace))
corpus_0301 <- tm_map(corpus_0301, content_transformer(stemDocument), language= "english")

# We also go ahead and remove some specific stopwords, like coronavirus
corpus_0301 <- tm_map(corpus_0301, removeWords, c("coronavirus","corona", "virus", "covid")) 

# Then turn it into a document-term matrix and run LDA
DTM_0301 <- DocumentTermMatrix(corpus_0301, control = list(wordLengths = c(2,Inf)))
topic_model_0301 <- LDA(DTM_0301, k=10, control = list(seed=321))
topics_0301 <- tidy(topic_model_0301, matrix="beta")

# Sort and display stuff
top_terms_0301 <- topics_0301 %>%
  group_by(topic) %>%
  top_n(15,beta) %>%
  ungroup() %>%
  arrange(topic,-beta)

top_terms_0301 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Checking to see if ampersands are being included

# tidy_covid_tweets <- tweets_0308_extended %>%
#   select(date, text) %>%
#   unnest_tokens("word",text)
# 
# amp_tweet <- tidy_covid_tweets %>% 
#   filter(word == "amp")
# 
# specific_amp_tweet <- tweets_0308_extended %>%
#   filter(date == "2020-03-08 11:37:31")
# 
# View(amp_tweet)
# sorted_tweet <- tidy_covid_tweets %>%
#   count(word) %>%
#   arrange(desc(n))
# View(sorted_tweet)


# Same thing for 03-08
corpus_0308 <- Corpus(VectorSource(as.vector(tweets_0308_extended$text)))
corpus_0308 <- tm_map(corpus_0308, removeWords, stopwords("english"))
corpus_0308 <- tm_map(corpus_0308, content_transformer(removePunctuation))
corpus_0308 <- tm_map(corpus_0308, content_transformer(removeNumbers))
corpus_0308 <- tm_map(corpus_0308, content_transformer(tolower))
corpus_0308 <- tm_map(corpus_0308, content_transformer(stripWhitespace))
corpus_0308 <- tm_map(corpus_0308, content_transformer(stemDocument), language= "english")

# We also go ahead and remove some specific stopwords, like coronavirus
corpus_0308 <- tm_map(corpus_0308, removeWords, c("coronavirus","corona", "virus", "covid", "amp", "'s", "y'")) 

# Then turn it into a document-term matrix and run LDA
DTM_0308 <- DocumentTermMatrix(corpus_0308, control = list(wordLengths = c(2,Inf)))
topic_model_0308 <- LDA(DTM_0308, k=10, control = list(seed=321))
topics_0308 <- tidy(topic_model_0308, matrix="beta")

# Sort and display stuff
top_terms_0308 <- topics_0308 %>%
  group_by(topic) %>%
  top_n(1,beta) %>%
  ungroup() %>%
  arrange(topic,-beta)

top_terms_0308 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

