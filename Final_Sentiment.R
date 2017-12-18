library(tidytext)
library(tidyverse)
library(tidyr)
library(knitr)
library(ggplot2)
library(streamR)
library(ROAuth)
library(wordcloud)
library(reshape2)

############################# Twitter Authorization ##############################

api_key <- "e6MCG5Ym5C8odoFj0YfXGKjuV"
api_secret <- "ddJ1uuQ8nAcS4KK6hBAWfX8nuFGYOSqxxN4mj5AtJeQafqMcak"
access_token <- "2171255556-xvHH9cm9aqwNXl93tRUJ8AhWfooEU2LYJvfhptd"
access_token_secret <- "iFGjV1ELi6pRGHdCgCxzAOzDTwbPdpZAg8qgfsH3zEOyx"

my_oauth <- OAuthFactory$new(consumerKey=api_key,
                             consumerSecret=api_secret, requestURL="https://api.twitter.com/oauth/request_token",
                             accessURL="https://api.twitter.com/oauth/access_token", 
                             authURL="http://api.twitter.com/oauth/authorize")

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

############################# Get Tweets into JSON file ##############################

filterStream(file = "Netflix.json", # Save tweets in a json file
             track = "Netflix", # Track mentions of Netflix
             timeout = 1100, # Keep connection alive for 1100 seconds / 18.33 minutes 
             oauth = my_oauth) # Use my_oath from above as OAuth credentials


############################# Make JSON File a DataFrame ##############################

tweets <- parseTweets(tweets = "Netflix.json") # Turn the JSON file into a dataframe for analysis

tweets <- filter(tweets, lang == "en") # Filter tweets for only English

netflix_df <- data_frame(line = 1:length(tweets$text), text = tweets$text)

#########################         Tokenize       ##############################

tidy_tweets <- netflix_df %>% # Store the tidied tweets in a new dataframe
  unnest_tokens(word,text) # tokenizes words for analysis

############################# Take Out Stop Words ##############################

data(stop_words)

tidy_tweets <- tidy_tweets %>% 
  anti_join(stop_words) # Remove stop words

############################# Look at Most Used Words ##############################

most_tweets <- tidy_tweets %>% # Makes a separate data frame to look at the most used words
  count(word, sort = TRUE) %>% # I used this to find which words were utterly unrelated 
  mutate(word = reorder(word, n)) # and filter them out, which is done with the code below

##################### Filter Out Junk Words and Not English ##########################

tidy_tweets <- filter(tidy_tweets, word != "t.co")            # filters out "t.co"
tidy_tweets <- filter(tidy_tweets, word != "https")           # filters out "https"
tidy_tweets <- filter(tidy_tweets, word != "amp")             # filters out "amp"
tidy_tweets <- filter(tidy_tweets, word != "imperialfanboy")  # filters out "imperialfanboy"
tidy_tweets <- filter(tidy_tweets, word != "xlnb")            # filters out "xlnb"
tidy_tweets <- filter(tidy_tweets, word != "rt")              # filters out "rt"

########################         Sentiment Analysis       ##############################

bing <- get_sentiments("bing") # gets bing (positive vs. negative) sentiments and stores them in bing

bing_words_count <- tidy_tweets %>% # stores tidy tweets' words that overlap with bing sentiment
  inner_join(bing) %>%              # words and sorts them by count in a tibble
  count(word, sentiment, sort = TRUE)

bing_words_count

tidy_tweets %>%                               # makes a wordcloud of the top 100 words used in tweets 
  count(word) %>%                             # about netflix
  with(wordcloud(word, n, max.words = 100))

tidy_tweets %>%                                 # makes a comparison cloud of the top 100 words used in tweets 
  inner_join(bing) %>%                          # this time split and colored based on bing sentiment 
  count(word, sentiment, sort = TRUE) %>%       # positive vs. negative
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#810dce", "#00bfc4"),
                   max.words = 100)

ggplot(bing_words_count, aes(x = sentiment, fill = sentiment, color = sentiment)) + # makes a bar graph of count of 
  geom_bar()                                                                        # negative and positive sentiment words

afinn <- get_sentiments("afinn") # gets afinn (positive vs. negative on a scale of -5 to 5) 
                                 # sentiments and stores them in afinn

afinn_counts <- tidy_tweets %>%      # stores tidy tweets' words that overlap with afinn sentiment
  inner_join(afinn) %>%              # words and sorts them by count in a tibble
  count(word, score, sort = TRUE)

write.csv(afinn_counts, "Netflix_Tweet_Sentiment.csv", row.names = FALSE) # writes a .csv to be used 
                                                                          # in the Shiny App

afinn_counts

tidy_tweets %>%                        # makes a comparison cloud of the top 100 words used in tweets
  inner_join(afinn) %>%                # this time split and colored based on afinn sentiment
  count(word, score, sort = TRUE) %>%  # positive vs. negative
  acast(word ~ score, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#810dce", "#00bfc4"),
                   max.words = 100)

ggplot(afinn_counts, aes(x = score, fill = "indianred", color = "indianred")) + # bar graph of count of each afinn
  geom_bar(show.legend = FALSE)                                                 # sentiment score (-5 to 5)

afinn_tweets <- tidy_tweets %>%        # Creates a tibble of afinn sum score for each tweet (instead of 
  inner_join(afinn) %>%                # splitting by word this regroups for each tweets total positive or 
  group_by(index = line) %>%           # negative rating)
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")
  
ggplot(afinn_tweets, aes(index, sentiment, fill = sentiment)) +  # Shows the sum afinn score for each tweet on 
  geom_col(show.legend = FALSE) +                                # one plot
  facet_wrap(~method, ncol = 1, scales = "free_y") +
  labs(title="Tweet Afinn Scores")
