library(dplyr)
library(readr)
library(leaflet)
library(rgdal)
library(twitteR)
library(streamR)
library(ROAuth)

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

############################# Get Tweet into JSON file ##############################

# I liked Anahita's method for filtering tweets as it was far more conducive to tweets with location 
# than searching alone
# https://github.com/anahitabahri/Rap-Twitter-Analysis/blob/master/get_tweets.R

filterStream(file = "US_Netflix.json", # Save tweets in a json file
             track = "Netflix", # Track mentions of Netflix
             language = "en", # Language English
             location = c(-125,25,-66,50), # Get locations in the U.S. 
             timeout = 900, # Keep connection alive for 900 seconds / 15 minutes 
             oauth = my_oauth) # Use my_oath from above as OAuth credentials

################################   Mapping   ########################################

# How to Map US Tweets
# https://github.com/anahitabahri/Rap-Twitter-Analysis/blob/master/mapping_us_tweets.R

# parse us_tweets.json file
us_netflix_tweets <- parseTweets("US_Netflix.json") # 20,592 tweets

# select some columns
us_netflix_tweets.df = us_netflix_tweets %>% select(text, retweet_count, favorited, retweeted, created_at, verified,
                                     location, description, user_created_at, statuses_count, followers_count,
                                     favourites_count, name, time_zone, friends_count, place_lat, place_lon)

# rename columns to lon / lat
us_netflix_tweets.df <- rename(us_netflix_tweets.df, lon = place_lon, lat = place_lat)

# filter for tweets from within the US
us_netflix_tweets.df <- filter(us_netflix_tweets.df, lat >= 19.50)
us_netflix_tweets.df <- filter(us_netflix_tweets.df, lat <= 64.85)
us_netflix_tweets.df <- filter(us_netflix_tweets.df, lon >= -161.76)
us_netflix_tweets.df <- filter(us_netflix_tweets.df, lon <= -68.01)


# remove na values for lon/lat coordinates
us_netflix_tweets.df <- us_netflix_tweets.df[complete.cases(us_netflix_tweets.df$lon),]

# export csv, so we don't have to keep parsing through json file
write.csv(us_netflix_tweets.df, "us_netflix_tweets.csv", row.names = FALSE)

# read csv, instead of parsing json file
us_netflix_tweets.df <- read_csv('us_netflix_tweets.csv')
us_netflix_tweets.df <- data.frame(us_netflix_tweets.df)

# plot map with leaflet
map <- leaflet(us_netflix_tweets.df) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
                                         attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 
# m %>% setView(-100.044345, 41.314352, zoom = 4) # sets central location to NEBRASKA, so we can zoom into the US
map %>% addCircles(~lon, ~lat, popup=us_netflix_tweets.df$lon, weight = 3, radius=40, 
                 color="#f70bb8", stroke = TRUE, fillOpacity = 0.8) 

# get simple stats for follower count
min(us_netflix_tweets.df$followers_count) # 0
mean(us_netflix_tweets.df$followers_count) # 2312.572 
median(us_netflix_tweets.df$followers_count) # 551
max(us_netflix_tweets.df$followers_count) # 1,163,554

# get simple stats for retweet count
min(us_netflix_tweets.df$favourites_count) # 0
mean(us_netflix_tweets.df$favourites_count) # 12,271.45 
median(us_netflix_tweets.df$favourites_count) # 3,847
max(us_netflix_tweets.df$favourites_count) # 595,674
