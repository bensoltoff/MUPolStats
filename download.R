# install the necessary packages
# install.packages("twitteR")
# install.packages("wordcloud")
# install.packages("tm")

require("twitteR")
require("wordcloud")
require("tm")
require("dplyr")
require("lubridate")
require("ggplot2")
require("googlesheets")

rm(list = ls())

# necessary file for Windows
# download.file(url = "http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")

# to get your consumerKey and consumerSecret see the twitteR documentation for instructions
## load keys.R to import keys
source("keys.R")

setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

# get tweets from all three classes
tweets_raw <- searchTwitter("#POL241 OR #POL351 OR #POL353", n = 5000, since = "2015-08-01")

# convert to data frame
tweets <- twListToDF(tweets_raw) %>%
  tbl_df %>%
  # keep only relevant columns
  select(text, created, screenName, retweetCount, isRetweet, longitude, latitude) %>%
  # rename columns
  rename(twitter_id = screenName) %>%
  # convert time columns
  mutate(date = as.Date(created, format="%Y-%m-%d"))

# link with student data
students <- gs_title("Register Your Twitter Account (Responses)") %>%
  gs_read %>%
  # rename columns
  rename(timestamp = Timestamp,
         first_name = First.Name,
         last_name = Last.Name,
         miami_id = Miami.UniqueID,
         twitter_id = What.is.your.registered.Twitter.username.,
         pol241 = Are.you.enrolled.in.POL.241..Section.G.with.Dr..Soltoff.only..,
         pol351 = Are.you.enrolled.in.POL.351.,
         pol353 = Are.you.enrolled.in.POL.353.) %>%
  # convert class indicators to binary TRUE/FALSE
  mutate_each(funs(convert = ifelse(. == "Yes", TRUE, FALSE)), pol241:pol353)

# combine tweets and students
left_join(tweets, students)




