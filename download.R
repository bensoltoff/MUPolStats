# install the necessary packages
# install.packages("twitteR")
# install.packages("wordcloud")
# install.packages("tm")

require("twitteR")
require("wordcloud")
require("RColorBrewer")
require("tm")
require("readr")
require("dplyr")
require("lubridate")
require("ggplot2")
require("googlesheets")
require("RWeka")
require("magrittr")

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
tweets_raw <- searchTwitter("#POL241 OR #POL352 OR #POL353", n = 5000, since = "2016-01-24")

# save locally
register_sqlite_backend("spring_2016.db")
store_tweets_db(tweets_raw)

# load all tweets
tweets <- load_tweets_db(as.data.frame = TRUE)

# convert to data frame
tweets %<>%
  tbl_df %>%
  # keep only distinct rows
  distinct %>%
  # keep only relevant columns
  select(text, created, screenName, retweetCount, isRetweet, longitude, latitude) %>%
  # rename columns
  rename(twitter_id = screenName) %>%
  # convert time columns
  mutate(date = as.Date(created, format="%Y-%m-%d")) %>%
  # fix tweets with non-ASCII characters
  mutate(text = iconv(text, "latin1", "ASCII", sub = "")) %>%
  # convert twitter_id to lowercase
  mutate(twitter_id = tolower(twitter_id))


# link with student data
students <- gs_title("Register Your Twitter Account (Responses - Spring 2016)") %>%
  gs_read %>%
  # rename columns
  rename(timestamp = Timestamp,
         first_name = `First Name`,
         last_name = `Last Name`,
         miami_id = `Miami UniqueID`,
         twitter_id = `What is your registered Twitter username?`,
         pol241 = `Are you enrolled in POL 241 (Section E with Dr. Soltoff only)?`,
         pol352 = `Are you enrolled in POL 352?`,
         pol353 = `Are you enrolled in POL 353?`) %>%
  # convert class indicators to binary TRUE/FALSE
  mutate_each(funs(convert = ifelse(. == "Yes", TRUE, FALSE)), pol241:pol353) %>%
  # convert twitter_id to lowercase
  mutate(twitter_id = tolower(twitter_id))

# combine tweets and students
tweets %<>%
  left_join(students)




