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
tweets <- searchTwitter("#POL241|#POL351|#POL353", n = 5000)

# convert to data frame
df <- twListToDF(tweets) %>%
  tbl_df








