# install the necessary packages
# install.packages("twitteR")
# install.packages("wordcloud")
# install.packages("tm")

require("twitteR")
require("wordcloud")
require("tm")

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

#the cainfo parameter is necessary only on Windows
r_stats <- searchTwitter("#POL241", n = 5000)

# should get 1500
length(r_stats)
