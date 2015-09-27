## plots.R
## 8/31/15 BCS
## Generate relevant plots


source("download.R", echo = TRUE)

# color palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# frequency of tweets, by date
tweets %>%
  # summarize for number of tweets by date
  group_by(date) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  scale_x_date() +
  theme_bw() +
  labs(x = "Date",
       y = "Number of Tweets")

# frequency of tweets, by day of week
tweets %>%
  # summarize for number of tweets by day of week
  mutate(day = wday(date, label = TRUE)) %>%
  group_by(day) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = day, y = n, fill = day)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Day of Week",
       y = "Number of Tweets") +
  theme(legend.position = "none")

# frequency of tweets, by hour of day
tweets %>%
  # summarize for number of tweets by hour
  mutate(hour = hour(created)) %>%
  group_by(hour) %>%
  summarize(n = n()) %>%
  ## merge with 0:23 data frame to fill in empty values
  right_join(data_frame(hour = 0:23)) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_line() +
  theme_bw() +
  labs(x = "Hour of Day",
       y = "Number of Tweets")

# frequency of tweets, by user
tweets %>%
  group_by(twitter_id) %>%
  summarize(n = n()) %>%
  left_join(students) %>%
  distinct(twitter_id) %>%
  arrange(-n) %>%
  mutate(twitter_id = factor(twitter_id, levels = twitter_id)) %>%
  ggplot(aes(x = twitter_id, y = n, fill = n)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Twitter Handle",
       y = "Number of Tweets")

# word cloud
## create a corpus
tweet_corpus <- Corpus(VectorSource(tweets$text))

# create document term matrix applying some transformations
tdm <- TermDocumentMatrix(tweet_corpus,
                          control = list(stopwords = c("#POL241", "#POL351", "#POL353", "pol",
                                                       stopwords("english")),
                                         removePunctuation = TRUE,
                                         removeNumbers = TRUE, tolower = TRUE,
                                         wordLengths = c(4, Inf)))

## define tdm as matrix
m <- as.matrix(tdm)

## get word counts in decreasing order
word_freqs <- sort(rowSums(m), decreasing = TRUE)

## create a data frame with words and their frequencies
dm <- data_frame(word = names(word_freqs), freq = word_freqs)

## plot wordcloud
wordcloud(dm$word, dm$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))





