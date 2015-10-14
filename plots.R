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

# frequency of tweets, by user, in table form
tweets %>%
  group_by(first_name, last_name, miami_id) %>%
  # count number of relevant tweets for each student
  summarize(n = n(),
            n_per_week = n() / 6) %>%
  # calculate points to be awarded based on per_week tweet frequency
  mutate(grade = ifelse(n_per_week > 3, 50,
                        ifelse(n_per_week > 2, 40,
                               ifelse(n_per_week > 1, 35,
                                      ifelse(n_per_week > 0, 30, 20))))) %>%
  left_join(students) %>%
  distinct(miami_id) %>%
  group_by %>%
  arrange(last_name, first_name) %>%
  select(-timestamp) %>%
  write_csv("tweet_summary_FA15.csv")
  

# word cloud
## extract text
tweets.text <- tweets %>%
  # clean up text
  mutate(
    # convert all text to lower case
    text = tolower(text),
    # Replace blank space (“rt”)
    text = gsub("rt", "", text),
    # Replace @UserName
    # text = gsub("@\\w+", "", text),
    # Remove punctuation
    text = gsub("[^[:alnum:][:space:]'@#]", "", text),
    # Remove links
    text = gsub("http\\w+", "", text),
    # Remove tabs
    text = gsub("[ |\t]{2,}", "", text),
    # Remove linebreaks
    text = gsub("\n", " ", text),
    # Remove blank spaces at the beginning
    text = gsub("^ ", "", text),
    # Remove blank spaces at the end
    text = gsub(" $", "", text),
    # Remove class hashtags
    text = gsub("#pol241|#pol351|#pol353", "", text))

## create corpus
tweets.text.corpus <- Corpus(VectorSource(tweets.text %>%
                                            # filter(pol351 == TRUE) %>%
                                            .$text))

## clean up by removing stop words
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x) removeWords(x, stopwords()))

## generate n-grams
options(mc.cores = 1)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 4))
tdm <- TermDocumentMatrix(tweets.text.corpus, control = list(tokenize = BigramTokenizer,
                                                             wordLengths = c(4, Inf)))

## define tdm as matrix
m <- as.matrix(tdm)

## get word counts in decreasing order
word_freqs <- sort(rowSums(m), decreasing = TRUE) 

# create a data frame with words and their frequencies
dm <- data_frame(word = names(word_freqs), freq = word_freqs)

## plot wordcloud
wordcloud(dm$word, dm$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"),
          rot.per = 0, fixed.asp = FALSE, max.words = 100, min.freq = 5)






