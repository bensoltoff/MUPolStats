## plots.R
## 8/31/15 BCS
## Generate relevant plots


source("download.R", echo = TRUE)


# frequency of tweets, by day
tweets %>%
  # get date of tweets
  mutate(date = yday(created)) %>%
  select(created, date) %>%
  # summarize for number of tweets by date
  group_by(date) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = date, y = n)) +
  geom_line()
