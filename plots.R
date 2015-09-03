## plots.R
## 8/31/15 BCS
## Generate relevant plots


source("download.R", echo = TRUE)


# frequency of tweets, by day
tweets %>%
  # summarize for number of tweets by date
  group_by(date) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  scale_x_date()
