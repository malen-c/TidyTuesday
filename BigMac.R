library(tidyverse)

bigmac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')
inflation <- 1.51 #ratio of 2020 to 2000 dollar via US gov CPI data

#creating two dataframes for the earliest and latest recording of data, then removing countries that weren't surveyed both times

bigmac2000 <- bigmac %>%
  filter(as.character(date) == '2000-04-01')
bigmac2020 <- bigmac %>%
  filter(as.character(date) == '2020-07-01') %>%
  filter(name %in% bigmac2000$name)

#adjusting for inflation then recombining

bigmac2000$dollar_price <- bigmac2000$dollar_price * inflation
bigmac <- rbind(bigmac2000, bigmac2020)

#arranging by size of difference
bigmac <- bigmac %>%
  group_by(name) %>%
  mutate(difference = diff(dollar_price)) %>%
  arrange(dollar_price)
order <- filter(bigmac, date == '2020-07-01')$name


ggplot(bigmac, aes(x = fct_relevel(name, order), 
                   y = dollar_price, 
                   color = factor(date))) +
  geom_point(size = 2, show.legend = FALSE) +
  geom_line(arrow = arrow(length=unit(0.2,"cm"), type = 'closed'), data = filter(bigmac, difference > 0), mapping = aes(group = name), color = 'red') +
  geom_line(arrow = arrow(length=unit(0.2,"cm"), ends = 'first', type = 'closed'), data = filter(bigmac, difference < 0), mapping = aes(group = name), color = 'blue') +
  coord_flip() +
  labs(x = 'Country', y = 'Price of Big Mac in 2020 USD', 
       color = 'Date', labels = '$') +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal()
