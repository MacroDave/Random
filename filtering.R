library(tidyverse)
library(data.table)
library(zoo)
library(lubridate)
library(modelr)
library(mFilter)
library(readabs)
library(ggthemes)

GDP <- read_abs(series_id = "A2304402X") %>%
  mutate(date = as.yearmon(date)) %>% 
  select(date, gdp = value) %>% 
  mutate(lgdp = 100 * log(as.numeric(gdp))) %>% 
  mutate(date = as.yearqtr(date))

hp <- hpfilter(GDP$lgdp, freq = 1600, type = "lambda")
GDP <- GDP %>% 
  mutate(hp_trend = c(hp$trend),
         cycle_hp = hp$cycle)

nlags <- 8:11
setDT(GDP)[, paste0("lgdp_lag", nlags) := shift(lgdp, nlags, type = "lag"), ] 

regression_string <- paste0("lgdp ~ ", paste(paste0("lgdp_lag", nlags), collapse = " + "))

mod <- lm(regression_string, data = GDP)

GDP <- GDP %>% 
  add_predictions(mod, var = "hamilton_trend") %>% 
  as.tibble() %>% 
  mutate(cycle_hamilton = lgdp - hamilton_trend) %>%
  filter(date > "2014 Q4")

GDP %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = cycle_hp), size=1.0) +
  geom_line(aes(y = cycle_hamilton), size=1.0, colour = "red") +
  annotate("text", x = 2017.75, y = 1.5, label = "HP Filter") +
  annotate("text", x = 2019.15, y = -1, label = "Hamilton Filter", colour = "red") +
  ggthemes::theme_economist() +
  ggtitle("Output Gap in Australia", subtitle = "Friends don't let friends use the HP filter")


