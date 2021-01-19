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

#BN Filter
# Source required functions
source("bnf_methods.R")

# Make the series GDP a 'ts' object
gdp <- ts(data = GDP$gdp, end = c(2020, 3), frequency = 4)
y <- transform_series(y = gdp, take_log = TRUE, pcode = "p1") # same as: log(raw_y) * 100.0

#Automatically determined delta and full sample mean demeaning method
bnf2 <- bnf(y)
BNF_FILTER <- as.data.frame(bnf2$cycle)
BNF_FILTER$date<-yearqtr(1959 + seq(3, 246)/4)

#HP Filter
hp <- hpfilter(GDP$lgdp, freq = 1600, type = "lambda")
GDP <- GDP %>% 
  mutate(hp_trend = c(hp$trend),
         cycle_hp = hp$cycle)

#Hamilton Regression
nlags <- 8:11
setDT(GDP)[, paste0("lgdp_lag", nlags) := shift(lgdp, nlags, type = "lag"), ] 
regression_string <- paste0("lgdp ~ ", paste(paste0("lgdp_lag", nlags), collapse = " + "))
mod <- lm(regression_string, data = GDP)

GDP <- GDP %>% 
  add_predictions(mod, var = "hamilton_trend") %>% 
  mutate(cycle_hamilton = lgdp - hamilton_trend)

# Join series together at a Qtly Frequency
CYCLES <- merge(GDP, BNF_FILTER, by="date", all = T)

##Plot Results
CYCLES %>% 
  filter(date > "2014 Q4") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = cycle_hp), size=1.0) +
  geom_line(aes(y = cycle_hamilton), size=1.0, colour = "red") +
  geom_line(aes(y = Cycle), size=1.0, colour = "blue") +
  annotate("text", x = 2017.75, y = 1.5, label = "HP Filter") +
  annotate("text", x = 2019.15, y = -1, label = "Hamilton Regression", colour = "red") +
  theme_minimal() +
  xlab("") + 
  ylab("% of Potential GDP") +
  ggtitle("Output Gap in Australia", subtitle = "Friends don't let friends use the HP filter")
