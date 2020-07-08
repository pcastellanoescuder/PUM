
library(ggplot2)
library(HistData)
library(tidyverse)

data(Nightingale) 

p <-select(Nightingale, Date, Disease, Wounds, Other) %>%
  pivot_longer(-Date, names_to = "Cause", values_to = "Deaths") %>%
  ggplot(aes(Date, Deaths)) +
  geom_line() +
  facet_wrap(~ Cause, ncol = 1)

p
