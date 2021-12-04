library(tidyverse)

dat <- data.table::fread(here::here("01","input.txt"))

## Part One
dat %>% 
  mutate(increase=ifelse(lead(V1)>V1, 1, 0)) %>% 
  summarize(sum(increase, na.rm=TRUE))

## Part Two
dat %>% 
  mutate(
    lag1 = lag(V1),
    lag2 = lag(V1, 2)
  ) %>% 
  mutate(
    lagsum = V1 + lag1 + lag2
  ) %>% 
  drop_na() %>% 
  mutate(increase = ifelse(lead(lagsum)>lagsum,1,0)) %>% 
  summarize(sum(increase,na.rm=TRUE))
