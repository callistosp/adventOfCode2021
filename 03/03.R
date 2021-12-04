library(tidyverse)

dat <- as.character(readLines(here::here("03","input.txt")))

## Part One
dat2 <- tibble(binstr=as.character(dat)) %>% 
  mutate(
    nc=nchar(binstr),
    c1 = substr(binstr, 1, 1),
    c2 = substr(binstr, 2, 2),
    c3 = substr(binstr, 3, 3),
    c4 = substr(binstr, 4, 4),
    c5 = substr(binstr, 5, 5),
    c6 = substr(binstr, 6, 6),
    c7 = substr(binstr, 7, 7),
    c8 = substr(binstr, 8, 8),
    c9 = substr(binstr, 9, 9),
    c10 = substr(binstr, 10, 10),
    c11 = substr(binstr, 11, 11),
    c12 = substr(binstr, 12, 12)
  ) %>% 
  select(c1:c12) %>% 
  mutate_all(as.numeric) 

mode_dat2 <- dat2 %>% 
  summarize_at(vars(c1:c12), sum) %>% 
  t() %>% as.vector
mode_dat2

dat3 <- data.frame(V1=mode_dat2) %>% 
  mutate(
    gamma = ifelse(V1>500, 1, 0),
    epsilon = ifelse(V1<500, 1, 0)
  )

base::strtoi(paste(as.character(dat3$gamma), sep="", collapse=""), base=2)
base::strtoi(paste(as.character(dat3$epsilon), sep="", collapse=""), base=2)
935*3160

## Part Two

