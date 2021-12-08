library(tidyverse)

dat <- data.table::fread(here::here("06","input.txt")) %>% as.numeric
tmp <- c(3,4,3,1,2)

## Part One
out <- dat
## iterate across 80 days
for(i in 1:80){
  ## subtract one for day countdown
  out <- out - 1
  ## how many fish countdown ended
  newfish <- length(out[out == -1])
  ## replace -1s with 6s
  out[out == -1] <- 6
  ## add new fish
  out <- c(out, rep(8, newfish))
}
# out
# data.frame(day=out) %>% count(day) %>% summarize(sum(n))
length(out)

## Part Two
## skeleton data frame with all possible days
dat2 <- data.frame(days=-1:8) %>% 
  ## structure data as counts of fish at a given day
  left_join(data.frame(days=dat) %>% count(days)) %>% 
  ## fill all day potential values
  replace_na(list(n=0))

incday <- function(ndays, df){
  ## recurse until reach day 0
  if(ndays==0) return(df)
  ## shift rows by one day (decrease count for all fish)
  out <- df %>% mutate(n=lead(n)) %>% replace_na(list(n=0))
  ## for all -1, want to create a new fish (8) and restart parent fish (6)
  out$n[out$days == 6] <- out$n[out$days == 6] + out$n[out$days == -1]
  out$n[out$days == 8] <- out$n[out$days == -1]
  ## clear -1 count to keep sum correct
  out$n[out$days == -1] <- 0
  return(incday(ndays-1, out))
}

out2 <- incday(256, dat2)
out2 %>% summarize(sum(n)) %>% sprintf("%f",.)
