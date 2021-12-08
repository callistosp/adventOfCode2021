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
length(out)

## Part Two
out <- dat
## iterate across 80 days
for(i in 1:256){
  ## subtract one for day countdown
  out <- out - 1
  ## how many fish countdown ended
  newfish <- length(out[out == -1])
  ## replace -1s with 6s
  out[out == -1] <- 6
  ## add new fish
  out <- c(out, rep(8, newfish))
}
out
length(out)