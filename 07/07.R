library(tidyverse)

dat <- data.table::fread(here::here("07","input.txt")) %>% as.numeric
tmp <- c(16,1,2,0,4,2,7,1,2,14)

## Part One
out <- list(pos=0,ss=Inf)
for(i in min(dat):max(dat)){
  test.ss <- sum(abs(dat-i))
  if(test.ss < out$ss){
    out <- list(pos=i, ss=test.ss)
  }
}
out

## Part Two
out <- list(pos=0,ss=Inf)
for(i in min(dat):max(dat)){
  test.ss <- 0
  for(j in seq_along(dat)){
    test.ss <- test.ss + sum(seq(abs(dat[j]-i)))
  }
  if(test.ss < out$ss){
    out <- list(pos=i, ss=test.ss)
  }
}
out
