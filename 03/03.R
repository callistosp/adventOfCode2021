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

get.mode <- function(df, .reverse=FALSE){
  out <- df %>% 
    summarize_at(vars(c1:c12), ~ floor(sum(.x)/(n()/2))) %>% 
    t() %>% as.vector
  if(.reverse) return(1-(out*1))
  return(out)
}

base::strtoi(paste(as.character(get.mode(dat2)), sep="", collapse=""), base=2)
base::strtoi(paste(as.character(get.mode(dat2, TRUE)), sep="", collapse=""), base=2)
935*3160

## Part Two

reverse <- FALSE
dfilt <- dat2
for(i in 1:ncol(dfilt)){
  modeval <- get.mode(dfilt, reverse)[i]
  print(modeval)
  dfilt <- dfilt[dfilt[,i] == modeval,]
  print(dfilt)
}

base::strtoi(paste(as.character(dfilt %>% t() %>% as.vector()), sep="", collapse=""), base=2)

reverse <- TRUE
dfilt <- dat2
for(i in 1:9){
  modeval <- get.mode(dfilt, reverse)[i]
  print(modeval)
  dfilt <- dfilt[dfilt[,i] == modeval,]
  print(dfilt)
}

base::strtoi(paste(as.character(dfilt %>% t() %>% as.vector()), sep="", collapse=""), base=2)

573*2902
