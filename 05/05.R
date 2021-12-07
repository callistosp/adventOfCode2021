library(tidyverse)

string <- readLines(here::here("05","input.txt"))

## Part One
## parse the data
split0 <- str_split(string, " -> ", simplify=TRUE)
split1 <- str_split(split0[,1], ",", simplify=TRUE)
split2 <- str_split(split0[,2], ",", simplify=TRUE)

dat <- data.frame(x1=split1[,1], y1=split1[,2],
                  x2=split2[,1], y2=split2[,2]) %>% 
  mutate_all(~ as.numeric(as.character(.x))) %>% 
  mutate(
    xsame = ifelse(x1 == x2, 1, 0),
    ysame = ifelse(y1 == y2, 1, 0)
  )

out <- list()
for(i in 1:nrow(dat)){
  if(dat$xsame[i] == 1){
    for(j in seq(dat$y1[i], dat$y2[i])){
      out[[length(out)+1]] <- paste0(dat$x1[i], ",", j)
      # out[[length(out)+1]] <- c(x=dat$x1[i], y=j)
    }
  }
  
  if(dat$ysame[i] == 1){
    for(j in seq(dat$x1[i], dat$x2[i])){
      out[[length(out)+1]] <- paste0(j,",",dat$y1[i])
      # out[[length(out)+1]] <- c(x=j, y=dat$y1[i])
    }
  }
}

res <- do.call(rbind, out) %>% as.data.frame()
names(res) <- "value"

res %>% 
  count(value) %>% 
  filter(n>1) %>% 
  nrow()

## Part Two
out <- list()
for(i in 1:nrow(dat)){
  if(dat$xsame[i] == 1){
    ## vertical
    for(j in seq(dat$y1[i], dat$y2[i])){
      out[[length(out)+1]] <- paste0(dat$x1[i], ",", j)
    }
  } else if(dat$ysame[i] == 1){
    ## horizontal
    for(j in seq(dat$x1[i], dat$x2[i])){
      out[[length(out)+1]] <- paste0(j,",",dat$y1[i])
    }
  } else {
    ## diagonal
    xseq <- seq(dat$x1[i], dat$x2[i])
    yseq <- seq(dat$y1[i], dat$y2[i])
    for(j in seq_along(xseq)){
      out[[length(out)+1]] <- paste0(xseq[j],",",yseq[j])
    }
  }
}

res <- do.call(rbind, out) %>% as.data.frame()
names(res) <- "value"

res %>% 
  count(value) %>% 
  filter(n>1) %>% 
  nrow()
