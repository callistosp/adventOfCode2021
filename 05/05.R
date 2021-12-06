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

## change output data structure here
## do this instead
# tmp <- list(c(x=1,y=1), c(x=1,y=2))
# tmp %>% do.call(bind_rows, .)

# out <- list()
# for(i in 1:nrow(dat)){
#   if(dat$xsame[i] == 1){
#     out[[i]] <- list(x = dat$x1[i], y = seq(dat$y1[i], dat$y2[i]))
#   }else if(dat$ysame[i] == 1){
#     out[[i]] <- list(x = seq(dat$x1[i], dat$x2[i]), y = dat$y1[i])
#   }else{
#     out[[i]] <- NULL
#   }
# }
# out  
