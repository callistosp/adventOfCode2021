library(tidyverse)

dat <- data.table::fread(here::here("02","input.txt"))

## Part One
position <- list(horizontal=0, depth=0)

for(i in 1:nrow(dat)){
  if(dat$V1[i] == "forward") position$horizontal <- position$horizontal + dat$V2[i]
  if(dat$V1[i] == "up") position$depth <- position$depth - dat$V2[i]
  if(dat$V1[i] == "down") position$depth <- position$depth + dat$V2[i]
}

position$horizontal * position$depth

## Part Two
position <- list(horizontal=0, depth=0, aim=0)

for(i in 1:nrow(dat)){
  if(dat$V1[i] == "forward") {
    position$horizontal <- position$horizontal + dat$V2[i]
    position$depth <- position$depth + position$aim * dat$V2[i]
  }
  if(dat$V1[i] == "up") position$aim <- position$aim - dat$V2[i]
  if(dat$V1[i] == "down") position$aim <- position$aim + dat$V2[i]
}

position$horizontal * position$depth
