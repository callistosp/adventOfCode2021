library(tidyverse)

ran.num <- readLines(here::here("04","input.txt"))[1]
ran.num <- unlist(strsplit(ran.num, ","))

boards.raw <- data.table::fread(here::here("04","input.txt"), 
                                skip=1, na="", fill=TRUE)

boards <- boards.raw %>% 
  drop_na() %>% 
  mutate(bn = ceiling(row_number()/5)) %>% 
  mutate_all(as.character)
boards

## Part One

## marks numbers called on boards
mark.boards <- function(df, num){
  df %>% 
    mutate_at(vars(V1:V5), ~ str_replace(.x, glue::glue("^{num}$"), "X"))
}
# mark.boards(boards, "4")
# tmp <- boards
# nums <- c("95","46","31","90","13")
# for(i in seq_along(nums)){
#   tmp <- mark.boards(tmp, nums[i])
# }   
# tmp

## checks for bingo on boards
bingo <- function(df){
  ## check vertical
  vert <- df %>%
    group_by(bn) %>%
    mutate_at(vars(V1:V5), ~ ifelse(.x == "X", 1, 0)) %>%
    summarize_at(vars(V1:V5), sum) %>%
    filter(
      V1==5 | V2==5 | V3==5 | V4==5 | V5==5
    )

  if(nrow(vert) > 0) return(filter(df, bn == vert$bn))

  ## check horizontal
  hori <- df %>%
    filter(
      V1 == "X" & V2 == "X" & V3 == "X" & V4 == "X" & V5 == "X"
    )
  if(nrow(hori) > 0) return(filter(df, bn == hori$bn))

  return(NA)
}

# bingo(tmp)
# bingo(boards)

brd <- boards
for(i in seq_along(ran.num)){
  ## mark board
  brd <- mark.boards(brd, ran.num[i])
  ## check bingo
  bingo.call <- bingo(brd)
  ## if bingo, then quit
  if(!is.na(bingo.call)){break}
}
bingo.call
ran.num[i]

## calculate score
bingo.call %>% 
  mutate_all(as.numeric) %>% 
  select(-bn) %>% 
  summarize_all(sum)
sum(365,145,310,288)*as.numeric(ran.num[i])

## Part Two
# ####### THIS DOESN'T WORK
# bingo <- function(df){
#   ## create copy to return
#   df.out <- df
#   ## check vertical
#   vert <- df %>%
#     group_by(bn) %>%
#     mutate_at(vars(V1:V5), ~ ifelse(.x == "X", 1, 0)) %>%
#     summarize_at(vars(V1:V5), sum) %>%
#     filter(
#       V1==5 | V2==5 | V3==5 | V4==5 | V5==5
#     ) %>% distinct(bn) %>% pull(bn)
#   
#   ## check horizontal
#   hori <- df %>%
#     filter(
#       V1 == "X" & V2 == "X" & V3 == "X" & V4 == "X" & V5 == "X"
#     ) %>% distinct(bn) %>% pull(bn)
#   
#   return(df.out %>% mutate(winner=ifelse(
#     bn %in% vert | bn %in% hori, winner + 1, winner
#   )))
# }
# 
# 
# 
# brd <- boards %>% mutate(winner = 0)
# for(i in seq_along(ran.num)){
#   ## mark board
#   check.brd <- mark.boards(brd, ran.num[i])
#   ## check bingo
#   check.brd <- bingo(check.brd)
#   ## quit on last card
#   if(!any(check.brd$winner == 0)){ 
#     break
#   }else{
#     ## update board if no board wins
#     brd <- mark.boards(brd, ran.num[i])
#     brd <- bingo(check.brd)
#   }
# }
# brd
# ran.num[i]
