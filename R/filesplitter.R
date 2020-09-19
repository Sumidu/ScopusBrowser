# Use this file to split a database among x users.

library(tidyverse)

# read file 
# This is the file you are working on.
df <- read_rds(here::here("ScopusBrowser-2020-06-12.rds"))

# How many readers should be there?
splits <- 5

total <- nrow(df)

# how many repetitions
reps <- ceiling(total / splits)

# generate 2 way worker split (4-eye principle)
w1 <- head(rep(1:splits, reps), nrow(df))
w2 <- head(
  tail(rep(1:splits, reps), nrow(df)+1), 
           nrow(df))

#setup data
df <- df %>% bind_cols(worker1 = w1, worker2 = w2)


# write subsets
for( i in 1:splits){
  df %>% filter(worker1 == i | worker1 == i) %>% select(-worker1, -worker2) %>% write_rds(paste0("worker",i,".rds"))
}


test1 <- read_rds(path = "worker1.rds")
test2 <- read_rds(path = "worker2.rds")
