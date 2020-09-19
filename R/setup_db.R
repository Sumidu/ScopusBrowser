# This script generates a database for local use.

library(tidyverse)
library(bibliometrix)
library(rlang)

filename <- here::here("data_exports_scopus","demo.csv")

df <- bibliometrix::convert2df(filename, dbsource = "scopus", format = "csv")
categories <- read_csv(here::here("categories.csv")) %>% pull(CATEGORY)

df <- df %>% add_column(selected = rep(NA, nrow(df)))
oldnames <- names(df)

for (cat in categories) {
  df <- df %>% add_column(cat = rep(NA, nrow(df)))
}

names(df) <- c(oldnames, categories)


write_rds(as_tibble(head(df,10)), here::here("database.rds"))
cat("Done.\n")


