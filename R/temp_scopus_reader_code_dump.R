# This will contains experimental code
# It is a scratch-pad. Do not run!!
# DO NOT RUN, unless you know what it does.
# create a .bib file from a scopus CSV.

library(tidyverse)
library(bibliometrix)
library(rlang)

filename <- here::here("data_exports_scopus","acv.csv")

df <- bibliometrix::convert2df(filename,dbsource = "scopus", format = "csv")
categories <- read_csv(here::here("categories.csv")) %>% pull(CATEGORY)

df <- df %>% add_column(selected = rep(NA, nrow(df)))
oldnames <- names(df)
for (cat in categories) {
  df <- df %>% add_column(cat = rep(NA, nrow(df)))
}

names(df) <- c(oldnames, categories)
df %>% as_tibble() 

# Save df for later use


# print individual articles into a bib file.

#currently only one is printed to console
index <- 3
library(RefManageR)
txt <- RefManageR::GetBibEntryWithDOI(df$DI[index]) %>% RefManageR::toBiblatex()
paste0(txt, collapse = "\n")

#TODO: bibfile


