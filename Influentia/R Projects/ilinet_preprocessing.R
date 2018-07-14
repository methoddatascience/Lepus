# Libraries ----
library(tidyverse)
library(magrittr)

# setwd('Documents/dev/Lepus/Influentia/R Projects/')

# Data Loading ----
# Working with ilinet only as the table broke by age group doesn't have State info
ilinet_tbl  <- read_csv('input/ILINet.csv') %>% 
  arrange(REGION, YEAR, WEEK)

# Preprocessing ----
# Check number of unique values per feature
map(ilinet_tbl, ~length(unique(.x)))

# Drop irrelevant features
ilinet_tbl <- ilinet_tbl[, map_lgl(ilinet_tbl, ~length(unique(.x)) > 2)]

# Investigate why WEEK has 53 distinct values (a year has 52 weeks)
table(ilinet_tbl$WEEK)

# TODO verify what is the week 53 (maybe leap year?), dropping for now
ilinet_tbl %<>%
  filter(WEEK != 53)

# Coerce numerical features to numeric (this will introduce NAs where value == 'X')
ilinet_tbl[, 4:7] <- map(ilinet_tbl[, 4:7], as.numeric)

# Check number of NAs per feature
ilinet_tbl %>% 
  group_by(REGION) %>% 
  summarise_all(function(x){sum(is.na(x))}) %>% 
  View

# NAs are mostly divided between Florida and Virgin Islands, let's
# investigate further
ilinet_tbl %>% 
  filter(REGION %in% c('Florida', 'Virgin Islands')) %>% 
  group_by(REGION) %>% 
  summarise_all(function(x){sum(is.na(x))/length(x)}) %>% 
  View

# 100% of Florida info is NA, 60% of Virgin Islands info is NA
# TODO decide what to do with these states later, dropping for now
ilinet_tbl %<>% 
  filter(!(REGION %in% c('Florida', 'Virgin Islands')))

# Combine YEAR and WEEK info
ilinet_tbl %<>%
  mutate(
    YEARWEEK   = paste(YEAR, WEEK, sep = '-'), 
    PRETTYDATE = as.Date(paste(YEAR, WEEK, 1, sep="-"), "%Y-%U-%u")
  )

# Train/Test split ----
# Use last 3 weeks as Test Set (as data is released w/ 2 weeks lag + 1 week protection
# against data release delay)
train <- ilinet_tbl %>% 
  filter(PRETTYDATE < max(PRETTYDATE) - lubridate::weeks(3))

test <- ilinet_tbl %>% 
  filter(PRETTYDATE >= max(PRETTYDATE) - lubridate::weeks(3))

# saveRDS(train, 'data/train.rds')
# saveRDS(test, 'data/test.rds')
# write_csv(train, 'data/train.csv')
# write_csv(test, 'data/test.csv')