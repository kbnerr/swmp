# -----------------------------------------------------------------------
# Project: SWMP fix multiple EXO file headers
# Creator: Chris Guo
# Updated: 2022.11.22
# Purpose: Fix weird issue cause by faulty central wipers,
# where after downloading data from EXO sonde, we get multiple disjointed files
# each with their own headers when exported as .csv


# Notes -------------------------------------------------------------------

# If you run into parsing failures, then check the .csv for 'false' columns..
# Select and delete all empty cols.

# If read_csv() returns a df with a single oddly named column,
# open the .csv file then save the file (still as .csv),
# then try again...  ¯_(ツ)_/¯

# Define workflow paths ---------------------------------------------------

# setwd('C:/SWMP/')
wd = getwd()
wd.data = file.path(wd, "data")

file = file.path(wd.data, "example_weird_data")


# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)


# Code --------------------------------------------------------------------

data = read_csv(file = file.path(file, ".csv"),
                locale = locale(encoding = "latin1"),
                skip = 8, skip_empty_rows = TRUE,
                na = c("", "NA", "NaN"))

out = data %>%
  mutate(Date = `Date (MM/DD/YYYY)` %>% mdy(), # header rows now have 'NA' instead of 'Date (MM/DD/YYYY)'
         Time = `Time (HH:mm:ss)` %>% hms()) %>%
  drop_na(Date) %>% # since we have NAs elsewhere, we will only drop rows missing a date
  arrange(Date, Time) %>%
  select(-Date, -Time)

write.csv(x = out, file = file.path(paste(file, "_fixed.csv", sep = "")),
          row.names = FALSE)





