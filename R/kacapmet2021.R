# -----------------------------------------------------------------------
# Title: 
# Creator: Chris Guo
# Date: 
# Purpose: 

# Notes -------------------------------------------------------------------
old.names = list.files(path = file.path(dir.data, "kacapmet2021"), pattern = ".dat", full.names = TRUE)
new.names = gsub(".dat$", ".csv", old.names)
file.rename(old.names, new.names)

col.types = "T, i, n, n, t, n, t, n, n, n, n, n, n, t, n, n, n, n"

# Load packages -----------------------------------------------------------
library(SWMPr)
library(tidyverse)
library(lubridate)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.docs = file.path(wd, "docs")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd, "data")
dir.R = file.path(wd,"R")

# Utility -----------------------------------------------------------------


check_MET_vars_raw = function (x) {
  for (i in 1:length(names(x))) {
      if (names(x[, i]) %>% str_detect("T$")) {
        x[, i] = x[, i] %>% mutate(across(.cols = last_col(), ~ format(as_datetime(hm(.)), "%H:%M")))
      }
    return(x)
  }
}

# Read in data ------------------------------------------------------------

tmp = list.files(path = file.path(dir.data, "kacapmet2021"), pattern = ".csv", full.names = TRUE) %>%
  map(., ~ read_csv(., skip = 1, col_types = "c")) %>%
  map(., ~ slice(., -(1:2))) %>%
  list_rbind() %>%
  mutate(across(.cols = c(MaxTempT, MinTempT, MaxWSpdT), ~ format(as_datetime(hm(.)), "%H:%M")))

which(duplicated(tmp)) %>% length

tmp2 = tmp %>% distinct()

tmp3 = tmp2 %>%
  mutate(TIMESTAMP = TIMESTAMP %>% as_datetime(),
         RECORD = RECORD %>% as.integer(),
         across(ends_with("T", ignore.case = FALSE), lubridate::hm),
         across(where(is.character), as.numeric))


