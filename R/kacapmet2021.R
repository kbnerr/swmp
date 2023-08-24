# -----------------------------------------------------------------------
# Title: 
# Creator: Chris Guo
# Date: 
# Purpose: 

# Notes -------------------------------------------------------------------

## Used following code to convert .dat to .csv (although not necessary)
# old.names = list.files(path = file.path(dir.data, "kacapmet2021"), pattern = ".dat", full.names = TRUE)
# new.names = gsub(".dat$", ".csv", old.names)
# file.rename(old.names, new.names)

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

# Unused
check_MET_vars_dat = function (x) {
  for (i in 1:length(names(x))) {
      if (names(x[, i]) %>% str_detect("T$")) {
        x[, i] = x[, i] %>% mutate(across(.cols = last_col(), ~ format(as_datetime(hm(.)), "%H:%M")))
      }
    return(x)
  }
}


## Read in data from local path
tmp1 = list.files(path = file.path(dir.data, "kacapmet2021"), pattern = ".csv", full.names = TRUE) %>%
  # Skip first line with file summary info, declare all columns as.character type
  map(., ~ read_csv(., skip = 1, col_types = "c")) %>%
  # Remove lines 3-4 with unit info
  map(., ~ slice(., -(1:2))) %>%
  # Convert each data.frame to tibble type
  map(., tibble) %>%
  # Fix Max/Min times missing leading 0's
  map(., ~ mutate(., across(.cols = c(MaxTempT, MinTempT, MaxWSpdT), ~ format(as_datetime(hm(.)), "%H:%M"))))

# Bind all data together by rows
tmp2 = list_rbind(tmp1)

# Check for duplicated observations,
tmp2[which(duplicated(tmp2)), ] # which records
duplicated(tmp2) %>% sum() # how many duplicates are there\

# Remove duplicated observations and save to new object,
tmp3 = distinct(tmp2) %>%
  # Convert columns to appropriate object type,
  mutate(TIMESTAMP = TIMESTAMP %>% as_datetime(),
         RECORD = RECORD %>% as.integer(),
         across(ends_with("T", ignore.case = FALSE), as.character), # make sure to keep Max/Min times as.character
         across(where(is.character) & !ends_with("T", ignore.case = FALSE), as.numeric)) # all else to numeric


## RECORD data should be consistent regardless of GPS or incorrect time settings,
diff(tmp3$RECORD) %>% unique() # check for any differences other than 1

# ** CR1000x manual indicates a drop below 7V will affect data**
# 2021 field notes do not indicate any power removal,
# AvgVolt indicates no loss of power anywhere near 7V,
ggplot(data = tmp3) +
  geom_line(aes(x = RECORD, y = AvgVolt), color = "blue") +
  geom_hline(yintercept = 7, color = "red")

# We can continue assuming that RECORD has not been interrupted


## CDMO indicates timestamp issues occur after 08/29/2021 12:00, RECORD 21688, and continue into 2022,
# So we could use the last known good timestamp to substitute new timestamps at the correct 15min interval,

# Let's subset the dataset for the period containing timestamp issues,
# where we just include the last record without an issue as baseline,
tmp4 = filter(tmp3, RECORD >= 21688)

# Add a placeholder column for corrected timestamps,
tmp4 = mutate(tmp4, TIMESTAMP_new = NA) %>% relocate(TIMESTAMP_new, .after = TIMESTAMP)
# Record new timestamp entries at 15 min intervals beginning with our good timestamp,
for (i in 1:length(tmp4$TIMESTAMP)) {
  tmp4$TIMESTAMP_new[i] = (tmp4$TIMESTAMP[1] + ((i-1) * minutes(15)))
}
`# Convert new timestamps to datetime type,
tmp4 = mutate(tmp4, TIMESTAMP_new = as_datetime(TIMESTAMP_new))

# Check whether any of the new timestamps match the old ones,
which(tmp4$TIMESTAMP == tmp4$TIMESTAMP_new) %>% tmp4[.,]
# Looks like only the ~hour of records during site maintenance on 2023/09/22


## Now we can check how our Max/Min times look compared to what the actual time was,

# We'll need to convert Max/Min times to datetime so we can compare them to the timestamps,

tmp5$MaxTempT[1] %within% interval(tmp5$TIMESTAMP_new[1], tmp5$TIMESTAMP_new[1] - minutes(15))


