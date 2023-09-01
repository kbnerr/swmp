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
library(magrittr)

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

# Check for duplicated observations- these are likely due to submitting aggregated raw data during primary QAQC,
tmp2[which(duplicated(tmp2)), ] # which records
duplicated(tmp2) %>% sum() # how many duplicates are there

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

# It appears that RECORD has not been interrupted,
tmp3 %>% 
  group_by(RECORD) %>%
  filter(n() > 1)

## HOWEVER, we may have repeat observations..
# These look like separate records but share the same timestamp,
# CDMO found this issue in 2022 data, but may not easily show up in 2021 because of odd timestamps,

## CDMO indicates timestamp issues occur after 08/29/2021 12:00, RECORD 21688, and continue into 2022,
# So we could use the last known good timestamp to substitute new timestamps at a corrected 15min interval,

# Let's subset the dataset for the period containing timestamp issues, keeping last known good timestamp,
tmp4 = filter(tmp3, RECORD >= 21688)

# Add a placeholder column for corrected timestamps,
tmp4 = mutate(tmp4, TIMESTAMP_fix = NA) %>% relocate(TIMESTAMP_fix, .after = TIMESTAMP)

# Record new timestamps w/ 15 min interval beginning with last good timestamp,
for (i in 1:length(tmp4$TIMESTAMP)) {
  tmp4$TIMESTAMP_fix[i] = (tmp4$TIMESTAMP[1] + ((i-1) * minutes(15)))
}

# Convert new timestamps to datetime,
tmp4 = mutate(tmp4, TIMESTAMP_fix = as_datetime(TIMESTAMP_fix, tz = "UTC"))

# Check whether any of the new timestamps match the old ones,
which(tmp4$TIMESTAMP %within% interval(lag(tmp4$TIMESTAMP_fix, n = 4), lead(tmp4$TIMESTAMP_fix, n = 4))) %>% tmp4[.,]
# Looks like the ~hour of records during site maintenance on 2023-09-22,
# and the site maintenance on 2023-10-15 looks within 15 min

## According to CDMO, we have max/min times outside of the appropriate 15 min window,
# however, the max/min times look somewhat close, e.g., max temp at 12:58 is not within 13:00 - 13:15 but it's close,
# This should be enough to check whether the fixed timestamps are at fault or whether it's the max/min times, 

# Let's first determine logically whether each max/min time is within its correct 15 min interval,
tmp4 %>%
  # define the 15 min interval
  mutate(interval = interval(TIMESTAMP_fix - minutes(15), TIMESTAMP_fix),
         # New col for each max/min as datetime,
         MaxTempT_check = if_else(hm(MaxTempT) > hours(23) + minutes(45) & date(TIMESTAMP_fix) > date(lag(TIMESTAMP_fix, default = .$TIMESTAMP_fix[1], n = 2)),
                                  # Logic statement to account for close-but-incorrect times around date changes,
                                  # e.g., a max/min time of 23:59 is close to the interval 0:00-0:15 for the next day,
                                  # but these instances should be treated differently than if a max/min of 23:59 occurs within 23:45-0:00 of the same date,
                                  date(lag(TIMESTAMP_fix, default = .$TIMESTAMP_fix[1], n = 2)) + hm(MaxTempT),
                                  date(lag(TIMESTAMP_fix, default = .$TIMESTAMP_fix[1])) + hm(MaxTempT)),
         MinTempT_check = if_else(hm(MinTempT) > hours(23) + minutes(45) & date(TIMESTAMP_fix) > date(lag(TIMESTAMP_fix, default = .$TIMESTAMP_fix[1], n = 2)),
                                  date(lag(TIMESTAMP_fix, default = .$TIMESTAMP_fix[1], n = 2)) + hm(MinTempT),
                                  date(lag(TIMESTAMP_fix, default = .$TIMESTAMP_fix[1])) + hm(MinTempT)),
         MaxWSpdT_check = if_else(hm(MaxWSpdT) > hours(23) + minutes(45) & date(TIMESTAMP_fix) > date(lag(TIMESTAMP_fix, default = .$TIMESTAMP_fix[1], n = 2)),
                                  date(lag(TIMESTAMP_fix, default = .$TIMESTAMP_fix[1], n = 2)) + hm(MaxWSpdT),
                                  date(lag(TIMESTAMP_fix, default = .$TIMESTAMP_fix[1])) + hm(MaxWSpdT)),
         # TRUE if the timestamp does not fall within the interval, i.e., it is bad,
         MaxTempT_bad = !MaxTempT_check %within% interval,
         MinTempT_bad = !MinTempT_check %within% interval,
         MaxWSpdT_bad = !MaxWSpdT_check %within% interval) %>%
  # Relocate cols for convenience,
  relocate(interval, .after = TIMESTAMP_fix) %>%
  relocate(MaxTempT_check, .after = MaxTempT) %>%
  relocate(MinTempT_check, .after = MinTempT) %>% 
  relocate(MaxWSpdT_check, .after = MaxWSpdT) %T>%
  View() %>%
  # Plot TRUE or FALSE, solid bars indicate all three max/min times do not fall within interval,
  {ggplot(data = .) + geom_col(aes(x = TIMESTAMP_fix, y = MaxTempT_bad + MinTempT_bad + MaxWSpdT_bad))}

# We can see that max/min times appear to be randomly off to for the first half of September,
# but then in mid September begins a data block where almost times are bad,
# There is probably something wrong with the timestamps since no 'good' times are found even randomly,
# Looking at the tibble, RECORD 23993 begins an offset by 15 min in subsequent timestamps,
# This aligns with station maintenance on 2021-09-22,

tmp4 %>%
  # Correct the timestamps after RECORD 23993; subsequent code is essentially same as before,
  mutate(TIMESTAMP_fix_23993 = case_when(RECORD <= 23993 ~ TIMESTAMP_fix,
                                       RECORD > 23993 ~ lag(TIMESTAMP_fix))) %>%
  relocate(TIMESTAMP_fix_23993, .after = TIMESTAMP_fix) %>%
  mutate(interval = interval(TIMESTAMP_fix_23993 - minutes(15), TIMESTAMP_fix_23993),
         MaxTempT_check = if_else(hm(MaxTempT) > hours(23) + minutes(45) & date(TIMESTAMP_fix_23993) > date(lag(TIMESTAMP_fix_23993, default = .$TIMESTAMP_fix_23993[1], n = 2)),
                                  date(lag(TIMESTAMP_fix_23993, default = .$TIMESTAMP_fix_23993[1], n = 2)) + hm(MaxTempT),
                                  date(lag(TIMESTAMP_fix_23993, default = .$TIMESTAMP_fix_23993[1])) + hm(MaxTempT)),
         MinTempT_check = if_else(hm(MinTempT) > hours(23) + minutes(45) & date(TIMESTAMP_fix_23993) > date(lag(TIMESTAMP_fix_23993, default = .$TIMESTAMP_fix_23993[1], n = 2)),
                                  date(lag(TIMESTAMP_fix_23993, default = .$TIMESTAMP_fix_23993[1], n = 2)) + hm(MinTempT),
                                  date(lag(TIMESTAMP_fix_23993, default = .$TIMESTAMP_fix_23993[1])) + hm(MinTempT)),
         MaxWSpdT_check = if_else(hm(MaxWSpdT) > hours(23) + minutes(45) & date(TIMESTAMP_fix_23993) > date(lag(TIMESTAMP_fix_23993, default = .$TIMESTAMP_fix_23993[1], n = 2)),
                                  date(lag(TIMESTAMP_fix_23993, default = .$TIMESTAMP_fix_23993[1], n = 2)) + hm(MaxWSpdT),
                                  date(lag(TIMESTAMP_fix_23993, default = .$TIMESTAMP_fix_23993[1])) + hm(MaxWSpdT)),
         MaxTempT_bad = !MaxTempT_check %within% interval,
         MinTempT_bad = !MinTempT_check %within% interval,
         MaxWSpdT_bad = !MaxWSpdT_check %within% interval) %>%
  relocate(interval, .after = TIMESTAMP_fix_23993) %>%
  relocate(MaxTempT_check, .after = MaxTempT) %>%
  relocate(MinTempT_check, .after = MinTempT) %>% 
  relocate(MaxWSpdT_check, .after = MaxWSpdT) %T>%
  View() %>% 
  {ggplot(data = .) + geom_col(aes(x = TIMESTAMP_fix_23993, y = MaxTempT_bad + MinTempT_bad + MaxWSpdT_bad))}

# RECORD 26202 - same issue as before, here begins 15 min offset aligns with site maintenance on 2021-10-15,
  
tmp4 %>%
  mutate(TIMESTAMP_fix_23993 = case_when(RECORD <= 23993 ~ TIMESTAMP_fix,
                                        RECORD > 23993 ~ lag(TIMESTAMP_fix)),
         # Correct the timestamps after RECORD 26202; surrounding code is essentially same as before,
         TIMESTAMP_fix_23993_26202 = case_when(RECORD <= 26202 ~ TIMESTAMP_fix_23993,
                                                   RECORD > 26202 ~ lag(TIMESTAMP_fix_23993))) %>%
  relocate(TIMESTAMP_fix_23993, TIMESTAMP_fix_23993_26202, .after = TIMESTAMP_fix) %>%
  mutate(interval = interval(TIMESTAMP_fix_23993_26202 - minutes(15), TIMESTAMP_fix_23993_26202),
         MaxTempT_check = if_else(hm(MaxTempT) > hours(23) + minutes(45) & date(TIMESTAMP_fix_23993_26202) > date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)) + hm(MaxTempT),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1])) + hm(MaxTempT)),
         MinTempT_check = if_else(hm(MinTempT) > hours(23) + minutes(45) & date(TIMESTAMP_fix_23993_26202) > date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)) + hm(MinTempT),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1])) + hm(MinTempT)),
         MaxWSpdT_check = if_else(hm(MaxWSpdT) > hours(23) + minutes(45) & date(TIMESTAMP_fix_23993_26202) > date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)) + hm(MaxWSpdT),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1])) + hm(MaxWSpdT)),
         MaxTempT_bad = !MaxTempT_check %within% interval,
         MinTempT_bad = !MinTempT_check %within% interval,
         MaxWSpdT_bad = !MaxWSpdT_check %within% interval) %>%
  relocate(interval, .after = TIMESTAMP_fix_23993_26202) %>%
  relocate(MaxTempT_check, .after = MaxTempT) %>%
  relocate(MinTempT_check, .after = MinTempT) %>%
  relocate(MaxWSpdT_check, .after = MaxWSpdT) %T>%
  View() %>% 
  {ggplot(data = .) + geom_col(aes(x = TIMESTAMP_fix_23993_26202, y = MaxTempT_bad + MinTempT_bad + MaxWSpdT_bad))}

# Similar pattern of an offset during the next maintenance on 2021-11-08 and 2021-12-09.
# However, this time it's the max/min times instead of timestamps, and not all subsequent records were affected,
# Max/min times have a -1 hr offset, interestingly with different start/end RECORDs depending on the parameter:
  # MaxTempT 28515 - 28606, 31478 - 31486
  # MinTempT 28514 - 28607, 31478 - 31487
  # MaxWSpT 28514 - 28606, 31479 - 31486

tmp4 %>%
  mutate(TIMESTAMP_fix_23993 = case_when(RECORD <= 23993 ~ TIMESTAMP_fix,
                                         RECORD > 23993 ~ lag(TIMESTAMP_fix)),
         TIMESTAMP_fix_23993_26202 = case_when(RECORD <= 26202 ~ TIMESTAMP_fix_23993,
                                               RECORD > 26202 ~ lag(TIMESTAMP_fix_23993))) %>%
  relocate(TIMESTAMP_fix_23993, TIMESTAMP_fix_23993_26202, .after = TIMESTAMP_fix) %>%
  # Correct the max/min times where -1hr offset occurs,
  mutate(MaxTempT_fix = case_when(RECORD %in% c(28515:28606, 31478:31486) ~ date(TIMESTAMP_fix_23993_26202) + hm(MaxTempT) + hours(1),
                                  !RECORD %in% c(28515:28606, 31478:31486) ~ date(TIMESTAMP_fix_23993_26202) + hm(MaxTempT)) %>% format(., "%H:%M"),
         MinTempT_fix = case_when(RECORD %in% c(28514:28607, 31478:31487) ~ date(TIMESTAMP_fix_23993_26202) + hm(MinTempT) + hours(1),
                                  !RECORD %in% c(28514:28607, 31478:31487) ~ date(TIMESTAMP_fix_23993_26202) + hm(MinTempT)) %>% format(., "%H:%M"),
         MaxWSpdT_fix = case_when(RECORD %in% c(28514:28606, 31479:31486) ~ date(TIMESTAMP_fix_23993_26202) + hm(MaxWSpdT) + hours(1),
                                  !RECORD %in% c(28514:28606, 31479:31486) ~ date(TIMESTAMP_fix_23993_26202) + hm(MaxWSpdT)) %>% format(., "%H:%M")) %>%
  # Relocate fixed timestamps for convenience; surrounding code is essentially the same as previous,
  relocate(MaxTempT_fix, .after = MaxTempT) %>%
  relocate(MinTempT_fix, .after = MinTempT) %>%
  relocate(MaxWSpdT_fix, .after = MaxWSpdT) %>%
  mutate(interval = interval(TIMESTAMP_fix_23993_26202 - minutes(15), TIMESTAMP_fix_23993_26202),
         MaxTempT_check = if_else(hm(MaxTempT_fix) > hours(23) + minutes(45) & date(TIMESTAMP_fix_23993_26202) > date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)) + hm(MaxTempT_fix),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1])) + hm(MaxTempT_fix)),
         MinTempT_check = if_else(hm(MinTempT_fix) > hours(23) + minutes(45) & date(TIMESTAMP_fix_23993_26202) > date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)) + hm(MinTempT_fix),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1])) + hm(MinTempT_fix)),
         MaxWSpdT_check = if_else(hm(MaxWSpdT_fix) > hours(23) + minutes(45) & date(TIMESTAMP_fix_23993_26202) > date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)) + hm(MaxWSpdT_fix),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1])) + hm(MaxWSpdT_fix)),
         MaxTempT_bad = !MaxTempT_check %within% interval,
         MinTempT_bad = !MinTempT_check %within% interval,
         MaxWSpdT_bad = !MaxWSpdT_check %within% interval) %>%
  relocate(interval, .after = TIMESTAMP_fix_23993_26202) %>%
  relocate(MaxTempT_check, .after = MaxTempT_fix) %>%
  relocate(MinTempT_check, .after = MinTempT_fix) %>%
  relocate(MaxWSpdT_check, .after = MaxWSpdT_fix) %T>%
  View() %>% 
  {ggplot(data = .) + geom_col(aes(x = TIMESTAMP_fix_23993_26202, y = MaxTempT_bad + MinTempT_bad + MaxWSpdT_bad))}

# Good. At least our faulty mx/min times now appear random.
# Interesting that the corrected max/min times in Nov/Dec appear to be completely correct,
  
# I think major revisions to timetamps are done, so we can save the corrections as a new tibble,
tmp5 = tmp4 %>%
  mutate(TIMESTAMP_fix_23993 = case_when(RECORD <= 23993 ~ TIMESTAMP_fix,
                                         RECORD > 23993 ~ lag(TIMESTAMP_fix)),
         TIMESTAMP_fix_23993_26202 = case_when(RECORD <= 26202 ~ TIMESTAMP_fix_23993,
                                               RECORD > 26202 ~ lag(TIMESTAMP_fix_23993))) %>%
  relocate(TIMESTAMP_fix_23993, TIMESTAMP_fix_23993_26202, .after = TIMESTAMP_fix) %>%
  mutate(MaxTempT_fix = case_when(RECORD %in% c(28515:28606, 31478:31486) ~ date(TIMESTAMP_fix_23993_26202) + hm(MaxTempT) + hours(1),
                                  !RECORD %in% c(28515:28606, 31478:31486) ~ date(TIMESTAMP_fix_23993_26202) + hm(MaxTempT)) %>% format(., "%H:%M"),
         MinTempT_fix = case_when(RECORD %in% c(28514:28607, 31478:31487) ~ date(TIMESTAMP_fix_23993_26202) + hm(MinTempT) + hours(1),
                                  !RECORD %in% c(28514:28607, 31478:31487) ~ date(TIMESTAMP_fix_23993_26202) + hm(MinTempT)) %>% format(., "%H:%M"),
         MaxWSpdT_fix = case_when(RECORD %in% c(28514:28606, 31479:31486) ~ date(TIMESTAMP_fix_23993_26202) + hm(MaxWSpdT) + hours(1),
                                  !RECORD %in% c(28514:28606, 31479:31486) ~ date(TIMESTAMP_fix_23993_26202) + hm(MaxWSpdT)) %>% format(., "%H:%M")) %>%
  relocate(MaxTempT_fix, .after = MaxTempT) %>%
  relocate(MinTempT_fix, .after = MinTempT) %>%
  relocate(MaxWSpdT_fix, .after = MaxWSpdT) %>%
  mutate(interval = interval(TIMESTAMP_fix_23993_26202 - minutes(15), TIMESTAMP_fix_23993_26202),
         MaxTempT_check = if_else(hm(MaxTempT_fix) > hours(23) + minutes(45) & date(TIMESTAMP_fix_23993_26202) > date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)) + hm(MaxTempT_fix),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1])) + hm(MaxTempT_fix)),
         MinTempT_check = if_else(hm(MinTempT_fix) > hours(23) + minutes(45) & date(TIMESTAMP_fix_23993_26202) > date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)) + hm(MinTempT_fix),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1])) + hm(MinTempT_fix)),
         MaxWSpdT_check = if_else(hm(MaxWSpdT_fix) > hours(23) + minutes(45) & date(TIMESTAMP_fix_23993_26202) > date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1], n = 2)) + hm(MaxWSpdT_fix),
                                  date(lag(TIMESTAMP_fix_23993_26202, default = .$TIMESTAMP_fix_23993_26202[1])) + hm(MaxWSpdT_fix)),
         MaxTempT_bad = !MaxTempT_check %within% interval,
         MinTempT_bad = !MinTempT_check %within% interval,
         MaxWSpdT_bad = !MaxWSpdT_check %within% interval) %>%
  relocate(interval, .after = TIMESTAMP_fix_23993_26202) %>%
  relocate(MaxTempT_check, .after = MaxTempT_fix) %>%
  relocate(MinTempT_check, .after = MinTempT_fix) %>%
  relocate(MaxWSpdT_check, .after = MaxWSpdT_fix)
  
# Let's see if those max/min times are actually all correct,
tmp5 %>%
  filter(RECORD %in% c(28514:28607, 31478:31487)) %T>%
  View() %>% 
  {ggplot(data = .) + geom_col(aes(x = TIMESTAMP_fix_23993_26202, y = MaxTempT_bad + MinTempT_bad + MaxWSpdT_bad))}
# Yep- these data look to be ok.

# In general, max/min timestamps don't seem to be off by more than a couple minutes,
# Let's test a new interval of 17 minutes to see if what happens to our bad timestamps,

tmp5 %>%
  mutate(interval = interval(TIMESTAMP_fix_23993_26202 - minutes(17), TIMESTAMP_fix_23993_26202),
         # will need to also re-evaluate the time checks since they depend on the interval,
         MaxTempT_bad = !MaxTempT_check %within% interval,
         MinTempT_bad = !MinTempT_check %within% interval,
         MaxWSpdT_bad = !MaxWSpdT_check %within% interval) %T>%
  View() %>% 
  {ggplot(data = .) + geom_col(aes(x = TIMESTAMP_fix_23993_26202, y = MaxTempT_bad + MinTempT_bad + MaxWSpdT_bad))}
# No errors!

# So what if we were to add 2 minutes to all max/min times instead of altering the interval?







