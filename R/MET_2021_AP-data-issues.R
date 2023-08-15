# -----------------------------------------------------------------------
# Title: SWMP MET data clean up - 2021 Anchor POint
# Creator: Chris Guo
# Date: 28 February 2023
# Purpose: Figure out what happened to time signatures and observations from Anchor Point 2021

# Notes -------------------------------------------------------------------

## Claus O. Wilke's blog was super helpful in reading mult data files.
# https://clauswilke.com/blog/2016/06/13/reading-and-combining-many-tidy-data-files-in-r

## Code for anomalies plots were drafted by J. Schloemer (2022)

# This will take POSIOX.ct objects and return character in SWMP format 
mutate(across(.cols = last_col(), ~ format(.x, "%m/%d/%Y %H:%M")))

# Load packages -----------------------------------------------------------
library(tidyverse)
library(googledrive)
library(lubridate)
library(readxl)
library(magrittr)


# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.docs = file.path(wd, "docs")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd, "data")
dir.R = file.path(wd,"R")

# Utility -----------------------------------------------------------------

# Load local script for utiloity function
source(file = "/Users/chguo/Desktop/Rlocal/scripts/utility.R")

# Function to download google drive folder contents to a temporary directory,
if(!exists('drive_download_to_tempdir', mode = 'function')) {
  drive_download_to_tempdir = function(x, dir, overwrite = TRUE, ...) {
    purrr::pmap(x, function(name, id, drive_resource, ...) {
      googledrive::drive_download(as_id(id), file.path(dir, name), overwrite = overwrite, ...)
    },
    ...)
  }
}

# Function to check column types in MET secondary QC .xlsx files,
if(!exists('check_MET_vars', mode = 'function')) {
  check_MET_vars = function(x) {
    # Check to make sure Data and Time vars are the correct type.
    if(select(x, DateTimeStamp, MinTempT, MaxTempT, MaxWSpdT) %>% sapply(is.POSIXct) %>% sum() != 4) {
      stop("Error in Date/Time check. This function expects as.POSIXct for all of DateTimeStamp, MinTempT, MaxTempT, and MaxWSpdT.")
    }
    # For loop to change variable type one column at a time,
    for ( i in 1:length(names(x)) ) {
      if (names(x[, i]) %>% str_detect("Station")) {
        x[, i] = x[, i] %>% mutate(across(.cols = last_col(), as.factor))
      } else
        if (names(x[, i]) %>% str_detect("Date")) {
          x[, i] = x[, i] %>% mutate(across(.cols = last_col(), as.POSIXct))
        } else
          if (names(x[, i]) %>% str_detect("T$")) {
            x[, i] = x[, i] %>% mutate(across(last_col(), as.POSIXct)) %>% mutate(across(.cols = last_col(), ~ format(.x, "%H:%M")))
          } else
            if (names(x[, i]) %>% str_detect("^F_")) {
              x[, i] = x[, i] %>% mutate(across(.cols = last_col(), as.character))
            } else {
              x[, i] = x[, i] %>% mutate(across(.cols = last_col(), as.numeric))
            }
    }
    return(x)
  }
}

# Function to check column types in in MET raw .dat files
if(!exists('check_MET_vars_raw', mode = 'function')) {
  check_MET_vars_raw = function (x) {
    for ( i in 1:length(names(x)) ) {
      if (names(x[, i]) %>% str_detect("TIMESTAMP|DateTimeStamp")) {
        x[, i] = x[, i] %>%
          mutate(across(.cols = last_col(), ~ as.POSIXct(.x,
                                                         tryFormats = c("%m/%d/%Y %H:%M", "%m/%d/%y %H:%M"),
                                                         tz = "UTC")))
      } else
        if (names(x[, i]) %>% str_detect("RECORD")) {
          x[, i] = x[, i] %>%
            mutate(across(.cols = last_col(), as.numeric))
        } else
          if (names(x[, i]) %>% str_detect("T$")) {
            x[, i] = x[, i] %>%
              mutate(across(.cols = last_col(), lubridate::hm)) %>%
              mutate(across(.cols = last_col(), ~ format(.x %>% as.POSIXct(origin = origin), "%H:%M")))
          } else
            if (names(x[, i]) %>% str_detect("^F_")) {
              x[, i] = x[, i] %>%
                mutate(across(.cols = last_col(), as.character))
            } else {
              x[, i] = x[, i] %>% mutate(across(.cols = last_col(), as.numeric))
            }
    }
    return(x)
  }
}


# Read in data ------------------------------------------------------------

## We keep provisional data in UAA KBNERR SWMP google drive:

# URL for the folder we want,
drive_folder = "https://drive.google.com/drive/folders/1-lonx-K7ArouOi-E0yJkOvLEzOQgSfQ0" # MET/Data_QAQC/2021
ls_dribble = drive_ls(drive_folder) %>% filter(str_detect(name, ".dat"))

# Download files (won't run if there are already multiple files in the temporary directory),
if(length(list.files(path = tempdir())) <= 1) {
  drive_download_to_tempdir(ls_dribble, dir = tempdir())
} else {"There is more than one file in tempdir().\nUse list.files() to see what's in there.\nDo not run this function if you do not need to." %>% cat()
}

# Create a dataset from primary QAQC data (_QC.csv or _QC.dat from CDMO),
tmp = list.files(path = tempdir(), pattern = ".dat", full.names = TRUE) %>%
  # Grab only .dat files that went thru primary QC
  str_subset("(?<=QC).dat") %>%
  ## NOTE: missing last month of raw data! kacapmet120921.dat ##
  map(read_csv)

# Weird that August data is in different format: tab separated, and 'DateTimeStamp',
# usually this is the case after secondary QAQC..
tmp[[8]] = list.files(path = tempdir(), pattern = ".dat", full.names = TRUE) %>%
  str_subset("082321_QC") %>%
  read_tsv() %>%
  # We'll also need to specify the correct century for year,
  mutate(DateTimeStamp = as.POSIXct(DateTimeStamp, tryFormats = "%m/%d/%y %H:%M", tz = "UTC") %>%
           format("%m/%d/20%y %H:%M"))

# Run check on the column types and save results
tmp %<>% lapply(check_MET_vars_raw)

# DF's 9, 10, and 11 have the exact same head(10) rows...
anti_join(tmp[[9]], tmp[[11]])
# 9 and 11 are the same
slice_head(tmp[[9]], n = nrow(tmp[[10]])) %>% anti_join(., tmp[[10]])
# Whatever is in 10 is the same as 9 (and 11)

  
# Looks like our isues begin in 8 and continue into 9-11.
# Let's investigate these two...

x = tmp[[8]]
y = tmp[[9]] 

x$RECORD %>% range()
y$RECORD %>% range()
# So the records seem seqiuential from end of 8 into 9...

x$DateTimeStamp %>% range()
y$TIMESTAMP %>% range() # Here's where the time travel begins.
# What if we just look at data in 9 from this century,
filter(y, year(TIMESTAMP) > 2000)$TIMESTAMP %>% range
# We have datetimes in 8 that should not be there...

# So do we have 
x %>%
  group_by(DateTimeStamp) %>%
  filter(n() > 1)



