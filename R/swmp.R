# -----------------------------------------------------------------------
# Title: 
# Creator: Chris Guo
# Date: 
# Purpose: 

# Notes -------------------------------------------------------------------


# Load packages -----------------------------------------------------------
library(SWMPr)
library(tidyverse)
library(readxl)
library(lubridate)
library(dygraphs)
library(xts)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.docs = file.path(wd, "docs")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd, "data")
dir.R = file.path(wd,"R")

# Utility -----------------------------------------------------------------

source(file.path(dir.R, "utility.R"))

# Wrapper function for import_local to combine multiple stations
import_wq = function(station_code) {
  if(!exists('wq.hist', mode = "list")) {
    x = station_code
    tmp = list()
    for (i in 1:length(x)) {
      tmp[[i]] =  import_local(path = file.path(dir.data, '2001-2022.zip'), station_code = x[i], keep_qaqcstatus = TRUE) %>%
        mutate(`station code` = x[i]) %>%
        relocate(`station code`, .before = 1)
    }
    return(tmp)
  }
  stop("List wq.hist already exists. Remove wq.hist in Environment if you want to import data.")
}

# Function to check column types in .xlsx files,
check_WQ_xlsx = function(x) {
  # Remove RowID and make all column names lowercase,
  x %<>%
    select(-RowID) %>%
    rename_with( ~ str_to_lower(.), .cols = everything()) %>%
    mutate(historical = as.numeric(0),
           provisionalplus = as.numeric(1)) %>%
    relocate(c(historical, provisionalplus), .after = datetimestamp)
  # For loop to change variable type one column at a time,
  for (i in 1:length(names(x))) {
    # Make sure datetimestamps are in same format
    if (names(x[, i]) %>% str_detect("datetimestamp")) {
      x[, i] = x[, i] %>% mutate(across(.cols = last_col(), as.POSIXct))
    } else
      # Keep flag columns and station/site IDs as.character
      if (names(x[, i]) %>% str_detect("(^f_)|station|site")) {
        x[, i] = x[, i] %>% mutate(across(.cols = last_col(), as.character))
      } else {
        # All other data should be numeric
        x[, i] = x[, i] %>% mutate(across(.cols = last_col(), as.numeric))
      }
  }
  return(x)
}

# Read in data ------------------------------------------------------------

# Station info for all KAC data
kac = site_codes_ind('kac') %>% tibble()
# Concatenate stations by program
sites.wq = kac$station_code %>% unique() %>% str_subset("wq")
sites.nut = kac$station_code %>% unique() %>% str_subset("nut")
sites.met = kac$station_code %>% unique() %>% str_subset("met")

# Combine all WQ stations into tbl wq.hist- (this could take a few minutes),
wq.hist = import_wq(sites.wq) %>%
  reduce(bind_rows) %>%
  # Add col for site name to match .xlsx data,
  mutate(sitename = case_when(str_detect(`station code`, "bc") ~ "Bear Cove",
                              str_detect(`station code`, "pg") ~ "Port Graham",
                              str_detect(`station code`, "hs|h3") ~ "Homer Surface",
                              str_detect(`station code`, "ho|dl|hd") ~ "Homer Deep",
                              str_detect(`station code`, "ss") ~ "Seldovia Surface",
                              str_detect(`station code`, "se|sd") ~ "Seldovia Deep"))

# 2023 WQ data
wq.2023 = list.files(path = file.path(dirname(getwd()), "Library/CloudStorage/GoogleDrive-chguo@alaska.edu/Shared drives/UAA Kachemak Bay NERR/Monitoring/SWMP/WQ/Data_QAQC/2023"),
                     pattern = "_QC.xlsx$", full.names = TRUE) %>%
  map(~ read_xlsx(path = ., sheet = "Data")) %>%
  map(~ check_WQ_xlsx(x = .)) %>%
  reduce(bind_rows)

wq.all = bind_rows(wq.2023, wq.hist)

sd.xts = wq.all %>%
  filter(sitename == "Seldovia Deep") %>%
  xts(order.by = .$datetimestamp)

sd.xts %>% 
  select()



