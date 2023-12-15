# -----------------------------------------------------------------------
# Title: Submarine Groundwater Discharge
# Creator: dsiegert/Chris Guo
# Date: 12/2023
# Purpose: 

# Notes -------------------------------------------------------------------


# Load packages -----------------------------------------------------------
library(SWMPr)
library(tidyverse)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.docs = file.path(wd, "docs")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd, "data")
dir.R = file.path(wd,"R")

# Utility -----------------------------------------------------------------


# Read in data ------------------------------------------------------------

kac = site_codes_ind('kac') %>% tibble()

kacsdwq = import_local(path = file.path(dir.data, 'kacsdwq.zip'),
                       station_code = 'kacsdwq',
                       keep_qaqcstatus = TRUE)
kacsdwq$sal %>% range(na.rm = TRUE)
kacsdwq$depth %>% range(na.rm = TRUE)

#next: read in tide data

# Plot some data ------------------------------------------------------------
kacsdwq %>% 
  filter(datetimestamp > "2023-01-01 00:00:00") %>% 
  ggplot(aes(x=datetimestamp,y=depth)) + 
  geom_point()

