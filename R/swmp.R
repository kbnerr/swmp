# -----------------------------------------------------------------------
# Title: 
# Creator: Chris Guo
# Date: 
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

?import_local()

kachowq = import_local(path = file.path(dir.data, '2001-2022.zip'),
                       station_code = 'kachowq')
kachowq$sal %>% range(na.rm = TRUE)
kachowq$depth %>% range(na.rm = TRUE)

kacdlwq = import_local(path = file.path(dir.data, '2001-2022.zip'),
                       station_code = 'kacdlwq')
kacdlwq$sal %>% range(na.rm = TRUE)
kacdlwq$depth %>% range(na.rm = TRUE)


