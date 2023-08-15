# -----------------------------------------------------------------------
# Project: SWMP fix multiple data files
# Creator: Chris Guo
# Updated: 2023.01.23
# Purpose: Fix weird issue of multiple headers due to multiple files downloading from sonde.


# Notes -------------------------------------------------------------------

# If you run into parsing failures, then check the .csv for 'false' columns..
# Select and delete all empty cols.

# Load packages -----------------------------------------------------------
library(tidyverse)
library(lubridate)

# Define workflow paths ---------------------------------------------------

filename = "kacsswq122122_wierddata.csv"
path = file.path(wd, filename)

wd = getwd()
dir.docs = file.path(wd, "docs")
dir.figs = file.path(wd, "figs")
dir.data.final = file.path(wd, "data-final")
dir.data.inprog = file.path(wd, "data-inprog")
dir.R = file.path(wd,"R")

# define path to the weird data file,
path = file.path(dir.data.inprog, "2022")
filename = file.path("kacsswq122122_wierddata")

# Code --------------------------------------------------------------------

# Read in data ------------------------------------------------------------

data = read_csv(file = file.path(path, paste(filename, ".csv", sep = "")),
                locale = locale(encoding = "latin1"),
                skip = 8, skip_empty_rows = TRUE,
                na = c("", "NA", "NaN"))


# Code --------------------------------------------------------------------

out = data %>%
  # Change dates and times into date/time format,
  # This will also replace extra headers with NAs,
  mutate(Date = `Date (MM/DD/YYYY)` %>% mdy(),
         Time = `Time (HH:mm:ss)` %>% hms()) %>%
  # Remove rows with NA in either Date or Time,
  drop_na(Date, Time) %>%
  # Arrange by date/time
  arrange(Date, Time) %>%
  # Remove non-SWMP columns
  select(-Date, -Time)

# Save the output as a .csv file with new name,
write.csv(x = out, file = file.path(path, paste(filename, "_fixed", ".csv", sep = "")), row.names = FALSE)

# This^ fixed file still needs to be cropped for pre- and post- deployment!


