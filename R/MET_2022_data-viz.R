# -----------------------------------------------------------------------
# Title: SWMP MET EDA
# Creator: Chris Guo
# Date: 02 February 2023
# Purpose: Create report figures for the Gulfwatch report ending 2021

# Notes -------------------------------------------------------------------

## Claus O. Wilke's blog was super helpful in reading mult data files.
# https://clauswilke.com/blog/2016/06/13/reading-and-combining-many-tidy-data-files-in-r

## Code for anomalies plots were drafted by J. Schloemer (2022)

# Load packages -----------------------------------------------------------
library(tidyverse)
library(googledrive)
library(lubridate)
library(readxl)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.docs = file.path(wd, "docs")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd, "data")
dir.R = file.path(wd,"R")

# Utility -----------------------------------------------------------------

# Function to download google drive folder contents to a temporary directory,
if(!exists('drive_download_to_tempdir', mode = 'function')) {
  drive_download_to_tempdir = function(x, dir, overwrite = TRUE, ...) {
    purrr::pmap(x, function(name, id, drive_resource, ...) {
      googledrive::drive_download(as_id(id), file.path(dir, name), overwrite = overwrite, ...)
    },
    ...)
  }
}

# Function to check column types in .xlsx files,
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

# Read in data ------------------------------------------------------------

## First, read in the completed yearly datasets from the wd:

data_final =
  list.files(path = dir.data, pattern = "*.csv", full.names = TRUE) %>%
  map(read_csv) %>%
  reduce(rbind)

## Next, We grab provisional data from the SWMP google drive:

# URL for the folder we want,
drive_folder = "https://drive.google.com/drive/folders/1biNujImZTCaR-dGP9ybqVIRDtMVjDCu3" # MET/Data_QAQC/2022
ls_dribble = drive_ls(drive_folder) %>% filter(str_detect(name, ".DS_", negate = TRUE))

# Download files (won't run if there are already multiple files in the temporary directory),
if(length(list.files(path = tempdir())) <= 1) {
  drive_download_to_tmpdir(ls_dribble, dir = tempdir())
} else {"There is more than one file in tempdir().\nUse list.files() to see what's in there.\nDo not run this function if you do not need to." %>% cat()
}

# Create a dataset from primary QAQC data (_QC.csv or _QC.dat from CDMO),
tmp_hom = list.files(path = tempdir(), pattern = "_QC.dat$", full.names = TRUE) %>%
  str_subset("ho") %>%
  sort() %>%
  map(read_csv) %>%
  reduce(rbind) %>%
  mutate(`Station Code` = as.factor("kachomet")) %>%
  relocate(`Station Code`, .before = 1)
tmp_anc = list.files(path = tempdir(), pattern = "_QC.dat$", full.names = TRUE) %>%
  str_subset("ap") %>%
  sort() %>%
  map(read_csv) %>%
  reduce(rbind) %>%
  mutate(`Station Code` = as.factor("kacapmet"),
         TotSoRad = NA,
         F_TotSoRad = NA) %>%
  relocate(`Station Code`, .before = 1) %>%
  arrange(TIMESTAMP)
# Combine and clean up data objects
data_primary = bind_rows(tmp_anc, tmp_hom); rm(tmp_hom, tmp_anc)

# Create a dataset from secondary QAQC data (.xlsx used for quarterly/annual submissions),
data_secondary = list.files(path = tempdir(), pattern = "_QC.xlsx$", full.names = TRUE) %>%
  map(~ read_xlsx(path = ., sheet = "Data")) %>%
  lapply(check_MET_vars) %>%
  reduce(bind_rows)


# Below code will recreate old figs ---------------------------------------

# Select your data to interpret,
data_sd_T = 
  data_wq %>%
  # Seldovia Deep had a different code from 2001-03,
  filter(StationCode %in% c("kacsdwq", "kacsewq")) %>%
  # Create cols for date, your variable, and its flagging codes,
  mutate(date = DateTimeStamp %>% as.POSIXct(format = "%m/%d/%Y %H:%M") %>% date(),
         var = Temp,
         flag = F_Temp %>% parse_number()) %>%
  # Remove rejected and suspect data,
  filter(flag == 0 | flag > 1) %>%
  select(date, var)

# Find monthly means within each year, i.e., your monthly measurements
anom =
  data_sd_T %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(year, month) %>%
  summarize(var = mean(var, na.rm = TRUE))

# Find the mean of the monthly means, as a reference to the long-term average
ref =
  anom %>%
  group_by(month) %>% 
  summarize(var.ref = mean(var, na.rm = TRUE))

# Subtract your reference from your monthly mean to get your anomalies
anom.ref =
  anom %>% 
  left_join(ref, by = "month") %>%
  mutate(date = str_c(year, as.numeric(month), 1, sep = "-") %>% ymd(),
         var.anom = var - var.ref,
         sign = ifelse(var.anom > 0, "pos", "neg"))

# Plot
p.sd.T = 
  ggplot(anom.ref, aes(x = date)) + 
  geom_line(aes(y = var %>% scale, linetype = '')) +
  geom_bar(aes(y = var.anom, fill = sign), stat = "identity") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  scale_y_continuous(name = "Monthly average temperature (degrees C)",
                     limits = c(-3, 3), breaks = seq(-3, 3, 1), labels = seq(1, 13, 2),
                     # Code for the second axis,
                     sec.axis = sec_axis(~., name = "Monthly anomaly (degrees C)",
                                         breaks = seq(-3, 3, 1), labels = seq(-3, 3, 1))) +
  scale_fill_manual(values = c("#4D70B2", "#C30000"), labels = c('', '')) +
  scale_linetype_manual(values = "dashed") +
  labs(title = "Seldovia water temperature, August 2001 - December 2021",
       x = "", fill = "Anomaly", linetype = "Temperature") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
p.sd.T

p.sd.T + 
  geom_vline(xintercept = date("2017-01-01"), color = "limegreen")
# Save plot as image
ggsave(filename = "2001-21 SD Temp anomalies.png", path = dir.figs, plot = p.sd.T, device = "png",
       height = 7, width = 10.5, units = "in", dpi = 330)


# Select your data to interpret,
data_sd_Sal = 
  data_wq %>%
  # Seldovia Deep had a different code from 2001-03,
  filter(StationCode %in% c("kacsdwq", "kacsewq")) %>%
  # Create cols for date, your variable, and its flagging codes,
  mutate(date = DateTimeStamp %>% as.POSIXct(format = "%m/%d/%Y %H:%M") %>% date(),
         var = Sal,
         flag = F_Sal %>% parse_number()) %>%
  # Remove rejected and suspect suspect data <1>,
  filter(flag == 0 | flag > 1) %>%
  # For some reason, Steve/Jim had removed data prior to this date,
  filter(date > "2003-01-31") %>%
  select(date, var)

# Find monthly means within each year, i.e., your monthly measurements
anom =
  data_sd_Sal %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(year, month) %>%
  summarize(var = mean(var, na.rm = TRUE))

# Find the mean of the monthly means, as a reference to the long-term average
ref =
  anom %>%
  group_by(month) %>% 
  summarize(var.ref = mean(var, na.rm = TRUE))

# Subtract your reference from your monthly mean to get your anomalies
anom.ref =
  anom %>% 
  left_join(ref, by = "month") %>%
  mutate(date = str_c(year, as.numeric(month), 1, sep = "-") %>% ymd(),
         var.anom = var - var.ref,
         sign = ifelse(var.anom > 0, "pos", "neg"))

# plot anomalies bar chart. Adjust labels according to your needs
p.sd.Sal = 
  ggplot(anom.ref, aes(x = date)) + 
  geom_line(aes(y = var %>% scale, linetype = '')) +
  geom_bar(aes(y = var.anom, fill = sign), stat = "identity") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  scale_y_continuous(name = "Monthly average salinity (PSU)",
                     limits = c(-4, 2), breaks = seq(-4, 2, 1), labels = seq(27, 33, 1),
                     # Code for the second axis,
                     sec.axis = sec_axis(~., name = "Monthly anomaly (PSU)",
                                         breaks = seq(-4, 2, 1), labels = seq(-4, 2, 1))) +
  scale_fill_manual(values = c("#50A7CF", "#A1D25F"), labels = c('', '')) +
  scale_linetype_manual(values = "dashed") +
  labs(title = "Seldovia salinity, February 2003 - December 2021",
       x = "", fill = "Anomaly", linetype = "Salinity") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
p.sd.Sal

# Save plot as image
ggsave(filename = "2003-21 SD Sal anomalies.png", path = dir.figs, plot = p.sd.Sal, device = "png",
       height = 7, width = 10.5, units = "in", dpi = 330)

# Select your data to interpret,
data_hd_T = 
  data_wq %>%
  # Seldovia Deep had a different code from 2001-03,
  filter(StationCode %in% c("kachdwq", "kacdlwq")) %>%
  # Create cols for date, your variable, and its flagging codes,
  mutate(date = DateTimeStamp %>% as.POSIXct(format = "%m/%d/%Y %H:%M") %>% date(),
         var = Temp,
         flag = F_Temp %>% parse_number()) %>%
  # Remove rejected and suspect data,
  filter(flag == 0 | flag > 1) %>%
  select(date, var)

# Find monthly means within each year, i.e., your monthly measurements
anom =
  data_hd_T %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(year, month) %>%
  summarize(var = mean(var, na.rm = TRUE))

# Find the mean of the monthly means, as a reference to the long-term average
ref =
  anom %>%
  group_by(month) %>% 
  summarize(var.ref = mean(var, na.rm = TRUE))

# Subtract your reference from your monthly mean to get your anomalies
anom.ref =
  anom %>% 
  left_join(ref, by = "month") %>%
  mutate(date = str_c(year, as.numeric(month), 1, sep = "-") %>% ymd(),
         var.anom = var - var.ref,
         sign = ifelse(var.anom > 0, "pos", "neg"))

# Plot
p.hd.T = 
  ggplot(anom.ref, aes(x = date)) + 
  geom_line(aes(y = var %>% scale, linetype = '')) +
  geom_bar(aes(y = var.anom, fill = sign), stat = "identity") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  scale_y_continuous(name = "Monthly average temperature (degrees C)",
                     limits = c(-3, 3), breaks = seq(-3, 3, 1), labels = seq(1, 13, 2),
                     # Code for the second axis,
                     sec.axis = sec_axis(~., name = "Monthly anomaly (degrees C)",
                                         breaks = seq(-3, 3, 1), labels = seq(-3, 3, 1))) +
  scale_fill_manual(values = c("#4D70B2", "#C30000"), labels = c('', '')) +
  scale_linetype_manual(values = "dashed") +
  labs(title = "Homer water temperature, October 2002 - December 2021",
       x = "", fill = "Anomaly", linetype = "Temperature") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
p.hd.T

# Save plot as image
ggsave(filename = "2001-21 HD Temp anomalies.png", path = dir.figs, plot = p.hd.T, device = "png",
       height = 7, width = 10.5, units = "in", dpi = 330)
# Another with the 2017 start marked
p.hd.T + 
  geom_vline(xintercept = date(c("2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01")), color = "limegreen")

# Select your data to interpret,
data_hd_Sal = 
  data_wq %>%
  # Seldovia Deep had a different code from 2001-03,
  filter(StationCode %in% c("kachdwq", "kacdlwq")) %>%
  # Create cols for date, your variable, and its flagging codes,
  mutate(date = DateTimeStamp %>% as.POSIXct(format = "%m/%d/%Y %H:%M") %>% date(),
         var = Sal,
         flag = F_Sal %>% parse_number()) %>%
  # Remove rejected and suspect suspect data <1>,
  filter(flag == 0 | flag > 1) %>%
  select(date, var)

# Find monthly means within each year, i.e., your monthly measurements
anom =
  data_hd_Sal %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(year, month) %>%
  summarize(var = mean(var, na.rm = TRUE))

# Find the mean of the monthly means, as a reference to the long-term average
ref =
  anom %>%
  group_by(month) %>% 
  summarize(var.ref = mean(var, na.rm = TRUE))

# Subtract your reference from your monthly mean to get your anomalies
anom.ref =
  anom %>% 
  left_join(ref, by = "month") %>%
  mutate(date = str_c(year, as.numeric(month), 1, sep = "-") %>% ymd(),
         var.anom = var - var.ref,
         sign = ifelse(var.anom > 0, "pos", "neg"))

# plot anomalies bar chart. Adjust labels according to your needs
p.hd.Sal = 
  ggplot(anom.ref, aes(x = date)) + 
  geom_line(aes(y = var %>% scale, linetype = '')) +
  geom_bar(aes(y = var.anom, fill = sign), stat = "identity") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  scale_y_continuous(name = "Monthly average salinity (PSU)",
                     limits = c(-4, 2), breaks = seq(-4, 2, 1), labels = seq(27, 33, 1),
                     # Code for the second axis,
                     sec.axis = sec_axis(~., name = "Monthly anomaly (PSU)",
                                         breaks = seq(-4, 2, 1), labels = seq(-4, 2, 1))) +
  scale_fill_manual(values = c("#50A7CF", "#A1D25F"), labels = c('', '')) +
  scale_linetype_manual(values = "dashed") +
  labs(title = "Homer salinity, October 2002 - December 2021",
       x = "", fill = "Anomaly", linetype = "Salinity") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
p.hd.Sal

# Save plot as image
ggsave(filename = "2003-21 HD Sal anomalies.png", path = dir.figs, plot = p.hd.Sal, device = "png",
       height = 7, width = 10.5, units = "in", dpi = 330)
