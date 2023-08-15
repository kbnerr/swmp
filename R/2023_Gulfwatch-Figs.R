# -----------------------------------------------------------------------
# Title: Gulfwatch Five Year Report
# Creator: Chris Guo
# Created: 17 January 2023
# updated: 11 July 2023
# Purpose: Create report figures for the Gulfwatch report ending 2021

# Notes -------------------------------------------------------------------

## Claus O. Wilke's blog was super helpful in reading mult data files.
# https://clauswilke.com/blog/2016/06/13/reading-and-combining-many-tidy-data-files-in-r

## Code for anomalies plots were drafted by J. Schloemer (2022)

# Load packages -----------------------------------------------------------
library(tidyverse)
library(lubridate)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.docs = file.path(wd, "docs")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd, "data")
dir.R = file.path(wd,"R")

# Utility -----------------------------------------------------------------


# Read in data ------------------------------------------------------------

data_wq =
  list.files(path = dir.data, pattern = "*.csv", full.names = TRUE) %>%
  map(read_csv) %>%
  reduce(rbind)


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
  scale_fill_manual(values = c("#4D70B2", "#C30000"), labels = c('Cold', 'Warm')) +
  scale_linetype_manual(values = "dashed") +
  labs(title = "Seldovia Deep water temperature, August 2001 - December 2021",
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
  # filter(date > "2003-01-31") %>%
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
  scale_fill_manual(values = c("#50A7CF", "#A1D25F"), labels = c('Fresh', 'Saline')) +
  scale_linetype_manual(values = "dashed") +
  labs(title = "Seldovia Deep salinity, August 2001 - December 2021",
       x = "", fill = "Anomaly", linetype = "Salinity") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
p.sd.Sal

# Save plot as image
ggsave(filename = "2001-21 SD Sal anomalies.png", path = dir.figs, plot = p.sd.Sal, device = "png",
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
  scale_fill_manual(values = c("#4D70B2", "#C30000"), labels = c('Cold', 'Warm')) +
  scale_linetype_manual(values = "dashed") +
  labs(title = "Homer Deep water temperature, October 2002 - December 2021",
       x = "", fill = "Anomaly", linetype = "Temperature") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
p.hd.T

# Save plot as image
ggsave(filename = "2002-21 HD Temp anomalies.png", path = dir.figs, plot = p.hd.T, device = "png",
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
  scale_fill_manual(values = c("#50A7CF", "#A1D25F"), labels = c('Fresh', 'Saline')) +
  scale_linetype_manual(values = "dashed") +
  labs(title = "Homer Deep salinity, October 2002 - December 2021",
       x = "", fill = "Anomaly", linetype = "Salinity") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
p.hd.Sal

# Save plot as image
ggsave(filename = "2002-21 HD Sal anomalies.png", path = dir.figs, plot = p.hd.Sal, device = "png",
       height = 7, width = 10.5, units = "in", dpi = 330)


# Select your data to interpret,
data_hs_T = 
  data_wq %>%
  # Homer Surface had different codes in 2004 and 2012,
  filter(StationCode %in% c("kachowq", "kachswq", "kach3wq")) %>%
  # Create cols for date, your variable, and its flagging codes,
  mutate(date = DateTimeStamp %>% as.POSIXct(format = "%m/%d/%Y %H:%M") %>% date(),
         var = Temp,
         flag = F_Temp %>% parse_number()) %>%
  # Remove rejected and suspect data,
  filter(flag == 0 | flag > 1) %>%
  select(date, var)

# Find monthly means within each year, i.e., your monthly measurements
anom =
  data_hs_T %>%
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
p.hs.T = 
  ggplot(anom.ref, aes(x = date)) + 
  geom_line(aes(y = var %>% scale, linetype = '')) +
  geom_bar(aes(y = var.anom, fill = sign), stat = "identity") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  scale_y_continuous(name = "Monthly average temperature (degrees C)",
                     limits = c(-3, 3), breaks = seq(-3, 3, 1), labels = seq(1, 13, 2),
                     # Code for the second axis,
                     sec.axis = sec_axis(~., name = "Monthly anomaly (degrees C)",
                                         breaks = seq(-3, 3, 1), labels = seq(-3, 3, 1))) +
  scale_fill_manual(values = c("#4D70B2", "#C30000"), labels = c('Cold', 'Warm')) +
  scale_linetype_manual(values = "dashed") +
  labs(title = "Homer Surface water temperature, July 2001 - December 2021",
       x = "", fill = "Anomaly", linetype = "Temperature") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
p.hs.T

# Save plot as image
ggsave(filename = "2001-21 HS Temp anomalies.png", path = dir.figs, plot = p.hs.T, device = "png",
       height = 7, width = 10.5, units = "in", dpi = 330)
# Another with the 2017 start marked
p.hd.T + 
  geom_vline(xintercept = date(c("2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01")), color = "limegreen")

# Select your data to interpret,
data_hs_Sal = 
  data_wq %>%
  # Homer Surface had different codes in 2004 and 2012,
  filter(StationCode %in% c("kachowq", "kachswq", "kach3wq")) %>%
  # Create cols for date, your variable, and its flagging codes,
  mutate(date = DateTimeStamp %>% as.POSIXct(format = "%m/%d/%Y %H:%M") %>% date(),
         var = Sal,
         flag = F_Sal %>% parse_number()) %>%
  # Remove rejected and suspect suspect data <1>,
  filter(flag == 0 | flag > 1) %>%
  select(date, var)

# Find monthly means within each year, i.e., your monthly measurements
anom =
  data_hs_Sal %>%
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
p.hs.Sal = 
  ggplot(anom.ref, aes(x = date)) + 
  geom_line(aes(y = var %>% scale, linetype = '')) +
  geom_bar(aes(y = var.anom, fill = sign), stat = "identity") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  scale_y_continuous(name = "Monthly average salinity (PSU)",
                     limits = c(-4, 2), breaks = seq(-4, 2, 1), labels = seq(27, 33, 1),
                     # Code for the second axis,
                     sec.axis = sec_axis(~., name = "Monthly anomaly (PSU)",
                                         breaks = seq(-4, 2, 1), labels = seq(-4, 2, 1))) +
  scale_fill_manual(values = c("#50A7CF", "#A1D25F"), labels = c('Fresh', 'Saline')) +
  scale_linetype_manual(values = "dashed") +
  labs(title = "Homer Surface salinity, July 2001 - December 2021",
       x = "", fill = "Anomaly", linetype = "Salinity") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
p.hs.Sal

# Save plot as image
ggsave(filename = "2001-21 HS Sal anomalies.png", path = dir.figs, plot = p.hs.Sal, device = "png",
       height = 7, width = 10.5, units = "in", dpi = 330)


# Select your data to interpret,
data_ss_T = 
  data_wq %>%
  filter(StationCode %in% c("kacsswq")) %>%
  # Create cols for date, your variable, and its flagging codes,
  mutate(date = DateTimeStamp %>% as.POSIXct(format = "%m/%d/%Y %H:%M") %>% date(),
         var = Temp,
         flag = F_Temp %>% parse_number()) %>%
  # Remove rejected and suspect data,
  filter(flag == 0 | flag > 1) %>%
  select(date, var)

# Find monthly means within each year, i.e., your monthly measurements
anom =
  data_ss_T %>%
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
p.ss.T = 
  ggplot(anom.ref, aes(x = date)) + 
  geom_line(aes(y = var %>% scale, linetype = '')) +
  geom_bar(aes(y = var.anom, fill = sign), stat = "identity") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  scale_y_continuous(name = "Monthly average temperature (degrees C)",
                     limits = c(-3, 3), breaks = seq(-3, 3, 1), labels = seq(1, 13, 2),
                     # Code for the second axis,
                     sec.axis = sec_axis(~., name = "Monthly anomaly (degrees C)",
                                         breaks = seq(-3, 3, 1), labels = seq(-3, 3, 1))) +
  scale_fill_manual(values = c("#4D70B2", "#C30000"), labels = c('Cold', 'Warm')) +
  scale_linetype_manual(values = "dashed") +
  labs(title = "Seldovia Surface water temperature, January 2004 - December 2021",
       x = "", fill = "Anomaly", linetype = "Temperature") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
p.ss.T

# Save plot as image
ggsave(filename = "2004-21 SS Temp anomalies.png", path = dir.figs, plot = p.ss.T, device = "png",
       height = 7, width = 10.5, units = "in", dpi = 330)
# Another with the 2017 start marked
p.hd.T + 
  geom_vline(xintercept = date(c("2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01")), color = "limegreen")

# Select your data to interpret,
data_ss_Sal = 
  data_wq %>%
  filter(StationCode %in% c("kacsswq")) %>%
  # Create cols for date, your variable, and its flagging codes,
  mutate(date = DateTimeStamp %>% as.POSIXct(format = "%m/%d/%Y %H:%M") %>% date(),
         var = Sal,
         flag = F_Sal %>% parse_number()) %>%
  # Remove rejected and suspect suspect data <1>,
  filter(flag == 0 | flag > 1) %>%
  select(date, var)

# Find monthly means within each year, i.e., your monthly measurements
anom =
  data_ss_Sal %>%
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
p.ss.Sal = 
  ggplot(anom.ref, aes(x = date)) + 
  geom_line(aes(y = var %>% scale, linetype = '')) +
  geom_bar(aes(y = var.anom, fill = sign), stat = "identity") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  scale_y_continuous(name = "Monthly average salinity (PSU)",
                     limits = c(-4, 2), breaks = seq(-4, 2, 1), labels = seq(27, 33, 1),
                     # Code for the second axis,
                     sec.axis = sec_axis(~., name = "Monthly anomaly (PSU)",
                                         breaks = seq(-4, 2, 1), labels = seq(-4, 2, 1))) +
  scale_fill_manual(values = c("#50A7CF", "#A1D25F"), labels = c('Fresh', 'Saline')) +
  scale_linetype_manual(values = "dashed") +
  labs(title = "Seldovia Surface salinity, January 2004 - December 2021",
       x = "", fill = "Anomaly", linetype = "Salinity") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
p.ss.Sal

# Save plot as image
ggsave(filename = "2004-21 SS Sal anomalies.png", path = dir.figs, plot = p.ss.Sal, device = "png",
       height = 7, width = 10.5, units = "in", dpi = 330)
