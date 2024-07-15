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
library(skimr)
# library(esquise) # try this for education
library(ggThemeAssist)

# Define workflow paths ---------------------------------------------------

wd = here::here()
dirs = wd %>% list.files() %>% str_subset(pattern = "^README|^LICENSE|^DS_|.md$|.Rproj$", negate = TRUE)
for (i in seq_along(dirs)) {
  name = str_replace_all(dirs[i], "^", "dir.")
  path = str_replace_all(dirs[i], "^", str_c(wd, "/"))
  assign(name, path)
  rm(name, path, i)
}

# Utility -----------------------------------------------------------------

source(file.path(dir.R, "utility.R"))

# Wrapper function for SWMPr::import_local to combine multiple stations
import_swmp = function(station_code) {
    x = station_code
    tmp = list()
    for (i in 1:length(x)) {
      tmp[[i]] =  import_local(path = file.path(dir.data, '2001-2022.zip'), station_code = x[i], keep_qaqcstatus = TRUE) %>%
        mutate(`station code` = x[i]) %>%
        relocate(`station code`, .before = 1)
    }
    return(tmp)
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
wq.hist = import_swmp(sites.wq) %>%
  reduce(bind_rows) %>%
  # Add col for site name to match .xlsx data,
  mutate(sitename = case_when(str_detect(`station code`, "bc") ~ "Bear Cove",
                              str_detect(`station code`, "pg") ~ "Port Graham",
                              str_detect(`station code`, "hs|h3") ~ "Homer Surface",
                              str_detect(`station code`, "ho|dl|hd") ~ "Homer Deep",
                              str_detect(`station code`, "ss") ~ "Seldovia Surface",
                              str_detect(`station code`, "se|sd") ~ "Seldovia Deep"))

# Check when the data ends - should be last authenticated year
range(wq.hist$datetimestamp)

# 2023 WQ data
wq.2023 = list.files(path = file.path(dirname(getwd()), "Library/CloudStorage/GoogleDrive-chguo@alaska.edu/Shared drives/UAA Kachemak Bay NERR/Monitoring/SWMP/WQ/Data_QAQC/2023"),
                     pattern = "_QC.xlsx$", full.names = TRUE) %>%
  map(~ read_xlsx(path = ., sheet = "Data")) %>%
  map(~ check_WQ_xlsx(x = .)) %>%
  reduce(bind_rows)

# new object combining historical and provisional wq data; remove subsets
wq_all = bind_rows(wq.2023, wq.hist); rm(wq.hist, wq.2023)


# Combine all NUT stations into tbl nut.hist- (this could take a few minutes),
nut.hist = import_swmp(sites.nut) %>%
  reduce(bind_rows) %>%
  # Add col for site name to match .xlsx data,
  mutate(sitename = case_when(str_detect(`station code`, "hh") ~ "Homer Harbor",
                              str_detect(`station code`, "hs|h3") ~ "Homer Surface",
                              str_detect(`station code`, "ho|dl|hd") ~ "Homer Deep",
                              str_detect(`station code`, "ss") ~ "Seldovia Surface",
                              str_detect(`station code`, "se|sd") ~ "Seldovia Deep"))

# Check when the data ends - should be last authenticated year
range(nut.hist$datetimestamp)


# Homer Deep XTS object ---------------------------------------------------

hd.xts = wq.all %>%
  filter(sitename == "Homer Deep") %>%
  xts(order.by = .$datetimestamp)


# Seldovia Deep Temp 2001-2023 --------------------------------------------

# Subset data for site, parameter, and flagging 
wq_sd_T = 
  wq_all %>%
  filter(sitename == "Seldovia Deep") %>%
  # Create cols for date, your variable, and its flagging codes,
  mutate(date = datetimestamp %>% date(),
         var = temp,
         flag = f_temp %>% parse_number()) %>%
  # Remove rejected and suspect data,
  filter(flag == 0 | flag > 1) %>%
  select(date, var)

# Find monthly means within each year, i.e., your monthly measurements
sd_T_anom =
  wq_sd_T %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(year, month) %>%
  summarize(var = mean(var, na.rm = TRUE))

# Find the mean of the monthly means, as a reference to the long-term average
sd_T_ref =
  sd_T_anom %>%
  group_by(month) %>% 
  summarize(var.ref = mean(var, na.rm = TRUE))

# Subtract your reference from your monthly mean to get your anomalies
sd_T_anom_ref =
  sd_T_anom %>% 
  left_join(sd_T_ref, by = "month") %>%
  mutate(date = str_c(year, as.numeric(month), 1, sep = "-") %>% ymd(),
         var.anom = var - var.ref,
         sign = ifelse(var.anom > 0, "pos", "neg"))

# Plot
p_sd_T = 
  ggplot(sd_T_anom_ref, aes(x = date)) + 
  geom_line(aes(y = var %>% scale, linetype = '')) +
  geom_bar(aes(y = var.anom, fill = sign), stat = "identity") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  scale_y_continuous(name = "Temperature (degrees C)",
                     limits = c(-3, 3), breaks = seq(-3, 3, 1), labels = seq(1, 13, 2),
                     # Code for the second axis,
                     sec.axis = sec_axis(~., name = "Temperature anomaly (degrees C)",
                                         breaks = seq(-3, 3, 1), labels = seq(-3, 3, 1))) +
  scale_fill_manual(values = c("#4D70B2", "#C30000"), labels = c('Cold', 'Warm')) +
  scale_linetype_manual(values = "dashed") +
  labs(title = "Seldovia Deep monthly water temperature anomalies, August 2001 - December 2023",
       x = "", fill = "Monthly anomaly", linetype = "Monthly average temperature") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
p_sd_T

# Save plot as image
ggsave(filename = "2001-23 SD Temp anomalies.png", path = dir.figs, plot = p_sd_T, device = "png",
       height = 6, width = 12, units = "in", dpi = 330)


# Seldovia Surface, Temp & CHLA, for Ingrid -------------------------------

# Subset wq data for site, parameter, and flagging 
wq_ss_T = 
  wq_all %>%
  filter(sitename == "Seldovia Surface") %>%
  # Create cols for date, your variable, and its flagging codes,
  mutate(date = datetimestamp %>% date(),
         var = temp,
         flag = f_temp %>% parse_number()) %>%
  # Remove rejected and suspect data,
  filter(flag == 0 | flag > 1) %>%
  select(date, var)

# Find monthly means within each year, i.e., your monthly measurements
ss_T_anom =
  wq_ss_T %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(year, month) %>%
  summarize(var = mean(var, na.rm = TRUE))

# Find the mean of the monthly means, as a reference to the long-term average
ss_T_ref =
  ss_T_anom %>%
  group_by(month) %>% 
  summarize(temp = mean(var, na.rm = TRUE),
            temp.sd = sd(var, na.rm = TRUE),
            .groups = "keep")

# Subset nut data for site, parameter, and flagging
nut_ss_chl = nut.hist %>%
  filter(sitename == "Seldovia Surface") %>%
  # Create cols for date, your variable, and its flagging codes,
  mutate(date = datetimestamp %>% date(),
         var = chla_n,
         flag = f_chla_n %>% parse_number()) %>%
  # Remove rejected and suspect data,
  filter(flag == 0 | flag > 1) %>%
  select(date, var)

# Find monthly means within each year, i.e., your monthly measurements
ss_chl_anom =
  nut_ss_chl %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(year, month) %>%
  summarize(var = mean(var, na.rm = TRUE))

# Find the mean of the monthly means, as a reference to the long-term average
ss_chl_ref =
  ss_chl_anom %>%
  group_by(month) %>% 
  summarize(chla = mean(var, na.rm = TRUE),
            chla.sd = sd(var, na.rm= TRUE),
            .groups = "keep")



# Join the two data sets and plot
p_ss_T_chl =  left_join(ss_T_ref, ss_chl_ref, by = "month") %>%
  ggplot(aes(x = month)) +
  geom_line(aes(y = temp, linetype = ''), color = "gray20") +
  geom_col(aes(y = chla), fill = "green") +
  scale_x_continuous(breaks = seq(1, 12, 1), labels = month.abb) +
  scale_y_continuous(name = "Temperature (degrees C)",
                     limits = c(0, 12), breaks = seq(0, 12, 2),
                     # secondary axis
                     sec.axis = sec_axis(~., name = "Chlorophyll a concentration (ug/L)",
                                         breaks = seq(0, 12, 2))) +
  scale_linetype_manual(values = "solid") +
  labs(title = "Seldovia Surface average monthly water temperature, 2004 - 2023",
       x = "", y = "Chlorophyll a", linetype = "Average monthly temperature") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
p_ss_T_chl

# Save the plot as a png file,
ggsave(filename = "2004-23 SS month avg T and chla.png", path = dir.figs, plot = p_ss_T_chl, device = "png",
       height = 6, width = 10, units = "in", dpi = 330)


# SGD at Seldovia Deep ----------------------------------------------------

# Make a subset of Seldovia Deep data
wq_sd = wq_all %>%
  filter(sitename == "Seldovia Deep") %>%
  mutate(across(.cols = contains("f_"), .fns =  ~ parse_number(.x)))

# And make a Seldovia Surface subset
wq_ss = wq_all %>%
  filter(sitename == "Seldovia Surface") %>%
  mutate(across(.cols = contains("f_"), .fns =  ~ parse_number(.x)))

# skim summary
skim(wq_sd)

# plot salinity for entire dataset, highlighting suspect data in blue
wq_sd %>%
  select(datetimestamp, sal, f_sal) %>%
  filter(f_sal >= 0) %>%
  mutate(sal_ok = if_else(f_sal != 1, sal, NA),
         sal_1 = if_else(f_sal == 1, sal, NA)) %>%
  ggplot(data = ., aes(x = datetimestamp)) +
  geom_point(aes(y = sal_ok), color = 'grey10') +
  geom_point(aes(y = sal_1), color = 'skyblue')

# subset 2023 data
wq_sd_2023 = filter(wq_sd, year(datetimestamp) == 2023)
wq_ss_2023 = filter(wq_ss, year(datetimestamp) == 2023)

# plot 2023 sal data, and highlight suspect data
wq_sd_2023 %>%
  select(datetimestamp, sal, f_sal) %>%
  filter(f_sal >= 0) %>%
  mutate(sal_ok = if_else(f_sal != 1, sal, NA),
         sal_1 = if_else(f_sal == 1, sal, NA)) %>%
  ggplot(data = ., aes(x = datetimestamp)) +
  geom_point(aes(y = sal_ok), color = 'grey10') +
  geom_point(aes(y = sal_1), color = 'skyblue')

# draftsman plots
pairs

# define intervals for all deployments
dec. = ymd_hm('2023-01-01 00:00') %--% ymd_hm('2023-01-12 10:48')
jan = ymd_hm('2023-01-12 10:52') %--% ymd_hm('2023-02-16 12:11')
feb = ymd_hm('2023-02-16 12:16') %--% ymd_hm('2023-03-09 11:22')
mar = ymd_hm('2023-03-09 11:25') %--% ymd_hm('2023-04-04 09:42')
apr = ymd_hm('2023-04-04 12:05') %--% ymd_hm('2023-05-12 10:07')
may = ymd_hm('2023-05-12 10:11') %--% ymd_hm('2023-06-09 09:50')
jun = ymd_hm('2023-06-09 09:53') %--% ymd_hm('2023-07-10 10:37')
jul = ymd_hm('2023-07-10 10:40') %--% ymd_hm('2023-08-02 10:01')
aug = ymd_hm('2023-08-02 09:05') %--% ymd_hm('2023-09-04 08:54')
sep = ymd_hm('2023-09-04 08:57') %--% ymd_hm('2023-10-06 11:19')
oct = ymd_hm('2023-10-06 11:22') %--% ymd_hm('2023-11-02 10:52')
nov = ymd_hm('2023-11-02 10:54') %--% ymd_hm('2023-12-01 12:01')
dec = ymd_hm('2023-12-01 12:06') %--% ymd_hm('2023-12-31 23:45')

# assign the sonde label to the data
wq_sd_2023 = wq_sd_2023 %>% 
  mutate(sonde = case_when(datetimestamp %within% dec. ~ "Taxi",
                           datetimestamp %within% jan ~ "Valence",
                           datetimestamp %within% feb ~ "Taxi",
                           datetimestamp %within% mar ~ "Valence",
                           datetimestamp %within% apr ~ "Taxi",
                           datetimestamp %within% may ~ "Valence",
                           datetimestamp %within% jun ~ "Sierra",
                           datetimestamp %within% jul ~ "Romeo",
                           datetimestamp %within% aug ~ "Taxi",
                           datetimestamp %within% sep ~ "Valence",
                           datetimestamp %within% oct ~ "Taxi",
                           datetimestamp %within% nov ~ "Valence",
                           datetimestamp %within% dec ~ "Taxi"), 
         sonde = as.factor(sonde))

# assign a number to each deployment
wq_sd_2023 = wq_sd_2023 %>%
  mutate(deploy = case_match(datetimestamp,
                             int_start(dec.) %>% ceiling_date("15 mins") ~ 0,
                             int_start(jan) %>% ceiling_date("15 mins") ~ 1,
                             int_start(feb) %>% ceiling_date("15 mins") ~ 2,
                             int_start(mar) %>% ceiling_date("15 mins") ~ 3,
                             int_start(apr) %>% ceiling_date("15 mins") ~ 4,
                             int_start(may) %>% ceiling_date("15 mins") ~ 5,
                             int_start(jun) %>% ceiling_date("15 mins") ~ 6,
                             int_start(jul) %>% ceiling_date("15 mins") ~ 7,
                             int_start(aug) %>% ceiling_date("15 mins") ~ 8,
                             int_start(sep) %>% ceiling_date("15 mins") ~ 9,
                             int_start(oct) %>% ceiling_date("15 mins") ~ 10,
                             int_start(nov) %>% ceiling_date("15 mins") ~ 11,
                             int_start(dec) %>% ceiling_date("15 mins") ~ 12,
                             .default = NA))

# define the Valence deployments
valence = wq_sd_2023 %>%
  drop_na(deploy) %>%
  filter(sonde == "Valence") %>%
  distinct(deploy) %>%
  as.vector() %>%
  unname() %>%
  unlist()

# adjust Sal and SpCond values for Valence deployments:
# subtract/add the difference in mismatched Sal/SpCond values when Valence was deployed,
wq_sd_2023 = wq_sd_2023 %>%
  mutate(sal.diff = case_when(deploy %in% valence ~ sal - lag(sal, 1),
                              sonde == "Valence" & !deploy %in% valence ~ NA,
                              .default = 0),
         spcond.diff = case_when(deploy %in% valence ~ lag(spcond, 1) - spcond,
                                 sonde == "Valence" & !deploy %in% valence ~ NA,
                                 .default = 0)) %>%
  fill(sal.diff) %>%
  fill(spcond.diff) %>%
  mutate(sal = sal - sal.diff,
         spcond = spcond + spcond.diff)

## SD plots

# Temp, all of 2023
wq_sd_2023 %>%
  ggplot(aes(x = datetimestamp)) +
  geom_point(aes(y = temp))

# SpCond, all of 2023
wq_sd_2023 %>%
  ggplot(aes(x = datetimestamp)) +
  geom_point(y = spcond) +
  scale_y_continuous(limits = c(40, 55))

# DO_pct, all of 2023
wq_sd_2023 %>%
  ggplot(aes(x = datetimestamp, y = do_pct)) +
  geom_point()

# pH, all of 2023
wq_sd_2023 %>%
  ggplot(aes(x = datetimestamp, y = ph)) +
  geom_point() +
  scale_y_continuous(limits = c(7.5, 8.5))

## SS plots

# DO%, all of 2023
wq_ss_2023 %>%
  ggplot(aes(x = datetimestamp, y = do_pct)) +
  geom_point(color = "orange") +
  scale_y_continuous(limits = c(0, 120))

# pH, all of 2023
wq_ss_2023 %>%
  ggplot(aes(x = datetimestamp, y = ph)) +
  geom_point(color = "orange") +
  scale_y_continuous(limits = c(7.5, 8.5))


list.files(path = file.path(dir.data, "Snotel987_PG_2023"), full.names = TRUE) %>%
  map(~ read_csv(file = ., skip = 4, col_select = -c(`Site Id`, last_col()))) %>%
  reduce(bind_cols)


