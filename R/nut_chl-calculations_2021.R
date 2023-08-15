# -----------------------------------------------------------------------
# Project: EVOS Gulfwatch Alaska, Environmental Drivers: Oceanographic monitoring in Cook Inlet and Kachemak Bay
# Creator: Chris Guo
# Date: 2022.01.27
# Purpose: Chlorophyll-a calculations using fluorescence readings from 2021 shipboard samples


# Notes -------------------------------------------------------------------

# Analysis workflow was constructed within a different directory than archival.
# This script will not work unless data and scripts are organized by the workflow paths as described below.


# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)


# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")
dir.scripts = file.path(wd,"scripts")


# Utility -----------------------------------------------------------------

k = 2.8376 # slope from the 2013 calibration
tau = 1.9 # average Fo/Fa from 2013 calibration
D = 1 # dilution factor
# D is different only if samples had to be diluted for fluorometer to read

fluor_to_chla = function(Fo, Fa, Ve, Vf) {
  K = 1/k # response factor of calibration curve
  chla = (K)* (tau / (tau-1)) * (Fo - Fa) * (Ve/Vf) * D
  return(chla)
}

MDL = 0.001 # recorded 8/31/2022

# Read in data ------------------------------------------------------------

fluor = read_csv(file = file.path(dir.data, "2021_fluorescence_data-entry.csv"))
ctd_21 = read_csv(file = file.path(dir.data, "aggregated/CookInletKachemakBay_CTD_2021.csv"))

# Wrangle the fluorescence data --------------------------------------------

# First, let's set aside the Station/Date info from the CTD data
ctd = select(ctd_21, Station, Date) %>% distinct()

## Next, we need to clean up our fluorescence data...

unique(fluor$Station)
# There are 'lab' readings that don't need to be included

# Remove unnecessary obs and vars:
x.1 = fluor %>% 
  filter(!Station %in% 'lab') %>%  # drop any 'lab' readings
  select(-c(Date_run, Range, Dilution), # clean up vars
         Date = Date_col) # match the ctd 'Date' label

# Reformat Station labels to match the GWA ctd dataset:
x.2 = x.1 %>%
  mutate(A = sub(x = Station, ".", "")) %>% # remove erroneous first character
  separate(A, into = c('transect', 'site'), sep = 1) %>% 
  mutate(site = as.character(as.numeric(site)), # remove erroneous zeros from sites
         transect = replace(transect, transect == 'B', 'AlongBay')) %>%
  unite(col = "Station", transect, site, sep = "_") # match the ctd 'Station' format

# Replace 'Sample' values with abbr:
x.3 = x.2 %>%
  mutate(Sample = case_when(Sample == 'surface' ~ 'S',
                            Sample == 'deep' ~ 'D'))

# Match date formats with 'lubridate':
x.4 = mutate(x.3, Date = mdy(Date))
y.1 = mutate(ctd, Date = ymd(Date))

is.Date(x.4$Date) # check
is.Date(y.1$Date) # check

# Join the fluorescence data to ctd station-date pairs:
chla.1 = right_join(y.1, x.4, by = c("Station", "Date"))


# Calculate chlorophyll-a -------------------------------------------------

# Using custom fxn fluor_to_chla(), see Utility section above:
chla.2 = chla.1 %>%
  mutate(chla = fluor_to_chla(Fo = F_0,
                              Fa = F_a,
                              Ve = Vol_ext,
                              Vf = Vol_fil)
  )

unique(chla.2$Station)
unique(chla.2$Notes) # check notes
# We have lab replicates in addition to field replicates,
# we should not include these in averages of surface/deep grabs


# With this^^ in mind, let's make a df for plotting purposes,
# with an average chla measurement per station per date per grab: 
chla.plot = chla.2 %>%
  mutate(Replicate = replace(Replicate, Notes %in% 'lab replicate', ' lab-rep')) %>%
  mutate(Replicate = replace(Replicate, Replicate %in% c(1, 2), "")) %>%
  unite(col = Sample, Sample, Replicate, sep = "") %>%
  group_by(Station, Date, Sample) %>%
  mutate(mean.chla = mean(chla),
         min.chla = min(chla),
         max.chla = max(chla)) %>%
  select(Station,
         Date,
         Sample,
         mean.chla,
         min.chla,
         max.chla)


# Plotting chla by station ------------------------------------------------

chla.plot %>%
  filter(Station == 'AlongBay_3') %>%
  ggplot(data = ., aes(x = Date, y = mean.chla, ymin = min.chla, ymax = max.chla, color = Sample)) +
  geom_pointrange() +
  labs(title = 'AlongBay_3')


chla.plot %>%
  filter(Station == 'AlongBay_6') %>%
  ggplot(data = ., aes(x = Date, y = mean.chla, ymin = min.chla, ymax = max.chla, color = Sample)) +
  geom_pointrange() +
  labs(title = 'AlongBay_6')


chla.plot %>%
  filter(Station == 'AlongBay_10') %>%
  ggplot(data = ., aes(x = Date, y = mean.chla, ymin = min.chla, ymax = max.chla, color = Sample)) +
  geom_pointrange() +
  labs(title = 'AlongBay_10')


chla.plot %>%
  filter(Station %in% c('6_2', '6_3', '6_4')) %>%
  ggplot(data = ., aes(x = Date, y = mean.chla, ymin = min.chla, ymax = max.chla, color = Sample)) +
  geom_pointrange() +
  labs(title = 'T6_2-4')


chla.plot %>%
  filter(Station == '3_13') %>%
  ggplot(data = ., aes(x = Date, y = mean.chla, ymin = min.chla, ymax = max.chla, color = Sample)) +
  geom_pointrange() +
  labs(title = 'T3_13')


# Finalize dataset --------------------------------------------------------

# Now that we've done a visual check, let's finalize the CHLA data...
(notes = unique(chla.2$Notes)) # Revisit the notes
# Notes look good.

chla_21 = chla.2 %>%
  filter(!Notes %in% 'lab replicate') %>% # no need to keep these
  select(Station,
         Date,
         Sample,
         Replicate,
         `CHLA_N_ug/L` = chla) %>%
  mutate(`CHLA_N_MDL_ug/L` = MDL)
  

# Clean up ----------------------------------------------------------------

# In case we source() this script, let's clean our environment except for our final data:
rm(list = ls()[!ls() %in% c('chla_21',
                            'wd',
                            'dir.data',
                            'dir.figs',
                            'dir.output',
                            'dir.scripts')])

























