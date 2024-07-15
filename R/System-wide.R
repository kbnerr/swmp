library(tidyverse)

wd = getwd()
dir.doc = file.path(wd, "doc")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd, "data")
dir.R = file.path(wd,"R")

dat = read_csv(file = file.path(dir.data, "NERRS_sampling_stations.csv"))

dat2 = dat %>%
  filter(Status != "Inactive") %>%
  summarise(Count = n(), .by = `NERR Site ID`)

mean(dat2$Count)
