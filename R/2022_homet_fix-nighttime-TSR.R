library(tidyverse)
library(readxl)
library(lubridate)

wd = getwd()
dir.doc = file.path(wd, "doc")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd, "data")
dir.R = file.path(wd,"R")

sun = read_delim(file = file.path(dir.data, "2022_sunrise-sunset.rtf"),
                 delim = "  ", skip = 12, col_names = FALSE, na = "9999 9999")

homet_22 = read_xlsx(path = file.path(dir.data, "KACHOMET2022.xlsx"), sheet = "Data") %>%
  select(DateTimeStamp, TotSoRad, F_TotSoRad) %>%
  filter(DateTimeStamp < "2022-04-01") %>%
  mutate(Date = date(DateTimeStamp),
         TotSoRad = round(TotSoRad, 0))

sun_interval = sun %>%
  select(-c(5:13)) %>%
  rename(Day = X1, '01' = X2, '02' = X3, '03' = X4) %>%
  pivot_longer(cols = c(2:4), names_to = "Month", values_to = "rise_set") %>%
  drop_na(rise_set) %>%
  mutate(Year = "2022") %>%
  arrange(Month) %>%
  relocate(c(Year, Month), .before = 1) %>%
  unite(col = Date, c(Year, Month, Day), sep = "-") %>%
  separate_wider_delim(rise_set, delim = " ", names = c('rise', 'set')) %>%
  unite(rise, c(Date, rise), sep = " ", remove = FALSE) %>%
  unite(set, c(Date, set), sep = " ", remove = FALSE) %>%
  select(c(1:3)) %>%
  mutate(across(c(1:2), ~ ymd_hm(.x)),
         Date = ymd(Date),
         Daylight = interval(rise - hours(1), set + hours(1))) %>%
  select(-c(1:2))

dat = left_join(sun_interval, homet_22, by = "Date") %>%
  mutate(F_TotSoRad = if_else(!DateTimeStamp %within% Daylight & TotSoRad != 0,
                              "<1> (CSM)",
                              "0"))
writexl::write_xlsx(dat, path = file.path(dir.data, "kachomet2022_fixed-tsr.xlsx"))

