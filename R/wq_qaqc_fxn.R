## Header
# Name, date, title, purpose, etc.


## Load packages
library(SWMPr)
library(tidyverse)
library(lubridate)
library(readxl)
library(rlang)


## Define workflow
wd = here::here()
dirs = wd %>% list.files() %>% str_subset(pattern = "^README|^LICENSE|^DS_|.md$|.Rproj$", negate = TRUE)
for (i in seq_along(dirs)) {
  name = str_replace_all(dirs[i], "^", "dir.")
  path = str_replace_all(dirs[i], "^", str_c(wd, "/"))
  assign(name, path)
  rm(name, path, i)
}


## Utility
# Wrapper function for SWMPr::import_local to combine multiple stations
import_swmp = function(station_code) {
  x = station_code
  out = list()
  for (i in 1:length(x)) {
    out[[i]] =  import_local(path = file.path(dir.data, '2001-2022.zip'), station_code = x[i], keep_qaqcstatus = TRUE) %>%
      mutate(`station code` = x[i]) %>%
      relocate(`station code`, .before = 1)
  }
  return(out)
}

rm_flags = function(data, params = "all") {
  # Get all possible params from CDMO
  param_list = SWMPr::site_codes()$params_reported %>% 
    str_subset(".+") %>%
    str_flatten(",") %>%
    str_extract_all(boundary("word")) %>%
    unlist() %>%
    unique() %>%
    append(c("clevel", "cdepth"))
  
  # Check for invalid entries
  stopifnot(
    "One or more parameters are not valid" = {{ params }} %in% c(param_list, 'all', 'record')
  )
  
  # Define function args
  if ("all" %in% {{ params }}) { # If user indicates that all params in data should be used:
    params = {{ data }} %>%
      names() %>% 
      str_to_lower() %>% 
      unique() %>% 
      subset(. %in% c(param_list, "f_record")) %>% 
      str_remove_all("f_") %>% 
      sort()
    f_params = {{ data }} %>%
      names() %>%
      str_to_lower() %>%
      unique() %>% 
      subset(str_detect(., "f_")) %>% 
      sort()
    stopifnot( # Stop if all params do not have an equivalent flagged column
      "params and f_params do not match" = params == f_params %>% str_remove_all("f_")
    )
    # Define the params and flags to use
    use.params = params
    use.flags = f_params
  } else { # If user designates their own set of params
    use.params = {{ params }} %>% 
      str_to_lower() %>% 
      sort()
    use.flags = str_c(rep("f_", length(use.params)), use.params) %>%
      sort()
  }
  
  # Define the data table to use
  use.data = {{ data }} %>% 
    mutate(across(all_of(use.flags), ~ parse_number(.x))) %>%
    select(contains(c("site", "station", "date", use.params)))
  
  # Iterative replacement of flagged values with NA
  for (i in seq_along(use.params)) {
    cur.data = select(use.data, use.params[i], use.flags[i])
    colnames(cur.data) = c("cur.param", "f_cur.param")
    out.data = mutate(cur.data, out.param = if_else(f_cur.param == -3, NA, cur.param))
    use.data[use.params[i]] = out.data["out.param"]
  }
  # Remove flagged columns
  out = select(use.data, -all_of(use.flags))
  # Return output dataframe
  return(out)
}

## Read in data
# Grab station info for all KAC data from CDMO
kac = site_codes_ind('kac') %>% tibble()
# Concatenate WQ stations
sites.wq = kac$station_code %>% unique() %>% str_subset("wq")
# Define the sites to download (surface only)
sites.wq.DL = str_subset(sites.wq, "hs|h3|ss")
# DL and combine the WQ stations into df wq.srf (this could take a few minutes)
wq.srf = import_swmp(sites.wq.DL) %>%
  reduce(bind_rows) %>%
  # Add col for site name
  mutate(sitename = case_when(str_detect(`station code`, "hs|h3") ~ "Homer Surface",
                              str_detect(`station code`, "ss") ~ "Seldovia Surface"))


## Restructure and QC the data
# Define start and end dates
start = ymd('2008-01-01', tz = 'Etc/GMT+9')
end = ymd('2022-12-31', tz = 'Etc/GMT+9')
# Define parameters manually:
params = c("temp", "sal", "do_pct", "do_mgl", "ph", "turb", "chlfluor")
# Subset data by time and parameters
wq.srf.qc = wq.srf %>%
  filter(datetimestamp %within% interval(start, end)) %>%
  rm_flags(data = ., params = params)







## View data structure
skimr::skim(data)

# View the different sites in the data
data$StationCode %>% unique()


dat = read_csv(file = file.path(dir.data, "641725", "641725.csv"))

## Restructure the data table
dat = dat %>%
  select(-c(isSWMP, `...19`)) %>%
  rename(Record = F_Record) %>%
  mutate(DateTimeStamp = mdy_hm(DateTimeStamp),
         across(.cols = contains("F_"), ~ parse_number(.x)))

# list out the parameters
params = names(tmp) %>% str_subset("Station|DateTime|Record|^F_", negate = TRUE)

var = pluck(params, 1)
flag = paste0("F_", var)
mutate(across(var, ~ ifelse(flag == -3, NA, var)))

