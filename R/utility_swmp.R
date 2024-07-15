# -----------------------------------------------------------------------
# Project: Utility swmp
# Creator: Chris Guo
# Updated: 
# Purpose: Clearing house for jack knife functions and objects

# Notes -------------------------------------------------------------------


# Load packages -----------------------------------------------------------

library(tidyverse)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.out = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")
dir.R = file.path(wd,"R")


# For plotting ------------------------------------------------------------

## For adding labels to boxplot graphs,
boxplot.n.max <- function(x){
  return(c(y = max(x), label = length(x)))
}

boxplot.n.min <- function(x){
  return(c(y = min(x), label = length(x)))
}

boxplot.n.median <- function(x){
  return(c(y = median(x), label = length(x)))
}

## Themes
pca_theme = theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

nmds_theme = theme(axis.title = element_text(face = NULL, colour = "black"), 
                   panel.background = element_blank(),
                   panel.border = element_rect(fill = NA, colour = "black"), 
                   axis.ticks = element_blank(),
                   axis.text = element_blank(),
                   legend.key = element_blank(), 
                   legend.title = element_text(face = NULL, colour = "black"), 
                   legend.text = element_text(colour = "black"))


# Useful spatial objects --------------------------------------------------

# Bounding Boxes
# All boxes in (Lng, Lat) and EPSG: 4326 - WGS 84
# Check out bboxfinder.com to quickly make new bboxes
bbox.CookInlet = c(-154.715792, 58.501428, -148.574435, 61.566390)

# Kachemak Bay NERR boundaries

# start swmp- delete header later -----------------------------------------

if(require("lme4")){
  print("lme4 is loaded correctly")
} else {
  print("trying to install lme4")
  install.packages("lme4")
  if(require(lme4)){
    print("lme4 installed and loaded")
  } else {
    stop("could not install lme4")
  }
}

# exo standard calculations by linear interpolation
exo_std_calc = function(std_temp = 20, param = NULL) {
  requireNamespace("tidyverse")
  names = c("temp",
            "chla_rfu",
            "chla_ugl",
            "phycocyanin_rfu",
            "phycocyanin_ugl",
            "phycoerythrin_rfu",
            "phycoerythrin_ugl",
            "fdom_rfu",
            "fdom_qsu")
  stopifnot(is.numeric(std_temp))
  if (is.null(param)) names_select = names
    else names_select = param
  readxl::read_xlsx(path = file.path(getwd(), "data", "TAL_fDOM_TempAdjCalc.xlsx"),
                    range = "B3:J14",
                    col_names = names,
                    col_types = "numeric") %>%
    select(temp, contains(names_select) | matches(names_select)) %>%
    pivot_longer(cols = -temp, names_to = "param", values_to = "value") %>%
    nest_by(param) %>%
    mutate(interp = list(approx(data$temp, data$value, xout = std_temp)),
           std_temp = interp$x,
           std_value = interp$y) %>%
    select(param, std_temp, std_value) %>%
    unnest(everything())
}

cor_test_temp = function(df) cor.test(df$temp, df$y, method = "pearson") %>% broom::tidy()
pivot_longer(dat_adjust, cols = -temp, names_to = "param", values_to = "y") %>%
  group_by(param) %>%
  nest() %>%
  mutate(model = map(.x = data, .f = cor_test_temp)) %>%
  select(-data) %>%
  unnest(cols = c(model))


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
