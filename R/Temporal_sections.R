#####################################################################
# This script generates a temporal CTD section plots used in
# Gulf Watch Alaska presentations and reports
#####################################################################

rm(list = ls())
dev.off()

setwd("P://Your//Directory//Here")
path <- '/Your//Path//Here'

library(dplyr)
library(tidyverse)
library(lubridate)
library(readr)
library(ggplot2)
library(metR)
library(rlang)


# import and aggregate CTD data from a folder. See example @
# https://github.com/kbnerr/SWMP/tree/main/data/CTD
ctd_data <- list.files(path = path,
                       pattern = "*.csv",
                       full.names = TRUE) %>%
 lapply(read_csv) %>%
 bind_rows %>%
 select(
  File.Name,
  Date,
  Station,
  Time,
  CTD.serial,
  Latitude_DD,
  Longitude_DD,
  Bottom.Depth,
  pressure_db,
  Temperature_ITS90_DegC,
  Salinity_PSU,
  Density_sigma.theta.kg.m.3,
  Fluorescence_mg_m3
 )

# use pressure as depth and round to floor
ctd_data$pressure_db <- floor(ctd_data$pressure_db)
colnames(ctd_data)[9] <- "Depth.m" #for aggregate

# filter for 9_6, AB10, AB03 (or edit as needed)
AB10 <- ctd_data %>%
 filter(Station == "AlongBay_10" &
         Date >= as.Date('2017-01-01') & Date <= as.Date('2021-12-31')) %>%
 na.omit()

AB03 <- ctd_data %>%
 filter(Station == "AlongBay_3" &
         Date >= as.Date('2017-01-01') & Date <= as.Date('2021-12-31')) %>%
 na.omit()

T906 <- ctd_data %>%
 filter(Station == "9_6") %>%
 na.omit()

# plotting function
temporal_plot <- function(data, var, saveplot) {
 options(warn=-1)
 
 temp.interp =
  akima::interp(
   x = data$Date,
   y = data$Depth.m,
   z = unlist(data[, {{ var }}]),
   duplicate = "mean",
   nx = 200,
   ny = 200
  )
 
 temp.interp = akima::interp2xyz(temp.interp) %>%
  as_tibble() %>%
  na.omit()
 
 ODV_colours <-
  c("#feb483",
    "#d31f2a",
    "#ffc000",
    "#27ab19",
    "#0db5e6",
    "#7139fe",
    "#d16cfa")
 
 limit_fun <-
  if ({{ var }} == "Temperature_ITS90_DegC") {
   c(0, 14)
  } else if ({{ var }} == "Salinity_PSU") {
   c(25, 33)
  } else if ({{ var }} == "Density_sigma.theta.kg.m.3") {
   c(20, 26)
  } else if ({{ var }} == "Fluorescence_mg_m3") {
   c(0, 10)
  }
 
 break_fun <-
  if ({{ var }} == "Temperature_ITS90_DegC") {
   c(0, 2.5, 5.0, 7.5, 10, 12.5)
  } else if ({{ var }} == "Salinity_PSU") {
   c(26, 28, 30, 32)
  } else if ({{ var }} == "Density_sigma.theta.kg.m.3") {
   c(20, 21, 22, 23, 24, 25, 26)
  } else if ({{ var }} == "Fluorescence_mg_m3") {
   c(0, 2, 4, 6, 8, 10)
  }

 p <-
  ggplot(data = temp.interp, aes(x = x, y = y)) +
  geom_raster(aes(fill = z), interpolate = TRUE) +
  geom_contour2(aes(z = z, label = stat(level)), size = .4) +
  scale_fill_gradientn(
   colours = rev(ODV_colours),
   name = " ",
   limits = limit_fun,
   breaks = break_fun
  ) +
  scale_y_reverse(
   expand = c(0, 0), 
   limits = c(max(temp.interp$y), min(temp.interp$y))) +
  scale_x_continuous(
   expand = c(0, 0),
   limits = c(min(temp.interp$x), max(temp.interp$x)),
   breaks = seq(min(temp.interp$x), max(temp.interp$x), by = 365),
   labels = unique(year(data$Date))
  ) +
  labs(
   title = " ",
   subtitle = {{ var }},
   x = "Date",
   y = "Depth (m)"
  ) +
  theme_bw()
 
 p2 <-
  p + theme(
   axis.title = element_text(size = 14, face = "bold"),
   axis.text.y = element_text(
    size = 13,
    color = "black",
    face = "bold",
    margin = margin(
     t = 0,
     r = 3,
     b = 0,
     l = 0
    )
   ),
   axis.text.x = element_text(
    size = 13,
    color = "black",
    face = "bold",
    margin = margin(
     t = 4,
     r = 0,
     b = 0,
     l = 0
    )
   ),
   plot.title = element_blank(),
   plot.subtitle = element_text(
    size = 13,
    hjust = 1,
    face = "bold",
    color = "black",
    margin = margin(
     t = 0,
     r = ,
     b = 6,
     l = 0
    )
   ),
   legend.text = element_text(size = 12, face = "bold"),
   legend.title = element_blank(),
   legend.key.height = unit(2.75, "lines"),
   panel.grid = element_blank(),
   panel.border = element_rect(
    colour = "black",
    fill = NA,
    size = 1.2
   ),
   plot.margin = margin(.7, .5, .5, .5, "cm")
  )
 

# save plot as pdf?
 df.name <- deparse(substitute(data))
 df.yrmin <- year(min(as.Date(data$Date)))
 df.yrmax <- year(max(as.Date(data$Date)))
 pdfname <- paste0(df.name,
                   "_",
                   {{ var}},
                   "_",
                   df.yrmin,
                   "-",
                   df.yrmax,
                   ".pdf")
 
 if ({{ saveplot }} == TRUE) {
  pdf(file = pdfname, width = 10, height = 5)
  print(p2)
  dev.off()
  p2
 } else if ({{ saveplot }} == FALSE) {
  p2
 }
 
 options(warn=0)
}

# try it out! temporal_plot(station, "Variable", plotsave)
temporal_plot(T906, "Temperature_ITS90_DegC", plotsave = TRUE)
