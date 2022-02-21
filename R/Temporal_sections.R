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

# write plotting function
temporal_plot <- function(df, var) {
 if (var == "Temp") {
  ###### Temperature ######
  ctd.tbl.all = df
  
  temp.interp = akima::interp(
   x = ctd.tb.all$Date,
   y = ctd.tb.all$Depth.m,
   z = ctd.tb.all$Temperature_ITS90_DegC,
   duplicate = "mean",
   nx = 200,
   ny = 200
  )
  
  temp.interp = akima::interp2xyz(temp.interp) %>%
   as_tibble() %>%
   rename(
    Date = x,
    Depth.m = y,
    Temperature_ITS90_DegC = z
   ) %>%
   na.omit() # %>%
  # filter(pressure <=40) # top 40
  
  ODV_colours <-
   c("#feb483",
     "#d31f2a",
     "#ffc000",
     "#27ab19",
     "#0db5e6",
     "#7139fe",
     "#d16cfa")
  
  p <-
   ggplot(data = temp.interp, aes(x = Date, y = Depth.m)) +
   geom_raster(aes(fill = Temperature_ITS90_DegC), interpolate = TRUE) +
   geom_contour2(aes(z = Temperature_ITS90_DegC, label = stat(level)), size = .4) +
   scale_fill_gradientn(
    colours = rev(ODV_colours),
    name = " ",
    limits = c(0, 14),
    breaks = c(0, 2.5, 5.0, 7.5, 10, 12.5)
   ) +
   #scale_y_reverse(expand = c(0, 0), limits = c(100, 0))+
   scale_y_reverse(expand = c(0, 0), limits = c(max(temp.interp$Depth.m), min(temp.interp$Depth.m))) +
   scale_x_continuous(
    expand = c(0, 0),
    limits = c(min(temp.interp$Date), max(temp.interp$Date)),
    breaks = seq(min(temp.interp$Date), max(temp.interp$Date), by = 365),
    labels = unique(year(ctd.tb.all$Date))
   ) +
   labs(
    title = " ",
    subtitle = "Temperature (C)",
    x = "Date",
    y = "Depth (m)"
   ) +
   theme_bw()
  
  # graphic edits to make similar to ODV
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
    #plot.title = element_text(size=15, face="bold", hjust = 1),
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
    #legend.title=element_text(size=14, face="bold",
    #                          margin = margin(t = 0, r = , b = 10, l = 0)),
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
  
  #pdf("T9_S06_TemporalContour_2012-2021_Temp_100m.pdf", width = 10, height =5)
  p2
  #dev.off()
 } else if (var == "Sal") {
  ###### Salinity ######
  ctd.tbl.all = df
  
  temp.interp = akima::interp(
   x = ctd.tb.all$Date,
   y = ctd.tb.all$Depth.m,
   z = ctd.tb.all$Salinity_PSU,
   duplicate = "mean",
   nx = 200,
   ny = 200
  )
  
  temp.interp = akima::interp2xyz(temp.interp) %>%
   as_tibble() %>%
   rename(Date = x,
          Depth.m = y,
          Salinity_PSU = z) %>%
   na.omit() # %>%
  # filter(pressure <=40) # top 40
  
  ODV_colours <-
   c("#feb483",
     "#d31f2a",
     "#ffc000",
     "#27ab19",
     "#0db5e6",
     "#7139fe",
     "#d16cfa")
  
  p <-
   ggplot(data = temp.interp, aes(x = Date, y = Depth.m)) +
   geom_raster(aes(fill = Salinity_PSU), interpolate = TRUE) +
   geom_contour2(aes(z = Salinity_PSU, label = stat(level)), size = .4) +
   scale_fill_gradientn(
    colours = rev(ODV_colours),
    name = " ",
    limits = c(25, 33),
    breaks = c(26, 28, 30, 32)
   ) +
   #scale_y_reverse(expand = c(0, 0), limits = c(100, 0))+
   scale_y_reverse(expand = c(0, 0), limits = c(max(temp.interp$Depth.m), min(temp.interp$Depth.m))) +
   scale_x_continuous(
    expand = c(0, 0),
    limits = c(min(temp.interp$Date), max(temp.interp$Date)),
    breaks = seq(min(temp.interp$Date), max(temp.interp$Date), by = 365),
    labels = unique(year(ctd.tb.all$Date))
   ) +
   labs(
    title = " ",
    subtitle = "Salinity (PSU)",
    x = "Date",
    y = "Depth (m)"
   ) +
   theme_bw()
  
  # graphic edits to make similar to ODV
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
    #plot.title = element_text(size=15, face="bold", hjust = 1),
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
    #legend.title=element_text(size=14, face="bold",
    #                          margin = margin(t = 0, r = , b = 10, l = 0)),
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
  
  #pdf("T9_S06_TemporalContour_2012-2021_Sal_100m.pdf", width = 10, height =5)
  p2
  #dev.off()
  
 } else if (var == "Density") {
  ###### Density #####
  
  ctd.tbl.all = df
  
  temp.interp = akima::interp(
   x = ctd.tb.all$Date,
   y = ctd.tb.all$Depth.m,
   z = ctd.tb.all$Density_sigma.theta.kg.m.3,
   duplicate = "mean",
   nx = 200,
   ny = 200
  )
  
  temp.interp = akima::interp2xyz(temp.interp) %>%
   as_tibble() %>%
   rename(
    Date = x,
    Depth.m = y,
    Density_sigma.theta.kg.m.3 = z
   ) %>%
   na.omit() # %>%
  # filter(pressure <=40) # top 40
  
  ODV_colours <-
   c("#feb483",
     "#d31f2a",
     "#ffc000",
     "#27ab19",
     "#0db5e6",
     "#7139fe",
     "#d16cfa")
  
  p <-
   ggplot(data = temp.interp, aes(x = Date, y = Depth.m)) +
   geom_raster(aes(fill = Density_sigma.theta.kg.m.3), interpolate = TRUE) +
   geom_contour2(aes(z = Density_sigma.theta.kg.m.3, label = stat(level)), size = .4) +
   scale_fill_gradientn(
    colours = rev(ODV_colours),
    name = " ",
    limits = c(20, 26),
    breaks = c(20, 21, 22, 23, 24, 25, 26)
   ) +
   #scale_y_reverse(expand = c(0, 0), limits = c(100, 0))+
   scale_y_reverse(expand = c(0, 0), limits = c(max(temp.interp$Depth.m), min(temp.interp$Depth.m))) +
   scale_x_continuous(
    expand = c(0, 0),
    limits = c(min(temp.interp$Date), max(temp.interp$Date)),
    breaks = seq(min(temp.interp$Date), max(temp.interp$Date), by = 365),
    labels = unique(year(ctd.tb.all$Date))
   ) +
   labs(
    title = " ",
    subtitle = "Density (sigma-theta) [kg/m3]",
    x = "Date",
    y = "Depth (m)"
   ) +
   theme_bw()
  
  # graphic edits to make similar to ODV
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
    #plot.title = element_text(size=15, face="bold", hjust = 1),
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
    #legend.title=element_text(size=14, face="bold",
    #                          margin = margin(t = 0, r = , b = 10, l = 0)),
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
  
  #pdf("T9_S06_TemporalContour_2012-2021_Den_100m.pdf", width = 10, height =5)
  p2
  #dev.off()
 } else if (var == "Fluor") {
  ###### Fluorescence ######
  ctd.tbl.all = df
  
  temp.interp = akima::interp(
   x = ctd.tb.all$Date,
   y = ctd.tb.all$Depth.m,
   z = ctd.tb.all$Fluorescence_mg_m3,
   duplicate = "mean",
   nx = 200,
   ny = 200
  )
  
  temp.interp = akima::interp2xyz(temp.interp) %>%
   as_tibble() %>%
   rename(Date = x,
          Depth.m = y,
          Fluorescence_mg_m3 = z) %>%
   na.omit() # %>%
  # filter(pressure <=40) # top 40
  
  ODV_colours <-
   c("#feb483",
     "#d31f2a",
     "#ffc000",
     "#27ab19",
     "#0db5e6",
     "#7139fe",
     "#d16cfa")
  
  p <-
   ggplot(data = temp.interp, aes(x = Date, y = Depth.m)) +
   geom_raster(aes(fill = Fluorescence_mg_m3), interpolate = TRUE) +
   geom_contour2(aes(z = Fluorescence_mg_m3, label = stat(level)), size = .4) +
   scale_fill_gradientn(
    colours = rev(ODV_colours),
    name = " ",
    limits = c(0, 10),
    breaks = c(0, 2, 4, 6, 8, 10)
   ) +
   #scale_y_reverse(expand = c(0, 0), limits = c(100, 0))+
   scale_y_reverse(expand = c(0, 0), limits = c(max(temp.interp$Depth.m), min(temp.interp$Depth.m))) +
   scale_x_continuous(
    expand = c(0, 0),
    limits = c(min(temp.interp$Date), max(temp.interp$Date)),
    breaks = seq(min(temp.interp$Date), max(temp.interp$Date), by = 365),
    labels = unique(year(ctd.tb.all$Date))
   ) +
   labs(
    title = " ",
    subtitle = "Chlorophyll fluorescence (mg/m3)",
    x = "Date",
    y = "Depth (m)"
   ) +
   theme_bw()
  
  # graphic edits to make similar to ODV
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
    #plot.title = element_text(size=15, face="bold", hjust = 1),
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
    #legend.title=element_text(size=14, face="bold",
    #                          margin = margin(t = 0, r = , b = 10, l = 0)),
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
  
  #pdf("T9_S06_TemporalContour_2012-2021_Fluor_100m.pdf", width = 10, height =5)
  p2
  #dev.off()
 }
}

# try it out! temporal_plot(station, "Variable")
temporal_plot(T906, "Temp")
