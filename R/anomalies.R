#####################################################################
# This script generates a generic anomalies bar chart as used in SWMP 
# presentations and reports
#####################################################################

rm(list = ls())
dev.off()
setwd("P://Your//Directory//Here")

library(ggplot2)
library(lubridate)
library(stringr)
library(tidyverse)

# import data. must contain "date" column and a variable column
data <- read.csv("your.csv", head=T)
data$date <- as.Date(data$date, "%m/%d/%y")

data$var <- #data$your_variable

# group by year and month and summarize for differencing
anom <- data %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(year, month) %>%
  summarize(var = mean(var, na.rm = TRUE))

# group by month and summarize for reference monthly means
ref <- anom %>%
  group_by(month) %>% 
  summarize(var.ref = mean(var, na.rm = TRUE))

# join tables for plotting
anom.ref <- anom %>% 
  left_join(ref, by = "month") %>%
  mutate(date = str_c(year, as.numeric(month), 1, sep = "-") %>% ymd(),
        var.anom = var - var.ref,
        sign = ifelse(var.anom >0, "pos", "neg"))

# plot anomalies bar chart. Adjust labels according to your needs
p <- ggplot(anom.ref, aes(date, var.anom, fill = sign)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  scale_fill_manual(values = c("#034e7b", "#99000d")) +
  labs(y = "Anomaly", x = "") +
  theme_classic()

p2 <- p + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 13),
  axis.text.y = element_text(size=13),
  axis.title=element_text(size=14))

p2
