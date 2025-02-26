library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

rm(list=ls())

calls_hr_22 <- read.csv("data_raw/dcalls2022.csv")
calls_hr_22$hour <- as.POSIXct(calls_hr_22$hour)
calls_hr_22$hour <- force_tz(calls_hr_22$hour, "UTC")
calls_day_22 <- calls_hr_22 %>%
  mutate(date = as.Date(hour)) %>%
  group_by(date) %>% 
  summarize(Dcalls = sum(Dcalls))
calls_hr_23 <- read.csv("data_raw/dcalls2023.csv")
calls_hr_23$hour <- as.POSIXct(calls_hr_23$hour)
calls_hr_23$hour <- force_tz(calls_hr_23$hour, "UTC")
calls_day_23 <- calls_hr_23 %>%
  mutate(date = as.Date(hour)) %>%
  group_by(date) %>% 
  summarize(Dcalls = sum(Dcalls))

##### Combine 2022 and 2023
dcalls_hr <- rbind(calls_hr_22, calls_hr_23)
dcalls_day <- rbind(calls_day_22, calls_day_23)

##### save
save(dcalls_hr, file = "data_processed/dcalls_hourly.Rdata")
save(dcalls_day, file = "data_processed/dcalls_daily.Rdata")

