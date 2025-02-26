library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

rm(list=ls())

##### salinity anomaly data
load("data_raw/BWO_M1_SalinityAnomaly.RData")

##### daily avg
salinity_2022 <- M122 %>%
  group_by(time) %>%
  summarise(salinity_anomaly = mean(SA, na.rm = TRUE)) %>%
  rename(date = time)

salinity_2023 <- M123 %>%
  group_by(time) %>%
  summarise(salinity_anomaly = mean(SA, na.rm = TRUE)) %>%
  rename(date = time)

##### save
save(salinity_2022, file = "data_processed/salinity_2022.Rdata")
save(salinity_2023, file = "data_processed/salinity_2023.Rdata")


