library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

rm(list=ls())


##### load processed datasets
## physics
#load("data_processed/M1-SalinityAnomaly_80-250m.RData")
load("data_processed/salinity_2022.RData")
load("data_processed/salinity_2023.RData")
load("data_processed/cuti.Rdata")

## prey
load("data_processed/wg_zoop_2022.RData")
load("data_processed/wg_zoop_2023.RData")
load("data_processed/adcp_backscatter_2022.RData")
load("data_processed/adcp_backscatter_2023.RData")

## predator
load("data_processed/ci_daily.Rdata")
load("data_processed/dcalls_daily.Rdata")


##### calculate D:B ratio (predator vocal behavior metric)
vocal <- dcalls_day %>%
  left_join(ci_daily %>% select(blue_ci, date), by = "date")

vocal$ci_norm <- (vocal$blue_ci - min(vocal$blue_ci, na.rm = T))/(max(vocal$blue_ci, na.rm = T) - min(vocal$blue_ci, na.rm = T))
vocal$d_norm <- (vocal$Dcalls - min(vocal$Dcalls, na.rm = T))/(max(vocal$Dcalls, na.rm = T) - min(vocal$Dcalls, na.rm = T))
vocal$ratio <- vocal$d_norm/vocal$ci_norm
vocal$ratio[vocal$ratio == Inf ] <- NA

##### combine metrics
## prey
adcp_backscatter <- rbind(adcp_backscatter_2022, adcp_backscatter_2023)
zoop <- rbind(zoop_2022, zoop_2023)
prey_all <- left_join(adcp_backscatter, zoop, by = "date")

## physics
salinity <- rbind(salinity_2022, salinity_2023)
salinity$date <- as.Date(salinity$date)

# combing prey with M1 salinity
ts1 <- prey_all %>%
  full_join(select(salinity, date, salinity_anomaly), by = "date") %>%
  arrange(date)

# combine with CUTI 
ts2 <- ts1 %>%
  left_join(select(cuti_csum, date, cuti), by = "date")

## predator
full_ts <- ts2 %>%
  left_join(select(vocal, date, Dcalls, blue_ci, ratio), by = "date")

##### physical regime (defined by final day with salinity anomaly > 0.5)
full_ts$date <- as.Date(full_ts$date)
full_ts$year <- year(full_ts$date)
full_ts$year <- as.factor(full_ts$year)
full_ts <- full_ts %>%
  mutate(
    regime = case_when(
      year == 2022 & date < as.Date("2022-10-04") ~ "upwelling",
      year == 2022 & date >= as.Date("2022-10-04") ~ "post-upwelling",
      year == 2023 & date < as.Date("2023-09-11") ~ "upwelling",
      year == 2023 & date >= as.Date("2023-09-11") ~ "post-upwelling",
    ),
    regime = factor(regime)  
  )

## order factor levels so upwelling comes first
full_ts$regime <- factor(full_ts$regime, levels = c("upwelling", "post-upwelling"))

##### truncate 2023 so we are not biasing the interannual comparison with extra late season days for 1 year but not the other
full_ts <- full_ts %>% 
  filter(date < as.Date("2023-12-01"))

##### save to file
full_ts <- full_ts %>%
  rename(adcp_back_anom = anom)
save(full_ts, file = "data_processed/full_ts.Rdata")


