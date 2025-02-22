library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

rm(list=ls())


##### load processed datasets
## physics
#load("data_processed/M1-SalinityAnomaly_80-250m.RData")
load("data_processed/M1-SalinityAnomaly.RData")
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
