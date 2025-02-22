library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

rm(list=ls())

################################################################################
## 2022

## load in waveglider echosounder data
wg_22 <- read.csv("data_raw/wg_2022.csv")

## for averaging of logarithmic Sv values
wg_22$zoop_linear <- 10^(wg_22$zoop_daily_mean_sv/10)
## average the wg E and W regions
wg_22_avg <- wg_22 %>%
  group_by(date) %>%
  summarize(
    avg_total_daily_mean_sv = mean(total_daily_mean_sv, na.rm = TRUE),
    avg_zoop_daily_mean_sv = mean(zoop_daily_mean_sv, na.rm = TRUE),
    avg_zoop_daily_mean_sv_linear = mean(zoop_linear, na.rm = TRUE),
    avg_total_daily_area_scatter = mean(total_daily_area_scatter, na.rm = TRUE),
    avg_zoop_daily_area_scatter = mean(zoop_daily_area_scatter, na.rm = TRUE)
  )
## log transform Sv values again
wg_22_avg$avg_zoop_daily_mean_sv_corrected <- log10(wg_22_avg$avg_zoop_daily_mean_sv_linear)*10

## date
wg_22_avg$date <- ymd(wg_22_avg$date)

## extract the metrics that will be used in downstream analyses
zoop_2022 <- wg_22_avg %>% 
  select(date,avg_zoop_daily_mean_sv_corrected,avg_zoop_daily_area_scatter)
colnames(zoop_2022) <- c("date","zoop_sv","zoop_nasc")

################################################################################
## 2023

## load in waveglider echosounder data
wg_23 <- read.csv("data_raw/wg_2023.csv")

## for averaging of logarithmic Sv values
wg_23$zoop_linear <- 10^(wg_23$zoop_daily_mean_sv/10)
## average the wg E and W regions
wg_23_avg <- wg_23 %>%
  group_by(date) %>%
  summarize(
    avg_total_daily_mean_sv = mean(total_daily_mean_sv, na.rm = TRUE),
    avg_zoop_daily_mean_sv = mean(zoop_daily_mean_sv, na.rm = TRUE),
    avg_zoop_daily_mean_sv_linear = mean(zoop_linear, na.rm = TRUE),
    avg_total_daily_area_scatter = mean(total_daily_area_scatter, na.rm = TRUE),
    avg_zoop_daily_area_scatter = mean(zoop_daily_area_scatter, na.rm = TRUE)
  )
## log transform Sv values again
wg_23_avg$avg_zoop_daily_mean_sv_corrected <- log10(wg_23_avg$avg_zoop_daily_mean_sv_linear)*10

## date
wg_23_avg$date <- ymd(wg_23_avg$date)

## extract the metrics that will be used in downstream analyses
zoop_2023 <- wg_23_avg %>% 
  select(date,avg_zoop_daily_mean_sv_corrected,avg_zoop_daily_area_scatter)
colnames(zoop_2023) <- c("date","zoop_sv","zoop_nasc")


################################################################################
## save to file
save(zoop_2022, file = "data_processed/wg_zoop_2022.RData")
save(zoop_2023, file = "data_processed/wg_zoop_2023.RData")

