library(tidyverse)
library(dplyr)
library(lubridate)
library(suncalc)
library(circular)

rm(list=ls())

################################################################################
## Process to 5-min resolution metrics for 2022 and 2023 #######################
################################################################################
##### load in ADCP backscatter metrics from Echo View
dmean22 <- read.csv("data_raw/ADCP_backscatter_mean_2022.csv")
dmean23 <- read.csv("data_raw/ADCP_backscatter_mean_2023.csv")

##### for each year, calculate max and mean metrics for depth layers of interest
## 2022
# set up dataframe and keep time
d22 <- as.data.frame(matrix(NA,nrow = length(dmean22$Ping_index), ncol = 10))
colnames(d22) <- c("ping_index","ping_date","ping_time","ping_dt","all_mean","all_max","all_95","bm_mean","bm_max","bm_95")
d22$ping_index <- dmean22$Ping_index
d22$ping_date <- dmean22$Ping_date
d22$ping_time <- dmean22$Ping_time
d22$ping_dt <- as.POSIXct(paste(d22$ping_date, d22$ping_time), format="%m/%d/%y %H:%M:%S")

# convert to linear units
dmean22 <- dmean22 %>%
  mutate(across(starts_with("X"), ~ 10^(.x / 10)))

# depth-range max and mean
# "all" = full depth range except first and final 5 bins from the transducer 
# this leads to an "all" depth average over 41-289m in 2022; 35-283m in 2023 
# "bm" = depth range at which ENP blue whales typically forage (75-250m; Kahane-Rapport et al. 2022)
# this leads to bm depth averages over 73-249m in 2022; 75-251m in 2023
d22$all_mean <- rowMeans(dmean22[, c("X285", "X277", "X269", "X261", "X253", "X245", "X237", "X229", "X221", "X213", "X205", "X197", "X189", "X181", "X173", "X165", "X157", "X149", "X141", "X133", "X125", "X117", "X109", "X101", "X93", "X85", "X77", "X69", "X61", "X53", "X45")])
d22$all_max <- do.call(pmax, dmean22[,c("X285", "X277", "X269", "X261", "X253", "X245", "X237", "X229", "X221", "X213", "X205", "X197", "X189", "X181", "X173", "X165", "X157", "X149", "X141", "X133", "X125", "X117", "X109", "X101", "X93", "X85", "X77", "X69", "X61", "X53", "X45")])  
d22$all_95 <- apply(dmean22[,c("X285", "X277", "X269", "X261", "X253", "X245", "X237", "X229", "X221", "X213", "X205", "X197", "X189", "X181", "X173", "X165", "X157", "X149", "X141", "X133", "X125", "X117", "X109", "X101", "X93", "X85", "X77", "X69", "X61", "X53", "X45")],
                    1, function(x) quantile(x, 0.95))  
d22$bm_mean <- rowMeans(dmean22[, c("X245", "X237", "X229", "X221", "X213", "X205", "X197", "X189", "X181", "X173", "X165", "X157", "X149", "X141", "X133", "X125", "X117", "X109", "X101", "X93", "X85", "X77")])
d22$bm_max <- do.call(pmax, dmean22[, c("X245", "X237", "X229", "X221", "X213", "X205", "X197", "X189", "X181", "X173", "X165", "X157", "X149", "X141", "X133", "X125", "X117", "X109", "X101", "X93", "X85", "X77")])
d22$bm_95 <- apply(dmean22[,c("X245", "X237", "X229", "X221", "X213", "X205", "X197", "X189", "X181", "X173", "X165", "X157", "X149", "X141", "X133", "X125", "X117", "X109", "X101", "X93", "X85", "X77")],
                   1, function(x) quantile(x, 0.95)) 
d22$mean200 <- rowMeans(dmean22[, c("X245", "X237", "X229", "X221", "X213", "X205")])
d22$mean150 <- rowMeans(dmean22[, c("X197", "X189", "X181", "X173", "X165", "X157")])
d22$mean100 <- rowMeans(dmean22[, c("X149", "X141", "X133", "X125", "X117", "X109")])
d22$mean50 <- rowMeans(dmean22[, c("X93", "X85", "X77", "X69", "X61", "X53")])

## 2023
# set up dataframe and keep time
d23 <- as.data.frame(matrix(NA,nrow = length(dmean23$Ping_index), ncol = 8))
colnames(d23) <- c("ping_index","ping_date","ping_time","ping_dt","all_mean","all_max","bm_mean","bm_max")
d23$ping_index <- dmean23$Ping_index
d23$ping_date <- dmean23$Ping_date
d23$ping_time <- dmean23$Ping_time
d23$ping_dt <- as.POSIXct(paste(d23$ping_date, d23$ping_time), format="%m/%d/%y %H:%M:%S")

# convert to linear units
dmean23 <- dmean23 %>%
  mutate(across(starts_with("X"), ~ 10^(.x / 10)))

# depth-averaged (for mean and max of 4 transducers)
d23$all_mean <- rowMeans(dmean23[, c("X279", "X271", "X263", "X255", "X247", "X239", "X231", "X223", "X215", "X207", "X199", "X191", "X183", "X175", "X167", "X159", "X151", "X143", "X135", "X127", "X119", "X111", "X103", "X95", "X87", "X79", "X71", "X63", "X55", "X47", "X39")])
d23$all_max <- do.call(pmax, dmean23[, c("X279", "X271", "X263", "X255", "X247", "X239", "X231", "X223", "X215", "X207", "X199", "X191", "X183", "X175", "X167", "X159", "X151", "X143", "X135", "X127", "X119", "X111", "X103", "X95", "X87", "X79", "X71", "X63", "X55", "X47", "X39")])
d23$all_95 <- apply(dmean23[,c("X279", "X271", "X263", "X255", "X247", "X239", "X231", "X223", "X215", "X207", "X199", "X191", "X183", "X175", "X167", "X159", "X151", "X143", "X135", "X127", "X119", "X111", "X103", "X95", "X87", "X79", "X71", "X63", "X55", "X47", "X39")],
                    1, function(x) quantile(x, 0.95))  
d23$bm_mean <- rowMeans(dmean23[, c("X247", "X239", "X231", "X223", "X215", "X207", "X199", "X191", "X183", "X175", "X167", "X159", "X151", "X143", "X135", "X127", "X119", "X111", "X103", "X95", "X87", "X79")])
d23$bm_max <- do.call(pmax, dmean23[, c("X247", "X239", "X231", "X223", "X215", "X207", "X199", "X191", "X183", "X175", "X167", "X159", "X151", "X143", "X135", "X127", "X119", "X111", "X103", "X95", "X87", "X79")])
d23$bm_95 <- apply(dmean23[,c("X247", "X239", "X231", "X223", "X215", "X207", "X199", "X191", "X183", "X175", "X167", "X159", "X151", "X143", "X135", "X127", "X119", "X111", "X103", "X95", "X87", "X79")],
                   1, function(x) quantile(x, 0.95))  
d23$mean200 <- rowMeans(dmean23[, c("X247", "X239", "X231", "X223", "X215", "X207")])
d23$mean150 <- rowMeans(dmean23[, c("X199", "X191", "X183", "X175", "X167", "X159")])
d23$mean100 <- rowMeans(dmean23[, c("X151", "X143", "X135", "X127", "X119", "X111")])
d23$mean50 <- rowMeans(dmean23[, c("X103", "X95", "X87", "X79", "X71", "X63", "X55")])

##### avg ADCP 5-min data in time (hourly and daily, mean, max, and 95th pctl)
## 2022
# hourly mean, max, 95th pctl, and depth bins
adcp_hr_22 <- d22 %>%
  mutate(hour = format(ping_dt, "%Y-%m-%d %H:00:00")) %>%  
  group_by(hour) %>% 
  summarize(hour_mean = mean(bm_mean),
            hour_max = max(bm_max),
            hour_95 = quantile(bm_95, 0.95),
            hour_mean50 = mean(mean50),
            hour_mean100 = mean(mean100),
            hour_mean150 = mean(mean150),
            hour_mean200 = mean(mean200)) %>%  
  mutate(hour = as.POSIXct(hour)) 
adcp_hr_22$hour <- force_tz(adcp_hr_22$hour, "UTC")
# daily mean, max, and 95th pctl
adcp_day_22 <- d22 %>%
  mutate(date = as.Date(ping_dt)) %>%  
  group_by(date) %>%  
  summarize(day_mean = mean(bm_mean),
            day_max = max(bm_max),
            day_95 = quantile(bm_95, 0.95),
            day_mean50 = mean(mean50),
            day_mean100 = mean(mean100),
            day_mean150 = mean(mean150),
            day_mean200 = mean(mean200))  
# log-transform
adcp_hr_22$hour_mean_log <- log10(adcp_hr_22$hour_mean)
adcp_hr_22$hour_max_log <- log10(adcp_hr_22$hour_max)
adcp_hr_22$hour_95_log <- log10(adcp_hr_22$hour_95)
adcp_hr_22$hour_mean50_log <- log10(adcp_hr_22$hour_mean50)
adcp_hr_22$hour_mean100_log <- log10(adcp_hr_22$hour_mean100)
adcp_hr_22$hour_mean150_log <- log10(adcp_hr_22$hour_mean150)
adcp_hr_22$hour_mean200_log <- log10(adcp_hr_22$hour_mean200)
adcp_day_22$day_mean_log <- log10(adcp_day_22$day_mean)
adcp_day_22$day_max_log <- log10(adcp_day_22$day_max)
adcp_day_22$day_95_log <- log10(adcp_day_22$day_95)
adcp_day_22$day_mean50_log <- log10(adcp_day_22$day_mean50)
adcp_day_22$day_mean100_log <- log10(adcp_day_22$day_mean100)
adcp_day_22$day_mean150_log <- log10(adcp_day_22$day_mean150)
adcp_day_22$day_mean200_log <- log10(adcp_day_22$day_mean200)

## 2023
# hourly mean, max, and 95th pctl
adcp_hr_23 <- d23 %>%
  mutate(hour = format(ping_dt, "%Y-%m-%d %H:00:00")) %>%  
  group_by(hour) %>% 
  summarize(hour_mean = mean(bm_mean),
            hour_max = max(bm_max),
            hour_95 = quantile(bm_95, 0.95),
            hour_mean50 = mean(mean50),
            hour_mean100 = mean(mean100),
            hour_mean150 = mean(mean150),
            hour_mean200 = mean(mean200)) %>%  
  mutate(hour = as.POSIXct(hour)) 
adcp_hr_23$hour <- force_tz(adcp_hr_23$hour, "UTC")
# daily mean, max, and 95th pctl
adcp_day_23 <- d23 %>%
  mutate(date = as.Date(ping_dt)) %>%  
  group_by(date) %>%  
  summarize(day_mean = mean(bm_mean),
            day_max = max(bm_max),
            day_95 = quantile(bm_95, 0.95),
            day_mean50 = mean(mean50),
            day_mean100 = mean(mean100),
            day_mean150 = mean(mean150),
            day_mean200 = mean(mean200))  
# log-transform
adcp_hr_23$hour_mean_log <- log10(adcp_hr_23$hour_mean)
adcp_hr_23$hour_max_log <- log10(adcp_hr_23$hour_max)
adcp_hr_23$hour_95_log <- log10(adcp_hr_23$hour_95)
adcp_hr_23$hour_mean50_log <- log10(adcp_hr_23$hour_mean50)
adcp_hr_23$hour_mean100_log <- log10(adcp_hr_23$hour_mean100)
adcp_hr_23$hour_mean150_log <- log10(adcp_hr_23$hour_mean150)
adcp_hr_23$hour_mean200_log <- log10(adcp_hr_23$hour_mean200)
adcp_day_23$day_mean_log <- log10(adcp_day_23$day_mean)
adcp_day_23$day_max_log <- log10(adcp_day_23$day_max)
adcp_day_23$day_95_log <- log10(adcp_day_23$day_95)
adcp_day_23$day_mean50_log <- log10(adcp_day_23$day_mean50)
adcp_day_23$day_mean100_log <- log10(adcp_day_23$day_mean100)
adcp_day_23$day_mean150_log <- log10(adcp_day_23$day_mean150)
adcp_day_23$day_mean200_log <- log10(adcp_day_23$day_mean200)

################################################################################
## ADCP anomaly metrics ########################################################
################################################################################
day_mean <- mean(c(log(adcp_day_22$day_mean),log(adcp_day_23$day_mean)))
day_sd <- sd(c(log(adcp_day_22$day_mean),log(adcp_day_23$day_mean)))

## 2022
adcp_day_22$anom <- (log(adcp_day_22$day_mean) - day_mean)/day_sd

ggplot(adcp_day_22, aes(date,anom)) + geom_point()

adcp_backscatter_2022 <- adcp_day_22 %>%
  select(date,anom)

save(adcp_backscatter_2022, file = "data_processed/adcp_backscatter_2022.RData")

## 2023
adcp_day_23$anom <- (log(adcp_day_23$day_mean) - day_mean)/day_sd

ggplot(adcp_day_23, aes(date,anom)) + geom_point()

adcp_backscatter_2023 <- adcp_day_23 %>%
  select(date,anom)

save(adcp_backscatter_2023, file = "data_processed/adcp_backscatter_2023.RData")
