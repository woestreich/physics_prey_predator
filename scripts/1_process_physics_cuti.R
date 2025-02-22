library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(zoo)

rm(list=ls())

## set running mean window size
windowsize = 10

## load in daily cuti
cuti_daily <- read.csv("data_raw/CUTI_daily.csv",header = TRUE) 
cuti_daily$date <- as.Date(with(cuti_daily, paste(year, month, day,sep="-")), "%Y-%m-%d")

## calculate cumulative sum of cuti for each year of time series (1988-2023)
cuti_daily$yday <- yday(cuti_daily$date)
cuti_daily$csum <- 0
i1 <- 1
for (y in 1988:2023) {
  b <- cuti_daily %>% filter(year == y)
  bcsum <- cumsum(b$X37N)
  i2 <- i1 + length(bcsum) - 1
  cuti_daily$csum[i1:i2] <- bcsum
  i1 <- i2 + 1
}

# 2022
cuti2022 <- filter(cuti_daily, year==2022)
cuti2022 <- select(cuti2022,c("year","date","X37N"))
cuti2022$csum <- cumsum(cuti2022$X37N)
cuti2022$csumrm <- rollapply(cuti2022$csum,windowsize,mean,fill=NA,na.rm = TRUE)
cuti2022$yday <- yday(cuti2022$date)
colnames(cuti2022)[colnames(cuti2022) == 'X37N'] <- 'cuti'

# 2023
cuti2023 <- filter(cuti_daily, year==2023)
cuti2023 <- select(cuti2023,c("year","date","X37N"))
cuti2023$csum <- cumsum(cuti2023$X37N)
cuti2023$csumrm <- rollapply(cuti2023$csum,windowsize,mean,fill=NA,na.rm = TRUE)
cuti2023$yday <- yday(cuti2023$date)
colnames(cuti2023)[colnames(cuti2023) == 'X37N'] <- 'cuti'

# combine
cuti_csum <- do.call("rbind", list(cuti2022,cuti2023))

## calculate climatological mean, 5th percentile, and 95th percentile for cumulative cuti curves 
cuti_clim <- data.frame(matrix(ncol = 6, nrow = 366))
colnames(cuti_clim) <- c("yday","csummean","csum5pctl","csum95pctl","csum1pctl","csum99pctl")
for (i in 1:366) {
  b <- cuti_daily %>% filter(yday == i)
  cuti_clim$yday[i] <- i
  cuti_clim$csummean[i] <- mean(b$csum,na.rm = TRUE)
  cuti_clim$csum5pctl[i] <- quantile(b$csum,.05,na.rm = TRUE)
  cuti_clim$csum95pctl[i] <- quantile(b$csum,.95,na.rm = TRUE)
  cuti_clim$csum1pctl[i] <- quantile(b$csum,.01,na.rm = TRUE)
  cuti_clim$csum99pctl[i] <- quantile(b$csum,.99,na.rm = TRUE)
}
# running mean to smooth
cuti_clim$csummean <- rollapply(cuti_clim$csummean,windowsize,mean,fill=NA,na.rm = TRUE)
cuti_clim$csum1pctl <- rollapply(cuti_clim$csum1pctl,windowsize,mean,fill=NA,na.rm = TRUE)
cuti_clim$csum5pctl <- rollapply(cuti_clim$csum5pctl,windowsize,mean,fill=NA,na.rm = TRUE)
cuti_clim$csum95pctl <- rollapply(cuti_clim$csum95pctl,windowsize,mean,fill=NA,na.rm = TRUE)
cuti_clim$csum99pctl <- rollapply(cuti_clim$csum99pctl,windowsize,mean,fill=NA,na.rm = TRUE)

cuti_clim$date <- as.Date(cuti_clim$yday, origin = "2022-01-01")
cuti_csum$date <- as.Date(cuti_csum$yday, origin = "2022-01-01")

# save
save(cuti_csum, file = "data_processed/cuti.Rdata")
save(cuti_clim, file = "data_processed/cuti_clim.Rdata")

