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
