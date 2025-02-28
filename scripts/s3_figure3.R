library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(scales)
library(zoo)

rm(list=ls())

## BWO time series
load("data_processed/full_ts.Rdata")
full_ts$date <- as.POSIXct(full_ts$date)
ts_22 <- full_ts %>% 
  filter(year == 2022)
ts_23 <- full_ts %>% 
  filter(year == 2023)

## also need depth-specific salinity data 
load("data_raw/BWO_M1_SalinityAnomaly.RData")
# adjust for visualization (i.e., avoid "white-out" of super anomalously low sal values)
M123$SA <- ifelse(M123$SA > -3, M123$SA, -3)

##### PLOT

## 1. salinity hovmoller
pa_22 <- ggplot(M122, aes(x = time, y = depth, z = SA)) +
  geom_contour_filled(breaks = c(-3, -2, -1, -0.25, 0.25, 1, 2, 3)) +
  geom_contour(color = NA, breaks = c(-3, -2, -1, -0.25, 0.25, 1, 2, 3)) + 
  scale_fill_manual(values = c("#2166ac", "#67a9cf", "#d1e5f0", "#f7f7f7", "#fddbc7", "#ef8a62", "#b2182b"),
    name = "Anomaly (sd)") +
  scale_y_reverse() +  
  geom_vline(xintercept = as.POSIXct("2022-10-04"), linetype = "dashed") + 
  labs(x = "", y = "") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b",
                   limits = c(as.POSIXct("2022-08-01"),as.POSIXct("2022-12-01")),
                   position = "top") +
  theme_bw() +
  theme(legend.position = "none")

pa_23 <- ggplot(M123, aes(x = time, y = depth, z = SA)) +
  geom_contour_filled(breaks = c(-3, -2, -1, -0.25, 0.25, 1, 2, 3)) +
  geom_contour(color = NA, breaks = c(-3, -2, -1, -0.25, 0.25, 1, 2, 3)) + 
  scale_fill_manual(values = c("#2166ac", "#67a9cf", "#d1e5f0", "#f7f7f7", "#fddbc7", "#ef8a62", "#b2182b"),
                    name = "Anomaly (sd)") +
  scale_y_reverse() +  
  geom_vline(xintercept = as.POSIXct("2023-09-11"), linetype = "dashed") + 
  labs(x = "", y = "") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b",
                   limits = c(as.POSIXct("2023-08-01"), as.POSIXct("2023-12-01")),
                   position = "top") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_blank())


## 2. salinity daily time series
ts_22$salinity_anomaly_rm <- rollapply(
  ts_22$salinity_anomaly,
  width = 10,
  FUN = mean,
  align = "center",
  fill = NA,
  partial = TRUE)

ts_23$salinity_anomaly_rm <- rollapply(
  ts_23$salinity_anomaly,
  width = 10,
  FUN = mean,
  align = "center",
  fill = NA,
  partial = TRUE)

pb_22 <- ggplot(ts_22, aes(date, salinity_anomaly)) + 
  geom_point(alpha = 0.3, color = "black") +  
  geom_line(aes(y = salinity_anomaly_rm), color = "black", size = 1.5) + 
  geom_hline(yintercept = 0, linetype = "dashed") +  
  geom_vline(xintercept = as.POSIXct("2022-10-04"), linetype = "dashed") + 
  labs(x = "", y = "") +
  ylim(c(-2.05,2.05)) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b",
                   limits = c(as.POSIXct("2022-08-01"),as.POSIXct("2022-12-01"))) +
  theme_bw() +
  theme(axis.text.x = element_blank())

pb_23 <- ggplot(ts_23, aes(date, salinity_anomaly)) + 
  geom_point(alpha = 0.3, color = "black") +  
  geom_line(aes(y = salinity_anomaly_rm), color = "black", size = 1.5) + 
  geom_hline(yintercept = 0, linetype = "dashed") +  
  geom_vline(xintercept = as.POSIXct("2023-09-11"), linetype = "dashed") + 
  labs(x = "", y = "") +
  ylim(c(-2.05,2.05)) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b",
                   limits = c(as.POSIXct("2023-08-01"),as.POSIXct("2023-12-01"))) +
  theme_bw() +
  theme(axis.text.y = element_blank()) +
  theme(axis.text.x = element_blank())


## 3. zoop biomass
pc_22 <- ggplot(ts_22, aes(date, zoop_nasc)) +
  geom_point() +
  geom_vline(xintercept = as.POSIXct("2022-10-04"), linetype = "dashed") + 
  labs(x = "", y = "") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b",
                   limits = c(as.POSIXct("2022-08-01"),as.POSIXct("2022-12-01"))) +
  ylim(c(0,1785)) +
  theme_bw() +
  theme(axis.text.x = element_blank())

pc_23 <- ggplot(ts_23, aes(date, zoop_nasc)) +
  geom_point() +
  geom_vline(xintercept = as.POSIXct("2023-09-11"), linetype = "dashed") + 
  labs(x = "", y = "") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b",
                   limits = c(as.POSIXct("2023-08-01"),as.POSIXct("2023-12-01"))) +
  ylim(c(0,1785)) +
  theme_bw() +
  theme(axis.text.y = element_blank()) +
  theme(axis.text.x = element_blank())

## 4. zoop density
pd_22 <- ggplot(ts_22, aes(date, zoop_sv)) +
  geom_point() +
  geom_vline(xintercept = as.POSIXct("2022-10-04"), linetype = "dashed") + 
  labs(x = "", y = "") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b",
                   limits = c(as.POSIXct("2022-08-01"),as.POSIXct("2022-12-01"))) +
  ylim(c(-70, -57.9)) +
  theme_bw() +
  theme(axis.text.x = element_blank())

pd_23 <- ggplot(ts_23, aes(date, zoop_sv)) +
  geom_point() +
  geom_vline(xintercept = as.POSIXct("2023-09-11"), linetype = "dashed") + 
  labs(x = "", y = "") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b",
                   limits = c(as.POSIXct("2023-08-01"),as.POSIXct("2023-12-01"))) +
  ylim(c(-70, -57.9)) +
  theme_bw() +
  theme(axis.text.y = element_blank()) +
  theme(axis.text.x = element_blank())

## 5. predator vocal metrics
pe_22 <- ggplot(ts_22, aes(date, sqrt(ratio))) +
  geom_point() +
  geom_vline(xintercept = as.POSIXct("2022-10-04"), linetype = "dashed") + 
  labs(x = "", y = "") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b",
                   limits = c(as.POSIXct("2022-08-01"),as.POSIXct("2022-12-01"))) +
  ylim(c(0,1.52)) +
  scale_y_sqrt() +
  theme_bw() 

pe_23 <- ggplot(ts_23, aes(date, ratio)) +
  geom_point() +
  geom_vline(xintercept = as.POSIXct("2023-09-11"), linetype = "dashed") + 
  labs(x = "", y = "") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b",
                   limits = c(as.POSIXct("2023-08-01"),as.POSIXct("2023-12-01"))) +
  ylim(c(0,1.52)) +
  theme_bw() +
  scale_y_sqrt() +
  theme(axis.text.y = element_blank())


##### save
p <- (pa_22 | pa_23) / 
  (pb_22 | pb_23) / 
  (pc_22 | pc_23) / 
  (pd_22 | pd_23) /
  (pe_22 | pe_23) 

png("figures/Fig3.png", units = "in", width = 7.7, height = 7.2, res = 300)
p 
dev.off()

# png("figures/Fig3_1.png", units = "in", width = 8.2, height = 1.5, res = 300)
# pa_22 + pa_23
# dev.off()
# 
# png("figures/Fig3_2.png", units = "in", width = 8.2, height = 1.5, res = 300)
# pb_22 + pb_23
# dev.off()
# 
# png("figures/Fig3_3.png", units = "in", width = 8.2, height = 1.5, res = 300)
# pc_22 + pc_23
# dev.off()
# 
# png("figures/Fig3_4.png", units = "in", width = 8.2, height = 1.5, res = 300)
# pd_22 + pd_23
# dev.off()
# 
# png("figures/Fig3_5.png", units = "in", width = 8.2, height = 1.5, res = 300)
# pe_22 + pe_23
# dev.off()

