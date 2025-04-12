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

##### Figure S1: Interannual comparisons constrained to the upwelling regime in 
##### each year.
kruskal_p_value_year <- function(data, variable) {
  test <- kruskal.test(data[[variable]] ~ data$year)
  return(test$p.value)
}

full_ts_upwelling_only <- full_ts %>% 
  filter(regime == "upwelling")

pv <- kruskal_p_value_year(full_ts_upwelling_only, "zoop_nasc")
pa <- ggplot(full_ts_upwelling_only, aes(x = year, y = zoop_nasc)) +
  geom_boxplot(aes(fill = year), alpha = 0.3, outlier.shape = NA) +
  geom_jitter(aes(color = year), width = 0.2, size = 2, alpha = 0.6) +
  scale_fill_manual(values = c("2022" = "#66c2a5", "2023" = "#8da0cb")) +
  scale_color_manual(values = c("2022" = "#66c2a5", "2023" = "#8da0cb")) + 
  labs(title = "Prey\nabundance", 
       y = expression(atop("Zooplankton", paste("biomass (", m^2, " ", nmi^-2, ")")))) +
  annotate("text", x = 2, y = max(full_ts$zoop_nasc, na.rm = T),
           label = paste("p =", format(pv, digits = 2)), hjust = 0.5, size = 4) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0)) + 
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        text = element_text(size = 14), 
        plot.title = element_text(size = 12))

pv <- kruskal_p_value_year(full_ts_upwelling_only, "ratio")
pb <- ggplot(full_ts_upwelling_only, aes(x = year, y = ratio)) +
  geom_boxplot(aes(fill = year), alpha = 0.3, outlier.shape = NA) +
  geom_jitter(aes(color = year), width = 0.2, size = 2, alpha = 0.6) +
  scale_fill_manual(values = c("2022" = "#66c2a5", "2023" = "#8da0cb")) +
  scale_color_manual(values = c("2022" = "#66c2a5", "2023" = "#8da0cb")) + 
  labs(title = "Predator\nsocial behavior", 
       y = "D call : song\nnormalized ratio") +
  annotate("text", x = 2, y = 1.52, 
           label = paste("p =", format(pv, digits = 2)), hjust = 0.5, size = 4) +
  ylim(c(0,1.5)) +
  theme_bw() +
  scale_y_sqrt() +
  theme(plot.title = element_text(hjust = 0)) + 
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        text = element_text(size = 14), 
        plot.title = element_text(size = 12))

png("figures/FigS1.png", units="in", width=6, height=4.5, res=300)
pa+pb
dev.off()


##### Figure S2: Daily time series of social behavior for each year, broken down
##### into individual components (D calls, song, D:song normalized ratio).
pa <- ggplot(ts_22, aes(date, Dcalls)) +
  geom_point() +
  geom_vline(xintercept = as.POSIXct("2022-10-04"), linetype = "dashed") + 
  labs(x = "", y = "D calls\n per day") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b",
                   limits = c(as.POSIXct("2022-08-01"),as.POSIXct("2022-12-01"))) +
  ylim(c(0,130)) +
  theme_bw() +
  ggtitle("2022")
pb <- ggplot(ts_23, aes(date, Dcalls)) +
  geom_point() +
  geom_vline(xintercept = as.POSIXct("2023-09-11"), linetype = "dashed") + 
  labs(x = "", y = "") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b",
                   limits = c(as.POSIXct("2023-08-01"),as.POSIXct("2023-12-01"))) +
  ylim(c(0,130)) +
  theme_bw() +
  ggtitle("2023")

pc <- ggplot(ts_22, aes(date, blue_ci)) +
  geom_point() +
  geom_vline(xintercept = as.POSIXct("2022-10-04"), linetype = "dashed") + 
  labs(x = "", y = "Song Index") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b",
                   limits = c(as.POSIXct("2022-08-01"),as.POSIXct("2022-12-01"))) +
  ylim(c(1,1.175)) +
  theme_bw() 
pd <- ggplot(ts_23, aes(date, blue_ci)) +
  geom_point() +
  geom_vline(xintercept = as.POSIXct("2023-09-11"), linetype = "dashed") + 
  labs(x = "", y = "") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b",
                   limits = c(as.POSIXct("2023-08-01"),as.POSIXct("2023-12-01"))) +
  ylim(c(1,1.175)) +
  theme_bw() 

pe <- ggplot(ts_22, aes(date, ratio)) +
  geom_point() +
  geom_vline(xintercept = as.POSIXct("2022-10-04"), linetype = "dashed") + 
  labs(x = "", y = "D call : song\nnormalized ratio") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b",
                   limits = c(as.POSIXct("2022-08-01"),as.POSIXct("2022-12-01"))) +
  scale_y_sqrt(limits = c(0,2)) +
  theme_bw() 
pf <- ggplot(ts_23, aes(date, ratio)) +
  geom_point() +
  geom_vline(xintercept = as.POSIXct("2023-09-11"), linetype = "dashed") + 
  labs(x = "", y = "") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b",
                   limits = c(as.POSIXct("2023-08-01"),as.POSIXct("2023-12-01"))) +
  scale_y_sqrt(limits = c(0,2)) +
  theme_bw() 

png("figures/FigS2.png", units="in", width=7.7, height=5, res=300)
(pa+pb)/(pc+pd)/(pe+pf)
dev.off()


