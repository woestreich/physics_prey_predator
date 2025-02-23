library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

rm(list=ls())

## load time series
load("data_processed/full_ts.Rdata")

## plot
# function to perform Kruskal-Wallis test and return p-value
kruskal_p_value <- function(data, variable) {
  test <- kruskal.test(data[[variable]] ~ data$regime)
  return(test$p.value)
}

# physics
pv <- kruskal_p_value(full_ts,"salinity_anomaly")
pa <- ggplot(full_ts, aes(x = regime, y = salinity_anomaly)) +
  geom_boxplot(aes(fill = regime), alpha = 0.3, outlier.shape = NA, color = "black", fill = "black") +
  geom_jitter(aes(color = regime), width = 0.2, size = 2, alpha = 0.6, color = "black") +
  labs(title = "Physical forcing", y = "Salinity anomaly \n (upwelling indicator)") +
  annotate("text", x = 2, y = max(full_ts$salinity_anomaly, na.rm = T), label = paste("p =", format(pv, digits = 2)), hjust = 0.5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_blank(),
        legend.position = "none") 

# prey
pv <- kruskal_p_value(full_ts,"zoop_sv")
pb <- ggplot(full_ts, aes(x = regime, y = zoop_sv)) +
  geom_boxplot(aes(fill = regime), alpha = 0.3, outlier.shape = NA, color = "black", fill = "black") +
  geom_jitter(aes(color = regime), width = 0.2, size = 2, alpha = 0.6, color = "black") +
  labs(title = "Prey density", y = "Zooplankton volume scattering (Sv)") +
  annotate("text", x = 2, y = max(full_ts$zoop_sv, na.rm = T), label = paste("p =", format(pv, digits = 2)), hjust = 0.5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_blank(),
        legend.position = "none") 

# predator
pv <- kruskal_p_value(full_ts,"ratio")
pc <- ggplot(full_ts, aes(x = regime, y = ratio)) +
  geom_boxplot(aes(fill = regime), alpha = 0.3, outlier.shape = NA, color = "black", fill = "black") +
  geom_jitter(aes(color = regime), width = 0.2, size = 2, alpha = 0.6, color = "black") +
  labs(title = "Predator social behavior", y = "D call : song\nnormalized ratio") +
  annotate("text", x = 2, y = max(full_ts$ratio, na.rm = T)*0.95, label = paste("p =", format(pv, digits = 2)), hjust = 0.5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_blank(),
        legend.position = "none") 

