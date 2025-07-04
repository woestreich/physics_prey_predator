library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

rm(list=ls())

## BWO time series
load("data_processed/full_ts.Rdata")

## plot
# function to perform Kruskal-Wallis test and return p-value
wilcox_p_value <- function(data, variable) {
  test <- wilcox.test(data[[variable]] ~ data$regime)
  return(test$p.value)
}

# physics
pv <- wilcox_p_value(full_ts,"salinity_anomaly")
pa <- ggplot(full_ts, aes(x = regime, y = salinity_anomaly)) +
  geom_boxplot(aes(fill = regime), alpha = 0.3, outlier.shape = NA) +
  geom_jitter(aes(color = regime), width = 0.2, size = 2, alpha = 0.6) +
  scale_fill_manual(values = c("upwelling" = "#AB3535", "post-upwelling" = "#244683")) +
  scale_color_manual(values = c("upwelling" = "#AB3535", "post-upwelling" = "#244683")) + 
  labs(title = "Physical\nforcing", y = "Salinity anomaly (sd)\n(upwelling indicator)") +
  annotate("text", x = 2, y = max(full_ts$salinity_anomaly, na.rm = T), label = "p < .00001", hjust = 0.5, size = 4) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0),
        axis.title.x = element_blank(),
        legend.position = "none",text = element_text(size = 10)) 

# prey
pv <- wilcox_p_value(full_ts,"zoop_sv")
pb <- ggplot(full_ts, aes(x = regime, y = zoop_sv)) +
  geom_boxplot(aes(fill = regime), alpha = 0.3, outlier.shape = NA) +
  geom_jitter(aes(color = regime), width = 0.2, size = 2, alpha = 0.6) +
  scale_fill_manual(values = c("upwelling" = "#AB3535", "post-upwelling" = "#244683")) +
  scale_color_manual(values = c("upwelling" = "#AB3535", "post-upwelling" = "#244683")) + 
  labs(title = "Prey\ndensity", y = "Krill swarm\nlocal density (Sv)") +
  annotate("text", x = 2, y = max(full_ts$zoop_sv, na.rm = T), label = "p < .0001", hjust = 0.5, size = 4) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0),
        axis.title.x = element_blank(),
        legend.position = "none",text = element_text(size = 10)) 

# predator
pv <- wilcox_p_value(full_ts,"ratio")
pc <- ggplot(full_ts, aes(x = regime, y = ratio)) +
  geom_boxplot(aes(fill = regime), alpha = 0.3, outlier.shape = NA) +
  geom_jitter(aes(color = regime), width = 0.2, size = 2, alpha = 0.6) +
  scale_fill_manual(values = c("upwelling" = "#AB3535", "post-upwelling" = "#244683")) +
  scale_color_manual(values = c("upwelling" = "#AB3535", "post-upwelling" = "#244683")) + 
  labs(title = "Predator\nsocial behavior", y = "D call:song\nnormalized ratio") +
  annotate("text", x = 2, y = 2.2, label = "p < .00001", hjust = 0.5, size = 4) +
  scale_y_sqrt(limits = c(0,2.2)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0),
        axis.title.x = element_blank(),
        legend.position = "none",text = element_text(size = 10)) 


png("figures/Fig4.png", units="in", width=8, height=4, res=300)
pa + pb + pc
dev.off()

