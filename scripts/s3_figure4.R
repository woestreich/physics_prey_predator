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
kruskal_p_value <- function(data, variable) {
  test <- kruskal.test(data[[variable]] ~ data$regime)
  return(test$p.value)
}

# physics
pv <- kruskal_p_value(full_ts,"salinity_anomaly")
pa <- ggplot(full_ts, aes(x = regime, y = salinity_anomaly)) +
  geom_boxplot(aes(fill = regime), alpha = 0.3, outlier.shape = NA) +
  geom_jitter(aes(color = regime), width = 0.2, size = 2, alpha = 0.6) +
  scale_fill_manual(values = c("upwelling" = "#AB3535", "post-upwelling" = "#244683")) +
  scale_color_manual(values = c("upwelling" = "#AB3535", "post-upwelling" = "#244683")) + 
  labs(title = " \nPhysical forcing", y = "Salinity anomaly \n (upwelling indicator)") +
  annotate("text", x = 2, y = max(full_ts$salinity_anomaly, na.rm = T), label = paste("p =", format(pv, digits = 2)), hjust = 0.5, size = 4) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0),
        axis.title.x = element_blank(),
        legend.position = "none",text = element_text(size = 10)) 

# prey
pv <- kruskal_p_value(full_ts,"zoop_sv")
pb <- ggplot(full_ts, aes(x = regime, y = zoop_sv)) +
  geom_boxplot(aes(fill = regime), alpha = 0.3, outlier.shape = NA) +
  geom_jitter(aes(color = regime), width = 0.2, size = 2, alpha = 0.6) +
  scale_fill_manual(values = c("upwelling" = "#AB3535", "post-upwelling" = "#244683")) +
  scale_color_manual(values = c("upwelling" = "#AB3535", "post-upwelling" = "#244683")) + 
  labs(title = " \nPrey density", y = "Zooplankton\nvolume scattering (Sv)") +
  annotate("text", x = 2, y = max(full_ts$zoop_sv, na.rm = T), label = paste("p =", format(pv, digits = 2)), hjust = 0.5, size = 4) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0),
        axis.title.x = element_blank(),
        legend.position = "none",text = element_text(size = 10)) 

# predator
pv <- kruskal_p_value(full_ts,"ratio")
pc <- ggplot(full_ts, aes(x = regime, y = ratio)) +
  geom_boxplot(aes(fill = regime), alpha = 0.3, outlier.shape = NA) +
  geom_jitter(aes(color = regime), width = 0.2, size = 2, alpha = 0.6) +
  scale_fill_manual(values = c("upwelling" = "#AB3535", "post-upwelling" = "#244683")) +
  scale_color_manual(values = c("upwelling" = "#AB3535", "post-upwelling" = "#244683")) + 
  labs(title = "Predator\nsocial behavior", y = "D call : song\nnormalized ratio") +
  annotate("text", x = 2, y = 1.49, label = paste("p =", format(pv, digits = 2)), hjust = 0.5, size = 4) +
  ylim(c(0,1.5)) +
  scale_y_sqrt() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0),
        axis.title.x = element_blank(),
        legend.position = "none",text = element_text(size = 10)) 


png("figures/Fig4.png", units="in", width=8, height=4, res=300)
pa + pb + pc
dev.off()

