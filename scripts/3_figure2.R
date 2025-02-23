library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

rm(list=ls())

## CUTI
load("data_processed/cuti.Rdata")
load("data_processed/cuti_clim.Rdata")

## plot

load("data_processed/migration_transition.Rdata")
migration_transition$year <- year(migration_transition$date)
migrate_22 <- migration_transition %>%
  filter(format(date, "%Y") == "2022")
migrate_onset_22 <- migrate_22 %>%
  arrange(date) %>%
  slice(1) %>%
  pull(date)
migrate_onset_22 <- as.Date(migrate_onset_22)
migrate_mean_22 <- as.Date(mean(migrate_22$date))

migrate_23 <- migration_transition %>%
  filter(format(date, "%Y") == "2023")
migrate_onset_23 <- migrate_23 %>%
  arrange(date) %>%
  slice(1) %>%
  pull(date)
migrate_onset_23 <- as.Date(migrate_onset_23)
migrate_onset_23 <- update(migrate_onset_23, year = 2022)
migrate_mean_23 <- as.Date(mean(migrate_23$date))
migrate_mean_23 <- update(migrate_mean_23, year = 2022)
  
custom_breaks <- seq(as.Date("2022-01-01"), as.Date("2022-12-15"), by = "1 month")

png("figures/Fig2.png", units="in", width=6, height=4, res=300)
pa <- ggplot(cuti_clim, aes(date_adj, csummean)) +
  geom_rect(aes(xmin = as.Date("2022-08-17"), xmax = as.Date("2022-11-29"), ymin = -Inf, ymax = Inf), fill = "lightgray", alpha = 0.5) +
  geom_line(aes(y = csum5pctl), linetype="dashed", color="black", linewidth=0.5) +
  geom_line(aes(y = csum95pctl), linetype="dashed", color="black", linewidth=0.5) +
  geom_line(color="black", linewidth=1) +
  geom_line(data=cuti_csum, aes(x=date_adj, y=csumrm, color=as.factor(year)), linewidth=1.8) +
  geom_point(aes(x = migrate_onset_22, y = 250), color = "black", fill = "#66c2a5", shape = 25, size = 4) +
  geom_point(aes(x = migrate_onset_23, y = 250), color = "black", fill = "#8da0cb", shape = 25, size = 4) +
  ylab(expression(atop(paste("Upwelling transport (CUTI ", m^2, " ", s^-1, ")"), "cumulative sum"))) +
  xlab("") +
  scale_x_date(breaks = custom_breaks, date_labels = "%b", limits = c(min(cuti_clim$date), as.Date("2023-01-15"))) +
  scale_color_manual(values = c("#66c2a5", "#8da0cb")) +
  annotate("text", x = as.Date("2022-12-31"), y = 246, label = "2022", color = "#66c2a5", size = 4, hjust = 0) +
  annotate("text", x = as.Date("2022-12-31"), y = 172, label = "2023", color = "#8da0cb", size = 4, hjust = 0) +
  geom_segment(aes(x = as.Date("2022-01-10"), xend = as.Date("2022-02-20"), y = 240, yend = 240), 
               color = "black", size = 1) +
  geom_segment(aes(x = as.Date("2022-01-10"), xend = as.Date("2022-02-20"), y = 220, yend = 220), 
               linetype = "dashed", color = "black", size = 0.5) +
  geom_point(aes(x = as.Date("2022-02-10"), y = 200), color = "black", fill = "black", shape = 25, size = 4) +
  geom_rect(aes(xmin = as.Date("2022-01-10"), xmax = as.Date("2022-02-20"), ymin = 175, ymax = 185), fill = "lightgray", alpha = 0.5) +
  annotate("text", x = as.Date("2022-02-25"), y = 240, label = "climatological mean", color = "black", size = 3, hjust = 0) +
  annotate("text", x = as.Date("2022-02-25"), y = 220, label = "climatological 5th-95th pctl", color = "black", size = 3, hjust = 0) +
  annotate("text", x = as.Date("2022-02-25"), y = 200, label = "migration onset", color = "black", size = 3, hjust = 0) +
  annotate("text", x = as.Date("2022-02-25"), y = 180, label = "study period", color = "black", size = 3, hjust = 0) +
  geom_rect(aes(xmin = as.Date("2022-01-02"), xmax = as.Date("2022-06-25"), ymin = 170, ymax = 250), color = "black", fill = NA, size = 1) +
  theme_classic() +
  theme(legend.position="none",text = element_text(size = 14))
pa

dev.off()

