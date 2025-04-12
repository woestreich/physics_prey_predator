library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(egg)
library(R.matlab)
library(pracma)
library(geosphere)
library(ggspatial)


##### 1. MAP

world <- ne_countries(scale = "large", returnclass = "sf")
MARS = data.frame(lon = -122.186, lat = 36.7128)

BWO <- data.frame(lon = -122.107, lat = 36.794)
WGc <- data.frame(lon = -122.0973, lat = 36.8008) # radius of 1500 m
WGpath <- data.frame(destPoint(c(WGc$lon,WGc$lat), seq(0,360), 1500))
M1 <- data.frame(lon = -122, lat = 36.75)

load("data_raw/GEBCObathymetry.RData")


blevs <- c(0,100,200,500,1000,2000,3000)

XL <- c(-122.3,-121.77); YL <- c(36.5,37)
lontick = seq(-122.3,-121.8,by=0.2)
lattick = seq(36.6,37,by=0.2)

BathyMap <- ggplot(data = world) + 
  geom_contour_filled(data=B,aes(lon,lat,z=depth), breaks = blevs) +
  scale_fill_brewer() +
  geom_sf(fill="gray25", lwd = 0) + theme_bw(base_size = 12) +
  coord_sf(xlim = XL, ylim = YL, label_graticule = "SW") +
  geom_point(data = MARS, aes(lon,lat), shape = 25, size = 3, fill = "black") +
  geom_point(data = BWO, aes(lon,lat), size = 3.5) +
  geom_point(data = M1, aes(lon,lat), shape = 18, size = 4) +
  geom_path(data=WGpath,aes(x=lon,y=lat), colour="black") +
  scale_x_continuous(breaks = lontick) + scale_y_continuous(breaks = lattick) +
  annotate("text", x = -119.85, y = 36.85, label = "Depth (km)", size=3.75, color="white") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.key.size = unit(4, 'mm'),
        legend.text = element_text(size = 9),
        plot.title = element_text(hjust = 0.5)) +
  labs(fill='Depth (m)') +
  annotation_scale(location = "bl", width_hint = 0.4)

png("figures/Fig1_map.png", units = "in", width = 4, height = 4, res = 300)
BathyMap
dev.off()


##### 2. SALINITY HOVMOLLER
load("data_raw/BWO_M1_SalinityAnomaly.RData")

pa <- ggplot(M122, aes(x = time, y = depth, z = SA)) +
  geom_contour_filled(breaks = c(-3, -2, -1, -0.25, 0.25, 1, 2, 3)) +
  geom_contour(color = NA, breaks = c(-3, -2, -1, -0.25, 0.25, 1, 2, 3)) + 
  scale_fill_manual(values = c("#2166ac", "#67a9cf", "#d1e5f0", "#f7f7f7", "#fddbc7", "#ef8a62", "#b2182b"),
                    name = "Salinity anomaly (sd)\n(upwelling indicator)") +
  scale_y_reverse() +  
  labs(x = "", y = "") +
  scale_x_datetime(
    breaks = seq(as.POSIXct("2022-08-01"), as.POSIXct("2022-12-01"), by = "2 weeks"),  # Set custom breaks
    date_labels = "%b %d", 
    limits = c(as.POSIXct("2022-08-01"), as.POSIXct("2022-12-01")),
    position = "bottom") +
  geom_point(aes(x = as.POSIXct("2022-08-28 00:00:00"), y = 5), color = "black", size = 4, shape = 8) +  
  theme_bw() 

png("figures/Fig1_salinity.png", units = "in", width = 7.5, height = 2.7, res = 300)
pa
dev.off()

