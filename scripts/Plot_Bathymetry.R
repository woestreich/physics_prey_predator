setwd("/Volumes/Ryan/Y2024/BIA_II/")

lodup <- FALSE
if(lodup) {
  
  library(tidyverse)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(rnaturalearthhires)
  library(egg)
  library(R.matlab)
  library(pracma)
  library(geosphere)
  
  world <- ne_countries(scale = "large", returnclass = "sf")
  MARS = data.frame(lon = -122.186, lat = 36.7128)
  
  BWO <- data.frame(lon = -122.107, lat = 36.794)
  WGc <- data.frame(lon = -122.0973, lat = 36.8008) # radius of 1500 m
  WGpath <- data.frame(destPoint(c(WGc$lon,WGc$lat), seq(0,360), 1500))
  M1 <- data.frame(lon = -122, lat = 36.75)

  MBNMS <- read.csv(file = '/Users/ryjo/Documents/R/MBNMS.csv')
  Davidson = data.frame(lon = c(-122.5,-123,-123,-122.5,-122.5), lat = c(35.5,35.5,35.9,35.9,35.5))
  
  load("/Users/ryjo/Data/GEBCObathymetry.RData")
  
}

plotbathy <- TRUE
if(plotbathy) {
  blevs <- c(0,100,200,500,1000,2000,3000)
  
  XL <- c(-122.3,-121.77); YL <- c(36.5,37)
  lontick = seq(-122.3,-121.8,by=0.2)
  lattick = seq(36.6,37,by=0.2)
  
  BathyMap <- ggplot(data = world) + 
    geom_contour_filled(data=B,aes(lon,lat,z=depth), breaks = blevs) +
    scale_fill_brewer() +
    geom_sf(fill="gray25", lwd = 0) + theme_bw(base_size = 12) +
    coord_sf(xlim = XL, ylim = YL, label_graticule = "SW") +
    geom_point(data = MARS, aes(lon,lat), shape = 15, size = 3.5) +
    geom_point(data = BWO, aes(lon,lat), size = 3.5) +
    geom_point(data = M1, aes(lon,lat), shape = 18, size = 3.5) +
    geom_path(data=WGpath,aes(x=lon,y=lat), colour="black") +
    geom_path(data=MBNMS,aes(x=lon,y=lat), colour="black") +
    geom_path(data=Davidson,aes(x=lon,y=lat), colour="black") +
    scale_x_continuous(breaks = lontick) + scale_y_continuous(breaks = lattick) +
    annotate("text", x = -119.85, y = 36.85, label = "Depth (km)", size=3.75, color="white") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          legend.key.size = unit(4, 'mm'),
          legend.text = element_text(size = 9),
          plot.title = element_text(hjust = 0.5)) +
    labs(fill='Depth (m)') 
}

