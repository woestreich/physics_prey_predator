library(ggOceanMaps)
library(tidyverse)
library(geosphere)
library(mapdata)
library(marmap)

lon1 <- -122.3
lon2 <- -121.73
lat1 <- 36.5
lat2 <- 37

bathy <- getNOAA.bathy(lon1, lon2, lat1, lat2 ,res=1, keep=TRUE)

blues <- colorRampPalette(c("navy","midnightblue", "royalblue4", "royalblue3", "lightsteelblue1"))(15)
greys <- c(grey(0.6))

plot(bathy, im=TRUE, land=TRUE, xlim=c(lon1, lon2), ylim=c(lat1, lat2), xaxs="i", yaxs="i", bpal=list(c(min(bathy),0,blues),c(0,max(bathy),greys)), lwd=0, las=1, xlab="")
plot(bathy, deep=0, shallow=0, step=0, lwd=1.5, drawlabel=FALSE, add=TRUE, xlim=c(-124.5,-120.5), ylim=c(35.3,37.5), xaxs="i", yaxs="i", xlab="")

# MARS
symbols(-122.1868, 36.7125, squares=rep(0.03, 1), 
        inches=FALSE, bg="white", fg=NA, lwd=3, add=TRUE)

# BWO deep
symbols(-122.107, 36.794, circles=rep(0.01, 1), 
        inches=FALSE, bg="white", fg=NA, lwd=3, add=TRUE)

# WG circle
symbols(-122.0973, 36.8008, circles=rep(0.03, 1), 
        inches=FALSE, bg=NA, fg="white", lwd=3, add=TRUE)

# M1
triangle_x <- c(-122.015, -122, -121.985)  # X coordinates for the triangle's vertices
triangle_y <- c(36.76, 36.74, 36.76)    # Y coordinates for the triangle's vertices
polygon(triangle_x, triangle_y, col="white", border="white", lwd=3)
