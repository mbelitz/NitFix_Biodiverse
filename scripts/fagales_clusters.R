library(terra)
library(sf)
library(rnaturalearth)
library(ggplot2)

## IN ORDER TO EXPORT CLUSTER FOR PLOTTING CLICK ON THE EXPORT
## NEXUS AND THEN CLICK ADD COLORS AND TIF

## fagales shape
fs <- terra::rast("c:/Users/Mike/Desktop/Mimosoid_BD/nexus_(nolist)_COLOUR.tif")
terra::plot(fs)

fs_df <- terra::as.data.frame(fs, xy = T)

head(fs_df)

world <- 

ggplot() + 
  geom_tile()