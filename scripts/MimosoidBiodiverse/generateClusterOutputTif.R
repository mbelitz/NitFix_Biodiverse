library(terra)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)

## IN ORDER TO EXPORT CLUSTER FOR PLOTTING CLICK ON THE EXPORT
## NEXUS AND THEN CLICK ADD COLORS AND TIF

# read in chao estimate
chao <- rast("mimosoidBiodiverse/spatial__CHAO2_ESTIMATE.tif")

# read in cluster
clus <- rast("mimosoidBiodiverse/userdefinedCluster_COLOUR.tif")
clusr <- raster::raster("mimosoidBiodiverse/userdefinedCluster_COLOUR.tif")

chao_ag <- terra::aggregate(chao, 5)

# make chao df
chao_df <- terra::as.data.frame(chao, xy = T) %>% 
  mutate(z = 1:150006) %>%
  select(x,y,z)

# extract cluster vlaues
library(sp)
spdf <- chao_df
coordinates(spdf) <- ~ x + y

e <- raster::extract(clusr, spdf)

chao_df <- chao_df %>% 
  mutate(cluster = e)

# plot initial result
ggplot() +
  geom_tile(chao_df, mapping = aes(x = x, y = y, fill = as.factor(cluster)))

# problem, lets do some neighborhood math
#make chao_df a raster
chao_df <- chao_df %>% 
  na_if(2147483647)

chao_r <- dplyr::select(chao_df, x,y, cluster) %>% 
  raster::rasterFromXYZ()

chao_rast <- rast(chao_r)

f <- focal(chao_rast, w = 9, fun = "modal")
plot(f)

# extract to resultion of analysis
rich <- rast("mimosoidBiodiverse/spatial__CHAO2_ESTIMATE.tif")

rich_df <- terra::as.data.frame(rich, xy = T) %>% 
  mutate(z = 1:150006)

rich_r <- dplyr::select(rich_df, x,y,z) %>% 
  raster::rasterFromXYZ()

# make f into df to make finer resolution
f_df <- terra::as.data.frame(f, xy = T) 

# extract cluster vlaues
spdf <- f_df
coordinates(spdf) <- ~ x + y

e <- raster::extract(rich_r, spdf)

f_df <- f_df %>% 
  mutate(z = e)

f_df_lj <- left_join(f_df, rich_df, by = "z") %>% 
  filter(!is.na(z))

# plot initial result
ggplot() +
  geom_tile(f_df_lj, mapping = aes(x = x.y, y = y.y, fill = as.factor(focal_modal))) +
  coord_equal() +
  theme_void()


csv_toSave <- f_df_lj %>% 
  dplyr::select(x.y, y.y, focal_modal) %>% 
  dplyr::rename(x = x.y, y = y.y, cluster = focal_modal)


write.csv(x = csv_toSave, file = "mimosoidBiodiverse/clusterCSV.csv", row.names = F)

