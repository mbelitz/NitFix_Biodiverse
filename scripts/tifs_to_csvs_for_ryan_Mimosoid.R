library(dplyr)
library(ggplot2)
library(raster)


## generating csvs of x,y,metric to share with Ryan Folk

## write a funciton to make this easy

tif_to_csv <- function(x, name){
  
  r <- terra::rast(x)
  crs(r) <- "+proj=moll +lon_0=1 +datum=WGS84 +units=m +no_defs"
  r <- terra::project(r, y = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
  r_csv <- terra::as.data.frame(r,xy = T) %>% 
    na.omit() %>% 
    dplyr::rename(value = 3)
  
  write.csv(r_csv,
            paste("Mimosoid_CSVs_ToShare_WGS84/",
                  name, "_20km.csv", sep = ""),
            row.names = F)
  
}

tif_to_csv(x = "mimosoidBiodiverse/spatial__CHAO2_ESTIMATE.tif", name = "richness")

tif_to_csv(x = "mimosoidBiodiverse/spatial__PD_P.tif", name = "PD_P")

tif_to_csv(x = "mimosoidBiodiverse/spatial__PHYLO_RPD2.tif", name = "RPD")

tif_to_csv(x = "mimosoidBiodiverse/spatial__PE_CWE.tif", name = "CWE")

tif_to_csv(x = "mimosoidBiodiverse/spatial__PMPD2_MEAN.tif", name = "PMPD")

tif_to_csv(x = "mimosoidBiodiverse/rand__PD_P.tif", name = "rand_PD_P")

tif_to_csv(x = "mimosoidBiodiverse/rand__PHYLO_RPD2.tif", name = "rand_RPD")


## CANAPE
xx <- rast("mimosoidBiodiverse/rand__PE_CWE.tif")
crs(xx) <- "+proj=moll +lon_0=1 +datum=WGS84 +units=m +no_defs"
xx <- terra::project(xx, y = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
xx_df <- terra::as.data.frame(xx, xy = TRUE) %>% 
  dplyr::rename(PE_WE_P = 3)

yy <- rast("mimosoidBiodiverse/rand__PHYLO_RPE_NULL2.tif")
crs(yy) <- "+proj=moll +lon_0=1 +datum=WGS84 +units=m +no_defs"
yy <- terra::project(yy, y = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
yy_df <- terra::as.data.frame(yy, xy = TRUE) %>% 
  dplyr::rename(RPE_NULL2 = 3)

zz <- rast("mimosoidBiodiverse/rand__PHYLO_RPE2.tif")
crs(zz) <- "+proj=moll +lon_0=1 +datum=WGS84 +units=m +no_defs"
zz <- terra::project(zz, y = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
zz_df <- terra::as.data.frame(zz, xy = TRUE) %>% 
  dplyr::rename(RPE2 = 3)


significance_fun <- function(x, y, z){
  #  simplify the logic below
  ifelse(test = is.na(x), yes = 0, no = x)
  ifelse(test = is.na(y), yes = 0, no = x)
  ifelse(test = is.na(z), yes = 0.5, no = x)
  
  ifelse(test = x <= 0.95 & y <= 0.95, yes = "Not Sig",
         no = 
           ifelse(z < 0.025, yes = "Neo",
                  no = 
                    ifelse(z > 0.975, yes = "Paleo",
                           no = "Mixed")
           ))
}

Significance <- significance_fun(x = xx_df$PE_WE_P, 
                                 y = yy_df$RPE_NULL2, 
                                 z = zz_df$RPE2)

df2 <- left_join(xx_df, yy_df) 
df2 <- left_join(df2, zz_df)

df3 <- df2 %>% 
  mutate(PE_WE_P = tidyr::replace_na(PE_WE_P, 0),
         RPE_NULL2 = tidyr::replace_na(RPE_NULL2, 0),
         RPE2 = tidyr::replace_na(RPE2, 0.5)) 

Significance <- significance_fun(x = df3$PE_WE_P, y = df3$RPE_NULL2,
                                 z = df3$RPE2)

canape_csv <- cbind(df3, Significance) %>% 
  dplyr::select(x, y, Significance)

write.csv(x = canape_csv, file = "Mimosoid_CSVs_ToShare_WGS84/CANAPE_20km.csv",row.names = F)

## prop nodulating
# read in nodulating data
nod <- data.table::fread("mimosoid_nodulationstatus_persepecies (1).csv", header = T)

# read in sdm data
sdm <- data.table::fread("Mimosoid_SDMs_AsCSV_20KM_presencesOnly.csv") 

sdm_nod <- left_join(sdm, nod, by = c("binomial" = "species"))

rich <- raster::raster("mimosoidBiodiverse/spatial__CHAO2_ESTIMATE.tif")

sp_df <- sdm_nod
coordinates(sp_df) <- ~ x_reg + y_reg

# regularlize coordinates
biomdf <- raster::as.data.frame(rich, xy = T)
biomdf_noNA <- na.omit(biomdf) %>%
  dplyr::rename(z = 3)
biomdf_noNA <- mutate(biomdf_noNA, z = 1:nrow(biomdf_noNA))
bio_2 <- raster::rasterFromXYZ(biomdf_noNA)

e <- raster::extract(bio_2, sp_df)
sp_df$cell_id <- e

tbl_fread2 <- base::as.data.frame(sp_df)
sdm_nod2 <- na.omit(tbl_fread2)

sdm_nod2_reg <- left_join(sdm_nod2, biomdf_noNA, by = c("cell_id" = "z"))

nod_sum <- sdm_nod2_reg %>% 
  group_by(x, y) %>% 
  summarise(prop_nod = sum(nodulation == "Yes", na.rm = T) / n(), 
            richness = n())

nod_sum_sf <- st_as_sf(nod_sum, coords = c("x","y"),
                       crs = "+proj=moll +lon_0=1 +datum=WGS84 +units=m +no_defs")

nod_sum_sf <- st_transform(nod_sum_sf, crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")

cc <- st_coordinates(nod_sum_sf)

nod_sum <- ungroup(nod_sum) %>% 
  mutate(long = cc[,1],
         lat = cc[,2])

nod_sum <- nod_sum %>% 
  dplyr::select(long, lat, prop_nod, richness) %>% 
  dplyr::rename(x = long, y = lat)

write.csv(nod_sum, file = "Mimosoid_CSVs_ToShare_WGS84/propNodulating_20km.csv", row.names = F)
