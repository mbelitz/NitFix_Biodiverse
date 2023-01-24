library(dplyr)
library(ggplot2)
library(raster)
world <- rnaturalearth::ne_countries(returnclass = "sf")

## generating csvs of x,y,metric to share with Ryan Folk

## write a funciton to make this easy

tif_to_csv <- function(x, name){
  
  r <- raster::raster(x)
  r_csv <- raster::as.data.frame(r,xy = T) %>% 
    na.omit() %>% 
    dplyr::rename(value = 3)
  
  write.csv(r_csv,
            paste("Astragalus_CSVs_ToShare_15km//",
                  name, "_50km.csv", sep = ""),
            row.names = F)
  
}


# start with richness
tif_to_csv(x = "Astragalus/outputs/Astragalus_richness.tif", name = "richness")

# now proportional PD
tif_to_csv(x = "Astragalus/outputs/spatial_PD_P.tif", name = "PD_P")

# rand PDP
tif_to_csv("Astragalus/outputs/rand_PD_P.tif", name = "rand_PD_P")

# RPD
tif_to_csv(x = "Astragalus/outputs/spatial_PHYLO_RPD2.tif", name = "RPD")

#rand RPD
tif_to_csv(x = "Astragalus/outputs/rand_PHYLO_RPD2.tif", name = "rand_RPD")

# PE_CWE
tif_to_csv(x = "Astragalus/outputs/spatial_PE_CWE.tif", name = "PE_CWE")

#RPE
tif_to_csv(x = "Astragalus/outputs/spatial_PHYLO_RPE2.tif", name = "RPE")


# canape
xx <- raster("Astragalus/outputs/rand_PE_WE_P.tif")
xx_df <- raster::as.data.frame(xx, xy = TRUE) %>% 
  dplyr::rename(PE_WE_P = 3)

yy <- raster("Astragalus/outputs/rand_PHYLO_RPE_NULL2.tif")
yy_df <- raster::as.data.frame(yy, xy = TRUE) %>% 
  dplyr::rename(RPE_NULL2 = 3)

zz <- raster("Astragalus/outputs/rand_PHYLO_RPE2.tif")
zz_df <- raster::as.data.frame(zz, xy = TRUE) %>% 
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

head(canape_csv)

write.csv(canape_csv, "Astragalus_CSVs_ToShare_15km/CANAPE.csv", row.names = F)
