library(raster)
library(dplyr)

t <- read.csv("Astragalus/Astragalus_50km_csvsToShare/PD_P.csv")
r <- rasterFromXYZ(t)
plot(r)

f <- function(x, fn){
  
  df <- read.csv(x)
  r <- rasterFromXYZ(df)
  
  raster::writeRaster(r, filename = fn)
  
}

#pdp
f("Astragalus/Astragalus_50km_csvsToShare/PD_P.csv", "Astragalus/Astragalus_50kmtifsToShare/PD_P.tif")
t <- raster("Astragalus/Astragalus_50kmtifsToShare/PD_P.tif")
plot(t)

#pe_cwe
f("Astragalus/Astragalus_50km_csvsToShare/PE_CWE.csv", "Astragalus/Astragalus_50kmtifsToShare/PE_CWE.tif")

f("Astragalus/Astragalus_50km_csvsToShare/richness.csv", "Astragalus/Astragalus_50kmtifsToShare/richness.tif")
f("Astragalus/Astragalus_50km_csvsToShare/RPD.csv", "Astragalus/Astragalus_50kmtifsToShare/RPD.tif")

t <- read.csv("Astragalus/Astragalus_50km_csvsToShare/rand_PD_P.csv") %>% 
  dplyr::select(x,y,Significance)
r <- rasterFromXYZ(t)
plot(r)
r <- raster("Astragalus/outputs50km/rand_PD_P.tif")
plot(r)

c <- read.csv("Astragalus/Astragalus_50km_csvsToShare/CANAPE.csv")
head(c)

f("Astragalus/Astragalus_50km_csvsToShare/CANAPE.csv", "Astragalus/Astragalus_50kmtifsToShare/CANAPE.tif")


r <- terra::rast("Astragalus/Astragalus_50km_tifsToShare/PD_P.tif")
terra::plot(r)



df <- read.csv("Astragalus/Astragalus_50km_csvsToShare/CANAPE.csv")
df2 <- mutate(df, 
              CANAPE = case_when(
                Significance == "Not sig" ~ 0,
                Significance == "Mixed" ~ 1,
                Significance == "Paleo" ~ 2,
                Significance == "Neo" ~ 3
              ))
r <- rasterFromXYZ(dplyr::select(df2, x,y,CANAPE))
plot(r)
raster::writeRaster(r, filename ="Astragalus/Astragalus_50km_tifsToShare/CANAPE.tif", overwrite = T)

