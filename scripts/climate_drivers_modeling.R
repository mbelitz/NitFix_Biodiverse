library(raster)
library(MuMIn)
library(dplyr)
library(knitr)
library(car)
library(sf)
library(gt)

# read in response variables
pd_sig <- raster("data/Biodiverse_Outputs_Randomizations/randomization_50km_PD_P.tif")
pd_df <- as.data.frame(pd_sig, xy = T)  %>% 
  dplyr::filter(!is.na(randomization_50km_PD_P))

coordinates(pd_df) <- ~ x + y

## Read in model covariates 
temp <- raster("data/allVars/bio1.tif")
t <- extract(temp, pd_df) 

prec <- raster("data/allVars/bio4.tif")
p <- extract(prec, pd_df) 

tempSeas <- raster("data/allVars/bio12.tif")
ts <- extract(tempSeas, pd_df)

precSeas <- raster("data/allVars/bio15.tif")
ps <- extract(precSeas, pd_df)

soc <- raster("data/allVars/soc.tif")
c <- extract(soc, pd_df)

elev <- raster("data/allVars/elevation.tif")
e <- extract(elev, pd_df)