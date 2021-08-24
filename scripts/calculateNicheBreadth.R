library(tidyverse)
library(raster)

# read in sdms
sdm <- data.table::fread("data/SDM_Results/Fagales_ModelResults_AsCSV_50km.csv")

# read in raster data
rasters <- list.files(path = "data/allVars", full.names = T)
s <- lapply(rasters, stack)

test_spp <- filter(sdm, binomial == "Alfaroa_colombiana")
coordinates(test_spp) <- ~ x + y

ff <- function(x){
  
  e <- extract(s[[x]], test_spp)
  return(e)
}

ee <- lapply(1:length(s), ff)

edf <- data.frame(matrix(unlist(ee), nrow = length(ee), byrow = T)) %>% 
  dplyr::rename(bio1 = 1, bio12 = 2, bio13 = 3, bio14 = 4, bio15 = 5, bio16 = 6,
                bio17 = 7, bio2 = 8, bio4 = 9, bio5 = 10, bio6 = 11, bio8 = 12,
                bio9 = 13, elevation = 14, nitrogen = 15, ruggedness = 16, sand = 17,
                soc = 18)




coordinates(sdm) <- ~ x + y

ff <- function(x){
  
  e <- extract(s[[x]], sdm)
  return(e)
}

ee <- lapply(1:length(s), ff)

edf <- data.frame(matrix(unlist(ee), nrow = 195394, byrow = T)) %>% 
  dplyr::rename(bio1 = 1, bio12 = 2, bio13 = 3, bio14 = 4, bio15 = 5, bio16 = 6,
                bio17 = 7, bio2 = 8, bio4 = 9, bio5 = 10, bio6 = 11, bio8 = 12,
                bio9 = 13, elevation = 14, nitrogen = 15, ruggedness = 16, sand = 17,
                soc = 18)

# bring back regular sdm results
sdm <- data.table::fread("data/SDM_Results/Fagales_ModelResults_AsCSV_50km.csv")


sdm_vars <- cbind(sdm, edf)

sdm_vars <- sdm_vars %>% 
  mutate(bio1 = scale(bio1), bio12 = scale(bio12), bio13 = scale(bio13),
         bio14 = scale(bio14), bio15 = scale(bio15), bio16 = scale(bio16),
         bio17 = scale(bio17), bio2 = scale(bio2), bio4 = scale(bio4), bio5 = scale(bio5), 
         bio6 = scale(bio6), bio8 = scale(bio8), bio9 = scale(bio9)) %>% 
  dplyr::select(-elevation, -nitrogen, -ruggedness, -sand, - soc)

qs <- sdm_vars %>% 
  group_by(binomial) %>% 
  summarise(bio1_q5 =    quantile(bio1, probs = 0.05,na.rm = T),   
            bio12_q5 =  quantile(bio12, probs = 0.05,na.rm = T), 
            bio13_q5 =  quantile(bio13, probs = 0.05,na.rm = T),
            bio14_q5 =  quantile(bio14, probs = 0.05,na.rm = T), 
            bio15_q5 =  quantile(bio15, probs = 0.05,na.rm = T), 
            bio16_q5 =  quantile(bio16, probs = 0.05,na.rm = T),
            bio17_q5 =  quantile(bio17, probs = 0.05,na.rm = T), 
            bio2_q5 =   quantile(bio2, probs = 0.05, na.rm = T), 
            bio4_q5 =   quantile(bio4, probs = 0.05, na.rm = T), 
            bio5_q5 =   quantile(bio5, probs = 0.05, na.rm = T), 
            bio6_q5 =   quantile(bio6, probs = 0.05, na.rm = T),   
            bio8_q5 =   quantile(bio8, probs = 0.05, na.rm = T), 
            bio9_q5 =   quantile(bio9, probs = 0.05, na.rm = T),
            bio1_q95 =  quantile(bio1, probs = 0.95, na.rm = T),   
            bio12_q95 = quantile(bio12, probs = 0.95, na.rm = T), 
            bio13_q95 = quantile(bio13, probs = 0.95, na.rm = T),
            bio14_q95 = quantile(bio14, probs = 0.95, na.rm = T), 
            bio15_q95 = quantile(bio15, probs = 0.95, na.rm = T), 
            bio16_q95 = quantile(bio16, probs = 0.95, na.rm = T),
            bio17_q95 = quantile(bio17, probs = 0.95, na.rm = T), 
            bio2_q95 =   quantile(bio2, probs = 0.95, na.rm = T), 
            bio4_q95 =   quantile(bio4, probs = 0.95, na.rm = T), 
            bio5_q95 =   quantile(bio5, probs = 0.95, na.rm = T), 
            bio6_q95 =   quantile(bio6, probs = 0.95, na.rm = T),   
            bio8_q95 =   quantile(bio8, probs = 0.95, na.rm = T), 
            bio9_q95 =   quantile(bio9, probs = 0.95, na.rm = T))

qs2 <- qs %>% 
  replace_na(list(bio1_q5 =  0,   
            bio12_q5 =  0,
            bio13_q5 =  0,
            bio14_q5 =  0,
            bio15_q5 =  0,
            bio16_q5 =  0,
            bio17_q5 =  0,
            bio2_q5 =   0,
            bio4_q5 =   0,
            bio5_q5 =   0,
            bio6_q5 =   0,  
            bio8_q5 =   0,
            bio9_q5 =   0,
            bio1_q95 =  1,  
            bio12_q95 = 1, 
            bio13_q95 = 1,
            bio14_q95 = 1, 
            bio15_q95 = 1, 
            bio16_q95 = 1,
            bio17_q95 = 1, 
            bio2_q95 =  1, 
            bio4_q95 =  1, 
            bio5_q95 =  1, 
            bio6_q95 =  1,   
            bio8_q95 =  1, 
            bio9_q95 =  1))

niche_breadth <- qs2 %>% 
  group_by(binomial) %>% 
  summarise(niche_breadth = (bio1_q95 - bio1_q5) *
                             (bio13_q95- bio13_q5) *
                             (bio14_q95- bio14_q5) *
                             (bio12_q95- bio12_q5) * 
                             (bio15_q95- bio15_q5) *
                             (bio16_q95- bio16_q5) *
                             (bio17_q95- bio17_q5) *
                             (bio2_q95 - bio2_q5)  *
                             (bio4_q95 - bio4_q5)  *
                             (bio5_q95 - bio5_q5)  *
                             (bio6_q95 - bio6_q5)  *
                             (bio8_q95 - bio8_q5)  *
                             (bio9_q95 - bio9_q5))

write.csv(x = niche_breadth, file = "niche_breadth.csv", row.names = F)
