library(dplyr)
library(ggplot2)
library(raster)


## generating csvs of x,y,metric to share with Ryan Folk

## write a funciton to make this easy

tif_to_csv <- function(x, name){
  
  r <- raster::raster(x)
  r_csv <- raster::as.data.frame(r,xy = T) %>% 
    na.omit() %>% 
    dplyr::rename(value = 3)
  
  write.csv(r_csv,
            paste("Fagales_CSVs_ToShare/",
                  name, "_50km.csv", sep = ""),
            row.names = F)
  
}


# start with proportional PD
tif_to_csv(x = "Biodiverse_Outputs/res50km_PD_P.tif", name = "PD_P")

# rand PDP
tif_to_csv("Randomization_Outputs/rand_50km_PD_P.tif", name = "rand_PD_P")

# RPD
tif_to_csv(x = "Biodiverse_Outputs/res15km_PHYLO_RPD2.tif", name = "RPD")

#rand RPD
tif_to_csv(x = "Randomization_Outputs/rand_50km_PHYLO_RPD2.tif", name = "rand_RPD")

# PE_CWE
tif_to_csv(x = "Biodiverse_Outputs/res50km_PE_CWE.tif", name = "PE_CWE")

#RPE
tif_to_csv(x = "Biodiverse_Outputs/res50km_PHYLO_RPE2.tif", name = "RPE")

#PMPD
tif_to_csv(x = "Biodiverse_Outputs/res50km_PMPD2_MEAN.tif", name = "PMPD")


# canape
xx <- raster("Randomization_Outputs/rand_50km_PE_WE_P.tif")
xx_df <- raster::as.data.frame(xx, xy = TRUE) %>% 
  dplyr::rename(PE_WE_P = 3)

yy <- raster("Randomization_Outputs/rand_50km_PHYLO_RPE_NULL2.tif")
yy_df <- raster::as.data.frame(yy, xy = TRUE) %>% 
  dplyr::rename(RPE_NULL2 = 3)

zz <- raster("Randomization_Outputs/rand_50km_PHYLO_RPE2.tif")
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

write.csv(canape_csv, "Fagales_CSVs_ToShare/CANAPE.csv", row.names = F)


world <- rnaturalearth::ne_countries(returnclass = "sf")

ggplot() + 
  geom_sf(world, mapping = aes(), fill = NA, color = "grey25") +
  geom_tile(canape_csv, mapping = aes(x = x, y = y, fill = Significance)) + 
  scale_fill_manual(values = c("#CB7FFF", "red", "transparent", "royalblue1")) +
  theme_void()

## prop nodulating
# read in nodulating data
world <- rnaturalearth::ne_countries(returnclass = "sf")

nod <- data.table::fread("data/Fagales_nodulation.tsv", header = T)

library(stringr)

# read in sdm data
sdm <- data.table::fread("data/SDM_Results/Fagales_ModelResults_AsCSV_50km.csv") %>% 
  mutate(Genus = word(binomial, start = 1, sep = fixed("_")))

sdm_nod <- left_join(sdm, nod)

nod_sum <- sdm_nod %>% 
  group_by(x,y) %>% 
  summarise(prop_nod = sum(Nodulator == "Yes", na.rm = T) / n())

## write proportional noduclating csv
head(nod_sum)

write.csv(nod_sum, "Fagales_CSVs_ToShare/proportion_nodulating.csv", row.names = F)

ggplot() +
  geom_tile(nod_sum, mapping = aes(x = x, y = y, fill = prop_nod)) +
  scale_fill_viridis_c() +
  theme_classic()

