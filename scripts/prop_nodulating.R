library(tidyverse)

# read in nodulating data
nod <- data.table::fread("data/Fagales_nodulation.tsv", header = T)

# read in sdm data
sdm <- data.table::fread("data/SDM_Results/Fagales_ModelResults_AsCSV_50km.csv") %>% 
  mutate(Genus = word(binomial, start = 1, sep = fixed("_")))

sdm_nod <- left_join(sdm, nod)

nod_sum <- sdm_nod %>% 
  group_by(x,y) %>% 
  summarise(prop_nod = sum(Nodulator == "Yes", na.rm = T) / n())

ggplot() +
  geom_tile(nod_sum, mapping = aes(x = x, y = y, fill = prop_nod)) +
  scale_fill_viridis_c() +
  theme_classic()
