library(raster)
library(sf)
library(stringr)
library(dplyr)
library(ggplot2)

df <- read.csv("Clusters/cluster_r2.csv")

df2 <- dplyr::select(df, -X)

m <- as.matrix(df2)
d <- as.dist(m)

h <- hclust(d)

ct <- cutree(h, 9)

ct_df <- data.frame(cutree(h,9))

ct_df <- tibble::rownames_to_column(ct_df, "cell")

lon <- as.numeric(word(df$X, start = 1, sep = fixed(":")))  
lat <- as.numeric(word(df$X, start = 2, sep = fixed(":")))  

cluster_df <- ct_df %>% 
  mutate(x = lon,
         y = lat) %>% 
  dplyr::rename(cluster = 2)

cluster_df$cluster <- as.character(cluster_df$cluster)

ggplot() +
  geom_tile(cluster_df, mapping = aes(x = x, y = y, fill = cluster)) +
  scale_fill_brewer(palette = "Set1") +
  theme_classic()
