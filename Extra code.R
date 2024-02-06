library(tidyverse)
library(sf)
library(rmapshaper)
library(corrplot)
library(spdep)

# Local Government Area 2016 shape file, quality reduced for faster plotting.
LGA_SHP_Simple <- read_sf("Data/Shape Files/Simplified LGA","rLGA") %>%
  filter(!st_is_empty(.)) %>%
  filter(STE_CODE16 != "9")
lga_adjacency <- poly2nb(LGA_SHP_Simple)
adj_matrix <- nb2mat(lga_adjacency, style="B", zero.policy=TRUE)

