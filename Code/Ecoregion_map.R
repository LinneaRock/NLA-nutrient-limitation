#### Aggregated ecoregion map ####

library(sf)
library(tidyverse)
library(colorblindr)
source("C:/Users/lrock1/Downloads/PhD_code/STOICH_NARSchallenge/Data/NLA/Call_NLA_data.R")

regions.sf <- read_sf("Data/aggr_ecoregions_2015/Aggr_Ecoregions_2015.shp")
nla_locations.sf <- st_as_sf(all_NLA, coords = c("LON_DD", "LAT_DD"), crs = 4326)

ggplot() +
  geom_sf(data = regions.sf, aes(fill = WSA9_NAME), alpha = 0.25) +
  geom_sf(data = nla_locations.sf |> filter(year == 2007, !is.na(TSTATE_CHL)), aes(color = TSTATE_CHL), size = 2) +
  theme_minimal() +
  scale_fill_manual("", values = c(palette_OkabeIto_black[], '#FFFFFF')) +
  scale_color_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]),
                     labels = c("Olig.", "Meso.", "Eutro.", "Hyper.")) 
