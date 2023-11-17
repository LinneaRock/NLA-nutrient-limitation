library(tidyverse)
library(sf)



### calculate limits in NARS_challenge_data_analysis.R first ### 


# Create map ####
regions.sf <- read_sf("Data/aggr_ecoregions_2015/Aggr_Ecoregions_2015.shp")
nla_locations.sf <- st_as_sf(limits, coords = c("LON_DD", "LAT_DD"), crs = 4326)
muted <- c("#CC6677", "#332288", "#DDCC77", "#117733", "#88CCEE", "#882255", "#44AA99", "#999933", "#AA4499" ) # ecoreg color scheme


ggplot() +
  geom_sf(data = regions.sf, aes(fill = WSA9_NAME), alpha = 0.15) +
  geom_sf(data = nla_locations.sf |> filter(year == '2017', limitation %in% c("N-limitation","P-limitation")), aes(color = limitation), size = 0.25) +
  geom_sf(data = nla_locations.sf |> filter(year == '2017', limitation %in% c("Co-nutrient limitation")), aes(color = limitation), size = 0.25) +
  theme_minimal() +
  scale_fill_manual("", values = muted) +
  scale_color_manual("", values = c("grey60","red4", "#336a98"), labels = c('Co-nutrient \nlimitation', 'N-limitation', 'P-limitation'))  +
  guides(fill = 'none') +
  theme(legend.position = c(1, 0.4), 
        legend.key.size = unit(0.05, 'cm'), #change legend key size
        legend.key.height = unit(0.05, 'cm'), #change legend key height
        legend.key.width = unit(0.05, 'cm'), #change legend key width
        legend.text = element_text(size=6), #change legend text font size
        axis.text = element_text(size = 6),
        legend.title = element_blank())

ggsave("Figures/AbstractArt_map.png", height = 1.75, width = 3.25, units = "in", dpi = 500)




