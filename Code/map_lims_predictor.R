
# maps with pie charts of limited lakes

# call datasets and libraries 
source("Data/NLA/Call_NLA_data.R")
library(colorblindr)
library(sf)
library(patchwork)
regions.sf <- read_sf("Data/aggr_ecoregions_2015/Aggr_Ecoregions_2015.shp")
nla_locations.sf <- st_as_sf(limits, coords = c("LON_DD", "LAT_DD"), crs = 4326)



#### Calculate the limitations first ####
# get some information about the entire dataset
averages_np <- all_NLA |>
  group_by(ECO_REG, year) |>
  mutate(yearlymeanlogNP = mean(log(tn.tp))) |>
  ungroup() |>
  group_by(ECO_REG_NAME) |>
  mutate(
    meanlogNP = mean(log(tn.tp)),
    medianlogNP = median(log(tn.tp)),
    medianTN_PPM = median(NTL_PPM),
    medianTP_PPB = median(PTL_PPB),
    percentile25TN_PPM = quantile(NTL_PPM, probs = 0.25),
    percentile25TP_PPB = quantile(PTL_PPB, probs = 0.25)) |>
  ungroup() |>
  select(year, ECO_REG_NAME, meanlogNP, yearlymeanlogNP, percentile25TN_PPM, percentile25TP_PPB) |>
  distinct()

# uses 25th percentile nutrient thresholds for each ecoregion (over all years) and logged average N:P for each ecoregion (over each year)

limits <- all_NLA |>
  left_join(averages_np) |>
  mutate(limitation = NA) |>
  mutate(limitation = ifelse(PTL_PPB > percentile25TP_PPB & log(tn.tp) < yearlymeanlogNP, "Potential N-limitation", 
                             ifelse(NTL_PPM > percentile25TN_PPM & log(tn.tp) > yearlymeanlogNP, "Potential P-limitation",
                                    ifelse(is.na(limitation), "Potential co-nutrient limitation", limitation))))


#### prep the spatial data ####
WSA9_NAME <- as.vector(all_NLA |> select(ECO_REG_NAME) |> distinct())[["ECO_REG_NAME"]]
best_predictor <- as.vector(c("TN", "TP", "TP", "TP", "TP", "TN", "TN", "TN", "TP"))
add <- cbind(data.frame(WSA9_NAME), data.frame(best_predictor))

regions.sf1 <- regions.sf |>
  mutate(WSA9_NAME = ifelse(WSA9_NAME == "Temporate Plains", "Temperate Plains", WSA9_NAME)) |>
  left_join(add) 


final2 <- final1 |>
  filter(Category != "Total") |>
  rename(WSA9_NAME = Subpopulation) |>
  left_join(regions.sf1) 

# get the centroids of the ecoregion shapes
centers <- st_as_sf(final2) |>
  group_by(WSA9_NAME) |>
  st_centroid(geometry) %>% 
  # this is the crs from d, which has no EPSG code:
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  # since you want the centroids in a second geometry col:
  st_geometry() |>
  distinct()

centers1 <- data.frame(unlist(centers)) |>
  mutate(geom = rep(c("LON", "LAT"), times = 90)) |>
  distinct() |>
  rename(coord = unlist.centers.) |>
  filter(coord != "NaN") |>
 # mutate(coord = as.character(coord)) |>
  pivot_wider(names_from = geom, values_from = coord) |>
  unnest()

names <- data.frame(final2$WSA9_NAME) |> distinct() |> filter(final2.WSA9_NAME != "National")
  
centers2 <- cbind(centers1, names) |>
  distinct() |>
  rename(WSA9_NAME = final2.WSA9_NAME) 

final3 <- left_join(final2, centers2) |>
  pivot_wider(names_from = Category, values_from = Estimate.P)



#### create the base map ####
library(scatterpie)
base <- ggplot() +
 # geom_sf(data = regions.sf1, aes(fill = best_predictor)) +
  geom_scatterpie(aes(x=LON, y=LAT, group = WSA9_NAME), data = final3 |> filter(year == "2007"), cols = c("Potential N-limitation", "Potential P-limitation", "Potential co-nutrient limitation")) +
  coord_equal()
 # theme_minimal() +
 # scale_fill_manual("Better predictor of trophic state", values=c("#084c61", "#ffc857")) 

#### pie charts of limitations per year ####
ggplot(final1 |> filter(year == "2007",
                              Category != "Total"), aes(x= 1, y = Estimate.P, fill = Category)) +
  facet_wrap(~Subpopulation, ncol = 3) +
  geom_col() +
  coord_polar(theta = 'y') +
  scale_fill_manual("", values = palette_OkabeIto[5:7]) +
  theme_minimal() +
  labs(x = "", y = "", title = "2007") +
  theme(axis.text = element_blank())



ggplot(final1 |> filter(year == "2012",
                        Category != "Total"), aes(x= 1, y = Estimate.P, fill = Category)) +
  facet_wrap(~Subpopulation, ncol = 3) +
  geom_col() +
  coord_polar(theta = 'y') +
  scale_fill_manual("", values = palette_OkabeIto[5:7]) +
  theme_minimal() +
  labs(x = "", y = "", title = "2012") +
  theme(axis.text = element_blank())



ggplot(final1 |> filter(year == "2017",
                        Category != "Total"), aes(x= 1, y = Estimate.P, fill = Category)) +
  facet_wrap(~Subpopulation, ncol = 3) +
  geom_col() +
  coord_polar(theta = 'y') +
  scale_fill_manual("", values = palette_OkabeIto[5:7]) +
  theme_minimal() +
  labs(x = "", y = "", title = "2017") +
  theme(axis.text = element_blank())

library(plyr)
pies_2007 <- dlply(final2 |> filter(year == "2007"), .(WSA9_NAME), function(z)
  ggplot(final1 |> filter(year == "2007"), aes(x= 1, y = Estimate.P, fill = Category)) +
    #facet_wrap(~Subpopulation, ncol = 3) +
    geom_col() +
    coord_polar(theta = 'y') +
    scale_fill_manual("", values = palette_OkabeIto[5:7]) +
    theme_minimal() +
    labs(x = "", y = "", title = "2007") +
    theme(axis.text = element_blank(),
          legend.position = "none")
  )
detach("package:plyr", unload=TRUE) # plyr causes so much trouble...

n <- length(pies_2007)
for(i in 1:n) {
  nms <- names(pies_2007)[i]
}



