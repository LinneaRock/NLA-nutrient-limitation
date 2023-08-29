library(tidyverse)
library(sf)
library(httr)

# ecoregions
regions.sf <- read_sf("Data/aggr_ecoregions_2015/Aggr_Ecoregions_2015.shp")


# 1. GET RIVERS DATA
#---------

get_data <- function() {
  
  url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_na_shp.zip"
  
  res <- GET(url,
             write_disk("na_rivers.zip"),
             progress())
  unzip("na_rivers.zip")
  filenames <- list.files("HydroRIVERS_v10_na_shp", pattern="*.shp", full.names=T)
  
  riv_list <- lapply(filenames, st_read)
  
  return(riv_list)
}

get_data()
filenames <- list.files("HydroRIVERS_v10_na_shp", pattern="*.shp", full.names=T)

load_rivers <- function() {
  list_riv <- lapply(filenames, sf::st_read)
  na_riv <- list_riv[[1]] |>
    sf::st_cast("MULTILINESTRING")
  
  return(na_riv)
}


na_riv <- load_rivers()
head(na_riv)


# change size of lines
get_river_width <- function() {
  na_riv_width <- na_riv |>
    dplyr::mutate(
      width = as.numeric(ORD_FLOW),
      width = dplyr::case_when(
        width == 3 ~ 1,
        width == 4 ~ 0.8,
        width == 5 ~ 0.6,
        width == 6 ~ 0.4,
        width == 7 ~ 0.2,
        width == 8 ~ 0.2,
        width == 9 ~ 0.1,
        width == 10 ~ 0.1,
        TRUE ~ 0
      )
    ) |>
    sf::st_as_sf()
  
  return(na_riv_width)
}

na_riv_width <- get_river_width()




ggplot() +
  geom_sf(data = regions.sf, mapping=aes()) +
  #geom_sf(na_riv_width, mapping=aes(color=factor(ORD_FLOW), size=width, alpha=factor(ORD_FLOW))) +
  coord_sf(
    crs = 4326,
    xlim = c(24.521208, 49.382808),
    ylim = c( 66.945392, 124.736342)
  ) +
  theme_minimal() +
  theme(legend.position = c(1, 0.4), 
        legend.key.size = unit(0.05, 'cm'), #change legend key size
        legend.key.height = unit(0.05, 'cm'), #change legend key height
        legend.key.width = unit(0.05, 'cm'), #change legend key width
        legend.text = element_text(size=6), #change legend text font size
        axis.text = element_text(size = 6),
        legend.title = element_blank())
