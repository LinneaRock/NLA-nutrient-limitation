library(tidyverse)
library(sf)



#### Load and subset dataframe ####
source("Data/NLA/Call_NLA_data.R")
nla_data_subset <- all_NLA |>
  select(ECO_REG_NAME, UNIQUE_ID, DATE_COL, VISIT_NO, NTL_PPM, PTL_PPB, DIN_PPM, tn.tp, DIN.TP, CHLA_PPB, TROPHIC_STATE, year,WGT_NLA, LON_DD, LAT_DD, AREA_HA, ELEV_PT, PCT_DEVELOPED_BSN, PCT_AGRIC_BSN, SITE_TYPE, URBAN, LAKE_ORIGIN) |>
  rename(DIN.TP_molar = DIN.TP,
         TN.TP_molar = tn.tp) |>
  filter(year != "2012") |>
  filter(AREA_HA >= 4) |> # removes 216 observations 
  distinct()


#### Calculate limitations ####
# get information about the refernece lakes
ref_np <- nla_data_subset |>
  filter(SITE_TYPE %in% c("REF_Lake", "HAND")) |> # subset of 230 lakes
  group_by(ECO_REG_NAME, year) |>
  # 75th percentile of nutrient concentrations from reference lakes
  summarise(percentile75TN_PPM = quantile(NTL_PPM, probs = 0.75), 
            percentile75TP_PPB = quantile(PTL_PPB, probs = 0.75),
            percentile75DIN_PPM = quantile(DIN_PPM, probs = 0.75)) |>
  ungroup()

# get some information about the entire dataset
averages_np <- nla_data_subset |>
  filter(is.finite(log(DIN.TP_molar))) |>
  group_by(ECO_REG_NAME, year) |>
  # 25th percentile of nutrient concentrations from total assessed lakes and mean ratios
  summarise(meanlogNP = mean(log(TN.TP_molar)),
            meanlogDINP = mean(log(DIN.TP_molar), na.rm = TRUE),
            medianlogDINP = median(log(DIN.TP_molar), na.rm = TRUE),
            meanNP = mean((TN.TP_molar)),
            meanDINP = mean((DIN.TP_molar), na.rm = TRUE),
            medianDINP = median(DIN.TP_molar, na.rm = TRUE),
            percentile25TN_PPM = quantile(NTL_PPM, probs = 0.25),
            percentile25TP_PPB = quantile(PTL_PPB, probs = 0.25),
            percentile25DIN_PPM = quantile(DIN_PPM, probs = 0.25)) |>
  ungroup() |>
  # select(ECO_REG_NAME, meanlogNP, meanlogDINP, percentile25TN_PPM, percentile25TP_PPB, percentile25DIN_PPM) |>
  distinct()

criteria <- left_join(averages_np, ref_np) |>
  group_by(ECO_REG_NAME, year) |>
  mutate(TP_threshold = median(c(percentile75TP_PPB, percentile25TP_PPB)),
         TN_threshold = median(c(percentile75TN_PPM, percentile25TN_PPM)),
         DIN_threshold = median(c(percentile75DIN_PPM, percentile25DIN_PPM))) |>
  # There were no reference lakes in the Northern Plains in 2007. So, concentration thresholds were determined solely by the 25th percentile of all assessed lakes in that region in that year. 
  mutate(TP_threshold = ifelse(is.na(TP_threshold), percentile25TP_PPB, TP_threshold),
         TN_threshold = ifelse(is.na(TN_threshold), percentile25TN_PPM, TN_threshold),
         DIN_threshold = ifelse(is.na(DIN_threshold), percentile25DIN_PPM, DIN_threshold))

# generate nutrient limitations
limits <- nla_data_subset|>
  filter(is.finite(log(DIN.TP_molar))) |>
  filter(!is.na(DIN_PPM)) |> # 2371 observations, loss of 74 observations from analysis
  left_join(criteria) |>
  mutate(limitation = NA) |>
  mutate(limitation = ifelse(PTL_PPB > TP_threshold & log(DIN.TP_molar) < medianlogDINP, "N-limitation", 
                             ifelse(DIN_PPM > DIN_threshold & log(DIN.TP_molar) > medianlogDINP, "P-limitation",
                                    ifelse(is.na(limitation), "Co-nutrient limitation", limitation))))


# Create map ####
regions.sf <- read_sf("Data/aggr_ecoregions_2015/Aggr_Ecoregions_2015.shp")
nla_locations.sf <- st_as_sf(limits, coords = c("LON_DD", "LAT_DD"), crs = 4326)
muted <- c("#CC6677", "#332288", "#DDCC77", "#117733", "#88CCEE", "#882255", "#44AA99", "#999933", "#AA4499" ) # ecoreg color scheme


ggplot() +
  geom_sf(data = regions.sf, aes(fill = WSA9_NAME), alpha = 0.25) +
  geom_sf(data = nla_locations.sf |> filter(year == '2017', !is.na(limitation)), aes(color = limitation)) +
  theme_minimal() +
  scale_fill_manual("", values = muted) +
  scale_color_manual("", values = c("grey60","red4", "#336a98")) 

ggsave("Figures/AbstractArt_map.png", height = 4.5, width = 6.5, units = "in", dpi = 1200)



