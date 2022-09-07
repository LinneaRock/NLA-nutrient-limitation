
library(tidyverse)
library(colorblindr)

source("Data/NLA/Call_NLA_data.R")
nla_data_subset <- all_NLA |>
  select(ECO_REG_NAME, UNIQUE_ID, DATE_COL, VISIT_NO, NTL_PPM, PTL_PPB, DIN_PPM, tn.tp, DIN.TP, CHLA_PPB, TROPHIC_STATE, year,WGT_NLA, LON_DD, LAT_DD, AREA_HA, ELEV_PT, PCT_DEVELOPED_BSN, PCT_AGRIC_BSN, SITE_TYPE, URBAN, LAKE_ORIGIN) |>
  rename(DIN.TP_molar = DIN.TP,
         TN.TP_molar = tn.tp)

  
  
#### Calculate limitations ####
# get information about the refernece lakes
ref_np <- nla_data_subset |>
  filter(SITE_TYPE %in% c("REF_Lake", "HAND")) |> # subset of 230 lakes
  group_by(ECO_REG_NAME) |>
  summarise(medianDINP = median(DIN.TP_molar, na.rm = TRUE),
            medianNP = (median(TN.TP_molar)),
            medianTN_PPM = median(NTL_PPM),
            medianTP_PPB = median(PTL_PPB))

# get some information about the entire dataset
averages_np <- all_NLA |>
  group_by(ECO_REG, year) |>
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


# How do the lower 25th percentiles of TN and TP compare the the median  concentrations of TN and TP in the reference lakes? 
t.test(ref_np$medianTN_PPM, averages_np$percentile25TN_PPM) # these are similar to each other!! 
t.test(ref_np$medianTP_PPB, averages_np$percentile25TP_PPB) # these are similar to each other!!


# uses 25th percentile nutrient thresholds for each ecoregion (over all years) and logged average N:P for each ecoregion (over each year)

limits <- all_NLA |>
  left_join(averages_np) |>
  mutate(limitation = NA) |>
  mutate(limitation = ifelse(PTL_PPB > percentile25TP_PPB & log(tn.tp) < yearlymeanlogNP, "Potential N-limitation", 
                             ifelse(NTL_PPM > percentile25TN_PPM & log(tn.tp) > yearlymeanlogNP, "Potential P-limitation",
                                    ifelse(is.na(limitation), "Potential co-nutrient limitation", limitation))))

nrow(limits |> filter(limitation == "Potential P-limitation")) # 1299 (1285 when using overal mean N:P)
nrow(limits |> filter(limitation == "Potential N-limitation")) # 1703 (1727 when using overal mean N:P)
nrow(limits |> filter(limitation == "Potential co-nutrient limitation")) # 651 (641 when using overal mean N:P)



ggplot(limits) +
  geom_point(aes(log(PTL_PPB, base = 10), log(NTL_PPM, base = 10), fill = limitation), size = 2.5, shape = 21, alpha = 0.8) +
  theme_minimal() +
  scale_fill_manual("",values = palette_OkabeIto[5:7]) +
  labs(y = "Log TN"~(m*g~L^-1), x = "Log TP"~(mu*g~L^-1))
ggsave("Figures/Qlim.Figs/FINAL_limitationsmethod.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

