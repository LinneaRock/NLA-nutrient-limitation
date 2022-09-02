

source("Data/NLA/Call_NLA_data.R")
library(spsurvey)

#### When a lake is classified as eutrophic or hypereutrophic by TP standards, what percentage of chlorophyll-a considered eutrophic/hypereutrophic? ####

TP_eutro_hyper <- all_NLA |>
  filter(TSTATE_TP %in% c("EUTROPHIC (25-100 ug/L)", "HYPEREUTROPHIC (> 100 ug/L)")) |>
  filter(VISIT_NO == 1) |># for this analysis, we are using just the first visit from each lake
  filter(WGT_NLA > 0) # the sp survey package is not designed to use the reference lakes, so those are ignored when using this package for analyses.


# use categorical variable analysis

national_tp <- cat_analysis(
  TP_eutro_hyper,
  siteID = "UNIQUE_ID",
  vars = "TROPHIC_STATE",
  weight = "WGT_NLA", 
  xcoord = "LON_DD", 
  ycoord = "LAT_DD"
)

ecoregion_tp <- cat_analysis(
  TP_eutro_hyper,
  siteID = "UNIQUE_ID",
  vars = "TROPHIC_STATE",
  weight = "WGT_NLA", 
  xcoord = "LON_DD", 
  ycoord = "LAT_DD",
  subpop = "ECO_REG_NAME"
)

tp_perc_chla <- rbind(national_tp, ecoregion_tp) |>
  mutate(Subpopulation = ifelse(Subpopulation == "All Sites", "National", Subpopulation))

tp_perc_chla$Subpopulation = factor(tp_perc_chla$Subpopulation,
                              levels = c("National","Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))



#### When a lake is classified as eutrophic or hypereutrophic by TN standards, what percentage of chlorophyll-a considered eutrophic/hypereutrophic? ####

TN_eutro_hyper <- all_NLA |>
  filter(TSTATE_TN %in% c("EUTROPHIC (0.75-1.4 mg/L)", "HYPEREUTROPHIC (> 1.4 mg/L)")) |>
  filter(VISIT_NO == 1) |># for this analysis, we are using just the first visit from each lake
  filter(WGT_NLA > 0) # the sp survey package is not designed to use the reference lakes, so those are ignored when using this package for analyses.


# use categorical variable analysis

national_tn <- cat_analysis(
  TN_eutro_hyper,
  siteID = "UNIQUE_ID",
  vars = "TROPHIC_STATE",
  weight = "WGT_NLA", 
  xcoord = "LON_DD", 
  ycoord = "LAT_DD"
)

ecoregion_tn <- cat_analysis(
  TN_eutro_hyper,
  siteID = "UNIQUE_ID",
  vars = "TROPHIC_STATE",
  weight = "WGT_NLA", 
  xcoord = "LON_DD", 
  ycoord = "LAT_DD",
  subpop = "ECO_REG_NAME"
)

tn_perc_chla <- rbind(national_tn, ecoregion_tn) |>
  mutate(Subpopulation = ifelse(Subpopulation == "All Sites", "National", Subpopulation))

tn_perc_chla$Subpopulation = factor(tn_perc_chla$Subpopulation,
                                    levels = c("National","Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))
