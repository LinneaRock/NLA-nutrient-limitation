

source("Data/NLA/Call_NLA_data.R")
library(spsurvey)

##### CATEGORICAL ASSESSMENT #####
#### When a lake is classified as eutrophic or hypereutrophic by TP standards, what percentage of chlorophyll-a considered eutrophic/hypereutrophic? ####

TP_eutro_hyper <- all_NLA |>
  filter(TSTATE_TP %in% c("EUTROPHIC (25-100 ug/L)", "HYPEREUTROPHIC (> 100 ug/L)")) |>
  filter(VISIT_NO == 1) |># for this analysis, we are using just the first visit from each lake
  filter(WGT_NLA > 0)  # the sp survey package is not designed to use the reference lakes, so those are ignored when using this package for analyses.


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

tp_perc_chla1 <- tp_perc_chla |>
  filter(Category %in% c("Eutro.", "Hyper.")) |>
  group_by(Subpopulation) |>
  summarise(sum(Estimate.P))  |>
  rename(TP_corr = `sum(Estimate.P)`)



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

tn_perc_chla1 <- tn_perc_chla |>
  filter(Category %in% c("Eutro.", "Hyper.")) |>
  group_by(Subpopulation) |>
  summarise(sum(Estimate.P)) |>
  rename(TN_corr = `sum(Estimate.P)`)


# create a pretty table
library(gt)
gt_tbl <- gt(left_join(tp_perc_chla1, tn_perc_chla1))
simpleregtable <- gt_tbl %>%
  cols_label(
    Subpopulation = "Ecoregion",
    TP_corr = "% eutro. lakes with TP > 25 ug/L",
    TN_corr = "% eutro. lakes with TN > 0.75 mg/L"
  ) %>%
  tab_header(
    title = "Percent of eutrophic/hypereutrophic lakes that correspond with high TP or TN concentrations"
  ); simpleregtable
gtsave(simpleregtable, "Figures/QNvsP.Figs/ecoregion_linearmodels_table.png")







#####RISK ASSESSMENT #####
#### When a lakes is classified as eutrophic + for TP, but is less than eutrophic for TN, what is the probability it will be eutrophic + for chlorophyll-a? ####

TP_eutro_hyper_risk <- all_NLA |>
  filter(!(TSTATE_TN %in% c("EUTROPHIC (0.75-1.4 mg/L)", "HYPEREUTROPHIC (> 1.4 mg/L)"))) |> # get rid of any lakes that are eutrophic+ for TN
  mutate(TP_risk_state = ifelse(TSTATE_TP %in% c("EUTROPHIC (25-100 ug/L)", "HYPEREUTROPHIC (> 100 ug/L)"), "Poor", "Good")) |> # risk analysis needs these in terms of poor/good, same for next line
  mutate(tstate_risk = ifelse(TROPHIC_STATE %in% c("Oligo.", "Meso."), "Good", "Poor")) |>
  filter(VISIT_NO == 1) |># for this analysis, we are using just the first visit from each lake
  filter(WGT_NLA > 0)  # the sp survey package is not designed to use the reference lakes, so those are ignored when using this package for analyses.

# relative risk - how likely is the lake to be eutrophic+ (by chla) when TP is eutrophic+ but TN is not (compared to when TP is in good condition)?
relrisk_TP_nat <- relrisk_analysis(
  TP_eutro_hyper_risk, 
  siteID = "UNIQUE_ID",
  vars_response = "tstate_risk",
  vars_stressor = "TP_risk_state",
  weight = "WGT_NLA", 
  xcoord = "LON_DD", 
  ycoord = "LAT_DD"
)

relrisk_TP_eco <- relrisk_analysis(
  TP_eutro_hyper_risk, 
  siteID = "UNIQUE_ID",
  vars_response = "tstate_risk",
  vars_stressor = "TP_risk_state",
  weight = "WGT_NLA", 
  xcoord = "LON_DD", 
  ycoord = "LAT_DD",
  subpop = "ECO_REG_NAME"
)

relrisk_TP <- rbind(relrisk_TP_nat, relrisk_TP_eco) |>
  mutate(Subpopulation = ifelse(Subpopulation == "All Sites", "National", Subpopulation))

# Attributable risk - the higher the attributable risk, the more improvement possible if TP concentrations are reduced
attrisk_TP_nat <- attrisk_analysis(
  TP_eutro_hyper_risk, 
  siteID = "UNIQUE_ID",
  vars_response = "tstate_risk",
  vars_stressor = "TP_risk_state",
  weight = "WGT_NLA", 
  xcoord = "LON_DD", 
  ycoord = "LAT_DD"
)

attrisk_TP_eco <- attrisk_analysis(
  TP_eutro_hyper_risk, 
  siteID = "UNIQUE_ID",
  vars_response = "tstate_risk",
  vars_stressor = "TP_risk_state",
  weight = "WGT_NLA", 
  xcoord = "LON_DD", 
  ycoord = "LAT_DD",
  subpop = "ECO_REG_NAME"
)

attrisk_TP <- rbind(attrisk_TP_nat, attrisk_TP_eco) |>
  mutate(Subpopulation = ifelse(Subpopulation == "All Sites", "National", Subpopulation))




#### When a lakes is classified as eutrophic + for TN, but is less than eutrophic for Tp, what is the probability it will be eutrophic + for chlorophyll-a? ####

TN_eutro_hyper_risk <- all_NLA |>
  filter(!(TSTATE_TP %in% c("EUTROPHIC (25-100 ug/L)", "HYPEREUTROPHIC (> 100 ug/L)"))) |> # get rid of any lakes that are eutrophic+ for TP
  mutate(TN_risk_state = ifelse(TSTATE_TN %in% c("EUTROPHIC (0.75-1.4 mg/L)", "HYPEREUTROPHIC (> 1.4 mg/L)"), "Poor", "Good")) |> # risk analysis needs these in terms of poor/good, same for next line
  mutate(tstate_risk = ifelse(TROPHIC_STATE %in% c("Oligo.", "Meso."), "Good", "Poor")) |>
  filter(VISIT_NO == 1) |># for this analysis, we are using just the first visit from each lake
  filter(WGT_NLA > 0)  # the sp survey package is not designed to use the reference lakes, so those are ignored when using this package for analyses.

# relative risk - how likely is the lake to be eutrophic+ (by chla) when TN is eutrophic+ but TN is not (compared to when TN is in good condition)?
relrisk_TN_nat <- relrisk_analysis(
  TN_eutro_hyper_risk, 
  siteID = "UNIQUE_ID",
  vars_response = "tstate_risk",
  vars_stressor = "TN_risk_state",
  weight = "WGT_NLA", 
  xcoord = "LON_DD", 
  ycoord = "LAT_DD"
)

relrisk_TN_eco <- relrisk_analysis(
  TN_eutro_hyper_risk, 
  siteID = "UNIQUE_ID",
  vars_response = "tstate_risk",
  vars_stressor = "TN_risk_state",
  weight = "WGT_NLA", 
  xcoord = "LON_DD", 
  ycoord = "LAT_DD",
  subpop = "ECO_REG_NAME"
)

relrisk_TN <- rbind(relrisk_TN_nat, relrisk_TN_eco) |>
  mutate(Subpopulation = ifelse(Subpopulation == "All Sites", "National", Subpopulation))


# Attributable risk - the higher the attributable risk, the more improvement possible if TN concentrations are reduced
attrisk_TN_nat <- attrisk_analysis(
  TN_eutro_hyper_risk, 
  siteID = "UNIQUE_ID",
  vars_response = "tstate_risk",
  vars_stressor = "TN_risk_state",
  weight = "WGT_NLA", 
  xcoord = "LON_DD", 
  ycoord = "LAT_DD"
)

attrisk_TN_eco <- attrisk_analysis(
  TN_eutro_hyper_risk, 
  siteID = "UNIQUE_ID",
  vars_response = "tstate_risk",
  vars_stressor = "TN_risk_state",
  weight = "WGT_NLA", 
  xcoord = "LON_DD", 
  ycoord = "LAT_DD",
  subpop = "ECO_REG_NAME"
)

attrisk_TN <- rbind(attrisk_TN_nat, attrisk_TN_eco) |>
  mutate(Subpopulation = ifelse(Subpopulation == "All Sites", "National", Subpopulation))





