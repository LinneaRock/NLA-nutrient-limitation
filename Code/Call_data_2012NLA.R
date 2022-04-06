# Organizing data  - NLA 2012

library(tidyverse)
library(biogas) # gets molar masses 


# unique site id crosswalk
uniques <- readxl::read_xlsx('Data/raw_data/crosswalk_ID.xlsx', sheet = 'LONG_NARS_ALLSurvey_SITE_ID_CRO')
uniques1 <- uniques |>
  filter(SURVEY %in% c('NLA', 'NRSA')) |>
  select(UNIQUE_ID, SITE_ID)

# call in relevant datasets
nla2012_wq <- read.csv("C:/Users/linne/OneDrive/Desktop/nla2012_waterchem_wide.csv") 
nla2012_siteinfo <- read.csv('Data/raw_data/nla2012_alldata/NLA2012_wide_siteinfo_08232016.csv')
nla2012_condition <- read.csv('Data/raw_data/nla2012_alldata/nla_2012_condition_categories.csv')


# filter for N & P
nla2012_wq1 <- nla2012_wq |>
  select(UID, NITRATE_N_UNITS, NITRATE_N_RESULT, NTL_UNITS, NTL_RESULT, PTL_UNITS, PTL_RESULT) |> # filter for TP, TN, nitrate
  rename(NO3N_PPM = NITRATE_N_RESULT,
         NTL_PPM = NTL_RESULT,
         PTL_PPB = PTL_RESULT,
         VISIT_ID = UID) |>
  select(-contains("UNITS")) 


# filter for useful site informaiton
nla2012_siteinfo1 <- nla2012_siteinfo |>
  select(SITE_ID, UID, VISIT_NO, DATE_COL, SITETYPE, LON_DD83, LAT_DD83, EPA_REG, WGT_ALL, URBAN, LAKE_ORIGIN, AREA_HA, ELEVATION, HUC8) |>
  left_join(uniques1) |> # join with uniques dataset
  rename(VISIT_ID = UID,
         SITE_TYPE = SITETYPE,
         LON_DD = LON_DD83,
         LAT_DD = LAT_DD83,
         WGT_NLA = WGT_ALL,
         ELEV_PT = ELEVATION,
         HUC_8 = HUC8) |>
  filter(!is.na(VISIT_ID)) |>
  mutate(UNIQUE_ID = ifelse(is.na(UNIQUE_ID), SITE_ID, UNIQUE_ID))


# filter for condition information
nla2012_condition1 <- nla2012_condition |>
  select(SITE_ID, UID, AGGR_ECO9_2015, CHLA_COND, NTL_COND, PTL_COND, TROPHIC_STATE) |>
  rename(VISIT_ID = UID)


nla2012 <- left_join(nla2012_wq1, nla2012_siteinfo1) |>
  select(-NO3N_PPM)

nla2012 <- left_join(nla2012, nla2012_condition1)


#add column for molar TN:TP, TN, TP
Pmol <- molMass("P") * 1000000 #ug/mol 
Nmol <- molMass("N") * 1000 #mg/mol

nla2012.tntp <- nla2012 |>
  mutate(tn.tp = (NTL_PPM/Nmol)/(PTL_PPB/Pmol),
         TN_mol = (NTL_PPM)/Nmol,
         TP_mol = (PTL_PPB)/Pmol)


write.csv(nla2012.tntp, "Data/NLA_2012.csv")

