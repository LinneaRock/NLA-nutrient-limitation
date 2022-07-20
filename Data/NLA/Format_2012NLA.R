# Organizing data  - NLA 2012

library(tidyverse)
library(biogas) # gets molar masses 


# unique site id crosswalk
uniques <- readxl::read_xlsx('C:/Users/lrock1/OneDrive/Desktop/raw_data/crosswalk_ID.xlsx', sheet = 'LONG_NARS_ALLSurvey_SITE_ID_CRO')
uniques1 <- uniques |>
  filter(SURVEY %in% c('NLA', 'NRSA')) |>
  select(UNIQUE_ID, SITE_ID)

# call in relevant datasets
nla2012_wq <- read.csv("C:/Users/lrock1/OneDrive/Desktop/raw_data/nla2012_alldata/nla2012_waterchem_wide.csv") 
nla2012_siteinfo <- read.csv('C:/Users/lrock1/OneDrive/Desktop/raw_data/nla2012_alldata/NLA2012_wide_siteinfo_08232016.csv')
nla2012_condition <- read.csv('C:/Users/lrock1/OneDrive/Desktop/raw_data/nla2012_alldata/nla_2012_condition_categories.csv')
nla2012_keyinfo <- read.csv('C:/Users/lrock1/OneDrive/Desktop/raw_data/nla2012_alldata/nla12_keyvariables_data.csv') 

ws_data_2012 <- read.csv('C:/Users/lrock1/OneDrive/Desktop/raw_data/nla2012_alldata/nla2012_wide_watershed.csv') |>
  select(SITE_ID, NLCD2006_DEVELOPEDPCT_BSN, NLCD2006_AGRICPCT_BSN, NLCD2006_WATERPCT_BSN, NLCD2006_WETLANDPCT_BSN, NLCD2006_FORESTPCT_BSN) |>
  rename(PCT_WATER_BSN = NLCD2006_WATERPCT_BSN,
         PCT_DEVELOPED_BSN = NLCD2006_DEVELOPEDPCT_BSN,
         PCT_FOREST_BSN = NLCD2006_FORESTPCT_BSN,
         PCT_AGRIC_BSN = NLCD2006_AGRICPCT_BSN,
         PCT_WETLAND_BSN = NLCD2006_WETLANDPCT_BSN)


key <- nla2012_keyinfo |>
  mutate(keeprow = "YES") |>
  select(SITE_ID, UID, PTL_RESULT, NTL_RESULT, CHLX_RESULT, keeprow) |>
  rename(VISIT_ID = UID,
         CHLA_PPB = CHLX_RESULT)

add_chl <- nla2012_keyinfo |>
  select(UID, CHLX_RESULT) |>
  rename(VISIT_ID = UID,
         CHLA_PPB = CHLX_RESULT)

# filter for N & P
nla2012_wq1 <- nla2012_wq |>
  select(UID, NTL_UNITS, NTL_RESULT, PTL_UNITS, PTL_RESULT, AMMONIA_N_RESULT, AMMONIA_N_UNITS, NITRATE_N_RESULT, NITRATE_N_UNITS, DOC_RESULT, DOC_UNITS) |> # filter for TP, TN
  rename(NTL_PPM = NTL_RESULT,
         NH4N_PPM = AMMONIA_N_RESULT,
         NO3N_PPM = NITRATE_N_RESULT,
         PTL_PPB = PTL_RESULT,
         DOC_PPM = DOC_RESULT,
         VISIT_ID = UID) |>
  select(-contains("UNITS")) |>
  left_join(add_chl)


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
  mutate(UNIQUE_ID = ifelse(is.na(UNIQUE_ID), SITE_ID, UNIQUE_ID)) |>
  left_join(ws_data_2012)


# filter for condition information
nla2012_condition1 <- nla2012_condition |>
  select(SITE_ID, UID, AGGR_ECO9_2015, CHLA_COND, NTL_COND, PTL_COND, TROPHIC_STATE) |>
  rename(VISIT_ID = UID,
         ECO_REG = AGGR_ECO9_2015)


nla2012 <- left_join(nla2012_wq1, nla2012_siteinfo1) 

nla2012 <- left_join(nla2012, nla2012_condition1) |>
  group_by(SITE_ID) |> # Take the mean of parameters in intentionally resampled locations 
  mutate(NH4N_PPM = mean(NH4N_PPM), 
         NO3N_PPM = mean(NO3N_PPM), 
         NTL_PPM = mean(NTL_PPM),
         PTL_PPB = mean(PTL_PPB),
         CHLA_PPB = mean(CHLA_PPB),
         DOC_PPM = mean(DOC_PPM)) |>
  ungroup() |>
  mutate(DIN_PPM = NH4N_PPM + NO3N_PPM)


#add column for molar TN:TP, TN, TP
Pmol <- molMass("P") * 1000000 #ug/mol 
Nmol <- molMass("N") * 1000 #mg/mol
Cmol <- molMass("C") * 1000 #mg/mol

nla2012.tntp <- nla2012 |>
  mutate(tn.tp = (NTL_PPM/Nmol)/(PTL_PPB/Pmol),
         TN_mol = (NTL_PPM)/Nmol,
         NH4N_mol = (NH4N_PPM)/Nmol,
         NO3N_mol = (NO3N_PPM)/Nmol,
         TP_mol = (PTL_PPB)/Pmol,
         DOC_mol = DOC_PPM/Cmol,
         DIN_mol = DIN_PPM/Nmol) 
  


# delete rows not in key information? - yes. these data were not included here so I'm chucking them
check <- nla2012.tntp |>
  full_join(key) 

nla2012.tntp1 <- nla2012.tntp |>
  drop_na(WGT_NLA) # exactly 92 rows less than my original. See "looking_for_missingdata.R" commented out section



## write.csv(nla2012.tntp1, "Data/NLA/NLA_2012.csv")




###missing data resolved using this code (from "looking_for_missingdata.R":
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This issue has been resolved using the key variables dataset !!
NLA_2012 <- nla2012.tntp1


missing12 <- NLA_2012 |>
  filter(is.na(ECO_REG)) |> # total of 192 observations are missing this information
  select(UNIQUE_ID) |>
  distinct()


# can I get this information from other unique IDs from 2012?

missinginfo <- NLA_2012 |>
  select(UNIQUE_ID, WGT_NLA, ECO_REG, CHLA_COND, NTL_COND, PTL_COND, TROPHIC_STATE)

## weights -- nope.

all_wgt <- missinginfo |>
  select(UNIQUE_ID, WGT_NLA) |>
  #drop_na() |>
  distinct()

miss_wgt <- missinginfo |>
  select(UNIQUE_ID, WGT_NLA) |>
  filter(is.na(WGT_NLA)) |>
  distinct() |> # 92 ids are missing weights
  select(UNIQUE_ID)


fill_wgt <- left_join(miss_wgt, all_wgt) # this fixes nothing


## other info

all_info <- missinginfo |>
  # select(-WGT_NLA) |>
  drop_na() |>
  distinct()


miss_info <- missinginfo |>
  # select(-WGT_NLA) |>
  filter(is.na(ECO_REG)) |>
  distinct() |> # 100 ids are missing this information
  select(UNIQUE_ID)

fill_info <- left_join(miss_info, all_info) # filled in all of the data!!
fill_info <- fill_info |>
  rename(WGT2 = WGT_NLA,
         ECO2 = ECO_REG,
         chcond2 = CHLA_COND,
         tpcond2 = PTL_COND,
         tncond2 = NTL_COND,
         tstate = TROPHIC_STATE)


# how many leftover that need information?

leftover <- fill_info |>
  filter(is.na(ECO2)) # none are leftover.


# add fill info and resave :) -- copy code into "Call_data_2012NLA.R"

NLA_2012_1 <- NLA_2012 |>
  left_join(fill_info) |>
  mutate(WGT_NLA = ifelse(is.na(WGT_NLA), WGT2, WGT_NLA),
         ECO_REG = ifelse(is.na(ECO_REG), ECO2, ECO_REG),
         CHLA_COND = ifelse(is.na(CHLA_COND), chcond2, CHLA_COND),
         PTL_COND = ifelse(is.na(PTL_COND), tpcond2, PTL_COND),
         NTL_COND = ifelse(is.na(NTL_COND), tncond2, NTL_COND),
         TROPHIC_STATE = ifelse(is.na(TROPHIC_STATE), tstate, TROPHIC_STATE))  |>
  select(-WGT2, -ECO2, -chcond2, -tpcond2, -tncond2, -tstate)


write.csv(NLA_2012_1, "Data/NLA/NLA_2012.csv")



#NLA 2012 needs eco reg names and chlorophyll and trophic states
# nla2012_keyinfo <- read.csv('C:/Users/lrock1/OneDrive/Desktop/raw_data/nla2012_alldata/nla12_keyvariables_data.csv') |>
#   select(UID, CHLX_RESULT) |>
#   rename(VISIT_ID = UID,
#          CHLA_PPB = CHLX_RESULT)

nla12 <- NLA_2012_1 |>
  left_join(ecoregs) |>
  #left_join(nla2012_keyinfo) |>
  get_tstate()

write.csv(nla12, "Data/NLA/NLA_2012.csv")

