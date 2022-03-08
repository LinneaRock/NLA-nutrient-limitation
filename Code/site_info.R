#Creating site information based on Unique ID

library(tidyverse)

uniques <- readxl::read_xlsx('Data/raw_data/crosswalk_ID.xlsx', sheet = 'LONG_NARS_ALLSurvey_SITE_ID_CRO')
uniques1 <- uniques |>
  filter(SURVEY %in% c('NLA', 'NRSA')) |> #filter for lakes and rivers assessments
  rename(STUDY = SURVEY,
         PSTL_CODE = State) #renaming to match column headers in the basic info datasets


#information from recent surveys
nla_info_2017 <- read.csv('Data/raw_data/nla2017_alldata/nla_2017_site_information-data.csv')
nrsa_info_2018 <- read.csv('Data/raw_data/nrsa2018_alldata/nrsa-1819-site-information-data-updated.csv')

nla_info_basic <- nla_info_2017 |>
  select(UNIQUE_ID, UID, SITE_ID, STUDY, EPA_REG, AG_ECO9_NM, AREA_HA, ELEVATION, FTYPE, HUC8, LAKE_ORGN, LAT_DD83, LON_DD83, OWN_NARS, PSTL_CODE, URBN_NLA07, URBN_NLA17, WGT_TP_CORE)

nrsa_info_basic <- nrsa_info_2018 |>
  select(UNIQUE_ID, UID, SITE_ID, EPA_REG, AG_ECO9_NM, ELEVATION, FTYPE, HUC8, LAT_DD83, LON_DD83, OWN_NARS, PSTL_CODE, URBN_NRS08, URBN_NRS18, WGT_TP_CORE, STRAH_CAT, STRAH_ORD) |>
  mutate(STUDY = 'NRSA')

info_basic_combined <- bind_rows(nla_info_basic, nrsa_info_basic) #nla and nrsa pertinent information





