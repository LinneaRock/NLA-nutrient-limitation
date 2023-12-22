# Organizing data  - NLA 2017

library(tidyverse)
library(biogas) # gets molar masses 


# unique site id crosswalk
uniques <- readxl::read_xlsx('C:/Users/linne/OneDrive/Desktop/raw_data/crosswalk_ID.xlsx', sheet = 'LONG_NARS_ALLSurvey_SITE_ID_CRO')
uniques1 <- uniques |>
  filter(SURVEY %in% c('NLA', 'NRSA')) |>
  select(UNIQUE_ID, SITE_ID)

# call in relevant datasets
nla2017_wq <- read.csv("C:/Users/linne/OneDrive/Desktop/raw_data/nla2017_alldata/nla_2017_water_chemistry_chla-data.csv") 
nla2017_siteinfo <- read.csv('C:/Users/linne/OneDrive/Desktop/raw_data/nla2017_alldata/nla_2017_site_information-data.csv')


analytes <- data.frame(unique(nla2017_wq$ANALYTE))

# filter for N & P
nla2017_wq1 <- nla2017_wq |>
  select(UID, SITE_ID, VISIT_NO, DATE_COL, ANALYTE, RESULT, RESULT_UNITS) |> 
  rename(VISIT_ID = UID) |>
  filter(ANALYTE %in% c('NTL', 'PTL', 'AMMONIA_N', 'NITRATE_N', 'DOC', 'CHLA')) |>
  mutate(RESULT = as.numeric(RESULT)) |>
  drop_na(RESULT) |>
  select(-RESULT_UNITS) |>
  pivot_wider(names_from = ANALYTE, values_from = RESULT) |>
  rename(NTL_PPM = NTL,
         PTL_PPB = PTL,
         NH4N_PPM = AMMONIA_N,
         NO3N_PPM = NITRATE_N,
         DOC_PPM = DOC,
         CHLA_PPB = CHLA) |>
  group_by(SITE_ID, VISIT_ID, DATE_COL) |> # There are some duplicate sample runs in the data, this averages duplicate analyses 
  mutate(NH4N_PPM = mean(NH4N_PPM), 
         NO3N_PPM = mean(NO3N_PPM), 
         #NO3NO2_PPM = mean(NO3NO2_PPM), #this is collected using a different method.It is sometimes less than NO3, which doesn't make sense
         NTL_PPM = mean(NTL_PPM),
         PTL_PPB = mean(PTL_PPB),
         CHLA_PPB = mean(CHLA_PPB),
         #TOC_PPM = mean(TOC_PPM),
         DOC_PPM = mean(DOC_PPM)) |>
  ungroup() |> 
  mutate(DIN_PPM = NH4N_PPM + NO3N_PPM)


# filter for useful site informaiton
nla2017_siteinfo1 <- nla2017_siteinfo |>
   select(SITE_ID, UID, VISIT_NO, SITETYPE, AG_ECO9, AG_ECO9_NM, LON_DD83, LAT_DD83, EPA_REG, WGT_TP_EXTENT, URBN_NLA17, LAKE_ORGN, AREA_HA, ELEVATION, HUC8) |>
   left_join(uniques1) |> # join with uniques dataset
   rename(VISIT_ID = UID,
          SITE_TYPE = SITETYPE,
          LON_DD = LON_DD83,
          LAT_DD = LAT_DD83,
          WGT_NLA = WGT_TP_EXTENT,
          ELEV_PT = ELEVATION,
          HUC_8 = HUC8,
          ECO_REG = AG_ECO9,
          ECO_REG_NAME = AG_ECO9_NM,
          URBAN = URBN_NLA17,
          LAKE_ORIGIN = LAKE_ORGN) |>
   filter(!is.na(VISIT_ID)) |>
   mutate(UNIQUE_ID = ifelse(is.na(UNIQUE_ID), SITE_ID, UNIQUE_ID))


nla2017 <- left_join(nla2017_wq1, nla2017_siteinfo1)



#add column for molar TN:TP, TN, TP
Pmol <- molMass("P") * 1000000 #ug/mol 
Nmol <- molMass("N") * 1000 #mg/mol
Cmol <- molMass("C") * 1000 #mg/mol

nla2017.tntp <- nla2017 |>
  mutate(tn.tp = (NTL_PPM/Nmol)/(PTL_PPB/Pmol),
         TN_mol = (NTL_PPM)/Nmol,
         NH4N_mol = (NH4N_PPM)/Nmol,
         NO3N_mol = (NO3N_PPM)/Nmol,
         TP_mol = (PTL_PPB)/Pmol,
         DOC_mol = DOC_PPM/Cmol,
         DIN_mol = DIN_PPM/Nmol)


#write.csv(nla2017.tntp, "Data/NLA_2017.csv")


#NLA 2017 needs chlorophyll and trophic states
# nla2017_wq <- read.csv("C:/Users/linne/OneDrive/Desktop/raw_data/nla2017_alldata/nla_2017_water_chemistry_chla-data.csv") |>
#   select(UID, ANALYTE, RESULT, RESULT_UNITS) |> 
#   rename(VISIT_ID = UID) |>
#   filter(ANALYTE %in% c('CHLA')) |>
#   mutate(RESULT = as.numeric(RESULT)) |>
#   drop_na(RESULT) |>
#   select(-RESULT_UNITS) |>
#   pivot_wider(names_from = ANALYTE, values_from = RESULT) |>
#   rename(CHLA_PPB = CHLA) 

source("Data/NLA/calculate_trophic_states.R")

nla17 <- nla2017.tntp |>
  #left_join(nla2017_wq) |>
  get_tstate()


write.csv(nla17, "Data/NLA/NLA_2017.csv")

nla17call <- read.csv("Data/NLA/NLA_2017.csv")

ws_data_2017 <- read.csv('C:/Users/linne/Downloads/nla2017_landMets.csv') |>
  select(SITE_ID, PCTURBHI2011, PCTURBLO2011, PCTURBMD2011, PCTURBOP2011, PCTHAY2011, PCTCROP2011) |>
  mutate(#PCT_WATER_BSN = NLCD2006_WATERPCT_BSN,
         PCT_DEVELOPED_BSN = (PCTURBHI2011 + PCTURBLO2011 + PCTURBMD2011 + PCTURBOP2011),
        # PCT_FOREST_BSN = NLCD2006_FORESTPCT_BSN,
         PCT_AGRIC_BSN = (PCTHAY2011 + PCTCROP2011)
         #PCT_WETLAND_BSN = NLCD2006_WETLANDPCT_BSN
        ) |>
  select(SITE_ID, PCT_DEVELOPED_BSN, PCT_AGRIC_BSN) |>
  distinct()

nla17call2 <- nla17call |>
  left_join(ws_data_2017)

write.csv(nla17call2, "Data/NLA/NLA_2017.csv")





                                                                
                                                                


# Finding better reference lake parameters from site information
References_17 <- read.csv("C:/Users/linne/Downloads/nla_2017_site_information-data.csv") |>
  select(SITE_ID, RT_NLA17) |>
  filter(RT_NLA17 == 'R') |>
  distinct() |> # 214
  mutate(REFERENCE = 'Y') |>
  select(-RT_NLA17)


nla17 <- read.csv('Data/NLA/NLA_2017.csv') |>
  left_join(References_17)

write.csv(nla17, "Data/NLA/NLA_2017.csv")
