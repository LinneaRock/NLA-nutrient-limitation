# Organizing data  - NLA 2007

library(tidyverse)
library(biogas) #gets molar masses 

uniques <- readxl::read_xlsx('C:/Users/lrock1/OneDrive/Desktop/raw_data/crosswalk_ID.xlsx', sheet = 'LONG_NARS_ALLSurvey_SITE_ID_CRO')
uniques1 <- uniques |>
  filter(SURVEY %in% c('NLA', 'NRSA')) |>
  select(UNIQUE_ID, SITE_ID)


nla2007_wq <- read.csv('C:/Users/lrock1/OneDrive/Desktop/raw_data/nla2007_alldata/NLA2007_WaterQuality_20091123.csv') 
nla2007_siteinfo <- read.csv('C:/Users/lrock1/OneDrive/Desktop/raw_data/nla2007_alldata/NLA2007_sampledlakeinformation_20091113.csv')
nla2007_trophicstatus <- read.csv('C:/Users/lrock1/OneDrive/Desktop/raw_data/nla2007_alldata/NLA2007_Trophic_ConditionEstimate_20091123.csv')
nla2007_reccondition <- read.csv('C:/Users/lrock1/OneDrive/Desktop/raw_data/nla2007_alldata/NLA2007_Recreational_ConditionEstimates_20091123.csv')
nla2007_chemcondition <- read.csv('C:/Users/lrock1/OneDrive/Desktop/raw_data/nla2007_alldata/NLA2007_Chemical_ConditionEstimates_20091123.csv')

ws_data_2007 <- read.csv('C:/Users/lrock1/OneDrive/Desktop/raw_data/nla2007_alldata/NLA2007_Basin_Landuse_Metrics_20061022.csv') |>
  select(SITE_ID, PCT_WATER_BSN, PCT_DEVELOPED_BSN, PCT_FOREST_BSN, PCT_AGRIC_BSN, PCT_WETLAND_BSN)

#filter for N & P
nla2007_wq1 <- nla2007_wq |>
  select(SITE_ID, VISIT_ID, VISIT_NO, DATE_COL, 27, 30, 33, 37, 40, 44, 51, 142) |> #filter for TP, TN, nitrate, ammonium, and chlorophyll a
  rename(CHLA_PPB = CHLA,
         PTL_PPB = PTL,
         #NO3NO2_PPM = NO3_NO2,
         #TOC_PPM = TOC, # don't keep TOC -- it is minimally collected and not collected at all in 2017
         DOC_PPM = DOC) |>
  group_by(SITE_ID, VISIT_ID, DATE_COL) |>
  summarise(NH4N_PPM = mean(NH4N_PPM), #There are some duplicates in the data, this averages duplicate analyses
           NO3N_PPM = mean(NO3N_PPM), 
           #NO3NO2_PPM = mean(NO3NO2_PPM), #this is collected using a different method.It is sometimes less than NO3, which doesn't make sense
           NTL_PPM = mean(NTL_PPM),
           PTL_PPB = mean(PTL_PPB),
           CHLA_PPB = mean(CHLA_PPB),
           #TOC_PPM = mean(TOC_PPM),
           DOC_PPM = mean(DOC_PPM)) |>
  ungroup() |> 
  #mutate(check = NO3NO2_PPM >= NO3N_PPM)
  mutate(DIN_PPM = NH4N_PPM + NO3N_PPM) 

#filter for useful site informaiton
nla2007_siteinfo1 <- nla2007_siteinfo |>
  select(SITE_ID, VISIT_ID, VISIT_NO, DATE_COL, SITE_TYPE, LON_DD, LAT_DD, EPA_REG, WGT_NLA, URBAN, WSA_ECO9, LAKE_ORIGIN, AREA_HA, ELEV_PT, HUC_8) |>
  mutate(VISIT_ID = ifelse(SITE_ID == "NLA06608-3846", 8844, VISIT_ID)) |> # for some reason this is missing in the original dataset and messes up joining later.
  left_join(uniques1) |>
  rename(ECO_REG = WSA_ECO9) |>
  left_join(ws_data_2007)
  
#filterfor trophic information
nla2007_trophicstatus1 <- nla2007_trophicstatus |>
  select(SITE_ID, VISIT_NO, 45:47) # trophic status based on TN, TP, and chlorophyll 

# #filterfor condition - recreation
# nla2007_reccondition1 <- nla2007_reccondition |>
#   select(SITE_ID, VISIT_NO, 44, 48) # recreation condition based on chlorophyll and cyanobacteria 

#filter for condition - chemical
nla2007_chemcondition1 <- nla2007_chemcondition |>
  select(SITE_ID, VISIT_NO, 46:48) # chemical condition based on TN, TP, and chlorophyll 

#combine the informaiton 
nla2007 <- left_join(nla2007_wq1, nla2007_siteinfo1, by = c("SITE_ID", "VISIT_ID")) |>
  left_join(nla2007_trophicstatus1) |>
 # left_join(nla2007_reccondition1) |>
  left_join(nla2007_chemcondition1)


#add column for molar TN:TP, NH4:TP, NO3:TP
Pmol <- molMass("P") * 1000000 #ug/mol
Nmol <- molMass("N") * 1000 #mg/mol
Cmol <- molMass("C") * 1000 #mg/mol

nla2007_tntp <- nla2007 |>
  mutate(tn.tp = (NTL_PPM/Nmol)/(PTL_PPB/Pmol),
         TN_mol = (NTL_PPM)/Nmol,
         NH4N_mol = (NH4N_PPM)/Nmol,
         NO3N_mol = (NO3N_PPM)/Nmol,
         TP_mol = (PTL_PPB)/Pmol,
         #TOC_mol = TOC_PPM/Cmol,
         DOC_mol = DOC_PPM/Cmol,
         DIN_mol = DIN_PPM/Nmol)

#quick view - raw
ggplot(nla2007_tntp) +
  geom_point(aes((PTL_PPB/Pmol), (NTL_PPM/Nmol))) +
  geom_abline(slope = 16, intercept = 0) 

#quick view - log10
ggplot(nla2007_tntp) +
  geom_point(aes(log((PTL_PPB/Pmol), base = 10), log((NTL_PPM/Nmol), base = 10))) 

#quick view - natural log
ggplot(nla2007_tntp) +
  geom_point(aes(log((PTL_PPB/Pmol)), log((NTL_PPM/Nmol))))  


# get rid of extra date column and save dataset
nla2007_tntp1 <-nla2007_tntp |>
  select(-DATE_COL.y) |>
  rename(DATE_COL =  DATE_COL.x) |>
  mutate(UNIQUE_ID = ifelse(is.na(UNIQUE_ID), SITE_ID, UNIQUE_ID)) # this resolves missing data problem :)



write.csv(nla2007_tntp1, "Data/NLA/NLA_2007.csv")





             