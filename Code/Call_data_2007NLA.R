#Organizing data 

library(tidyverse)
library(biogas) #gets molar masses 

uniques <- readxl::read_xlsx('Data/raw_data/crosswalk_ID.xlsx', sheet = 'LONG_NARS_ALLSurvey_SITE_ID_CRO')
uniques1 <- uniques |>
  filter(SURVEY %in% c('NLA', 'NRSA')) |>
  select(UNIQUE_ID, SITE_ID)


nla2007_wq <- read.csv('Data/raw_data/nla2007_alldata/NLA2007_WaterQuality_20091123.csv') 
nla2007_siteinfo <- read.csv('Data/raw_data/nla2007_alldata/NLA2007_sampledlakeinformation_20091113.csv')
nla2007_trophicstatus <- read.csv('Data/raw_data/nla2007_alldata/NLA2007_Trophic_ConditionEstimate_20091123.csv')
nla2007_reccondition <- read.csv('Data/raw_data/nla2007_alldata/NLA2007_Recreational_ConditionEstimates_20091123.csv')
nla2007_chemcondition <- read.csv('Data/raw_data/nla2007_alldata/NLA2007_Chemical_ConditionEstimates_20091123.csv')


#filter for N & P
nla2007_wq1 <- nla2007_wq |>
  select(SITE_ID, VISIT_ID, VISIT_NO, DATE_COL, 33, 37, 40, 44, 51, 142) |> #filter for TP, TN, nitrate, ammonium, and chlorophyll a
  rename(CHLA_PPB = CHLA,
          PTL_PPB = PTL,
         NO3NO2_PPM = NO3_NO2) |>
  group_by(SITE_ID, VISIT_ID, DATE_COL) |>
  summarise(NH4N_PPM = mean(NH4N_PPM), #There are some duplicates in the data, this averages duplicate analyses
           NO3N_PPM = mean(NO3N_PPM), 
           #NO3NO2_PPM = mean(NO3NO2_PPM), #this is collected using a different method.It is sometimes less than NO3, which doesn't make sense
           NTL_PPM = mean(NTL_PPM),
           PTL_PPB = mean(PTL_PPB),
           CHLA_PPB = mean(CHLA_PPB)) |>
  ungroup() 
  #mutate(check = NO3NO2_PPM >= NO3N_PPM)
 

#filter for useful site informaiton
nla2007_siteinfo1 <- nla2007_siteinfo |>
  select(SITE_ID, VISIT_ID, VISIT_NO, DATE_COL, SITE_TYPE, LON_DD, LAT_DD, EPA_REG, WGT_NLA, URBAN, NUT_REG, NUTREG_NAME, LAKE_ORIGIN, AREA_HA, SLD, DEPTHMAX, ELEV_PT, HUC_8) |>
  mutate(VISIT_ID = ifelse(SITE_ID == "NLA06608-3846", 8844, VISIT_ID)) |> # for some reason this is missing in the original dataset and messes up joining later.
  left_join(uniques1)
  
#filterfor trophic information
nla2007_trophicstatus1 <- nla2007_trophicstatus |>
  select(SITE_ID, VISIT_NO, 45:47) # trophic status based on TN, TP, and chlorophyll 

#filterfor condition - recreation
nla2007_reccondition1 <- nla2007_reccondition |>
  select(SITE_ID, VISIT_NO, 44, 48) # recreation condition based on chlorophyll and cyanobacteria 

#filter for condition - chemical
nla2007_chemcondition1 <- nla2007_chemcondition |>
  select(SITE_ID, VISIT_NO, 46:48) # chemical condition based on TN, TP, and chlorophyll 

#combine the informaiton 
nla2007 <- left_join(nla2007_wq1, nla2007_siteinfo1, by = c("SITE_ID", "VISIT_ID")) |>
  left_join(nla2007_trophicstatus1) |>
  left_join(nla2007_reccondition1) |>
  left_join(nla2007_chemcondition1)


#add column for molar TN:TP, NH4:TP, NO3:TP
Pmol <- molMass("P") * 1000000 #ug/mol
Nmol <- molMass("N") * 1000 #mg/mol

nla2007_tntp <- nla2007 |>
  mutate(tn.tp = (NTL_PPM/Nmol)/(PTL_PPB/Pmol),
         no3.tp = (NO3N_PPM/Nmol)/(PTL_PPB/Pmol),
         nh4.tp = (NH4N_PPM/Nmol)/(PTL_PPB/Pmol),
         `nh4+no3.tp` = ((NH4N_PPM + NO3N_PPM)/Nmol)/(PTL_PPB/Pmol))

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
  rename(DATE_COL =  DATE_COL.x)

write.csv(nla2007_tntp1, "Data/NLA_2007.csv")
             