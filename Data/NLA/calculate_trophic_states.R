## Script to add missing trophic states ##
####### email from 4/14/2022 #######

source("Data/NLA/Call_NLA_data.R")


# get eco region names for all datasets
ecoregs <- NLA17 |>
  select(ECO_REG, ECO_REG_NAME) |>
  distinct()

# write funtion to calculate trophic states
get_tstate <- function(data) {
  data1 <- data |>
    mutate(TSTATE_TP = NA,
           TSTATE_TN = NA,
           TSTATE_CHL = NA) |>
    mutate(TSTATE_TP = ifelse(PTL_PPB <= 10, "OLIGOTROPHIC (<= 10 ug/L)", # total P trophic states
                              ifelse(between(PTL_PPB, 10, 25), "MESOTROPHIC (10-25 ug/L)",
                                     ifelse(between(PTL_PPB, 25, 100), "EUTROPHIC (25-100 ug/L)",
                                            ifelse(PTL_PPB > 100, "HYPEREUTROPHIC (> 100 ug/L)", TSTATE_TP))))) |>
    mutate(TSTATE_TN = ifelse(NTL_PPM <= 0.35, "OLIGOTROPHIC (<= 0.35 mg/L)", # total N trophic states
                              ifelse(between(NTL_PPM, 0.35, 0.75), "MESOTROPHIC (0.35-0.75 mg/L)",
                                     ifelse(between(NTL_PPM, 0.75, 1.4), "EUTROPHIC (0.75-1.4 mg/L)",
                                            ifelse(NTL_PPM > 1.4, "HYPEREUTROPHIC (> 1.4 mg/L)", TSTATE_TN))))) |>
    mutate(TSTATE_CHL = ifelse(CHLA_PPB <= 2, "OLIGOTROPHIC (<= 2 ug/L)", # chlorophyll-a trophic states
                               ifelse(between(CHLA_PPB, 2, 7), "MESOTROPHIC (2-7 ug/L)",
                                      ifelse(between(CHLA_PPB, 7, 30), "EUTROPHIC (7-30 ug/L)",
                                             ifelse(CHLA_PPB > 30, "HYPEREUTROPHIC (> 30 ug/L)", TSTATE_CHL)))))
}



# NLA 2007 just needs eco reg names and to standardize trophic status names
nla07 <- NLA07 |>
  left_join(ecoregs) |>
  get_tstate()

write.csv(nla07, "Data/NLA/NLA_2007.csv")


#NLA 2012 needs eco reg names and chlorophyll and trophic states
# nla2012_keyinfo <- read.csv('C:/Users/lrock1/OneDrive/Desktop/raw_data/nla2012_alldata/nla12_keyvariables_data.csv') |>
#   select(UID, CHLX_RESULT) |>
#   rename(VISIT_ID = UID,
#          CHLA_PPB = CHLX_RESULT)

nla12 <- NLA12 |>
  left_join(ecoregs) |>
  #left_join(nla2012_keyinfo) |>
  get_tstate()

write.csv(nla12, "Data/NLA/NLA_2012.csv")

#NLA 2012 needs chlorophyll and trophic states
# nla2017_wq <- read.csv("C:/Users/lrock1/OneDrive/Desktop/raw_data/nla2017_alldata/nla_2017_water_chemistry_chla-data.csv") |>
#   select(UID, ANALYTE, RESULT, RESULT_UNITS) |> 
#   rename(VISIT_ID = UID) |>
#   filter(ANALYTE %in% c('CHLA')) |>
#   mutate(RESULT = as.numeric(RESULT)) |>
#   drop_na(RESULT) |>
#   select(-RESULT_UNITS) |>
#   pivot_wider(names_from = ANALYTE, values_from = RESULT) |>
#   rename(CHLA_PPB = CHLA) 

nla17 <- NLA17 |>
  #left_join(nla2017_wq) |>
  get_tstate()


write.csv(nla17, "Data/NLA/NLA_2017.csv")


