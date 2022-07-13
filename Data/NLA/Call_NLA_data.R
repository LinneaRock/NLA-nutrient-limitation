
## Script to pull in all data and combine the datasets

library(tidyverse)
library(lubridate)


## Looking at plots of  all data

#NLA07 <-read.csv("Data/NLA/NLA_2007.csv") |> 
NLA07 <-   read.csv("C:/Users/lrock1/Downloads/PhD_code/STOICH_NARSchallenge/Data/NLA/NLA_2007.csv") |>
  select(-X) |>
  mutate(DATE_COL = as.Date(DATE_COL))

#str(NLA07) # what variables need to be altered (e.g. to as date or as numeric, etc.)? 

#NLA12 <- read.csv("Data/NLA/NLA_2012.csv") |> 
NLA12 <- read.csv("C:/Users/lrock1/Downloads/PhD_code/STOICH_NARSchallenge/Data/NLA/NLA_2012.csv") |>
  select(-X) |>
  mutate(DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"))

#str(NLA12) # what variables need to be altered (e.g. to as date or as numeric, etc.)? 

#NLA17 <- read.csv("Data/NLA/NLA_2017.csv") |> 
NLA17 <- read.csv("C:/Users/lrock1/Downloads/PhD_code/STOICH_NARSchallenge/Data/NLA/NLA_2017.csv") |>
  select(-X) |>
  mutate(DATE_COL = as.Date(DATE_COL),
         HUC_8 = as.integer(substring(HUC_8, 2)))

#str(NLA17) # what variables need to be altered (e.g. to as date or as numeric, etc.)? 

# combine and plot all TN:TP
all_NLA <- bind_rows(NLA07, NLA12, NLA17) |>
  mutate(year = as.character(year(DATE_COL))) |>
  mutate(URBAN = ifelse(URBAN %in% c("No", "NO", "Non-Urban"), "Non-Urban", "Urban")) |>
  drop_na(PTL_PPB) |>
  mutate(LAKE_ORIGIN = ifelse(LAKE_ORIGIN == "MAN_MADE", "MAN-MADE", LAKE_ORIGIN)) |>
  mutate(TROPHIC_STATE  = ifelse(startsWith(TSTATE_CHL, "OLIGOTROPHIC"), "Oligo.",
                                 ifelse(startsWith(TSTATE_CHL, "MESOTROPHIC"), "Meso.", 
                                        ifelse(startsWith(TSTATE_CHL,"EUTROPHIC"), "Eutro.",
                                               ifelse(startsWith(TSTATE_CHL, "HYPEREUTROPHIC"), "Hyper.", TROPHIC_STATE))))) |>
  mutate(TROPHIC_STATE = factor(TROPHIC_STATE, levels = c("Oligo.", "Meso.", "Eutro.", "Hyper."))) 


all_NLA$TSTATE_TN = factor(all_NLA$TSTATE_TN,
                           levels = c("OLIGOTROPHIC (<= 0.35 mg/L)", "MESOTROPHIC (0.35-0.75 mg/L)","EUTROPHIC (0.75-1.4 mg/L)","HYPEREUTROPHIC (> 1.4 mg/L)"))

all_NLA$TSTATE_TP = factor(all_NLA$TSTATE_TP,
                            levels = c("OLIGOTROPHIC (<= 10 ug/L)", "MESOTROPHIC (10-25 ug/L)","EUTROPHIC (25-100 ug/L)","HYPEREUTROPHIC (> 100 ug/L)"))

all_NLA$TSTATE_CHL = factor(all_NLA$TSTATE_CHL,
                            levels = c("OLIGOTROPHIC (<= 2 ug/L)", "MESOTROPHIC (2-7 ug/L)","EUTROPHIC (7-30 ug/L)","HYPEREUTROPHIC (> 30 ug/L)"))

