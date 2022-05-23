
## Script to pull in all data and combine the datasets

library(tidyverse)
library(lubridate)


## Looking at plots of  all data

NLA07 <- read.csv("C:/Users/linne/Downloads/PhD_code/STOICH_NARSchallenge/Data/NLA/NLA_2007.csv") |>
  select(-X) |>
  mutate(DATE_COL = as.Date(DATE_COL))

#str(NLA07) # what variables need to be altered (e.g. to as date or as numeric, etc.)? 

NLA12 <- read.csv("C:/Users/linne/Downloads/PhD_code/STOICH_NARSchallenge/Data/NLA/NLA_2012.csv") |>
  select(-X) |>
  mutate(DATE_COL = as.Date(DATE_COL))

#str(NLA12) # what variables need to be altered (e.g. to as date or as numeric, etc.)? 

NLA17 <- read.csv("C:/Users/linne/Downloads/PhD_code/STOICH_NARSchallenge/Data/NLA/NLA_2017.csv") |>
  select(-X) |>
  mutate(DATE_COL = as.Date(DATE_COL),
         HUC_8 = as.integer(substring(HUC_8, 2)))

#str(NLA17) # what variables need to be altered (e.g. to as date or as numeric, etc.)? 

# combine and plot all TN:TP
all_NLA <- bind_rows(NLA07, NLA12, NLA17) |>
  mutate(year = as.character(year(DATE_COL))) |>
  mutate(URBAN = ifelse(URBAN %in% c("No", "NO", "Non-Urban"), "Non-Urban", "Urban")) 
