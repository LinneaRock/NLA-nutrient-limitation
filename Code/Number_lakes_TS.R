
# Script to calculate number of lakes classified in trophic state in each ecoregion and year.
source("Code/Functions/no_lakes_ts.R")
# using spsurvey package in R -- reports number of sampled lakes along with percentage of lakes it representson the landscape and standard error of that assessment

library(tidyverse)
# use the SpSurvey package from EPA
library(spsurvey)
## citation("spsurvey")


#first, subset the data

ts <- all_NLA |>
  select(UNIQUE_ID, year, WGT_NLA, ECO_REG_NAME, TSTATE_CHL, TSTATE_TP, TSTATE_TN, LAT_DD, LON_DD) |>
  mutate(WGT_NLA = ifelse(WGT_NLA == 0, 1, WGT_NLA)) # the package I'm using to analyze number of lakes in each category along with the percent of lakes represented requires weights to be all positive numbers. I'm not sure why there are zeros here at all... they should be representative of at least themselves. 



################################################################################
###################################Chla#########################################

chla_trophic_states <- no_lakes_ts(ts, "TSTATE_CHL") |> # And do all this dumb formatting
  filter(Category != "Total") |>
  mutate(fill_column = paste(nResp, "(", round(Estimate.P, 1), "±", round(StdError.P, 1), ")")) |>
  mutate(column_names = paste(Category, ".", year)) |>
  pivot_wider(id_cols = c(Subpopulation), names_from = "column_names", values_from = "fill_column")


#make the table
library(gt)

#this is not exactly what I want for my table, but it is close enough for now
chla_trophic_states |>
  group_by(Subpopulation) |>
  gt() |>
  tab_spanner_delim(delim = ".")
# 
# chla_tbl <- gt(chla_trophic_states)
# chla_ts_table <- chla_tbl |>
#   cols_label(
#     Subpopulation = "Ecoregion"
#     ) |>
#   tab_spanner(chla_tbl |> filter(Category == "OLIGOTROPHIC (<= 2 ug/L)"), label = "Oligotrophic", columns = vars(`2007`, `2012`, `2017`));chla_ts_table

################################################################################
###################################TP###########################################


TP_trophic_states <- no_lakes_ts(ts, "TSTATE_TP") |> # And do all this dumb formatting
  filter(Category != "Total") |>
  mutate(fill_column = paste(nResp, "(", round(Estimate.P, 1), "±", round(StdError.P, 1), ")")) |>
  mutate(column_names = paste(Category, ".", year)) |>
  pivot_wider(id_cols = c(Subpopulation), names_from = "column_names", values_from = "fill_column")


#make the table

#this is not exactly what I want for my table, but it is close enough for now
TP_trophic_states |>
  group_by(Subpopulation) |>
  gt() |>
  tab_spanner_delim(delim = ".")

################################################################################
###################################TN###########################################

TN_trophic_states <- no_lakes_ts(ts, "TSTATE_TN") |> # And do all this dumb formatting
  filter(Category != "Total") |>
  mutate(fill_column = paste(nResp, "(", round(Estimate.P, 1), "±", round(StdError.P, 1), ")")) |>
  mutate(column_names = paste(Category, ".", year)) |>
  pivot_wider(id_cols = c(Subpopulation), names_from = "column_names", values_from = "fill_column")


#make the table

#this is not exactly what I want for my table, but it is close enough for now
TN_trophic_states |>
  group_by(Subpopulation) |>
  gt() |>
  tab_spanner_delim(delim = ".")