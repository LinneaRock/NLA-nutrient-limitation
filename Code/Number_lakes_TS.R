
# Script to calculate number of lakes classified in trophic state in each ecoregion and year.

# using spsurvey package in R -- reports number of sampled lakes along with percentage of lakes it representson the landscape and standard error of that assessment

library(tidyverse)
# use the SpSurvey package from EPA
library(spsurvey)
## citation("spsurvey")



ts <- all_NLA |>
  select(UNIQUE_ID, year, WGT_NLA, ECO_REG_NAME, TSTATE_CHL, TSTATE_TP, TSTATE_TN, LAT_DD, LON_DD) |>
  mutate(WGT_NLA = ifelse(WGT_NLA == 0, 1, WGT_NLA)) # the package I'm using to analyze number of lakes in each category along with the percent of lakes represented requires weights to be all positive numbers. I'm not sure why there are zeros here at all... they should be representative of at least themselves. 



################################################################################
###################################Chla#########################################

# 2007
chla_2007_ecoreg <- cat_analysis(ts |> filter(year == 2007), subpop = "ECO_REG_NAME", vars = "TSTATE_CHL", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")

chla_2007_ecoreg.1 <- chla_2007_ecoreg |>
  select(Subpopulation, Category, nResp, Estimate.P, StdError.P) |>
  mutate(year = 2007)

# 2012
chla_2012_ecoreg <- cat_analysis(ts |> filter(year == 2012), subpop = "ECO_REG_NAME", vars = "TSTATE_CHL", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")

chla_2012_ecoreg.1 <- chla_2012_ecoreg |>
  select(Subpopulation, Category, nResp, Estimate.P, StdError.P) |>
  mutate(year = 2012)

# 2017
chla_2017_ecoreg <- cat_analysis(ts |> filter(year == 2017), subpop = "ECO_REG_NAME", vars = "TSTATE_CHL", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")

chla_2017_ecoreg.1 <- chla_2017_ecoreg |>
  select(Subpopulation, Category, nResp, Estimate.P, StdError.P) |>
  mutate(year = 2017)

chla_national <- cat_analysis(ts, vars = "TSTATE_CHL", subpop = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")

chla_national.1 <- chla_national|>
  select(Subpopulation, Category, nResp, Estimate.P, StdError.P) |>
  mutate(year = Subpopulation,
         Subpopulation = "National")

chla_trophic_states <- rbind(chla_national.1, chla_2007_ecoreg.1, chla_2012_ecoreg.1, chla_2017_ecoreg.1) |>
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

# 2007
TP_2007_ecoreg <- cat_analysis(ts |> filter(year == 2007), subpop = "ECO_REG_NAME", vars = "TSTATE_TP", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")

TP_2007_ecoreg.1 <- TP_2007_ecoreg |>
  select(Subpopulation, Category, nResp, Estimate.P, StdError.P) |>
  mutate(year = 2007)

# 2012
TP_2012_ecoreg <- cat_analysis(ts |> filter(year == 2012), subpop = "ECO_REG_NAME", vars = "TSTATE_TP", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")

TP_2012_ecoreg.1 <- TP_2012_ecoreg |>
  select(Subpopulation, Category, nResp, Estimate.P, StdError.P) |>
  mutate(year = 2012)

# 2017
TP_2017_ecoreg <- cat_analysis(ts |> filter(year == 2017), subpop = "ECO_REG_NAME", vars = "TSTATE_TP", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")

TP_2017_ecoreg.1 <- TP_2017_ecoreg |>
  select(Subpopulation, Category, nResp, Estimate.P, StdError.P) |>
  mutate(year = 2017)

TP_national <- cat_analysis(ts, vars = "TSTATE_TP", subpop = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")

TP_national.1 <- TP_national|>
  select(Subpopulation, Category, nResp, Estimate.P, StdError.P) |>
  mutate(year = Subpopulation,
         Subpopulation = "National")

TP_trophic_states <- rbind(TP_national.1, TP_2007_ecoreg.1, TP_2012_ecoreg.1, TP_2017_ecoreg.1) |>
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

# 2007
TN_2007_ecoreg <- cat_analysis(ts |> filter(year == 2007), subpop = "ECO_REG_NAME", vars = "TSTATE_TN", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")

TN_2007_ecoreg.1 <- TN_2007_ecoreg |>
  select(Subpopulation, Category, nResp, Estimate.P, StdError.P) |>
  mutate(year = 2007)

# 2012
TN_2012_ecoreg <- cat_analysis(ts |> filter(year == 2012), subpop = "ECO_REG_NAME", vars = "TSTATE_TN", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")

TN_2012_ecoreg.1 <- TN_2012_ecoreg |>
  select(Subpopulation, Category, nResp, Estimate.P, StdError.P) |>
  mutate(year = 2012)

# 2017
TN_2017_ecoreg <- cat_analysis(ts |> filter(year == 2017), subpop = "ECO_REG_NAME", vars = "TSTATE_TN", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")

TN_2017_ecoreg.1 <- TN_2017_ecoreg |>
  select(Subpopulation, Category, nResp, Estimate.P, StdError.P) |>
  mutate(year = 2017)

TN_national <- cat_analysis(ts, vars = "TSTATE_TN", subpop = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")

TN_national.1 <- TN_national|>
  select(Subpopulation, Category, nResp, Estimate.P, StdError.P) |>
  mutate(year = Subpopulation,
         Subpopulation = "National")

TN_trophic_states <- rbind(TN_national.1, TN_2007_ecoreg.1, TN_2012_ecoreg.1, TN_2017_ecoreg.1) |>
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