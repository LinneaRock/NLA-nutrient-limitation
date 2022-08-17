
# Script to run change analysis to see how/if stoichiometry has shifted over time

# call datasets and libraries 
source("Data/NLA/Call_NLA_data.R")
library(spsurvey)

#### Calculate the limitations first ####
# get some information about the entire dataset
averages_np <- all_NLA |>
  group_by(ECO_REG, year) |>
  mutate(yearlymeanlogNP = mean(log(tn.tp))) |>
  ungroup() |>
  group_by(ECO_REG_NAME) |>
  mutate(
    meanlogNP = mean(log(tn.tp)),
    medianlogNP = median(log(tn.tp)),
    medianTN_PPM = median(NTL_PPM),
    medianTP_PPB = median(PTL_PPB),
    percentile25TN_PPM = quantile(NTL_PPM, probs = 0.25),
    percentile25TP_PPB = quantile(PTL_PPB, probs = 0.25)) |>
  ungroup() |>
  select(year, ECO_REG_NAME, meanlogNP, yearlymeanlogNP, percentile25TN_PPM, percentile25TP_PPB) |>
  distinct()

# uses 25th percentile nutrient thresholds for each ecoregion (over all years) and logged average N:P for each ecoregion (over each year)

limits <- all_NLA |>
  left_join(averages_np) |>
  mutate(limitation = NA) |>
  mutate(limitation = ifelse(PTL_PPB > percentile25TP_PPB & log(tn.tp) < yearlymeanlogNP, "Potential N-limitation", 
                             ifelse(NTL_PPM > percentile25TN_PPM & log(tn.tp) > yearlymeanlogNP, "Potential P-limitation",
                                    ifelse(is.na(limitation), "Potential co-nutrient limitation", limitation))))



#### Prep the data for the change analysis ####

change_dat <- limits|> # total 3066 lakes for this analysis 
  filter(VISIT_NO == 1) |># for this analysis, we are using just the first visit from each lake
  filter(WGT_NLA > 0) # the sp survey package is not designed to use the reference lakes, so those are ignored when using this package for analyses.



### write a function for this change analysis 
stoich_change_fun <- function(data, name, year1, year2) {
  
  change_ecoreg<- change_analysis(data |> filter(year %in% c(year1, year2),
                                                 ECO_REG_NAME == name), siteID = "UNIQUE_ID", vars_cat = "tn.tp", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
  
  change_ecoreg.1 <- change_ecoreg[["catsum"]]  |>
    select(Subpopulation, Category, Indicator, DiffEst.P, StdError.P) |>
    mutate(ECO_REG = name) |>
    rename(Trophic.State = Subpopulation)
  
  
  change_national <- change_analysis(data |> filter(year %in% c(year1, year2)), siteID = "UNIQUE_ID", vars_cat = "tn.tp", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
  
  
  change_national.1 <- change_national[["catsum"]] |>
    select(Subpopulation, Category, Indicator, DiffEst.P, StdError.P) |>
    mutate(ECO_REG = "National") |>
    rename(Trophic.State = Subpopulation) 
  
  final <- bind_rows(change_national.1, change_ecoreg.1)
  
}


#### Run change analysis ####

list <- as.vector(change_dat |> select(ECO_REG_NAME) |> distinct())[["ECO_REG_NAME"]]


### change in nutrient limitations from 2007 to 2012
change0712 <- data.frame()

for(name in list) {
  
  tmp <- stoich_change_fun(change_dat, name, "2007", "2012")
  
  change0712 <- bind_rows(change0712, tmp) |> 
    distinct()
  
}

#check warnings and actions
actions0712 <- data.frame(warn_df$warning, warn_df$action) # repeat sites covariance was not included in standard error, covariance estimate

stoich_change0712 <- change0712 |>
  mutate(year.shift = "2007-2012")



### change in nutrient limitations from 2012 to 2017
change1217 <- data.frame()

for(name in list) {
  
  tmp <- stoich_change_fun(change_dat, name, "2012", "2017")
  
  change1217 <- bind_rows(change1217, tmp) |> 
    distinct()
  
}
#check warnings and actions
actions1217 <- data.frame(warn_df$warning, warn_df$action) # repeat sites covariance was not included in standard error, covariance estimate

stoich_change1217 <- change1217 |>
  mutate(year.shift = "2012-2017")

changes.final <- rbind(lim_change0712, lim_change1217) |>
  mutate(Trophic.State = factor(Trophic.State, levels = c("Oligo.", "Meso.", "Eutro.", "Hyper.")))
