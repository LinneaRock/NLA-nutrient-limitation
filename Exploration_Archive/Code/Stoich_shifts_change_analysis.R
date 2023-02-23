
# Script to run change analysis to see how/if stoichiometry has shifted over time

# call datasets and libraries 
source("Data/NLA/Call_NLA_data.R")
library(spsurvey)

source("Data/NLA/Call_NLA_data.R")
nla_data_subset <- all_NLA |>
  select(ECO_REG_NAME, UNIQUE_ID, DATE_COL, VISIT_NO, NTL_PPM, PTL_PPB, DIN_PPM, tn.tp, DIN.TP, CHLA_PPB, TROPHIC_STATE, year,WGT_NLA, LON_DD, LAT_DD, AREA_HA, ELEV_PT, PCT_DEVELOPED_BSN, PCT_AGRIC_BSN, SITE_TYPE, URBAN, LAKE_ORIGIN) |>
  rename(DIN.TP_molar = DIN.TP,
         TN.TP_molar = tn.tp) |>
  filter(year != "2012") |>
  filter(AREA_HA >= 4) |> # removes 216 observations 
  distinct()

all_NLA <- nla_data_subset


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

change_dat <- limits|> # total 3066 lakes for this analysis 1773 - lakes with new dataset
  filter(VISIT_NO == 1) |># for this analysis, we are using just the first visit from each lake
  filter(WGT_NLA > 0) # the sp survey package is not designed to use the reference lakes, so those are ignored when using this package for analyses.

###########################################################################

#### write a function for this change analysis ####
stoich_change_fun <- function(data, name, year1, year2) {
  
  change_ecoreg<- change_analysis(data |> filter(year %in% c(year1, year2),
                                                 ECO_REG_NAME == name), siteID = "UNIQUE_ID",
                                vars_cont = "TN.TP_molar", surveyID = "year", weight = "WGT_NLA",
                                xcoord = "LON_DD", ycoord = "LAT_DD")
  
  change_ecoreg.1 <- change_ecoreg[["contsum_mean"]]  |>
    dplyr::select(Subpopulation, Indicator, DiffEst, StdError) |>
    mutate(ECO_REG = name) #|>
    #rename(Trophic.State = Subpopulation)
  
  
  change_national <- change_analysis(data |> filter(year %in% c(year1, year2)), siteID = "UNIQUE_ID", 
                                     vars_cont =  "TN.TP_molar", surveyID = "year", weight = "WGT_NLA", 
                                     xcoord = "LON_DD", ycoord = "LAT_DD")
  
  
  change_national.1 <- change_national[["contsum_mean"]] |>
    dplyr::select(Subpopulation, Indicator, DiffEst, StdError) |>
    mutate(ECO_REG = "National") #|>
   # rename(Trophic.State = Subpopulation) 
  
  final <- bind_rows(change_national.1, change_ecoreg.1)
  
}


#### Run change analysis ####

list <- as.vector(change_dat |> select(ECO_REG_NAME) |> distinct())[["ECO_REG_NAME"]]


### change in nutrient limitations from 2007 to 2017
change0717 <- data.frame()

for(name in list) {
  
  tmp <- stoich_change_fun(change_dat, name, "2007", "2017")
  
  change0717 <- bind_rows(change0717, tmp) |> 
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

changes.final <- rbind(stoich_change0712, stoich_change1217) #|>
  #mutate(Trophic.State = factor(Trophic.State, levels = c("Oligo.", "Meso.", "Eutro.", "Hyper.")))


###########################################################################

#### write a function for this change analysis now with trophic state ####
stoich_change_fun <- function(data, name, year1, year2) {
  
  change_ecoreg<- change_analysis(data |> filter(year %in% c(year1, year2),
                                                 ECO_REG_NAME == name), siteID = "UNIQUE_ID", subpop = "TROPHIC_STATE", vars_cont = "tn.tp", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
  
  change_ecoreg.1 <- change_ecoreg[["contsum_mean"]]  |>
    dplyr::select(Subpopulation, Indicator, DiffEst, StdError) |>
    mutate(ECO_REG = name) |>
  rename(Trophic.State = Subpopulation)
  
  
  change_national <- change_analysis(data |> filter(year %in% c(year1, year2)), siteID = "UNIQUE_ID", subpop = "TROPHIC_STATE", vars_cont = "tn.tp", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
  
  
  change_national.1 <- change_national[["contsum_mean"]] |>
    dplyr::select(Subpopulation, Indicator, DiffEst, StdError) |>
    mutate(ECO_REG = "National") |>
   rename(Trophic.State = Subpopulation) 
  
  final <- bind_rows(change_national.1, change_ecoreg.1)
  
}


#### Run change analysis ####

list <- as.vector(change_dat |> select(ECO_REG_NAME) |> distinct())[["ECO_REG_NAME"]]


### change in nutrient limitations from 2007 to 2012
change0712ts <- data.frame()

for(name in list) {
  
  tmp <- stoich_change_fun(change_dat, name, "2007", "2012")
  
  change0712ts <- bind_rows(change0712ts, tmp) |> 
    distinct()
  
}

#check warnings and actions
actions0712ts <- data.frame(warn_df$warning, warn_df$action) # repeat sites covariance was not included in standard error, covariance estimate

stoich_change0712ts <- change0712ts |>
  mutate(year.shift = "2007-2012")



### change in nutrient limitations from 2012 to 2017
change1217ts <- data.frame()

for(name in list) {
  
  tmp <- stoich_change_fun(change_dat, name, "2012", "2017")
  
  change1217ts <- bind_rows(change1217ts, tmp) |> 
    distinct()
  
}

#check warnings and actions
actions1217ts <- data.frame(warn_df$warning, warn_df$action) # repeat sites covariance was not included in standard error, covariance estimate

stoich_change1217ts <- change1217ts |>
  mutate(year.shift = "2012-2017")

changes.finalts <- rbind(stoich_change0712ts, stoich_change1217ts) |>
  mutate(Trophic.State = factor(Trophic.State, levels = c("Oligo.", "Meso.", "Eutro.", "Hyper.")))

###########################################################################


#### write a function for this change analysis now with limitation ####
stoich_change_fun <- function(data, name, year1, year2) {
  
  change_ecoreg<- change_analysis(data |> filter(year %in% c(year1, year2),
                                                 ECO_REG_NAME == name), siteID = "UNIQUE_ID", subpop = "limitation", vars_cont = "tn.tp", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
  
  change_ecoreg.1 <- change_ecoreg[["contsum_mean"]]  |>
    dplyr::select(Subpopulation, Indicator, DiffEst, StdError) |>
    mutate(ECO_REG = name) |>
    rename(Limitation = Subpopulation)
  
  
  change_national <- change_analysis(data |> filter(year %in% c(year1, year2)), siteID = "UNIQUE_ID", subpop = "limitation", vars_cont = "tn.tp", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
  
  
  change_national.1 <- change_national[["contsum_mean"]] |>
    dplyr::select(Subpopulation, Indicator, DiffEst, StdError) |>
    mutate(ECO_REG = "National") |>
    rename(Limitation = Subpopulation) 
  
  final <- bind_rows(change_national.1, change_ecoreg.1)
  
}


#### Run change analysis ####

list <- as.vector(change_dat |> select(ECO_REG_NAME) |> distinct())[["ECO_REG_NAME"]]


### change in nutrient limitations from 2007 to 2012
change0712lim <- data.frame()

for(name in list) {
  
  tmp <- stoich_change_fun(change_dat, name, "2007", "2012")
  
  change0712lim <- bind_rows(change0712lim, tmp) |> 
    distinct()
  
}

#check warnings and actions
actions0712lim <- data.frame(warn_df$warning, warn_df$action) # repeat sites covariance was not included in standard error, covariance estimate

stoich_change0712lim <- change0712lim |>
  mutate(year.shift = "2007-2012")



### change in nutrient limitations from 2012 to 2017
change1217lim <- data.frame()

for(name in list) {
  
  tmp <- stoich_change_fun(change_dat, name, "2012", "2017")
  
  change1217lim <- bind_rows(change1217lim, tmp) |> 
    distinct()
  
}
#check warnings and actions
actions1217lim <- data.frame(warn_df$warning, warn_df$action) # repeat sites covariance was not included in standard error, covariance estimate

stoich_change1217lim <- change1217lim |>
  mutate(year.shift = "2012-2017")

changes.finallim <- rbind(stoich_change0712lim, stoich_change1217lim) 


###########################################################################

#### create some figures to visualize this analysis ####
# National plot for overall shifts
ggplot(changes.final |>
         filter(ECO_REG == "National")) +
  geom_point(aes(year.shift, DiffEst, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(year.shift, DiffEst, ymin = DiffEst-StdError, ymax = DiffEst+StdError, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_minimal() +
  facet_grid(.~year.shift, scales = "free_x") +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  theme(axis.text.x = element_blank()) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in N:P stoichiometry",
       title = "National") 
ggsave("Figures/Qtrend.Figs/overall_stoich_shift_national.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

# ecoregion plots for overall shift
ggplot(changes.final |>
         filter(ECO_REG != "National")) +
  geom_point(aes(year.shift, DiffEst, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(year.shift, DiffEst, ymin = DiffEst-StdError, ymax = DiffEst+StdError, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_bw() +
  facet_grid(~year.shift, scales = "free_x") +
  facet_wrap(~ECO_REG, ncol = 3, scales = "free_y") +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in N:P stoichiometry")
ggsave("Figures/Qtrend.Figs/overall_stoich_shift_regional.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 


#*******************************************************************# 

ggplot(changes.finalts |>
         filter(ECO_REG == "National")) +
  geom_point(aes(Trophic.State, DiffEst, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Trophic.State, DiffEst, ymin = DiffEst-StdError, ymax = DiffEst+StdError, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_bw() +
  facet_grid(.~Trophic.State, scales = "free_x") +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  theme(axis.text.x = element_blank()) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in N:P stoichiometry",
       title = "National") 
ggsave("Figures/Qtrend.Figs/ts_stoich_shift_national.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

# ecoregion plots 
ggplot(changes.finalts |>
         filter(ECO_REG != "National")) +
  geom_point(aes(Trophic.State, DiffEst, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Trophic.State, DiffEst, ymin = DiffEst-StdError, ymax = DiffEst+StdError, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_bw() +
  facet_grid(~Trophic.State) +
  facet_wrap(~ECO_REG, ncol = 3, scales = "free_y") +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in N:P stoichiometry") +
  scale_x_discrete(labels = c("Olig.", "Meso.", "Eutro.", "Hyper."))
ggsave("Figures/Qtrend.Figs/ts_stoich_shift_regional.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 



#*******************************************************************# 

ggplot(changes.finallim |>
         filter(ECO_REG == "National") |>
         mutate(Limitation = ifelse(Limitation == "Potential co-nutrient limitation", "Potential co-limitation", Limitation))) +
  geom_point(aes(Limitation, DiffEst, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Limitation, DiffEst, ymin = DiffEst-StdError, ymax = DiffEst+StdError, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_bw() +
  facet_grid(.~Limitation, scales = "free_x") +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  theme(axis.text.x = element_blank()) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in N:P stoichiometry",
       title = "National") 
ggsave("Figures/Qtrend.Figs/lim_stoich_shift_national.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

# ecoregion plots 
ggplot(changes.finallim |>
         filter(ECO_REG != "National") |>
         mutate(Limitation = ifelse(Limitation == "Potential co-nutrient limitation", "Potential co-limitation", Limitation))) +
  geom_point(aes(Limitation, DiffEst, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Limitation, DiffEst, ymin = DiffEst-StdError, ymax = DiffEst+StdError, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_bw() +
  facet_grid(~Limitation) +
  facet_wrap(~ECO_REG, ncol = 3, scales = "free_y") +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in N:P stoichiometry") +
  scale_x_discrete(labels = c("Co-lim", "N-lim", "P-lim"))
 # theme(axis.text.x = element_text(angle = 45))
ggsave("Figures/Qtrend.Figs/lim_stoich_shift_regional.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 
