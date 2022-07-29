
##### How does nutrient limitation vary spatially and temporally across the US?  #####



source("Data/NLA/Call_NLA_data.R")
library(colorblindr)


#### N and P limitations from the literature ####

### ATTEMPT 1 ### 
#uses nutrient thresholds for trophic status and full dat average ratio

# uses all data from all years 
ggplot(all_NLA) +
  geom_point(aes(log(PTL_PPB, base = 10), log(tn.tp, base = 10), fill = year), size = 2.5, shape = 21, alpha = 0.5) +
  geom_abline(slope = 0, intercept = log(19, base = 10), linetype = "dashed") + # bergstrom N limitation line
  geom_abline(slope = 0, intercept = log(41, base = 10), linetype = "dashed") + # bergstrom P limitation line
  geom_abline(slope = 0, intercept = log(16, base = 10), color = "red4") + # redfield
  #geom_abline(slope = 0, intercept = log(22.80457, base = 10), linetype = "dotted", color = '#CC5500') + # average N:P based on this data
  #geom_vline(xintercept = log(30, base = 10), color = '#336a98') + # dodds mccauley N limitation P > 30
 # geom_abline(slope = 0, intercept = log(14, base = 10), color = '#336a98') + # dodds mccauley N limitation TN:TP < 14
  geom_abline(slope = 0, intercept = log(38, base = 10), color = '#336a98') + # Sakamoto, 1966; Smith 1982; Rhee 1980, Forsberg 1980  
  geom_abline(slope = 0, intercept = log(22, base = 10), color = '#336a98') + # Sakamoto, 1966; Smith 1982; Rhee 1980, Forsberg 1980
  geom_abline(slope = 0, intercept = log(53, base = 10), color = "#ffc857") + # Ptacnik, 2010
  theme_minimal() +
  scale_fill_manual("Survey Year",values = palette_OkabeIto[5:7]) +
  labs(y = "Log TN:TP", x = "Log TP"~(mu*g~L^-1)) +
  annotate('text', label = 'Redfield 16:1 line', x = 3.5, y = 1.1, hjust = 0, size = 3, color = "red4") +
  annotate('text', label = 'Predicted N limitation from \n below dashed line \n (Bergström, 2010)', x = -1, y = 1, hjust = 0, size = 3) +
  annotate('text', label = 'Predicted P limitation from \n above dashed line \n (Bergström, 2010)', x = 3, y = 2, hjust = 0, size = 3) +
  annotate('text', label = 'Predicted N limitation from \n below blue line \n (Forsberg, 1980; Rhee, 1980; \n Sakamoto, 1966; Smith, 1982)', x = -1, y = 0.25, hjust = 0, size = 3, color = '#336a98') +
  annotate('text', label = 'Predicted P limitation from \n above blue line \n (Forsberg, 1980; Rhee, 1980; \n Sakamoto, 1966; Smith, 1982)', x = 3, y = 3, hjust = 0, size = 3, color = '#336a98') + 
 # annotate('text', label = 'Predicted N limitation from \n (Dodds & McCauley, 1992)', x = 2.5, y = -0.25, hjust = 0, size = 3, color = '#336a98') +
 # annotate('text', label = 'TP = 30'~(mu*g~L^-1), x = 0.9, y = -0.25, hjust = 0, size = 3, color = '#336a98') +
 # annotate('text', label = 'TN:TP = 14', x = 0, y = 1, hjust = 0, size = 3, color = '#336a98') +
 # annotate('text', label = 'Average TN:TP line \n dotted orange', x = -1, y = 1.5, hjust = 0, size = 3, color = '#CC5500')  +
  annotate('text', label = 'Predicted P limitation from \n (Ptacnik et al., 2010)', x = -1, y = 2, hjust = 0, size = 3, color = "#ffc857") 

ggsave("Figures/Q1.Figs/Literature_limitations.png", height = 4.5, width = 6.5, units = "in", dpi = 500)  

## Could I use trophic state based on TN and TP along with average TN:TP ratio to determine 
## which lakes are limited by which nutrients? 
TotalAveNP<- mean(all_NLA$TN_mol)/mean(all_NLA$TP_mol) # 22.5064 
mean(all_NLA$tn.tp) # 59.58256
# limited by P:
# How many oligotrophic based on TP are there? 
# Be sure to select certain criteria to get rid of redundant measurements 
nrow(all_NLA |> select(UNIQUE_ID, SITE_ID, PTL_PPB, NTL_PPM, tn.tp, TSTATE_TP, TSTATE_TN, TROPHIC_STATE) |>
       distinct()|> filter(TSTATE_TP == "OLIGOTROPHIC (<= 10 ug/L)")) #602
# How many lakes are limited by P based on average ratio alone
nrow(all_NLA |> select(UNIQUE_ID, SITE_ID, PTL_PPB, NTL_PPM, tn.tp, TSTATE_TP, TSTATE_TN, TROPHIC_STATE) |>
       distinct()|> filter(tn.tp > 22.80457)) #2689

#limited by N:
# How many oligotrophic based on TP are there? 
nrow(all_NLA |> select(UNIQUE_ID, SITE_ID, PTL_PPB, NTL_PPM, tn.tp, TSTATE_TP, TSTATE_TN, TROPHIC_STATE) |> 
       distinct()|> filter(TSTATE_TN == "OLIGOTROPHIC (<= 0.35 mg/L)")) #952
# How many lakes are limited by P based on average ratio alone
nrow(all_NLA |> select(UNIQUE_ID, SITE_ID, PTL_PPB, NTL_PPM, tn.tp, TSTATE_TP, TSTATE_TN, TROPHIC_STATE) |>
       distinct()|> filter(tn.tp < 22.80457)) #890


#combine those two metrics 
limits <- all_NLA |>
  #select(UNIQUE_ID, SITE_ID, PTL_PPB, NTL_PPM, tn.tp, TSTATE_TP, TSTATE_TN, TROPHIC_STATE, year, WGT_NLA, ECO_REG_NAME, LAT_DD, LON_DD) |>
  #distinct() |>
  mutate(limitation = ifelse(TSTATE_TP == "OLIGOTROPHIC (<= 10 ug/L)" & tn.tp > TotalAveNP, "Potential P-limitation", NA)) |>
  mutate(limitation = ifelse(TSTATE_TN == "OLIGOTROPHIC (<= 0.35 mg/L)" & tn.tp < TotalAveNP, "Potential N-limitation", limitation)) 

nrow(limits |> filter(limitation == "Potential P-limitation")) #607 #when I based this on redfield, there were 551 limited by P
nrow(limits |> filter(limitation == "Potential N-limitation")) #221 #when I based this on redfield, there were 125 limited by N

# Check the graph -- do they generally fall in areas that could be considered N or P limited? 
## Yes they do.... It makes sense to me that limitation should be based on both the absolute amounts and stoichiometry, 
## however, this method leaves a lot of lakes wihtout limitation designation. So what would they be??

ggplot(limits) +
  geom_point(aes(log(PTL_PPB, base = 10), log(tn.tp, base = 10), fill = year), size = 2.5, shape = 21, alpha = 0.8) +
  geom_point(aes(log(PTL_PPB, base = 10), log(tn.tp, base = 10), shape = limitation), size = 2.5,  alpha = 0.8) +
  geom_abline(slope = 0, intercept = log(19, base = 10), linetype = "dashed") + # bergstrom N limitation line
  geom_abline(slope = 0, intercept = log(41, base = 10), linetype = "dashed") + # bergstrom P limitation line
  geom_abline(slope = 0, intercept = log(16, base = 10), color = "red4") + # redfield
  #geom_abline(slope = 0, intercept = log(22.80457, base = 10), linetype = "dotted", color = '#CC5500') + # average N:P based on this data
  #geom_vline(xintercept = log(30, base = 10), color = '#336a98') + # dodds mccauley N limitation P > 30
  # geom_abline(slope = 0, intercept = log(14, base = 10), color = '#336a98') + # dodds mccauley N limitation TN:TP < 14
  geom_abline(slope = 0, intercept = log(38, base = 10), color = '#336a98') + # Sakamoto, 1966; Smith 1982; Rhee 1980, Forsberg 1980  
  geom_abline(slope = 0, intercept = log(22, base = 10), color = '#336a98') + # Sakamoto, 1966; Smith 1982; Rhee 1980, Forsberg 1980
  geom_abline(slope = 0, intercept = log(53, base = 10), color = "#ffc857") +
  theme_minimal() +
  scale_fill_manual("Survey Year",values = palette_OkabeIto[5:7]) +
  scale_shape_manual("Limitation", values = c(3, 4)) +
  labs(y = "Log TN:TP", x = "Log TP"~(mu*g~L^-1)) +
  annotate('text', label = 'Redfield 16:1 line', x = 3.5, y = 1.1, hjust = 0, size = 3, color = "red4") +
  annotate('text', label = 'Predicted N limitation from \n below dashed line \n (Bergström, 2010)', x = -1, y = 1, hjust = 0, size = 3) +
  annotate('text', label = 'Predicted P limitation from \n above dashed line \n (Bergström, 2010)', x = 3, y = 2, hjust = 0, size = 3) +
  annotate('text', label = 'Predicted N limitation from \n below blue line \n (Forsberg, 1980; Rhee, 1980; \n Sakamoto, 1966; Smith, 1982)', x = -1, y = 0.25, hjust = 0, size = 3, color = '#336a98') +
  annotate('text', label = 'Predicted P limitation from \n above blue line \n (Forsberg, 1980; Rhee, 1980; \n Sakamoto, 1966; Smith, 1982)', x = 3, y = 3, hjust = 0, size = 3, color = '#336a98') + 
  # annotate('text', label = 'Predicted N limitation from \n (Dodds & McCauley, 1992)', x = 2.5, y = -0.25, hjust = 0, size = 3, color = '#336a98') +
  # annotate('text', label = 'TP = 30'~(mu*g~L^-1), x = 0.9, y = -0.25, hjust = 0, size = 3, color = '#336a98') +
  # annotate('text', label = 'TN:TP = 14', x = 0, y = 1, hjust = 0, size = 3, color = '#336a98') +
  # annotate('text', label = 'Average TN:TP line \n dotted orange', x = -1, y = 1.5, hjust = 0, size = 3, color = '#CC5500')  +
  annotate('text', label = 'Predicted P limitation from \n (Ptacnik et al., 2010)', x = -1, y = 2, hjust = 0, size = 3, color = "#ffc857") 
ggsave("Figures/Q1.Figs/Limits_attempt1.png", height = 4.5, width = 6.5, units = "in", dpi = 500)  




### ATTEMPT 2 ###  -- note, overwrite the "limits" df
# uses 25th percentile nutrient thresholds for each ecoregion and average N:P for each ecoregion 

# get information about the refernece lakes
ref_np <- all_NLA |>
  filter(SITE_TYPE %in% c("REF_Lake", "HAND")) |> # subset of 230 lakes
  group_by(ECO_REG_NAME) |>
  summarise(meanNP = (mean(TN_mol)/mean(TP_mol)),
            medianNP = (median(TN_mol)/median(TP_mol)),
            medianTN_PPM = median(NTL_PPM),
            medianTP_PPB = median(PTL_PPB))

# get some information about the entire dataset
averages_np <- all_NLA |>
  group_by(ECO_REG_NAME) |>
  summarise(meanNP = (mean(TN_mol)/mean(TP_mol)),
            medianNP = (median(TN_mol)/median(TP_mol)),
            medianTN_PPM = median(NTL_PPM),
            medianTP_PPB = median(PTL_PPB),
            percentile25TN_PPM = quantile(NTL_PPM, probs = 0.25),
            percentile25TP_PPB = quantile(PTL_PPB, probs = 0.25))

# How do the lower 25th percentiles of TN and TP compare the the median  concentrations of TN and TP in the reference lakes? 
t.test(ref_np$medianTN_PPM, averages_np$percentile25TN_PPM) # these are similar to each other!! 
t.test(ref_np$medianTP_PPB, averages_np$percentile25TP_PPB) # these are similar to each other!!

#Try using these values as the thresholds in addition to the average N:P ratios in each ecoregion 

limits <- all_NLA |>
  left_join(averages_np) |>
  mutate(limitation = NA) |>
  mutate(limitation = ifelse(PTL_PPB > percentile25TP_PPB & tn.tp > meanNP, "Potential P-limitation", 
                             ifelse(NTL_PPM > percentile25TN_PPM & tn.tp < meanNP, "Potential N-limitation",
                                    ifelse(is.na(limitation), "Potential co-nutrient limitation", limitation))))

nrow(limits |> filter(limitation == "Potential P-limitation")) # 1699
nrow(limits |> filter(limitation == "Potential N-limitation")) # 848
nrow(limits |> filter(limitation == "Potential co-nutrient limitation")) #1106
# still a lot of leftovers.... but maybe these are co-limited? 


ggplot(limits) +
  geom_point(aes(log(PTL_PPB, base = 10), log(NTL_PPM, base = 10), fill = limitation), size = 2.5, shape = 21, alpha = 0.8) +
  theme_minimal() +
  scale_fill_manual("",values = palette_OkabeIto[5:7]) +
  labs(y = "Log TN"~(m*g~L^-1), x = "Log TP"~(mu*g~L^-1))
ggsave("Figures/Q1.Figs/Limits_attempt2.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 



### ATTEMPT 3 ###  -- note, overwrite the "limits" df
# uses TN:TP limits of 22 and 38 from Sakamoto, 1966; Smith 1982; Rhee 1980, Forsberg 1980  

limits <- all_NLA |>
  mutate(limitation = NA) |>
  mutate(limitation = ifelse(tn.tp > 38, "Potential P-limitation", 
                             ifelse(tn.tp < 22, "Potential N-limitation",
                                    ifelse(is.na(limitation), "Potential co-nutrient limitation", limitation))))

nrow(limits |> filter(limitation == "Potential P-limitation")) # 1858
nrow(limits |> filter(limitation == "Potential N-limitation")) # 854
nrow(limits |> filter(limitation == "Potential co-nutrient limitation")) #941
# still a lot of leftovers.... but maybe these are co-limited? 


ggplot(limits) +
  geom_point(aes(log(PTL_PPB, base = 10), log(NTL_PPM, base = 10), fill = limitation), size = 2.5, shape = 21, alpha = 0.8) +
  theme_minimal() +
  scale_fill_manual("",values = palette_OkabeIto[5:7]) +
  labs(y = "Log TN"~(m*g~L^-1), x = "Log TP"~(mu*g~L^-1)) 
ggsave("Figures/Q1.Figs/Limits_attempt3.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 





#### Limitation shifts over time ####
library(spsurvey) 

# how to the nutrient limitations change from 2007 to 2012?
limits_change_prep <- limits|> # total 3066 lakes for this analysis 
  filter(VISIT_NO == 1) |># for this analysis, we are using just the first visit from each lake
  filter(WGT_NLA > 0) # the sp survey package is not designed to use the reference lakes, so those are ignored when using this package for analyses.


# lakes with limitations considered for this analysis now - the remainder are considered not nutrient limited 
nrow(limits_change_prep |> filter(limitation == "Potential P-limitation")) #1461 
nrow(limits_change_prep |> filter(limitation == "Potential N-limitation")) #729
nrow(limits_change_prep |> filter(limitation == "Potential co-nutrient limitation")) #879


### write a function for this change analysis 
lim_change_fun <- function(data, name, limit_var, year1, year2) {
  
  change_ecoreg<- change_analysis(data |> filter(year %in% c(year1, year2),
                                                 ECO_REG_NAME == name), siteID = "UNIQUE_ID", subpop = "TROPHIC_STATE", vars_cat = limit_var, surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")

change_ecoreg.1 <- change_ecoreg[["catsum"]]  |>
  select(Subpopulation, Category, Indicator, DiffEst.P, StdError.P) |>
  mutate(ECO_REG = name) |>
  rename(Trophic.State = Subpopulation)


change_national <- change_analysis(data |> filter(year %in% c(year1, year2)), siteID = "UNIQUE_ID", subpop = "TROPHIC_STATE", vars_cat = limit_var, surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")


change_national.1 <- change_national[["catsum"]] |>
  select(Subpopulation, Category, Indicator, DiffEst.P, StdError.P) |>
  mutate(ECO_REG = "National") |>
  rename(Trophic.State = Subpopulation) 

final <- bind_rows(change_national.1, change_ecoreg.1)

}

list <- as.vector(limits_change_prep |> select(ECO_REG_NAME) |> distinct())[["ECO_REG_NAME"]]



### change in nutrient limitations from 2007 to 2012
change0712 <- data.frame()

for(name in list) {
 
  tmp <- lim_change_fun(limits_change_prep, name, "limitation", "2007", "2012")
   
  change0712 <- bind_rows(change0712, tmp) |> 
    distinct()
    
}

#check warnings and actions
actions0712 <- data.frame(warn_df$warning, warn_df$action) # repeat sites covariance was not included in standard error, covariance estimate

lim_change0712 <- change0712 |>
  mutate(year.shift = "2007-2012")



### change in nutrient limitations from 2012 to 2017
change1217 <- data.frame()

for(name in list) {
  
  tmp <- lim_change_fun(limits_change_prep, name, "limitation", "2012", "2017")
  
  change1217 <- bind_rows(change1217, tmp) |> 
    distinct()
  
}
#check warnings and actions
actions1217 <- data.frame(warn_df$warning, warn_df$action) # repeat sites covariance was not included in standard error, covariance estimate

lim_change1217 <- change1217 |>
  mutate(year.shift = "2012-2017")

changes.final <- rbind(lim_change0712, lim_change1217) |>
  mutate(Trophic.State = factor(Trophic.State, levels = c("Oligo.", "Meso.", "Eutro.", "Hyper.")))



### create some figures to visualize this analysis 
# National plot for N limitation
ggplot(changes.final |>
         filter(ECO_REG == "National",
                Category == "Potential N-limitation")) +
  geom_point(aes(Trophic.State, DiffEst.P, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Trophic.State, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_minimal() +
  facet_grid(.~Trophic.State, scales = "free_x") +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  theme(axis.text.x = element_blank()) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in potential N-limitation lakes",
       title = "National") 
ggsave("Figures/Q1.Figs/N_limitation_changes_percentdiff_national.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

# ecoregion plots 
ggplot(changes.final |>
         filter(ECO_REG != "National",
                Category == "Potential N-limitation")) +
  geom_point(aes(Trophic.State, DiffEst.P, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Trophic.State, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_minimal() +
  facet_grid(~Trophic.State, scales = "free_x") +
  facet_wrap(~ECO_REG, ncol = 3) +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in potential N-limitation lakes") +
  scale_x_discrete(labels = c("Olig.", "Meso.", "Eutro.", "Hyper."))
ggsave("Figures/Q1.Figs/N_limitation_changes_percentdiff_regional.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

 
# National plot for P limitation
ggplot(changes.final |>
         filter(ECO_REG == "National",
                Category == "Potential P-limitation")) +
  geom_point(aes(Trophic.State, DiffEst.P, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Trophic.State, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_minimal() +
  facet_grid(.~Trophic.State, scales = "free_x") +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  theme(axis.text.x = element_blank()) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in potential P-limitation lakes",
       title = "National") 
ggsave("Figures/Q1.Figs/P_limitation_changes_percentdiff_national.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

# ecoregion plots 
ggplot(changes.final |>
         filter(ECO_REG != "National",
                Category == "Potential P-limitation")) +
  geom_point(aes(Trophic.State, DiffEst.P, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Trophic.State, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_minimal() +
  facet_grid(~Trophic.State, scales = "free_x") +
  facet_wrap(~ECO_REG, ncol = 3) +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in potential P-limitation lakes") +
  scale_x_discrete(labels = c("Olig.", "Meso.", "Eutro.", "Hyper."))
ggsave("Figures/Q1.Figs/P_limitation_changes_percentdiff_regional.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 



# Nationalplot for co-limitation
ggplot(changes.final |>
         filter(ECO_REG == "National",
                Category == "Potential co-nutrient limitation")) +
  geom_point(aes(Trophic.State, DiffEst.P, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Trophic.State, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_minimal() +
  facet_grid(.~Trophic.State, scales = "free_x") +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  theme(axis.text.x = element_blank()) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in potential co-nutrient limitation lakes",
       title = "National") 
ggsave("Figures/Q1.Figs/no_limitation_changes_percentdiff_national.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

# ecoregion plots 
ggplot(changes.final |>
         filter(ECO_REG != "National",
                Category == "Potential co-nutrient limitation")) +
  geom_point(aes(Trophic.State, DiffEst.P, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Trophic.State, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_minimal() +
  facet_grid(~Trophic.State, scales = "free_x") +
  facet_wrap(~ECO_REG, ncol = 3) +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in potential co-nutrient limitation lakes") +
  scale_x_discrete(labels = c("Olig.", "Meso.", "Eutro.", "Hyper."))
ggsave("Figures/Q1.Figs/no_limitation_changes_percentdiff_regional.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 





#### Bar charts of lakes in each trophic state that are N, P, not limited in each ecoregion ####
## take into account the weights for proportions
## plots show what percentage of N, P, or Potential co-nutrient limitation lakes are which trophic state
weighted_limits <- limits |>
  group_by(year, ECO_REG_NAME, TROPHIC_STATE, limitation) |>
  mutate(weighted_lim = sum(WGT_NLA)) |>
  ungroup() |>
  select(year, ECO_REG_NAME, TROPHIC_STATE, limitation, weighted_lim) |>
  distinct() |>
  group_by(year, ECO_REG_NAME, limitation) |>
  mutate(weighted_total = sum(weighted_lim)) |>
  mutate(prop = (weighted_lim/weighted_total) * 100) |>
  ungroup() |>
  drop_na(prop) |>
  mutate(limitation = ifelse(is.na(limitation), "Potential co-nutrient limitation", limitation))

proportional_columns <- function( year1, filename) {
  ggplot(weighted_limits |> filter( year == year1)) +
    geom_col(aes(limitation, prop, fill = TROPHIC_STATE)) +
    theme_bw() +
    scale_fill_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1])) +
    labs(x = "",
         y = "% lakes in each limitation type")  +
    facet_wrap(~ECO_REG_NAME, ncol = 3) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  ggsave(paste("Figures/Q1.Figs/", filename, ".png", sep = ""), height = 4.5, width = 6.5, units = "in", dpi = 500)
  
}

# create the grap
proportional_columns("2007", "Limitations_2007")
proportional_columns("2012", "Limitations_2012")
proportional_columns("2017", "Limitations_2017")

