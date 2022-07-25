################################################################################
############## How do nutrient limitation/enrichment  vary across ############## 
############## ecoregions and what are the underlying mechanisms? ############## 
################################################################################


source("Data/NLA/Call_NLA_data.R")
library(colorblindr)


all_NLA1 <- all_NLA |>
  filter(VISIT_NO == 1) # for this analysis, we are using just the first visit from each lake

#### N and P limitations from the literature ####

#### Looking at the NLA data with N and P limits from Bergström, 2010. 
# The predicted limitations are above/below the dotted lines. The red line is the Redfield 16:1 line. 
#### From Downing and McCauley, 1992. N limitation singificantly occurs when TN:TP <= 14 and when TP > 30 ug/L
#### Also adding a line for the average N:P for these data
mean(all_NLA1$TN_mol)/mean(all_NLA1$TP_mol) # 22.80457 <- use this one!!!
mean(all_NLA1$tn.tp) # 59.58256


# uses all data from all years 
ggplot(all_NLA1) +
  geom_point(aes(log(PTL_PPB, base = 10), log(tn.tp, base = 10), fill = year), size = 2.5, shape = 21, alpha = 0.8) +
  geom_abline(slope = 0, intercept = log(19, base = 10), linetype = "dashed") + # bergstrom N limitation line
  geom_abline(slope = 0, intercept = log(41, base = 10), linetype = "dashed") + # bergstrom P limitation line
  geom_abline(slope = 0, intercept = log(16, base = 10), color = "red4") + # redfield
  geom_abline(slope = 0, intercept = log(22.80457, base = 10), linetype = "dotted", color = '#CC5500') + # average N:P based on this data
  geom_vline(xintercept = log(30, base = 10), color = '#336a98') + # dodds mccauley N limitation P > 30
  geom_abline(slope = 0, intercept = log(14, base = 10), color = '#336a98') + # dodds mccauley N limitation TN:TP < 14
  theme_minimal() +
  scale_fill_manual("Year",values = palette_OkabeIto[5:7]) +
  labs(y = "Log TN:TP", x = "Log TP"~(mu*g~L^-1)) +
  annotate('text', label = 'Redfield 16:1 line', x = 3.5, y = 1.1, hjust = 0, size = 3, color = "red4") +
  annotate('text', label = 'Predicted N limitation \n below dashed line \n (Bergström, 2010)', x = -1, y = 1, hjust = 0, size = 3) +
  annotate('text', label = 'Predicted P limitation \n above dashed line \n (Bergström, 2010)', x = 3, y = 2, hjust = 0, size = 3) + 
  annotate('text', label = 'N limitation likely \n (Dodds & McCauley, 1992)', x = 2.5, y = -0.25, hjust = 0, size = 3) +
  annotate('text', label = 'TP = 30'~(mu*g~L^-1), x = 0.9, y = -0.25, hjust = 0, size = 3, color = '#336a98') +
  annotate('text', label = 'TN:TP = 14', x = 0, y = 1, hjust = 0, size = 3, color = '#336a98') +
  annotate('text', label = 'Average TN:TP line \n dotted orange', x = -1, y = 1.5, hjust = 0, size = 3, color = '#CC5500') 


## Could I use trophic state based on TN and TP along with Redfield TN:TP ratio to determine 
## which lakes are limited by which nutrients? 

# limited by P:
# How many oligotrophic based on TP are there? 
# Be sure to select certain criteria to get rid of redundant measurements 
nrow(all_NLA1 |> select(UNIQUE_ID, SITE_ID, PTL_PPB, NTL_PPM, tn.tp, TSTATE_TP, TSTATE_TN, TROPHIC_STATE) |>
       distinct()|> filter(TSTATE_TP == "OLIGOTROPHIC (<= 10 ug/L)")) #558
# How many lakes are limited by P based on average ratio alone
nrow(all_NLA1 |> select(UNIQUE_ID, SITE_ID, PTL_PPB, NTL_PPM, tn.tp, TSTATE_TP, TSTATE_TN, TROPHIC_STATE) |>
       distinct()|> filter(tn.tp > 22.80457)) #2483

#limited by N:
# How many oligotrophic based on TP are there? 
nrow(all_NLA1 |> select(UNIQUE_ID, SITE_ID, PTL_PPB, NTL_PPM, tn.tp, TSTATE_TP, TSTATE_TN, TROPHIC_STATE) |> 
       distinct()|> filter(TSTATE_TN == "OLIGOTROPHIC (<= 0.35 mg/L)")) #877
# How many lakes are limited by P based on average ratio alone
nrow(all_NLA1 |> select(UNIQUE_ID, SITE_ID, PTL_PPB, NTL_PPM, tn.tp, TSTATE_TP, TSTATE_TN, TROPHIC_STATE) |>
       distinct()|> filter(tn.tp < 22.80457)) #804


#combine those two metrics 
all_NLA1_limits <- all_NLA1 |>
  select(UNIQUE_ID, SITE_ID, PTL_PPB, NTL_PPM, tn.tp, TSTATE_TP, TSTATE_TN, TROPHIC_STATE, year, WGT_NLA, ECO_REG_NAME, LAT_DD, LON_DD) |>
  distinct() |>
  mutate(limitation = ifelse(TSTATE_TP == "OLIGOTROPHIC (<= 10 ug/L)" & tn.tp > 22.80457, "P-limited", NA)) |>
  mutate(limitation = ifelse(TSTATE_TN == "OLIGOTROPHIC (<= 0.35 mg/L)" & tn.tp < 22.80457, "N-limited", limitation)) 

nrow(all_NLA1_limits |> filter(limitation == "P-limited")) #541 #when I based this on redfield, there were 551 limited by P
nrow(all_NLA1_limits |> filter(limitation == "N-limited")) #208 #when I based this on redfield, there were 125 limited by N

# Check the graph -- do they generally fall in areas that could be considered N or P limited? 
## Yes they do.... And I think this makes sense. When I look at some of the lakes that are in the "N-limitation likely" 
#### based on D & M, some are hypereutrophic based on the absolute N concentration. It makes sense to me that limitation
 #### should be based on both the absolute amounts and stoichiometry. 

ggplot(all_NLA1_limits) +
  geom_point(aes(log(PTL_PPB, base = 10), log(tn.tp, base = 10), fill = year), size = 2.5, shape = 21, alpha = 0.8) +
  geom_point(aes(log(PTL_PPB, base = 10), log(tn.tp, base = 10), shape = limitation), size = 2.5,  alpha = 0.8) +
  geom_abline(slope = 0, intercept = log(19, base = 10), linetype = "dashed") + # bergstrom N limitation line
  geom_abline(slope = 0, intercept = log(41, base = 10), linetype = "dashed") + # bergstrom P limitation line
  geom_abline(slope = 0, intercept = log(16, base = 10), color = "red4") + # redfield
  geom_abline(slope = 0, intercept = log(22.5032, base = 10), color = '#CC5500') + # average N:P based on this data
  geom_vline(xintercept = log(30, base = 10), color = '#336a98') + # dodds mccauley N limitation P > 30
  geom_abline(slope = 0, intercept = log(14, base = 10), color = '#336a98') + # dodds mccauley N limitation TN:TP < 14
  theme_minimal() +
  scale_fill_manual("Survey Year",values = palette_OkabeIto[5:7]) +
  scale_shape_manual("Limitation", values = c(3, 4)) +
  labs(y = "Log TN:TP", x = "Log TP"~(mu*g~L^-1)) +
  annotate('text', label = 'Redfield 16:1 line', x = 3.5, y = 1.1, hjust = 0, size = 2, color = "red4") +
  annotate('text', label = 'Predicted N limitation \n below dotted line \n (Bergström, 2010)', x = -1, y = 0.9, hjust = 0, size = 2) +
  annotate('text', label = 'Predicted P limitation \n above dotted line \n (Bergström, 2010)', x = 3, y = 2, hjust = 0, size = 2) + 
  annotate('text', label = 'N limitation likely \n (Dodds & McCauley, 1992)', x = 2.5, y = -0.25, hjust = 0, size = 2) +
  annotate('text', label = 'TP = 30'~(mu*g~L^-1), x = 0.9, y = -0.25, hjust = 0, size = 2, color = '#336a98') +
  annotate('text', label = 'TN:TP = 14', x = 0, y = 1, hjust = 0, size = 2, color = '#336a98')   +
  annotate('text', label = 'Average TN:TP line', x = -1, y = 1.5, hjust = 0, size = 2, color = '#CC5500') 

ggsave("Figures/Q1.Figs/P_N_Limitation_Thresholds.png", height = 4.5, width = 6.5, units = "in", dpi = 500)  

# note, there are 10 samples that do not have trophic state based on chlorophyll associated with them. 2 are N-limited, 1 is P-limited


#### Limitation shifts over time ####
library(spsurvey)

# how to the nutrient limitations change from 2007 to 2012?
limits_change_prep <- all_NLA1_limits |>
  filter(WGT_NLA > 0) |> # the sp survey package is not designed to use the reference lakes, so those are ignored when using this package for analyses.
  mutate(limitation = ifelse(is.na(limitation), "No limitation", limitation))

# lakes with limitations considered for this analysis now - the remainder are considered not nutrient limited 
nrow(limits_change_prep |> filter(limitation == "P-limited")) #454 
nrow(limits_change_prep |> filter(limitation == "N-limited")) #200



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
                Category == "N-limited")) +
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
       y = "% Difference in N-limited lakes",
       title = "National") 
ggsave("Figures/Q1.Figs/N_limitation_changes_percentdiff_national.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

# ecoregion plots 
ggplot(changes.final |>
         filter(ECO_REG != "National",
                Category == "N-limited")) +
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
       y = "% Difference in N-limited lakes") +
  scale_x_discrete(labels = c("Olig.", "Meso.", "Eutro.", "Hyper."))
ggsave("Figures/Q1.Figs/N_limitation_changes_percentdiff_regional.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

 
# National plot for P limitation
ggplot(changes.final |>
         filter(ECO_REG == "National",
                Category == "P-limited")) +
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
       y = "% Difference in P-limited lakes",
       title = "National") 
ggsave("Figures/Q1.Figs/P_limitation_changes_percentdiff_national.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

# ecoregion plots 
ggplot(changes.final |>
         filter(ECO_REG != "National",
                Category == "P-limited")) +
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
       y = "% Difference in P-limited lakes") +
  scale_x_discrete(labels = c("Olig.", "Meso.", "Eutro.", "Hyper."))
ggsave("Figures/Q1.Figs/P_limitation_changes_percentdiff_regional.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 



# Nationalplot for no limitation
ggplot(changes.final |>
         filter(ECO_REG == "National",
                Category == "No limitation")) +
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
       y = "% Difference in non-limited lakes",
       title = "National") 
ggsave("Figures/Q1.Figs/no_limitation_changes_percentdiff_national.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

# ecoregion plots 
ggplot(changes.final |>
         filter(ECO_REG != "National",
                Category == "No limitation")) +
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
       y = "% Difference in non-limited lakes") +
  scale_x_discrete(labels = c("Olig.", "Meso.", "Eutro.", "Hyper."))
ggsave("Figures/Q1.Figs/no_limitation_changes_percentdiff_regional.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 





#### Bar graphs showing number/proportion of lakes in each trophic state that are N, P, not limited in each ecoregion ####

#nitrogen limited
ggplot(all_NLA1_limits |> filter(limitation == "N-limited")) +
  geom_bar(aes(ECO_REG_NAME, fill = TROPHIC_STATE)) +
  theme_minimal() +
  scale_fill_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1])) +
  labs(x = "",
       y = "No. lakes in each trophic category",
       title = "N-limited lakes only")  +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Figures/Q1.Figs/N_lim_numbers.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

#phosphorus limited
ggplot(all_NLA1_limits |> filter(limitation == "P-limited")) +
  geom_bar(aes(ECO_REG_NAME, fill = TROPHIC_STATE)) +
  theme_minimal() +
  scale_fill_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1])) +
  labs(x = "",
       y = "No. lakes in each trophic category",
       title = "P-limited lakes only")  +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Figures/Q1.Figs/P_lim_numbers.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

#non-limited
ggplot(all_NLA1_limits |> filter(is.na(limitation))) +
  geom_bar(aes(ECO_REG_NAME, fill = TROPHIC_STATE)) +
  theme_minimal() +
  scale_fill_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1])) +
  labs(x = "",
       y = "No. lakes in each trophic category",
       title = "Non-limited lakes only")  +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("Figures/Q1.Figs/no_lim_numbers.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 
