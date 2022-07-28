# Look at patterns in N and P limited lakes based on the first rule

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
  #select(UNIQUE_ID, SITE_ID, PTL_PPB, NTL_PPM, tn.tp, TSTATE_TP, TSTATE_TN, TROPHIC_STATE, year, WGT_NLA, ECO_REG_NAME, LAT_DD, LON_DD) |>
  #distinct() |>
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

# breakpoints? 
library(segmented)

logged_dat <- all_NLA |>
  mutate(log10chla = log(CHLA_PPB, base = 10),
         log10TP = log(PTL_PPB, base = 10),
         log10TN = log(NTL_PPM, base = 10),
         log10NP = log(tn.tp, base = 10)) |>
  filter(!is.infinite(log10chla))

lm <- lm(log10chla~log10TP, logged_dat)
summary(lm)
seg.lm <- segmented(lm, seg.Z = ~log10TP, psi = 1)
summary(seg.lm)
seg.lm$psi
slope(seg.lm)
fitted.seg <- fitted(seg.lm)
my.model <- data.frame(log10TP = logged_dat$log10TP, log10chla = fitted.seg)

ggplot() +
  geom_point(logged_dat, mapping = aes(log10TP, log10chla)) +
  geom_line(my.model, mapping = aes(log10TP, log10chla)) 
  


lm <- lm(log10chla~log10NP, logged_dat)
summary(lm)
seg.lm <- segmented(lm, seg.Z = ~log10NP, psi = 1)
summary(seg.lm)
seg.lm$psi
slope(seg.lm)
fitted.seg <- fitted(seg.lm)
my.model <- data.frame(log10NP = logged_dat$log10NP, log10chla = fitted.seg)

ggplot() +
  geom_point(logged_dat, mapping = aes(log10NP, log10chla)) +
  geom_line(my.model, mapping = aes(log10NP, log10chla)) 


#### N-limited lakes only ####
n_lim <- all_NLA1_limits |>
  filter(limitation == "N-limited")

mean(n_lim$TN_mol)/mean(n_lim$TP_mol) # 11.13405
log(mean(n_lim$TN_mol)/mean(n_lim$TP_mol), base = 10) # 1.046653

ggplot(n_lim) +
  geom_point(aes(tn.tp, CHLA_PPB)) +
  scale_y_log10() +
  scale_x_log10()



#### P-limited lakes only ####
p_lim <- all_NLA1_limits |>
  filter(limitation == "P-limited")

mean(p_lim$TN_mol)/mean(p_lim$TP_mol) # 105.9128
log(mean(p_lim$TN_mol)/mean(p_lim$TP_mol), base = 10) # 2.024949

summary(all_NLA$DIN_mol)
summary(all_NLA$TN_mol)
summary(all_NLA$TP_mol)

####################################################################################
averages_np <- all_NLA |>
  group_by(ECO_REG_NAME) |>
  summarise(meanNP = (mean(TN_mol)/mean(TP_mol)),
            medianNP = (median(TN_mol)/median(TP_mol)),
            medianTN_PPM = median(NTL_PPM),
            medianTP_PPB = median(PTL_PPB),
            firstQTN_PPM = summary())


ggplot(all_NLA) +
  geom_boxplot(aes(TP_mol, TN_mol, color = ECO_REG_NAME)) +
  scale_y_log10()

limits <- all_NLA |>
  left_join(averages_np)

