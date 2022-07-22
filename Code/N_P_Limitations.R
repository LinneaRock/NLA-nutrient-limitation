
### N and P limitations from the literature ####

source("C:/Users/lrock1/Downloads/PhD_code/STOICH_NARSchallenge/Data/NLA/Call_NLA_data.R")
library(colorblindr)

#### Looking at the NLA data with N and P limits from Bergström, 2010. 
# The predicted limitations are above/below the dotted lines. The red line is the Redfield 16:1 line. 
#### From Downing and McCauley, 1992. N limitation singificantly occurs when TN:TP <= 14 and when TP > 30 ug/L
#### Also adding a line for the average N:P for these data
mean(all_NLA$TN_mol)/mean(all_NLA$TP_mol) # 22.52032 <- use this one!!!
mean(all_NLA$tn.tp) # 59.0166


# uses all data from all years 
ggplot(all_NLA) +
  geom_point(aes(log(PTL_PPB, base = 10), log(tn.tp, base = 10), fill = year), size = 2.5, shape = 21, alpha = 0.8) +
  geom_abline(slope = 0, intercept = log(19, base = 10), linetype = "dashed") + # bergstrom N limitation line
  geom_abline(slope = 0, intercept = log(41, base = 10), linetype = "dashed") + # bergstrom P limitation line
  geom_abline(slope = 0, intercept = log(16, base = 10), color = "red4") + # redfield
  geom_abline(slope = 0, intercept = log(22.5032, base = 10), linetype = "dotted", color = '#CC5500') + # average N:P based on this data
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
nrow(all_NLA |> filter(TSTATE_TP == "OLIGOTROPHIC (<= 10 ug/L)")) #623
# How many lakes are limited by P based on average ratio
nrow(all_NLA |> filter(tn.tp > 22.52032)) #2782

#limited by N:
# How many oligotrophic based on TP are there? 
nrow(all_NLA |> filter(TSTATE_TN == "OLIGOTROPHIC (<= 0.35 mg/L)")) #971
# How many lakes are limited by P based on average ratio
nrow(all_NLA |> filter(tn.tp < 22.52032)) #881


#combine those two metrics 
all_NLA_limits <- all_NLA |>
  mutate(limitation = ifelse(TSTATE_TP == "OLIGOTROPHIC (<= 10 ug/L)" & tn.tp > 22.52032, "P-limited", NA)) |>
  mutate(limitation = ifelse(TSTATE_TN == "OLIGOTROPHIC (<= 0.35 mg/L)" & tn.tp < 22.52032, "N-limited", limitation))

nrow(all_NLA_limits |> filter(limitation == "P-limited")) #608 #when I based this on redfield, there were 616 limited by P
nrow(all_NLA_limits |> filter(limitation == "N-limited")) #218 #when I based this on redfield, there were 138 limited by N

# Check the graph -- do they generally fall in areas that could be considered N or P limited? 
## Yes they do.... And I think this makes sense. When I look at some of the lakes that are in the "N-limitation likely" 
#### based on D & M, some are hypereutrophic based on the absolute N concentration. It makes sense to me that limitation
 #### should be based on both the absolute amounts and stoichiometry. 

ggplot(all_NLA_limits) +
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
  annotate('text', label = 'Predicted N limitation \n below dotted line \n (Bergström, 2010)', x = -1, y = 1, hjust = 0, size = 2) +
  annotate('text', label = 'Predicted P limitation \n above dotted line \n (Bergström, 2010)', x = 3, y = 2, hjust = 0, size = 2) + 
  annotate('text', label = 'N limitation likely \n (Dodds & McCauley, 1992)', x = 2.5, y = -0.25, hjust = 0, size = 2) +
  annotate('text', label = 'TP = 30'~(mu*g~L^-1), x = 0.9, y = -0.25, hjust = 0, size = 2, color = '#336a98') +
  annotate('text', label = 'TN:TP = 14', x = 0, y = 1, hjust = 0, size = 2, color = '#336a98')   +
  annotate('text', label = 'Average TN:TP orange line', x = -1, y = 1.5, hjust = 0, size = 2, color = '#CC5500') 

ggsave("Figures/P_N_Limitation_Thresholds.jpg", height = 4.5, width = 6.5, units = "in", dpi = 500)  

