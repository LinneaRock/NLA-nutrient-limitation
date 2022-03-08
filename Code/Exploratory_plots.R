#looking at some figures

source("Code/Call_data.R")
library(colorblindr)


#TN:TP
ggplot(nla2007_tntp) +
  geom_point(aes(log(PTL_PPB/Pmol), log(NTL_PPM/Nmol))) +
  geom_abline(slope = 16, intercept = 0) 

#NO3:TP
ggplot(nla2007_tntp) +
  geom_point(aes((PTL_PPB/Pmol), (NO3N_PPM/Nmol))) +
  geom_abline(slope = 16, intercept = 0) 

#NH4:TP 
ggplot(nla2007_tntp) +
  geom_point(aes((PTL_PPB/Pmol), (NH4N_PPM/Nmol))) +
  geom_abline(slope = 16, intercept = 0) 

#NH4+NO3:TP
ggplot(nla2007_tntp) +
  geom_point(aes((PTL_PPB/Pmol), ((NH4N_PPM + NO3N_PPM)/Nmol))) +
  geom_abline(slope = 16, intercept = 0) 


######################plotting TN vs NO3 + NH4###################################
#the commented out lines are creating a figure with lines connecting the paired points,
#so TN and DIN in the same sampling location. This was really messy graph though so I'm 
#scrapping it

# longWQ <- nla2007_tntp |>
#   mutate(TN = NTL_PPM,
#          `NH4 + NO3` = NH4N_PPM + NO3N_PPM) |>
#   pivot_longer(cols = c(TN, `NH4 + NO3`), names_to = "NitrogenForms", values_to = "N_PPM")

# ggplot(longWQ, aes((PTL_PPB/Pmol), (N_PPM/Nmol))) +
#   geom_jitter(aes(color = NitrogenForms), size = 2.5, shape = 21, alpha = 0.4) +
#   geom_abline(slope = 16, intercept = 0) +
#   #geom_path(aes(group = SITE_ID)) +
#   geom_abline(slope = 16, intercept = 0) +
#   theme_light() +
#   scale_color_manual(values = palette_OkabeIto[1:2]) +
#   theme(legend.title = element_blank()) +
#   labs(x = "Phosphorus (molar)", y = "Nitrogen (molar)") +
#   annotate('text', label = 'Redfield 16:1 line', x = 0.0001, y = 0.0015, hjust = 0, size = 4) 


#N:P figure 
ggplot(nla2007_tntp) +
  geom_point(aes((PTL_PPB/Pmol), (NTL_PPM/Nmol), fill = 'TN'), size = 2.5, shape = 21, alpha = 0.4) +
  geom_abline(slope = 16, intercept = 0) +
  geom_jitter(aes((PTL_PPB/Pmol), ((NH4N_PPM + NO3N_PPM)/Nmol), fill = 'NH4 + NO3'), size = 2.5, shape = 21, alpha = 0.4) +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[1:2]) +
  theme(legend.title = element_blank()) +
  labs(x = "Phosphorus (molar)", y = "Nitrogen (molar)") +
  annotate('text', label = 'Redfield 16:1 line', x = 0.0001, y = 0.0015, hjust = 0, size = 4)

######################CHLOROPHYLL RELATIONSHIPS#################################

#TN:TP vs chlorophylla
ggplot(nla2007_tntp) +
  geom_point(aes(log(CHLA_PPB), log(tn.tp))) +
  theme_light() +
  labs(y = 'ln(TN:TP) - molar ratio',
       x = 'ln(Chl-a) - ppb')
summary(lm(log(CHLA_PPB)~log(tn.tp), nla2007_tntp)) #r = 0.1577 


#NO3:TP vs chlorophylla
ggplot(nla2007_tntp) +
  geom_point(aes(log(CHLA_PPB), log(no3.tp)))
summary(lm(log(CHLA_PPB)~log(no3.tp), nla2007_tntp)) #r = 0.3225


#NH4:TP vs chlorophylla
ggplot(nla2007_tntp) +
  geom_point(aes(log(CHLA_PPB), log(nh4.tp)))
summary(lm(log(CHLA_PPB)~log(nh4.tp), nla2007_tntp)) #r = 0.3377 


#NH4 + NO3:TP vs chlorophylla
ggplot(nla2007_tntp) +
  geom_point(aes(log(CHLA_PPB), log(`nh4+no3.tp`))) +
  theme_light() +
  labs(y = 'ln(NH4 + NO3:TP) - molar ratio',
       x = 'ln(Chl-a) - ppb')

summary(lm(log(CHLA_PPB)~log(`nh4+no3.tp`), nla2007_tntp)) #r = 0.3234  


#############################trophic states####################################
###P classificaiton###################################################
#TN:TP -- P trophic state
ggplot(nla2007_tntp, aes(TSTATE_TP, tn.tp, fill = TSTATE_TP)) +
  geom_violin() +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color='grey47', alpha=0.2) +
  scale_y_log10() +
  coord_flip() +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[]) +
  theme(legend.position = 'none') +
  labs(x = 'Trophic state based on P',
       y = 'TN:TP (molar)')

#NO3:TP -- P trophic state
ggplot(nla2007_tntp, aes(TSTATE_TP, no3.tp, fill = TSTATE_TP)) +
  geom_violin() +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color='grey47', alpha=0.2) +
  scale_y_log10() +
  coord_flip() +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[]) +
  theme(legend.position = 'none') +
  labs(x = 'Trophic state based on P',
       y = 'NO3:TP (molar)')

#NH4:TP -- P trophic state
ggplot(nla2007_tntp, aes(TSTATE_TP, nh4.tp, fill = TSTATE_TP)) +
  geom_violin() +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color='grey47', alpha=0.2) +
  scale_y_log10() +
  coord_flip() +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[]) +
  theme(legend.position = 'none') +
  labs(x = 'Trophic state based on P',
       y = 'NH4:TP (molar)')


#NH4 + NO3:TP -- P trophic state
ggplot(nla2007_tntp, aes(TSTATE_TP, `nh4+no3.tp`, fill = TSTATE_TP)) +
  geom_violin() +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color='grey47', alpha=0.2) +
  scale_y_log10() +
  coord_flip() +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[]) +
  theme(legend.position = 'none') +
  labs(x = 'Trophic state based on P',
       y = 'NH4 + No3:TP (molar)')


###N classificaiton#############################################
#TN:TP -- N trophic state
ggplot(nla2007_tntp, aes(TSTATE_TN, tn.tp, fill = TSTATE_TN)) +
  geom_violin() +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color='grey47', alpha=0.2) +
  scale_y_log10() +
  coord_flip() +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[]) +
  theme(legend.position = 'none') +
  labs(x = 'Trophic state based on N',
       y = 'TN:TP (molar)')

#NO3:TP -- N trophic state
ggplot(nla2007_tntp, aes(TSTATE_TN, no3.tp, fill = TSTATE_TN)) +
  geom_violin() +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color='grey47', alpha=0.2) +
  scale_y_log10() +
  coord_flip() +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[]) +
  theme(legend.position = 'none') +
  labs(x = 'Trophic state based on N',
       y = 'NO3:TP (molar)')

#NH4:TP -- P trophic state
ggplot(nla2007_tntp, aes(TSTATE_TN, nh4.tp, fill = TSTATE_TN)) +
  geom_violin() +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color='grey47', alpha=0.2) +
  scale_y_log10() +
  coord_flip() +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[]) +
  theme(legend.position = 'none') +
  labs(x = 'Trophic state based on N',
       y = 'NH4:TP (molar)')


###CHLA classificaiton#############################################
#TN:TP -- CHLA trophic state
ggplot(nla2007_tntp |>
         filter(!is.na(CHLA_PPB)), aes(TSTATE_CHL, tn.tp, fill = TSTATE_CHL)) +
  geom_violin() +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color='grey47', alpha=0.2) +
  scale_y_log10() +
  coord_flip() +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[]) +
  theme(legend.position = 'none') +
  labs(x = 'Trophic state based on chlorophyll-a',
       y = 'TN:TP (molar)')

#NO3:TP -- CHLA trophic state
ggplot(nla2007_tntp |>
         filter(!is.na(CHLA_PPB)), aes(TSTATE_CHL, no3.tp, fill = TSTATE_CHL)) +
  geom_violin() +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color='grey47', alpha=0.2) +
  scale_y_log10() +
  coord_flip() +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[]) +
  theme(legend.position = 'none') +
  labs(x = 'Trophic state based on chlorophyll-a',
       y = 'NO3:TP (molar)')

#NH4:TP -- CHLA trophic state
ggplot(nla2007_tntp |>
         filter(!is.na(CHLA_PPB)), aes(TSTATE_CHL, nh4.tp, fill = TSTATE_CHL)) +
  geom_violin() +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color='grey47', alpha=0.2) +
  scale_y_log10() +
  coord_flip() +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[]) +
  theme(legend.position = 'none') +
  labs(x = 'Trophic state based on chlorophyll-a',
       y = 'NH4:TP (molar)')


#NH4 + NO3:TP -- CHLA trophic state
  ggplot(nla2007_tntp |>
           filter(!is.na(CHLA_PPB)), aes(TSTATE_CHL, `nh4+no3.tp`,fill = TSTATE_CHL)) +
  geom_violin() +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color='grey47', alpha=0.2) +
  scale_y_log10() +
  coord_flip() +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[]) +
  theme(legend.position = 'none') +
  labs(x = 'Trophic state based on chlorophyll-a',
       y = 'NH4 + No3:TP (molar)')


#TN:TP figure with trophic states lines!
#N trophic state
ggplot(nla2007_tntp) +
  geom_point(aes((PTL_PPB/Pmol), (NTL_PPM/Nmol), fill = TSTATE_TN), size = 2.5, shape = 21, alpha = 0.8) +
  geom_abline(slope = 16, intercept = 0) +
 # geom_jitter(aes((PTL_PPB/Pmol), ((NH4N_PPM + NO3N_PPM)/Nmol), fill = 'NH4 + NO3'), size = 2.5, shape = 21, alpha = 0.4) +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[]) +
  theme(legend.title = element_blank()) +
  labs(x = "TP (molar)", y = "TN (molar)",
       title = 'Trophic status based on TN') +
  annotate('text', label = 'Redfield 16:1 line', x = 0.0001, y = 0.0015, hjust = 0, size = 4) 


#P trophic state
ggplot(nla2007_tntp) +
  geom_point(aes((PTL_PPB/Pmol), (NTL_PPM/Nmol), fill = TSTATE_TP), size = 2.5, shape = 21, alpha = 0.8) +
  geom_abline(slope = 16, intercept = 0) +
  # geom_jitter(aes((PTL_PPB/Pmol), ((NH4N_PPM + NO3N_PPM)/Nmol), fill = 'NH4 + NO3'), size = 2.5, shape = 21, alpha = 0.4) +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[]) +
  theme(legend.title = element_blank()) +
  labs(x = "TP (molar)", y = "TN (molar)",
       title = 'Trophic Status based on TP') +
  annotate('text', label = 'Redfield 16:1 line', x = 0.0001, y = 0.0015, hjust = 0, size = 4) 
  

#CHla trophic state
ggplot(nla2007_tntp) +
  geom_point(aes((PTL_PPB/Pmol), (NTL_PPM/Nmol), fill = TSTATE_CHL), size = 2.5, shape = 21, alpha = 0.8) +
  geom_abline(slope = 16, intercept = 0) +
  # geom_jitter(aes((PTL_PPB/Pmol), ((NH4N_PPM + NO3N_PPM)/Nmol), fill = 'NH4 + NO3'), size = 2.5, shape = 21, alpha = 0.4) +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[]) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  labs(x = "TP (molar)", y = "TN (molar)",
        title = 'Trophic status based on Chl-a') +
  annotate('text', label = 'Redfield 16:1 line', x = 0.0001, y = 0.0015, hjust = 0, size = 4) 
