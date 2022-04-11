

library(tidyverse)
library(colorblindr)
library(lubridate)

## Looking at plots of  all data

NLA07 <- read.csv("DAta/NLA_2007.csv") |>
  select(-X, -X.1) |>
  mutate(DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"))

#str(NLA07) # what variables need to be altered (e.g. to as date or as numeric, etc.)? 

NLA12 <- read.csv("Data/NLA_2012.csv") |>
  select(-X, -X.1) |>
  mutate(DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"))

#str(NLA12) # what variables need to be altered (e.g. to as date or as numeric, etc.)? 

NLA17 <- read.csv("Data/NLA_2017.csv") |>
  select(-X) |>
  mutate(DATE_COL = as.Date(DATE_COL, format = "%d-%b-%y"),
         HUC_8 = as.integer(substring(HUC_8, 2)))

#str(NLA17) # what variables need to be altered (e.g. to as date or as numeric, etc.)? 



# combine and plot all TN:TP
all_NLA <- bind_rows(NLA07, NLA12, NLA17) |>
  mutate(year = as.character(year(DATE_COL)))

ggplot(all_NLA) +
  geom_point(aes(TP_mol, TN_mol, fill = year), size = 2.5, shape = 21, alpha = 0.8) +
 # geom_abline(slope = 16, intercept = 0) +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[]) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  labs(x = "TP (molar)", y = "TN (molar)") +
  #annotate('text', label = 'Redfield 16:1 line', x = 0.0001, y = 0.0015, hjust = 0, size = 4) +
  scale_y_log10() +
  scale_x_log10()



#plots showing N and P limitation based on Bergstrom, 2010 study 
ggplot(all_NLA) +
  geom_point(aes(log(PTL_PPB, base = 10), log(tn.tp, base = 10), fill = year), size = 2.5, shape = 21, alpha = 0.8) +
  geom_abline(slope = 0, intercept = log(19, base = 10), linetype = "dashed") +
  geom_abline(slope = 0, intercept = log(28, base = 10)) +
  geom_abline(slope = 0, intercept = log(41, base = 10), linetype = "dashed") +
  geom_abline(slope = 0, intercept = log(16, base = 10), color = "red") +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[]) +
  labs(y = "Log TN:TP", x = "Log TP"~(mu~g~L^-1)) +
  annotate('text', label = 'Redfield 16:1 line', x = 3.5, y = 1.1, hjust = 0, size = 3, color = "red") +
  annotate('text', label = 'Predicted N limitation \n (Bergstrom, 2010)', x = -1, y = 1, hjust = 0, size = 3) +
  annotate('text', label = 'Predicted P limitation \n (Bergstrom, 2010)', x = 3, y = 2, hjust = 0, size = 3) 

ggplot(all_NLA) +
  geom_point(aes(log(PTL_PPB, base = 10), log(tn.tp, base = 10), fill = log(NTL_PPM, base = 10)), size = 2.5, shape = 21, alpha = 0.8) +
  geom_abline(slope = 0, intercept = log(19, base = 10), linetype = "dashed") +
  geom_abline(slope = 0, intercept = log(28, base = 10)) +
  geom_abline(slope = 0, intercept = log(41, base = 10), linetype = "dashed") +
  geom_abline(slope = 0, intercept = log(16, base = 10), color = "red") +
  theme_light() +
  scale_fill_viridis_c() +
  labs(y = "Log TN:TP", x = "Log TP"~(mu~g~L^-1)) +
  annotate('text', label = 'Redfield 16:1 line', x = 3.5, y = 1.1, hjust = 0, size = 3, color = "red") +
  annotate('text', label = 'Predicted N limitation \n (Bergstrom, 2010)', x = -1, y = 1, hjust = 0, size = 3) +
  annotate('text', label = 'Predicted P limitation \n (Bergstrom, 2010)', x = 3, y = 2, hjust = 0, size = 3) 

