
# script to look at how lakes changed TN:TP ratios, single elements, chlorophyll, and trophic states between 2007 and 2017

library(tidyverse)
library(patchwork)
source("Data/NLA/Call_NLA_data.R")


## Filter and prepaare the comparison dataset
# keep data of interest and characteristics that might be interesting (e.g., elevation, lake type, etc.)
#only keeping information from the first visit for this analysis 

temp.07 <- NLA07 |>
  select(UNIQUE_ID, ECO_REG_NAME, tn.tp, NTL_PPM, PTL_PPB, CHLA_PPB, TSTATE_CHL, WGT_NLA) |> 
  rename(tn.tp_2007 = tn.tp,
         NTL_PPM_2007 = NTL_PPM,
         PTL_PPB_2007 = PTL_PPB,
         CHLA_PPB_2007 = CHLA_PPB,
         TSTATE_2007 = TSTATE_CHL,
         WGT_2007 = WGT_NLA) |>
  distinct()

temp.12 <- NLA12 |>
  select(UNIQUE_ID, tn.tp, NTL_PPM, PTL_PPB, CHLA_PPB, TSTATE_CHL, WGT_NLA, PCT_DEVELOPED_BSN, PCT_AGRIC_BSN) |> 
  rename(tn.tp_2012 = tn.tp,
         NTL_PPM_2012 = NTL_PPM,
         PTL_PPB_2012 = PTL_PPB,
         CHLA_PPB_2012 = CHLA_PPB,
         TSTATE_2012 = TSTATE_CHL,
         WGT_2012 = WGT_NLA)|>
  distinct()

temp.17 <- NLA17 |>
  select(UNIQUE_ID, tn.tp, NTL_PPM, PTL_PPB, CHLA_PPB, TSTATE_CHL, WGT_NLA, LAKE_ORIGIN, ELEV_PT) |>
  rename(tn.tp_2017 = tn.tp,
         NTL_PPM_2017 = NTL_PPM,
         PTL_PPB_2017 = PTL_PPB,
         CHLA_PPB_2017 = CHLA_PPB,
         TSTATE_CHL_2017 = TSTATE_CHL,
         WGT_2017 = WGT_NLA)|>
  distinct()

# looking at lakes that were sampled across all 3 sampling years -- 278 lakes total 
comparison <- inner_join(temp.07, temp.12) |>
  inner_join(temp.17)


## Creating some plots 

p1 <- ggplot(comparison) +
  geom_point(aes(tn.tp_2007, tn.tp_2017)) +
  theme_minimal() +
  #scale_color_manual(values = "black") +
  theme(legend.position = "none") +
  labs(x = "2007 value",
       y = "2017 value",
       title = "TN:TP molar ratio") +
  geom_abline(slope = 1, intercept = 0, color = "red3")


p2 <- ggplot(comparison) +
  geom_point(aes(NTL_PPM_2007, NTL_PPM_2017)) +
  theme_minimal() +
  #scale_color_manual(values = "black") +
  theme(legend.position = "none") +
  labs(x = "2007 value",
       y = "2017 value",
       title = "Total Nitrogen"~(mg~L^-1)) +
  geom_abline(slope = 1, intercept = 0, color = "red3")

p3 <- ggplot(comparison) +
  geom_point(aes(PTL_PPB_2007, PTL_PPB_2017)) +
  theme_minimal() +
  #scale_color_manual(values = "black") +
  theme(legend.position = "none") +
  theme(legend.position = "none") +
  labs(x = "2007 value",
       y = "2017 value",
       title = "Total Phosphorus"~(mu*g~L^-1)) +
  geom_abline(slope = 1, intercept = 0, color = "red3")

p4 <- ggplot(comparison) +
  geom_point(aes(CHLA_PPB_2007, CHLA_PPB_2017)) +
  theme_minimal() +
  #scale_color_manual(values = "black") +
  theme(legend.position = "none") +
  labs(x = "2007 value",
       y = "2017 value",
       title = "Chlorophyll-a"~(mu*g~L^-1)) +
  geom_abline(slope = 1, intercept = 0, color = "red3")

(p1 | p4) / (p2 | p3)
# red line is 1:1, i.e., no  change 


## Calculating differences between surveys
survey_changes <- comparison
  
