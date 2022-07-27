################################################################################
############## What are the trends of stoichiometry and trophic   ############## 
############## levels across ecoregional and the national scale?  ##############
################################################################################


source("Data/NLA/Call_NLA_data.R")


#### Trophic status and across ecoregions in 2017 data  ####
library(colorblindr)

## The total numbers of lakes in each category 
ggplot(all_NLA |> filter(year == "2017")) +
  geom_bar(aes(ECO_REG, fill = TSTATE_CHL)) +
  theme_minimal() +
  scale_fill_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]),
                    labels = c("Olig.", "Meso.", "Eutro.", "Hyper.")) +
  labs(x = "",
       y = "# Lakes in each trophic category")
ggsave("Figures/Q3.Figs/2017_TS_Totals.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

## Proportion of lakes in each category
proportion_ts <- all_NLA |>
  filter(year == "2017") |>
  count(ECO_REG_NAME, TROPHIC_STATE) |>
  rename(lakes_ts = n) |>
  group_by(ECO_REG_NAME) |>
  mutate(tot_lakes = sum(lakes_ts)) |>
  ungroup() |>
  mutate(proportion = (lakes_ts/tot_lakes)*100)

ggplot(proportion_ts, aes(x = 1, y = proportion, fill = TROPHIC_STATE)) +
  facet_wrap(~ECO_REG_NAME, ncol = 3) +
  geom_col()+
  coord_polar(theta = 'y') +
  theme_minimal() +
  scale_fill_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]),
                    labels = c("Olig.", "Meso.", "Eutro.", "Hyper.")) +
  labs(x = "", y = "")+
  theme(axis.text = element_blank())
ggsave("Figures/Q3.Figs/2017_TS_Percents.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 
  
library(lme4)
library(lmerTest)
m.1 <- glmer(proportion ~ TROPHIC_STATE + (1|ECO_REG_NAME), proportion_ts)
summary(m.1)

#### How do trophic statuses and stoichiometries compare across ecoregions in natural vs. manmade lakes in 2017? ####
source("Code/Functions/split_violin.R")

#filter the dataset
ts_laketype <- all_NLA |>
  filter(year == "2017") |>
  select(URBAN, LAKE_ORIGIN, TROPHIC_STATE, ECO_REG_NAME, year, tn.tp) 

#2017
ggplot(ts_laketype) +
  geom_split_violin(aes(y = tn.tp, x = TROPHIC_STATE,  fill = LAKE_ORIGIN)) +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual("", values = c("#ffc857", "#323031"),
                    labels = c("Man-made lakes", "Natural lakes")) +
  labs(y = "TN:TP molar ratio",
       x = "",
       title = "National")
ggsave("Figures/Q3.Figs/2017_ratio_ts_natvsmmlakes_national.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

ggplot(ts_laketype) +
  geom_split_violin(aes(y = tn.tp, x = TROPHIC_STATE,  fill = LAKE_ORIGIN)) +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual("", values = c("#ffc857", "#323031"),
                    labels = c("Man-made lakes", "Natural lakes")) +
  labs(y = "TN:TP molar ratio",
       x = "") +
  facet_wrap(~ECO_REG_NAME, ncol = 3)
ggsave("Figures/Q3.Figs/2017_ratio_ts_natvsmmlakes_regions.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 




