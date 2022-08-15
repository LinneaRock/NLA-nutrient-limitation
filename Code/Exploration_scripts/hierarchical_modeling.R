

## Hierarchical modeling ##
# This is an assignment for class, but maybe will be useful? 

library(tidyverse)
library(lubridate)
library(colorblindr)

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
  mutate(year = as.character(year(DATE_COL))) |>
  mutate(URBAN = ifelse(URBAN %in% c("No", "NO", "Non-Urban"), "Non-Urban", "Urban")) |>
  mutate(lakearea = as.numeric(scale(AREA_HA)),
         elev = as.numeric(scale(ELEV_PT)))





ggplot(all_NLA) +
  geom_point(aes(AREA_HA, tn.tp), size = 2.5, shape = 21, alpha = 0.8) +
  # geom_abline(slope = 16, intercept = 0) +
  theme_light() +
  scale_fill_manual(values = palette_OkabeIto[]) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  # labs(x = "TP (molar)", y = "TN (molar)") +
  #annotate('text', label = 'Redfield 16:1 line', x = 0.0001, y = 0.0015, hjust = 0, size = 4) +
  scale_y_log10()# +
  #scale_x_log10()


library(lme4)
library(lmerTest)

m1 <- lmer(tn.tp ~ AREA_HA * ELEV_PT + URBAN + (AREA_HA|ECO_REG) + (1|UNIQUE_ID) + (1|year), data = all_NLA)
performance::r2(m1)
summary(m1)
coef(m1)

#how many unique ids?
uniqueid <- as.data.frame(all_NLA$UNIQUE_ID) |> distinct()

