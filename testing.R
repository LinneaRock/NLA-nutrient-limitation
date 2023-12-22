library(broom)
library(tidyverse)

#### Load and subset dataframe ####
source("Data/NLA/Call_NLA_data.R")
nla_data_subset <- all_NLA |>
  select(ECO_REG_NAME, UNIQUE_ID, DATE_COL, VISIT_NO, NTL_PPM, PTL_PPB, DIN_PPM, tn.tp, DIN.TP, CHLA_PPB, TROPHIC_STATE, year,WGT_NLA, LON_DD, LAT_DD, AREA_HA, ELEV_PT, PCT_DEVELOPED_BSN, PCT_AGRIC_BSN, SITE_TYPE, URBAN, LAKE_ORIGIN, PTL_COND, NTL_COND, CHLA_COND) |>
  rename(DIN.TP_molar = DIN.TP,
         TN.TP_molar = tn.tp) |>
  filter(AREA_HA >= 4) |> # removes 382 observations 
  distinct()



# CDF

plot(ecdf((nla_data_subset |> filter(ECO_REG_NAME =='Upper Midwest' &
                                     CHLA_COND %in% c('3:MOST DISTURBED', 'Poor')))$PTL_PPB/1000))

# has same outcome as individuals ####

test_dat <- nla_data_subset |>
  filter(is.finite(log10(CHLA_PPB))) |>
  mutate(NTL_PPB = NTL_PPM * 1000) 

m1 <- aov(log10(CHLA_PPB) ~ ECO_REG_NAME * log10(NTL_PPB) * log10(PTL_PPB), data = test_dat)
summary(m1)
anova(m1)
coef(m1)


test_dat$fit <- 10^predict(m1)

test_dat <- test_dat |>
  pivot_longer(cols = c(PTL_PPB, NTL_PPB), names_to = "nutrient", values_to = "concentration") |>
  select(-NTL_PPM)

ggplot(test_dat, aes(concentration, CHLA_PPB, color = nutrient, group = nutrient)) +
  geom_point() +
  geom_smooth(method= 'lm', aes(y=fit), color='black') +
  facet_wrap(.~ECO_REG_NAME, ncol=3) +
  scale_y_log10() +
  scale_x_log10()





# 
# 
# lm_dat <- nla_data_subset |>
#   select(ECO_REG_NAME, CHLA_PPB, PTL_PPB, NTL_PPM) |>
#   mutate(NTL_PPB = NTL_PPM * 1000) |>
#   pivot_longer(cols = c(PTL_PPB, NTL_PPB), names_to = "nutrient", values_to = "concentration") |>
#   select(-NTL_PPM)
# 
# lm_dat$ECO_REG_NAME = factor(lm_dat$ECO_REG_NAME,
#                                         levels = c("Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))
# 
# 
# 
# m.0region <- aov(CHLA_PPB~concentration * nutrient * ECO_REG_NAME, lm_dat)
# 
# summary(m.0region) # all vars significant 
# 
# glance(m.0region) # r = 0.594, AIC = 7063
# performance::r2(m.0region) # adj r = 0.592
# 
# coef(m.0region)
# 
# lm_dat <- lm_dat |> 
#   mutate(fit = predict(m.0region))
# 
# ggplot(lm_dat, aes(concentration, CHLA_PPB, color = nutrient, group = nutrient)) +
#   geom_point() +
#   geom_line(aes(y=fit)) +
#   facet_wrap(.~ECO_REG_NAME, ncol=3) +
#   scale_y_log10() +
#   scale_x_log10()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# library(lme4)
# library(lmerTest)
#  m.1 <- lmer(log10(CHLA_PPB)~log10(concentration)*nutrient + (nutrient|ECO_REG_NAME), lm_dat |> filter(is.finite(log10(CHLA_PPB))))
# 
# #m.1 <- lmer(log10(CHLA_PPB)~log10(NTL_PPB)*log10(PTL_PPB) + (log10(NTL_PPB)*log10(PTL_PPB)|ECO_REG_NAME), lm_dat |> filter(is.finite(log10(CHLA_PPB))))
# 
# ### Plot all these together to show how they are related
# fixef(m.1) # global intercept and slope
# ranef(m.1) # random effects(these are alpha j [i])
# coef(m.1) # these are the slopes and intercepts for each group that are used for prediction!!
# performance::r2(m.1)
# summary(m.1)
# 
# plot(m.1)
# 
# library(coefplot)
# coefplot(m.1)
# 
# lm_dat <- lm_dat |> 
#   filter(is.finite(log10(CHLA_PPB))) |>
#   mutate(fit = predict(m.1))



lm_dat <- nla_data_subset |>
  select(ECO_REG_NAME, CHLA_PPB, PTL_PPB, NTL_PPM) |>
  mutate(NTL_PPB = NTL_PPM * 1000) |>
  pivot_longer(cols = c(PTL_PPB, NTL_PPB), names_to = "nutrient", values_to = "concentration") |>
  select(-NTL_PPM) |>
  filter(is.finite(log10(CHLA_PPB)))

lm_dat$ECO_REG_NAME = factor(lm_dat$ECO_REG_NAME,
                             levels = c("Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))

m.1 <- lmer(log10(CHLA_PPB)~nutrient + (log10(concentration)|ECO_REG_NAME), lm_dat)

lm_dat$fit <- 10^predict(m.1)

ggplot(lm_dat, aes(concentration, CHLA_PPB, color = nutrient, group = nutrient)) +
  geom_point() +
  geom_line(aes(y=fit), color='black') +
  facet_wrap(.~ECO_REG_NAME, ncol=3) +
  scale_y_log10() +
  scale_x_log10()

summary(m.1)
coef(m.1)




######################                                                                 
lm_dat <- nla_data_subset |>
  select(ECO_REG_NAME, CHLA_PPB, PTL_PPB, NTL_PPM) |>
  mutate(NTL_PPB = NTL_PPM * 1000) |>
  #pivot_longer(cols = c(PTL_PPB, NTL_PPB), names_to = "nutrient", values_to = "concentration") |>
  select(-NTL_PPM) |>
  filter(is.finite(log10(CHLA_PPB)))

lm_dat$ECO_REG_NAME = factor(lm_dat$ECO_REG_NAME,
                             levels = c("Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))

m.1 <- lmer(log10(CHLA_PPB)~log10(NTL_PPB)*log10(PTL_PPB) + ECO_REG_NAME + (1|ECO_REG_NAME), lm_dat)

lm_dat$fit <- 10^predict(m.1)

lm_dat <- lm_dat |>
  pivot_longer(cols = c(PTL_PPB, NTL_PPB), names_to = "nutrient", values_to = "concentration") 

ggplot(lm_dat, aes(concentration, CHLA_PPB, color = nutrient, group = nutrient)) +
  geom_point() +
  geom_line(aes(y=fit), color='black') +
  facet_wrap(.~ECO_REG_NAME, ncol=3) +
  scale_y_log10() +
  scale_x_log10()

summary(m.1)
coef(m.1)
anova(m.1)
