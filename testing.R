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

UM <- ecdf((nla_data_subset |> filter(ECO_REG_NAME =='Upper Midwest'))$TN.TP_molar)
UM(90)

ggplot(nla_data_subset, aes(TN.TP_molar, CHLA_PPB, color = CHLA_COND)) +
  geom_point() +
  facet_wrap(~ECO_REG_NAME, ncol=3)

UM_dat <- nla_data_subset |>
  filter(ECO_REG_NAME=='Upper Midwest') |>
  select(CHLA_PPB, TN.TP_molar, NTL_PPM, PTL_PPB)

ggplot(UM_dat, aes(PTL_PPB, CHLA_PPB)) +
  geom_point() +
  geom_smooth(method = 'gam')
  

ggplot(UM_dat, aes(NTL_PPM, CHLA_PPB)) +
  geom_point() +
  geom_smooth(method = 'gam')

ggplot(UM_dat, aes(TN.TP_molar, CHLA_PPB)) +
  geom_point() +
  geom_smooth(method = 'gam')

# library(mcp)
# model <- list(CHLA_PPB~1, 1~1)
# fit_mcp <- mcp(model, data=UM_dat, par_x = 'TN.TP_molar')
# summary(fit_mcp)
# 
# 
# library(patchwork)
# plot(fit_mcp) + plot_pars(fit_mcp, pars = c("cp_1"), type = "dens_overlay")
# 
# 
# model2 <- list(CHLA_PPB~1, 1~1)
# fit_mcp2 <- mcp(model2, data=UM_dat, par_x = 'NTL_PPM')
# summary(fit_mcp2)
# 
# plot(fit_mcp2) + plot_pars(fit_mcp2, pars = c("cp_1"), type = "dens_overlay")
# 
# 
# 
# model3 <- list(CHLA_PPB~1, 1~1)
# fit_mcp3 <- mcp(model3, data=UM_dat, par_x = 'PTL_PPB')
# summary(fit_mcp3)
# 
# plot(fit_mcp3) + plot_pars(fit_mcp3, pars = c("cp_1"), type = "dens_overlay")
# 
# 




# has same outcome as individuals ####

test_dat <- nla_data_subset |>
  filter(is.finite(log10(CHLA_PPB))) |>
  mutate(NTL_PPB = NTL_PPM * 1000) 

m1 <- lm(log10(CHLA_PPB) ~ ECO_REG_NAME * log10(NTL_PPB) * log10(PTL_PPB), data = test_dat)
summary(m1)
anova(m1)
coef(m1)


coefs <- coef(m1)
coefs <- 10^coefs
print(coefs)

test_dat$fit <- 10^predict(m1)

test_dat <- test_dat |>
  pivot_longer(cols = c(PTL_PPB, NTL_PPB), names_to = "nutrient", values_to = "concentration") |>
  select(-NTL_PPM)

options(scipen = 999)
ggplot(test_dat, aes(concentration, CHLA_PPB, color = nutrient, group = nutrient)) +
  geom_point() +
  geom_line(aes(y=fit), color='black') +
  facet_wrap(.~ECO_REG_NAME, ncol=3) +
  scale_y_log10() +
  scale_x_log10()






library(sjPlot)
plot_model(m1, type='pred', terms=c('ECO_REG_NAME', 'NTL_PPB', 'PTL_PPB'))
plot_model(m1, type='int')


































#### 3. Calculate limitations ####
# categorize lakes based on size using quantiles to apply 4 size classes within each ecoregion
# range(nla_data_subset$AREA_HA)
# # there is a big range of lake sizes
# 
# nla_data_subset2 <- nla_data_subset |>
#   group_by(ECO_REG_NAME) |>
#   mutate(lakesizeclass = NA) |>
#   mutate(lakesizeclass = ifelse(AREA_HA <= as.numeric(quantile(AREA_HA, 0.25)), 'lil',
#                                 ifelse(between(AREA_HA, as.numeric(quantile(AREA_HA, 0.25)),as.numeric(quantile(AREA_HA, 0.50))), 'small',
#                                        ifelse(between(AREA_HA, as.numeric(quantile(AREA_HA, 0.50)), as.numeric(quantile(AREA_HA, 0.75))), 'med',
#                                               ifelse(AREA_HA > as.numeric(quantile(AREA_HA, 0.75)), 'big', lakesizeclass))))) |>
#   ungroup()
# 
# nla_data_subset <- nla_data_subset2
# rm(nla_data_subset2)
# 
# countlakesizes <- nla_data_subset |>
#   group_by(ECO_REG_NAME, year, lakesizeclass) |>
#   summarise(n=n()) |>
#   ungroup()
# 
# sum(countlakesizes$n)
# 
# # shows how many lakes are in each combination of conditions (good/least dist., fair/intermediate dist., poor/most dist.) in each ecoregion and year and size class
# condition_checks <- nla_data_subset |>
#   group_by(ECO_REG_NAME, year, lakesizeclass, PTL_COND, NTL_COND, CHLA_COND) |>
#   count()

# shows how many EPA hand-selected reference lakes exist in each ecoregion and year and size class. these overlap with our best condition lakes from 'condition_checks,' but also include lakes that are not in best conditions, but still are representative of healthy lakes deemed by folks collecting the data who have in-depth knowledge of the area they are surveying. 
reference_checks <- nla_data_subset |>
  filter(SITE_TYPE %in% c('HAND', 'REF_Lake')) |>
  group_by(ECO_REG_NAME) |>
  count()
# THERE WERE NO REFERENCE LAKES IN 2012!


# We will combine these 'best' lakes with the selected 'reference' lakes to build our reference condition nutrient thresholds that determine healthy waters.

# ## first get the best condition lakes
# cond.tmp <- nla_data_subset |>
#   # lakes in good condition for CHLA
#   filter(CHLA_COND %in% c('1:LEAST DISTURBED', 'Good')) |> #|> # 1875 lakes
#   # lakes in good condition for CHLA and phosphorus
#   filter(PTL_COND %in% c('1:LEAST DISTURBED', 'Good')) |> # 1281 lakes
#   # lakes in good condition for CHLA, P, and nitrogen
#   filter(NTL_COND %in% c('1:LEAST DISTURBED', 'Good')) # 1048 lakes

## now get the specified reference lakes 
ref.tmp <- nla_data_subset |>
  filter(SITE_TYPE %in% c('HAND', 'REF_Lake')) # 222 lakes



## combine reference and best lakes 
reflakes <- ref.tmp|>
  # get rid of the duplicates 
  distinct() |> # total of 1132 lakes
  # how many lakes in each region and year? 
  group_by(ECO_REG_NAME) |>
  mutate(n=n()) |>
  ungroup() |>
  # 75th percentile of nutrient concentrations from reference lakes
  group_by(ECO_REG_NAME, n) |>
  summarise(percentile75TP_PPB = quantile(PTL_PPB, probs = 0.75),
            percentile75TN_PPM = quantile(NTL_PPM, probs = 0.75, na.rm=TRUE),
            meanNP = mean(TN.TP_molar, na.rm=TRUE),
            medianNP = median(TN.TP_molar, na.rm=TRUE)) |>
  ungroup() 



# get median N:P ratio in each ecoregion, year, and size class to use as threshold 
averages_np <- nla_data_subset |>
  filter(is.finite(log(TN.TP_molar))) |>
  group_by(ECO_REG_NAME, year) |>
  summarise(medianlogTNP = median(log(TN.TP_molar), na.rm = TRUE),
            medianTNP = median(TN.TP_molar, na.rm = TRUE)) |>
  ungroup() |>
  distinct()

# combine ratio and concentration threshold criteria 
criteria <- left_join(averages_np, reflakes)  |>
  rename(TP_threshold = percentile75TP_PPB,
         DIN_threshold = percentile75DIN_PPM) 

# No reference lakes for 2012 coastal plains (big), temperate plains (small), and northern plains (lil), for each, use average between 2007 and 2017 values - manual add unfortunately 
CP_TP <- as.numeric(mean(c(((criteria |> filter(ECO_REG_NAME == 'Coastal Plains',year == '2017', lakesizeclass == 'big'))$TP_threshold), ((criteria |> filter(ECO_REG_NAME == 'Coastal Plains',year == '2007',lakesizeclass == 'big'))$TP_threshold))))
CP_DIN <- as.numeric(mean(c(((criteria |> filter(ECO_REG_NAME == 'Coastal Plains',year == '2017', lakesizeclass == 'big'))$DIN_threshold), ((criteria |> filter(ECO_REG_NAME == 'Coastal Plains',year == '2007',lakesizeclass == 'big'))$DIN_threshold))))

TP_TP <- as.numeric(mean(c(((criteria |> filter(ECO_REG_NAME == 'Temperate Plains',year == '2017', lakesizeclass == 'small'))$TP_threshold), ((criteria |> filter(ECO_REG_NAME == 'Temperate Plains',year == '2007',lakesizeclass == 'small'))$TP_threshold))))
TP_DIN <- as.numeric(mean(c(((criteria |> filter(ECO_REG_NAME == 'Temperate Plains',year == '2017', lakesizeclass == 'small'))$DIN_threshold), ((criteria |> filter(ECO_REG_NAME == 'Temperate Plains',year == '2007',lakesizeclass == 'small'))$DIN_threshold))))

NP_TP <- as.numeric(mean(c(((criteria |> filter(ECO_REG_NAME == 'Northern Plains',year == '2017', lakesizeclass == 'lil'))$TP_threshold), ((criteria |> filter(ECO_REG_NAME == 'Northern Plains',year == '2007',lakesizeclass == 'lil'))$TP_threshold))))
NP_DIN <- as.numeric(mean(c(((criteria |> filter(ECO_REG_NAME == 'Northern Plains',year == '2017', lakesizeclass == 'lil'))$DIN_threshold), ((criteria |> filter(ECO_REG_NAME == 'Northern Plains',year == '2007',lakesizeclass == 'lil'))$DIN_threshold))))

criteria <- criteria |>
  mutate(TP_threshold = ifelse(is.na(TP_threshold) & 
                                 ECO_REG_NAME=='Coastal Plains', CP_TP, TP_threshold),
         DIN_threshold = ifelse(is.na(DIN_threshold) & 
                                  ECO_REG_NAME=='Coastal Plains', CP_DIN, DIN_threshold)) |>
  mutate(TP_threshold = ifelse(is.na(TP_threshold) & 
                                 ECO_REG_NAME=='Temperate Plains', TP_TP, TP_threshold),
         DIN_threshold = ifelse(is.na(DIN_threshold) & 
                                  ECO_REG_NAME=='Temperate Plains', TP_DIN, DIN_threshold)) |>
  mutate(TP_threshold = ifelse(is.na(TP_threshold) & 
                                 ECO_REG_NAME=='Northern Plains', NP_TP, TP_threshold),
         DIN_threshold = ifelse(is.na(DIN_threshold) & 
                                  ECO_REG_NAME=='Northern Plains', NP_DIN, DIN_threshold))

#write.csv(criteria, "criteria.csv") 


# generate nutrient limitations
limits <- nla_data_subset|> #3270 lakes
  filter(is.finite(log(DIN.TP_molar))) |> #3172
  filter(!is.na(DIN_PPM)) |> # 3172 observations, loss of 98 observations from analysis
  left_join(criteria) |>
  mutate(limitation = NA) |>
  mutate(limitation = ifelse(PTL_PPB > TP_threshold & log(DIN.TP_molar) < medianlogDINP, "N-limitation", 
                             ifelse(DIN_PPM > DIN_threshold & log(DIN.TP_molar) > medianlogDINP, "P-limitation",
                                    ifelse(is.na(limitation), "Co-nutrient limitation", limitation))))

nrow(limits |> filter(limitation == "P-limitation")) # 907
nrow(limits |> filter(limitation == "N-limitation")) # 1287
nrow(limits |> filter(limitation == "Co-nutrient limitation")) # 978

## plot the limited lakes
ggplot(limits) +
  geom_point(aes(log(PTL_PPB, base = 10), log(NTL_PPM, base = 10), fill = limitation), size = 2.5, shape = 21, alpha = 0.8) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_fill_manual("",values = c("grey60","red4", "#336a98")) +
  labs(y = "Log TN"~(m*g~L^-1), x = "Log TP"~(mu*g~L^-1),
       caption = "Figure X. N-limited, P-limited, and co-nutrient limited lakes across the total assessed lakes dataset. Plotted as TN vs 
TP rather than DIN vs TP becaue of better correlation between the total nutrients.") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"))

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
