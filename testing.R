library(broom)
library(tidyverse)

#### Load and subset dataframe ####
source("Data/NLA/Call_NLA_data.R")
nla_data_subset <- all_NLA |>
  select(ECO_REG_NAME, UNIQUE_ID, DATE_COL, VISIT_NO, NTL_PPM, PTL_PPB, DIN_PPM, tn.tp, DIN.TP, CHLA_PPB, TROPHIC_STATE, year,WGT_NLA, LON_DD, LAT_DD, AREA_HA, ELEV_PT, PCT_DEVELOPED_BSN, PCT_AGRIC_BSN, SITE_TYPE, URBAN, LAKE_ORIGIN, PTL_COND, NTL_COND, CHLA_COND, REFERENCE) |>
  rename(DIN.TP_molar = DIN.TP,
         TN.TP_molar = tn.tp) |>
  filter(AREA_HA >= 4) |> # removes 382 observations 
  distinct() |>
  mutate(NTL_PPB = NTL_PPM*1000)



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
  geom_smooth(method='lm', aes(y=fit), color='black') +
  facet_wrap(.~ECO_REG_NAME, ncol=3) +
  scale_y_log10() +
  scale_x_log10()






library(sjPlot)
plot_model(m1, type='pred', terms=c('ECO_REG_NAME', 'NTL_PPB', 'PTL_PPB'))
plot_model(m1, type='int')


































ggplot() +
  geom_density(nla_data_subset, mapping=aes(x=NTL_PPM)) +
  geom_density(nla_data_subset |> filter(REFERENCE == 'Y'),mapping= aes(x=NTL_PPM, color = 'Reference Conditions')) +
  facet_wrap(~ECO_REG_NAME, ncol=3)


ggplot() +
  geom_density(nla_data_subset, mapping=aes(x=log10(CHLA_PPB)))


ggplot(nla_data_subset) +
 # geom_boxplot(mapping=aes(ECO_REG_NAME, CHLA_PPB)) +
  geom_boxplot(nla_data_subset |> filter(REFERENCE == 'Y'), mapping=aes(ECO_REG_NAME, CHLA_PPB), color = 'red')




# find where N and P differ and chlorophyll a is disturbed -- maybe this will appease them ?
chla_dist <- nla_data_subset |>
  filter(CHLA_COND %in% c('Poor', 'Fair', '3:MOST DISTURBED', '2:INTERMEDIATE DISTURBANCE'))

LOW_N <-chla_dist |>
  filter(NTL_COND %in% c('Good', '1:LEAST DISTURBED', 'Fair', '2:INTERMEDIATE DISTURBANCE')) |>
 # filter(!PTL_COND %in% c('Good', '1:LEAST DISTURBED')) |>
  group_by(ECO_REG_NAME) |>
  mutate(meanNP_N = mean(TN.TP_molar))

LOW_P <-chla_dist |>
  filter(PTL_COND %in% c('Good', '1:LEAST DISTURBED', 'Fair', '2:INTERMEDIATE DISTURBANCE')) |>
 # filter(!PTL_COND %in% c('Good', '1:LEAST DISTURBED')) |>
  group_by(ECO_REG_NAME) |>
  mutate(meanNP_P = mean(TN.TP_molar))

ratio <- left_join(LOW_N, LOW_P) |>
  group_by(ECO_REG_NAME) |>
  summarise(medianNP = median(c(TN.TP_molar, TN.TP_molar)))


ggplot() +
  geom_density(LOW_N, mapping=aes(x=TN.TP_molar), color='red') +
  geom_density(LOW_P, mapping=aes(x=TN.TP_molar), color='blue') +
  geom_point(ratio, mapping = aes(medianNP, 0.025)) +
  facet_wrap(~ECO_REG_NAME, ncol=3)



# shows how many EPA hand-selected reference lakes exist in each ecoregion and year and size class. these overlap with our best condition lakes from 'condition_checks,' but also include lakes that are not in best conditions, but still are representative of healthy lakes deemed by folks collecting the data who have in-depth knowledge of the area they are surveying. 
reference_checks <- nla_data_subset |>
  filter(REFERENCE == 'Y') |>
  group_by(ECO_REG_NAME, year) |>
  count()



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
  filter(REFERENCE == 'Y')  # 437 lakes



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
            percentile75TN_PPM = quantile(NTL_PPM, probs = 0.75, na.rm=TRUE)) |>
  ungroup() |>
  left_join(ratio)

criteria <- reflakes  |>
  rename(TP_threshold = percentile75TP_PPB,
         TN_threshold = percentile75TN_PPM) 

# generate nutrient limitations
limits <- nla_data_subset|> #3270 lakes
 # filter(is.finite(log(DIN.TP_molar))) |> #3172
 # filter(!is.na(DIN_PPM)) |> # 3172 observations, loss of 98 observations from analysis
  left_join(criteria) |>
  mutate(limitation = NA) |>
  mutate(limitation = ifelse(PTL_PPB > TP_threshold & TN.TP_molar < medianNP, "N-limitation", 
                             ifelse(NTL_PPM > TN_threshold & TN.TP_molar > medianNP, "P-limitation",
                                    ifelse(is.na(limitation), "Co-nutrient limitation", limitation))))

nrow(limits |> filter(limitation == "P-limitation")) # 920
nrow(limits |> filter(limitation == "N-limitation")) # 1208
nrow(limits |> filter(limitation == "Co-nutrient limitation")) # 1142





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

library(lme4)

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



##########################################################
# perc95chla <- nla_data_subset |>
#   group_by(ECO_REG_NAME) |>
#   mutate(perc_95 = quantile(CHLA_PPB, 0.95)) |>
#   ungroup() |>
#   filter(CHLA_PPB >= perc_95) # |>
#   # find median N:P at high chlorophyll
#  # group_by(ECO_REG_NAME) |>
# #  summarise(medNP = median(TN.TP_molar))
# 
# ggplot() +
#   geom_point(nla_data_subset, mapping=aes(PTL_PPB, CHLA_PPB)) +
#   geom_smooth(method='lm', perc95chla, mapping=aes(PTL_PPB, CHLA_PPB, color=ECO_REG_NAME, group=ECO_REG_NAME), se=FALSE) +
#   # geom_smooth(method='lm', perc95chla, mapping=aes(PTL_PPB, CHLA_PPB), color = 'red', se=FALSE)  +
#   scale_y_log10() +
#   scale_x_log10()
# 
# ggplot() +
#   geom_point(nla_data_subset, mapping=aes(NTL_PPM, CHLA_PPB)) +
#   geom_smooth(method='lm', se=FALSE, perc95chla, mapping=aes(NTL_PPM, CHLA_PPB, color=ECO_REG_NAME, group=ECO_REG_NAME)) +
#   scale_y_log10() +
#   scale_x_log10()



library(zoo)
# following methods are adapted from from Moon et al. 2021
# they used ~10% of data moving sample subset - this will be different for each ecoregion in our data
ten_perc <- nla_data_subset |> count(ECO_REG_NAME) |> mutate(ten_perc = round(0.1*n)) 


nla_order_P <-nla_data_subset |>
  # arrange low to high P
  arrange(ECO_REG_NAME, PTL_PPB) |>
  left_join(ten_perc) |>
# pull 95th percentile CHLA and 50th percentile TN and TP from moving 10% 
  group_by(ECO_REG_NAME) |>
  reframe(perc95CHLa = rollapply(CHLA_PPB, width=ten_perc, FUN='quantile', p = 0.95),
          perc50TP = rollapply(PTL_PPB, width=ten_perc, FUN='quantile', p = 0.50))


nla_order_N <-nla_data_subset |>
 # mutate(NTL_PPB = NTL_PPM*1000) |>
  # arrange low to high N
  arrange(ECO_REG_NAME, NTL_PPB) |>
  left_join(ten_perc) |>
  # pull 95th percentile CHLA and 50th percentile TN and TP from moving 10% 
  group_by(ECO_REG_NAME) |>
  reframe(perc95CHLa = rollapply(CHLA_PPB, width=ten_perc, FUN='quantile', p = 0.95),
          perc50TN = rollapply(NTL_PPB, width=ten_perc, FUN='quantile', p = 0.50))

# high yield linear regressions on log transformed data
highyield_P_lm <- lm(log10(perc95CHLa)~log10(perc50TP)*ECO_REG_NAME, nla_order_P)
summary(highyield_P_lm)
coefp<-coef(highyield_P_lm)

highyield_N_lm <- lm(log10(perc95CHLa)~log10(perc50TN)*ECO_REG_NAME, nla_order_N)
summary(highyield_N_lm)
coefn<-coef(highyield_N_lm)


# pull linear regression info into a table
lm_dat_highyield <- data.frame(ECO_REG_NAME = unique(nla_data_subset$ECO_REG_NAME)) |>
  # get betas for each ecoregion from Chla-TP model
  mutate(P_slope = case_when(ECO_REG_NAME=='Northern Appalachians'~coefp[2],
                             ECO_REG_NAME=='Southern Appalachians'~coefp[2]+coefp[11],
                             ECO_REG_NAME=='Coastal Plains'~coefp[2]+coefp[12],
                             ECO_REG_NAME=='Temperate Plains'~coefp[2]+coefp[13],
                             ECO_REG_NAME=='Upper Midwest'~coefp[2]+coefp[14],
                             ECO_REG_NAME=='Northern Plains'~coefp[2]+coefp[15],
                             ECO_REG_NAME=='Southern Plains'~coefp[2]+coefp[16],
                             ECO_REG_NAME=='Xeric'~coefp[2]+coefp[17],
                             ECO_REG_NAME=='Western Mountains'~coefp[2]+coefp[18])) |>
  # get alphas for each ecoregion from Chla-TP model
  mutate(P_intercept = case_when(ECO_REG_NAME=='Northern Appalachians'~coefp[1],
                             ECO_REG_NAME=='Southern Appalachians'~coefp[1]+coefp[3],
                             ECO_REG_NAME=='Coastal Plains'~coefp[1]+coefp[4],
                             ECO_REG_NAME=='Temperate Plains'~coefp[1]+coefp[5],
                             ECO_REG_NAME=='Upper Midwest'~coefp[1]+coefp[6],
                             ECO_REG_NAME=='Northern Plains'~coefp[1]+coefp[7],
                             ECO_REG_NAME=='Southern Plains'~coefp[1]+coefp[8],
                             ECO_REG_NAME=='Xeric'~coefp[1]+coefp[9],
                             ECO_REG_NAME=='Western Mountains'~coefp[1]+coefp[10])) |>
  # get betas for each ecoregion from Chla-TN model
  mutate(N_slope = case_when(ECO_REG_NAME=='Northern Appalachians'~coefn[2],
                             ECO_REG_NAME=='Southern Appalachians'~coefn[2]+coefn[11],
                             ECO_REG_NAME=='Coastal Plains'~coefn[2]+coefn[12],
                             ECO_REG_NAME=='Temperate Plains'~coefn[2]+coefn[13],
                             ECO_REG_NAME=='Upper Midwest'~coefn[2]+coefn[14],
                             ECO_REG_NAME=='Northern Plains'~coefn[2]+coefn[15],
                             ECO_REG_NAME=='Southern Plains'~coefn[2]+coefn[16],
                             ECO_REG_NAME=='Xeric'~coefn[2]+coefn[17],
                             ECO_REG_NAME=='Western Mountains'~coefn[2]+coefn[18])) |>
  # get alphas for each ecoregion from Chla-TN model
  mutate(N_intercept = case_when(ECO_REG_NAME=='Northern Appalachians'~coefn[1],
                                 ECO_REG_NAME=='Southern Appalachians'~coefn[1]+coefn[3],
                                 ECO_REG_NAME=='Coastal Plains'~coefn[1]+coefn[4],
                                 ECO_REG_NAME=='Temperate Plains'~coefn[1]+coefn[5],
                                 ECO_REG_NAME=='Upper Midwest'~coefn[1]+coefn[6],
                                 ECO_REG_NAME=='Northern Plains'~coefn[1]+coefn[7],
                                 ECO_REG_NAME=='Southern Plains'~coefn[1]+coefn[8],
                                 ECO_REG_NAME=='Xeric'~coefn[1]+coefn[9],
                                 ECO_REG_NAME=='Western Mountains'~coefn[1]+coefn[10]))


# calculate high yield Chla from each real observation of TN and TP using the high yield regressions
HighYield_Chla <- nla_data_subset |> 
  left_join(lm_dat_highyield) |>
  mutate(log10chla_HY_P = P_slope*log10(PTL_PPB) + P_intercept,
         log10chla_HY_N = N_slope*log10(NTL_PPB) + N_intercept) |>
  # then calculate fraction yield Chla - as observed Chla/high yield chla
  mutate(log_fractionyieldP = log10(CHLA_PPB)/log10chla_HY_P,
         log_fractionyieldN = log10(CHLA_PPB)/log10chla_HY_N) #|>
  # at what concentration of TN is highyield N equal to high yield P?
  # mutate(theoretical_N = 10^((log10chla_HY_P-N_intercept)/N_slope)) |> # 10^ unlogs it :)
  # mutate(tippingPoint_NP_N = (theoretical_N/PTL_PPB)*2.11306) |> # mutliply by 2.11306 to convert to molar ratio
  # # Now complete that same thing to find concentrations of P and then the tipping point - theoretically, they should be the same tipping point?
  # mutate(theoretical_P = 10^((log10chla_HY_N-P_intercept)/P_slope)) |>
  # mutate(tippingPoint_NP_P = (NTL_PPB/theoretical_P)*2.11306)


ggplot(HighYield_Chla, aes(log10chla_HY_P, log10chla_HY_N)) +
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  facet_wrap(~ECO_REG_NAME,scales='free')

ggplot(HighYield_Chla, aes(log_fractionyieldP, log_fractionyieldN)) +
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  facet_wrap(~ECO_REG_NAME,scales='free')


# add high and low TP values in ug/L that will be regressed to find high yield CHLas
lm_dat_highyield_pred <- lm_dat_highyield |>
  mutate(TP_low_ugL = 1,
         TP_high_ugL = 5000) |>
  pivot_longer(6:7, names_to = 'TP_cat', values_to = 'TP_ugL') |>
# When TP is 1 or 5000 ug/L, what is high yield CHLA in ug/L?
  mutate(HY_CHLA_atTP = 10^(log10(TP_ugL)*P_slope + P_intercept)) |>
# NOW -- what are the TN concentrations ug/L at each of these high yields
  mutate(TN_atHYchla_ugL = 10^((log10(HY_CHLA_atTP)-N_intercept)/N_slope)) |>
# get N:P by mass here
  mutate(tippingPt_NP = TN_atHYchla_ugL/TP_ugL)

# New regressions to find N:P tipping point line based on TP concentration
tippint_point_calc <- lm(log10(tippingPt_NP)~log10(TP_ugL)*ECO_REG_NAME, lm_dat_highyield_pred)
summary(tippint_point_calc)
coeftip<-coef(tippint_point_calc)



# pull linear regression info into a new table -- remember these are logged!!
tipping_point_regressions <- data.frame(ECO_REG_NAME = unique(nla_data_subset$ECO_REG_NAME)) |>
  # get betas for each ecoregion from N:P vs TP model
  mutate(tip_slope = case_when(ECO_REG_NAME=='Northern Appalachians'~coeftip[2],
                             ECO_REG_NAME=='Southern Appalachians'~coeftip[2]+coeftip[11],
                             ECO_REG_NAME=='Coastal Plains'~coeftip[2]+coeftip[12],
                             ECO_REG_NAME=='Temperate Plains'~coeftip[2]+coeftip[13],
                             ECO_REG_NAME=='Upper Midwest'~coeftip[2]+coeftip[14],
                             ECO_REG_NAME=='Northern Plains'~coeftip[2]+coeftip[15],
                             ECO_REG_NAME=='Southern Plains'~coeftip[2]+coeftip[16],
                             ECO_REG_NAME=='Xeric'~coeftip[2]+coeftip[17],
                             ECO_REG_NAME=='Western Mountains'~coeftip[2]+coeftip[18])) |>
  # get alphas for each ecoregion from N:P vs TP model
  mutate(tip_intercept = case_when(ECO_REG_NAME=='Northern Appalachians'~coeftip[1],
                                 ECO_REG_NAME=='Southern Appalachians'~coeftip[1]+coeftip[3],
                                 ECO_REG_NAME=='Coastal Plains'~coeftip[1]+coeftip[4],
                                 ECO_REG_NAME=='Temperate Plains'~coeftip[1]+coeftip[5],
                                 ECO_REG_NAME=='Upper Midwest'~coeftip[1]+coeftip[6],
                                 ECO_REG_NAME=='Northern Plains'~coeftip[1]+coeftip[7],
                                 ECO_REG_NAME=='Southern Plains'~coeftip[1]+coeftip[8],
                                 ECO_REG_NAME=='Xeric'~coeftip[1]+coeftip[9],
                                 ECO_REG_NAME=='Western Mountains'~coeftip[1]+coeftip[10])) 

# double check that coefs are in the right places
tipping_point_regressions_check <- tipping_point_regressions |>
  mutate(NP_1 = 10^(log10(1)*tip_slope + tip_intercept),
         NP_5000 = 10^(log10(5000)*tip_slope + tip_intercept)) # looks good


# determine limitations using rules from Moon et al., 2021
limits<- nla_data_subset |>
  left_join(tipping_point_regressions) |>
  mutate(tipping_pt_NP_molar = (10^(log10(PTL_PPB)*tip_slope+tip_intercept))*2.11306) |> # unlog to get N:P by mass and multiply by 2.11306 to get molar
  # divide observed N:P by tipping point
  mutate(NP_deviation = TN.TP_molar/tipping_pt_NP_molar) |>
  # calculate limitations!
  mutate(limitation = case_when(NP_deviation > 2 ~ 'P-limitation',
                                NP_deviation < 0.5 ~ 'N-limitation', 
                                between(NP_deviation,0.5,2) ~ 'Co-limitation'))

nrow(limits |> filter(limitation == "P-limitation")) # 436
nrow(limits |> filter(limitation == "N-limitation")) # 350
nrow(limits |> filter(limitation == "Co-limitation")) # 2484
  
nrow(limits |> filter(limitation == "P-limitation"))/nrow(limits) *100 # 13.3%
nrow(limits |> filter(limitation == "N-limitation"))/nrow(limits) *100 # 10.7%
nrow(limits |> filter(limitation == "Co-limitation"))/nrow(limits) *100 # 76%
