
##### Script to run analyses for Nutrient limitation patterns of United States lakes #####
## Linnea A. Rock, 2024


#### Load necessary libraries ####
library(tidyverse)
library(colorblindr)
library(spsurvey)
library(broom)
library(lme4)
library(zoo)
library(patchwork)
library(ggpubr)
library(readxl)


#### Load and subset dataframe ####
source("Data/NLA/Call_NLA_data.R")
nla_data_subset <- all_NLA |>
  select(ECO_REG_NAME, UNIQUE_ID, DATE_COL, VISIT_NO, NTL_PPM, PTL_PPB, DIN_PPM, tn.tp, DIN.TP, CHLA_PPB, TROPHIC_STATE, year,WGT_NLA, LON_DD, LAT_DD, AREA_HA, ELEV_PT, PCT_DEVELOPED_BSN, PCT_AGRIC_BSN, SITE_TYPE, URBAN, LAKE_ORIGIN, PTL_COND, NTL_COND, CHLA_COND, REFERENCE) |>
  mutate(NTL_PPB = NTL_PPM*1000) |>
  rename(DIN.TP_molar = DIN.TP,
         TN.TP_molar = tn.tp) |>
  filter(AREA_HA >= 4) |> # removes 382 observations 
  distinct()

tinylakesobs <- all_NLA |>
  distinct() |>
  filter(AREA_HA < 4) |>
  distinct() # 309 observations

tinylakes_lake <- tinylakesobs |>
  select(UNIQUE_ID, SITE_TYPE) |>
  distinct() # 269 lakes, 8 of these were reference lakes 

rm(tinylakes_lake)
rm(tinylakesobs)


#### 1. General trends in the data ####
# limits1 <- nla_data_subset |>
#   mutate(logDINP = log10(DIN.TP_molar)) |>
#   mutate(year = as.factor(year))
# 
# #stoichiometry across years
# ggplot(limits1, aes(year, logDINP)) +
#   geom_boxplot()
# anova(aov(logDINP~year, limits1)) # p < 2.2e-16 
# TukeyHSD(aov(logDINP~year, limits1), conf.level = 0.95) # p < 0.001 - all groups are different at the 95% CI
# plot(TukeyHSD(aov(logDINP~year, limits1), conf.level = 0.95), las = 2)
# 
# # stoichiometry across limitation categories
# ggplot(limits1, aes(limitation, logDINP)) +
#   geom_boxplot()
# anova(aov(logDINP~limitation, limits1)) # p < 2.2e-16 
# TukeyHSD(aov(logDINP~limitation, limits1), conf.level = 0.95) # p < 0.001 - all groups are different at the 95% CI
# plot(TukeyHSD(aov(logDINP~limitation, limits1), conf.level = 0.95), las = 2)
# 
# # stoichioemtery across limitation categories and trophic states 
# ggplot(limits1, aes(limitation, logDINP, color = TROPHIC_STATE)) +
#   geom_boxplot()
# anova(aov(logDINP~limitation*TROPHIC_STATE, limits1)) # p < 2.2e-16 
# TukeyHSD(aov(logDINP~limitation*TROPHIC_STATE, limits1), conf.level = 0.95) # various, some p < 0.05, many p <0.0001. 14 relationships are not significantly different
# plot(TukeyHSD(aov(logDINP~limitation*TROPHIC_STATE, limits1), conf.level = 0.95), las = 2)

# How many reservoirs are there in the western U.S. vs eastern U.S. NLA?
reservoir <- nla_data_subset |>
  select(ECO_REG_NAME, UNIQUE_ID, LAKE_ORIGIN, year) |>
  distinct() |>
  mutate(region = ifelse(ECO_REG_NAME %in% c('Western Mountains', 'Xeric', 'Northern Plains', 'Southern Plains'), 'west', 'east')) |>
  group_by(year, LAKE_ORIGIN, region) |>
  mutate(n=n()) |>
  ungroup() |>
  group_by(year, region) |>
  mutate(total = n()) |>
  select(-UNIQUE_ID, -ECO_REG_NAME) |>
  distinct() |>
  filter(LAKE_ORIGIN == 'MAN-MADE') |>
  mutate(percentRES = (n/total) * 100)



#### 2. Nutrient correlations with eutrophication ####
# use logged parameters
# create wide dataframe with nutrient as a column
correlations_data <- nla_data_subset |>
  # filter(year == "2007") |> # same as total except TN better predictor in coastal plains
  # filter(year == "2017") |> # same as total except TP better predictor in xeric and w. mtn
  # filter(year == '2012') |> # same as total except TN better predictor in temperate plains
  pivot_longer(cols = c(PTL_PPB, NTL_PPB), names_to = "nutrient", values_to = "concentration") 

correlations_data$ECO_REG_NAME = factor(correlations_data $ECO_REG_NAME,
                                        levels = c("Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))

m.0 <- aov(log10(CHLA_PPB)~log10(concentration) * nutrient, correlations_data  |> filter(is.finite(log10(CHLA_PPB))))
summary(m.0) # all vars significant
tidy(m.0)
glance(m.0) #  r = 0.505, AIC = 8300

m.0region <- aov(log10(CHLA_PPB)~log10(concentration) * nutrient * ECO_REG_NAME, correlations_data  |> filter(is.finite(log10(CHLA_PPB))))
summary(m.0region) # all vars significant 
anova(m.0, m.0region) # adding Ecoregion is a significantly better model
glance(m.0region) # r = 0.594, AIC = 7063
performance::r2(m.0region) # adj r = 0.592

m.1region <- lmer(log10(CHLA_PPB)~log10(concentration) * nutrient * factor(year) + (1|ECO_REG_NAME), correlations_data  |> filter(is.finite(log10(CHLA_PPB))))
summary(m.1region) 
anova(m.1region) 
anova(m.1region, m.0region) #m.0region has lower AIC and is significantly better, i.e., adding year doesn't improve the model
performance::r2(m.1region) #Conditional R2: 0.573, Marginal R2: 0.482


##Table of values from linear models 
# first get info from national model
m.P <- lm(log10(CHLA_PPB)~log10(concentration), correlations_data |> filter(is.finite(log10(CHLA_PPB)), nutrient == "PTL_PPB"))
m.N <- lm(log10(CHLA_PPB)~log10(concentration), correlations_data |> filter(is.finite(log10(CHLA_PPB)), nutrient == "NTL_PPB"))

slopeP <- coef(m.P)[2] # slope of linear model of Chla vs. P
slopeN <- coef(m.N)[2] # slope of linear model of Chla vs. N
intP <- coef(m.P)[1] # intercept of linear model of Chla vs. P
intN <- coef(m.N)[1] # intercept of linear model of Chla vs. N
rP <- as.numeric(glance(m.P)[2]) # adj. r-squraed of linear model of Chla vs. P
rN <- as.numeric(glance(m.N)[2]) # adj. r-squraed of linear model of Chla vs. N
pP <- as.numeric(glance(m.P)[5]) # p-value of linear model of Chla vs. P
pN <- as.numeric(glance(m.N)[5]) # p-value of linear model of Chla vs. N
aicP <- AIC(m.P)
aicN <- AIC(m.N)

Ecoregion <- c("National", "National")
`Model Predictor` <- c("PTL_PPB", "NTL_PPB")
Slope <- c(slopeP, slopeN)
Intercept <- c(intP, intN)
`r-squared` <- c(rP, rN)
`p-value` <- c(pP, pN)
AIC <- c(aicP, aicN)

# by ecoregion
list <- as.vector(correlations_data |> select(ECO_REG_NAME) |> distinct())[["ECO_REG_NAME"]]

#add national information
lm_ecoreg_df <- data.frame()
lm_ecoreg_df <- cbind(data.frame(Ecoregion), data.frame(`Model Predictor`), data.frame(Slope),  data.frame(AIC), data.frame(`r-squared`), data.frame(`p-value`))

# add ecoregional information
for(name in list) {
  m.P <- lm(log10(CHLA_PPB)~log10(concentration), correlations_data |> filter(is.finite(log10(CHLA_PPB)), ECO_REG_NAME == name, nutrient == "PTL_PPB"))
  m.N <- lm(log10(CHLA_PPB)~log10(concentration), correlations_data |> filter(is.finite(log10(CHLA_PPB)), ECO_REG_NAME == name, nutrient == "NTL_PPB"))
  
  
  slopeP <- coef(m.P)[2] # slope of linear model of Chla vs. P
  slopeN <- coef(m.N)[2] # slope of linear model of Chla vs. N
  intP <- coef(m.P)[1] # intercept of linear model of Chla vs. P
  intN <- coef(m.N)[1] # intercept of linear model of Chla vs. N
  rP <- as.numeric(glance(m.P)[2]) # adj. r-squraed of linear model of Chla vs. P
  rN <- as.numeric(glance(m.N)[2]) # adj. r-squraed of linear model of Chla vs. N
  pP <- as.numeric(glance(m.P)[5]) # p-value of linear model of Chla vs. P
  pN <- as.numeric(glance(m.N)[5]) # p-value of linear model of Chla vs. N
  aicP <- AIC(m.P) # AIC of linear model of Chla vs. N
  aicN <- AIC(m.N) # AIC of linear model of Chla vs. N
  
  
  Ecoregion <- c(name, name)
  `Model Predictor` <- c("PTL_PPB", "NTL_PPB")
  Slope <- c(slopeP, slopeN)
  Intercept <- c(intP, intN)
  `r-squared` <- c(rP, rN)
  `p-value` <- c(pP, pN)
  AIC <- c(aicP, aicN)
  
  tmp <- cbind(data.frame(Ecoregion), data.frame(`Model Predictor`), data.frame(Slope), data.frame(AIC), data.frame(`r-squared`), data.frame(`p-value`))
  
  lm_ecoreg_df <- bind_rows(lm_ecoreg_df, tmp) |>
    distinct()
  
}

lm_ecoreg_df1 <- lm_ecoreg_df |>
  select(-p.value)


lm_ecoreg_df1$Ecoregion = factor(lm_ecoreg_df1$Ecoregion,
                                 levels = c("National","Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))


add_corr <- lm_ecoreg_df1 |>
  rename(nutrient = Model.Predictor,
         ECO_REG_NAME = Ecoregion) |>
  filter(ECO_REG_NAME != "National") |>
  mutate(AIC = paste0("AIC=",round(AIC, digits = 1)),
         r.squared = paste0("adj.r=", round(r.squared, digits=2)))

library(lemon)
options(scipen = 999)
ggplot(correlations_data, aes(concentration, CHLA_PPB)) +
  geom_abline(slope = 0, intercept = log10(2)) +
  geom_abline(slope = 0, intercept = log10(7)) +
  geom_abline(slope = 0, intercept = log10(30)) +
  scale_y_log10() +
  scale_x_log10() +
  geom_point(alpha = 0.25, aes(color = nutrient)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = nutrient), color = "grey60") +
  geom_text(add_corr |> filter(nutrient == "NTL_PPB"), mapping = aes(x = 1000, y = 5000, label = r.squared), size = 2.5, vjust = 0.75, hjust = 0) +
  geom_text(add_corr |> filter(nutrient == "NTL_PPB"), mapping = aes(x = 1000, y = 900, label = AIC), size = 2.5, vjust = 0.75, hjust = 0) +
  geom_text(add_corr |> filter(nutrient == "PTL_PPB"), mapping = aes(x = 1, y = 5000, label = r.squared), size = 2.5, vjust = 0.75, hjust = 0) +
  geom_text(add_corr |> filter(nutrient == "PTL_PPB"), mapping = aes(x = 1, y = 900, label = AIC), size = 2.5, vjust = 0.75, hjust = 0) +
  facet_rep_wrap(~nutrient, scales = "free_y") +
  annotation_logticks(
    short = unit(0,"mm"),
    mid = unit(0.1, "cm"),
    long = unit(0.2, "cm"),
  ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  facet_wrap(~ECO_REG_NAME, ncol = 3) +
  labs(y = "log-scale chlorophyll-a concentration "~(mu*g~L^-1),
       x = "log-scale nutrient concentration "~(mu*g~L^-1)) +
  scale_color_manual("", labels = c("TN", "TP"), values=c("red4", "#336a98")) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"),
        strip.placement = "outside",             # place facet strips outside axis
        axis.ticks.length = unit(-2.75, "pt"),   # point axis ticks inwards (2.75pt is 
        # the default axis tick length here)
        axis.text.x.top = element_blank(),       # do not show top / right axis labels
        axis.text.y.right = element_blank(),     # for secondary axis
        axis.title.x.top = element_blank(),      # as above, don't show axis titles for
        axis.title.y.right = element_blank())    # secondary axis either)

ggsave("Figures/linregs.png", height = 4.5, width = 6.5, units = "in", dpi = 1200) 

# NOTE: I also ran these for just 2007 and just 2017 data. -- results on lines 87-89 of this script

# create a map of the better predictor in each ecoregion
#read in the ecoregion shapefiles
regions.sf <- read_sf("Data/aggr_ecoregions_2015/Aggr_Ecoregions_2015.shp") |>
  mutate(WSA9_NAME = ifelse(WSA9_NAME == "Temporate Plains", "Temperate Plains", WSA9_NAME)) |>
  rename(ECO_REG_NAME = WSA9_NAME)


#determine better predictor in each
compare_r_values <- lm_ecoreg_df |>
  select(-Slope, - p.value) |>
  pivot_wider(names_from = "Model.Predictor", values_from = c("AIC", "r.squared")) |>
  mutate(best_predictor = ifelse(
    AIC_PTL_PPB < AIC_NTL_PPB & r.squared_PTL_PPB > r.squared_NTL_PPB, "TP", "TN"
  )) |>
  rename(ECO_REG_NAME=Ecoregion)

# combine shapefiles with better predictor information
regions.sf1 <- left_join(regions.sf, compare_r_values)


# create the map
ggplot(data = regions.sf1) +
  geom_sf(aes(fill = best_predictor), color = 'black') +
  theme_bw() +
  geom_sf_label(aes(label = WSA9_NAME), size = 2.5) +
  labs(x = "", y = "") +
  scale_fill_manual("", values=c("red4", "#336a98")) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif")) +
  theme(legend.position = c(0.1, 0.2))
ggsave("Figures/Map_NP.png", height = 4.5, width = 6.5, units = "in", dpi = 1200) 


# faceted unlabelled map to add to lin regs plot (doing in PP because my laptop can't handle the purrr method to add these)

ggplot() +
  geom_sf(regions.sf |> select(-ECO_REG_NAME), mapping=aes(),fill='white', color='grey30') +
  geom_sf(regions.sf1, mapping=aes(fill=best_predictor),color='black') +
  theme_minimal() +
  labs(x = "", y = "") +
  scale_fill_manual("", values=c("red4", "#336a98")) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"),
        legend.position = 'none',
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  facet_wrap(~ECO_REG_NAME)

ggsave("Figures/maps_4_linregs.png", height = 4.5, width = 6.5, units = "in", dpi = 1200) 




## S1. Create map with just ecoregions ####
muted <- c("#CC6677", "#332288", "#DDCC77", "#117733", "#88CCEE", "#882255", "#44AA99", "#999933", "#AA4499" ) # ecoreg color scheme



ggplot() +
  geom_sf(data = regions.sf, aes(fill = WSA9_NAME)) +
  scale_fill_manual("", values = muted) +
  theme_bw()

ggsave("Figures/S1_ecoregions.png", height = 4.5, width = 6.5, units = "in", dpi = 1200) 


#### 3. Calculate limitations ####
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
         log_fractionyieldN = log10(CHLA_PPB)/log10chla_HY_N) 
median_fracyields <- HighYield_Chla |>
  mutate(fracyieldTN = 10^log_fractionyieldN,
         fracyieldTP = 10^log_fractionyieldP) |>
  group_by(ECO_REG_NAME) |>
  summarise(med_fracyield_N = median(fracyieldTN),
            med_fracyield_P = median(fracyieldTP)) |>
  ungroup()


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

median_NP_tippoints <- limits |>
  group_by(ECO_REG_NAME) |>
  summarise(mediantippoint = median(tipping_pt_NP_molar),
            median_deviation = median(NP_deviation))

nrow(limits |> filter(limitation == "P-limitation")) # 436
nrow(limits |> filter(limitation == "N-limitation")) # 350
nrow(limits |> filter(limitation == "Co-limitation")) # 2484

nrow(limits |> filter(limitation == "P-limitation"))/nrow(limits) *100 # 13.3%
nrow(limits |> filter(limitation == "N-limitation"))/nrow(limits) *100 # 10.7%
nrow(limits |> filter(limitation == "Co-limitation"))/nrow(limits) *100 # 76%


## plot the limited lakes
ggplot(limits) +
  geom_point(aes(log(PTL_PPB, base = 10), log(NTL_PPM, base = 10), fill = limitation), size = 2.5, shape = 21, alpha = 0.8) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_fill_manual("",values = c("grey60","red4", "#336a98")) +
  labs(y = "Log TN"~(m*g~L^-1), x = "Log TP"~(mu*g~L^-1)) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"))
ggsave("Figures/limitedlakes_dist.png", height = 4.5, width = 6.5, units = "in", dpi = 1200) 


#### 3. Spatial, temporal distribution of limitations ####
# Percent lakes in each ecoregion 
# prep the data
limits_survey_prep <- limits|> # total 2768 lakes for this analysis 
  filter(VISIT_NO == 1) |># for this analysis, we are using just the first visit from each lake
  filter(WGT_NLA > 0) # the sp survey package is not designed to use the reference lakes, so those are ignored when using this package for analyses.

# lakes with limitations considered for this analysis now 
nrow(limits_survey_prep |> filter(limitation == "P-limitation")) # 362
nrow(limits_survey_prep |> filter(limitation == "N-limitation")) # 307
nrow(limits_survey_prep |> filter(limitation == "Co-limitation")) # 2099

# use categorical analysis from spsurvey package
years <- c("2007","2012", "2017")
percent_lim <- data.frame()

for(i in 1:length(years)) {
  data <- limits_survey_prep |>
    filter(year == years[i])
  
  tmp <- cat_analysis(
    data,
    siteID = "UNIQUE_ID",
    vars = "limitation",
    weight = "WGT_NLA",
    subpops = "ECO_REG_NAME",
    xcoord = "LON_DD",
    ycoord = "LAT_DD"
  )
  
  tmp <- tmp |>
    mutate(year = years[i])
  
  percent_lim <- rbind(percent_lim, tmp)
}

nat <- cat_analysis(
  limits_survey_prep,
  siteID = "UNIQUE_ID",
  vars = "limitation",
  weight = "WGT_NLA",
  subpops = "year",
  xcoord = "LON_DD",
  ycoord = "LAT_DD"
)

nat1 <- nat |>
  mutate(year = Subpopulation,
         Subpopulation = "National")

percent_lim1 <- rbind(percent_lim, nat1) |>
  filter(Category != "Total")
percent_lim1$Subpopulation = factor(percent_lim1$Subpopulation,
                                    levels = c("National", "Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))

text_percents_lim <- percent_lim1 |>
  select(year, Subpopulation, Category, Estimate.P) |>
  filter(Estimate.P > 10) |>
  mutate(perc = paste0(round(Estimate.P, digits = 0), "%")) 



ggplot(percent_lim1, aes(year, Estimate.P, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(text_percents_lim, mapping = aes(label = perc), position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(~Subpopulation) +
  scale_fill_manual("", values = c("grey60","red4", "#336a98")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 6.5)) +
  labs(x = "", y = "% lakes") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"),
        legend.position = c(0.75, 0.165))

ggsave("Figures/limitbars_ecoreg.png", height = 4.5, width = 6.5, units = "in", dpi = 1200)



lim_natplot <- ggplot(percent_lim1 |>
                        filter(Subpopulation == "National"), aes(year, Estimate.P, fill = Category, shape=Category)) +
  geom_point(size=2, color = "black", position=position_dodge(width=0.5)) +
  geom_errorbar(aes(color = Category, year, Estimate.P, ymin = Estimate.P - StdError.P, ymax = Estimate.P + StdError.P, color = Category), width = 0.2, position=position_dodge(width=0.5)) +
  scale_color_manual("",values = c("grey60","red4", "#336a98"))  +
  scale_fill_manual("",values = c("grey60","red4", "#336a98")) +
  scale_shape_manual('',values = c(21,22,23)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  geom_vline(xintercept = c(1.5, 2.5)) +
  labs(x = '',
       y = '% U.S. lakes') +
  theme(legend.title = element_blank(),
        legend.position='bottom',
        legend.margin = margin(t = 0.1,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0), # Left margin
        legend.box.margin=margin(-10,-10,-10,-10),
        legend.box.spacing = unit(0, "pt")) 

lim_regplot <- ggplot(percent_lim1 |>
                        filter(Subpopulation != "National"), aes(year, Estimate.P, fill = Category, shape=Category)) +
  geom_point(size=2, color = "black", position=position_dodge(width=0.5)) +
  geom_errorbar(aes(color = Category, year, Estimate.P, ymin = Estimate.P - StdError.P, ymax = Estimate.P + StdError.P, color = Category), width = 0.2, position=position_dodge(width=0.5)) +
  scale_color_manual("",values = c("grey60","red4", "#336a98"))  +
  scale_fill_manual("",values = c("grey60","red4", "#336a98")) +
  scale_shape_manual('',values = c(21,22,23)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  geom_vline(xintercept = c(1.5, 2.5)) +
  labs(x = '',
       y = '% lakes in ecoregion') +
  theme(legend.title = element_blank(),
        legend.position='bottom',
        legend.margin = margin(t = 0.1,  # Top margin
                               r = 0,  # Right margin
                               b = 0,  # Bottom margin
                               l = 0), # Left margin
        legend.box.margin=margin(-10,-10,-10,-10),
        legend.box.spacing = unit(0, "pt"))  +
  facet_wrap(~Subpopulation, ncol = 3)



regions_lim <- regions.sf |>
  rename(Subpopulation = WSA9_NAME) |>
  left_join(percent_lim1)

ggplot() +
  geom_sf(data = regions_lim |> 
            filter(Category == 'Co-limitation'), aes(fill = Estimate.P)) +
  scale_fill_gradient('% Co-limited lakes', low='grey95',high='grey60') +
  ggthemes::theme_map() +
  facet_wrap(~year, nrow=1) +
  theme(legend.position = 'bottom')

ggsave("Figures/percentLimitationsMaps_CO.png", height = 3, width = 6.5, units = "in", dpi = 1200) 

ggplot() +
  geom_sf(data = regions_lim |> 
            filter(Category == 'N-limitation'), aes(fill = Estimate.P)) +
  scale_fill_gradient('% N-limited lakes', low='grey95',high='red4') +
  ggthemes::theme_map() +
  facet_wrap(~year, nrow=1) +
  theme(legend.position = 'bottom')

ggsave("Figures/percentLimitationsMaps_N.png", height = 3, width = 6.5, units = "in", dpi = 1200) 

ggplot() +
  geom_sf(data = regions_lim |> 
            filter(Category == 'P-limitation'), aes(fill = Estimate.P)) +
  scale_fill_gradient('% P-limited lakes', low='grey95',high='#336a98') +
  ggthemes::theme_map() +
  facet_wrap(~year, nrow=1) +
  theme(legend.position = 'bottom')

ggsave("Figures/percentLimitationsMaps_P.png", height = 3, width = 6.5, units = "in", dpi = 1200) 


#### 4. Limitations change analysis 2007 to 2017 ####
## find only sites that are sampled in 2007 and 2017
crossover <- limits_survey_prep |>
  select(year, UNIQUE_ID) |>
  filter(year == "2007") |>
  rename(year1 = year) |>
  distinct() |>
  # inner_join(limits_survey_prep |>
  #              select(year, UNIQUE_ID) |>
  #              filter(year == "2012") |>
  #              rename(year3 = year) |>
  #              distinct()) |> #346 sites sampled in 2007 and 2012
  inner_join(limits_survey_prep |>
               select(year, UNIQUE_ID) |>
               filter(year == "2017") |>
               rename(year2 = year) |>
               distinct()) |>
  select(UNIQUE_ID) # 232 sites sampled in all 3 years

limits_change_prep <- limits_survey_prep |> # analysis has same restrictions as categorical analysis
  inner_join(crossover) |> # this keeps only resampled sites
  filter(year != '2012') # we are only looking at change over the full period
#464 lakes

# use change_analysis from the spsurvey package
# ecoregional change analysis
change_ecoreg <- change_analysis(limits_change_prep, subpops = "ECO_REG_NAME", siteID = "UNIQUE_ID", vars_cat = "limitation", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
change_ecoreg.1 <- change_ecoreg[["catsum"]]  |>
  select(Subpopulation, Category, Indicator, DiffEst.P, MarginofError.P) 
warnprnt()
# national change analysis
change_nat <- change_analysis(limits_change_prep, siteID = "UNIQUE_ID", vars_cat = "limitation", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
change_nat.1 <- change_nat[["catsum"]]  |>
  select(Subpopulation, Category, Indicator, DiffEst.P, MarginofError.P)  |>
  mutate(Subpopulation = "National")
warnprnt()


lim_change0717 <- rbind(change_ecoreg.1, change_nat.1)|>
  mutate(year.shift = "2007-2017")
lim_change0717$Subpopulation = factor(lim_change0717$Subpopulation,
                                      levels = c("National","Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))


#### Now run the analysis on the full population of lakes
limits_change_prep <- limits_survey_prep |> # analysis has same restrictions as categorical analysis
  filter(year != '2012')

# use change_analysis from the spsurvey package
# ecoregional change analysis
change_ecoreg <- change_analysis(limits_change_prep, subpops = "ECO_REG_NAME", siteID = "UNIQUE_ID", vars_cat = "limitation", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
change_ecoreg.1 <- change_ecoreg[["catsum"]]  |>
  select(Subpopulation, Category, Indicator, DiffEst.P, MarginofError.P) 
warnprnt()
# national change analysis
change_nat <- change_analysis(limits_change_prep, siteID = "UNIQUE_ID", vars_cat = "limitation", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
change_nat.1 <- change_nat[["catsum"]]  |>
  select(Subpopulation, Category, Indicator, DiffEst.P, MarginofError.P)  |>
  mutate(Subpopulation = "National")
warnprnt()


lim_change0717_allSITES <- rbind(change_ecoreg.1, change_nat.1)|>
  mutate(year.shift = "2007-2017")
lim_change0717_allSITES$Subpopulation = factor(lim_change0717_allSITES$Subpopulation,
                                               levels = c("National","Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))



## Combine the resampled with the full population into one graph for easier comparison?!
lim_changes_fullset <- rbind(lim_change0717 |> mutate(sample_set = "Resampled lakes"), lim_change0717_allSITES |> mutate(sample_set = "All surveyed lakes"))

# compare confidence interval between all surveyed lakes and resampled lakes
t.test((lim_changes_fullset |> filter(sample_set == "Resampled lakes"))$MarginofError.P, (lim_changes_fullset |> filter(sample_set != "Resampled lakes"))$MarginofError.P) # p = 0.01456
ggplot(lim_changes_fullset, aes(sample_set, MarginofError.P)) +
  geom_boxplot()
# error is larger in resampled lakes

# compare all to resampled lakes within each category and region to find statistical differences for plotting
comparison_lim <- lim_changes_fullset |>
  mutate(lwr.est = DiffEst.P-MarginofError.P,
         upr.est = DiffEst.P+MarginofError.P) |>
  pivot_longer(c('DiffEst.P','lwr.est','upr.est'), names_to='est',values_to='values') |>
  select(-MarginofError.P, -year.shift, -est, -Indicator) |>
  pivot_wider(names_from='sample_set', values_from='values') |>
  unchop(everything()) |>
  group_by(Subpopulation, Category) |>
  mutate(p.value = t.test(`Resampled lakes`,`All surveyed lakes`)[['p.value']]) |>
  ungroup() |>
  filter(p.value <=0.05) |>
  select(-4,-3) |>
  unique() |>
  mutate(p.value = '*')  
# None were statistically different!

# add significance to dataset
lim_changes_fullset <- left_join(lim_changes_fullset, comparison_lim) |>
  mutate(p.value=ifelse(sample_set!='Resampled lakes', NA, p.value))

ecoreg_plot_limchange <- lim_changes_fullset |>
  filter(Subpopulation != "National") |>
  ggplot() +
  geom_point(aes(Category, DiffEst.P, fill = Category, shape=Category, group = sample_set),
             color = "black", size = 2,
             position = position_dodge(width = 0.75)) +
  geom_errorbar(aes(Category, DiffEst.P,
                    ymin = DiffEst.P - MarginofError.P,
                    ymax = DiffEst.P + MarginofError.P,
                    color = Category, linetype = sample_set),
                width = 0.2,
                position = position_dodge(width = 0.75)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  facet_wrap(~Subpopulation, ncol = 3, scales = "free_y") +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% change 2007-2017") +
  scale_color_manual("",values = c("grey60","red4", "#336a98"))  +
  scale_fill_manual("",values = c("grey60","red4", "#336a98")) +
  scale_shape_manual('',values = c(21,22,23)) +
  theme(strip.text.x = element_text(size = 7.5),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"),
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        legend.position='bottom',
        legend.margin = margin(t = 0,  # Top margin
                               r = 0,  # Right margin
                               b = 0,  # Bottom margin
                               l = 0), # Left margin
        legend.box.margin=margin(-10,-10,-10,-10),
        legend.box.spacing = unit(0, "pt")) +
  guides(color='none',
         shape='none',
         fill = 'none')


#national plot
nat_plot_limchange <- lim_changes_fullset %>%
  filter(Subpopulation == "National") %>%
  ggplot() +
  geom_point(aes(Category, DiffEst.P, fill = Category, shape=Category, group = sample_set),
             color = "black",  size = 2,
             position = position_dodge(width = 0.75)) +
  geom_errorbar(aes(Category, DiffEst.P,
                    ymin = DiffEst.P - MarginofError.P,
                    ymax = DiffEst.P + MarginofError.P,
                    color = Category, linetype = sample_set),
                width = 0.2,
                position = position_dodge(width = 0.75)) +
  geom_text(aes(Category, DiffEst.P, label = p.value),
            nudge_x = -0.25, nudge_y = 0.25) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% change 2007-2017") +
  scale_color_manual("", values = c("grey60", "red4", "#336a98")) +
  scale_fill_manual("", values = c("grey60", "red4", "#336a98")) +
  scale_shape_manual('',values = c(21,22,23)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.75, 0.75)) +
  guides(color='none',
         shape='none',
         fill = 'none')


layout <- "
AAAA
AAAA
BBBB
BBBB
"

lim_natplot/nat_plot_limchange +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') 

ggsave("Figures/F3_national_limitations.png", height = 5.5, width = 6.5, units = "in", dpi = 1200) 

lim_regplot/ecoreg_plot_limchange +
  plot_layout(
              design = layout) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') 

ggsave("Figures/S2_ecoregions_limitations.png", height = 8, width = 6.5, units = "in", dpi = 1200) 




#### 5. Spatial, temporal distribution of trophic state ####
# how many observations of each trophic state
nrow(limits |> filter(TROPHIC_STATE == "Oligo.")) # 520
nrow(limits |> filter(TROPHIC_STATE == "Meso.")) # 1047
nrow(limits |> filter(TROPHIC_STATE == "Eutro.")) # 990
nrow(limits |> filter(TROPHIC_STATE == "Hyper.")) # 713
# Percent lakes in each ecoregion 
# use categorical analysis from spsurvey package
years <- c("2007",'2012', "2017")
percent_TS <- data.frame()

for(i in 1:length(years)) {
  data <- limits_survey_prep |>
    filter(year == years[i])
  
  tmp <- cat_analysis(
    data,
    siteID = "UNIQUE_ID",
    vars = "TROPHIC_STATE",
    weight = "WGT_NLA",
    subpops = "ECO_REG_NAME",
    xcoord = "LON_DD",
    ycoord = "LAT_DD"
  )
  
  tmp <- tmp |>
    mutate(year = years[i])
  
  percent_TS <- rbind(percent_TS, tmp)
}

nat <- cat_analysis(
  limits_survey_prep,
  siteID = "UNIQUE_ID",
  vars = "TROPHIC_STATE",
  weight = "WGT_NLA",
  subpops = "year",
  xcoord = "LON_DD",
  ycoord = "LAT_DD"
)

nat1 <- nat |>
  mutate(year = Subpopulation,
         Subpopulation = "National")

percent_TS1 <- rbind(percent_TS, nat1) |>
  filter(Category != "Total")
percent_TS1$Subpopulation = factor(percent_TS1$Subpopulation,
                                   levels = c("National", "Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))
percent_TS1$Category = factor(percent_TS1$Category,
                              levels = c("Oligo.", "Meso.", "Eutro.", "Hyper."))


text_percents_ts <- percent_TS1 |>
  select(year, Subpopulation, Category, Estimate.P) |>
  filter(Estimate.P > 10) |>
  mutate(perc = paste0(round(Estimate.P, digits = 0), "%"))


ggplot(percent_TS1, aes(year, Estimate.P, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(text_percents_ts, mapping = aes(label = perc), position = position_stack(vjust = 0.7), size = 3) +
  facet_wrap(~Subpopulation) +
  scale_fill_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1])) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 6.5)) +
  labs(x = "", y = "% lakes") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"),
        legend.position = c(0.75, 0.175)) +
  guides(fill = guide_legend(ncol=2))

ggsave("Figures/S3_TSbars_ecoreg_pub.png", height = 4.5, width = 6.5, units = "in", dpi = 1200)


#### 6. Trophic state change analysis within limitation status at the national level ####
## find only sites that are sampled in both 2007 and 2017
crossover <- limits_survey_prep |>
  select(year, UNIQUE_ID) |>
  filter(year == "2007") |>
  rename(year1 = year) |>
  distinct() |>
  inner_join(limits_survey_prep |>
               select(year, UNIQUE_ID) |>
               filter(year == "2017") |>
               rename(year2 = year) |>
               distinct()) |>
  select(UNIQUE_ID) # 232 sites

TS_change_prep <- limits_survey_prep |> # analysis has same restrictions as categorical analysis
  inner_join(crossover) |> # this keeps only resampled sites
  filter(year != '2012')

# use change_analysis from the spsurvey package
# national change analysis
change_nat <- change_analysis(TS_change_prep, subpops = "limitation", siteID = "UNIQUE_ID", vars_cat = "TROPHIC_STATE", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
change_nat.1 <- change_nat[["catsum"]]  |>
  select(Subpopulation, Category, Indicator, DiffEst.P, MarginofError.P)  
warnprnt()
# also get changes without limitation status defined:
change_nat_full <- change_analysis(TS_change_prep, siteID = "UNIQUE_ID", vars_cat = "TROPHIC_STATE", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
change_nat_full.1 <- change_nat_full[["catsum"]]  |>
  select(Category, Indicator, DiffEst.P, MarginofError.P) |>
  mutate(Subpopulation = "All lakes & limitations")
warnprnt()

TS_change0717 <- rbind(change_nat.1, change_nat_full.1)
TS_change0717$Category = factor(TS_change0717$Category,
                                levels = c("Oligo.", "Meso.", "Eutro.", "Hyper."))



#### Now use the full population of lakes, not just resampled. 
TS_change_prep <- limits_survey_prep |> # analysis has same restrictions as categorical analysis
  filter(year != '2012')

# use change_analysis from the spsurvey package
# national change analysis
change_nat <- change_analysis(TS_change_prep, subpops = "limitation", siteID = "UNIQUE_ID", vars_cat = "TROPHIC_STATE", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
change_nat.1 <- change_nat[["catsum"]]  |>
  select(Subpopulation, Category, Indicator, DiffEst.P, MarginofError.P)  
warnprnt()
# also get changes without limitation status defined:
change_nat_full <- change_analysis(TS_change_prep, siteID = "UNIQUE_ID", vars_cat = "TROPHIC_STATE", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
change_nat_full.1 <- change_nat_full[["catsum"]]  |>
  select(Category, Indicator, DiffEst.P, MarginofError.P) |>
  mutate(Subpopulation = "All lakes & limitations")
warnprnt()

TS_change0717_allSITES <- rbind(change_nat.1, change_nat_full.1)
TS_change0717_allSITES$Category = factor(TS_change0717_allSITES$Category,
                                         levels = c("Oligo.", "Meso.", "Eutro.", "Hyper."))


## Combine the resampled with the full population into one graph for easier comparison?!
TS_changes_fullset  <- rbind(TS_change0717 |> mutate(sample_set = "Resampled lakes"), TS_change0717_allSITES |> mutate(sample_set = "All surveyed lakes"))
TS_changes_fullset$Category = factor(TS_changes_fullset$Category,
                                     levels = c("Oligo.", "Meso.", "Eutro.", "Hyper."))


# compare confidence interval between all surveyed lakes and resampled lakes
t.test((TS_changes_fullset |> filter(sample_set == "Resampled lakes"))$MarginofError.P, (TS_changes_fullset |> filter(sample_set != "Resampled lakes"))$MarginofError.P) # p = 0.09637 - error is the same for trophic state
ggplot(TS_changes_fullset, aes(sample_set, MarginofError.P)) +
  geom_boxplot()

# compare all to resampled lakes within each category and region to find statistical differences for plotting
comparison_ts <- TS_changes_fullset |>
  mutate(lwr.est = DiffEst.P-MarginofError.P,
         upr.est = DiffEst.P+MarginofError.P) |>
  pivot_longer(c('DiffEst.P','lwr.est','upr.est'), names_to='est',values_to='values') |>
  select(-MarginofError.P, -est, -Indicator) |>
  pivot_wider(names_from='sample_set', values_from='values') |>
  unchop(everything()) |>
  group_by(Subpopulation, Category) |>
  mutate(p.value = t.test(`Resampled lakes`,`All surveyed lakes`)[['p.value']]) |>
  ungroup() |>
  filter(p.value <=0.05) |>
  select(-4,-3) |>
  unique() |>
  mutate(p.value = '*') 
# all were not statistically different!

# add significance to dataset
TS_changes_fullset <- left_join(TS_changes_fullset, comparison_ts) |>
  mutate(p.value=ifelse(sample_set!='Resampled lakes', NA, p.value))



#national plot
ggplot(TS_changes_fullset) +
  geom_point(aes(Category, DiffEst.P, fill = Category, group = sample_set),
             color = "black", pch = 21, size = 2,
             position = position_dodge(width = 0.75)) +
  geom_errorbar(aes(Category, DiffEst.P,
                    ymin = DiffEst.P - MarginofError.P,
                    ymax = DiffEst.P + MarginofError.P,
                    color = Category, linetype = sample_set),
                width = 0.2,
                position = position_dodge(width = 0.75)) +
  geom_text(aes(Category, DiffEst.P, label = p.value),
            nudge_x = -0.25, nudge_y = 0.25) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  facet_wrap(~Subpopulation, scales = "free_y", ncol = 2) +
  #facet_wrap(~Subpopulation, ncol = 3, scales = "free_y") +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% change 2007-2017")+                                                 
  scale_color_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]))  +
  scale_fill_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1])) +
  theme(axis.text.x = element_text(angle = 49, vjust = 1, hjust =1),
        strip.text.x = element_text(size = 7.5),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"),
        legend.title = element_blank(),
        legend.position = 'top') +
  guides(color=FALSE,
         fill= FALSE)

ggsave("Figures/F4_TSchanges_07-17.png", height = 4.5, width = 6.5, units = "in", dpi = 1200) 



#### 7. Trophic state change analysis within ecoregions ####
## find only sites that are sampled in both 2007 and 2017
crossover <- limits_survey_prep |>
  select(year, UNIQUE_ID) |>
  filter(year == "2007") |>
  rename(year1 = year) |>
  distinct() |>
  inner_join(limits_survey_prep |>
               select(year, UNIQUE_ID) |>
               filter(year == "2017") |>
               rename(year2 = year) |>
               distinct()) |>
  select(UNIQUE_ID) # 232 sites

TS_change_prep_eco <- limits_survey_prep |> # analysis has same restrictions as categorical analysis
  inner_join(crossover) |> # this keeps only resampled sites
  filter(year !='2012')

# use change_analysis from the spsurvey package
# ecoregional change analysis
change_eco <- change_analysis(TS_change_prep_eco, subpops = "ECO_REG_NAME", siteID = "UNIQUE_ID", vars_cat = "TROPHIC_STATE", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
change_eco.1 <- change_eco[["catsum"]]  |>
  select(Subpopulation, Category, Indicator, DiffEst.P, MarginofError.P)  
warnprnt()


TS_change0717eco <- change_eco.1
TS_change0717eco$Category = factor(TS_change0717eco$Category,
                                   levels = c("Oligo.", "Meso.", "Eutro.", "Hyper."))



#### Now use the full population of lakes, not just resampled. 
TS_change_prep <- limits_survey_prep |> # analysis has same restrictions as categorical analysis
  filter(year != '2012')

# use change_analysis from the spsurvey package
# national change analysis
change_eco <- change_analysis(TS_change_prep, subpops = "ECO_REG_NAME", siteID = "UNIQUE_ID", vars_cat = "TROPHIC_STATE", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
change_eco.1 <- change_eco[["catsum"]]  |>
  select(Subpopulation, Category, Indicator, DiffEst.P, MarginofError.P)  
warnprnt()


TS_change0717_allSITESeco <- change_eco.1
TS_change0717_allSITESeco$Category = factor(TS_change0717_allSITESeco$Category,
                                            levels = c("Oligo.", "Meso.", "Eutro.", "Hyper."))


## Combine the resampled with the full population into one graph for easier comparison?!
TS_changes_fullseteco  <- rbind(TS_change0717eco |> mutate(sample_set = "Resampled lakes"), TS_change0717_allSITESeco |> mutate(sample_set = "All surveyed lakes"))
TS_changes_fullseteco$Category = factor(TS_changes_fullseteco$Category,
                                        levels = c("Oligo.", "Meso.", "Eutro.", "Hyper."))


# compare confidence interval between all surveyed lakes and resampled lakes
t.test((TS_changes_fullseteco |> filter(sample_set == "Resampled lakes"))$MarginofError.P, (TS_changes_fullseteco |> filter(sample_set != "Resampled lakes"))$MarginofError.P) # p =  0.004014 - errors are different
ggplot(TS_changes_fullseteco, aes(sample_set, MarginofError.P)) +
  geom_boxplot()

# compare all to resampled lakes within each category and region to find statistical differences for plotting
comparison_tseco <- TS_changes_fullseteco |>
  mutate(lwr.est = DiffEst.P-MarginofError.P,
         upr.est = DiffEst.P+MarginofError.P) |>
  pivot_longer(c('DiffEst.P','lwr.est','upr.est'), names_to='est',values_to='values') |>
  select(-MarginofError.P, -est, -Indicator) |>
  pivot_wider(names_from='sample_set', values_from='values') |>
  unchop(everything()) |>
  group_by(Subpopulation, Category) |>
  mutate(p.value = t.test(`Resampled lakes`,`All surveyed lakes`)[['p.value']]) |>
  ungroup() |>
  filter(p.value <=0.05) |>
  select(-4,-3) |>
  unique() |>
  mutate(p.value = '*') 

# add significance to dataset
TS_changes_fullseteco <- left_join(TS_changes_fullseteco, comparison_tseco) |>
  mutate(p.value=ifelse(sample_set!='Resampled lakes', NA, p.value))

# factor ecoregion names for consistent plotting
TS_changes_fullseteco$Subpopulation = factor(TS_changes_fullseteco$Subpopulation,
                                             levels = c("Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))

#ecoregion plot
ggplot(TS_changes_fullseteco) +
  geom_point(aes(Category, DiffEst.P, fill = Category, group = sample_set),
             color = "black", pch = 21, size = 2,
             position = position_dodge(width = 0.75)) +
  geom_errorbar(aes(Category, DiffEst.P,
                    ymin = DiffEst.P - MarginofError.P,
                    ymax = DiffEst.P + MarginofError.P,
                    color = Category, linetype = sample_set),
                width = 0.2,
                position = position_dodge(width = 0.75)) +
  geom_text(aes(Category, DiffEst.P, label = p.value),
            nudge_x = -0.25, nudge_y = 0.25) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  facet_wrap(~Subpopulation, scales = "free_y", ncol = 3) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% change 2007-2017")+                                                 
  scale_color_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]))  +
  scale_fill_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1])) +
  theme(axis.text.x = element_text(angle = 49, vjust = 1, hjust =1),
        strip.text.x = element_text(size = 7.5),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"),
        legend.title = element_blank()) +
  guides(color=FALSE,
         fill= FALSE)

ggsave("Figures/TSchangesECO_01-17.png", height = 4.5, width = 6.5, units = "in", dpi = 1200) 


# #### 8. Relationship between trophic state and limitation ####
#  Categorical analysis of trophic states within limitation categories

nat_limTS <- cat_analysis(
  limits_survey_prep,
  siteID = "UNIQUE_ID",
  vars = "TROPHIC_STATE",
  weight = "WGT_NLA",
  subpops = "limitation",
  xcoord = "LON_DD",
  ycoord = "LAT_DD"
) |>
  filter(Category != 'Total')

nat_limTS$Category = factor(nat_limTS$Category,
                            levels = c("Oligo.", "Meso.", "Eutro.", "Hyper."))


ggplot(nat_limTS) +
  geom_point(aes(Category, Estimate.P, fill=Category), color='black', pch=21, size=2) +
  facet_wrap(~Subpopulation) +
  geom_errorbar(aes(Category, Estimate.P, ymin = Estimate.P-MarginofError.P,
                    ymax = Estimate.P+MarginofError.P, color = Category), width = 0.2) +
  scale_color_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]))  +
  scale_fill_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1])) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 49, vjust = 1, hjust =1),
        strip.text.x = element_text(size = 7.5),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"),
        legend.position = 'none') +
  labs(x = '', y = '% lakes from all surveys')
ggsave("Figures/F5_trophicstates_bylim.png", height = 4.5, width = 6.5, units = "in", dpi = 1200) 

