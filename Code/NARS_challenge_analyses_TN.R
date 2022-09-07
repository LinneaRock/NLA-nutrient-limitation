
library(tidyverse)
library(colorblindr)
library(broom)
library(lme4)
library(gt)
library(spsurvey)
# library(sf) - messes with dplyr. So call and detach after use written in code
library(ggrepel)

source("Data/NLA/Call_NLA_data.R")
nla_data_subset <- all_NLA |>
  select(ECO_REG_NAME, UNIQUE_ID, DATE_COL, VISIT_NO, NTL_PPM, PTL_PPB, DIN_PPM, tn.tp, DIN.TP, CHLA_PPB, TROPHIC_STATE, year,WGT_NLA, LON_DD, LAT_DD, AREA_HA, ELEV_PT, PCT_DEVELOPED_BSN, PCT_AGRIC_BSN, SITE_TYPE, URBAN, LAKE_ORIGIN) |>
  rename(DIN.TP_molar = DIN.TP,
         TN.TP_molar = tn.tp) |>
  filter(year != "2012") |>
  distinct()


#### 1. Calculate limitations ####
# get information about the refernece lakes
ref_np <- nla_data_subset |>
  filter(SITE_TYPE %in% c("REF_Lake", "HAND")) |> # subset of 230 lakes
  group_by(ECO_REG_NAME) |>
  summarise(medianDINP = median(DIN.TP_molar, na.rm = TRUE),
            medianNP = (median(TN.TP_molar)),
            medianTN_PPM = median(NTL_PPM),
            medianTP_PPB = median(PTL_PPB),
            medianDIN_PPM = median(DIN_PPM)) |>
  ungroup()

# get some information about the entire dataset
averages_np <- nla_data_subset |>
  filter(is.finite(log(DIN.TP_molar))) |>
  group_by(ECO_REG_NAME) |>
  mutate(
    meanlogNP = mean(log(TN.TP_molar)),
    meanlogDINP = mean(log(DIN.TP_molar), na.rm = TRUE),
    medianlogNP = median(log(TN.TP_molar)),
    medianTN_PPM = median(NTL_PPM),
    medianTP_PPB = median(PTL_PPB),
    medianDIN_PPM = median(DIN_PPM),
    percentile25TN_PPM = quantile(NTL_PPM, probs = 0.25),
    percentile25TP_PPB = quantile(PTL_PPB, probs = 0.25),
    percentile25DIN_PPM = quantile(DIN_PPM, probs = 0.25)) |>
  ungroup() |>
  select(ECO_REG_NAME, meanlogNP, meanlogDINP, percentile25TN_PPM, percentile25TP_PPB, percentile25DIN_PPM) |>
  distinct()


# How do the lower 25th percentiles of TN and TP compare the the median concentrations of TN and TP in the reference lakes? 
t.test(ref_np$medianTN_PPM, averages_np$percentile25TN_PPM) # these are similar to each other!! 
t.test(ref_np$medianTP_PPB, averages_np$percentile25TP_PPB) # these are similar to each other!!
t.test(ref_np$medianDIN_PPM, averages_np$percentile25DIN_PPM) # these are similar to each other!!


# uses 25th percentile nutrient thresholds for each ecoregion and logged average TN:TP for each ecoregion 
limitsTN <- nla_data_subset|> #2520 lakes
  left_join(averages_np) |>
  mutate(limitation = NA) |>
  mutate(limitation = ifelse(PTL_PPB > percentile25TP_PPB & log(TN.TP_molar) < meanlogNP, "N-limitation", 
                             ifelse(NTL_PPM > percentile25TN_PPM & log(TN.TP_molar) > meanlogNP, "P-limitation",
                                    ifelse(is.na(limitation), "Co-nutrient limitation", limitation))))

nrow(limitsTN |> filter(limitation == "P-limitation")) # 830
nrow(limitsTN |> filter(limitation == "N-limitation")) # 1223
nrow(limitsTN |> filter(limitation == "Co-nutrient limitation")) # 467




## Plot the limited lakes
ggplot(limitsTN) +
  geom_point(aes(log(PTL_PPB, base = 10), log(NTL_PPM, base = 10), fill = limitation), size = 2.5, shape = 21, alpha = 0.8) +
  theme_minimal() +
  scale_fill_manual("",values = palette_OkabeIto[5:7]) +
  labs(y = "Log TN"~(m*g~L^-1), x = "Log TP"~(mu*g~L^-1))
ggsave("Figures/TN_analyses/limitedlakes_TN.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 




#### 2. Nutrient correlations with eutrophication ####
# use logged parameters
# create wide dataframe with nutrient as a column
limits_wide_TN <- limitsTN |>
  mutate(NTL_PPB = NTL_PPM / 1000) |>
  pivot_longer(cols = c(PTL_PPB, NTL_PPB), names_to = "nutrient", values_to = "concentration") 

limits_wide_TN$ECO_REG_NAME = factor(limits_wide_TN$ECO_REG_NAME,
                                  levels = c("Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))


ggplot(limits_wide_TN, aes(log10(concentration), log10(CHLA_PPB))) +
  geom_point(alpha = 0.25, aes(color = nutrient)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = nutrient), color = "grey60") +
  geom_abline(slope = 0, intercept = log10(2)) +
  geom_abline(slope = 0, intercept = log10(7)) +
  geom_abline(slope = 0, intercept = log10(30)) +
  geom_vline(xintercept = 0) +
  facet_wrap(~nutrient, scales = "free_x") +
  theme_bw() +
  facet_wrap(~ECO_REG_NAME, ncol = 3) +
  labs(y = "log10(chlorophyll-a concentration)",
       x = "log10(nutrient concentration)") +
  scale_color_manual("", labels = c("TN", "TP"), values=c("#084c61", "#ffc857"))
ggsave("Figures/TN_analyses/ecoregion_linearmodels_TN.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 


m.0 <- aov(log10(CHLA_PPB)~log10(concentration) * nutrient, limits_wide_TN |> filter(is.finite(log10(CHLA_PPB))))
summary(m.0) # all vars significant
tidy(m.0)
glance(m.0) #  r = 0.527, AIC = 6241

m.0region <- aov(log10(CHLA_PPB)~log10(concentration) * nutrient * ECO_REG_NAME, limits_wide_TN |> filter(is.finite(log10(CHLA_PPB))))
summary(m.0region) # all vars signficant excent conc:nutrient:ecoreg
anova(m.0, m.0region) # adding Ecoregion is a significantly better model
glance(m.0region) # r = 0.618, AIC = 5236
performance::r2(m.0region) # adj r = 0.615

m.1region <- lmer(log10(CHLA_PPB)~log10(concentration) * nutrient * factor(year) + (1|ECO_REG_NAME), limits_wide_TN |> filter(is.finite(log10(CHLA_PPB))))
summary(m.1region) 
anova(m.1region) 
anova(m.1region, m.0region) #m.0region has lower AIC and is significantly better
performance::r2(m.1region) #Conditional R2: 0.601, Marginal R2: 0.506


##Table of values from linear models 
# first get info from national model
m.P <- lm(log10(CHLA_PPB)~log10(concentration), limits_wide_TN |> filter(is.finite(log10(CHLA_PPB)), nutrient == "PTL_PPB"))
m.N <- lm(log10(CHLA_PPB)~log10(concentration), limits_wide_TN |> filter(is.finite(log10(CHLA_PPB)), nutrient == "NTL_PPB"))


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
`Model Predictor` <- c("TP", "TN")
Slope <- c(slopeP, slopeN)
Intercept <- c(intP, intN)
`r-squared` <- c(rP, rN)
`p-value` <- c(pP, pN)
AIC <- c(aicP, aicN)

# by ecoregion
list <- as.vector(all_NLA |> select(ECO_REG_NAME) |> distinct())[["ECO_REG_NAME"]]

#add national information
lm_ecoreg_df <- data.frame()
lm_ecoreg_df <- cbind(data.frame(Ecoregion), data.frame(`Model Predictor`), data.frame(Slope),  data.frame(AIC), data.frame(`r-squared`), data.frame(`p-value`))

# add ecoregional information
for(name in list) {
  m.P <- lm(log10(CHLA_PPB)~log10(concentration), limits_wide_TN |> filter(is.finite(log10(CHLA_PPB)), ECO_REG_NAME == name, nutrient == "PTL_PPB"))
  m.N <- lm(log10(CHLA_PPB)~log10(concentration), limits_wide_TN |> filter(is.finite(log10(CHLA_PPB)), ECO_REG_NAME == name, nutrient == "NTL_PPB"))
  
  
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
  
  
  Ecoregion <- c(name, name)
  `Model Predictor` <- c("TP", "TN")
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


#create a pretty table
gt_tbl <- gt(lm_ecoreg_df1)
simpleregtable <- gt_tbl %>%
  cols_label(
    Ecoregion = "Ecoregion",
    Model.Predictor = "Model Predictor",
    Slope = "Slope",
    AIC = "AIC",
    r.squared = "r-squared"
  ) %>%
  cols_align(
    align = "left"
  ) %>%
  tab_header(
    title = "Chlorophyll concentration (proxy for trophic status) vs nutrient concentration relationships",
    subtitle = "Log10(chlorophyll-a) - Log(nutrient) used in linear models"
  ); simpleregtable
gtsave(simpleregtable, "Figures/TN_analyses/linreg_TN.png") 

# NOTE: I also ran these for just 2007 and just 2017 data. 

# create a map of the better predictor in each ecoregion
library(sf)
#read in the ecoregion shapefiles
regions.sf <- read_sf("Data/aggr_ecoregions_2015/Aggr_Ecoregions_2015.shp") |>
  mutate(WSA9_NAME = ifelse(WSA9_NAME == "Temporate Plains", "Temperate Plains", WSA9_NAME))

#determine better predictor in each
compare_r_values <- lm_ecoreg_df |>
  select(-Slope, - p.value) |>
  pivot_wider(names_from = "Model.Predictor", values_from = c("AIC", "r.squared")) |>
  mutate(best_predictor = ifelse(
    AIC_TP < AIC_TN & r.squared_TP > r.squared_TN, "TP", "TN"
  )) |>
  rename(WSA9_NAME = Ecoregion)

# combine shapefiles with better predictor information
regions.sf1 <- left_join(regions.sf, compare_r_values)


# create the map
ggplot(data = regions.sf1) +
  geom_sf(aes(fill = best_predictor)) +
  theme_minimal() +
  geom_sf_label(aes(label = WSA9_NAME)) +
  scale_fill_manual("Better predictor of trophic state", values=c("#084c61", "#ffc857")) 
detach(package:sf, unload=TRUE)


#### 3. Spatial, temporal distribution of limitations ####
# Percent lakes in each ecoregion 
# use categorical analysis from spsurvey package
# prep the data
limits_survey_prep_TN <- limitsTN|> # total 2033 lakes for this analysis 
  filter(VISIT_NO == 1) |># for this analysis, we are using just the first visit from each lake
  filter(WGT_NLA > 0) # the sp survey package is not designed to use the reference lakes, so those are ignored when using this package for analyses.

# lakes with limitations considered for this analysis now 
nrow(limits_survey_prep_TN |> filter(limitation == "P-limitation")) # 679 
nrow(limits_survey_prep_TN |> filter(limitation == "N-limitation")) # 1020
nrow(limits_survey_prep_TN |> filter(limitation == "Co-nutrient limitation")) # 334

years <- c("2007", "2017")
percent_lim <- data.frame()

for(i in 1:length(years)) {
  data <- limits_survey_prep_TN |>
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
  limits_survey_prep_TN,
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

ggplot(percent_lim1 |>
         filter(Subpopulation != "National"), aes(year, Estimate.P, fill = Category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Subpopulation) +
  scale_fill_manual("", values = palette_OkabeIto[5:7]) +
  theme_bw() +
  labs(x = "", y = "% lakes")
ggsave("Figures/TN_analyses/limbars_ecoreg.png", height = 4.5, width = 6.5, units = "in", dpi = 500)

ggplot(percent_lim1 |>
         filter(Subpopulation == "National"), aes(year, Estimate.P, fill = Category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Subpopulation) +
  scale_fill_manual("", values = palette_OkabeIto[5:7]) +
  theme_bw() +
  labs(x = "", y = "% lakes")
ggsave("Figures/TN_analyses/limbars_national.png", height = 4.5, width = 6.5, units = "in", dpi = 500)




#### 4. Limitations change analysis ####
# prep the data
# find only sites that are sampled in both 2007 and 2017
crossover <- limits_survey_prep_TN |>
  select(year, UNIQUE_ID) |>
  filter(year == "2007") |>
  rename(year1 = year) |>
  distinct() |>
  inner_join(limits_survey_prep_TN |>
               select(year, UNIQUE_ID) |>
               filter(year == "2017") |>
               rename(year2 = year) |>
               distinct()) |>
  select(UNIQUE_ID) # 239 sites

limits_change_prep_TN <- limits_survey_prep_TN |> # analysis has same restrictions as categorical analysis
  inner_join(crossover) # this keeps only resampled sites

# use change_analysis from the spsurvey package
# ecoregional change analysis
change_ecoreg <- change_analysis(limits_change_prep_TN, subpops = "ECO_REG_NAME", siteID = "UNIQUE_ID", vars_cat = "limitation", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
change_ecoreg.1 <- change_ecoreg[["catsum"]]  |>
  select(Subpopulation, Category, Indicator, DiffEst.P, StdError.P) 
warnprnt()
# national change analysis
change_nat <- change_analysis(limits_change_prep_TN, siteID = "UNIQUE_ID", vars_cat = "limitation", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
change_nat.1 <- change_nat[["catsum"]]  |>
  select(Subpopulation, Category, Indicator, DiffEst.P, StdError.P)  |>
  mutate(Subpopulation = "National")
warnprnt()


lim_change0717 <- rbind(change_ecoreg.1, change_nat.1)|>
  mutate(year.shift = "2007-2017")
lim_change0717$Subpopulation = factor(lim_change0717$Subpopulation,
                                      levels = c("National","Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))



ggplot(lim_change0717 |>
         filter(Subpopulation != "National")) +
  geom_point(aes(Category,DiffEst.P, fill = Category), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Category, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = Category), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_bw() +
  #facet_grid(~Trophic.State, scales = "free_x") +
  facet_wrap(~Subpopulation, ncol = 3, scales = "free_y") +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in lakes") +
  scale_color_manual("",values = palette_OkabeIto[5:7])  +
  scale_fill_manual("",values = palette_OkabeIto[5:7]) 
ggsave("Figures/TN_analyses/ecoregion_limchanges_TN_repeatlakes.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 


ggplot(lim_change0717 |>
         filter(Subpopulation == "National")) +
  geom_point(aes(Category,DiffEst.P, fill = Category), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Category, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = Category), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_bw() +
  #facet_grid(~Trophic.State, scales = "free_x") +
  #facet_wrap(~Subpopulation, ncol = 3, scales = "free_y") +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in lakes") +
  scale_color_manual("",values = palette_OkabeIto[5:7])  +
  scale_fill_manual("",values = palette_OkabeIto[5:7]) 
ggsave("Figures/TN_analyses/national_limchanges_TN_repeatlakes.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

## TRY this way too to compare
# skip the resampled step to compare
# crossover <- limits_survey_prep_TN |>
#   select(year, UNIQUE_ID) |>
#   filter(year == "2007") |>
#   rename(year1 = year) |>
#   distinct() |>
#   inner_join(limits_survey_prep_TN |>
#                select(year, UNIQUE_ID) |>
#                filter(year == "2017") |>
#                rename(year2 = year) |>
#                distinct()) |>
#   select(UNIQUE_ID)

limits_change_prep_TN <- limits_survey_prep_TN #|> # analysis has same restrictions as categorical analysis
#inner_join(crossover) # this keeps only resampled sites

# use change_analysis from the spsurvey package
# ecoregional change analysis
change_ecoreg <- change_analysis(limits_change_prep_TN, subpops = "ECO_REG_NAME", siteID = "UNIQUE_ID", vars_cat = "limitation", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
change_ecoreg.1 <- change_ecoreg[["catsum"]]  |>
  select(Subpopulation, Category, Indicator, DiffEst.P, StdError.P) 
warnprnt()
# national change analysis
change_nat <- change_analysis(limits_change_prep_TN, siteID = "UNIQUE_ID", vars_cat = "limitation", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
change_nat.1 <- change_nat[["catsum"]]  |>
  select(Subpopulation, Category, Indicator, DiffEst.P, StdError.P)  |>
  mutate(Subpopulation = "National")
warnprnt()


lim_change0717_allSITES <- rbind(change_ecoreg.1, change_nat.1)|>
  mutate(year.shift = "2007-2017")

lim_change0717_allSITES$Subpopulation = factor(lim_change0717_allSITES$Subpopulation,
                                               levels = c("National","Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))



ggplot(lim_change0717_allSITES |>
         filter(Subpopulation != "National")) +
  geom_point(aes(Category,DiffEst.P, fill = Category), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Category, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = Category), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_bw() +
  #facet_grid(~Trophic.State, scales = "free_x") +
  facet_wrap(~Subpopulation, ncol = 3, scales = "free_y") +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in lakes") +
  scale_color_manual("",values = palette_OkabeIto[5:7])  +
  scale_fill_manual("",values = palette_OkabeIto[5:7]) 
ggsave("Figures/TN_analyses/ecoregion_limchanges_TN_allSITES.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

ggplot(lim_change0717_allSITES |>
         filter(Subpopulation == "National")) +
  geom_point(aes(Category,DiffEst.P, fill = Category), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Category, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = Category), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_bw() +
  #facet_grid(~Trophic.State, scales = "free_x") +
  #facet_wrap(~Subpopulation, ncol = 3, scales = "free_y") +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in lakes") +
  scale_color_manual("",values = palette_OkabeIto[5:7])  +
  scale_fill_manual("",values = palette_OkabeIto[5:7]) 
ggsave("Figures/TN_analyses/national_limchanges_TN_allSITES.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 
