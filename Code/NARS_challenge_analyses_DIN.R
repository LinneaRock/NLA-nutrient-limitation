
library(tidyverse)
library(colorblindr)
library(broom)
library(lme4)
library(gt)

source("Data/NLA/Call_NLA_data.R")
nla_data_subset <- all_NLA |>
  select(ECO_REG_NAME, UNIQUE_ID, DATE_COL, VISIT_NO, NTL_PPM, PTL_PPB, DIN_PPM, tn.tp, DIN.TP, CHLA_PPB, TROPHIC_STATE, year,WGT_NLA, LON_DD, LAT_DD, AREA_HA, ELEV_PT, PCT_DEVELOPED_BSN, PCT_AGRIC_BSN, SITE_TYPE, URBAN, LAKE_ORIGIN) |>
  rename(DIN.TP_molar = DIN.TP,
         TN.TP_molar = tn.tp) |>
  filter(year != "2012")

  
  
#### Calculate limitations ####
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


# uses 25th percentile nutrient thresholds for each ecoregion and logged average DIN:TP for each ecoregion 
limitsDIN <- nla_data_subset|>
  filter(is.finite(log(DIN.TP_molar))) |>
  filter(!is.na(DIN_PPM)) |> # 2445 lakes
  left_join(averages_np) |>
  mutate(limitation = NA) |>
  mutate(limitation = ifelse(PTL_PPB > percentile25TP_PPB & log(DIN.TP_molar) < meanlogDINP, "N-limitation", 
                             ifelse(NTL_PPM > percentile25TN_PPM & log(DIN.TP_molar) > meanlogDINP, "P-limitation",
                                    ifelse(is.na(limitation), "Co-nutrient limitation", limitation))))

nrow(limitsDIN |> filter(limitation == "P-limitation")) # 698 
nrow(limitsDIN |> filter(limitation == "N-limitation")) # 1241
nrow(limitsDIN |> filter(limitation == "Co-nutrient limitation")) # 506


## Plot the limited lakes
ggplot(limitsDIN) +
  geom_point(aes(log(PTL_PPB, base = 10), log(NTL_PPM, base = 10), fill = limitation), size = 2.5, shape = 21, alpha = 0.8) +
  theme_minimal() +
  scale_fill_manual("",values = palette_OkabeIto[5:7]) +
  labs(y = "Log TN"~(m*g~L^-1), x = "Log TP"~(mu*g~L^-1))
ggsave("Figures/DIN_analyses/limitedlakes_DIN.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 







#### Nutrient correlations with eutrophication #### 
## NOTE: DIN is a terrible predictor it doesn't even seem worth it to compare these. 
# use logged parameters
# create wide dataframe with nutrient as a column
# limits_wide_DIN <- limitsDIN |>
#   mutate(DIN_PPB = DIN_PPM / 1000) |>
#   pivot_longer(cols = c(PTL_PPB, DIN_PPB), names_to = "nutrient", values_to = "concentration") 
# 
# limits_wide_DIN$ECO_REG_NAME = factor(limits_wide_DIN$ECO_REG_NAME,
#                                   levels = c("Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))
# 
# ggplot(limits_wide_DIN, aes(log10(concentration), log10(CHLA_PPB))) +
#   geom_point(alpha = 0.25, aes(color = nutrient)) +
#   geom_smooth(method = "lm", se = FALSE, aes(group = nutrient), color = "grey60") +
#   geom_abline(slope = 0, intercept = log10(2)) +
#   geom_abline(slope = 0, intercept = log10(7)) +
#   geom_abline(slope = 0, intercept = log10(30)) +
#   geom_vline(xintercept = 0) +
#   facet_wrap(~nutrient, scales = "free_x") +
#   theme_bw() +
#   facet_wrap(~ECO_REG_NAME, ncol = 3) +
#   labs(y = "log10(chlorophyll-a concentration)",
#        x = "log10(nutrient concentration)") +
#   scale_color_manual("", labels = c("DIN", "TP"), values=c("#084c61", "#ffc857"))
# ggsave("Figures/DIN_analyses/ecoregion_linearmodels_DIN.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 
# 
# # run some basic models
# m.0 <- aov(log10(CHLA_PPB)~log10(concentration) * nutrient, limits_wide_DIN |> filter(is.finite(log10(CHLA_PPB))))
# summary(m.0) # all vars significant
# tidy(m.0)
# glance(m.0) # r =0.294, AIC = 8028
# 
# m.0region <- aov(log10(CHLA_PPB)~log10(concentration) * nutrient * ECO_REG_NAME, limits_wide_DIN |> filter(is.finite(log10(CHLA_PPB))))
# summary(m.0region) # all vars significant 
# anova(m.0, m.0region) # adding Ecoregion is a significantly better model
# glance(m.0region) # r = 0.466, AIC = 6730
# 
# m.1region <- lmer(log10(CHLA_PPB)~log10(concentration) * nutrient * factor(year) + (1|ECO_REG_NAME), limits_wide_DIN |> filter(is.finite(log10(CHLA_PPB))))
# summary(m.1region) 
# anova(m.1region) 
# anova(m.1region, m.0region) #m.0region has lower AIC and is significantly better model
# performance::r2(m.1region) #Conditional R2: 0.376, Marginal R2: 0.222
# 
# ### compare AIC, R-squared values from models 
# # first get info from national model
# m.P <- lm(log10(CHLA_PPB)~log10(concentration), limits_wide_DIN |> filter(is.finite(log10(CHLA_PPB)), nutrient == "PTL_PPB"))
# m.N <- lm(log10(CHLA_PPB)~log10(concentration), limits_wide_DIN |> filter(is.finite(log10(CHLA_PPB)), nutrient == "DIN_PPB"))
# 
# 
# slopeP <- coef(m.P)[2] # slope of linear model of Chla vs. P
# slopeN <- coef(m.N)[2] # slope of linear model of Chla vs. N
# intP <- coef(m.P)[1] # intercept of linear model of Chla vs. P
# inDIN <- coef(m.N)[1] # intercept of linear model of Chla vs. N
# rP <- as.numeric(glance(m.P)[2]) # adj. r-squraed of linear model of Chla vs. P
# rN <- as.numeric(glance(m.N)[2]) # adj. r-squraed of linear model of Chla vs. N
# pP <- as.numeric(glance(m.P)[5]) # p-value of linear model of Chla vs. P
# pN <- as.numeric(glance(m.N)[5]) # p-value of linear model of Chla vs. N
# aicP <- AIC(m.P)
# aicN <- AIC(m.N)
# 
# 
# Ecoregion <- c("National", "National")
# `Model Predictor` <- c("TP", "DIN")
# Slope <- c(slopeP, slopeN)
# Intercept <- c(intP, intN)
# `r-squared` <- c(rP, rN)
# `p-value` <- c(pP, pN)
# AIC <- c(aicP, aicN)
# 
# # by ecoregion
# list <- as.vector(all_NLA |> select(ECO_REG_NAME) |> distinct())[["ECO_REG_NAME"]]
# 
# #add national information
# lm_ecoreg_df <- data.frame()
# lm_ecoreg_df <- cbind(data.frame(Ecoregion), data.frame(`Model Predictor`), data.frame(Slope),  data.frame(AIC), data.frame(`r-squared`), data.frame(`p-value`))
# 
# # add ecoregional information
# for(name in list) {
#   m.P <- lm(log10(CHLA_PPB)~log10(concentration), limits_wide_DIN |> filter(is.finite(log10(CHLA_PPB)), ECO_REG_NAME == name, nutrient == "PTL_PPB"))
#   m.N <- lm(log10(CHLA_PPB)~log10(concentration), limits_wide_DIN |> filter(is.finite(log10(CHLA_PPB)), ECO_REG_NAME == name, nutrient == "DIN_PPB"))
#   
#   
#   slopeP <- coef(m.P)[2] # slope of linear model of Chla vs. P
#   slopeN <- coef(m.N)[2] # slope of linear model of Chla vs. N
#   intP <- coef(m.P)[1] # intercept of linear model of Chla vs. P
#   intN <- coef(m.N)[1] # intercept of linear model of Chla vs. N
#   rP <- as.numeric(glance(m.P)[2]) # adj. r-squraed of linear model of Chla vs. P
#   rN <- as.numeric(glance(m.N)[2]) # adj. r-squraed of linear model of Chla vs. N
#   pP <- as.numeric(glance(m.P)[5]) # p-value of linear model of Chla vs. P
#   pN <- as.numeric(glance(m.N)[5]) # p-value of linear model of Chla vs. N
#   aicP <- AIC(m.P)
#   aicN <- AIC(m.N)
#   
#   
#   Ecoregion <- c(name, name)
#   `Model Predictor` <- c("TP", "DIN")
#   Slope <- c(slopeP, slopeN)
#   Intercept <- c(intP, intN)
#   `r-squared` <- c(rP, rN)
#   `p-value` <- c(pP, pN)
#   AIC <- c(aicP, aicN)
#   
#   tmp <- cbind(data.frame(Ecoregion), data.frame(`Model Predictor`), data.frame(Slope), data.frame(AIC), data.frame(`r-squared`), data.frame(`p-value`))
#   
#   lm_ecoreg_df <- bind_rows(lm_ecoreg_df, tmp) |>
#     distinct()
#   
# }
# 
# lm_ecoreg_df1 <- lm_ecoreg_df |>
#   select(-p.value)
# 
# 
# compare_r_values <- lm_ecoreg_df |>
#   pivot_wider(names_from = "Model.Predictor", values_from = "r.squared")
# 
# lm_ecoreg_df1$Ecoregion = factor(lm_ecoreg_df1$Ecoregion,
#                                  levels = c("National","Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))
# 
# #create a pretty table
# gt_tbl <- gt(lm_ecoreg_df1)
# simpleregtable <- gt_tbl %>%
#   cols_label(
#     Ecoregion = "Ecoregion",
#     Model.Predictor = "Model Predictor",
#     Slope = "Slope",
#     AIC = "AIC",
#     r.squared = "r-squared"
#   ) %>%
#   cols_align(
#     align = "left"
#   ) %>%
#   tab_header(
#     title = "Chlorophyll concentration (proxy for trophic status) vs nutrient concentration relationships",
#     subtitle = "Log10(chlorophyll-a) - Log(nutrient) used in linear models"
#   ); simpleregtable
# gtsave(simpleregtable, "Figures/DIN_analyses/linregDIN_BAD.html") 

 ## 


#### Spatial distribution of limited lakes ####



#### Spatial, temporal distribution of limitations ####
# Percent lakes in each ecoregion 
# use categorical analysis from spsurvey package
# prep the data
limits_change_prep_DIN <- limitsDIN|> # total 2033 lakes for this analysis 
  filter(VISIT_NO == 1) |># for this analysis, we are using just the first visit from each lake
  filter(WGT_NLA > 0) # the sp survey package is not designed to use the reference lakes, so those are ignored when using this package for analyses.

# lakes with limitations considered for this analysis now 
nrow(limits_change_prep_DIN |> filter(limitation == "P-limitation")) # 580
nrow(limits_change_prep_DIN |> filter(limitation == "N-limitation")) # 1017
nrow(limits_change_prep_DIN |> filter(limitation == "Co-nutrient limitation")) # 365

years <- c("2007", "2017")
percent_lim <- data.frame()

for(i in 1:length(years)) {
  data <- limits_change_prep_DIN |>
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
  limits_change_prep_DIN,
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
ggsave("Figures/DIN_analyses/limbars_ecoreg_DIN.png", height = 4.5, width = 6.5, units = "in", dpi = 500)

ggplot(percent_lim1 |>
         filter(Subpopulation == "National"), aes(year, Estimate.P, fill = Category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Subpopulation) +
  scale_fill_manual("", values = palette_OkabeIto[5:7]) +
  theme_bw() +
  labs(x = "", y = "% lakes")
ggsave("Figures/DIN_analyses/limbars_national_DIN.png", height = 4.5, width = 6.5, units = "in", dpi = 500)
