
##### Script to run analyses for EPA's National Aquatic Resource Surveys Data Challenge #####
## Linnea Rock, 2022


#### Load necessary libraries ####
library(tidyverse)
library(colorblindr)
library(spsurvey)
library(broom)
library(lme4)
#library(gt)
library(patchwork)
library(ggpubr)


#### Load and subset dataframe ####
source("Data/NLA/Call_NLA_data.R")
nla_data_subset <- all_NLA |>
  select(ECO_REG_NAME, UNIQUE_ID, DATE_COL, VISIT_NO, NTL_PPM, PTL_PPB, DIN_PPM, tn.tp, DIN.TP, CHLA_PPB, TROPHIC_STATE, year,WGT_NLA, LON_DD, LAT_DD, AREA_HA, ELEV_PT, PCT_DEVELOPED_BSN, PCT_AGRIC_BSN, SITE_TYPE, URBAN, LAKE_ORIGIN) |>
  rename(DIN.TP_molar = DIN.TP,
         TN.TP_molar = tn.tp) |>
  filter(year != "2012") |>
  filter(AREA_HA >= 4) |> # removes 216 observations 
  distinct()

tinylakesobs <- all_NLA |>
  distinct() |>
  filter(year != "2012",
         AREA_HA < 4) |>
  distinct() # 216 observations

tinylakes_lake <- tinylakesobs |>
  select(UNIQUE_ID, SITE_TYPE) |>
  distinct() # 211 lakes, 8 of these were reference lakes 





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








#### 2. Nutrient correlations with eutrophication ####
# use logged parameters
# create wide dataframe with nutrient as a column
correlations_data <- nla_data_subset |>
 # filter(year == "2007") |> # same as total except TN better predictor in coastal plains
 # filter(year == "2017") |> # same as total except TP better predictor in xeric and w. mtn
  mutate(NTL_PPB = NTL_PPM / 1000) |>
  pivot_longer(cols = c(PTL_PPB, NTL_PPB), names_to = "nutrient", values_to = "concentration") 

correlations_data$ECO_REG_NAME = factor(correlations_data $ECO_REG_NAME,
                                     levels = c("Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))

m.0 <- aov(log10(CHLA_PPB)~log10(concentration) * nutrient, correlations_data  |> filter(is.finite(log10(CHLA_PPB))))
summary(m.0) # all vars significant
tidy(m.0)
glance(m.0) #  r = 0.527, AIC = 6241

m.0region <- aov(log10(CHLA_PPB)~log10(concentration) * nutrient * ECO_REG_NAME, correlations_data  |> filter(is.finite(log10(CHLA_PPB))))
summary(m.0region) # all vars signficant excent conc:nutrient:ecoreg
anova(m.0, m.0region) # adding Ecoregion is a significantly better model
glance(m.0region) # r = 0.618, AIC = 5236
performance::r2(m.0region) # adj r = 0.615

m.1region <- lmer(log10(CHLA_PPB)~log10(concentration) * nutrient * factor(year) + (1|ECO_REG_NAME), correlations_data  |> filter(is.finite(log10(CHLA_PPB))))
summary(m.1region) 
anova(m.1region) 
anova(m.1region, m.0region) #m.0region has lower AIC and is significantly better
performance::r2(m.1region) #Conditional R2: 0.601, Marginal R2: 0.506


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
  aicP <- AIC(m.P)
  aicN <- AIC(m.N)
  
  
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


#create a pretty table
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
#gtsave(simpleregtable, "Figures/TN_analyses/linreg_TN.png") 

add_corr <- lm_ecoreg_df1 |>
  rename(nutrient = Model.Predictor,
         ECO_REG_NAME = Ecoregion) |>
  filter(ECO_REG_NAME != "National") |>
  mutate(AIC = paste0("AIC=",round(AIC, digits = 1)),
         r.squared = paste0("adj.r=", round(r.squared, digits=2)))


ggplot(correlations_data, aes(log10(concentration), log10(CHLA_PPB))) +
  geom_point(alpha = 0.25, aes(color = nutrient)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = nutrient), color = "grey60") +
  geom_text(add_corr |> filter(nutrient == "NTL_PPB"), mapping = aes(x = -5, y = 3.5, label = r.squared), size = 2, vjust = 0.75, hjust = 0) +
  geom_text(add_corr |> filter(nutrient == "NTL_PPB"), mapping = aes(x = -5, y = 3, label = AIC), size = 2, vjust = 0.75, hjust = 0) +
  geom_text(add_corr |> filter(nutrient == "PTL_PPB"), mapping = aes(x = 1, y = 3.5, label = r.squared), size = 2, vjust = 0.75, hjust = 0) +
  geom_text(add_corr |> filter(nutrient == "PTL_PPB"), mapping = aes(x = 1, y = 3, label = AIC), size = 2, vjust = 0.75, hjust = 0) +
  geom_abline(slope = 0, intercept = log10(2)) +
  geom_abline(slope = 0, intercept = log10(7)) +
  geom_abline(slope = 0, intercept = log10(30)) +
  geom_vline(xintercept = 0) +
  facet_wrap(~nutrient, scales = "free_x") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  facet_wrap(~ECO_REG_NAME, ncol = 3) +
  labs(y = "log10(chlorophyll-a concentration)",
       x = "log10(nutrient concentration)") +
#        caption = "Figure 1. Chlorophyll-a vs. nutrient concentration (log-log) in each ecoregion. Color indicates either total nitrogen or 
# total phosphorus. AIC and adjusted r-squared (adj.r) are displayed on each panel. Horizontal lines indicate trophic state
# from oligotrophic (below the lowest line to hypereutrophic (above the highest line).") +
  scale_color_manual("", labels = c("TN", "TP"), values=c("red4", "#336a98")) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"))
#ggsave("Figures/F1_linregs.png", height = 4.5, width = 6.4, units = "in", dpi = 1200) 
ggsave("Figures/F1_linregs_pub.png", height = 4.5, width = 6.4, units = "in", dpi = 1200) 

# NOTE: I also ran these for just 2007 and just 2017 data. 

# create a map of the better predictor in each ecoregion
#read in the ecoregion shapefiles
regions.sf <- read_sf("Data/aggr_ecoregions_2015/Aggr_Ecoregions_2015.shp") |>
  mutate(WSA9_NAME = ifelse(WSA9_NAME == "Temporate Plains", "Temperate Plains", WSA9_NAME))

#determine better predictor in each
compare_r_values <- lm_ecoreg_df |>
  select(-Slope, - p.value) |>
  pivot_wider(names_from = "Model.Predictor", values_from = c("AIC", "r.squared")) |>
  mutate(best_predictor = ifelse(
    AIC_PTL_PPB < AIC_NTL_PPB & r.squared_PTL_PPB > r.squared_NTL_PPB, "TP", "TN"
  )) |>
  rename(WSA9_NAME = Ecoregion)

# combine shapefiles with better predictor information
regions.sf1 <- left_join(regions.sf, compare_r_values)


## does N:TP correlate with chlorophyll-a? 
# ggplot(limits) +
#   geom_point(aes(log10(TN.TP_molar), log10(CHLA_PPB))) +
#   geom_smooth(method = "lm", se = FALSE, aes(log10(TN.TP_molar), log10(CHLA_PPB))) +
#   facet_wrap(~limitation)
# 
# ggplot(limits) +
#   geom_point(aes(log10(DIN.TP_molar), log10(CHLA_PPB))) +
#   geom_smooth(method = "lm", se = FALSE, aes(log10(DIN.TP_molar), log10(CHLA_PPB))) +
#   facet_wrap(~limitation)

# create the map
ggplot(data = regions.sf1) +
  geom_sf(aes(fill = best_predictor)) +
  theme_bw() +
  geom_sf_label(aes(label = WSA9_NAME), size = 2.5) +
  labs(x = "", y = "") +
#        caption = "Figure 2. Map displaying the best correlation variable of the chlorophyll-a (trophic state proxy) vs nutrient linear
# models in each (labelled) ecoregion.") +
  scale_fill_manual("", values=c("red4", "#336a98")) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif")) +
  theme(legend.position = c(0.1, 0.2)) 
#ggsave("Figures/F2_Map.png", height = 4.5, width = 6.5, units = "in", dpi = 1200) 
ggsave("Figures/F2_Map_pub.png", height = 4.5, width = 6.5, units = "in", dpi = 1200) 

#detach(package:sf, unload=TRUE)


# ## Create a beautiful plot -- too big to save well
# (map / lm_plots) +
#   plot_annotation(tag_levels = 'a', tag_suffix = ')',
#                   caption = "Figure 1. a) Map displaying the better nutrient predictor of trophic state in each (labelled) ecoregion.
# b) Chlorophyll-a vs. nutrient concentration (log-log) in each ecoregion. Color indicates either total nitrogen or total 
# phosphorus. AIC and adjusted r-squared are displayed on each panel. Horizontal lines indicate trophic state from 
# oligotrophic (below the lowest line to hypereutrophic (above the highest line).") &
#   theme(plot.tag = element_text(size = 8),
#         plot.caption.position = "plot",
#         plot.caption = element_text(hjust = 0))
# ggsave("Figures/F1_Map_linregs.png", height = 8.5, width = 8, units = "in", dpi = 500) 



#### 3. Calculate limitations ####
# get information about the refernece lakes
ref_np <- nla_data_subset |>
  filter(SITE_TYPE %in% c("REF_Lake", "HAND")) |> # subset of 230 lakes
  group_by(ECO_REG_NAME, year) |>
  # 75th percentile of nutrient concentrations from reference lakes
  summarise(percentile75TN_PPM = quantile(NTL_PPM, probs = 0.75), 
            percentile75TP_PPB = quantile(PTL_PPB, probs = 0.75),
            percentile75DIN_PPM = quantile(DIN_PPM, probs = 0.75)) |>
  ungroup()

# get some information about the entire dataset
averages_np <- nla_data_subset |>
  filter(is.finite(log(DIN.TP_molar))) |>
  group_by(ECO_REG_NAME, year) |>
  # 25th percentile of nutrient concentrations from total assessed lakes and mean ratios
  summarise(meanlogNP = mean(log(TN.TP_molar)),
            meanlogDINP = mean(log(DIN.TP_molar), na.rm = TRUE),
            medianlogDINP = median(log(DIN.TP_molar), na.rm = TRUE),
            meanNP = mean((TN.TP_molar)),
            meanDINP = mean((DIN.TP_molar), na.rm = TRUE),
            medianDINP = median(DIN.TP_molar, na.rm = TRUE),
            percentile25TN_PPM = quantile(NTL_PPM, probs = 0.25),
            percentile25TP_PPB = quantile(PTL_PPB, probs = 0.25),
            percentile25DIN_PPM = quantile(DIN_PPM, probs = 0.25)) |>
  ungroup() |>
  # select(ECO_REG_NAME, meanlogNP, meanlogDINP, percentile25TN_PPM, percentile25TP_PPB, percentile25DIN_PPM) |>
  distinct()

criteria <- left_join(averages_np, ref_np) |>
  group_by(ECO_REG_NAME, year) |>
  mutate(TP_threshold = median(c(percentile75TP_PPB, percentile25TP_PPB)),
         TN_threshold = median(c(percentile75TN_PPM, percentile25TN_PPM)),
         DIN_threshold = median(c(percentile75DIN_PPM, percentile25DIN_PPM))) |>
  # There were no reference lakes in the Northern Plains in 2007. So, concentration thresholds were determined solely by the 25th percentile of all assessed lakes in that region in that year. 
  mutate(TP_threshold = ifelse(is.na(TP_threshold), percentile25TP_PPB, TP_threshold),
         TN_threshold = ifelse(is.na(TN_threshold), percentile25TN_PPM, TN_threshold),
         DIN_threshold = ifelse(is.na(DIN_threshold), percentile25DIN_PPM, DIN_threshold))

#write.csv(criteria, "criteria.csv")

# How do the lower 25th percentiles of TN and TP compare the the 75th percentile concentrations of TN and TP in the reference lakes? 
t.test(criteria$percentile75TN_PPM, criteria$percentile25TN_PPM) # p = 0.1459
t.test(criteria$percentile75TP_PPB, criteria$percentile25TP_PPB) # p = 0.1587
t.test(criteria$percentile75DIN_PPM, criteria$percentile25DIN_PPM) # p = 0.1718
# reject the null hypotheses for these tests. The true difference in means is equal to 0.

# generate nutrient limitations
limits <- nla_data_subset|>
  filter(is.finite(log(DIN.TP_molar))) |>
  filter(!is.na(DIN_PPM)) |> # 2371 observations, loss of 74 observations from analysis
  left_join(criteria) |>
  mutate(limitation = NA) |>
  mutate(limitation = ifelse(PTL_PPB > TP_threshold & log(DIN.TP_molar) < medianlogDINP, "N-limitation", 
                             ifelse(DIN_PPM > DIN_threshold & log(DIN.TP_molar) > medianlogDINP, "P-limitation",
                                    ifelse(is.na(limitation), "Co-nutrient limitation", limitation))))

nrow(limits |> filter(limitation == "P-limitation")) # 700
nrow(limits |> filter(limitation == "N-limitation")) # 885
nrow(limits |> filter(limitation == "Co-nutrient limitation")) # 594 

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
ggsave("Figures/Forig3_limitedlakes.png", height = 4.5, width = 6.5, units = "in", dpi = 1200) 

# how many of the co-nutrient limited lakes occur in lakes with excess N or P?
co_lim <- limits |>
  filter(limitation == "Co-nutrient limitation") |> # 594 obs
  mutate(highDIN = ifelse(DIN_PPM > DIN_threshold, 1, 0),
         highTN = ifelse(NTL_PPM >TN_threshold, 1, 0),
         highTP = ifelse(PTL_PPB > TP_threshold, 1,0)) |>
  filter(highDIN == 1 |
           highTN == 1 |
           highTP == 1) # 92 obs





#### 3. Spatial, temporal distribution of limitations ####
# Percent lakes in each ecoregion 
# prep the data
limits_survey_prep <- limits|> # total 1773 lakes for this analysis 
  filter(VISIT_NO == 1) |># for this analysis, we are using just the first visit from each lake
  filter(WGT_NLA > 0) # the sp survey package is not designed to use the reference lakes, so those are ignored when using this package for analyses.

# lakes with limitations considered for this analysis now 
nrow(limits_survey_prep |> filter(limitation == "P-limitation")) # 589
nrow(limits_survey_prep |> filter(limitation == "N-limitation")) # 729
nrow(limits_survey_prep |> filter(limitation == "Co-nutrient limitation")) # 455

# use categorical analysis from spsurvey package
years <- c("2007", "2017")
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

text_percents <- percent_lim1 |>
  select(year, Subpopulation, Category, Estimate.P) |>
  filter(Estimate.P > 10) |>
  mutate(perc = paste0(round(Estimate.P, digits = 1), "%")) 



ggplot(percent_lim1, aes(year, Estimate.P, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(text_percents, mapping = aes(label = perc), position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(~Subpopulation) +
  scale_fill_manual("", values = c("grey60","red4", "#336a98")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 6.5)) +
  labs(x = "", y = "% lakes") +
#        caption = "Figure 3. Percent of lakes nationally and in each ecoregion in each nutrient limitation
# category per year. Percents were extrapolated from the indidvudal lakes in the dataset
# to represent lakes across the conterminous U.S. using the weights. Each bar is labelled 
# with the percent of lakes in that category (percents <10% were not labelled).")  +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"),
        legend.position = c(0.75, 0.175))
#ggsave("Figures/F3_limitbars_ecoreg.png", height = 4.5, width = 6.5, units = "in", dpi = 1200)
ggsave("Figures/F3_limitbars_ecoreg_pub.png", height = 4.5, width = 6.5, units = "in", dpi = 1200)



#### 4. Limitations change analysis ####
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

limits_change_prep <- limits_survey_prep |> # analysis has same restrictions as categorical analysis
  inner_join(crossover) # this keeps only resampled sites

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
limits_change_prep <- limits_survey_prep #|> # analysis has same restrictions as categorical analysis
#inner_join(crossover) # this keeps only resampled sites

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
t.test((lim_changes_fullset |> filter(sample_set == "Resampled lakes"))$MarginofError.P, (lim_changes_fullset |> filter(sample_set != "Resampled lakes"))$MarginofError.P) # p = 0.006251
ggplot(lim_changes_fullset, aes(sample_set, MarginofError.P)) +
  geom_boxplot()

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
  mutate(p.value = '*') # no changes were significantly different 

# add significance to dataset
lim_changes_fullset <- left_join(lim_changes_fullset, comparison_lim) |>
  mutate(p.value=ifelse(sample_set!='Resampled lakes', NA, p.value))

ecoreg_plot <- ggplot(lim_changes_fullset |>
         filter(Subpopulation != "National")) +
  geom_point(aes(Category,DiffEst.P, fill = Category), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5))+
  geom_errorbar(aes(Category, DiffEst.P, ymin = DiffEst.P-MarginofError.P, ymax = DiffEst.P+MarginofError.P, color = Category, linetype = sample_set), width = 0.2)  +
  geom_text(lim_changes_fullset|>
               filter(Subpopulation != "National"), mapping=aes(Category, DiffEst.P, label=p.value), nudge_x=-0.25, nudge_y=0.25) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  #facet_grid(~Trophic.State, scales = "free_x") +
  facet_wrap(~Subpopulation, ncol = 3, scales = "free_y") +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% change 2007-2017") +
#        caption = "Figure 4. Percent change in lakes in nutrient limitation status a) nationally, and b) in the nine aggregated 
# ecoregions from 2007-2017. The change is represented as a percent difference in the population (point) with 
# standard error bars. Error bars that cross zero are not statistically significant. The solid lines are the 
# entire population of all surveyed lakes, and the dotted lines are the resampled lakes in both surveys only.") + 
  scale_color_manual("",values = c("grey60","red4", "#336a98"))  +
  scale_fill_manual("",values = c("grey60","red4", "#336a98")) +
  theme(#axis.text.x = element_text(angle = 49, vjust = 1, hjust =1),
        strip.text.x = element_text(size = 7.5),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"),
        legend.title = element_blank(),
        axis.text.x = element_blank())


#national plot
nat_plot <- ggplot(lim_changes_fullset |>
         filter(Subpopulation == "National")) +
  geom_point(aes(Category,DiffEst.P, fill = Category), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Category, DiffEst.P, ymin = DiffEst.P-MarginofError.P, ymax = DiffEst.P+MarginofError.P, color = Category, linetype = sample_set), width = 0.2)  + 
  geom_text(lim_changes_fullset|>
              filter(Subpopulation == "National"), mapping=aes(Category, DiffEst.P, label=p.value), nudge_x=-0.25, nudge_y=0.25) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  #facet_grid(~Trophic.State, scales = "free_x") +
  #facet_wrap(~Subpopulation, ncol = 3, scales = "free_y") +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% change 2007-2017",
       title = "National") +
  scale_color_manual("",values = c("grey60","red4", "#336a98"))  +
  scale_fill_manual("",values = c("grey60","red4", "#336a98")) +
  theme(legend.title = element_blank())

layout <- "
AAAA
BBBB
BBBB
"

nat_plot/ecoreg_plot +
  plot_layout(guides = "collect",
              design = layout) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') 
#ggsave("Figures/F4_limchanges.png", height = 8.5, width = 6.5, units = "in", dpi = 1200) 
ggsave("Figures/F4_limchanges_pub.png", height = 6, width = 6.5, units = "in", dpi = 1200) 






#### 5. Spatial, temporal distribution of trophic state ####
# how many observations of each trophic state
nrow(limits |> filter(TROPHIC_STATE == "Oligo.")) # 348
nrow(limits |> filter(TROPHIC_STATE == "Meso.")) # 697
nrow(limits |> filter(TROPHIC_STATE == "Eutro.")) # 676
nrow(limits |> filter(TROPHIC_STATE == "Hyper.")) # 458
# Percent lakes in each ecoregion 
# use categorical analysis from spsurvey package
years <- c("2007", "2017")
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


text_percents <- percent_TS1 |>
  select(year, Subpopulation, Category, Estimate.P) |>
  filter(Estimate.P > 10) |>
  mutate(perc = paste0(round(Estimate.P, digits = 1), "%"))


ggplot(percent_TS1, aes(year, Estimate.P, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(text_percents, mapping = aes(label = perc), position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(~Subpopulation) +
  scale_fill_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1])) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 6.5)) +
  labs(x = "", y = "% lakes") +
#        caption = "Figure 5. Percent of lakes nationally and in each ecoregion in each trophic state per year. Percents 
# were extrapolated from the indidvudal lakes in the dataset to represent lakes across the conterminous 
# U.S. using the weights. Each bar is labelled with the percent of lakes in that category (percents <10%
# were not labelled).")  +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"),
        legend.position = c(0.75, 0.175)) +
  guides(fill = guide_legend(ncol=2))
#ggsave("Figures/F5_TSbars_ecoreg.png", height = 4.5, width = 6.5, units = "in", dpi = 1200)
ggsave("Figures/F5_TSbars_ecoreg_pub.png", height = 4.5, width = 6.5, units = "in", dpi = 1200)


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
  inner_join(crossover) # this keeps only resampled sites

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
TS_change_prep <- limits_survey_prep #|> # analysis has same restrictions as categorical analysis
#inner_join(crossover) # this keeps only resampled sites
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
t.test((TS_changes_fullset |> filter(sample_set == "Resampled lakes"))$MarginofError.P, (TS_changes_fullset |> filter(sample_set != "Resampled lakes"))$MarginofError.P) # p = 0.0001986
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
  mutate(p.value = '*') # all values were between 0.01 and 0.05

# add significance to dataset
TS_changes_fullset <- left_join(TS_changes_fullset, comparison_ts) |>
  mutate(p.value=ifelse(sample_set!='Resampled lakes', NA, p.value))



#national plot
ggplot(TS_changes_fullset) +
  geom_point(aes(Category,DiffEst.P, fill = Category), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Category, DiffEst.P, ymin = DiffEst.P-MarginofError.P, ymax = DiffEst.P+MarginofError.P, color = Category, linetype = sample_set), width = 0.2)  + 
 geom_text(TS_changes_fullset, mapping=aes(Category, DiffEst.P, label=p.value), nudge_x=-0.25, nudge_y=0.25) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  facet_wrap(~Subpopulation, scales = "free_y", ncol = 2) +
  #facet_wrap(~Subpopulation, ncol = 3, scales = "free_y") +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% change 2007-2017")+                                                 
#        caption = "Figure 6. Percent change in trophic state across lakes nationally from 2007-2017, panels separated to show all lakes 
# at all limitations and separated by limitation category. The change is represented as a percent difference in the 
# population (point) with standard error bars. Change bars that cross zero are not statistically significant. The 
# solid lines are the entire population of all surveyed lakes, and the dotted lines are the resampled lakes in both 
# surveys only. Note there is a difference in y-axis scales.") + 
  scale_color_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]))  +
  scale_fill_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1])) +
  theme(axis.text.x = element_text(angle = 49, vjust = 1, hjust =1),
        strip.text.x = element_text(size = 7.5),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"),
        legend.title = element_blank())
#ggsave("Figures/F6_TSchanges.png", height = 4.5, width = 6.5, units = "in", dpi = 1200) 
ggsave("Figures/F6_TSchanges_pub.png", height = 4.5, width = 6.5, units = "in", dpi = 1200) 


# library(scales)
# show_col(c("grey60","red4", "#336a98"))
# show_col(palette_OkabeIto[5:7])
