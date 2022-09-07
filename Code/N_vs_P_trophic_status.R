
# Script to explore if trophic status is more dependent on N or P, limitation status, etc.

# call datasets and libraries 
source("Data/NLA/Call_NLA_data.R")
library(colorblindr)
library(lme4)

#### Calculate the limitations first ####
# get some information about the entire dataset
averages_np <- all_NLA |>
  group_by(ECO_REG, year) |>
  mutate(yearlymeanlogNP = mean(log(tn.tp))) |>
  ungroup() |>
  group_by(ECO_REG_NAME) |>
  mutate(
    meanlogNP = mean(log(tn.tp)),
    medianlogNP = median(log(tn.tp)),
    medianTN_PPM = median(NTL_PPM),
    medianTP_PPB = median(PTL_PPB),
    percentile25TN_PPM = quantile(NTL_PPM, probs = 0.25),
    percentile25TP_PPB = quantile(PTL_PPB, probs = 0.25)) |>
  ungroup() |>
  select(year, ECO_REG_NAME, meanlogNP, yearlymeanlogNP, percentile25TN_PPM, percentile25TP_PPB) |>
  distinct()

# uses 25th percentile nutrient thresholds for each ecoregion (over all years) and logged average N:P for each ecoregion (over each year)

limits <- all_NLA |>
  left_join(averages_np) |>
  mutate(limitation = NA) |>
  mutate(limitation = ifelse(PTL_PPB > percentile25TP_PPB & log(tn.tp) < yearlymeanlogNP, "Potential N-limitation", 
                             ifelse(NTL_PPM > percentile25TN_PPM & log(tn.tp) > yearlymeanlogNP, "Potential P-limitation",
                                    ifelse(is.na(limitation), "Potential co-nutrient limitation", limitation))))


#### Visual representation of relationships ####
# use logged parameters
# create wide dataframe with nutrient as a column
limits_wide <- limits |>
  mutate(NTL_PPB = NTL_PPM / 1000) |>
  pivot_longer(cols = c(PTL_PPB, NTL_PPB), names_to = "nutrient", values_to = "concentration") 
  
limits_wide$ECO_REG_NAME = factor(limits_wide$ECO_REG_NAME,
                              levels = c("Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))



ggplot(limits_wide, aes(log10(concentration), log10(CHLA_PPB))) +
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
ggsave("Figures/QNvsP.Figs/ecoregion_linearmodels.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 


m.0 <- aov(log10(CHLA_PPB)~log10(concentration) * nutrient, limits_wide |> filter(is.finite(log10(CHLA_PPB))))
summary(m.0) #adj R = 0.5364
anova(m.0)
coef(m.0)
library(broom)
tidy(m.0)
glance(m.0) # r =0.507, AIC = 9356

m.0region <- aov(log10(CHLA_PPB)~log10(concentration) * nutrient * ECO_REG_NAME, limits_wide |> filter(is.finite(log10(CHLA_PPB))))
summary(m.0region) #adj R = 0.594
anova(m.0region) # all variables including the interactions are significant, meaning the slopes and intercepts among groups are different. 
anova(m.0, m.0region) # adding Ecoregion is a significantly better model
#plot(TukeyHSD(m.0region, conf.level = 0.95), las = 2)
glance(m.0region) # r = 0.594, AIC = 7975
performance::r2(m.0region)


library(lme4)
m.1region <- lmer(log10(CHLA_PPB)~log10(concentration) * nutrient * factor(year) + (1|ECO_REG_NAME), limits_wide |> filter(is.finite(log10(CHLA_PPB))))
summary(m.1region) 
anova(m.1region) 
anova(m.1region, m.0region) #m.0region has lower AIC
#plot(TukeyHSD(m.0region, conf.level = 0.95), las = 2)
library(broom)
performance::r2(m.1region) #Conditional R2: 0.570, Marginal R2: 0.481


#### Table of values from linear models ####
# first get info from national model
m.P <- lm(log10(CHLA_PPB)~log10(concentration), limits_wide |> filter(is.finite(log10(CHLA_PPB)), nutrient == "PTL_PPB"))
m.N <- lm(log10(CHLA_PPB)~log10(concentration), limits_wide |> filter(is.finite(log10(CHLA_PPB)), nutrient == "NTL_PPB"))


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
  m.P <- lm(log10(CHLA_PPB)~log10(concentration), limits_wide |> filter(is.finite(log10(CHLA_PPB)), ECO_REG_NAME == name, nutrient == "PTL_PPB"))
  m.N <- lm(log10(CHLA_PPB)~log10(concentration), limits_wide |> filter(is.finite(log10(CHLA_PPB)), ECO_REG_NAME == name, nutrient == "NTL_PPB"))
  
  
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


compare_r_values <- lm_ecoreg_df |>
  pivot_wider(names_from = "Model.Predictor", values_from = "r.squared")

lm_ecoreg_df1$Ecoregion = factor(lm_ecoreg_df1$Ecoregion,
                                  levels = c("National","Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))


#create a pretty table
library(gt)
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
gtsave(simpleregtable, "Figures/QNvsP.Figs/ecoregion_linearmodels_table.png") 





#### trying some linear models ####
m.1 <- lm(log10(CHLA_PPB)~log10(concentration), limits_wide |> filter(is.finite(log10(CHLA_PPB))))
summary(m.1) #adj R = 0.02399
anova(m.1)

m.2 <- lm(log10(CHLA_PPB)~log10(concentration)*nutrient, limits_wide |> filter(is.finite(log10(CHLA_PPB))))
summary(m.2) #adj R = 0.5068
anova(m.2, m.1)

m.3 <- lm(log10(CHLA_PPB)~log10(concentration)*nutrient*limitation, limits_wide |> filter(is.finite(log10(CHLA_PPB))))
summary(m.3) #adj R = 0.5364
anova(m.3, m.2) # i don't think this adds anything

m.4 <- lm(log10(CHLA_PPB)~log10(NTL_PPM), limits_wide |> filter(is.finite(log10(CHLA_PPB))))
summary(m.4) # p < 0.0001, r = 0.5153
ggplot(limits, aes(log10(NTL_PPM), log10(CHLA_PPB))) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(m.4)
par(mfrow=c(1,1)) # Change back to 1 x 1



m.5 <- lm(log10(CHLA_PPB)~log10(NTL_PPM)*TROPHIC_STATE, limits_wide |> filter(is.finite(log10(CHLA_PPB))))
summary(m.5) # p < 0.0001, r = 0.9156 
ggplot(limits_wide, aes(log10(NTL_PPM), log10(CHLA_PPB), color = TROPHIC_STATE)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)
anova(m.5)
coef(m.5)


m.6 <- lm(log10(CHLA_PPB)~log10(PTL_PPB), limits |> filter(is.finite(log10(CHLA_PPB))))
summary(m.6) # p < 0.0001, r = 0.4985 
ggplot(limits, aes(log10(PTL_PPB), log10(CHLA_PPB))) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(m.6)
par(mfrow=c(1,1)) # Change back to 1 x 1

m.7 <- lm(log10(CHLA_PPB)~log10(PTL_PPB)*TROPHIC_STATE, limits |> filter(is.finite(log10(CHLA_PPB))))
summary(m.7) # p < 0.0001, r = 0.9104
ggplot(limits, aes(log10(PTL_PPB), log10(CHLA_PPB), color = TROPHIC_STATE)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)
anova(m.7)
coef(m.7)

m.8 <- lm(log10(CHLA_PPB)~log10(PTL_PPB)*TROPHIC_STATE * ECO_REG_NAME, limits |> filter(is.finite(log10(CHLA_PPB))))
summary(m.8) # p < 0.0001, r = 0.9175 

m.9 <- lmer(log10(CHLA_PPB)~log10(PTL_PPB)*TROPHIC_STATE + (1|ECO_REG_NAME), limits |> filter(is.finite(log10(CHLA_PPB))))
summary(m.9) 
performance::r2(m.9) #marginal 0.907, conditional 0.911


limits1 <- limits |>
  mutate(logChla_ppb = log10(CHLA_PPB)) |>
  filter(is.finite(logChla_ppb))

ggplot(limits_wide, aes(limitation, log10(CHLA_PPB))) +
  geom_boxplot() 

ggplot(limits_wide, aes(TROPHIC_STATE, log10(CHLA_PPB), color = limitation)) +
  geom_boxplot() 
