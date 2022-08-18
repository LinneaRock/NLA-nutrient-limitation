
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
# create long dataframe with nutrient as a column
limits_wide <- limits |>
  mutate(NTL_PPB = NTL_PPM / 1000) |>
  pivot_longer(cols = c(PTL_PPB, NTL_PPB), names_to = "nutrient", values_to = "concentration") 
  

ggplot(limits_wide, aes(log10(concentration), log10(CHLA_PPB), color = limitation)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE, aes(group = limitation)) +
  geom_abline(slope = 0, intercept = log10(2)) +
  geom_abline(slope = 0, intercept = log10(7)) +
  geom_abline(slope = 0, intercept = log10(30)) +
  facet_wrap(~nutrient, scales = "free_x") +
  theme_minimal() +
  scale_color_manual("",values = palette_OkabeIto[5:7])

m.0 <- lm(log10(CHLA_PPB)~log10(concentration) * nutrient * limitation, limits_wide |> filter(is.finite(log10(CHLA_PPB))))
summary(m.0) #adj R = 0.5364
anova(m.0)
coef(m.0)



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
