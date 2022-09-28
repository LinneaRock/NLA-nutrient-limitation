################################################################################
############## Is trophic status (based on chlorophyll) more      ############## 
############## influenced by nitrogen or phosphorus and how/ why  ##############
############## does this relationship vary spatially?             ##############
################################################################################


source("Data/NLA/Call_NLA_data.R")


#### Looking at relationships of nutrients with chlorophyll ####
library(lme4)
library(lmerTest)

plot(CHLA_PPB ~ tn.tp color = ECO_REG_NAME, data =all_NLA)

m.1 <- lmer(CHLA_PPB ~ NTL_PPM*tn.tp + PTL_PPB*tn.tp + (1|ECO_REG_NAME), all_NLA)
summary(m.1)
coef(m.1)


# does n:p stoichiometry impact the relationships between trophic state and indiviudal nutrients?


####   ####
ggplot(all_NLA) +
  geom_point(aes(log(tn.tp), log(CHLA_PPB), color = TROPHIC_STATE)) +
  geom_smooth(aes(log(tn.tp), log(CHLA_PPB), color = TROPHIC_STATE))


ggplot(all_NLA) +
  geom_point(aes(log(tn.tp), log(CHLA_PPB), color = ECO_REG_NAME)) +
  geom_smooth(aes(log(tn.tp), log(CHLA_PPB), color = ECO_REG_NAME), se = FALSE)
