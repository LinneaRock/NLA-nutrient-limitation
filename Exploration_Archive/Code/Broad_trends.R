
# Script to look at trends of stoichiometry, limitations, and trophic states across the US

# call datasets and libraries 
source("Data/NLA/Call_NLA_data.R")
library(colorblindr)
library(khroma)
library(sf)
library(patchwork)

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


#### Get some basic info for the maps ####

regions.sf <- read_sf("Data/aggr_ecoregions_2015/Aggr_Ecoregions_2015.shp")
nla_locations.sf <- st_as_sf(limits, coords = c("LON_DD", "LAT_DD"), crs = 4326)
muted <- c("#CC6677", "#332288", "#DDCC77", "#117733", "#88CCEE", "#882255", "#44AA99", "#999933", "#AA4499" ) # ecoreg color scheme


#### Function to plot trophic state across US ####

TS_map_fun <- function(year1) {
  
  ggplot() +
    geom_sf(data = regions.sf, aes(fill = WSA9_NAME), alpha = 0.25) +
    geom_sf(data = nla_locations.sf |> filter(year == year1, !is.na(TROPHIC_STATE)), aes(color = TROPHIC_STATE)) +
    theme_minimal() +
    scale_fill_manual("", values = muted) +
    scale_color_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1])) 
  
}

#### Function to plot limitation across US ####

limit_map_fun <- function(year1) {
  
  ggplot() +
    geom_sf(data = regions.sf, aes(fill = WSA9_NAME), alpha = 0.50) +
    geom_sf(data = nla_locations.sf |> filter(year == year1, !is.na(limitation)), aes(color = limitation), size =2) +
    theme_minimal() +
    scale_fill_manual("", values = muted) +
    scale_color_manual("", values = c("grey60","red4", "#336a98")) 
}


#### Function to plot N:P stoichiometry across US ####

stoich_map_fun <- function(year1) {
  
  ggplot() +
    geom_sf(data = regions.sf, aes(fill = WSA9_NAME), alpha = 0.25) +
    geom_sf(data = nla_locations.sf |> filter(year == year1, !is.na(tn.tp)), aes(color = log10(tn.tp))) +
    theme_minimal() +
    scale_fill_manual("", values = muted) +
    scale_color_viridis_c('Log10 TN:TP molar ratio') 
  
}


#### Creating the maps ####

# legend for ecoregions on map
leg = ggplot() +
  theme_void() +
  xlim(0,1) +
  ylim(0,1) +
  geom_point(aes(x = 0, y = seq(1,0.2,length.out = 9)), 
             size = 2, shape = c(rep(22,9)), 
             fill = muted,
             alpha = 0.25) +
  geom_text(aes(x=0.08,y=0.6,
            label='Coastal Plains\nNorthern Appalachians\nSouther Appalachians\nSouthern Plains\nTemporate Plains\nUpper Midwest\nWestern Mountains\nXeric '),
            hjust = 0,
            size = 2) +
  xlab(NULL) + plot_layout(tag_level = 'new'); leg


TS_map_fun(year1 ="2007")
TS_map_fun(year1 ="2012")
p1 <- TS_map_fun(year1 ="2017") +
  guides(fill ="none") 


limit_map_fun(year1 ="2007")
limit_map_fun(year1 ="2012")
p2 <- limit_map_fun(year1 ="2017") +
  guides(fill = "none")  


stoich_map_fun(year1 ="2007")
stoich_map_fun(year1 ="2012")
p3 <- stoich_map_fun(year1 ="2017") +
  guides(fill = "none") 


(p1/p2/p3)
ggsave("Figures/Qtrend.Figs/trendmaps.png", height = 8.5, width = 6.5, units = "in", dpi = 500) 
leg
ggsave("Figures/Qtrend.Figs/ecoreglegend.png", height = 4.5, width = 6.5, units = "in", dpi = 500)


#### Map of N vs P as predictor (information discovered in N_vs_P_trophic_status.R script) ####
WSA9_NAME <- as.vector(all_NLA |> select(ECO_REG_NAME) |> distinct())[["ECO_REG_NAME"]]
best_predictor <- as.vector(c("TN", "TP", "TP", "TP", "TP", "TN", "TN", "TN", "TP"))
add <- cbind(data.frame(WSA9_NAME), data.frame(best_predictor))

regions.sf1 <- regions.sf |>
  mutate(WSA9_NAME = ifelse(WSA9_NAME == "Temporate Plains", "Temperate Plains", WSA9_NAME)) |>
  left_join(add)
  

ggplot() +
  geom_sf(data = regions.sf1, aes(fill = best_predictor)) +
  theme_minimal() +
  scale_fill_manual("Better predictor of trophic state", values=c("#084c61", "#ffc857")) 

ggsave("Figures/QNvsP.Figs/N_vs_P_map.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 


#### relationships between limits, stoich, trophic state ####

limits1 <- limits |>
  mutate(logDINP = log10(DIN.TP_molar)) |>
  mutate(year = as.factor(year))

ggplot(limits1, aes(year, logDINP)) +
  geom_boxplot()
anova(aov(logDINP~year, limits1)) # p < 2.2e-16 
TukeyHSD(aov(logDINP~year, limits1), conf.level = 0.95) # p < 0.001 - all groups are different at the 95% CI
plot(TukeyHSD(aov(logDINP~year, limits1), conf.level = 0.95), las = 2)

ggplot(limits1, aes(limitation, logDINP)) +
  geom_boxplot()
 anova(aov(logDINP~limitation, limits1)) # p < 2.2e-16 
TukeyHSD(aov(logDINP~limitation, limits1), conf.level = 0.95) # p < 0.001 - all groups are different at the 95% CI
plot(TukeyHSD(aov(logDINP~limitation, limits1), conf.level = 0.95), las = 2)

ggplot(limits1, aes(TROPHIC_STATE, logDINP)) +
  geom_boxplot()
anova(aov(logDINP~TROPHIC_STATE, limits1)) # p < 2.2e-16 
TukeyHSD(aov(logDINP~TROPHIC_STATE, limits1), conf.level = 0.95) # p < 0.001 - all groups are different EXCEPT comparing meso-oligo
plot(TukeyHSD(aov(logDINP~TROPHIC_STATE, limits1), conf.level = 0.95), las = 2)

ggplot(limits1, aes(limitation, logDINP, color = TROPHIC_STATE)) +
  geom_boxplot()
anova(aov(logDINP~limitation*TROPHIC_STATE, limits1)) # p < 2.2e-16 
TukeyHSD(aov(logDINP~limitation*TROPHIC_STATE, limits1), conf.level = 0.95) # various, some p < 0.05, many p <0.0001. 14 relationships are not significantly different
plot(TukeyHSD(aov(logDINP~limitation*TROPHIC_STATE, limits1), conf.level = 0.95), las = 2)

ggplot(limits1, aes(ECO_REG_NAME, limitation)) +
  geom_boxplot() # check out patterns!!
anova(aov(logDINP~ECO_REG_NAME, limits1)) # p < 2.2e-16 
TukeyHSD(aov(logDINP~ECO_REG_NAME, limits1), conf.level = 0.95) 

ggplot(limits1, aes(limitation, logDINP, color = ECO_REG_NAME)) +
  geom_boxplot() # check out patterns!!
anova(aov(logDINP~limitation*ECO_REG_NAME, limits1)) # p < 2.2e-16 
TukeyHSD(aov(logDINP~limitation*ECO_REG_NAME, limits1), conf.level = 0.95) # various, some p < 0.05, many p <0.0001. Good amount are not significantly different (over 100 relationships compared)
plot(TukeyHSD(aov(logDINP~limitation*ECO_REG_NAME, limits1), conf.level = 0.95), las = 2)

ggplot(limits1, aes(TROPHIC_STATE, logDINP, color = ECO_REG_NAME)) +
  geom_boxplot() # check out patterns!! - OVERALL N:P highest in oligo and decreases to Hyper   
anova(aov(logDINP~TROPHIC_STATE*ECO_REG_NAME, limits1)) # p < 2.2e-16 
TukeyHSD(aov(logDINP~TROPHIC_STATE*ECO_REG_NAME, limits1), conf.level = 0.95) # good amount are not significantly different (over 400 relationships compared)
plot(TukeyHSD(aov(logDINP~TROPHIC_STATE*ECO_REG_NAME, limits1), conf.level = 0.95), las = 2)

library(lme4)
m.1 <- lmer(logNP~limitation*TROPHIC_STATE + (1|ECO_REG_NAME), limits1)
anova(m.1)
summary(m.1)
performance::r2(m.1) # marginal 0.448, conditional 0.599

m.2 <- lmer(logNP~limitation*TROPHIC_STATE + URBAN + (1|ECO_REG_NAME), limits1)
anova(m.2)
summary(m.2)
performance::r2(m.2) # marginal 0.448, conditional 0.599
anova(m.2, m.1) # adding URBAN makes it marginally worse - higher AIC, but barely. No significant difference

m.3 <- lmer(logNP~limitation*TROPHIC_STATE + LAKE_ORIGIN + (1|ECO_REG_NAME), limits1)
anova(m.3)
summary(m.3)
performance::r2(m.3) # marginal 0.452, conditional 0.597 - performs slightly worse

m.4 <- lmer(logNP~limitation*TROPHIC_STATE + year + (1|ECO_REG_NAME), limits1)
anova(m.4)
summary(m.4)
performance::r2(m.4) # marginal 0.481, conditional  0.631
anova(m.4, m.1) # this model incorporating year is significantly better than m.1 (AIC is waaaay lower)

m.5 <- lmer(logNP~scale(AREA_HA)*limitation*TROPHIC_STATE + year + (1|ECO_REG_NAME), limits1 |> filter(SITE_ID != "NLA17_WA-10052"))
anova(m.5)
summary(m.5) # singular fit?
performance::r2(m.5) # marginal 0.483, conditional  0.632
anova(m.5, m.4) # this model incorporating year is significantly better than m.4 (AIC slightly lower, but significant difference)

m.6 <- lmer(logNP~scale(AREA_HA)*limitation*TROPHIC_STATE + (1|year) + (1|ECO_REG_NAME), limits1)
performance::r2(m.6)
anova(m.6, m.5) # m.5 is a better fit model



m.7 <- lmer(logNP~scale(AREA_HA)*limitation*TROPHIC_STATE + year + scale(PCT_DEVELOPED_BSN)+ (scale(PCT_DEVELOPED_BSN)|ECO_REG_NAME), limits1|> filter(SITE_ID != "NLA17_WA-10052")) # have to remove one site that does not have basin characteristics.
performance::r2(m.7) # marginal 0.485, conditional  0.633
anova(m.7, m.5) # models were not all fitted to the same size of dataset?? -- had to delete one row of data :( but it worked
# m.7 is significantly better than m.5



m.8 <-  lmer(logNP~scale(AREA_HA)*limitation*TROPHIC_STATE + year + scale(PCT_DEVELOPED_BSN) + (1|ECO_REG_NAME), limits1|> filter(SITE_ID != "NLA17_WA-10052"))
performance::r2(m.8) # marginal 0.484, conditional  0.632
anova(m.8, m.7) # m.7 is slightly significantly better 

m.9 <- lmer(logNP~scale(AREA_HA)*limitation*TROPHIC_STATE + year + scale(PCT_DEVELOPED_BSN)+ scale(PCT_AGRIC_BSN)+ (scale(PCT_DEVELOPED_BSN)|ECO_REG_NAME), limits1|> filter(SITE_ID != "NLA17_WA-10052"))
performance::r2(m.9) # marginal 0.485, conditional  0.633
anova(m.9, m.7) # no significant difference; but m.7 has lower AIC



m.10 <- lmer(logNP~scale(AREA_HA)*limitation*TROPHIC_STATE + year + scale(PCT_DEVELOPED_BSN)+ scale(PCT_AGRIC_BSN)+ (scale(PCT_DEVELOPED_BSN)|ECO_REG_NAME) + (scale(PCT_AGRIC_BSN)|ECO_REG_NAME), limits1|> filter(SITE_ID != "NLA17_WA-10052"))
summary(m.10)
anova(m.10)
performance::r2(m.10) # marginal 0.529, conditional  0.595
anova(m.10, m.7) # significant difference; m.10 has AIC ~30 vs m.7 AIC of ~40




m.11 <- lmer(logNP~scale(AREA_HA)*limitation*TROPHIC_STATE + year + scale(PCT_AGRIC_BSN)+ (scale(PCT_AGRIC_BSN)|ECO_REG_NAME), limits1|> filter(SITE_ID != "NLA17_WA-10052"))
performance::r2(m.11) # marginal 0.483, conditional  0.627
anova(m.11, m.9) # no significant difference using agriculture over development -- using both seems best 

m.12 <-  lmer(logNP~scale(AREA_HA)*limitation*TROPHIC_STATE + year + scale(PCT_AGRIC_BSN) + (1|ECO_REG_NAME), limits1|> filter(SITE_ID != "NLA17_WA-10052"))
performance::r2(m.12) # marginal 0.484, conditional  0.632
anova(m.12, m.8) # no significant difference using agriculture over development -- using both seems best 




m.13 <- lmer(logNP~scale(AREA_HA)*limitation*TROPHIC_STATE + year + scale(PCT_DEVELOPED_BSN)+ scale(PCT_AGRIC_BSN)+ (1|ECO_REG_NAME), limits1|> filter(SITE_ID != "NLA17_WA-10052"))
performance::r2(m.13) # marginal 0.484, conditional  0.632
anova(m.9, m.13) # m.9 slightly significantly better. 
