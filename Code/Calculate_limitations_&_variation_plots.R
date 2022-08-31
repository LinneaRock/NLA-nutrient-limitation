
## Script to calculate whether lakes are N, P, or co-limited & look at trends

# call datasets and libraries 
source("Data/NLA/Call_NLA_data.R")
library(colorblindr)
library(khroma)
library(patchwork)
library(spsurvey) 


#### Calculate limitations ####


# get information about the refernece lakes
ref_np <- all_NLA |>
  filter(SITE_TYPE %in% c("REF_Lake", "HAND")) |> # subset of 230 lakes
  group_by(ECO_REG_NAME) |>
  summarise(meanNP_t = mean(tn.tp),
            meanNP = (mean(TN_mol)/mean(TP_mol)),
            medianNP = (median(TN_mol)/median(TP_mol)),
            medianTN_PPM = median(NTL_PPM),
            medianTP_PPB = median(PTL_PPB))

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
 

# How do the lower 25th percentiles of TN and TP compare the the median  concentrations of TN and TP in the reference lakes? 
t.test(ref_np$medianTN_PPM, averages_np$percentile25TN_PPM) # these are similar to each other!! 
t.test(ref_np$medianTP_PPB, averages_np$percentile25TP_PPB) # these are similar to each other!!



# uses 25th percentile nutrient thresholds for each ecoregion (over all years) and logged average N:P for each ecoregion (over each year)

limits <- all_NLA |>
  left_join(averages_np) |>
  mutate(limitation = NA) |>
  mutate(limitation = ifelse(PTL_PPB > percentile25TP_PPB & log(tn.tp) < yearlymeanlogNP, "Potential N-limitation", 
                             ifelse(NTL_PPM > percentile25TN_PPM & log(tn.tp) > yearlymeanlogNP, "Potential P-limitation",
                                    ifelse(is.na(limitation), "Potential co-nutrient limitation", limitation))))

nrow(limits |> filter(limitation == "Potential P-limitation")) # 1299 (1285 when using overal mean N:P)
nrow(limits |> filter(limitation == "Potential N-limitation")) # 1703 (1727 when using overal mean N:P)
nrow(limits |> filter(limitation == "Potential co-nutrient limitation")) # 651 (641 when using overal mean N:P)



ggplot(limits) +
  geom_point(aes(log(PTL_PPB, base = 10), log(NTL_PPM, base = 10), fill = limitation), size = 2.5, shape = 21, alpha = 0.8) +
  theme_minimal() +
  scale_fill_manual("",values = palette_OkabeIto[5:7]) +
  labs(y = "Log TN"~(m*g~L^-1), x = "Log TP"~(mu*g~L^-1))
ggsave("Figures/Qlim.Figs/FINAL_limitationsmethod.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 


#### Bar charts of lakes in each trophic state that are N, P, or co-limited in each ecoregion ####
## take into account the weights for proportions
## plots show what percentage of N, P, or Potential co-nutrient limitation in each ecoregion and the breakdown into trophic state

weighted_limits <- limits |>
  group_by(year, ECO_REG_NAME, TROPHIC_STATE, limitation) |>
  mutate(weighted_lim = sum(WGT_NLA)) |>
  ungroup() |>
  select(year, ECO_REG_NAME, TROPHIC_STATE, limitation, weighted_lim) |>
  distinct() |>
  group_by(year, ECO_REG_NAME) |>
  mutate(weighted_total = sum(weighted_lim)) |>
  mutate(prop = (weighted_lim/weighted_total) * 100) |>
  ungroup() |>
  drop_na(prop) 

proportional_columns <- function( year1, filename) {
  ggplot(weighted_limits |> filter( year == year1)) +
    geom_col(aes(limitation, prop, fill = TROPHIC_STATE)) +
    theme_bw() +
    scale_fill_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1])) +
    labs(x = "",
         y = "% lakes in each limitation type",
         title = year1)  +
    facet_wrap(~ECO_REG_NAME, ncol = 3) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  ggsave(paste("Figures/Qlim.Figs/", filename, ".png", sep = ""), height = 4.5, width = 6.5, units = "in", dpi = 500)
  
}

# create the grap
proportional_columns("2007", "Limitations_2007")
proportional_columns("2012", "Limitations_2012")
proportional_columns("2017", "Limitations_2017")



#### Scatterplot of how # lakes in each trophic state has changed over time ####
## take into account the weights for proportions

muted <- colour("muted")
plot_scheme(muted(9), colours = TRUE, names = TRUE, size = 0.9)
muted(9)
muted <- c("#CC6677", "#332288", "#DDCC77", "#117733", "#88CCEE", "#882255", "#44AA99", "#999933", "#AA4499" )

no.lim <- weighted_limits |>
  select(year, weighted_lim, limitation, ECO_REG_NAME) |>
  group_by(year, limitation, ECO_REG_NAME) |>
  mutate(wgt_lim = sum(weighted_lim)) |>
  select(-weighted_lim) |>
  distinct()

nlim <- ggplot(no.lim |>
         filter(limitation == "Potential N-limitation")) +
  geom_point(aes(year, wgt_lim, color = ECO_REG_NAME)) +
  geom_line(aes(year, wgt_lim, group = ECO_REG_NAME, color = ECO_REG_NAME)) +
  theme_minimal() +
  scale_color_manual("", values = muted) +
  theme(legend.position = "none") +
  labs(x = "",
       y = "(Weighted) number of N-limited lakes")

#global trend (linear model):
N_model <- lm(wgt_lim~year, (no.lim |>
                               filter(limitation == "Potential N-limitation")))
summary(N_model) #adj R = 0.121, p = 0.08137


plim <- ggplot(no.lim |>
                 filter(limitation == "Potential P-limitation")) +
  geom_point(aes(year, wgt_lim, color = ECO_REG_NAME)) +
  geom_line(aes(year, wgt_lim, group = ECO_REG_NAME, color = ECO_REG_NAME)) +
  theme_minimal() +
  scale_color_manual("", values = muted) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(x = "",
       y = "(Weighted) number of P-limited lakes")

#global trend (linear model):
P_model <- lm(wgt_lim~year, (no.lim |>
                               filter(limitation == "Potential P-limitation")))
summary(P_model) #adj R = 0.1199, p = 0.08268


colim <- ggplot(no.lim |>
            filter(limitation == "Potential co-nutrient limitation")) +
  geom_point(aes(year, wgt_lim, color = ECO_REG_NAME)) +
  geom_line(aes(year, wgt_lim, group = ECO_REG_NAME, color = ECO_REG_NAME)) +
  theme_minimal() +
  scale_color_manual("", values = muted) +
  theme(legend.position = "none") +
  labs(x = "",
       y = "(Weighted) number of co-limited lakes")

#global trend (linear model):
co_model <- lm(wgt_lim~year, (no.lim |>
                               filter(limitation == "Potential co-nutrient limitation")))
summary(co_model) #adj R = -0.02681 , p = 0.5257


(nlim | plim | colim ) +
  plot_annotation(tag_levels = "a", tag_suffix = ")")
ggsave("Figures/Qlim.Figs/number_lakes_change.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 


#### looking at trophic states and limitations ####
no.lim.ts <- weighted_limits |>
  select(year, weighted_lim, limitation, TROPHIC_STATE) |>
  group_by(year, limitation, TROPHIC_STATE) |>
  mutate(wgt_lim = sum(weighted_lim)) |>
  select(-weighted_lim) |>
  distinct()

ggplot(no.lim.ts |> filter(year == "2007"), aes(TROPHIC_STATE, wgt_lim, fill = limitation)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("", values = palette_OkabeIto[5:7]) +
  theme_minimal() +
  labs(x = "", y = "(Weighted) number of lakes")
ggsave("Figures/Qlim.Figs/nolakes_by_ts_lim2007.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

ggplot(no.lim.ts |> filter(year == "2012"), aes(TROPHIC_STATE, wgt_lim, fill = limitation)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("", values = palette_OkabeIto[5:7]) +
  theme_minimal() +
  labs(x = "", y = "(Weighted) number of lakes")
ggsave("Figures/Qlim.Figs/nolakes_by_ts_lim2012.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

ggplot(no.lim.ts |> filter(year == "2017"), aes(TROPHIC_STATE, wgt_lim, fill = limitation)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("", values = palette_OkabeIto[5:7]) +
  theme_minimal() +
  labs(x = "", y = "(Weighted) number of lakes")
ggsave("Figures/Qlim.Figs/nolakes_by_ts_lim2017.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 





#### Limitation shifts over time ####
library(spsurvey) 

limits_change_prep <- limits|> # total 3066 lakes for this analysis 
  filter(VISIT_NO == 1) |># for this analysis, we are using just the first visit from each lake
  filter(WGT_NLA > 0) # the sp survey package is not designed to use the reference lakes, so those are ignored when using this package for analyses.


# lakes with limitations considered for this analysis now 
nrow(limits_change_prep |> filter(limitation == "Potential P-limitation")) # 1113 (1092 when using overal mean N:P)
nrow(limits_change_prep |> filter(limitation == "Potential N-limitation")) # 1459 (1492 when using overal mean N:P)
nrow(limits_change_prep |> filter(limitation == "Potential co-nutrient limitation")) # 494 (482 when using overal mean N:P)


### write a function for this change analysis 
lim_change_fun <- function(data, name, limit_var, year1, year2) {
  
  change_ecoreg<- change_analysis(data |> filter(year %in% c(year1, year2),
                                                 ECO_REG_NAME == name), siteID = "UNIQUE_ID", subpop = "TROPHIC_STATE", vars_cat = limit_var, surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
  
  change_ecoreg.1 <- change_ecoreg[["catsum"]]  |>
    select(Subpopulation, Category, Indicator, DiffEst.P, StdError.P) |>
    mutate(ECO_REG = name) |>
    rename(Trophic.State = Subpopulation)
  
  
  change_national <- change_analysis(data |> filter(year %in% c(year1, year2)), siteID = "UNIQUE_ID", subpop = "TROPHIC_STATE", vars_cat = limit_var, surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
  
  
  change_national.1 <- change_national[["catsum"]] |>
    select(Subpopulation, Category, Indicator, DiffEst.P, StdError.P) |>
    mutate(ECO_REG = "National") |>
    rename(Trophic.State = Subpopulation) 
  
  final <- bind_rows(change_national.1, change_ecoreg.1)
  
}

list <- as.vector(limits_change_prep |> select(ECO_REG_NAME) |> distinct())[["ECO_REG_NAME"]]



### change in nutrient limitations from 2007 to 2012
change0712 <- data.frame()

for(name in list) {
  
  tmp <- lim_change_fun(limits_change_prep, name, "limitation", "2007", "2012")
  
  change0712 <- bind_rows(change0712, tmp) |> 
    distinct()
  
}

#check warnings and actions
actions0712 <- data.frame(warn_df$warning, warn_df$action) # repeat sites covariance was not included in standard error, covariance estimate

lim_change0712 <- change0712 |>
  mutate(year.shift = "2007-2012")



### change in nutrient limitations from 2012 to 2017
change1217 <- data.frame()

for(name in list) {
  
  tmp <- lim_change_fun(limits_change_prep, name, "limitation", "2012", "2017")
  
  change1217 <- bind_rows(change1217, tmp) |> 
    distinct()
  
}
#check warnings and actions
actions1217 <- data.frame(warn_df$warning, warn_df$action) # repeat sites covariance was not included in standard error, covariance estimate

lim_change1217 <- change1217 |>
  mutate(year.shift = "2012-2017")

changes.final <- rbind(lim_change0712, lim_change1217) |>
  mutate(Trophic.State = factor(Trophic.State, levels = c("Oligo.", "Meso.", "Eutro.", "Hyper.")))



### create some figures to visualize this analysis 
# National plot for N limitation
ggplot(changes.final |>
         filter(ECO_REG == "National",
                Category == "Potential N-limitation")) +
  geom_point(aes(Trophic.State, DiffEst.P, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Trophic.State, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_minimal() +
  facet_grid(.~Trophic.State, scales = "free_x") +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  theme(axis.text.x = element_blank()) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in potential N-limitation lakes",
       title = "National") 
ggsave("Figures/Qlim.Figs/N_limitation_changes_percentdiff_national.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

# ecoregion plots 
ggplot(changes.final |>
         filter(ECO_REG != "National",
                Category == "Potential N-limitation")) +
  geom_point(aes(Trophic.State, DiffEst.P, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Trophic.State, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_bw() +
  facet_grid(~Trophic.State, scales = "free_x") +
  facet_wrap(~ECO_REG, ncol = 3, scales = "free_y") +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in potential N-limitation lakes") +
  scale_x_discrete(labels = c("Olig.", "Meso.", "Eutro.", "Hyper."))
ggsave("Figures/Qlim.Figs/N_limitation_changes_percentdiff_regional.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 


# National plot for P limitation
ggplot(changes.final |>
         filter(ECO_REG == "National",
                Category == "Potential P-limitation")) +
  geom_point(aes(Trophic.State, DiffEst.P, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Trophic.State, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_minimal() +
  facet_grid(.~Trophic.State, scales = "free_x") +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  theme(axis.text.x = element_blank()) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in potential P-limitation lakes",
       title = "National") 
ggsave("Figures/Qlim.Figs/P_limitation_changes_percentdiff_national.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

# ecoregion plots 
ggplot(changes.final |>
         filter(ECO_REG != "National",
                Category == "Potential P-limitation")) +
  geom_point(aes(Trophic.State, DiffEst.P, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Trophic.State, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_bw() +
  facet_grid(~Trophic.State, scales = "free_x") +
  facet_wrap(~ECO_REG, ncol = 3, scales = "free_y") +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in potential P-limitation lakes") +
  scale_x_discrete(labels = c("Olig.", "Meso.", "Eutro.", "Hyper."))
ggsave("Figures/Qlim.Figs/P_limitation_changes_percentdiff_regional.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 



# Nationalplot for co-limitation
ggplot(changes.final |>
         filter(ECO_REG == "National",
                Category == "Potential co-nutrient limitation")) +
  geom_point(aes(Trophic.State, DiffEst.P, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Trophic.State, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_minimal() +
  facet_grid(.~Trophic.State, scales = "free_x") +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  theme(axis.text.x = element_blank()) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in potential co-nutrient limitation lakes",
       title = "National") 
ggsave("Figures/Qlim.Figs/no_limitation_changes_percentdiff_national.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 

# ecoregion plots 
ggplot(changes.final |>
         filter(ECO_REG != "National",
                Category == "Potential co-nutrient limitation")) +
  geom_point(aes(Trophic.State, DiffEst.P, fill = year.shift), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Trophic.State, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = year.shift), width = 0.2, position=position_dodge(width=0.5))  + 
  theme_bw() +
  facet_grid(~Trophic.State, scales = "free_x") +
  facet_wrap(~ECO_REG, ncol = 3, scales = "free_y") +
  #theme(strip.background = element_rect(color = "black", fill = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]), size = 1.5, linetype = "solid")) +
  scale_fill_manual("", values = c("red4", "#336a98")) +
  scale_color_manual("", values = c("red4", "#336a98")) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% Difference in potential co-nutrient limitation lakes") +
  scale_x_discrete(labels = c("Olig.", "Meso.", "Eutro.", "Hyper."))
ggsave("Figures/Qlim.Figs/no_limitation_changes_percentdiff_regional.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 



###############################################################
#### # Percent lakes in each ecoregion ####


years <- c("2007", "2012", "2017")
final <- data.frame()

for(i in 1:length(years)) {
  data <- limits_change_prep |>
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
  
  final <- rbind(final, tmp)
}

nat <- cat_analysis(
  limits_change_prep,
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

final1 <- rbind(final, nat1)




p1 <- ggplot(final1 |> filter(year == "2007",
                              Category != "Total"), aes(Subpopulation, Estimate.P, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Estimate.P - StdError.P, ymax = Estimate.P + StdError.P), width = 0.2, position=position_dodge(width=0.9)) +
  scale_fill_manual("", values = palette_OkabeIto[5:7]) +
  theme_minimal() +
  coord_flip() +
  labs(x = "",
       y = "Percent of lakes",
       title = "2007") +
  theme(legend.position = "none")



p2 <- ggplot(final1 |> filter(year == "2012",
                              Category != "Total"), aes(Subpopulation, Estimate.P, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Estimate.P - StdError.P, ymax = Estimate.P + StdError.P), width = 0.2, position=position_dodge(width=0.9)) +
  scale_fill_manual("", values = palette_OkabeIto[5:7]) +
  theme_minimal() +
  coord_flip() +
  labs(x = "",
       y = "Percent of lakes",
       title = "2012") +
  theme(legend.position = "bottom",
        axis.text.y = element_blank())




p3 <- ggplot(final1 |> filter(year == "2017",
                              Category != "Total"), aes(Subpopulation, Estimate.P, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Estimate.P - StdError.P, ymax = Estimate.P + StdError.P), width = 0.2, position=position_dodge(width=0.9)) +
  scale_fill_manual("", values = palette_OkabeIto[5:7]) +
  theme_minimal() +
  coord_flip() +
  labs(x = "",
       y = "Percent of lakes",
       title = "2017") +
  theme(legend.position = "none",
        axis.text.y = element_blank())

(p1 | p2 | p3)
ggsave("Figures/Qlim.Figs/limitations_yearly.png", height = 4.5, width = 6.5, units = "in", dpi = 500) 