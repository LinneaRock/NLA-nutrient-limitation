# checking out Daniel's question about shifts in N:P stoichiometry
library(tidyverse)
library(colorblindr)
library(spsurvey)
library(broom)
library(lme4)
#library(gt)
library(patchwork)



#### Load and subset dataframe ####
source("Data/NLA/Call_NLA_data.R")
nla_data_subset <- all_NLA |>
  select(ECO_REG_NAME, UNIQUE_ID, DATE_COL, VISIT_NO, NTL_PPM, PTL_PPB, DIN_PPM, tn.tp, DIN.TP, CHLA_PPB, TROPHIC_STATE, year,WGT_NLA, LON_DD, LAT_DD, AREA_HA, ELEV_PT, PCT_DEVELOPED_BSN, PCT_AGRIC_BSN, SITE_TYPE, URBAN, LAKE_ORIGIN) |>
  rename(DIN.TP_molar = DIN.TP,
         TN.TP_molar = tn.tp) |>
  filter(year != "2012") |>
  filter(AREA_HA >= 4) |> # removes 216 observations 
  distinct()


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




#### Prep the data for the change analysis ####

change_dat <- limits|> # total 3066 lakes for this analysis 1773 - lakes with new dataset
  filter(VISIT_NO == 1) |># for this analysis, we are using just the first visit from each lake
  filter(WGT_NLA > 0) # the sp survey package is not designed to use the reference lakes, so those are ignored when using this package for analyses.



### write a function for this change analysis ####
stoich_change_fun <- function(data, name, year1, year2) {
  
  change_ecoreg<- change_analysis(data |> filter(year %in% c(year1, year2),
                                                 ECO_REG_NAME == name), siteID = "UNIQUE_ID",
                                  vars_cont = "TN.TP_molar", surveyID = "year", weight = "WGT_NLA",
                                  xcoord = "LON_DD", ycoord = "LAT_DD")
  
  change_ecoreg.1 <- change_ecoreg[["contsum_mean"]]  |>
    dplyr::select(Subpopulation, Indicator, DiffEst, StdError) |>
    mutate(ECO_REG = name) #|>
  #rename(Trophic.State = Subpopulation)
  
  
  change_national <- change_analysis(data |> filter(year %in% c(year1, year2)), siteID = "UNIQUE_ID", 
                                     vars_cont =  "TN.TP_molar", surveyID = "year", weight = "WGT_NLA", 
                                     xcoord = "LON_DD", ycoord = "LAT_DD")
  
  
  change_national.1 <- change_national[["contsum_mean"]] |>
    dplyr::select(Subpopulation, Indicator, DiffEst, StdError) |>
    mutate(ECO_REG = "National") #|>
  # rename(Trophic.State = Subpopulation) 
  
  final <- bind_rows(change_national.1, change_ecoreg.1)
  }

#### Run change analysis ####

list <- as.vector(change_dat |> select(ECO_REG_NAME) |> distinct())[["ECO_REG_NAME"]]

### change in nutrient limitations from 2007 to 2017
change0717 <- data.frame()

for(name in list) {
  
  tmp <- stoich_change_fun(change_dat, name, "2007", "2017")
  
  change0717 <- bind_rows(change0717, tmp) |> 
    distinct()
  
}









#### write a function for this change analysis now within limitation ####
stoich_change_lim_fun <- function(data, name, year1, year2) {
  
  change_ecoreg<- change_analysis(data |> filter(year %in% c(year1, year2),
                                                 ECO_REG_NAME == name), siteID = "UNIQUE_ID", subpop = "limitation", vars_cont = "TN.TP_molar", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
  
  change_ecoreg.1 <- change_ecoreg[["contsum_mean"]]  |>
    dplyr::select(Subpopulation, Indicator, DiffEst, StdError) |>
    mutate(ECO_REG = name) |>
    rename(Trophic.State = Subpopulation)
  
  
  change_national <- change_analysis(data |> filter(year %in% c(year1, year2)), siteID = "UNIQUE_ID", subpop = "limitation", vars_cont = "TN.TP_molar", surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
  
  
  change_national.1 <- change_national[["contsum_mean"]] |>
    dplyr::select(Subpopulation, Indicator, DiffEst, StdError) |>
    mutate(ECO_REG = "National") |>
    rename(Trophic.State = Subpopulation) 
  
  final <- bind_rows(change_national.1, change_ecoreg.1)
  
}


#### Run change analysis ####

list <- as.vector(change_dat |> select(ECO_REG_NAME) |> distinct())[["ECO_REG_NAME"]]


### change in nutrient limitations from 2007 to 2012
change0717_lim <- data.frame()

for(name in list) {
  
  tmp <- stoich_change_lim_fun(change_dat, name, "2007", "2017")
  
  change0717_lim <- bind_rows(change0717_lim, tmp) |> 
    distinct()
  
}


# change0717 is TN:TP shifts overall
# change0717_lim is TN:TP shifts within each limitation category


change0717$ECO_REG = factor(change0717$ECO_REG,
                                               levels = c("National","Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))

change0717_lim$ECO_REG = factor(change0717_lim$ECO_REG,
                            levels = c("National","Northern Appalachians", "Southern Appalachians", "Coastal Plains", "Temperate Plains", "Upper Midwest", "Northern Plains", "Southern Plains", "Xeric", "Western Mountains"))

ecoreg_plot <- 
ggplot(change0717_lim |>
                        filter(ECO_REG != "National")) +
  geom_point(aes(Trophic.State,DiffEst, fill = Trophic.State), 
             color = "black", pch = 21, size = 1, position=position_dodge(width=0.5))+
  geom_errorbar(aes(Trophic.State, DiffEst, ymin = DiffEst-StdError, ymax = DiffEst+StdError, color = Trophic.State), width = 0.2)  + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  #facet_grid(~Trophic.State, scales = "free_x") +
  facet_wrap(~ECO_REG, ncol = 3, scales = "free_y") +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% change TN:TP 2007-2017") +
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
nat_plot <- 
  ggplot(change0717_lim |>
                     filter(ECO_REG == "National")) +
  geom_point(aes(Trophic.State,DiffEst, fill = Trophic.State), color = "black", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Trophic.State, DiffEst, ymin = DiffEst-StdError, ymax = DiffEst+StdError, color = Trophic.State), width = 0.2)  + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  #facet_grid(~Trophic.State, scales = "free_x") +
  #facet_wrap(~ECO_REG, ncol = 3, scales = "free_y") +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% change TN:TP 2007-2017",
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
  ggsave("Exploration_Archive/Figs/Daniel_fig.png", height = 6, width = 6.5, units = "in", dpi = 1200) 
  
  
  
  
  

ggplot(change0717) +
    geom_point(aes(ECO_REG, DiffEst), 
               color = "black", pch = 21, size = 1, position=position_dodge(width=0.5))+
    geom_errorbar(aes(ECO_REG, DiffEst, ymin = DiffEst-StdError, 
                      ymax = DiffEst+StdError), width = 0.2)  + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    geom_hline(yintercept = 0) +
    labs(x = "",
         y = "% change TN:TP 2007-2017") +
    theme(axis.text.x = element_text(angle = 49, vjust = 1, hjust =1))
  
ggsave("Exploration_Archive/Figs/Daniel_fig2.png", height = 4.5, width = 6.5, units = "in", dpi = 1200)

  
  
  
  

  