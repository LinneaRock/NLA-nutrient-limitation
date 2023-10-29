#### 3. Calculate limitations ####
# get information about the refernece lakes
reflakes <- readxl::read_xlsx('Data/Reference_sites_2023-10-26.xlsx') |>
  left_join(readxl::read_xlsx('Data/crosswalk_ID.xlsx', sheet = 'LONG_NARS_ALLSurvey_SITE_ID_CRO') |>
              filter(SURVEY %in% c('NLA', 'NRSA')) |>
              select(UNIQUE_ID, SITE_ID)) |>
  select(-SITE_ID, -GROUP) |>
  rename(year = YEAR,
         ECO_REG = REGION) |>
  mutate(year = as.character(year)) |>
  pivot_wider(names_from = INDICATOR, values_from = VALUE) |>
  unique() |>
  mutate(ECO_REG = ifelse(ECO_REG %in% c('SPLnat', 'SPLman'), 'SPL', ECO_REG)) |>
  left_join(nla_data_subset |> select(UNIQUE_ID, year, NTL_PPM, PTL_PPB, DIN_PPM)) |>
  drop_na(DIN_PPM) |>
  left_join(all_NLA |> select(ECO_REG, ECO_REG_NAME) |> unique()) #|>
# get quantiles to remove extreme outliers
# group_by(year, ECO_REG_NAME) |>
# mutate(Q1_TP = quantile(PTL_PPB, 0.25),
#        Q3_TP = quantile(PTL_PPB, 0.75),
#        Q1_DIN = quantile(DIN_PPM, 0.25),
#        Q3_DIN = quantile(DIN_PPM, 0.75),
#        IQR_TP = IQR(PTL_PPB),
#        IQR_DIN = IQR(DIN_PPM)) |>
# ungroup() 

# reflakes_outrm <- reflakes |>
#   filter(PTL_PPB > Q1_TP-(IQR_TP*3),
#          PTL_PPB < Q3_TP+(IQR_TP*3), # dropped 7 values
#          DIN_PPM > Q1_DIN-(IQR_DIN*3),
#          DIN_PPM < Q3_DIN+(IQR_DIN*3)) # dropped 17 values


# 
# TPoutliers <- boxplot(PTL_PPB~ECO_REG_NAME *year, data=reflakes)$out #14
# DINoutliers <- boxplot(DIN_PPM~ECO_REG_NAME *year, data=reflakes)$out #28
# 
# reflakes_outrm <- reflakes[-which(reflakes$PTL_PPB %in% TPoutliers),]
# reflakes_outrm <- reflakes_outrm[-which(reflakes$DIN_PPM %in% DINoutliers),]

ref_np <- reflakes |>
  group_by(ECO_REG_NAME, year) |>
  # 75th percentile of nutrient concentrations from reference lakes
  summarise(percentile75TN_PPM = quantile(NTL_PPM, probs = 0.75), 
            percentile75TP_PPB = quantile(PTL_PPB, probs = 0.75),
            percentile75DIN_PPM = quantile(DIN_PPM, probs = 0.75)) |>
  ungroup() |>
  # in one case, the 75th percentile of TP was way higher than reasonable, so we discarded that and used the 75th percentile from the other survey in the same ecoregion.
  mutate(percentile75TP_PPB = ifelse(percentile75TP_PPB == 478.50000, 38.08750, percentile75TP_PPB))

# ref_np <- reflakes |>
#   group_by(ECO_REG_NAME, year) |>
#   # 75th percentile of nutrient concentrations from reference lakes
#   summarise(percentile75TN_PPM = quantile(NTL_PPM, probs = 0.75), 
#             percentile75TP_PPB = quantile(PTL_PPB, probs = 0.75),
#             percentile75DIN_PPM = quantile(DIN_PPM, probs = 0.75)) |>
#   ungroup()
# ref_np <- nla_data_subset |>
#   filter(SITE_TYPE %in% c("REF_Lake", "HAND")) |> # subset of 230 lakes
#   group_by(ECO_REG_NAME, year) |>
#   # 75th percentile of nutrient concentrations from reference lakes
#   summarise(percentile75TN_PPM = quantile(NTL_PPM, probs = 0.75), 
#             percentile75TP_PPB = quantile(PTL_PPB, probs = 0.75),
#             percentile75DIN_PPM = quantile(DIN_PPM, probs = 0.75)) |>
#   ungroup()

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
         DIN_threshold = median(c(percentile75DIN_PPM, percentile25DIN_PPM))) # |>
# # There were no reference lakes in the Northern Plains in 2007. So, concentration thresholds were determined solely by the 25th percentile of all assessed lakes in that region in that year. 
# mutate(TP_threshold = ifelse(is.na(TP_threshold), percentile25TP_PPB, TP_threshold),
#        TN_threshold = ifelse(is.na(TN_threshold), percentile25TN_PPM, TN_threshold),
#        DIN_threshold = ifelse(is.na(DIN_threshold), percentile25DIN_PPM, DIN_threshold)) 

# criteria <- left_join(averages_np, ref_np) |>
#   mutate(useRefN = percentile75DIN_PPM < percentile25DIN_PPM,
#          useRefP = percentile75TP_PPB < percentile25TP_PPB) |>
#   # mutate(TP_threshold = ifelse(useRefP == 'TRUE', percentile75TP_PPB, percentile25TP_PPB),
#   #        DIN_threshold = ifelse(useRefN == 'TRUE', percentile75DIN_PPM, percentile25DIN_PPM))

write.csv(criteria, "criteria.csv")

