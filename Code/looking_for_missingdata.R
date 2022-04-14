## looking into some problems with missing data 


library(tidyverse)

NLA_2007 <- read.csv("Data/NLA/NLA_2007.csv")
NLA_2012 <- read.csv("Data/NLA/NLA_2012.csv")
NLA_2017 <- read.csv("Data/NLA/NLA_2017.csv")



na07 <- NLA_2007 |>
  mutate(findNA = complete.cases(NLA_2007)) # just 5 missing unique IDs, so I will add the code in to write missing uniques with that year's site ID and resave the csv file -- RESOLVED :)

na12 <- NLA_2012 |>
  mutate(findNA = complete.cases(NLA_2012)) # issues resolved!!!

na17 <- NLA_2017 |>
  mutate(findNA = complete.cases(NLA_2017)) # the 4 missing TP values (I already knew these were missing)





# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # This issue has been resolved using the key variables dataset !!
# missing12 <- NLA_2012 |>
#   filter(is.na(ECO_REG)) |> # total of 192 observations are missing this information
#   select(UNIQUE_ID) |>
#   distinct()
# 
# 
# # can I get this information from other unique IDs from 2012?
# 
# missinginfo <- NLA_2012 |>
#   select(UNIQUE_ID, WGT_NLA, ECO_REG, CHLA_COND, NTL_COND, PTL_COND, TROPHIC_STATE)
# 
# ## weights -- nope.
# 
# all_wgt <- missinginfo |>
#   select(UNIQUE_ID, WGT_NLA) |>
#   #drop_na() |>
#   distinct()
# 
# miss_wgt <- missinginfo |>
#   select(UNIQUE_ID, WGT_NLA) |>
#   filter(is.na(WGT_NLA)) |>
#   distinct() |> # 92 ids are missing weights
#   select(UNIQUE_ID)
# 
# 
# fill_wgt <- left_join(miss_wgt, all_wgt) # this fixes nothing
# 
# 
# ## other info
# 
# all_info <- missinginfo |>
#  # select(-WGT_NLA) |>
#   drop_na() |>
#   distinct()
# 
# 
# miss_info <- missinginfo |>
#  # select(-WGT_NLA) |>
#   filter(is.na(ECO_REG)) |>
#   distinct() |> # 100 ids are missing this information
#   select(UNIQUE_ID)
# 
# fill_info <- left_join(miss_info, all_info) # filled in all of the data!!
# fill_info <- fill_info |>
#   rename(WGT2 = WGT_NLA,
#          ECO2 = ECO_REG,
#          chcond2 = CHLA_COND,
#          tpcond2 = PTL_COND,
#          tncond2 = NTL_COND,
#          tstate = TROPHIC_STATE)
# 
# 
# # how many leftover that need information?
# 
# leftover <- fill_info |>
#   filter(is.na(ECO_REG)) # none are leftover.
# 
# 
# # add fill info and resave :) -- copy code into "Call_data_2012NLA.R"
# 
# NLA_2012_1 <- NLA_2012 |>
#   left_join(fill_info) |>
#   mutate(WGT_NLA = ifelse(is.na(WGT_NLA), WGT2, WGT_NLA),
#          ECO_REG = ifelse(is.na(ECO_REG), ECO2, ECO_REG),
#          CHLA_COND = ifelse(is.na(CHLA_COND), chcond2, CHLA_COND),
#          PTL_COND = ifelse(is.na(PTL_COND), tpcond2, PTL_COND),
#          NTL_COND = ifelse(is.na(NTL_COND), tncond2, NTL_COND),
#          TROPHIC_STATE = ifelse(is.na(TROPHIC_STATE), tstate, TROPHIC_STATE)) |>
#   select(-WGT2, -ECO2, -chcond2, -tpcond2, -tncond2, -tstate)
# 
# 
# write.csv(NLA_2012_1, "Data/NLA_2012.csv")
# 
# 
# 
# 
