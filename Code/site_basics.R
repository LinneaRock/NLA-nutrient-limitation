
# Number of lakes in each sampling year 

source("Data/NLA/Call_NLA_data.R")

#how many sites in NLA 2007
unique_2007 <- NLA07 |>
  select(UNIQUE_ID, DATE_COL, VISIT_NO) |>
  distinct()

nrow(unique_2007 |> filter(VISIT_NO == 1) |> select(UNIQUE_ID) |> distinct()) # 1156 lakes surveyed
nrow(unique_2007 |> filter(VISIT_NO == 2)  |> select(UNIQUE_ID) |> distinct()) # 95 lakes resampled

nrow(NLA07 |> filter(REFERENCE == "Y") |> select(UNIQUE_ID) |> distinct())# 70 reference lakes

#how many sites in NLA 2012
unique_2012 <- NLA12 |>
  select(UNIQUE_ID, DATE_COL, VISIT_NO) |>
  distinct()

nrow(unique_2012 |> filter(VISIT_NO == 1)  |> select(UNIQUE_ID) |> distinct()) # 1038 lakes surveyed
nrow(unique_2012 |> filter(VISIT_NO == 2)  |> select(UNIQUE_ID) |> distinct()) # 100 lakes resampled

nrow(NLA12 |> filter(REFERENCE == "Y") |> select(UNIQUE_ID) |> distinct()) # 182 reference lakes


#how many sites in NLA 2017
unique_2017 <- NLA17 |>
  select(UNIQUE_ID, DATE_COL, VISIT_NO) |>
  distinct()

nrow(unique_2017 |> filter(VISIT_NO == 1)  |> select(UNIQUE_ID) |> distinct()) # 1112 lakes surveyed
nrow(unique_2017 |> filter(VISIT_NO == 2)  |> select(UNIQUE_ID) |> distinct()) # 97 lakes resampled

nrow(NLA17 |> filter(REFERENCE == "Y") |> select(UNIQUE_ID) |> distinct()) # 214 reference lakes


crossover2007_2012 <- unique_2007 |>
  select(UNIQUE_ID) |>
  distinct() |>
  inner_join(unique_2012 |> select(UNIQUE_ID) |>
               distinct()) |> distinct() # 364

crossover2012_2017 <- unique_2012 |>
  select(UNIQUE_ID) |>
  distinct() |>
  inner_join(unique_2017 |> select(UNIQUE_ID) |>
               distinct()) |> distinct() # 473

crossover2007_2017 <- unique_2007 |>
  select(UNIQUE_ID) |>
  distinct() |>
  inner_join(unique_2017 |> select(UNIQUE_ID) |>
               distinct()) |> distinct() # 282

crossover2007_2012_2017 <- crossover2007_2012|>
  inner_join(unique_2017 |> select(UNIQUE_ID) |>
               distinct()) |> distinct() # 234


mean(all_NLA$tn.tp) # 59.48564
sd(all_NLA$tn.tp) # 113.4884
