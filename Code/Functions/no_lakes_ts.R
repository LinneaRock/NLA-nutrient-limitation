
################################################################################
#####Function to get # Lakes in each ecoregion and trophic state and year#######
################################################################################

# data is the subsetted dataframe
# tstate_var is the trophic state variable name in the dataframe


no_lakes_ts <- function(data, tstate_var) {

# 2007
x2007_ecoreg <- cat_analysis(data |> filter(year == 2007), subpop = "ECO_REG_NAME", vars = tstate_var, weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")

x2007_ecoreg.1 <- x2007_ecoreg |>
  select(Subpopulation, Category, nResp, Estimate.P, StdError.P) |>
  mutate(year = 2007)

# 2012
x2012_ecoreg <- cat_analysis(data |> filter(year == 2012), subpop = "ECO_REG_NAME", vars = tstate_var, weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")

x2012_ecoreg.1 <- x2012_ecoreg |>
  select(Subpopulation, Category, nResp, Estimate.P, StdError.P) |>
  mutate(year = 2012)

# 2017
x2017_ecoreg <- cat_analysis(data |> filter(year == 2017), subpop = "ECO_REG_NAME", vars = tstate_var, weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")

x2017_ecoreg.1 <- x2017_ecoreg |>
  select(Subpopulation, Category, nResp, Estimate.P, StdError.P) |>
  mutate(year = 2017)

national <- cat_analysis(data, vars = tstate_var, subpop = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")

national.1 <- national|>
  select(Subpopulation, Category, nResp, Estimate.P, StdError.P) |>
  mutate(year = Subpopulation,
         Subpopulation = "National")

final <- rbind(national.1, x2007_ecoreg.1, x2012_ecoreg.1, x2017_ecoreg.1)

}



