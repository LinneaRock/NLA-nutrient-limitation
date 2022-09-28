
################################################################################
# Functions to get and plot % change in lakes in each trophic category between 2 years
################################################################################

# data is the subsetted dataframe
# tstate_var is the trophic state variable name in the dataframe, e.g. "TSTATE_CHL"
# year1 is first year of interest, e.g., "2007"
# year2 is the final year of interest, e.g., "2017


change_lakes_ts <- function(data, tstate_var, year1, year2) {
  
  change_ecoreg <- change_analysis(data |> filter(year %in% c(year1, year2)), siteID = "UNIQUE_ID", subpop = "ECO_REG_NAME", vars_cat = tstate_var, surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
  
  change_ecoreg.1 <- change_ecoreg[["catsum"]]  |>
    select(Subpopulation, Category, Indicator, DiffEst.P, StdError.P)
  
  
  change_national <- change_analysis(data |> filter(year %in% c(year1, year2)), siteID = "UNIQUE_ID", vars_cat = tstate_var, surveyID = "year", weight = "WGT_NLA", xcoord = "LON_DD", ycoord = "LAT_DD")
  
  change_national.1 <- change_national[["catsum"]] |>
    select(Subpopulation, Category, Indicator, DiffEst.P, StdError.P) |>
    mutate(Subpopulation = "National") 
  
  
  final <- rbind(change_national.1, change_ecoreg.1) |>
    mutate(Category = ifelse(startsWith(Category, "OLIGOTROPHIC"), "Oligotrophic",
                             ifelse(startsWith(Category, "MESOTROPHIC"), "Mesotrophic", 
                                    ifelse(startsWith(Category,"EUTROPHIC"), "Eutrophic",
                                           ifelse(startsWith(Category, "HYPEREUTROPHIC"), "Hypereutrophic", Category))))) |>
    mutate(Category = factor(Category, levels = c("Oligotrophic", "Mesotrophic", "Eutrophic", "Hypereutrophic"))) 
  
}


plot_change_national <- function(data, year1, year2) {
  
  ggplot(data |> filter(Subpopulation == "National")) +
    geom_point(aes(Category, DiffEst.P, fill = Category), color = "black", pch = 21, size = 3) +
    geom_errorbar(aes(Category, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = Category),   width = 0.2)  + 
    theme_light() +
    scale_fill_manual(values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1])) +
    scale_color_manual(values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1])) +
    geom_hline(yintercept = 0) +
    theme(legend.position = "none") +
    labs(x = "",
         y = paste("% Difference ", year1, ", ", year2, " Lakes", sep = ""),
         title = "National")
  
}


plot_change_regional <- function(data, year1, year2) {
  
  ggplot(data |> filter(Subpopulation != "National")) +
    geom_point(aes(Category, DiffEst.P, fill = Category), color = "black", pch = 21, size = 3) +
    geom_errorbar(aes(Category, DiffEst.P, ymin = DiffEst.P-StdError.P, ymax = DiffEst.P+StdError.P, color = Category),   width = 0.2)  +
    facet_wrap(~Subpopulation, ncol = 3) +
    theme_light() +
    scale_fill_manual(values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1])) +
    scale_color_manual(values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1])) +
    geom_hline(yintercept = 0) +
    theme(legend.position = "none") +
    labs(x = "",
         y = paste("% Difference ", year1, ", ", year2, " Lakes", sep = "")) +
    scale_x_discrete(labels = c("Olig.", "Meso.", "Eutro.", "Hyper."))
  
}

