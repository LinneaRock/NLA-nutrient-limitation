nreltest <- nla_data_subset |>
  select(ECO_REG_NAME, UNIQUE_ID, year, NTL_PPM, DIN_PPM, CHLA_PPB) |>
  mutate(DON = NTL_PPM - DIN_PPM) |>
  filter(DON > 0,
         !is.na(DON))

ggplot(nreltest) +
  geom_point(aes(DIN_PPM, CHLA_PPB)) +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth(aes(DON, CHLA_PPB))
