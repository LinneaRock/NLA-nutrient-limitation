

# create CDF of TN in each ecoregion

plot(ecdf((nla_data_subset |> filter(ECO_REG_NAME =='Upper Midwest' &
                                     CHLA_COND %in% c('3:MOST DISTURBED', 'Poor')))$PTL_PPB/1000))


m1 <- lm(log(CHLA_PPB) ~ ECO_REG_NAME * log(NTL_PPM) * log(PTL_PPB), data = nla_data_subset)
summary(m1)
anova(m1)
coef(m1)
