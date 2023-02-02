# Script to create dark figures for presentation - need all data and analyses run first from NARS_challenge_data_analysis.R
library(ggdark)

ggplot(correlations_data, aes(log10(concentration), log10(CHLA_PPB))) +
  geom_point(alpha = 0.25, aes(color = nutrient)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = nutrient), color = "grey60") +
  geom_text(add_corr |> filter(nutrient == "NTL_PPB"), 
            mapping = aes(x = -5, y = 3.5, label = r.squared), size = 2, vjust = 0.75, hjust = 0) +
  geom_text(add_corr |> filter(nutrient == "NTL_PPB"),
            mapping = aes(x = -5, y = 3, label = AIC), size = 2, vjust = 0.75, hjust = 0) +
  geom_text(add_corr |> filter(nutrient == "PTL_PPB"), 
            mapping = aes(x = 1, y = 3.5, label = r.squared), size = 2, vjust = 0.75, hjust = 0) +
  geom_text(add_corr |> filter(nutrient == "PTL_PPB"), 
            mapping = aes(x = 1, y = 3, label = AIC), size = 2, vjust = 0.75, hjust = 0) +
  dark_theme_bw() +
  geom_abline(slope = 0, intercept = log10(2), color = 'white') +
  geom_abline(slope = 0, intercept = log10(7), color = 'white') +
  geom_abline(slope = 0, intercept = log10(30), color = 'white') +
  geom_vline(xintercept = 0) +
  facet_wrap(~nutrient, scales = "free_x") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  facet_wrap(~ECO_REG_NAME, ncol = 3) +
  labs(y = "log10(chlorophyll-a concentration)",
       x = "log10(nutrient concentration)") +
  scale_color_manual("", labels = c("TN", "TP"), values=c("red4", "#336a98")) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"))
ggsave("Figures/BW_presentationfigs/linregs.png", height=4.5, width=6.5, units='in', dpi=1200)



ggplot(data = regions.sf1) +
  geom_sf(aes(fill = best_predictor)) +
  dark_theme_bw() +
  geom_sf_label(aes(label = WSA9_NAME), size = 2.5) +
  labs(x = "", y = "") +
  scale_fill_manual("", values=c("red4", "#336a98")) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif")) +
  theme(legend.position = c(0.1, 0.2)) 
ggsave("Figures/BW_presentationfigs/map.png", height=4.5, width=6.5, units='in', dpi=1200)



ggplot(limits) +
  geom_point(aes(log(PTL_PPB, base = 10), log(DIN_PPM, base = 10), fill = limitation), size = 2.5, shape = 21, alpha = 0.8) +
  dark_theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_fill_manual("",values = c("grey60","red4", "#336a98")) +
  labs(y = "Log DIN"~(m*g~L^-1), x = "Log TP"~(mu*g~L^-1)) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"),
        legend.position = c(0.15, 0.85))
ggsave("Figures/BW_presentationfigs/lim_calc.png", height = 4.5, width = 6.5, units = "in", dpi = 1200) 




ggplot(lim_changes_fullset |>
                        filter(Subpopulation != "National",
                               sample_set == 'All surveyed lakes')) +
  geom_point(aes(Category,DiffEst.P, fill = Category), color = "white", pch = 21, size = 1, 
             position=position_dodge(width=0.5))+
  geom_errorbar(aes(Category, DiffEst.P, ymin = DiffEst.P-StdError.P, 
                    ymax = DiffEst.P+StdError.P, color = Category, linetype = sample_set), width = 0.2)  + 
  dark_theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  facet_wrap(~Subpopulation, ncol = 3, scales = "free_y") +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% change 2007-2017") +
  scale_color_manual("",values = c("grey60","red4", "#336a98"))  +
  scale_fill_manual("",values = c("grey60","red4", "#336a98")) +
  theme(axis.text.x = element_text(angle = 49, vjust = 1, hjust =1),
    strip.text.x = element_text(size = 7.5),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, family = "serif"),
    legend.position = 'none')
ggsave("Figures/BW_presentationfigs/lim_shifts_ecoreg.png", height = 6, width = 6.5, units = "in", dpi = 1200) 






#national plot
ggplot(lim_changes_fullset |>
                     filter(Subpopulation == "National",
                            sample_set == 'All surveyed lakes')) +
  geom_point(aes(Category,DiffEst.P, fill = Category), 
             color = "white", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Category, DiffEst.P, ymin = DiffEst.P-StdError.P,
                    ymax = DiffEst.P+StdError.P, color = Category, linetype = sample_set), width = 0.2)  + 
  dark_theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% change 2007-2017",
       title = "National") +
  scale_color_manual("",values = c("grey60","red4", "#336a98"))  +
  scale_fill_manual("",values = c("grey60","red4", "#336a98")) +
  theme(legend.title = element_blank())
ggsave("Figures/BW_presentationfigs/lim_shifts_national.png", height = 4, width = 6.5, units = "in", dpi = 1200) 





ggplot(percent_lim1, aes(year, Estimate.P, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(text_percents, mapping = aes(label = perc), position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(~Subpopulation) +
  scale_fill_manual("", values = c("grey60","red4", "#336a98")) +
  dark_theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 6.5)) +
  labs(x = "", y = "% lakes") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"),
        legend.position = 'none')
ggsave("Figures/BW_presentationfigs/national_limbar.png", height = 4, width = 6.5, units = "in", dpi = 1200) 




ggplot(TS_changes_fullset |> filter(sample_set == 'All surveyed lakes')) +
  geom_point(aes(Category,DiffEst.P, fill = Category), 
             color = "white", pch = 21, size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(Category, DiffEst.P, ymin = DiffEst.P-StdError.P, 
                    ymax = DiffEst.P+StdError.P, color = Category, linetype = sample_set), width = 0.2)  + 
  dark_theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  facet_wrap(~Subpopulation, scales = "free_y", ncol = 2) +
  geom_hline(yintercept = 0) +
  labs(x = "",
       y = "% change 2007-2017")+                                                 
  scale_color_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1]))  +
  scale_fill_manual("", values = c(palette_OkabeIto[2], palette_OkabeIto[4], palette_OkabeIto[3], palette_OkabeIto[1])) +
  theme(axis.text.x = element_text(angle = 49, vjust = 1, hjust =1),
        strip.text.x = element_text(size = 7.5),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, family = "serif"),
        legend.title = element_blank())
ggsave("Figures/BW_presentationfigs/trophicstate.png", height = 4.5, width = 6.5, units = "in", dpi = 1200) 


