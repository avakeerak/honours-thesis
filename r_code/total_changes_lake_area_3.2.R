# This is the R code used for section "3.2 Total Changes in Glacial Lake Area".
# It uses the uncertainties calculated in uncertainties_2.4.R (from the dataframe named error_calc)
# to create Fig. 7, a plot of the total lake area with error bars. Microsoft Excel code from B. Karchewski
# (2025) was used to create Fig. 8, a plot of the rate of change in the total lake area.

library(ggplot2)

# uses error_calc from uncertaities file

# Figure 7 (total lake area w/ error bars)
ggplot(error_calc, aes(x = Year, y = AREA_GEO_km2)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              aes(color = "2nd Order Polynomial", linetype = "2nd Order Polynomial"),
              se = FALSE, linewidth = 0.75) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3),
              aes(color = "3rd Order Polynomial", linetype = "3rd Order Polynomial"),
              se = FALSE, linewidth = 0.75) +
  geom_errorbar(aes(ymin = AREA_GEO_km2 - Accuracy_sum_km2,
                    ymax = AREA_GEO_km2 + Accuracy_sum_km2,
                    color = "Error Bars", linetype = "Error Bars"),
                size = 0.5, width = 0.2) +
  geom_errorbar(aes(ymin = AREA_GEO_km2 - y_uncertainty_km2,
                    ymax = AREA_GEO_km2 + y_uncertainty_km2,
                    color = "Error Bars", linetype = "Error Bars"),
                size = 0.75, width = 0.2) +
  scale_x_continuous(breaks = seq(min(error_calc$Year), max(error_calc$Year), by = 1), minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, 210), expand = c(0, 0)) +
  labs(x = "Year", y = "Area (km²)", color = "Legend", linetype = "Legend") +
  scale_color_manual(values = c("2nd Order Polynomial" = "turquoise3", "3rd Order Polynomial" = "turquoise4", "Error Bars" = "black")) +
  scale_linetype_manual(values = c("2nd Order Polynomial" = "dashed", "3rd Order Polynomial" = "dotted", "Error Bars" = "solid")) +
  theme_bw() + theme(text = element_text(size = 16, family = "Cambria"))

# import file with rates of change in lake area (calculated in Microsoft Excel (version 2605) using code from B. Karchewski, 2025)
total_areas_rchange <- your_data_here 

# Figure 8 (plot of rates of change in total lake areas)
ggplot(total_areas_rchange, 
       aes(x = Year, y = Rate)) +
  geom_line(size = 1, color = "turquoise4") +
  labs(x = "Year", y = "Rate of Change (km²/yr)") +
  scale_x_continuous(breaks = unique(total_areas_rchange$Year)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_bw() + theme(text = element_text(size = 16, family = "Cambria"))
