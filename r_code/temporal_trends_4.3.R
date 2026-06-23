library(ggplot2)

# import df with mean, median, and standard deviation in the rate of change in the
# total lake area for each connection type (calculated using Microsoft Excel)
connection_rates_stats <- your_data_here

# Figure 18 (plot of the mean and median rates of change in total glacial lake 
# area and the standard deviation (SD) for each connection type)
ggplot(connection_rates_stats, aes(x = Connection)) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, color = "gray40") +
  geom_point(aes(y = Mean, shape = "Mean"), size = 3, color = "olivedrab") +
  geom_point(aes(y = Median, shape = "Median"), size = 3, color = "palevioletred") +
  scale_shape_manual(name = "Legend", values = c("Mean" = 16, "Median" = 17)) +
  labs(x = "Connection Type", y = "Rate of Change (Km²/Year)") +
  theme_bw(base_size = 14) + theme(text = element_text(size = 16, family = "Cambria"))

# import df with mean, median, and standard deviation in the rate of change in the
# total lake area for each dam type (calculated using Microsoft Excel)
dam_rates_stats <- your_data_here

# Figure 19 (plot of the mean and median rates of change in total glacial lake 
# area and the standard deviation (SD) for each dam type)
ggplot(dam_rates_stats, aes(x = Dam)) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, color = "gray40") +
  geom_point(aes(y = Mean, shape = "Mean"), size = 3, color = "olivedrab") +
  geom_point(aes(y = Median, shape = "Median"), size = 3, color = "palevioletred") +
  scale_shape_manual(name = "Legend", values = c("Mean" = 16, "Median" = 17)) +
  labs(x = "Dam type", y = "Rate of Change (Km²/Year)") +
  theme_bw(base_size = 14) + theme(text = element_text(size = 16, family = "Cambria")) 

