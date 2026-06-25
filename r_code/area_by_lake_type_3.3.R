# This is the R code for section "3.3 Changes in Glacial Lake Area by Lake Type".
# It was used to plot the total lake area for each year by connection type 
# (Fig. 9) and dam type (Fig. 11). Microsfot Excel code from B. Karchewski (2025)
# was used to find the rate of change in the total lake area and the rates of 
# change were plotted for each connection type (Fig. 10) and dam type (Fig. 12).

library(dplyr)
library(extrafont)
library(ggplot2)

# import data of all lakes w/in 2km buffer
bufferedlakes <- your_data_here

# import file with rates of change in lake area by connection type (calculated in Microsoft Excel (version 2605) using code from B. Karchewski, 2025)
c_rate_of_change <- your_data_here

# sum lake area by connection type and year, then convert to km2
connection_sums <- bufferedlakes %>%
  group_by(Connection, Year) %>%
  summarise(sum_areas = sum(AREA_GEO, na.rm = TRUE), .groups = 'drop')
connection_sums$"sum_areas_km^2" <- connection_sums$sum_areas / 10^6

# convert connection type names from numbers to actual names
connection_sums$Connection <- factor(connection_sums$Connection,
                                     levels = c("1","2","3","4"),
                                     labels = c("Detached","Proglacial","Marginal","Supraglacial"))

# Figure 9 (total lake area by connection type)
ggplot(connection_sums, 
       aes(x = Year, y = `sum_areas_km^2`, color = as.factor(Connection))) +
  geom_line(size = 0.75) +
  scale_color_manual(values = c("Detached" = "plum3", "Proglacial" = "lightgreen", "Marginal" = "palevioletred2", "Supraglacial" = "deepskyblue2")) + 
  labs(x = "Year", y = "Total Area (km²)", color = "Connection") +
  theme_bw() + theme(text = element_text(size = 16, family = "Cambria")) + facet_wrap(~ Connection, scales = "free")

# Figure 10 (rate of change in total lake area by connection type)
ggplot(c_rate_of_change, 
       aes(x = Year, y = Rate, color = as.factor(Connection))) +
  geom_line(size = 0.75) +
  scale_color_manual(values = c("Detached" = "plum3", "Proglacial" = "lightgreen", "Marginal" = "palevioletred2", "Supraglacial" = "deepskyblue2")) + 
  labs(x = "Year", y = "Rate of Change (km²/yr)", color = "Connection") +
  theme_bw() + theme(text = element_text(size = 16, family = "Cambria")) + facet_wrap(~ Connection, scales = "free")

# import file with rates of change in lake area by dam type (calculated in Microsoft Excel using code from B. Karchewski, 2025)
d_rate_of_change <- your_data_here

# sum lake area by dam type and year, then convert to km2
dam_sums <- bufferedlakes %>%
  group_by(Dam_Type, Year) %>%
  summarise(sum_areas = sum(AREA_GEO, na.rm = TRUE), .groups = 'drop')
dam_sums$"sum_areas_km^2" <- dam_sums$sum_areas / 10^6

# Figure 11 (total lake area by dam type)
ggplot(dam_sums, aes(x = Year, y = `sum_areas_km^2`, color = as.factor(Dam_Type))) +
  geom_line(size = 0.75) +
  scale_color_manual(values = c("Moraine" = "olivedrab3", "Bedrock" = "mediumpurple", "Landslide" = "lightpink3", "Ice" = "darkslategray3")) + 
  labs(x = "Year", y = "Total Area (km²)", color = "Dam Type") +
  theme_bw() + theme(text = element_text(size = 16, family = "Cambria")) + facet_wrap(~ Dam_Type, scales = "free")

# Figure 12 (rate of change in total lake area by dam type)
ggplot(d_rate_of_change, 
       aes(x = Year, y = Rate, color = as.factor(DamType))) +
  geom_line(size = 0.75) +
  scale_color_manual(values = c("Moraine" = "olivedrab3", "Bedrock" = "mediumpurple", "Landslide" = "lightpink3", "Ice" = "darkslategray3")) + 
  labs(x = "Year", y = "Rate of Change (km²/yr)", color = "Dam Type") +
  theme_bw() + theme(text = element_text(size = 16, family = "Cambria")) + facet_wrap(~ DamType, scales = "free")

