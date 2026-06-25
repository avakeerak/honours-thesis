# This is the R code corresponding to section "3.4 Regional Trends". It was used to create Figures 14-17,
# which show the total lake counts or the total lake area for each region, colour-coded by either
# connection type or dam type. There is a lot of repeated code, and I may simplify everything into
# a single function in the future.

library(dplyr)
library(ggplot2)
library(extrafont)

# import df containing lake data from 2016 and 2024 only
lakes_2016and2024_only <- your_data_here

# convert connection type names from numbers to actual names
lakes_2016and2024_only$Connection <- factor(lakes_2016and2024_only$Connection,
                                                levels = c("1","2","3","4"),
                                                labels = c("Detached","Proglacial","Marginal","Supraglacial"))

# count the total number of lakes for each region to be used as labels in Figs. 14 and 15. It is independent of connection and dam type.
total_region_counts <- lakes_2016and2024_only %>%
  group_by(Name, Year) %>%
  summarise(n_entries = n(), .groups = "drop")

# count the number of lakes for each connection type in each region for the years 2016 and 2024
connection_lake_count <- lakes_2016and2024_only %>%
  group_by(Name, Connection, Year) %>%
  summarise(n_entries = n()) %>%
  ungroup()

# Figure 14 (bar plots of the total number of glacial lakes and their respective connection types for each region)
ggplot(connection_lake_count, aes(x = Year, y = n_entries, fill = as.factor(Connection))) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.01) +
  labs(x = "Year", y = "Total Number of Lakes", fill = "Connection") +
  scale_fill_manual(values = c("Detached" = "plum3", "Proglacial" = "lightgreen", "Marginal" = "palevioletred2", "Supraglacial" = "deepskyblue2")) +
  scale_x_continuous(breaks = c(2016, 2024)) +
  theme_bw(base_family = "Cambria") +
  theme(axis.title.x = element_text()) + 
  geom_text(data = total_region_counts, aes(x = Year, y = n_entries + 25, label = round(n_entries, 1)),
    inherit.aes = FALSE, size = 3, family = "Cambria") + 
  theme(axis.text.x = element_text(family = "Cambria", color = "black")) +
  facet_wrap_paginate( ~ Name, ncol = 5, nrow = 3, page = 1,       
    scales = "free_x", labeller = label_wrap_gen(width = 18)) +
  theme(strip.text = element_text(size = 10, family = "Cambria", lineheight = 0.9),
        strip.text.x = element_text(margin = margin(t = 3, b = 3)),
        plot.margin = margin(t = 14, r = 8, b = 8, l = 8),
        legend.position = c(0.85, 0.05),
        legend.justification = c(0, 0),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(family = "Cambria"),
        legend.title = element_text(family = "Cambria"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0))

# count the number of lakes for each dam type in each region for the years 2016 and 2024
damtype_lake_count <- lakes_2016and2024_only %>%
  group_by(Name, DamType, Year) %>%
  summarise(n_entries = n()) %>%
  ungroup()

# Figure 15 (bar plots of the total number of glacial lakes and their respective dam types for each region)
ggplot(damtype_lake_count, aes(x = Year, y = n_entries, fill = as.factor(DamType))) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.01) +
  labs(x = "Year", y = "Total Number of Lakes", fill = "Dam Type") +
  scale_fill_manual(values = c("Bedrock" = "mediumpurple", "Moraine" = "olivedrab3", "Landslide" = "lightpink", "Ice" = "darkslategray2")) +
  scale_x_continuous(breaks = c(2016, 2024)) +
  theme_bw(base_family = "Cambria") +
  theme(axis.title.x = element_text()) + 
  geom_text(data = total_region_counts, aes(x = Year, y = n_entries + 25, label = round(n_entries, 1)),
    inherit.aes = FALSE, size = 3, family = "Cambria") + 
  theme(axis.text.x = element_text(family = "Cambria", color = "black")) +
  facet_wrap_paginate(~ Name, ncol = 5, nrow = 3, page = 1,       
    scales = "free_x", labeller = label_wrap_gen(width = 18)) +
  theme(strip.text = element_text(size = 10, family = "Cambria", lineheight = 0.9),
        strip.text.x = element_text(margin = margin(t = 3, b = 3)),
        plot.margin = margin(t = 14, r = 8, b = 8, l = 8),
        legend.position = c(0.85, 0.05),
        legend.justification = c(0, 0),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(family = "Cambria"),
        legend.title = element_text(family = "Cambria"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0))

# sum the total area of each connection type for each region for both 2016 and 2024
connection_type_aggregated <- lakes_2016and2024_only %>%
  group_by(Name, Connection, Year) %>%
  summarise(Total_AREA_GEO = sum(AREA_GEO), .groups = "drop") %>%
  ungroup()

# calculate the total lake area for each region to be used as labels in Figs. 16 and 17. It is independent of connection and dam type.
total_region_areas <- connection_type_aggregated %>%
  group_by(Name, Year) %>%
  summarise(Total = sum(Total_AREA_GEO), .groups = "drop")

# Figure 16 (bar plots with total glacial lake area with the area contributed by each connection type for each region)
ggplot(connection_type_aggregated, aes(x = Year, y = Total_AREA_GEO, fill = as.factor(Connection))) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.01) +
  labs(x = "Year", y = "Total Area (km²)", fill = "Connection") +
  scale_fill_manual(values = c("Detached" = "plum3", "Proglacial" = "lightgreen", "Marginal" = "palevioletred2", "Supraglacial" = "deepskyblue2")) +
  scale_x_continuous(breaks = c(2016, 2024)) +
  theme_bw(base_family = "Cambria") +
  theme(axis.title.x = element_text()) + 
  geom_text(data = total_region_areas, aes(x = Year, y = Total + 6, label = round(Total, 1)),
    inherit.aes = FALSE, size = 3, family = "Cambria") + 
  theme(axis.text.x = element_text(family = "Cambria", color = "black")) +
  facet_wrap_paginate( ~ Name, ncol = 5, nrow = 3, page = 1,       
    scales = "free_x", labeller = label_wrap_gen(width = 18)) +
  theme(strip.text = element_text(size = 10, family = "Cambria", lineheight = 0.9),
        strip.text.x = element_text(margin = margin(t = 3, b = 3)),
        plot.margin = margin(t = 14, r = 8, b = 8, l = 8),
        legend.position = c(0.85, 0.05),
        legend.justification = c(0, 0),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(family = "Cambria"),
        legend.title = element_text(family = "Cambria"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0))    

# sum the total area of each dam type for each region for both 2016 and 2024
damtype_aggregated <- lakes_2016and2024_only %>%
  group_by(Name, DamType, Year) %>%
  summarise(Total_AREA_GEO = sum(AREA_GEO), .groups = "drop") %>%
  ungroup()

# Figure 17 (bar plots with total glacial lake area with the area contributed by each dam type for each region)
ggplot(damtype_aggregated, aes(x = Year, y = Total_AREA_GEO, fill = as.factor(DamType))) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.01) +
  labs(x = "Year", y = "Total Area (km²)", fill = "Dam Type") +
  scale_fill_manual(values = c("Bedrock" = "mediumpurple", "Moraine" = "olivedrab3", "Landslide" = "lightpink", "Ice" = "darkslategray2")) +
  scale_x_continuous(breaks = c(2016, 2024)) +
  theme_bw(base_family = "Cambria") +
  theme(axis.title.x = element_text()) + 
  geom_text(data = total_region_areas, aes(x = Year, y = Total + 6, label = round(Total, 1)),
    inherit.aes = FALSE, size = 3, family = "Cambria") + 
  theme(axis.text.x = element_text(family = "Cambria", color = "black")) +
  facet_wrap_paginate(~ Name, ncol = 5, nrow = 3, page = 1,         
    scales = "free_x", labeller = label_wrap_gen(width = 18)) +
  theme(strip.text = element_text(size = 10, family = "Cambria", lineheight = 0.9),
        strip.text.x = element_text(margin = margin(t = 3, b = 3)),
        plot.margin = margin(t = 14, r = 8, b = 8, l = 8),
        legend.position = c(0.85, 0.05),
        legend.justification = c(0, 0),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(family = "Cambria"),
        legend.title = element_text(family = "Cambria"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0))
