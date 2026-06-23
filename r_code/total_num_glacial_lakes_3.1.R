library(dplyr)
library(extrafont)
library(ggplot2)

#import df containing list of all lakes and their year
bufferedlakes <- your_data_here

# count number of lakes for each year
lake_count <- bufferedlakes %>%
  group_by(Year) %>%
  summarise(n_entries = n())

# Figure 5 (barplot of lake counts)
ggplot(lake_count, aes(x = as.factor(Year), y = n_entries)) + 
  geom_col(fill = "turquoise3", color = "turquoise4") + 
  labs(x = "Year", y = "Number of Lakes") +
  theme_bw() + theme(text = element_text(size = 16, family = "Cambria"))

# import file with rates of change in lake counts (calculated in Microsoft Excel using code from B. Karchewski, 2025)
lake_count_rchange <- your_data_here

# Figure 6 (plot of rate of change in lake counts)
ggplot(lake_count_rchange, 
       aes(x = Year, y = Rate)) +
  geom_line(size = 1, color = "turquoise4") +
  labs(x = "Year", y = "Rate of Change (Number of Lakes per Year)") +
  scale_x_continuous(breaks = unique(lake_count_rchange$Year)) +
  theme_bw() + theme(text = element_text(size = 16, family = "Cambria"))