# This is the R code for section "4.5 Potential Hazards". It was used to calculated the percent
# change in visible area from the previous year for each lake and plot the number of lakes with
# a percent change < -50% by dam type (Fig. 20). The data was filtered to exclude lakes
# with a percent change >= 50% using Microsoft Excel (version 2605).

library(dplyr)
library(ggplot2)
library(writexl)

#import df containing list of all lakes and their year
bufferedlakes <- your_data_here

# calculate the percent change in area from the previous year for each lake
area_pct_change <- bufferedlakes %>%
  group_by(LakeID) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(prev_area = lag(AREA_GEO), pct_change = (AREA_GEO - prev_area) / prev_area * 100) %>%
  ungroup()

# save the df with % changes as xlsx to be looked at and filtered in Microsoft Excel
write_xlsx(area_pct_change, "your_directory_here/area_pct_change.xlsx")

# import filtered df containing only lakes with % changes in area < -50%. Filtered using Microsoft Excel 
area_pct_change_filtered <- read_excel("your_directory_here/area_pct_change_filtered.xlsx")

# count number of lakes with % changes < -50% for each dam type for each year
area_pct_change_filtered <- area_pct_change_filtered %>%
  count(Dam_Type, Year) 

# Figure 20 (bar plots with the number of lakes with % changes in area < -50% for each dam type for each year)
ggplot(area_pct_change_filtered, aes(x = as.factor(Year), y = n, fill = Dam_Type)) +
  geom_col(color = "black") + 
  facet_wrap(~ Dam_Type, scales = "free") +
  scale_x_discrete(limits = c("2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024")) +
  labs(x = "Year", y = "Number of Lakes that Drained Over 50%", fill = "Dam Type") +
  theme_bw(base_family = "Cambria") +
  theme(strip.text = element_text(family = "Cambria"), axis.text.x = element_text(family = "Cambria", color = "black")) + 
  theme(legend.position = "none")
