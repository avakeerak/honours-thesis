library(tidyverse)

all_params_ec <- your_data_here

#assign spatial resolution resolution based on the year
all_params_ec$spatial_res <- ifelse(all_params_ec$Year <= 2019, 5, 3)

# compute equation (2)
all_params_ec$"Accuracy" <- all_params_ec$Perimeter * all_params_ec$spatial_res

# compute equation (3)
all_params_ec$vertices_sqrt <- all_params_ec$Vertices^(-1/2) # take the inverse square of the number of vertices
all_params_ec$Precision <- all_params_ec$vertices_sqrt * all_params_ec$"Accuracy" # multiply result by inverse sqrt

#compute equation (4)
all_params_ec$xi_squared <- all_params_ec$Precision^2 # square the precision (xi)

error_calc <- all_params_ec %>% # create a new df with the sums of the errors and areas for each year
  group_by(Year) %>%
  summarise(AREA_GEO_sum = sum(AREA_GEO, na.rm = TRUE),err_sum = sum(xi_squared, na.rm = TRUE), Accuracy_sum = sum(Accuracy, na.rm = TRUE))

error_calc$yearly_uncertainty <- sqrt(error_calc$err_sum) #calculate Uc(y)

# convert everything from m^2 to km^2
error_calc$AREA_GEO_km2 <- error_calc$AREA_GEO_sum / 10^6
error_calc$y_uncertainty_km2 <- error_calc$yearly_uncertainty / 10^6
error_calc$Accuracy_sum_km2 <- error_calc$Accuracy_sum / 10^6