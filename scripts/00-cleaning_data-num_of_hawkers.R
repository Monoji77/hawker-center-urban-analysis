#### Preamble ####
# Purpose: Obtain average number of hawker stalls per region
# Author: Chris Yong Hong Sen 
# Date: 02 December 2024
# Contact:luke.yong@mail.utoronto.ca 
# License: MIT
# Pre-requisites: 
# - The `sf` package must be installed and loaded
# - 00-clean_data-response.R must have been run

### setup ###
library(sf)
hawker_data <- st_read("../data/01-analysis_data/shape/hawker_clean.geojson")


### clean data ###
num_of_hawker_data <- hawker_data |>
  st_drop_geometry() |>
  as_tibble() |>
  count(region, name='num_of_hawker')


### save data ###
write_csv(num_of_hawker_data,
          "../data/01-analysis_data/csv/num_of_hawkers.csv")
