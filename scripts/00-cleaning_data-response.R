#### Preamble ####
# Purpose: Obtain average shortest distance to hawker from MRT per region
# Subgoal: save cleaned mrt, planning region and hawker geojson
# Author: Chris Yong Hong Sen 
# Date: 02 December 2024
# Contact:luke.yong@mail.utoronto.ca 
# License: MIT
# Pre-requisites: 
# - The `tidyverse` package must be installed and loaded
# - The `sf` package must be installed and loaded

### setup ###
library(sf)

### read data ###

# an MRT_id (name), its description (description) and point data (geometry)
raw_data <- st_read("../data/00-raw_data/mrt_exit_data.geojson") |>
  janitor::clean_names()

# a hawker's region (name) and it's point data (geomtry)
hawker_data <- st_read("../data/00-raw_data/HawkerCentresGEOJSON.geojson") |>
  janitor::clean_names() 

# singapore planning regions
planning_region_data <- st_read("../data/00-raw_data/MasterPlan2019PlanningAreaBoundaryNoSea.geojson") |>
  janitor::clean_names()


### clean data ###
##################
# goal: extract get useful data from planning region's description column
#
#
###################
planning_region_data_clean <- planning_region_data |>
  mutate(
    # extract full station name
    region = str_extract(description, "(?<=<th>PLN_AREA_N</th> <td>)(.*?)(?=</td>)"),
    
    # obtain exit name
    area =  str_extract(description, "(?<=<th>REGION_N</th> <td>)(.*?)(?=</td>)")) |>
  select(region, area, geometry) |>
  arrange(area, region)


##################
# goal: extract get useful data from mrt's description column
#
#
###################
mrt_data <- raw_data |>
  mutate(
    # extract full station name
    station_name = str_extract(description, "(?<=<th>STATION_NA</th> <td>)(.*?)(?=</td>)"),
         
    # shorten station name by removing 'mrt'
    station_name = str_to_lower(str_remove(station_name, ' MRT STATION')),
    
    # obtain exit name
    exit_code =  str_extract(description, "(?<=<th>EXIT_CODE</th> <td>)(.*?)(?=</td>)"),
    
    # shorten exit code by removing 'exit'
    exit_code = str_to_lower(str_remove(exit_code, 'Exit '))) |>
  select(station_name, exit_code, geometry) |>
  arrange(station_name, exit_code) |>
  st_make_valid() |>
  st_join(st_make_valid(planning_region_data_clean), join=st_within)

##################
# goal: extract get useful data from hawker's description column
#
#
###################
hawker_data_clean <- hawker_data |>
  mutate(
    # extract full hawker name
    hawker_name = str_extract(description, "(?<=<th>NAME</th> <td>)(.*?)(?=</td>)"),
    hawker_name = str_to_lower(hawker_name)) |>
  select(hawker_name, geometry) |>
  arrange(hawker_name) |>
  st_make_valid() |>
  st_join(st_make_valid(planning_region_data_clean), join=st_within)

##################
# goal: obtain average shortest distance to hawker from mrt exits per region
#
#
###################
# find nearest hawker from each mrt
nearest_indexes <- st_nearest_feature(mrt_data, hawker_data_clean)
nearest_hawkers <- hawker_data_clean[nearest_indexes, ]

distance_to_hawker_from_mrt_data <- mrt_data |>
  mutate(hawker_name = nearest_hawkers$hawker_name,
         
         distance_matrix = st_distance(geometry, nearest_hawkers$geometry),
         distance_to_hawker = map_dbl(1:nrow(distance_matrix), 
                                              ~ min(distance_matrix[., ])),
         .before=geometry) |>
  select(-distance_matrix)

##################
# goal: choose shortest path from each station 
#
# motivation: a station could have multiple exits we only consider the exit that
#             has the shortest distance to the nearest hawker 
###################
distance_mrt_to_hawker_clean <- distance_mrt_to_hawker_clean |>
  group_by(region, station_name) |>
  filter(distance_to_hawker == min(distance_to_hawker)) |>
  ungroup()


##################
# goal: obtain the average shortest distance from mrt to hawker per region. This
#       would be our response column
#
# 
###################
clean_response_data <- distance_mrt_to_hawker_clean |>
  st_drop_geometry() |>
  as_tibble() |>
  group_by(region) |>
  summarise(avg_distance_to_hawker = mean(distance_to_hawker)) 

### save data ###
write_csv(clean_response_data, 
         "../data/01-analysis_data/csv/distance_mrt_to_hawker.csv")

st_write(planning_region_data_clean,
         "../data/01-analysis_data/shape/planning_region_clean.geojson",
         driver='GeoJSON')

st_write(mrt_data,
         "../data/01-analysis_data/shape/mrt_clean.geojson",
         driver='GeoJSON')

st_write(hawker_data_clean,
         "../data/01-analysis_data/shape/hawker_clean.geojson",
         driver='GeoJSON')
