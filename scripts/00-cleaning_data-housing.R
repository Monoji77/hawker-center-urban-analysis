#### Preamble ####
# Purpose: Downloads and saves the data from Open Map
# Author: Chris Yong Hong Sen
# Date: 13 Nov 2024
# Contact: luke.yong@mail.utoronto.ca
# License: MIT
# Pre-requisites: Knowledge in tmap, sf and tidyverse packages


#### Workspace setup #### 
library(tidyverse)
library(sf)
library(tmap)
library(httr2)

#### Download data ####
# follow instructions to get the access token from onemap
# I have hidden my access token for security reasons
access_token_onemap <- "insert token here"

# obtain 55 area names in singapore
get_regions <- function() {
  base_url_onemap <- "https://www.onemap.gov.sg/api/public/popapi/getAllPlanningarea?year=2019"
  req_obj <- request(base_url_onemap)
  req_header <- req_obj |>
    req_headers(Authorization = access_token_onemap)
  
  response <- req_header |>
    req_perform()
  
  results_onemap <- response |>
    resp_body_json()
  return (results_onemap)
}
region_names_json <- get_regions()

# given area names, convert every space to %20 to prepare for next API call
area_names <- c()
for (i in seq_along(region_names_json)) {
  current_name <- region_names_json[[i]]$pln_area_n
  clean_name <- gsub(" ", '%20',current_name)
  area_names <- c(area_names, clean_name)
}

area_names

# get all 55 planning areas in singapore
get_planning_area_polygons <- function() {
  base_url_onemap <- "https://www.onemap.gov.sg/api/public/popapi/getAllPlanningarea?year=2019"
  req_obj <- request(base_url_onemap)
  
  req_header <- req_obj |>
    req_headers(Authorization = access_token_onemap)
  
  response <- req_header |>
    req_perform()
  
  results_onemap <- response |>
    resp_body_json()
  
  return(results_onemap)
}

results <- get_planning_area_polygons()
temp_file <- tempfile(fileext = '.geojson')
writeLines(results$SearchResults[[1]]$geojson, temp_file)

sf_data <- st_read(temp_file)
unlink(temp_file)

# Initialize an empty list to hold sf objects
sf_list <- list()

for (i in seq_along(results$SearchResults)) {
  geojson_string <- results$SearchResults[[i]]$geojson
  pln_area_name <- results$SearchResults[[i]]$pln_area_n
  
  # Write the geojson string to a temporary file
  temp_geojson <- tempfile(fileext = ".geojson")
  writeLines(geojson_string, temp_geojson)
  
  # Read it into sf object
  sf_obj <- st_read(temp_geojson, quiet = TRUE)
  
  # Add the planning area name as a column
  sf_obj$pln_area_n <- pln_area_name
  
  # Store in list
  sf_list[[i]] <- sf_obj
  
  # Clean up temp file
  unlink(temp_geojson)
}

# Combine all sf objects into one
sf_data <- do.call(rbind, sf_list) 



df <- as_tibble(list(
  planning_area = '',
  year = 0,
  hdb_onetwo_room = 0,
  hdb_three_room = 0,
  hdb_four_room = 0,
  hdb_five_room = 0,
  condo = 0,
  landed = 0,
  other = 0,
  total_hdb = 0,
  total_housing = 0))[0,]

for (region in area_names) {
  base_url_onemap <- paste0("https://www.onemap.gov.sg/api/public/popapi/getTypeOfDwellingPop?planningArea=",region, "&year=2020")
  
  req_obj <- request(base_url_onemap)
  
  req_header <- req_obj |>
    req_headers(Authorization = access_token_onemap)
  
  response <- req_header |>
    req_perform()
  
  results_onemap <- response |>
    resp_body_json()
  
  df <- df |>
    add_row(
      tibble_row(
        planning_area = results_onemap[[1]]$planning_area,
        year = results_onemap[[1]]$year,
        hdb_onetwo_room = results_onemap[[1]]$hdb_1_and_2_room_flats,
        hdb_three_room = results_onemap[[1]]$hdb_3_room_flats,
        hdb_four_room = results_onemap[[1]]$hdb_4_room_flats,
        hdb_five_room = results_onemap[[1]]$hdb_5_room_and_executive_flats,
        condo = results_onemap[[1]]$condominiums_and_other_apartments,
        landed =  results_onemap[[1]]$landed_properties,
        other = results_onemap[[1]]$others,
        total_hdb = results_onemap[[1]]$total_hdb,
        total_housing = results_onemap[[1]]$total))
}

# prepare for join
df$planning_area = str_to_upper(df$planning_area)

# Geometry data with singapore boundaries and housing information
new_clean_sf_data <- sf_data |>
  left_join(df, by=c('pln_area_n'='planning_area')) |>
  mutate(non_hdb = total_housing - total_hdb,
         hdb_one_to_three_rooms = hdb_onetwo_room + hdb_three_room,
         condo_landed = condo + landed,
         .before = geometry) |>
  distinct()


# select only relevant data
housing_data_clean <- new_clean_sf_data |>
  select(region = pln_area_n,
         total_hdb,
         non_hdb)

#### Save data ####
st_write(housing_data_clean, "../data/01-analysis_data/shape/housing_geometry.geojson", 
         driver = "GeoJSON", append=FALSE)

housing_data_clean <- housing_data_clean |>
  st_drop_geometry() |>
  as_tibble()

write_csv(housing_data_clean, "../data/01-analysis_data/csv/housing.csv")


