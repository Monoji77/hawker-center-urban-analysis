#### Preamble ####
# Purpose: Combine all analysis data into one in preparation for linear regression
# Author: Chris Yong Hong Sen 
# Date: 02 December 2024
# Contact:luke.yong@mail.utoronto.ca 
# License: MIT
# Pre-requisites: 
# - The `sf` package must be installed and loaded
# - all 00-*.R must have been run

### set up ###
library(tidyverse)
library(sf)
library(fastDummies)

response_var_data <- read_csv("../data/01-analysis_data/csv/distance_mrt_to_hawker.csv")
median_income_data <- read_csv("../data/01-analysis_data/csv/median_income.csv")
num_of_hawkers_data <- read_csv("../data/01-analysis_data/csv/num_of_hawkers.csv")
housing_data <- read_csv("../data/01-analysis_data/csv/housing.csv")
planning_region_data <- st_read("../data/01-analysis_data/shape/planning_region_clean.geojson")

### join data ###
joined_data <- planning_region_data |>
  left_join(response_var_data, by = 'region') |>
  left_join(median_income_data, by = 'region') |>
  left_join(num_of_hawkers_data, by = 'region') |>
  left_join(housing_data, by = 'region')


joined_data_no_geometry <- joined_data |>
  st_drop_geometry() |>
  as_tibble() 

# #colSums(is.na(joined_data_no_geometry))
# one_data <- joined_data_no_geometry |>
#   drop_na(avg_distance_to_hawker) |>
#   select(avg_distance_to_hawker, num_of_hawker) |>
#   mutate(num_of_hawker = ifelse(is.na(num_of_hawker), 0, num_of_hawker),
#          avg_distance_to_hawker = sqrt(avg_distance_to_hawker))
# 
# lmmodel <- lm(avg_distance_to_hawker ~ num_of_hawker, one_data)
# summary(lmmodel)
# plot(lmmodel,1)

# Find regions with insufficient data
regions_with_missing_data <- joined_data_no_geometry |>
  rowwise() |>
  filter(sum(sapply(across(everything()), is.na)) >= 1) |>
  pull(region)

# Remove rows with missing values 
joined_data_clean <- joined_data_no_geometry |>
  drop_na() |>
  select(-absolute_count) |>
  mutate(area = factor(area),
         median_bin = factor(median_bin, levels=c('v3000_3999',
                                                  'v4000_4999',
                                                  'v5000_5999',
                                                  'v6000_6999',
                                                  'v9000_9999',
                                                  'v10000_10999',
                                                  'v12000_14999'), ordered=T))


numeric_col <- sapply(joined_data_clean, is.numeric)

joined_data_clean[numeric_col] <- scale(joined_data_clean[numeric_col])

joined_data_dummy <- dummy_cols(joined_data_clean, 
                                  select_columns = c('area', 'median_bin'),
                                remove_first_dummy = TRUE,
                                remove_selected_columns = TRUE) |>
  select(-region)

#unique(joined_data_clean$median_bin)
full_model <- lm(avg_distance_to_hawker ~ ., data = joined_data_dummy)

#hist(joined_data_dummy$avg_distance_to_hawker)
summary(full_model)

# library(MASS)
# boxcox_result <- boxcox(full_model)
# best_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
# run this in order to use select in tidyverse again
# detach("package:MASS", unload = TRUE)

#########
get_least_useful_var <- function(df) {
  variables <- df |> 
    select(-avg_distance_to_hawker) |>
    colnames()
  
  p_values <- c()
  
  for (var in variables) {
    reduced_data <- df |>
      select(-var)
    reduced_model <- lm(avg_distance_to_hawker ~ ., reduced_data)
    f_test <- anova(full_model, reduced_model)
    
    p_values <- c(p_values, f_test$`Pr(>F)`[2])
  }
  
  # extract least helpful variable
  p_value_tbl <- tibble(variables, p_values) |>
    arrange(desc(p_values))
  least_helpful_var <- p_value_tbl  |>
    top_n(1, p_values) |>
    as.list()
  var_to_keep <- p_value_tbl |>
    filter(p_values < 0.91) |>
    pull(variables)
  
  print(paste0('useless var: ', least_helpful_var$variables, 
               ' | p_value: ', least_helpful_var$p_values))
  
  print('---------------------')
  print(p_value_tbl)
  return(var_to_keep)
}

# set above function's filter to 0.8
var_to_keep <- get_least_useful_var(joined_data_dummy)
joined_data_dummy_1 <- joined_data_dummy |> select(all_of(var_to_keep), avg_distance_to_hawker)

summary(lm(avg_distance_to_hawker ~ ., joined_data_dummy_1))

# set the above function's filter to 0.9
var <- get_least_useful_var(joined_data_dummy_1)
joined_data_dummy_2 <- joined_data_dummy_1 |> select(all_of(var), avg_distance_to_hawker)

lm2 <- lm(avg_distance_to_hawker ~ ., joined_data_dummy_2)
summary(lm2)
plot(lm2, 1)

# set the above function's filter to 0.91
var <- get_least_useful_var(joined_data_dummy_2)
joined_data_dummy_3 <- joined_data_dummy_2 |> select(all_of(var), avg_distance_to_hawker)

lm3 <- lm(avg_distance_to_hawker ~ ., joined_data_dummy_3)
summary(lm3)
plot(lm3)
# 
# ##########################
# # first iteration remove (area_NORTH REGION: 0.995)
# var_to_keep <- get_least_useful_var(joined_data_dummy)
# joined_data_dummy_1 <- joined_data_dummy |> select(all_of(var_to_keep), avg_distance_to_hawker)
# 
# summary(lm(avg_distance_to_hawker ~ ., joined_data_dummy_1))
# 
# # second iteration (area_WEST REGION: 0.998)
# var <- get_least_useful_var(joined_data_dummy_1)
# joined_data_dummy_2 <- joined_data_dummy_1 |> select(all_of(var), avg_distance_to_hawker)
# 
# lm2 <- lm(avg_distance_to_hawker ~ ., joined_data_dummy_2)
# summary(lm2)
# plot(lm2, 1)
jhello world
# 
# # library(MASS)
# # boxcox_result <- boxcox(lm2)
# # best_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
# # #run this in order to use select in tidyverse again
# # detach("package:MASS", unload = TRUE)
# 
# 
# # third iteration (total_hdb: 0.999)
# var <- get_least_useful_var(joined_data_dummy_2)
# joined_data_dummy_3 <- joined_data_dummy_2 |> select(all_of(var), avg_distance_to_hawker)
# 
# lm3 <- lm(avg_distance_to_hawker ~ ., joined_data_dummy_3)
# summary(lm3)
# plot(lm3)
# 
# # fourth iteration (area_NORTH-EAST REGION: 1)
# var <- get_least_useful_var(joined_data_dummy_3)
# joined_data_dummy_4 <- joined_data_dummy_3 |> select(-var)
# 
# # fifth iteration (area_EAST REGION : 0.99)
# var <- get_least_useful_var(joined_data_dummy_4)
# joined_data_dummy_5 <- joined_data_dummy_4 |> select(-var)
# 
# # sixth iteration (median_bin_v12000_14999 : 0.98)
# var <- get_least_useful_var(joined_data_dummy_5)
# joined_data_dummy_6 <- joined_data_dummy_5 |> select(-var)
# 
# # seventh iteration (median_bin_v6000_6999 : 0.943)
# var <- get_least_useful_var(joined_data_dummy_6)
# joined_data_dummy_7 <- joined_data_dummy_6 |> select(-var)
# 
# # eigth iteration (median_bin_v9000_9999 : 0.946)
# var <- get_least_useful_var(joined_data_dummy_7)
# joined_data_dummy_8 <- joined_data_dummy_7 |> select(-var)
# 
# # ninth iteration (median_bin_v9000_9999 : 0.946)
# var <- get_least_useful_var(joined_data_dummy_8)
# joined_data_dummy_9 <- joined_data_dummy_8 |> select(-var)

unique((joined_data |> drop_na())$median_bin)
joined_data |>
  filter(median_bin == 'v3000_3999')
### save model ###
saveRDS(lm2, "../model/linear_model_four_var.rds")
saveRDS(lm3, "../model/linear_model_three_var.rds")

### check if model can see ###
summary(readRDS('../model/linear_model_four_var.rds'))
summary(readRDS('../model/linear_model_three_var.rds'))


### save combined geometry data ###
st_write(joined_data, "../data/02-combined_data/combined_data.geojson",
         driver = 'GeoJSON')
