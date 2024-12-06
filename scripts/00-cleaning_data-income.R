#### Preamble ####
# Purpose: clean raw income by region data
# Author: Chris Yong Hong Sen 
# Date: 02 December 2024
# Contact:luke.yong@mail.utoronto.ca 
# License: MIT
# Pre-requisites: 
# - The `tidyverse` package must be installed and loaded

### setup ###
library(tidyverse)
raw_income_data <- read_csv("../data/00-raw_data/raw_income_data.csv", )

### clean income data ###
intermediate_data_1 <- raw_income_data |>
  slice(10:42) |>
  select(1:16) |>
  janitor::row_to_names(row_number=1) |>
  janitor::clean_names()

clean_income_data <- intermediate_data_1 |>
  slice(-c(1,32)) |>
  rename(region = planning_area_of_residence,
         total_indiv = total,
         below_1000 = below_1_000,
         v1000_1999 = x1_000_1_999,
         v2000_2999 = x2_000_2_999,
         v3000_3999 = x3_000_3_999,
         v4000_4999 = x4_000_4_999,
         v5000_5999 = x5_000_5_999,
         v6000_6999 = x6_000_6_999,
         v7000_7999 = x7_000_7_999,
         v8000_8999 = x8_000_8_999,
         v9000_9999 = x9_000_9_999,
         v10000_10999 = x10_000_10_999,
         v11000_11999 = x11_000_11_999,
         v12000_14999 = x12_000_14_999,
         over_15000 = x15_000_over) |>
  mutate(region = str_to_upper(region)) |>
  mutate(across(c(total_indiv:over_15000), as.numeric))

### obtain median income per region
longer_income_data <- clean_income_data |>
  pivot_longer(cols = below_1000:over_15000,
               names_to = 'group',
               values_to = 'count') |> 
  group_by(region) |>
  mutate(cumsum = cumsum(count),
         median = total_indiv/2) |>
  filter(cumsum > median) |>
  slice(1) |>
  ungroup() |>
  select(region, 
         total_indiv, 
         median_bin = group,
         absolute_count = count)

### save clean data ###
write_csv(longer_income_data, 
          "../data/01-analysis_data/csv/median_income.csv")
