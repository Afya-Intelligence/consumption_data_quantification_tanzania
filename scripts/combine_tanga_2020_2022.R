# Script to combine the data from elmis for tanga

# Author: Peter Boshe
# Version: 2023-04-27

# Packages
library(tidyverse)
library(purrr)

# Parameters
# #
# file_raw_2020 <- here::here("data-raw/stock_imbalance_by_facility/tanga/2020/")
# file_raw_2021 <- here::here("data-raw/stock_imbalance_by_facility/tanga/2021/")
# file_raw_2022 <- here::here("data-raw/stock_imbalance_by_facility/tanga/2022/")
file_raw_2020 <- here::here("data-raw/stock_imbalance_by_facility/tanga/version 2/Tanga_Dataset_2020_2022/2020_/")
file_raw_2021 <- here::here("data-raw/stock_imbalance_by_facility/tanga/version 2/Tanga_Dataset_2020_2022/2021_/")
file_raw_2022 <- here::here("data-raw/stock_imbalance_by_facility/tanga/version 2/Tanga_Dataset_2020_2022/2022_/")

file_out <- here::here("data/tanga_2020_2022.rds")
# ============================================================================

# Code
# create a function to read data from each folder and combine the csvs
# read_csv_files <- function(path) {
#   # get a list of all CSV files in the folder
#   files <- list.files(path = path, pattern = "*.csv", full.names = TRUE)
#
#   # read each file into a data frame and add a column with the file name
#   df <- map_df(files, ~{
#     read_csv(.x, col_types = cols(.default = "character", col_with_blank_data = col_double()), na = c("", "NA")) %>%
#       mutate(file = basename(.x))
#   })
#
#   return(df)
# }

read_csv_files <- function(path) {
  # get a list of all CSV files in the folder
  files <- list.files(path = path, pattern = "*.csv", full.names = TRUE)

  # read each file into a data frame and add a column with the file name
  df <- map_df(files, ~{
    # use tryCatch to capture errors and print the file name that caused the error
    tryCatch(
      {
        read_csv(.x, col_types = cols(.default = "character")) |>
          mutate(file = basename(.x)) |>
          janitor::clean_names() |>
          # check for the presence of columns starting with "line" and rename them to "line_number"
          rename_with(~"line_number", starts_with("line")) |>
          mutate(
          # transmute(line_number = as.numeric(starts_with("line")),
                    facility_code = as.factor(facility_code),
                    facility = as.factor(facility),
                    product_name,
                    product_code_name = as.factor(product_code_name),
                    zone = as.factor(zone),
                    district = as.factor(district),
                    period,
                    physical_count = parse_number(physical_count),
                    amc = parse_number(amc),
                    mos = parse_number(mos),
                    order_quantity = parse_number(order_quantity),
                    status = as.factor(status),
                    file)
      },
      error = function(e) {
        message(paste("Error in", .x))
        message(e)
        NULL
      }
    )
  })

  return(df)
}

# function to clean the data
# read_csv_files(file_raw_2020) |>
#   janitor::clean_names() |>
#   transmute(line_number = as.numeric(line_number),
#             facility_code = as.factor(facility_code),
#             facility = as.factor(facility),
#             product_name,
#             product_code_name = as.factor(product_code_name),
#             zone = as.factor(zone),
#             district = as.factor(district),
#             period,
#             physical_count = parse_number(physical_count),
#             amc = parse_number(amc),
#             mos = parse_number(mos),
#             order_quantity = parse_number(order_quantity),
#             status = as.factor(status),
#             file)

# read in the seperate folders
#
tanga_2020 <- read_csv_files(file_raw_2020)
tanga_2021 <- read_csv_files(file_raw_2021)
tanga_2022 <- read_csv_files(file_raw_2022)

# combine the dataset
combined_tanga_2020_2022 <- bind_rows(list("2020" = tanga_2020, "2021" = tanga_2021, "2022" = tanga_2022), .id = "year")

# pre-process the data
combined_tanga_2020_2022 <- combined_tanga_2020_2022 |>
  mutate(group = str_extract(file, "_([a-z])\\.csv$"),
         group = str_extract(group,"[a-z]")) |>
  relocate(year, .after = group) |>
  select(-file)




# write out the dataset
#
write_rds(combined_tanga_2020_2022, file_out)
write_csv(combined_tanga_2020_2022, "data-raw/stock_imbalance_by_facility/tanga/tanga_2020_2022_combined.csv")
