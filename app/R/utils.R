# Script with functions for the shiny app

# Author: Peter Boshe
# Version: 2023-05-02

# Packages
library(tidyverse)
library(fpp2)
# library(visdat)
library(gt)
library(janitor)
library(purrr)

# Parameters

#############################################################################
#  Function for removing outliers                                           #
#############################################################################

replace_outliers_with_median <- function(x, coef = 1.5) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  fence_low <- q1 - coef * iqr
  fence_high <- q3 + coef * iqr
  x[x < fence_low | x > fence_high] <- median(x, na.rm = TRUE)
  return(x)
}

#############################################################################
#  Function for subsetting and fitting the model                            #
#############################################################################


forecast_facility_product <- function(df, product_name, facility_name, reporting_frequency, start_year, start_month, forecast_period) {

  df_selected <- df |>
    filter(product_cleaned == product_name) |>
    filter(facility == facility_name) |>
    mutate(reporting_period = month_range) |>
    complete(reporting_period,year,
             fill = list(order_quantity = 0)) |>
    arrange(year, reporting_period) |>
    mutate(order_month = case_when(
      reporting_period == "january - february" ~ "march",
      reporting_period == "february - march" ~ "april",
      reporting_period == "march - april" ~ "may",
      reporting_period == "april - may" ~ "june",
      reporting_period == "may - june" ~ "july",
      reporting_period == "june - july" ~ "august",
      reporting_period == "july - august" ~ "september",
      reporting_period == "august - september" ~ "october",
      reporting_period == "september - october" ~ "november",
      reporting_period == "october - november" ~ "december",
      reporting_period == "november - december" ~ "january",
      reporting_period == "december - january" ~ "february"
    ),
    year = as.numeric(as.character(year)),
    order_year = case_when(
      order_month == "january" ~ year + 1,
      order_month == "february" ~ year + 1,
      TRUE ~ year
    ),
    monthly_consumption = amc,
    status = case_when(
      is.na(status) ~ "No Record",
      TRUE ~ status
    )) |>
    fill(product_cleaned, district, facility) |>
    fill(monthly_consumption, .direction = "downup") |>
    select(
      reporting_period,
      year,
      order_month,
      order_year,
      closing_balance,
      monthly_consumption
    )

  # replace outliers with median
  df_selected$monthly_consumption <- replace_outliers_with_median(df_selected$monthly_consumption)
 print(df_selected)
 print(reporting_frequency)
 print(start_month)
 print(start_year)
 print("now lets see ts_selected")
  # creating time series object
  start_month_value <- start_month
  ts_selected <- ts(df_selected$monthly_consumption, frequency = reporting_frequency, start = c(start_year, start_month_value))

   print(ts_selected)
   #include code to subset data

  ts_selected |>
    Arima(order = c(2,0,1), include.constant = T) |>
    forecast(h = forecast_period) |>
    autoplot() -> plot_data

  # Create the summary table
  summary_data <- ts_selected |>
    Arima(order = c(2,0,1), include.constant = T) |>
    forecast(h = forecast_period) |>
    summary() |>
    as.data.frame()

  # create the annual requirement
  annual_forecast <- ts_selected |>
    Arima(order = c(2,0,1), include.constant = T) |>
    forecast(h = forecast_period) |>
    summary() |>
    as.data.frame() |>
    janitor::clean_names() |>
    summarise(annual_forecast = sum(point_forecast))


  # Return a list containing both the plot and the summary table
  return(list(plot = plot_data, summary = summary_data, forecast = annual_forecast))
}


############################################################################
#  function for checking accuracy                                          #
#############################################################################


check_model_accuracy <- function(df, forecast_year, product_name, facility_name) {

  df_selected <- df |>
    filter(product_cleaned == product_name) |>
    filter(facility == facility_name) |>
    mutate(reporting_period = month_range) |>
    complete(reporting_period,year,
             # fill = list(amc = median(test_df$amc, na.rm = T), order_quantity = 0)) |>
             fill = list(order_quantity = 0)) |>
    arrange(year, reporting_period) |>
    mutate(order_month = case_when(
      reporting_period == "january - february" ~ "march",
      reporting_period == "february - march" ~ "april",
      reporting_period == "march - april" ~ "may",
      reporting_period == "april - may" ~ "june",
      reporting_period == "may - june" ~ "july",
      reporting_period == "june - july" ~ "august",
      reporting_period == "july - august" ~ "september",
      reporting_period == "august - september" ~ "october",
      reporting_period == "september - october" ~ "november",
      reporting_period == "october - november" ~ "december",
      reporting_period == "november - december" ~ "january",
      reporting_period == "december - january" ~ "february"
    ),
    year = as.numeric(as.character(year)),
    order_year = case_when(
      order_month == "january" ~ year + 1,
      order_month == "february" ~ year + 1,
      TRUE ~ year
    ),
    monthly_consumption = amc,
    status = case_when(
      is.na(status) ~ "No Record",
      TRUE ~ status
    )) |>
    fill(product_cleaned, district, facility) |>
    fill(monthly_consumption, .direction = "downup") |>
    select(
      reporting_period,
      year,
      order_month,
      order_year,
      closing_balance,
      monthly_consumption
    ) |>
    mutate(month_num = match(order_month, tolower(month.name)),
           fiscal_year = if_else(month_num > 6, order_year, order_year - 1)) |>
    select(-month_num)

  annual_consumption <-   df_selected |>
    filter(fiscal_year == forecast_year) |>
    summarise(annual_consumption = sum(monthly_consumption))


  # replace outliers with median
  df_selected$monthly_consumption <- replace_outliers_with_median(df_selected$monthly_consumption)

  # creating time series object
  ts_selected <- ts(df_selected$monthly_consumption, frequency = 12, start = c(min(df_selected$fiscal_year), 3))

  # seperate the data into a training set and a test set so as to validate our forecasts
  train <- window(ts_selected, end = c((forecast_year), 6))

  rmse_score <-   train |>
    Arima(order = c(2,0,1), include.constant = T) |>
    forecast(h = 12) |>
    accuracy(ts_selected) |>
    as.data.frame() |>
    slice(2) |>
    pull("RMSE")

  annual_forecast <- train |>
   Arima(order = c(2, 0, 1), include.constant = T) |>
   forecast(h = 12) |>
   summary() |>
   as.data.frame() |>
   janitor::clean_names() |>
   summarise(annual_forecast = sum(point_forecast))

  # return(scores["Test set", "RMSE"])
  return(list(scores = rmse_score, forecast = annual_forecast, consumption = annual_consumption))


}

#############################################################################
#  forecast accuracy                                                              #
#############################################################################



calculate_accuracy <- function(df, forecast_year, product_name, facility_name) {
  annual_forecast <- round(check_model_accuracy(df = df, forecast_year = forecast_year, facility_name = facility_name, product_name = product_name)$forecast$annual_forecast, 2)
  actual_consumption <- round(check_model_accuracy(df = df, forecast_year = forecast_year, facility_name = facility_name, product_name = product_name)$consumption$annual_consumption, 2)
  accuracy <- (1 - ((annual_forecast - actual_consumption)/annual_forecast))*100
  return(accuracy)
}


#############################################################################
#  manual accuracy                                                              #
#############################################################################

calculate_manual_accuracy <- function(forecast, consumption){
  accuracy <- (1 - ((forecast - consumption)/forecast))*100
  return(accuracy)
}

#############################################################################
#  function for ui value boxes
#############################################################################

create_valuebox <- function(id) {
  column(
    width = 4,
    valueBoxOutput(id)
  )
}





