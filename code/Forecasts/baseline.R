
setwd("/Users/dk29776/Dropbox/UTAustin/City-Level-Forecasting")
source("epiENGAGE-baseline/code/quantile_baseline.R")
source("epiENGAGE-baseline/code/estimation.R")
source("epiENGAGE-baseline/code/transform.R")
source("epiENGAGE-baseline/code/prediction.R")


library(dplyr)
library(purrr)

# Define function to fit and predict using the baseline model
fit_and_forecast_baseline <- function(data, quantiles, horizon, num_samples) {
  # Ensure data is sorted by date
  data <- data %>% arrange(target_end_date)
  
  # Extract incidence data
  incidence <- data$observation
  
  # Fit the baseline model
  baseline_model <- fit_quantile_baseline(incidence, symmetrize = TRUE)
  
  # Generate forecasts
  forecast_results <- predict.quantile_baseline(
    quantile_baseline = baseline_model,
    inc_data = incidence,
    cum_data = cumsum(incidence),  # Simulated cumulative data
    quantiles = quantiles,
    horizon = horizon,
    num_samples = num_samples
  )
  
  # Add location information back
  forecast_results <- forecast_results %>%
    mutate(location = unique(data$location),
           target = unique(data$target))
  
  return(forecast_results)
}

data_clean <- function(c1, population_threshold = 250000){
  dat <- c1 %>% 
    rename(target_end_date = week_end,
           observation = inc) %>%
    mutate(location = paste(state, hsa_nci_id, sep = "_"),
           target = "Flu ED visits pct",
           population = if_else(hsa_nci_id != "All",
                                population_hsa, population_state)) %>%
    select(target_end_date, location, target, observation, population) %>%
    filter(!is.na(observation), population >= population_threshold) %>%
    distinct()
  
  return(dat)
}



baseline_forecast <- function(dat, quantiles, horizon = 4, forecast_date){
  reference_date = forecast_date + (6 - as.integer(format(forecast_date, "%u"))) %% 7
  print(reference_date)
  
  dat1 <- dat %>%
    filter(target_end_date < forecast_date)
  
  forecast_output <- dat1 %>%
    group_split(location) %>%  # Split data by location
    map_dfr(~fit_and_forecast_baseline(.x, quantiles, horizon, num_samples = 1000))  # Apply function and merge results
  
  forecast_output1 <- forecast_output %>%
    filter(type == "inc", location != "Unknown") %>%
    mutate(reference_date = reference_date, 
           horizon = horizon - 1,  # Conditional horizon update
           output_type = "quantile") %>%
    rename(output_type_id = quantile) %>%
    mutate(target_end_date = reference_date + 7*horizon) %>%
    select(reference_date, location, horizon, target, target_end_date, output_type, output_type_id, value)
  
  write.csv(forecast_output1, paste("model_output/baseline/", reference_date, "-baseline.csv", sep=""), 
            row.names = FALSE)
}


library(lubridate)
# Set parameters for forecasting
quantiles <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)  # Prediction intervals
horizon <- 4  # Forecast 7 days ahead
num_samples <- 1000  # Number of Monte Carlo samples

## 2324 season
setwd("/Users/dk29776/Dropbox/UTAustin/Forecasting/Local-Level-Forecasting")
c1 <- read.csv("data/cdc_nssp_20250804_2425_to_2122.csv")
dat <- data_clean(c1)
forecast_date_list <- seq(as.Date("2023-10-05"), as.Date("2024-03-30"), by = 7)


for(i in 1:length(forecast_date_list)){
  baseline_forecast(dat = dat, quantiles = quantiles, horizon = 4, forecast_date = forecast_date_list[i])
}



## 2223 season
c1 <- read.csv("data/cdc_nssp_20250804_2325_to_2022.csv")
dat <- data_clean(c1)
forecast_date_list <- seq(as.Date("2022-10-05"), as.Date("2023-03-30"), by = 7)


for(i in 1:length(forecast_date_list)){
  baseline_forecast(dat = dat, quantiles = quantiles, horizon = 4, forecast_date = forecast_date_list[i])
}


##2425 season

c1 <- read.csv("data/cdc_nssp_20250523.csv")
dat <- data_clean(c1)
forecast_date_list <- seq(as.Date("2024-10-02"), as.Date("2025-03-30"), by = 7)


for(i in 1:length(forecast_date_list)){
  baseline_forecast(dat = dat, quantiles = quantiles, horizon = 4, forecast_date = forecast_date_list[i])
}


