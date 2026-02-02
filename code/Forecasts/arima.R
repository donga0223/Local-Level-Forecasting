
library(tidyverse)
library(forecast)
library(dplyr)
library(purrr)

fit_and_forecast_arima <- function(data, quantiles, horizon, num_samples = 2000) {
  # Ensure data is sorted by date
  data <- data %>% arrange(target_end_date)
  
  # Extract incidence data
  incidence <- data$observation
  
  # Fit the arima model
  model <- forecast::auto.arima(incidence)
  
  # Simulate future paths and compute quantiles
  sims <- replicate(
    num_samples,
    as.numeric(stats::simulate(model, nsim = horizon)),
    simplify = "matrix"
  )
  
  q_mat <- apply(sims, 1, quantile, probs = quantiles, na.rm = TRUE)  # dim: length(quantiles) x horizon
  # No negative counts
  q_mat[q_mat < 0] <- 0
  q_mat <- t(q_mat)  # horizon x quantiles
  
  forecast_results <- tibble::tibble(
    horizon  = rep(seq_len(nrow(q_mat)), each = ncol(q_mat)),
    type     = "inc",
    quantile = rep(quantiles, times = nrow(q_mat)),
    value    = as.vector(t(q_mat))
  )
  
  # Add location & target metadata
  forecast_results <- forecast_results %>%
    mutate(
      location = dplyr::first(data$location),
      target = dplyr::first(data$target)
    )
  
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


arima_forecast <- function(dat, quantiles, horizon = 4, forecast_date){
  reference_date = forecast_date + (6 - as.integer(format(forecast_date, "%u"))) %% 7
  print(reference_date)
  
  dat1 <- dat %>%
    filter(target_end_date < forecast_date)
  
  forecast_output <- dat1 %>%
    group_split(location) %>%  # Split data by location
    map_dfr(~fit_and_forecast_arima(.x, quantiles, horizon, num_samples = 2000))  # Apply function and merge results
  
  
  forecast_output1 <- forecast_output %>%
    filter(type == "inc", location != "Unknown") %>%
    mutate(reference_date = reference_date, 
           horizon = horizon - 1,  # Conditional horizon update
           output_type = "quantile") %>%
    rename(output_type_id = quantile) %>%
    mutate(target_end_date = reference_date + 7*horizon) %>%
    select(reference_date, location, horizon, target, target_end_date, output_type, output_type_id, value)
  # View the first few rows
  #head(forecast_output1)
  
  write.csv(forecast_output1, paste("model_output/arima/", reference_date, "-arima.csv", sep=""), 
            row.names = FALSE)
}
  


quantiles <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)  # Prediction intervals
horizon <- 4


## 2324 season
c1 <- read.csv("data/cdc_nssp_20250804_2425_to_2122.csv")
dat <- data_clean(c1)
forecast_date_list <- seq(as.Date("2023-10-05"), as.Date("2024-03-30"), by = 7)


for(i in 1:length(forecast_date_list)){
  arima_forecast(dat = dat, quantiles = quantiles, horizon = 4, forecast_date = forecast_date_list[i])
}



## 2223 season
c1 <- read.csv("data/cdc_nssp_20250804_2325_to_2022.csv")
dat <- data_clean(c1)
forecast_date_list <- seq(as.Date("2022-10-05"), as.Date("2023-03-30"), by = 7)


for(i in 1:length(forecast_date_list)){
  arima_forecast(dat = dat, quantiles = quantiles, horizon = 4, forecast_date = forecast_date_list[i])
}


##2425 season

c1 <- read.csv("data/cdc_nssp_20250523.csv")
dat <- data_clean(c1)
forecast_date_list <- seq(as.Date("2024-10-02"), as.Date("2025-03-30"), by = 7)


for(i in 1:length(forecast_date_list)){
  arima_forecast(dat = dat, quantiles = quantiles, horizon = 4, forecast_date = forecast_date_list[i])
}
