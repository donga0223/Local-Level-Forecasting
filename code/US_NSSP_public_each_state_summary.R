
library(dplyr)
library(tidyverse)
library(here)
library(MMWRweek)
source("Local-Level-Forecasting/code/preprocess_and_plot.R")

## observed data format 
## target_end_date, location, inc (if you need you can add more variables)
e1 <- read.csv('/Users/dk29776/Dropbox/UTAustin/Forecasting/Local-Level-Forecasting/data/cdc_nssp_20250523.csv')
e2 <- read.csv('/Users/dk29776/Dropbox/UTAustin/Forecasting/Local-Level-Forecasting/data/cdc_nssp_20250804.csv')

date_list <- seq.Date(from = as.Date("2024-11-16"), 
                      to = as.Date("2025-03-30"), 
                      by = "week")


#### when you are loading forecasting output from each state folder
states = c("Texas", "North Carolina", "Tennessee", "Georgia", "Illinois", "New York", "Massachusetts")
abb_states = c("TX", "NC", "TN", "GA", "IL", "NY", "MA")

states = c("Nevada")
abb_states = c("NV")

for(i in 1:length(states)){
  assign(paste("df", abb_states[i], sep = "_"), cal_metrics(obs_data = e2, mystate = states[i], my_abb_state = abb_states[i], date_list, pop_size = 250000)
)
}

#### when you are loading forecasting output all together (state and locals are in one file)
US_NSSP <- cal_metrics(obs_data = e2, mystate = NULL, my_abb_state = "US_top81_NSSP_public_multiCounty_pct", 
            date_list, pop_size = 500000)

US_NSSP <- cal_metrics(obs_data = e2, mystate = NULL, my_abb_state = "US_NSSP_public_500K_pct", 
                       date_list, pop_size = 500000)
  
US_HSA_NSSP <- cal_metrics(obs_data = e2, mystate = NULL, my_abb_state = "US_NSSP_public_HSA_500K_pct", 
                       date_list, pop_size = 500000)

#### when you are loading forecasting output state and local from different folders (most recent one)
US_state_NSSP <- cal_metrics(obs_data = e2, mystate = NULL, my_abb_state = "US_NSSP_public_HSA_500K_pct",
                           state_level = "US_NSSP_public_state_pct", date_list, pop_size = 500000)

US_state_NSSP_250K <- cal_metrics(obs_data = e2, mystate = NULL, my_abb_state = "US_NSSP_public_HSA_250K_pct",
                             state_level = "US_NSSP_public_state_pct", date_list, pop_size = 250000)


