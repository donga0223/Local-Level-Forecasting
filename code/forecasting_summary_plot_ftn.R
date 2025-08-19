
library(dplyr)
library(tidyverse)
library(here)
library(MMWRweek)
library(scoringutils)
library(ggplot2)

root <- normalizePath(file.path(here::here(), ".."))  # 상위 폴더


forecast_est_data <- function(out, i){
  out <- out %>% mutate(no = as.factor(i),
                        horizon = horizon + 1) 
  est_low_data <- out %>%
    filter(output_type_id == 0.025) %>%
    mutate(est_low = value) %>%
    select(-output_type_id, -value)
  
  est_median_data <- out %>%
    filter(output_type_id == 0.5) %>%
    mutate(est_median = value) %>%
    select(-output_type_id, -value)
  
  est_high_data <- out %>%
    filter(output_type_id == 0.975) %>%
    mutate(est_high = value) %>%
    select(-output_type_id, -value)
  
  #ribbon_data <- out %>%
  #  filter(output_type_id %in% c(0.025, 0.5, 0.975)) %>%
  #  spread(output_type_id, value) %>%
  #  rename(est_low = `0.025`, est_high = `0.975`, est_median = `0.5`)
  
  ribbon_data <- est_low_data %>%
    left_join(est_median_data, by = c("location", "reference_date", "horizon", "target_end_date", "target", "output_type", "no")) %>%
    left_join(est_high_data, by =  c("location", "reference_date", "horizon", "target_end_date", "target", "output_type", "no")) %>%
    mutate(target_end_date = as.Date(target_end_date),
           reference_date = as.Date(reference_date))
  
  return(ribbon_data)
}


all_ribbon_data <- function(date_list, location_level = NULL, model_name ="GBQR", specific_h = NULL){
  date_length <- length(date_list)
  for (i in 1:date_length){
    assign(paste0("out",i), read.csv(paste(root, '/City-Level-Forecasting-local/', model_name, '/model_output/', location_level, '/', date_list[i], "-", model_name, '.csv', sep = "")))
  }
  
  ribbon_data <- forecast_est_data(out1, 1)
  for(j in 2:date_length){
    ribbon_data <- rbind(ribbon_data, forecast_est_data(get(paste0("out",j)), j))
  }
  
  if(!is.null(specific_h)){
    ribbon_data <- ribbon_data %>% filter(horizon == specific_h) 
  }
  
  return(ribbon_data)
}


df_obs_est <- function(ribbon_data, filtered_obs_data){
  state_inc <- filtered_obs_data %>%
    select(target_end_date = week_end, state, inc = inc_state) %>%
    mutate(hsa_nci_id = "All",
           location = paste(state, "All", sep = "_")) %>%
    distinct() %>%
    select(target_end_date, state, hsa_nci_id, location, inc)
  
  hsa_inc <- filtered_obs_data %>%
    select(target_end_date = week_end, state, hsa_nci_id, inc = inc_hsa) %>%
    mutate(location = paste(state, hsa_nci_id, sep = "_"),
           hsa_nci_id = as.character(hsa_nci_id)) %>%
    select(target_end_date, state, hsa_nci_id, location, inc)
  
  long_inc <-state_inc %>%
    bind_rows(hsa_inc)
  
  df <- long_inc %>%
    left_join(ribbon_data, by = c("location", "target_end_date")) 
  
  df_state <- df %>% 
    filter(hsa_nci_id == "All") 
  
  df1 <- df %>%
    filter(hsa_nci_id != "All") %>%
    left_join(df_state, by = c("target_end_date", "state", "reference_date", 
                               "horizon", "target", "output_type", "no"),
              suffix = c("", ".state")) %>%
    mutate(coverage = ifelse(inc >= est_low & inc <= est_high, 1, 0),
           coverage.state = ifelse(inc >= est_low.state & inc <= est_high.state, 1, 0),
           MAPE = ifelse(inc == 0, abs((inc-est_median)/((inc + est_median)/2)), abs((inc-est_median)/inc)),
           MAPE.state = ifelse(inc == 0, abs((inc - est_median.state)/((inc + est_median.state)/2)), abs((inc - est_median.state)/inc)))
  
  return(df1)
  
}


compute_wis_score <- function(df, value_col, target_date) {
  df_wis <- df %>%
    rename(predicted = !!sym(value_col),
           observed = inc,
           quantile_level = output_type_id) %>%
    select(reference_date, state, hsa_nci_id, location, horizon, target, target_end_date,
           output_type, quantile_level, observed, predicted) |>
    as_forecast_quantile(
      forecast_unit = c("state", "hsa_nci_id", "location", "target_end_date", "horizon", "target"),
      value_col = "predicted",
      quantile_col = "quantile_level"
    ) |>
    score() |>
    mutate(horizon = horizon + 1,
           reference_date = target_date) |>
    select(state, hsa_nci_id, location, target_end_date, horizon, reference_date, wis)
  return(df_wis)
}

get_wis <- function(target_date, location_level = NULL, state_level = NULL, model_name ="GBQR", filtered_obs_data){
  assign("out", read.csv(paste(root, '/City-Level-Forecasting-local/', model_name, '/model_output/', location_level, '/', target_date, "-", model_name, '.csv', sep = "")))
  if(!is.null(state_level)){
    assign("state.out", read.csv(paste(root, '/City-Level-Forecasting-local/', model_name, '/model_output/', state_level, '/', target_date, "-", model_name, '.csv', sep = "")))
    out = rbind(out, state.out)
  }
  out1 <- out %>%
    mutate(target_end_date = as.Date(target_end_date)) %>%
    left_join(filtered_obs_data,
              by = c("target_end_date", "location")) 
  
  out_hsa <- out1 %>% filter(hsa_nci_id != "All")
  out_state <- out1 %>% filter(hsa_nci_id == "All") 
  
  out_all <- out_hsa %>%
    left_join(out_state, by = c("reference_date", "horizon", "target", "target_end_date", 
                                "output_type", "output_type_id", "state"),
              suffix = c("", ".state"))
  
  
  wis_hsa <- compute_wis_score(out_all, "value", target_date)
  wis_state <- compute_wis_score(out_all, "value.state", target_date)
  return(list(wis_hsa = wis_hsa, wis_state = wis_state))
}

#for(i in 1:length(date_list)){
#  get_wis(target_date = date_list[i], location_level, state_level, model_name ="GBQR", filtered_obs_data)
#  print(date_list[i])
#}


forecast_plot <- function(date_list, location_level = NULL, state_level = NULL, model_name ="GBQR", 
                          specific_h = NULL, filtered_obs_data){
  
  ribbon_data = all_ribbon_data(date_list = date_list, 
                                location_level = location_level, 
                                model_name ="GBQR", 
                                specific_h = specific_h)
  if(!is.null(state_level)){
    state_ribbon_data = all_ribbon_data(date_list = date_list, 
                                        location_level = state_level, 
                                        model_name ="GBQR", 
                                        specific_h = specific_h)
    ribbon_data = rbind(ribbon_data, state_ribbon_data)
    
  }
  
  df_all <- df_obs_est(ribbon_data, filtered_obs_data)
  df_return <- df_all %>% 
    filter(!is.na(horizon)) %>%
    select(-target, -output_type, -no, -location.state, -hsa_nci_id.state)
  
  p <- df_all %>% filter(target_end_date >= as.Date(min(date_list)-7),
                         target_end_date <= as.Date(max(date_list)+28)) %>%
    ggplot(aes(target_end_date)) + 
    geom_point(aes(y = inc, col="City Data"), size=0.95, shape=1, alpha=0.7) +
    geom_line(aes(y = est_median, col = "City Estimate"), size = 1) +
    geom_ribbon(aes(ymin = est_low, ymax = est_high), fill = 'orange', alpha = 0.3) +  # Add shaded ribbon
    geom_point(aes(y = inc.state, col="State Data"), size=0.95, shape=1, alpha=0.9) +
    geom_line(aes(y = est_median.state, col = 'State Estimate'), size = 1) +
    geom_ribbon(aes(ymin = est_low.state, ymax = est_high.state), fill = 'gray30', alpha = 0.3) +  # Add shaded ribbon
    scale_color_manual(
      values = c(
        "City Data" = "red",
        "City Estimate" = "orange",
        "State Data" = "black",
        "State Estimate" = "gray30"
      ),
      name = "Legend"
    ) +
    ggtitle(paste("horizon = ", specific_h)) + 
    theme(panel.spacing=unit(0, "mm"),
          legend.position = "none",
          axis.text=element_text(size=15),
          axis.title=element_text(size=20,face="bold"),
          strip.text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5, size = 30, face = "bold"))+
    scale_x_date(date_labels = "%b %y",  # Format dates as "Jan 01, 2023"
                 date_breaks = "2 months")+
    labs(
      x = "",  # x-axis label
      y = "Flu ED visits pct",      # y-axis label
    ) +
    facet_wrap(~location, scales = "free_y") 
  print(p)
  
  
  p1 <- df_all %>% filter(target_end_date >= as.Date(min(date_list)-7),
                          target_end_date <= as.Date(max(date_list)+28)) %>%
    ggplot(aes(target_end_date)) + 
    geom_point(aes(y = inc, col="City Data"), size=1.5, shape=1, alpha=0.7) +
    geom_line(aes(y = est_median, col = "City Estimate"), size = 1) +
    geom_ribbon(aes(ymin = est_low, ymax = est_high), fill = 'orange', alpha = 0.3) +  # Add shaded ribbon
    #geom_point(aes(y = inc_state, col="State Data"), size=0.95, shape=1, alpha=0.9) +
    #geom_line(aes(y = est_median.state, col = 'State Estimate'), size = 1) +
    #geom_ribbon(aes(ymin = est_low.state, ymax = est_high.state), fill = 'gray30', alpha = 0.3) +  # Add shaded ribbon
    scale_color_manual(
      values = c(
        "City Data" = "red",
        "City Estimate" = "orange"
        #"State Data" = "black",
        #"State Estimate" = "gray30"
      ),
      name = "Legend"
    ) +
    ggtitle(paste("horizon = ", specific_h)) + 
    theme(panel.spacing=unit(0, "mm"),
          legend.position = "none",
          axis.text=element_text(size=15),
          axis.title=element_text(size=20,face="bold"),
          strip.text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5, size = 30, face = "bold"))+
    scale_x_date(date_labels = "%b %y",  # Format dates as "Jan 01, 2023"
                 date_breaks = "2 months")+
    labs(
      x = "",  # x-axis label
      y = "Flu ED visits pct",      # y-axis label
    ) +
    facet_wrap(~location, scales = "free_y") 
  print(p1)
  
  p2 <- df_all %>% filter(target_end_date >= as.Date(min(date_list)-7),
                          target_end_date <= as.Date(max(date_list)+28)) %>%
    ggplot(aes(target_end_date)) + 
    geom_point(aes(y = inc, col="City Data"), size=1.5, shape=1, alpha=0.7) +
    #geom_line(aes(y = est_median, col = "City Estimate"), size = 1) +
    #geom_ribbon(aes(ymin = est_low, ymax = est_high), fill = 'orange', alpha = 0.3) +  # Add shaded ribbon
    #geom_point(aes(y = inc_state, col="State Data"), size=0.95, shape=1, alpha=0.9) +
    geom_line(aes(y = est_median.state, col = 'State Estimate'), size = 1) +
    geom_ribbon(aes(ymin = est_low.state, ymax = est_high.state), fill = 'gray30', alpha = 0.3) +  # Add shaded ribbon
    scale_color_manual(
      values = c(
        "City Data" = "red",
        #"City Estimate" = "orange"
        #"State Data" = "black",
        "State Estimate" = "gray30"
      ),
      name = "Legend"
    ) +
    ggtitle(paste("horizon = ", specific_h)) + 
    theme(panel.spacing=unit(0, "mm"),
          legend.position = "none",
          axis.text=element_text(size=15),
          axis.title=element_text(size=20,face="bold"),
          strip.text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5, size = 30, face = "bold"))+
    scale_x_date(date_labels = "%b %y",  # Format dates as "Jan 01, 2023"
                 date_breaks = "2 months")+
    labs(
      x = "",  # x-axis label
      y = "Flu ED visits pct",      # y-axis label
    ) +
    facet_wrap(~location, scales = "free_y") 
  print(p2)
  return(df_return)
}


base_obs_data <- function(obs_data, date_var, mystate = NULL, pop_size){
  if (!is.null(mystate)) {
    obs_data <- obs_data %>% filter(state == mystate)
  }
  df <- obs_data %>%
    filter(population_hsa >= pop_size,
           !is.na(inc_hsa)) %>%
    mutate(target_end_date = as.Date(.data[[date_var]]),
           location = paste(state, hsa_nci_id, sep = "_")) %>%
    distinct()
  
  return(df)
}


obs_figure <- function(filtered_obs_data){
  
  p <- filtered_obs_data %>%
    filter(season %in% c("2022/23", "2023/24", "2024/25")) %>%
    ggplot(aes(x = season_week)) +
    geom_line(aes(y = inc_hsa, linetype = season, group = season), color = "orange", size = 1) +
    geom_line(aes(y = inc_state, linetype = season, group = season), color = "gray30", size = 1) +
    facet_wrap(location~population_hsa, scales = "free_y") +
    scale_linetype_manual(values = c("dashed", "dotted", "solid")) +  # 필요시 직접 지정
    labs(
      x = "Epiweek",
      y = "Flu ED visits pct",
      linetype = "Season"
    ) +
    #ggtitle("City vs State Trend by Season") +
    #scale_x_continuous(breaks = seq(0, 52, by = 4)) +  # 필요시 x축 조절
    theme(
      panel.spacing = unit(0, "mm"),
      legend.position = "bottom",
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 20, face = "bold"),
      strip.text = element_text(size = 15),
      plot.title = element_text(hjust = 0.5, size = 30, face = "bold")
    )
  print(p)
  
  
}


#location_level = "US_top81_NSSP_public_multiCounty_pct"
## create figures and calculate metrics 
cal_metrics <- function(obs_data, mystate = NULL, my_abb_state = NULL, state_level = NULL,
                        date_list, pop_size = 250000, pdfname = NULL){
  if(nchar(my_abb_state) == 2){
    location_level = paste("US_NSSP_public", my_abb_state, "pct", sep = "_")
    pdf(paste(root, "/Forecasting/Local-Level-Forecasting/each_state_figures/forecasting_", my_abb_state, ".pdf", sep = ""), width = 15, height = 10)
  }else if(!is.null(pdfname)){
    location_level = my_abb_state
    pdf(paste(root, "/Forecasting/Local-Level-Forecasting/each_state_figures/forecasting_", pdfname, ".pdf", sep = ""), width = 30, height = 30)
  }else{
    location_level = my_abb_state
    pdf(paste(root, "/Forecasting/Local-Level-Forecasting/each_state_figures/forecasting_", my_abb_state, ".pdf", sep = ""), width = 30, height = 30)
  }
  filtered_obs_data <- base_obs_data(obs_data, date_var = "week_end", mystate = mystate, pop_size = pop_size)
  
  obs_figure(filtered_obs_data)
  h1 <- forecast_plot(date_list, location_level = location_level, state_level = state_level,
                      model_name = "GBQR", specific_h = 1, filtered_obs_data = filtered_obs_data)
  h2 <- forecast_plot(date_list, location_level = location_level, state_level = state_level,
                      model_name = "GBQR", specific_h = 2, filtered_obs_data = filtered_obs_data)
  h3 <- forecast_plot(date_list, location_level = location_level, state_level = state_level,
                      model_name = "GBQR", specific_h = 3, filtered_obs_data = filtered_obs_data)
  h4 <- forecast_plot(date_list, location_level = location_level, state_level = state_level,
                      model_name = "GBQR", specific_h = 4, filtered_obs_data = filtered_obs_data)
  dev.off()
  
  all_h <- rbind(h1,h2,h3,h4)
  
  wis <- list()
  state_inc <- filtered_obs_data %>%
    select(target_end_date = week_end, state, inc = inc_state) %>%
    mutate(hsa_nci_id = "All",
           location = paste(state, "All", sep = "_")) %>%
    distinct() %>%
    select(target_end_date, state, hsa_nci_id, location, inc)
  
  hsa_inc <- filtered_obs_data %>%
    select(target_end_date = week_end, state, hsa_nci_id, inc = inc_hsa) %>%
    mutate(location = paste(state, hsa_nci_id, sep = "_"),
           hsa_nci_id = as.character(hsa_nci_id)) %>%
    select(target_end_date, state, hsa_nci_id, location, inc)
  
  long_inc <-state_inc %>%
    bind_rows(hsa_inc)
  
  for(i in 1:length(date_list)){
    print(date_list[i])
    wis[[i]] <- get_wis(target_date = date_list[i], location_level = location_level, 
                        state_level = state_level, model_name ="GBQR", 
                        filtered_obs_data = long_inc)
  }
  wis_hsa <- map_dfr(wis, ~ .x$wis_hsa )
  wis_state <- map_dfr(wis, ~ .x$wis_state )
  
  wis_all <- wis_hsa %>%
    left_join(wis_state, 
              by = c("state", "hsa_nci_id",  "location", "target_end_date", "horizon", "reference_date"),
              suffix = c("", ".state"))
  
  df_all <- all_h %>%
    left_join(wis_all, 
              by = c("state", "hsa_nci_id",  "location", "target_end_date", "horizon", "reference_date"))
  
  return(df_all)
}

