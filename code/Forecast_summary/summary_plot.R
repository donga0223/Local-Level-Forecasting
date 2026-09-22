
source("code/Forecast_summary/summary_plot_ftn.R")

hsa_state_inc <- read.csv("data/hsa_state_inc.csv")

only_2years_df <- hsa_state_inc %>%
  dplyr::count(hsa_nci_id, name = "n") %>% 
  filter(n < 148) 


hsa_state_inc <- hsa_state_inc %>%
  filter(!state %in% c("District of Columbia", "Wyoming", "Hawaii", "Alaska", "Louisiana"),
         !hsa_nci_id %in% c('286', '414', '143', '193', '705', only_2years_df$hsa_nci_id),
         !season == "2025/26") %>%
  mutate(week_end = as.Date(week_end)) %>%
  arrange(state, week_end)

hsa_state_inc1 <- hsa_state_inc %>% filter(state != 'Connecticut')


mymodel = "US_NSSP_public_Joint_250K_pct"
#mymodel = "arima"

US_state_NSSP_250K_2425 <- cal_metrics(obs_data = hsa_state_inc1, mystate = NULL, 
                                       my_abb_state = mymodel,
                                       #location_level = NULL,
                                       state_level = NULL, 
                                       date_list = seq.Date(from = as.Date("2024-10-05"), 
                                                            to = as.Date("2025-03-30"), 
                                                            by = "week"), 
                                       pop_size = 250000,
                                       pdfname = paste(mymodel,"2425",sep="_"))



US_state_NSSP_250K_2324 <- cal_metrics(obs_data = hsa_state_inc1, mystate = NULL, 
                                       my_abb_state = mymodel,
                                       state_level = NULL, 
                                       date_list = seq.Date(from = as.Date("2023-10-07"), 
                                                            to = as.Date("2024-03-30"), 
                                                            by = "week"), 
                                       pop_size = 250000,
                                       pdfname = paste(mymodel,"2324",sep="_"))



US_state_NSSP_250K_2223 <- cal_metrics(obs_data = hsa_state_inc1, mystate = NULL, 
                                       my_abb_state = mymodel,
                                       state_level = NULL, 
                                       date_list = seq.Date(from = as.Date("2022-10-08"), 
                                                            to = as.Date("2023-03-30"), 
                                                            by = "week"), 
                                       pop_size = 250000,
                                       pdfname = paste(mymodel,"2223",sep="_"))






all_forecasting_metric <- rbind(US_state_NSSP_250K_2223, 
                                US_state_NSSP_250K_2324,
                                US_state_NSSP_250K_2425)

write.csv(all_forecasting_metric, "data_joint/forecasting_metrics_3seasons.csv", row.names = FALSE)
write.csv(all_forecasting_metric, "data_joint/arima_forecasting_metrics_3seasons.csv", row.names = FALSE)

all_forecasting_metric <- read.csv("data_joint/forecasting_metrics_3seasons.csv")


all_forecasting_metric1 <- all_forecasting_metric %>%
  mutate(
    season = case_when(
      reference_date >= as.Date("2022-08-01") & reference_date <= as.Date("2023-07-31") ~ "2022/23",
      reference_date >= as.Date("2023-08-01") & reference_date <= as.Date("2024-07-31") ~ "2023/24",
      reference_date >= as.Date("2024-08-01") & reference_date <= as.Date("2025-07-31") ~ "2024/25",
      TRUE ~ NA_character_
    ),
    diff_wis = wis.state- wis
  )

aa <- all_forecasting_metric1 %>% 
  group_by(season, horizon) %>%
  summarise(mean_coverage = mean(coverage),
            sd_coverage = sd(coverage),
            mean_coverage.state = mean(coverage.state),
            sd_coverage.state = sd(coverage.state),
            mean_MAPE = mean(MAPE, na.rm = TRUE),
            sd_MAPE = sd(MAPE, na.rm = TRUE),
            mean_MAPE.state = mean(MAPE.state),
            sd_MAPE.state = sd(MAPE.state),
            MWIS = mean(wis),
            sd_WIS = sd(wis),
            MWIS.state = mean(wis.state),
            sd_WIS.state = sd(wis.state),
            MAE = mean(abs(error)),
            sd_MAE = sd(abs(error)),
            MAE.state = mean(abs(error.state)),
            sd_MAE.state = sd(abs(error.state))
  )
     
bb <- all_forecasting_metric1 %>% 
  group_by(horizon) %>%
  summarise(mean_coverage = mean(coverage),
            sd_coverage = sd(coverage),
            mean_coverage.state = mean(coverage.state),
            sd_coverage.state = sd(coverage.state),
            mean_MAPE = mean(MAPE, na.rm = TRUE),
            sd_MAPE = sd(MAPE, na.rm = TRUE),
            mean_MAPE.state = mean(MAPE.state),
            sd_MAPE.state = sd(MAPE.state),
            MWIS = mean(wis),
            sd_WIS = sd(wis),
            MWIS.state = mean(wis.state),
            sd_WIS.state = sd(wis.state),
            MAE = mean(abs(error)),
            sd_MAE = sd(abs(error)),
            MAE.state = mean(abs(error.state)),
            sd_MAE.state = sd(abs(error.state))
  )


cc <- all_forecasting_metric1 %>%
  group_by(hsa_nci_id, horizon) %>%
  summarise(mean_coverage = mean(coverage),
            sd_coverage = sd(coverage),
            mean_coverage.state = mean(coverage.state),
            sd_coverage.state = sd(coverage.state),
            mean_MAPE = mean(MAPE, na.rm = TRUE),
            sd_MAPE = sd(MAPE, na.rm = TRUE),
            mean_MAPE.state = mean(MAPE.state),
            sd_MAPE.state = sd(MAPE.state),
            MWIS = mean(wis),
            sd_WIS = sd(wis),
            MWIS.state = mean(wis.state),
            sd_WIS.state = sd(wis.state),
            MAE = mean(abs(error)),
            sd_MAE = sd(abs(error)),
            MAE.state = mean(abs(error.state)),
            sd_MAE.state = sd(abs(error.state))
  )

cc %>%
  group_by(horizon) %>%
  summarise(mean(MWIS < MWIS.state),
            mean(MAE < MAE.state))


all_forecasting_metric1 %>%
  ggplot(aes(x = error, y = error.state)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~horizon)

all_forecasting_metric1 %>%
  ggplot(aes(x = wis, y = wis.state)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~horizon)




all_forecasting_metric1 %>%
  ggplot(aes(x = diff_wis)) +
  geom_boxplot() + 
  facet_wrap(season~horizon, scales = "free")

all_forecasting_metric1 %>%
  ggplot(aes(x = diff_wis, group = as.factor(horizon), color = as.factor(horizon))) +
  geom_boxplot() + 
  facet_wrap(~season, scales = "free")

all_forecasting_metric1 %>%
  ggplot(aes(x = diff_wis, group = as.factor(horizon), color = as.factor(horizon))) +
  geom_histogram() + 
  facet_wrap(~season, scales = "free")

all_forecasting_metric1 %>%
  ggplot(aes(x = diff_wis, group = as.factor(horizon), color = as.factor(horizon))) +
  geom_histogram() 

all_forecasting_metric1 %>%
  ggplot(aes(x = diff_wis)) +
  geom_histogram() +
  facet_wrap(~horizon)

all_forecasting_metric1 %>%
  ggplot(aes(x = diff_wis, group = as.factor(horizon), color = as.factor(horizon))) +
  geom_density() + 
  facet_wrap(~season, scales = "free")



diff_wis_season <- all_forecasting_metric1 %>% 
  group_by(state, hsa_nci_id, location, season, horizon) %>%
  summarise(diff_wis_season = mean(diff_wis),
  )


diff_wis_overall <- all_forecasting_metric1 %>% 
  group_by(state, hsa_nci_id, location, horizon) %>%
  summarise(diff_wis_overall = mean(diff_wis),
  )

hsa_county_popdesc <- read.csv("data/us_hsa_county_popdesc.csv")


forecasting_metrics_overall <- diff_wis_season %>%
  left_join(diff_wis_overall, by = c("state", "hsa_nci_id", "location", "horizon")) %>%
  left_join(hsa_county_popdesc %>%
              select(state, hsa_nci_id, population_state, population_hsa, 
                     pop_ratio, pct_urban, n_hsa, area_km2_state, area_km2_hsa,
                     density_state, density_hsa) %>%
              distinct() %>%
              mutate(hsa_nci_id = as.character(hsa_nci_id)),
            by = c("state", "hsa_nci_id"))

write.csv(forecasting_metrics_overall, "data_joint/forecasting_metrics_overall.csv", row.names = FALSE)
write.csv(forecasting_metrics_overall, "data_joint/arima_forecasting_metrics_overall.csv", row.names = FALSE)

forecasting_metrics_overall <- read.csv("data_joint/forecasting_metrics_overall.csv")


forecasting_metrics_overall %>%
  ggplot(aes(x = diff_wis_overall)) + 
  geom_histogram() + 
  facet_wrap(~horizon)

forecasting_metrics_overall %>%
  filter(season == "2023/24", )

##### If you want to select only a few areas, you can choose them for the plot
source("code/Forecast_summary/summary_plot_forpaper_ftn.R")

hsa_state_inc_selected <- hsa_state_inc %>%
  filter(hsa_nci_id %in% c(153, 825, 287, 373, 101, 22, 54, 83, 167, 198, 408, 453))


hsa_state_inc_selected <- hsa_state_inc %>%
  filter(hsa_nci_id %in% c(408, 415, 434, 865, 277, 902)) #170))


## This is for paper
hsa_state_inc_selected <- hsa_state_inc %>%
  filter(hsa_nci_id %in% c(405, 538, 434, 170, 826, 865)) #170, 289))

mymodel = "US_NSSP_public_Joint_250K_pct"
mymodel = "arima"
US_state_NSSP_250K_2425_selected <- cal_metrics(obs_data = hsa_state_inc_selected, mystate = NULL, 
                                                my_abb_state = mymodel,
                                                state_level = NULL, 
                                                date_list = seq.Date(from = as.Date("2024-10-05"), 
                                                                     to = as.Date("2025-03-30"), 
                                                                     by = "week"), 
                                                pop_size = 250000,
                                                pdfname = paste(mymodel, "2425_selected", sep = "_"))
facet_map_paper <- c(
  "826" = "Cherokee, GA",
  "865" = "Rockland, NY",
  "170" = "Durham, NC",
  "405" = "Amarillo, TX",
  "434" = "Fort Worth, TX",
  "538" = "Laredo, TX"
)

facet_levels_paper <- c(
  "Cherokee, GA",
  "Rockland, NY",
  "Durham, NC",
  "Amarillo, TX",
  "Laredo, TX",
  "Fort Worth, TX"
)

forecast_compare_plot(df_all = US_state_NSSP_250K_2425_selected, 
                      #location_order = c(277, 902, 865, 408, 415, 434),
                      date_list = seq.Date(from = as.Date("2024-10-05"), 
                                           to = as.Date("2025-03-30"), 
                                           by = "week"),
                      facet = "paper",
                      facet_map = facet_map_paper, 
                      facet_levels = facet_levels_paper,
                      pdfname = "paper_2425")


US_state_NSSP_250K_2324_selected <- cal_metrics(obs_data = hsa_state_inc_selected, mystate = NULL, 
                                                my_abb_state = mymodel,
                                                state_level = NULL, 
                                                date_list = seq.Date(from = as.Date("2023-10-07"), 
                                                                     to = as.Date("2024-03-30"), 
                                                                     by = "week"), 
                                                pop_size = 250000,
                                                pdfname = paste(mymodel, "2324_selected", sep = "_"))
                                   


forecast_compare_plot(df_all = US_state_NSSP_250K_2324_selected, 
                      #location_order = c(277, 902, 865, 408, 415, 434),
                      date_list = seq.Date(from = as.Date("2023-10-07"), 
                                           to = as.Date("2024-03-30"), 
                                           by = "week"),
                      facet = "paper",
                      facet_map = facet_map_paper, 
                      facet_levels = facet_levels_paper,
                      pdfname = "paper_2324")



US_state_NSSP_250K_2223_selected <- cal_metrics(obs_data = hsa_state_inc_selected, mystate = NULL, 
                                                my_abb_state = mymodel,
                                                state_level = NULL, 
                                                date_list = seq.Date(from = as.Date("2022-10-08"), 
                                                                     to = as.Date("2023-03-30"), 
                                                                     by = "week"), 
                                                pop_size = 250000,
                                                pdfname = paste(mymodel, "2223_selected", sep = "_"))
                            


forecast_compare_plot(df_all = US_state_NSSP_250K_2223_selected, 
                      #location_order = c(277, 902, 865, 408, 415, 434),
                      date_list = seq.Date(from = as.Date("2022-10-08"), 
                                           to = as.Date("2023-03-30"), 
                                           by = "week"),
                      facet = "paper",
                      facet_map = facet_map_paper, 
                      facet_levels = facet_levels_paper,
                      pdfname = "paper_2223")






### least discrepancy 6 HSAs
## This is for paper
hsa_state_inc_least <- hsa_state_inc %>%
  filter(hsa_nci_id %in% c(708, 688, 16, 707, 36, 689)) 

US_state_NSSP_250K_2324_least <- cal_metrics(obs_data = hsa_state_inc_least, mystate = NULL, 
                                                my_abb_state = "US_NSSP_public_Joint_250K_pct",
                                                state_level = NULL, 
                                                date_list = seq.Date(from = as.Date("2023-10-07"), 
                                                                     to = as.Date("2024-03-30"), 
                                                                     by = "week"), 
                                                pop_size = 250000,
                                                pdfname = "US_NSSP_public_HSA_250K_2324_least")


facet_map_least <- c(
  "708" = "Salt Lake City, UT",
  "688" = "Denver, CO",
  "16" = "Baltimore, MD",
  #"869" = "Montgomery, MD",
  "707" = "Las Vegas, NV",
  "36" = "Jersey City, NJ",
  "689" = "Portland, OR"
)
facet_levels_least <- c("Salt Lake City, UT",
                        "Denver, CO",
                        "Baltimore, MD",
                        #"Montgomery, MD",
                        "Las Vegas, NV",
                        "Jersey City, NJ",
                        "Portland, OR")

forecast_compare_plot(df_all = US_state_NSSP_250K_2324_least, 
                      #location_order = c(277, 902, 865, 408, 415, 434),
                      date_list = seq.Date(from = as.Date("2023-10-07"), 
                                           to = as.Date("2024-03-30"), 
                                           by = "week"),
                      facet = "paper",
                      facet_map = facet_map_least, facet_levels = facet_levels_least,
                      pdfname = "paper_least_2324")


