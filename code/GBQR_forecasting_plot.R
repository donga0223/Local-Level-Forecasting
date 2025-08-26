
source("Local-Level-Forecasting/code/forecasting_summary_plot_ftn.R")

hsa_state_inc <- read.csv("Local-Level-Forecasting/data/hsa_state_inc.csv")

only_2years_df <- hsa_state_inc %>%
  count(hsa_nci_id, name = "n") %>% 
  filter(n < 148) 


hsa_state_inc <- hsa_state_inc %>%
  filter(!state %in% c("District of Columbia", "Wyoming", "Hawaii", "Alaska"),
       !hsa_nci_id %in% c('286', '414', '143', '193', '705', only_2years_df$hsa_nci_id),
       !season == "2025/26") %>%
  mutate(week_end = as.Date(week_end)) %>%
  arrange(state, week_end)



US_state_NSSP_250K_2425_selected %>% 
  filter(horizon == 3) %>%
  ggplot(aes(x = target_end_date)) +
  geom_line(aes(y = wis), color = 'orange') +
  geom_line(aes(y = wis.state), color = 'gray30') +
  facet_wrap(~hsa_nci_id)

US_state_NSSP_250K_2425 <- cal_metrics(obs_data = hsa_state_inc, mystate = NULL, 
                                       my_abb_state = "US_NSSP_public_HSA_250K_pct",
                                       state_level = "US_NSSP_public_state_pct", 
                                       date_list = seq.Date(from = as.Date("2024-11-16"), 
                                                            to = as.Date("2025-03-30"), 
                                                            by = "week"), 
                                       pop_size = 250000,
                                       pdfname = "US_NSSP_public_HSA_250K_2425")


US_state_NSSP_250K_2324 <- cal_metrics(obs_data = hsa_state_inc, mystate = NULL, 
                                       my_abb_state = "US_NSSP_public_HSA_250K_2425_to_2122_pct",
                                       state_level = "US_NSSP_public_state_2425_to_2122_pct", 
                                       date_list = seq.Date(from = as.Date("2023-10-07"), 
                                                            to = as.Date("2024-03-30"), 
                                                            by = "week"), 
                                       pop_size = 250000,
                                       pdfname = "US_NSSP_public_HSA_250K_2324")



US_state_NSSP_250K_2223 <- cal_metrics(obs_data = hsa_state_inc, mystate = NULL, 
                                       my_abb_state = "US_NSSP_public_HSA_250K_2425_to_2122_pct",
                                       state_level = "US_NSSP_public_state_2425_to_2122_pct", 
                                       date_list = seq.Date(from = as.Date("2022-10-08"), 
                                                            to = as.Date("2023-03-30"), 
                                                            by = "week"), 
                                       pop_size = 250000,
                                       pdfname = "US_NSSP_public_HSA_250K_2223")


all_forecasting_metric <- rbind(US_state_NSSP_250K_2223, 
                                US_state_NSSP_250K_2324,
                                US_state_NSSP_250K_2425)

write.csv(all_forecasting_metric, "Local-Level-Forecasting/data/forecasting_metrics_3seasons.csv", row.names = FALSE)


##### If you want to select only a few areas, you can choose them for the plot
hsa_state_inc_selected <- hsa_state_inc %>%
  filter(state %in% c("Texas"))


US_state_NSSP_250K_2425_selected <- cal_metrics(obs_data = hsa_state_inc_selected, mystate = NULL, 
                                                my_abb_state = "US_NSSP_public_HSA_250K_pct",
                                                state_level = "US_NSSP_public_state_pct", 
                                                date_list = seq.Date(from = as.Date("2024-11-16"), 
                                                                     to = as.Date("2025-03-30"), 
                                                                     by = "week"), 
                                                pop_size = 250000,
                                                pdfname = "US_NSSP_public_HSA_250K_2425_selected")
