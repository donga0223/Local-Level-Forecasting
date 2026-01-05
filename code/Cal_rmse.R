###########################################################################
## Load nssp data 
## create df_all : "week_end", "state", "hsa_nci_id", "population_hsa", "inc_hsa",
##                 "epi_week_hsa", "epi_year", "season_week", "season", "inc_state",
##                 "epi_week_state", "included" 
## create peak_all : "season", "state", "hsa_nci_id", "population_hsa", "week_end_hsa",
##                   "season_week_hsa", "inc_hsa", "week_end_state", "season_week_state",
##                   "inc_state", "RMSE_overall", "MAE_overall", "included", "RMSE_season",
##                   "MAE_season", "diff_peak_week", "diff_peak_magnitude", 
##                   "rel_diff_peak_magnitude"
###########################################################################

library(dplyr)
library(stringr)
library(tidyr)
library(epidatr)
library(here)
library(ggplot2)
library(ggrepel) 

source("Local-Level-Forecasting/code/rmse_season_ftn.R")

### Load nssp county level data

df <- epidatr::pub_covidcast(source = "nssp", 
                             signals = "pct_ed_visits_influenza", 
                             geo_type = "county", 
                             geo_values = "*", 
                             time_type = "week")

us_hsa_county_popdesc <- read.csv("Local-Level-Forecasting/data/us_hsa_county_popdesc.csv")
us_hsa_county_popdesc <- us_hsa_county_popdesc %>%
  mutate(
    fips = str_pad(as.character(fips), width = 5, side = "left", pad = "0")
    )

df1 <- df %>%
  mutate(week_end = time_value + 6) %>%
  rename(fips = geo_value,
         inc = value) %>%
  select(week_end, fips, inc) 

colSums(is.na(df1))

df2 <- df1 %>%
  left_join(us_hsa_county_popdesc %>%
              select(fips, county, state, hsa_nci_id, population_state,
                     population_hsa, pct_urban, pop_ratio, density_state, density_hsa,
                     area_km2_hsa, area_km2_state, n_hsa) %>%
              distinct(), by = "fips")

colSums(is.na(df2))

df2 %>% filter(is.na(state)) %>%
  select(fips) %>%
  distinct()

df3 <- df2 %>%
  filter(!is.na(state))


######### Note!! #############################################
### The Health.Service.Areas.xls dataset does not include FIPS 51019 or 51515.
### Accordingly, we exclude these codes from our analysis.
###############################################################

### Load nssp state level data 

df_state <- epidatr::pub_covidcast(source = "nssp", 
                             signals = "pct_ed_visits_influenza", 
                             geo_type = "state", 
                             geo_values = "*", 
                             time_type = "week")

state_abbrevs <- read.csv("Local-Level-Forecasting/data/state-abbrevs.csv")

df_state1 <- df_state %>%
  mutate(week_end = time_value + 6,
         abb = toupper(geo_value)) %>%
  rename(inc = value) %>%
  select(week_end, abb, inc) %>%
  left_join(state_abbrevs, by = c("abb" = "abbreviation"))

colSums(is.na(df_state1))


### join state and hsa data

df_hsa <- df3 %>%
  select(week_end, state, hsa_nci_id, population_state, population_hsa, pct_urban,
         pop_ratio, density_state, density_hsa, area_km2_hsa, area_km2_state, n_hsa, inc) %>%
  distinct()

df_hsa_season <- add_season(df_hsa)
df_state_season <- add_season(df_state1)

df_all <- df_hsa_season %>%
  left_join(df_state_season %>%
              select(-abb), by = c("week_end", "state", "season", "season_week", 
                                   "epi_year", "epi_week"),
            suffix = c("_hsa", "_state")) %>%
  filter(season %in% c("2022/23", "2023/24", "2024/25"))


colSums(is.na(df_all))
only_2years_df <- df_all %>%
  count(hsa_nci_id, name = "n") %>% 
  filter(n < 148)

seasons_order <- c("2022/23","2023/24","2024/25")

df_all %>%
  filter(hsa_nci_id %in% only_2years_df$hsa_nci_id) %>%
  count(state, hsa_nci_id, season, name = "n") %>%
  pivot_wider(
    names_from  = season,
    values_from = n,
    values_fill = 0L,                    # if no data : 0
    names_glue  = "{season}"             # ex: 2022/23, 2023/24 ...
  ) %>%
  arrange(state, hsa_nci_id) %>% 
  print(n = 30)


df_all %>%
  filter(state == "Pennsylvania", hsa_nci_id == "8") %>%
  ggplot(aes(x=as.Date(week_end))) +
    geom_line(aes(y = inc_hsa, color = "red")) +
    geom_line(aes(y = inc_state, color = "blue"))


write.csv(df_all, "Local-Level-Forecasting/data/hsa_state_inc.csv", row.names = FALSE)

### 27 HSA areas (25 Pennsylvania, 2 Rhode Island) don't have 24/25 season's data
### remove from analysis



df_all_season_rmse <- compute_pairwise_rmse_byseason(df_all)




df_all_season_rmse_inc <- df_all_season_rmse %>% 
  mutate(included = ifelse(population_hsa >= 250000, "Included", "Excluded")) %>%
  mutate(included = ifelse(is.na(included), "Excluded", included)) %>%
  mutate(included = ifelse(state %in% c("District of Columbia", "Wyoming", "Hawaii", "Alaska"), "Excluded", included)) %>%
  mutate(included = ifelse(hsa_nci_id %in% c('286', '414', '143', '193', '705', only_2years_df$hsa_nci_id), "Excluded", included)) 

### peak_time, magnitude (hsa)
peak_time <- df_hsa_season %>%
  group_by(season, state, hsa_nci_id) %>%
  slice_max(order_by = inc, n = 1, with_ties = FALSE) %>%
  dplyr::select(season, state, hsa_nci_id, population_hsa, week_end, season_week, inc)


### peak_time, magnitude (state)
peak_time_state <- df_state_season %>%
  group_by(season, state) %>%
  slice_max(order_by = inc, n = 1, with_ties = FALSE) %>%
  dplyr::select(season, state, week_end, season_week, inc)

df_all_season_rmse_inc_long <- df_all_season_rmse_inc %>%
  rename(RMSE_overall = RMSE, MAE_overall = MAE) %>%      # ← 충돌 방지
  pivot_longer(
    cols = matches("^(RMSE|MAE)_\\d{4}/\\d{2}$"),
    names_to = c(".value", "season"),
    names_pattern = "^(RMSE|MAE)_(\\d{4}/\\d{2})$"
  ) %>%
  relocate(season, .after = hsa_nci_id) %>%
  rename(RMSE_season = RMSE, MAE_season = MAE)

peak_all <- peak_time %>%
  left_join(peak_time_state, by = c("season", "state"),
            suffix = c("_hsa", "_state")) %>%
  left_join(df_all_season_rmse_inc_long, by = c("state", "hsa_nci_id", "population_hsa", "season")) %>%
  mutate(diff_peak_week = season_week_hsa - season_week_state,
         diff_peak_magnitude = inc_hsa - inc_state,
         rel_diff_peak_magnitude = (inc_hsa - inc_state)/inc_hsa,
         ratio_magnitude = inc_hsa/inc_state) 
  

write.csv(peak_all, "Local-Level-Forecasting/data/hsa_state_peak_all.csv", row.names = FALSE)

peak_all <- read.csv("Local-Level-Forecasting/data/hsa_state_peak_all.csv")
peak_all <- peak_all %>%
  mutate(included = tolower(included),
         season = gsub("/", "-", season))
#### figure 1
#pdf("Local-Level-Forecasting/figures/NSSP_data_description_RMSE.pdf", width = 15, height = 10)
pdf("Local-Level-Forecasting/figures/NSSP_data_description_RMSE1.pdf", width = 15, height = 10)


theme_form <- theme(
  text       = element_text(colour = "black"),
  axis.text  = element_text(size = 30, colour = "black"),
  axis.title = element_text(size = 34, colour = "black"),
  strip.text = element_text(size = 30, colour = "black"),
  plot.title = element_text(size = 40, colour = "black"),
  legend.text  = element_text(size = 30, colour = "black"),
  
  ## facet 위/아래 간격 (기존 0 → 약간 띄우기)
  panel.spacing.y = unit(2, "lines"),
  panel.spacing.x = unit(1, "lines"),
  # 그리드 제거 
  #panel.grid = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  ## y축 제목과 숫자 사이 간격
  axis.title.y = element_text(
    size = 34, colour = "black", #face = "bold", 
    margin = margin(r = 25)   # 오른쪽 여백 → 숫자와 더 띄우기
  ),
  axis.title.x = element_text(
    size   = 34,
    colour = "black",
    margin = margin(t = 25)     # 제목 위쪽 여백 (아래로 떨어져 보이게)
  ),
  
  axis.text.y = element_text(size = 28, colour = "black"),
  axis.text.x = element_text(size = 28, colour = "black"),
  
  ## x, y tick 은 유지 (명시적으로 써줘도 됨)
  #axis.ticks.x = element_line(colour = "black", linewidth = 1.5),
  #axis.ticks.y = element_line(colour = "black", linewidth = 1.5),
  #axis.ticks.length = unit(0.4, "cm"),
  
  #legend.position = c(0.9, 0.9),
  #legend.position = "bottom",
  legend.position = c(0.98, 0.98),
  legend.justification = c(1, 1),
  legend.key.height = unit(2, "cm"),
  legend.key.width = unit(2.5, "cm"),
  legend.background = element_rect(
    colour = "black",   # 테두리 색
    fill = "white",     # 배경색
    linewidth = 0.5
  ),
  legend.margin = margin(
    t = 0, r = 12, b = 6, l = 0,   # 단위: pt
    unit = "pt"
  )
)

### G1
peak_all %>%
  filter(season %in% c("2022-23", "2023-24", "2024-25")) %>%
  select(population_hsa, RMSE_overall, included, hsa_nci_id) %>%
  distinct() %>%
  filter(RMSE_overall!=0) %>%
  ggplot(aes(x = log10(population_hsa), y = log10(RMSE_overall), color = included)) +
  geom_point(size = 5, alpha = 0.8) +
  geom_vline(xintercept = log10(250000), linetype = "dashed", color = "gray40") +
  #annotate("text", x = 250000, y = max(rmse_res1$RMSE, na.rm = TRUE) - 3, 
  #         label = "250,000", vjust = -0.5, hjust = 0, size = 5) +
  scale_color_manual(values = c("included" = "deepskyblue", "excluded" = "deeppink")) +
  labs(
    x = "Population size (log)",
    y = "Local-state difference (log RMSE)",
    color = ""
  ) +
  theme_minimal() + 
  theme_form
  
## G2
peak_all %>%
  filter(season %in% c("2022-23", "2023-24", "2024-25")) %>%
  select(population_hsa, RMSE_overall, included, hsa_nci_id) %>%
  distinct() %>%
  ggplot(aes(x = population_hsa, y = RMSE_overall, color = included)) +
  geom_point(size = 5, alpha = 0.8) +
  geom_vline(xintercept = 250000, linetype = "dashed", color = "gray40") +
  #annotate("text", x = 250000, y = max(rmse_res1$RMSE, na.rm = TRUE) - 3, 
  #         label = "250,000", vjust = -0.5, hjust = 0, size = 5) +
  scale_color_manual(values = c("included" = "deepskyblue", "excluded" = "deeppink")) +
  labs(
    x = "Population size",
    y = "Local-state difference (RMSE)",
    color = ""
  ) +
  theme_minimal() + 
  theme_form
if(2==3){
  geom_text_repel(
    data = peak_all %>% 
      filter(season %in% c("2022-23", "2023-24", "2024-25")) %>%
      ungroup() %>%
      filter(hsa_nci_id %in% c('286', '414', '143', '193', '705') | 
               state %in% c("District of Columbia", "Wyoming", "Hawaii", "Alaska"),
             population_hsa >= 250000) %>%
      select(hsa_nci_id, population_hsa, RMSE_overall) %>%  
      distinct(),
    aes(label = hsa_nci_id),
    color = "black",    
    size = 7,
    box.padding = 0.3,
    point.padding = 0.3,
    max.overlaps = Inf
  ) 
}


## G3
peak_all %>%
  filter(population_hsa >= 250000,
         !state %in% c("District of Columbia", "Wyoming", "Hawaii", "Alaska"),
         !hsa_nci_id %in% c('286', '414', '143', '193', '705', only_2years_df$hsa_nci_id)) %>%
  filter(included == "included",
         !season == "2025-26") %>% 
  ggplot(aes(x = diff_peak_week, y = ratio_magnitude, color = RMSE_overall)) +
  geom_jitter(size = 5, width = 0.3, height = 0.3) +
  scale_color_viridis_c(option = "magma", direction = -1) +  
  #scale_shape_manual(values = c(16, 17, 15)) +  # ● ▲ ■ ) +
  labs(
    #title = "Filtering by Population Size for Simulation",
    x = "Peak week difference (local - state)",
    y = "Peak magnitude difference (local/state)",
    color = "RMSE"
  ) +
  #facet_wrap(~season) +
  theme_minimal() +
  theme_form


cols <- c(
  "2022-23" = "black", 
  "2023-24" = "firebrick3",
  "2024-25" = "royalblue"
)


peak_all %>%
  filter(population_hsa >= 250000,
         !state %in% c("District of Columbia", "Wyoming", "Hawaii", "Alaska"),
         !hsa_nci_id %in% c('286','414','143','193','705', only_2years_df$hsa_nci_id)) %>%
  filter(included == "included",
         season != "2025-26") %>%
  mutate(season = factor(season, levels = c("2022-23","2023-24","2024-25"))) %>%
  ggplot(aes(x = diff_peak_week, y = ratio_magnitude, color = season)) +
  #geom_point(size = 3, alpha = 0.8) +  
  geom_vline(xintercept = 0, linewidth = 0.8, color = "gray") +
  geom_hline(yintercept = 1, linewidth = 0.8, color = "gray") +
  
  geom_jitter(size = 5, width = 0.1, height = 0.1, alpha = 0.8) +
  # 1) viridis (이산형)
  scale_color_manual(values = cols, name = "") +
  labs(
    x = "Peak week difference (local - state)",
    y = "Peak magnitude ratio (local/state)"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(-0.5, 2.825784)) +
  theme_form

dev.off()


#G10 (RMSE)
peak_all %>%
  filter(population_hsa >= 250000,
         !state %in% c("District of Columbia", "Wyoming", "Hawaii", "Alaska"),
         !hsa_nci_id %in% c('286', '414', '143', '193', '705', only_2years_df$hsa_nci_id)) %>%
  filter(included == "included",
         !season == "2025-26") %>% 
  ggplot(aes(x = diff_peak_week, y = diff_peak_magnitude, color = RMSE_season,
             shape = season)) +
  geom_jitter(size = 5, width = 0.3, height = 0.3) +
  scale_color_viridis_c(option = "magma", direction = -1) +  
  scale_shape_manual(values = c(16, 17, 15)) +  # ● ▲ ■ ) +
  labs(
    #title = "Filtering by Population Size for Simulation",
    x = "Difference in peak week timing (HSA - State)",
    y = "Difference in peak magnitude (HSA - State)",
    color = "RMSE"
  ) +
  #facet_wrap(~season) +
  theme_minimal() +
  theme(
    text       = element_text(colour = "black"),
    axis.text  = element_text(size = 30, colour = "black"),
    axis.title = element_text(size = 34, face = "bold", colour = "black"),
    strip.text = element_text(size = 30, face = "bold", colour = "black"),
    plot.title = element_text(size = 40, face = "bold", colour = "black"),
    legend.text  = element_text(size = 28, colour = "black"),
    
    ## facet 위/아래 간격 (기존 0 → 약간 띄우기)
    panel.spacing.y = unit(2, "lines"),
    panel.spacing.x = unit(1, "lines"),
    # 그리드 제거 
    #panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    ## y축 제목과 숫자 사이 간격
    axis.title.y = element_text(
      size = 34, colour = "black", #face = "bold", 
      margin = margin(r = 18)   # 오른쪽 여백 → 숫자와 더 띄우기
    ),
    axis.text.y = element_text(size = 28, colour = "black"),
    axis.text.x = element_text(size = 28, colour = "black"),
    
    ## x, y tick 은 유지 (명시적으로 써줘도 됨)
    axis.ticks.x = element_line(colour = "black", linewidth = 1.5),
    axis.ticks.y = element_line(colour = "black", linewidth = 1.5),
    axis.ticks.length = unit(0.4, "cm"),
    
    legend.position = c(0.9, 0.9),
    #legend.position = "bottom",
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(2.5, "cm"),
  )



## G11 (MAE)
peak_all %>%
  filter(population_hsa >= 250000,
         !state %in% c("District of Columbia", "Wyoming", "Hawaii", "Alaska"),
         !hsa_nci_id %in% c('286', '414', '143', '193', '705', only_2years_df$hsa_nci_id)) %>%
  filter(included == "included",
         !season == "2025-26") %>% 
  ggplot(aes(x = diff_peak_week, y = diff_peak_magnitude, color = MAE_season,
             shape = season)) +
  geom_jitter(size = 5, width = 0.3, height = 0.3) +
  scale_color_viridis_c(option = "magma", direction = -1) +  
  scale_shape_manual(values = c(16, 17, 15)) +  # ● ▲ ■ ) +
  labs(
    #title = "Filtering by Population Size for Simulation",
    x = "Difference in peak week timing (HSA - State)",
    y = "Difference in peak magnitude (HSA - State)",
    color = "MAE"
  ) +
  #facet_wrap(~season) +
  theme_minimal() +
  theme(
    text       = element_text(colour = "black"),
    axis.text  = element_text(size = 30, colour = "black"),
    axis.title = element_text(size = 34, face = "bold", colour = "black"),
    strip.text = element_text(size = 30, face = "bold", colour = "black"),
    plot.title = element_text(size = 40, face = "bold", colour = "black"),
    legend.text  = element_text(size = 28, colour = "black"),
    
    ## facet 위/아래 간격 (기존 0 → 약간 띄우기)
    panel.spacing.y = unit(2, "lines"),
    panel.spacing.x = unit(1, "lines"),
    # 그리드 제거 
    #panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    ## y축 제목과 숫자 사이 간격
    axis.title.y = element_text(
      size = 34, colour = "black", #face = "bold", 
      margin = margin(r = 18)   # 오른쪽 여백 → 숫자와 더 띄우기
    ),
    axis.text.y = element_text(size = 28, colour = "black"),
    axis.text.x = element_text(size = 28, colour = "black"),
    
    ## x, y tick 은 유지 (명시적으로 써줘도 됨)
    axis.ticks.x = element_line(colour = "black", linewidth = 1.5),
    axis.ticks.y = element_line(colour = "black", linewidth = 1.5),
    axis.ticks.length = unit(0.4, "cm"),
    
    legend.position = c(0.9, 0.9),
    #legend.position = "bottom",
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(2.5, "cm"),
  )



#G10 (RMSE overall)
peak_all %>%
  filter(population_hsa >= 250000,
         !state %in% c("District of Columbia", "Wyoming", "Hawaii", "Alaska"),
         !hsa_nci_id %in% c('286', '414', '143', '193', '705', only_2years_df$hsa_nci_id)) %>%
  filter(included == "included",
         !season == "2025-26") %>% 
  ggplot(aes(x = diff_peak_week, y = diff_peak_magnitude, color = RMSE_overall)) +
  geom_jitter(size = 5, width = 0.3, height = 0.3) +
  scale_color_viridis_c(option = "magma", direction = -1) +  
  #scale_shape_manual(values = c(16, 17, 15)) +  # ● ▲ ■ ) +
  labs(
    #title = "Filtering by Population Size for Simulation",
    x = "Difference in peak week timing (HSA - State)",
    y = "Difference in peak magnitude (HSA - State)",
    color = "RMSE (overall)"
  ) +
  #facet_wrap(~season) +
  theme_minimal() +
  theme(
    #legend.position = "bottom",
    legend.key.size = unit(3, "lines"),
    text = element_text(size = 30),             # 기본 텍스트 크기
    axis.title = element_text(size = 30),       # x, y 축 제목 크기
    axis.text = element_text(size = 30),        # x, y 축 눈금 텍스트 크기
    legend.title = element_text(size = 30),     # 범례 제목 크기
    legend.text = element_text(size = 30),      # 범례 항목 크기
    plot.title = element_text(size = 30, face = "bold")  # 그래프 제목
  )


dev.off()

big_peak_week_small_magnitude <- peak_all %>% 
  filter(population_hsa >= 250000,
         !state %in% c("District of Columbia", "Wyoming", "Hawaii", "Alaska"),
         !hsa_nci_id %in% c('286', '414', '143', '193', '705', only_2years_df$hsa_nci_id)) %>%
  filter(included == "Included",
         !season == "2025-26") %>% 
  filter(abs(diff_peak_week) > 8, abs(diff_peak_magnitude) < 2) 

big_peak_week_small_magnitude %>%
  print(n = 45)

df_all %>% filter(hsa_nci_id %in% big_peak_week_small_magnitude$hsa_nci_id) %>%
  ggplot(aes(x = as.Date(week_end))) +
  geom_line(aes(y = inc_hsa), color = "red") + 
  geom_line(aes(y = inc_state), color = "blue") +
  facet_wrap(~hsa_nci_id)

peak_all %>% 
  filter(population_hsa >= 250000,
         !state %in% c("District of Columbia", "Wyoming", "Hawaii", "Alaska"),
         !hsa_nci_id %in% c('286', '414', '143', '193', '705', only_2years_df$hsa_nci_id)) %>%
  filter(included == "Included",
         !season == "2025-26") %>%
  ggplot(aes(x = season, y = RMSE_season)) +
  geom_boxplot(fill = "skyblue", alpha = 0.6, outlier.color = "red") +
  labs(
    title = "Distribution of RMSE by Season",
    x = "Season",
    y = "RMSE"
  ) +
  theme_minimal()


peak_all %>% 
  filter(population_hsa >= 250000,
         !state %in% c("District of Columbia", "Wyoming", "Hawaii", "Alaska"),
         !hsa_nci_id %in% c('286', '414', '143', '193', '705', only_2years_df$hsa_nci_id)) %>%
  filter(included == "Included",
         !season == "2025-26") %>%
  ggplot(aes(x = RMSE_season, color = season)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Density Plot of RMSE by Season",
    x = "RMSE",
    y = "Density"
  ) +
  theme_minimal()

peak_all %>% 
  filter(
    population_hsa >= 250000,
    !state %in% c("District of Columbia", "Wyoming", "Hawaii", "Alaska"),
    !hsa_nci_id %in% c('286', '414', '143', '193', '705', only_2years_df$hsa_nci_id),
    included == "Included",
    season != "2025-26"
  ) %>%
  ggplot(aes(x = RMSE_season, fill = season)) +
  geom_histogram(position = "identity", alpha = 0.8, bins = 30) +
  labs(
    title = "Histogram of RMSE by Season",
    x = "RMSE",
    y = "Count"
  ) +
  facet_wrap(~season) +
  theme_minimal()

View(peak_all %>% 
  filter(population_hsa >= 250000,
         !state %in% c("District of Columbia", "Wyoming", "Hawaii", "Alaska"),
         !hsa_nci_id %in% c('286', '414', '143', '193', '705', only_2years_df$hsa_nci_id)) %>%
  filter(included == "Included",
         !season == "2025-26") %>%
  filter(season == "2024-25") %>%
  arrange(desc(RMSE_season)) )


#df_all %>% filter(hsa_nci_id %in% c(538, 405, 170, 235, 277, 299, 826,154)) %>% ## by RMSE
#df_all %>% filter(hsa_nci_id %in% c(287, 408, 453, 22, 274, 153,688, 83, 66, 16,410,707)) %>% ## by popsize
df_all %>% filter(hsa_nci_id %in% c(153, 825, 287, 373, 101, 22, 54, 83, 167, 198, 408, 453)) %>% 
  filter(week_end >= '2024-09-01') %>%
  ggplot(aes(x = as.Date(week_end))) +
  geom_line(aes(y = inc_hsa), color = "red") + 
  geom_line(aes(y = inc_state), color = "blue") +
  facet_wrap(state~hsa_nci_id)

538,405, 434
170,235, 167, 225
277, 299
826, 154
390, 308
