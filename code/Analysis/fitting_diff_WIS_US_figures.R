library(dplyr)
library(ggplot2)
library(stringr)
library(dplyr)
all_metric2 <- read.csv("Local-Level-Forecasting/data/forecasting_metrics_overall.csv")
us_hsa_county_popdesc <- read.csv("Local-Level-Forecasting/data/us_hsa_county_popdesc.csv")

us_hsa_county_popdesc_bygroup <- us_hsa_county_popdesc %>%
  select(state, hsa_nci_id, area_km2_hsa, area_km2_state, n_hsa) %>% 
  distinct()

all_metric <- all_metric2 %>%
  left_join(us_hsa_county_popdesc_bygroup, by = c("state", "hsa_nci_id")) %>%
  mutate(rel_diff_wis_overall = (mean_wis.state_overall -mean_wis_overall)/mean_wis.state_overall )

############################
##### n_msa
############################
counties_sf <- counties(year = 2023, cb = TRUE) %>%
  st_transform(5070)  # equal-area (optional but stable for spatial ops)

cbsa <- core_based_statistical_areas(year = 2023) %>%
  st_transform(5070)

cbsa_metro <- cbsa %>%
  filter(MEMI == 1) %>%                  # 1=Metro, 2=Micro
  select(CBSAFP, CBSA_NAME = NAME)

county_pts <- st_point_on_surface(counties_sf) %>%
  select(GEOID)

county_state <- counties_sf %>%
  st_drop_geometry() %>%
  transmute(GEOID = GEOID, STATEFP = STATEFP)

county_cbsa_crosswalk <- st_join(
  county_pts,
  cbsa_metro,
  join = st_within,                      # safer for points
  left = TRUE
) %>%
  st_drop_geometry() %>%
  filter(!is.na(CBSAFP)) %>%
  distinct(GEOID, .keep_all = TRUE)


msa_count_by_state <- county_cbsa_crosswalk %>%
  left_join(county_state, by = "GEOID") %>%
  group_by(STATEFP) %>%
  summarise(n_msa = n_distinct(CBSAFP), .groups = "drop")

data("fips_codes")

msa_count_by_state <- msa_count_by_state %>%
  left_join(fips_codes %>% 
              distinct(state_code , state_name), 
            by = c("STATEFP" = "state_code")) %>%
  select(state = state_name, n_msa) 

############################

tmp_horizon <- function(ss, h){
  tmp <- all_metric %>% 
    filter(season == ss, horizon == h) %>%
    distinct() %>%
    mutate(density_ratio = density_hsa/density_state,
           area_ratio = area_km2_hsa/area_km2_state,
           log_pop_ratio = log(pop_ratio), 
           log_pct_urban = log(pct_urban),
           log_density_ratio = log(density_ratio),
           log_area_ratio = log(area_ratio),
           log_n_hsa = log(n_hsa),
           log_population_hsa = log(population_hsa),
           log_density_hsa = log(density_hsa)) %>%
    left_join(msa_count_by_state, by = "state")
}

tmp1 <- tmp_horizon(ss = '2023/24', h = 1)
tmp1 <- tmp1 %>%
  mutate(log_diff_wis_overall = log(diff_wis_overall + 0.01250801 + 0.01),
         available = 1)
tmp2 <- tmp_horizon(ss = '2023/24', h = 2)
tmp2 <- tmp2 %>%
  mutate(log_diff_wis_overall = log(diff_wis_overall + 0.07366552 + 0.01),
         available = 1)

tmp3 <- tmp_horizon(ss = '2023/24', h = 3)
tmp3 <- tmp3 %>%
  mutate(log_diff_wis_overall = log(diff_wis_overall + 0.1572375 + 0.01),
         available = 1)

tmp4 <- tmp_horizon(ss = '2023/24', h = 4)
tmp4 <- tmp4 %>%
  mutate(log_diff_wis_overall = log(diff_wis_overall + 0.2697638 + 0.01),
         available = 1)





hsa <- readxl::read_xls("Local-Level-Forecasting/data/Health.Service.Areas.xls") 
state_abbrevs <- read.csv("Local-Level-Forecasting/data/state-abbrevs.csv")

hsa <- hsa %>%
  mutate(`State-county` = str_remove(`State-county`, "\\s*\\(\\d+\\)$")) %>%
  tidyr::extract(`State-county`,
                 into = c("state_abbr", "county_name"),
                 regex = "^([A-Z]{2}):\\s*(.*)$" )
names(hsa) <- c("hsa_nci_id", "HSA Description", "state_abbr", "county_name", "fips")

n_hsa <- hsa %>%
  group_by(state_abbr) %>%
  summarise(
    n_hsa = n_distinct(hsa_nci_id, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(state_abbrevs, by = c("state_abbr" = "abbreviation"))


## US map data upload and filter


library(sf)
library(tigris)
library(stringr)
us_map <- readRDS("Local-Level-Forecasting/data/us_map_pop_sf.rds")

lower48 <- c(state.name, "District of Columbia")  # 48개 주 + DC
lower48 <- setdiff(lower48, c("Alaska", "Hawaii")) # 알래스카/하와이 제외

all_inc <- read.csv("Local-level-forecasting/data/hsa_state_inc.csv")


avail_hsa <- all_inc %>%
  select(state, hsa_nci_id, week_end) %>%
  group_by(state, hsa_nci_id) %>%
  summarise(max_week_end = max(week_end),
            min_week_end = min(week_end),
            n = n()) %>%
  filter(max_week_end == '2025-07-26')
us_map1 <- us_map %>% 
  filter(state %in% lower48) %>%
  left_join(avail_hsa %>%
              filter(n == 148), by = c("state", "hsa_nci_id")) %>%
  mutate(available = ifelse(is.na(n), 0, 1))


df_250_h <- function(us_map, tmp, h){
  df_250K <- us_map %>%
    filter(population_hsa >= 250000) %>%
    mutate(density_ratio = density_hsa/density_state,
           area_ratio = area_km2_hsa/area_km2_state,
           log_pop_ratio = log(pop_ratio),
           log_pct_urban = log(pct_urban)) %>%
    select(state, hsa_nci_id, population_hsa, population_state, density_state,
           density_hsa, pct_urban, pop_ratio, geometry_hsa, geometry, density_ratio, area_ratio,
           log_pop_ratio, log_pct_urban) %>%
    distinct() %>%
    left_join(tmp %>%
                ungroup() %>%
                select(state, hsa_nci_id, diff_wis_overall, log_diff_wis_overall, available),
              by = c("state", "hsa_nci_id")) %>%
    left_join(n_hsa %>% select(-state_abbr), by = "state") %>%
    left_join(msa_count_by_state, by = "state")
  return(df_250K)
}



df_250K_h1 <- df_250_h(us_map1, tmp = tmp1, h = 1)
df_250K_h2 <- df_250_h(us_map1, tmp = tmp2, h = 2)
df_250K_h3 <- df_250_h(us_map1, tmp = tmp3, h = 3)
df_250K_h4 <- df_250_h(us_map1, tmp = tmp4, h = 4)




wis_diff_hat_usmap <- function(df_conus, us_map, model_name, target_var, fill_var, target_name_var = NULL){
  p3 <- ggplot() +
    geom_sf(data = sf::st_set_geometry(sf::st_as_sf(df_conus), "geometry_hsa"),
            aes(fill = .data[[fill_var]]), color = 'gray20') +
    geom_sf(data = sf::st_set_geometry(sf::st_as_sf(us_map), "geometry"),
            fill = NA, color = "grey20", linewidth = 0.25) +
    geom_sf(data = sf::st_set_geometry(sf::st_as_sf(us_map), "geometry_hsa"),
            fill = NA, color = 'gray20') +
    #geom_sf(data = unavail_hsa,
    #        fill = NA, color = "yellow", linewidth = 0.5) +
    coord_sf() + 
    theme_void() +
    labs(title = "") +
    theme(
      legend.title = element_text(size = 15),
      legend.text  = element_text(size = 15),
    )
  if(!is.null(target_name_var)){
    p4 <- p3 +
      scale_fill_viridis_c(option = "magma", direction = -1,na.value = "white",
                           name = expression("Predicted " * Delta * " WIS"),
                           # limits = c(min(df_pred[[target_name_var]], na.rm = TRUE),
                           #            max(df_pred[[target_name_var]], na.rm = TRUE))
      ) 
    
  }else{
    p4 <- p3 +
      scale_fill_viridis_c(option = "magma", direction = -1,na.value = "white",
                           name = expression("Predicted " * Delta * " WIS")
                           #limits = c(min(df_pred[[target_var]], na.rm = TRUE),
                           #            max(df_pred[[target_var]], na.rm = TRUE))
      )
  }
  
  print(p4)
  
}


fit_predict_us_map <- function(gam_fit, df_250K, us_map, h, model_name, fill_var, target_name_var = NULL){
  predictors <- names(gam_fit$model)[-1]
  target_var <- names(gam_fit$model)[1]
  if(!is.null(target_name_var)){
    target_name_var <- target_name_var
  }
  newdata <- df_250K[, predictors, drop = FALSE]
  
  pred <- predict(
    gam_fit,
    newdata = newdata,
    type = "response",
    se.fit = TRUE
  )
  
  df_pred <- df_250K %>%
    mutate(
      pred_mean = as.numeric(pred$fit),
      pred_tran_mean = exp(pred_mean) + min(df_250K$diff_wis_overall, na.rm = TRUE) - 0.01,
      se_hat          = as.numeric(pred$se.fit),
      lwr95           = pred_mean - 1.96 * se_hat,
      upr95           = pred_mean + 1.96 * se_hat
    )
  
  p_gam <- wis_diff_hat_usmap(df_pred, us_map = us_map, model_name = model_name, target_var = target_var, fill_var = fill_var,
                              target_name_var = target_name_var)
  
  ggsave(paste0("Local-Level-Forecasting/paper_figures/", model_name, "_", h,"_USmap.png"),
         p_gam, width = 10, height = 5, dpi = 300)
}

library(mgcv)

lm_res1 <- lm(formula = log_diff_wis_overall~  log_pop_ratio * log_pct_urban + n_msa , data = tmp1) 
lm_res2 <- lm(formula = log_diff_wis_overall~  log_pop_ratio * log_pct_urban + n_msa , data = tmp2) 
lm_res3 <- lm(formula = log_diff_wis_overall~  log_pop_ratio * log_pct_urban + n_msa , data = tmp3) 

fit_predict_us_map(gam_fit = lm_res1, df_250K = df_250K_h1, h = 1, 
                   us_map = us_map1,
                   model_name = "LM_log_wis_log_var3_interaction_nmsa",
                   fill_var = "pred_tran_mean",
                   target_name_var = "diff_wis_overall")


fit_predict_us_map(gam_fit = lm_res3, df_250K = df_250K_h3, h = 3, 
                   us_map = us_map1,
                   model_name = "LM_log_wis_log_var3_interaction_nmsa",
                   fill_var = "pred_tran_mean",
                   target_name_var = "diff_wis_overall")


tmp1 <- tmp1 %>%
  mutate(
    fitted = fitted(lm_res1),
    trans_fitted = exp(fitted(lm_res1)) + min(tmp1$diff_wis_overall) - 0.01
  )

gam_res1 <- gam(formula = log_diff_wis_overall  ~  te(log_pop_ratio, n_msa) , data = tmp1) 

fit_predict_us_map(gam_fit = gam_res1, df_250K = df_250K_h1, us_map = us_map1, h = 1, 
                   model_name = "GAM_log_wis_log_var3",
                   fill_var = "pred_mean")

