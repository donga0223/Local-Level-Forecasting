
library(dplyr)
library(MMWRweek)  # MMWRweek()

# count last year MMWR week 
epiweeks_in_year <- function(year) {
  jan1  <- as.Date(paste0(year, "-01-01"))
  dow   <- as.integer(strftime(jan1, "%u"))      # Mon=1..Sun=7
  offset <- (4 - dow) %% 7                       # 첫 목요일까지 며칠
  first_thu <- jan1 + offset
  yday_first_thu <- as.integer(strftime(first_thu, "%j"))
  ifelse(yday_first_thu <= 4, 53L, 52L)
}

add_season <- function(df){
  df <- df %>%
    mutate(
      epi_week   = MMWRweek(week_end)$MMWRweek,
      epi_year = MMWRweek(week_end)$MMWRyear,
      # season starts : week 31 -> season_week = 1 (a.e., -30)
      season_week = if_else(
        epi_week >= 31,
        epi_week - 30L,
        epi_week + (epiweeks_in_year(epi_year - 1L) - 30L)  # last year 52 or 53 weeks
      ),
      season = if_else(
        epi_week >= 31,
        sprintf("%d/%02d", epi_year, (epi_year + 1L) %% 100),
        sprintf("%d/%02d", epi_year - 1L, epi_year %% 100)
      )
    )
  return(df)
}



# calculate RMSE function
compute_pairwise_rmse_byseason <- function(df_all) {
  n_na_rows <- sum(colSums(is.na(df_all)))
  if (n_na_rows > 0) {
    warning("Data include NA")
    }
  
  df_season <- df_all %>%
    mutate(sq_diff = (inc_hsa - inc_state)^2,
           abs_diff = abs(inc_hsa - inc_state)) %>%
    group_by(state, season, hsa_nci_id, population_hsa) %>%
    # compute RMSE over all timepoints and pairs
    summarise(RMSE = sqrt(mean(sq_diff, na.rm = TRUE)),
              MAE = mean(abs_diff, na.rm = TRUE))
  
  df_all_all <- df_all %>%
    mutate(sq_diff = (inc_hsa - inc_state)^2,
           abs_diff = abs(inc_hsa - inc_state)) %>%
    group_by(state, hsa_nci_id, population_hsa) %>%
    # compute RMSE over all timepoints and pairs
    summarise(RMSE = sqrt(mean(sq_diff, na.rm = TRUE)),
              MAE = mean(abs_diff, na.rm = TRUE))
  
  df_wide <- df_season %>%
    select(state, hsa_nci_id, population_hsa, season, RMSE, MAE) %>%
    pivot_wider(
      names_from  = season,
      values_from  = c(RMSE, MAE),
      names_glue   = "{.value}_{season}" 
    ) %>%
    left_join(df_all_all, by = c("state", "hsa_nci_id", "population_hsa"))
  return(df_wide)
}

