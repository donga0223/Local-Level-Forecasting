library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)


hsa <- read_xls("/Users/dk29776/Dropbox/UTAustin/Forecasting/Local-Level-Forecasting/data/Health.Service.Areas.xls")
hsa <- hsa %>%
  mutate(state = str_extract(`State-county`, "^[A-Z]{2}"))

hsa <- hsa %>%
  group_by(state) %>%
  mutate(n_hsa = n_distinct(`HSA # (NCI Modified)`)) %>%
  ungroup()

hsa <- hsa %>%
  group_by(`HSA # (NCI Modified)`) %>%
  mutate(n_county_hsa = n_distinct(FIPS)) %>%
  ungroup()

hsa1 <- hsa %>%
  select(`HSA # (NCI Modified)`, state, n_county_hsa, n_hsa) %>%
  distinct() %>%
  rename(hsa_nci_id = `HSA # (NCI Modified)`,
         state_abb = state)
source("Local-Level-Forecasting/code/rmse_season_ftn.R")


## 672 hsas RMSE
df_all <- read.csv("Local-Level-Forecasting/data/hsa_state_inc.csv")
df_all_season_rmse <- compute_pairwise_rmse_byseason(df_all)

df_all_season_rmse1 <- df_all_season_rmse %>%
  left_join(hsa1, by = "hsa_nci_id")

## forecast evaluation metrics

all_metric <- read.csv("Local-Level-Forecasting/data_joint/forecasting_metrics_overall.csv")
all_metric1 <- all_metric %>%
  select(state, hsa_nci_id, season, horizon, diff_wis_season, diff_wis_overall)

df_wide_season <- all_metric %>%
  select(state, hsa_nci_id, season, horizon, diff_wis_season) %>%
  mutate(season = gsub("/", "_", season)) %>%   # column 이름 문제 방지
  pivot_wider(
    id_cols = c(state, hsa_nci_id),
    names_from = c(season, horizon),
    values_from = diff_wis_season,
    names_glue = "diff_wis_season_{season}_h{horizon}"
  )


df_wide_overall <- all_metric %>%
  select(state, hsa_nci_id, horizon, diff_wis_overall) %>%
  distinct() %>%
  pivot_wider(
    id_cols = c(state, hsa_nci_id),
    names_from = horizon,
    values_from = diff_wis_overall,
    names_prefix = "diff_wis_overall_h"
  )

df_wide <- df_wide_season %>%
  left_join(df_wide_overall, by = c("state", "hsa_nci_id"))

df_all <- df_all_season_rmse1 %>%
  left_join(df_wide, by = c("state", "hsa_nci_id"))

####################################################################################
library(sf)
library(dplyr)
library(tigris)
library(tidycensus)

# 1) County polygons
county_sf <- counties(cb = TRUE, year = 2022)

hsa_sf <- county_sf %>%
  left_join(hsa, by = c("STUSPS" = "state", "GEOID" = "FIPS")) %>%
  rename("hsa_nci_id" = "HSA # (NCI Modified)") %>%
  filter(!is.na(hsa_nci_id)) %>%
  group_by(STATE_NAME, STUSPS, hsa_nci_id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# 2) state polygons
st_sf <- states(cb = TRUE, year = 2022)

# 3) urban areas polygons
ua_sf <- urban_areas(year = 2022)


### US map
exclude_states <- c("AK", "HI", "PR", "VI", "GU", "MP", "AS")

st_contig <- st_sf %>%
  filter(!STUSPS %in% exclude_states)
hsa_contig <- hsa_sf %>%
  filter(!STUSPS %in% exclude_states)

ua_contig <- ua_sf %>%
  st_transform(st_crs(st_contig))

# continental 영역 경계
contig_union <- st_union(st_contig)

# HSA / UA를 continental US 범위로 crop/intersection
hsa_contig <- st_intersection(hsa_contig, contig_union)
ua_contig  <- st_intersection(ua_contig, contig_union)

# map
ggplot() +
  geom_sf(data = hsa_contig, fill = NA, color = "gray", linewidth = 0.25) +
  geom_sf(data = st_contig, fill = NA, color = "black", linewidth = 0.2) +
  geom_sf(data = ua_contig, fill = "tomato", color = NA, alpha = 0.5) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  theme_minimal() +
  labs(
    title = "HSA Boundaries and 2022 Urban Areas (Continental U.S.)",
    subtitle = "Urban areas shown in red"
  )

################################################################################
################################################################################

# state 면적
st_sf_area <- st_sf %>%
  st_transform(5070) %>%   # US Albers Equal Area
  mutate(state_area = st_area(.))

# 면적 기준 urban share
hsa_sf_area <- hsa_sf %>%
  st_transform(5070) %>%   # US Albers Equal Area
  mutate(hsa_area = st_area(.))

library(sf)
library(dplyr)
library(purrr)

# 면적 계산용 projection
hsa_sf2 <- st_transform(hsa_sf, 5070)
ua_sf2  <- st_transform(ua_sf, 5070)

# id 이름 통일
hsa_sf2 <- hsa_sf2 %>% rename(hsa_id = hsa_nci_id)

# 먼저 어떤 HSA와 어떤 urban area가 겹치는지만 찾기
idx <- st_intersects(hsa_sf2, ua_sf2)

# HSA별 urban area 면적 계산
hsa_urban_area <- map_dfr(seq_along(idx), function(i) {
  j <- idx[[i]]
  
  if (length(j) == 0) {
    return(tibble(
      hsa_id = hsa_sf2$hsa_id[i],
      urban_area = units::set_units(0, "m^2")
    ))
  }
  
  ints <- st_intersection(hsa_sf2[i, ], ua_sf2[j, ])
  
  tibble(
    hsa_id = hsa_sf2$hsa_id[i],
    urban_area = sum(st_area(ints))
  )
})

## 면적기준으로 urban area 계산
hsa_area_share <- hsa_sf_area %>%
  left_join(hsa_urban_area, by = c("hsa_nci_id" = "hsa_id")) %>%
  mutate(urban_area = ifelse(is.na(urban_area), units::set_units(0, "m^2"), urban_area),
         pct_urban_area = as.numeric(urban_area / hsa_area)) %>%
  left_join(st_sf_area %>%
              st_drop_geometry() %>%
              select(NAME, state_area), by = c("STATE_NAME" = "NAME"))


state_area_share <- hsa_area_share %>%
  st_drop_geometry() %>%
  group_by(STATE_NAME, STUSPS, state_area) %>%
  summarise(sum_hsa_area = sum(hsa_area),
            sum_urban_area = sum(urban_area)) %>%
  mutate(state_pct_urban_area = sum_urban_area/sum_hsa_area)

write.csv(hsa_area_share, "Local-Level-Forecasting/data/hsa_pct_urban.csv",
          row.names = FALSE)

####################################################################################
## population size 로 urban %


#-----------------------------
# 0) HSA polygon 준비
#-----------------------------

hsa_sf2 <- hsa_area_share %>%
  select(STATE_NAME, STUSPS, hsa_nci_id, geometry) %>%
  st_make_valid() %>%
  st_transform(5070)

# 필요한 state만
state_vec <- sort(unique(hsa_sf2$STUSPS))

out_dir <- "/Users/dk29776/Dropbox/UTAustin/Forecasting/Local-Level-Forecasting/data/hsa_pop_by_state"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


#-----------------------------
# 1) state별 2020 block 가져오기
#-----------------------------

process_one_state <- function(st, hsa_sf2, out_dir) {
  message("========== Processing ", st, " ==========")
  
  out_file <- file.path(out_dir, paste0("hsa_pop_", st, ".csv"))
  
  if (file.exists(out_file)) {
    message("Already exists, skipping: ", out_file)
    return(invisible(NULL))
  }
  
  blk <- get_blocks_one_state(st)
  
  hsa_st <- hsa_sf2 %>%
    filter(STUSPS == st) %>%
    select(hsa_nci_id, geometry)
  
  if (nrow(hsa_st) == 0) {
    message("No HSA found for state: ", st)
    return(invisible(NULL))
  }
  
  pts <- st_point_on_surface(blk)
  
  joined <- st_join(
    pts,
    hsa_st,
    join = st_within,
    left = FALSE
  ) %>%
    distinct(GEOID, .keep_all = TRUE)
  
  hsa_pop_st <- joined %>%
    st_drop_geometry() %>%
    group_by(hsa_nci_id) %>%
    summarise(
      total_pop_sum = sum(total_pop, na.rm = TRUE),
      urban_pop_sum = sum(ifelse(is_urban, total_pop, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      urban_pop_share = ifelse(total_pop_sum > 0, urban_pop_sum / total_pop_sum, NA_real_)
    ) %>%
    rename(
      total_pop = total_pop_sum,
      urban_pop = urban_pop_sum
    )
  
  write.csv(hsa_pop_st, out_file, row.names = FALSE)
  message("Saved: ", out_file)
  
  rm(blk, hsa_st, pts, joined, hsa_pop_st)
  invisible(gc())
}


process_one_state("NJ", hsa_sf2, out_dir)
process_one_state("TX", hsa_sf2, out_dir)
process_one_state("CT", hsa_sf2, out_dir)


for (st in state_vec) {
  tryCatch(
    {
      process_one_state(st, hsa_sf2, out_dir)
    },
    error = function(e) {
      message("ERROR in ", st, ": ", e$message)
    }
  )
}

library(readr)
files <- list.files(out_dir, pattern = "^hsa_pop_.*\\.csv$", full.names = TRUE)

hsa_pop_urban <- bind_rows(lapply(files, read_csv, show_col_types = FALSE)) %>%
  group_by(hsa_nci_id) %>%
  summarise(
    total_pop = sum(total_pop, na.rm = TRUE),
    urban_pop = sum(urban_pop, na.rm = TRUE),
    urban_pop_share = ifelse(total_pop > 0, urban_pop / total_pop, NA_real_),
    .groups = "drop"
  )

hsa_final <- hsa_area_share %>%
  left_join(hsa_pop_urban, by = "hsa_nci_id")

####################################################################################
####################################################################################
####################################################################################

df_all1 <- df_all %>%
  left_join(hsa_final %>%
              select(STATE_NAME, hsa_nci_id, state_area,
                     hsa_area, urban_area, pct_urban_area,
                     total_pop, urban_pop, urban_pop_share), 
            by = c("state" = "STATE_NAME", "hsa_nci_id"))


write.csv(df_all1, "data/all_metrics_672.csv",
          row.names = FALSE)


df_all1 <- readRDS("Local-Level-Forecasting/data/all_metrics_672.rds")

plot(df_all1$pct_urban, df_all1$urban_pop_share)
plot(df_all1$pct_urban, df_all1$pct_urban_area)




cor.test(df_all$RMSE, df_all$diff_wis_overall_h1)
df_all %>%
  ggplot(aes(x = RMSE, y = diff_wis_overall_h1)) +
  geom_point()

library(GGally)

df_all1 %>%
  mutate(pct_urban_area1 = ifelse(pct_urban_area == 0, 0.001, pct_urban_area),
         urban_pop_share1 = ifelse(urban_pop_share == 0, 0.1, urban_pop_share)) %>%
  filter(hsa_nci_id != 61) %>%
  ungroup() %>%
  select(RMSE, pop_ratio, pct_urban, n_hsa, pct_urban_area1, urban_pop_share1) %>%
  mutate(log_RMSE = log(RMSE), 
         log_pop_ratio = log(pop_ratio),
         log_pct_urban = log(pct_urban),
         log_pct_urban_area1 = log(pct_urban_area1),
         log_urban_pop_share1 = log(urban_pop_share1),
         log_state_area = log(state_area)) %>%
  select(log_RMSE, log_pop_ratio, log_pct_urban, n_hsa, log_pct_urban_area1, 
         log_urban_pop_share1, log_state_area) %>%
  ggpairs()

summary(df_all1$pct_urban_area)
summary(df_all1$urban_pop_share)

df_all2 <- df_all1 %>%
  filter(hsa_nci_id != 61,
         !is.na(diff_wis_overall_h1)) %>%
  mutate(log_RMSE = log(RMSE), 
         log_pop_ratio = log(pop_ratio),
         log_pct_urban = log(pct_urban),
         log_pct_urban_area = log(pct_urban_area),
         log_urban_pop_share = log(urban_pop_share),
         log_state_area = log(state_area))

#### n_msa
library(tidycensus)
library(tigris)
library(dplyr)
library(tidyr)

options(tigris_use_cache = TRUE)

# county 정보
counties_sf <- counties(cb = TRUE, year = 2022) %>%
  st_drop_geometry() %>%
  select(GEOID, STATEFP)

# OMB MSA delineation 파일
library(readxl)

msa <- read_excel("Local-Level-Forecasting/data/list1_2023.xlsx",skip = 2)
msa <- msa %>%
  mutate(
    GEOID = sprintf("%02d%03d",
                    as.numeric(`FIPS State Code`),
                    as.numeric(`FIPS County Code`))
  ) %>%
  select(GEOID, `CBSA Code`)

# county → state → MSA 연결
state_msa <- counties_sf %>%
  left_join(msa, by = "GEOID") %>%
  filter(!is.na(`CBSA Code`))

# state별 MSA 개수
msa_count <- state_msa %>%
  group_by(STATEFP) %>%
  summarise(MSA_count = n_distinct(`CBSA Code`))

st_name <- st_sf %>%
  st_drop_geometry() %>%
  select(STATEFP, NAME) %>%
  distinct()

msa_count <- msa_count %>%
  left_join(st_name, by = "STATEFP") %>%
  rename(n_msa = MSA_count)

df_all3 <- df_all2 %>%
  left_join(msa_count, by = c("state" = "NAME"))

#####


saveRDS(df_all3, "Local-Level-Forecasting/data/hsa_pct_urban.rds")

summary(df_all3$diff_wis_overall_h1)
names(df_all3)

y_min <- min(df_all3$diff_wis_overall_h1)

lm_h1 <- lm(log(diff_wis_overall_h1 - y_min + 0.01)~ log(urban_pop_share) * log(pop_ratio) + n_msa,
   data = df_all3)
summary(lm_h1)
y <- log(df_all3$diff_wis_overall_h1 - y_min + 0.01)
resid <- lm_h1$residuals
y_pred <- lm_h1$fitted.values
plot(y, y_pred)
abline(a= 0, b= 1, col = 'blue')
plot(log(df_all3$pop_ratio), resid)
plot(log(df_all3$urban_pop_share), resid)


library(mgcv)
df_all3$y = y
gam_h1 <- gam(y~log_urban_pop_share + log_pop_ratio + s(n_msa) +
                te(log_urban_pop_share, log_pop_ratio), data = df_all3, method = "REML")

summary(gam_h1)
y_pred <- gam_h1$fitted.values
diag_4_fig(y_pred, y)
#gam_diag_figs(gam_res = gam_h1, xlabs = "1", ylabs = "1")
plot(gam_h1, shade = TRUE, select = 1, 
     #xlab = xlabs[i], 
     #ylab = ylabs[i],
     residuals = TRUE, cex = 3)


library(sandwich)
library(lmtest)

coeftest(lm_h1, vcov = vcovHC(lm_h1, type = "HC3"))

library(car)
vif(lm_h1, type = "predictor")

which(df_all3$urban_pop_share == max(df_all2$urban_pop_share ))
View(df_all3[12,])
which(df_all3$urban_pop_share == min(df_all2$urban_pop_share ))
View(df_all3[47,])

df_all3$urban_logit <- log(df_all3$urban_pop_share / (1 - df_all3$urban_pop_share))
df_all3$pop_ratio_logit <- log(df_all3$pop_ratio / (1 - df_all3$pop_ratio))

lm_h1 <- lm(
  log(diff_wis_overall_h1 - y_min + 0.01) ~ urban_logit * pop_ratio_logit + n_msa,
  data = df_all3
)
summary(lm_h1 )
y <- log(df_all3$diff_wis_overall_h1 - y_min + 0.01)
resid <- lm_h1$residuals
y_pred <- lm_h1$fitted.values
plot(y, y_pred)
abline(a= 0, b= 1, col = 'blue')
plot(df_all3$urban_logit, resid)
plot(df_all3$pop_ratio_logit, resid)


lm_h1 <- lm(
  log(diff_wis_overall_h1 - y_min + 0.01) ~ urban_logit * log(pop_ratio) + n_msa,
  data = df_all3
)
summary(lm_h1 )
y <- log(df_all3$diff_wis_overall_h1 - y_min + 0.01)
resid <- lm_h1$residuals
y_pred <- lm_h1$fitted.values
plot(y, y_pred)
abline(a= 0, b= 1, col = 'blue')
plot(df_all3$urban_logit, resid)
plot(log(df_all3$pop_ratio), resid)

lm_h1 <- lm(
  log(diff_wis_overall_h1 - y_min + 0.01) ~ 
    log(urban_pop_share) + I(log(urban_pop_share)^2) +
    log(pop_ratio) + 
    log(urban_pop_share):log(pop_ratio) +
    n_msa,
  data = df_all3
)
summary(lm_h1)




######################################################################
######################################################################
######################################################################
aa <- lm(log(diff_wis_overall_h1)~log_pop_ratio * log_pct_urban * log_pct_urban_area + poly(n_hsa,2) + log(state_area), data = df_all2)
step(aa)
aa2 <- lm(formula = log(diff_wis_overall_h1) ~ log_pop_ratio + log_pct_urban + 
            poly(n_hsa, 2) + log_pop_ratio:log_pct_urban, data = df_all2)
summary(aa2)
summary(lm(log(diff_wis_overall_h1)~pop_ratio * log(urban_pop_share) * pct_urban_area + poly(n_hsa,2) + log(state_area), data = df_all2))
summary(lm(log(diff_wis_overall_h1)~pop_ratio * log(pct_urban) * pct_urban_area + poly(n_hsa,2) + log(state_area), data = df_all2))




summary(lm(log(diff_wis_overall_h1)~log(pop_ratio) * log(urban_pop_share) + n_hsa, data = df_all2))
summary(lm(log(RMSE)~log(pop_ratio) * log(urban_pop_share) + poly(n_hsa,2), data = df_all2))
summary(lm(log(RMSE)~log(pop_ratio) * log(pct_urban_area) + poly(n_hsa , 2), data = df_all2))

summary(lm(log(RMSE)~log(pop_ratio) +  log(urban_pop_share1) *
             log(pct_urban_area1) + n_hsa, 
           data = df_all2))

summary(lm(diff_wis_overall_h1~log(pop_ratio) + # log(urban_pop_share1) +
             log(pct_urban_area1) + n_hsa, 
           data = df_all2))



library(mgcv)

m1 <- gam(
  log_RMSE ~ 
    s(log_pop_ratio) 
    + s(log_pct_urban_area) 
    + s(log_pct_urban) 
    + s(n_hsa) 
    #+ ti(log_pop_ratio, log_pct_urban_area)
  ,
  data = df_all2
)

summary(m1)
plot(m1, pages = 1, residuals = TRUE)
vis.gam(m1, view=c("log_pop_ratio", "log_pct_urban_area"))
gam.check(m1)


m2 <- gam(
  log(diff_wis_overall_h1) ~ s(log_pop_ratio) + s(log_pct_urban) + 
       s(n_hsa) + ti(log_pop_ratio,log_pct_urban), data = df_all2)

summary(m2)
plot(m2, pages = 1, residuals = TRUE)
vis.gam(m2, view=c("log_pop_ratio", "log_pct_urban_area"))
gam.check(m2)


m3 <- gam(
  log(RMSE) ~ s(log_pop_ratio) + s(log_pct_urban) + 
    s(n_hsa) + ti(log_pop_ratio,log_pct_urban), data = df_all2)

summary(m3)
plot(m3, pages = 1, residuals = TRUE)
vis.gam(m3, view=c("log_pop_ratio", "log_pct_urban"))
gam.check(m3)



summary(lm(
  log(RMSE) ~ 
    poly(log(pop_ratio),2) +
    poly(urban_pop_share,2) +
    n_hsa,
  data = df_all2
))

library(lme4)

lmer_res <- summary(lmer(
  log(RMSE) ~ log(pop_ratio) + urban_pop_share + n_hsa + (1|state),
  data = df_all2
))


library(ranger)

rf <- ranger(
  log(RMSE) ~ log(pop_ratio) + urban_pop_share + pct_urban_area + n_hsa,
  data = df_all2,
  importance = "impurity"
)



library(xgboost)




df_all_season_rmse %>%
  ggplot(aes(x = log(density_hsa), y = `RMSE_2023/24`)) +
  geom_point()

df_all_season_rmse %>%
  ggplot(aes(x = log(density_hsa), y = log(population_hsa))) +
  geom_point()

aa <- lm(RMSE~pop_ratio + pct_urban + log(density_hsa) + log(population_hsa), 
         data = df_all_season_rmse )
summary(aa)

aa <- gam(RMSE~pop_ratio + pct_urban, data = df_all_season_rmse )
summary(aa)


#############################################################################
#############################################################################
#############################################################################

df_plot <- df_all2 %>%
  mutate(improve_h1 = diff_wis_overall_h1 > 0,
         improve_h2 = diff_wis_overall_h2 > 0,
         improve_h3 = diff_wis_overall_h3 > 0,
         improve_h4 = diff_wis_overall_h4 > 0) %>%
  group_by(state) %>%
  summarise(
    p_improve_h1 = mean(improve_h1),
    p_improve_h2 = mean(improve_h2),
    p_improve_h3 = mean(improve_h3),
    p_improve_h4 = mean(improve_h4),
    n_hsa = first(n_hsa),
    pct_urban = first(pct_urban),
    pct_urban_area = first(pct_urban_area),
    state_area = first(state_area),
    .groups = "drop"
  ) %>%
  arrange(pct_urban) %>%
  pivot_longer(
    cols = starts_with("p_improve"),
    names_to = "h",
    values_to = "p_improve"
  ) %>%
  mutate(
    horizon = as.integer(str_extract(h, "\\d+")),   # 숫자만 추출
    state = factor(state, levels = unique(state))
  )



ggplot(df_plot, aes(x = state)) +
  
  geom_col(aes(y = p_improve),
           fill = "steelblue", alpha = 0.7) +
  
  geom_line(aes(y = pct_urban, color = "Urban population share", group = 1),
            linewidth = 1) +
  geom_point(aes(y = pct_urban, color = "Urban population share"),
             size = 2) +
  
  geom_line(aes(y = pct_urban_area, color = "Urban land share", group = 1),
            linewidth = 1) +
  geom_point(aes(y = pct_urban_area, color = "Urban land share"),
             size = 2) +
  
  facet_wrap(~h) +
  
  scale_color_manual(
    name = "Urbanization",
    values = c(
      "Urban population share" = "red",
      "Urban land share" = "darkgreen"
    ),
    guide = guide_legend(nrow = 1)
  ) +
  
  scale_y_continuous(
    name = "P(HSA forecast improves WIS)",
    limits = c(0,1),
    sec.axis = sec_axis(~., name = "Urban share")
  ) +
  
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#############################################################################
#############################################################################
#############################################################################


all_forecasting_metric <- read.csv("data_joint/forecasting_metrics_3seasons.csv")

within_state_cor <- all_forecasting_metric %>%
  group_by(state, horizon) %>%
  group_modify(~{
    
    mat <- .x %>%
      select(hsa_nci_id, target_end_date, est_median) %>%
      pivot_wider(names_from = target_end_date,
                  values_from = est_median)
    
    mat <- as.matrix(mat[,-1])
    
    cor_mat <- cor(t(mat), use="pairwise.complete.obs")
    
    cor_vals <- cor_mat[upper.tri(cor_mat)]
    
    tibble(
      mean_cor = mean(cor_vals, na.rm=TRUE),
      median_cor = median(cor_vals, na.rm=TRUE)
    )
  })

df_plot1 <- df_plot %>%
  select(-h) %>%
  left_join(within_state_cor, by = c("state", "horizon")) %>%
  left_join(state_area_share %>%
              select(-STUSPS), by = c("state" = "STATE_NAME", "state_area")) %>%
  left_join(all_metric %>%
              select(state, population_state) %>%
              distinct(), by = "state")

library(units)

df_plot1 %>%
  mutate(across(where(~inherits(.x, "units")), as.numeric)) %>%
  filter(!is.na(mean_cor)) %>%
  mutate(log_state_area = log(state_area)) %>%
  ggplot(aes(median_cor, log_state_area)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE) +
  facet_wrap(~horizon)

df_plot1 %>%
  mutate(across(where(~inherits(.x, "units")), as.numeric)) %>%
  filter(!is.na(mean_cor)) %>%
  ggplot(aes(mean_cor, state_pct_urban_area)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE) +
  facet_wrap(~horizon)

df_plot1 %>%
  mutate(across(where(~inherits(.x, "units")), as.numeric)) %>%
  filter(!is.na(mean_cor)) %>%
  mutate(log_population_state = log(population_state)) %>%
  ggplot(aes(median_cor, log_population_state)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE) +
  facet_wrap(~horizon)

df_plot1 %>%
  mutate(across(where(~inherits(.x, "units")), as.numeric)) %>%
  filter(!is.na(mean_cor)) %>%
  mutate(log_pct_urban = log(pct_urban)) %>%
  ggplot(aes(median_cor, log_pct_urban)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 2),
              se = TRUE) +
  facet_wrap(~horizon)






library(mgcv)
# te()는 두 변수의 척도가 다를 때 사용하는 상호작용 함수입니다.
gam_res <- gam(y_h1 ~ te(log_pop_ratio, log_urban_pop_share) + n_msa, data = df_all3)
gam_res <- gam(y_h1 ~ te(log_pop_ratio, log_urban_pop_share, k=10) + s(n_msa), 
               data = df_all3, method = "REML")

summary(gam_res)
plot(gam_res)
vis.gam(gam_res, view = c("log_pop_ratio", "log_urban_pop_share"), 
        plot.type = "contour", color = "topo")
vis.gam(gam_res, view = c("log_pop_ratio", "log_urban_pop_share"), 
        theta = 30, phi = 30, ticktype = "detailed")
# 1. 상호작용항의 2D 단면 (Nick에게 보여주기 가장 좋음)
# log_urban_pop_share의 특정 값들(평균, +-1SD)에서의 기울기 변화
eff_gam <- predict_response(gam_res, terms = c("log_pop_ratio", "log_urban_pop_share"))
plot(eff_gam, show_data = TRUE, dot_alpha = 0.8) + 
  labs(title = "GAM Partial Effects with Interaction")
plot(eff_gam, show_data = TRUE, colors = "Set1", dot_alpha = 0.8) + 
  theme_bw() +
  labs(title = "GAM Partial Effects: Solid Colors")



# 2. n_msa의 독립적인 효과 (직선인지 곡선인지 확인)
# n_msa를 모델에서 s(n_msa)로 넣었다면 곡선으로, 그냥 넣었다면 직선으로 나옵니다.
eff_msa <- predict_response(gam_res, terms = "n_msa")
plot(eff_msa, show_data = TRUE)
gam.check(gam_res)


library(quantreg)
# tau = 0.5 는 중앙값 회귀 (Median Regression)
qr_res <- rq(y_h1 ~ log_pop_ratio * log_urban_pop_share + n_msa, 
             data = df_all3, tau = 0.5)
summary(qr_res)

#GLS (Generalized Least Squares)
library(nlme)
# log_urban_pop_share에 따라 분산이 커지는 것을 반영
gls_res <- gls(y_h1 ~ log_pop_ratio * log_urban_pop_share + n_msa, 
               data = df_all3, weights = varPower(form = ~ log_urban_pop_share))
summary(gls_res)
library(ggeffects)
library(ggplot2)
# 1. 상호작용 효과 계산 (x축 변수, 그룹화 변수 순서)
# [levels]를 지정하지 않으면 자동으로 평균 ± 1SD 지점을 잡습니다.
eff_data <- predict_response(gls_res, terms = c("log_pop_ratio", "log_urban_pop_share"))

# 2. 그래프 그리기 (show_data = TRUE로 관측치 추가)
plot(eff_data, show_data = TRUE) +
  labs(title = "Interaction Effect (GLS Model)",
       x = "log_pop_ratio",
       y = "y_h1") +
  theme_minimal()


library(sjPlot)
library(ggplot2)
plot_model(gls_res, type = "eff", terms = "log_pop_ratio")
plot_model(gls_res, 
           type = "pred", 
           terms = "log_pop_ratio", 
           show.data = TRUE) +
  theme_minimal()

plot_model(gls_res, 
           type = "pred", 
           terms = "log_urban_pop_share", 
           show.data = TRUE) +
  theme_minimal()
# 1. Fitted vs Residuals (이분산성 확인)
plot(gls_res, resid(., type = "pearson") ~ fitted(.), abline = 0)

# 2. QQ-plot (정규성 확인)
qqnorm(gls_res, abline = c(0, 1))
