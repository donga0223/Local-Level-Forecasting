library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(tidycensus)


library(sf)
library(tigris)
library(stringr)

####### population_msa
options(tigris_use_cache = TRUE)

pop_county <- get_acs(
  geography = "county",
  variables = "B01001_001",
  year = 2023,
  survey = "acs5"
) %>%
  transmute(
    GEOID = GEOID,                       # county FIPS (5 chars)
    population = estimate,
    STATEFP = substr(GEOID, 1, 2)        # state FIPS (2 chars)
  )

# Counties + CBSA (Metro only) ----
counties_sf <- counties(year = 2023, cb = TRUE) %>%
  st_transform(5070)  # equal-area (optional but stable for spatial ops)

cbsa <- core_based_statistical_areas(year = 2023) %>%
  st_transform(5070)

cbsa_metro <- cbsa %>%
  filter(MEMI == 1) %>%                  # 1=Metro, 2=Micro
  select(CBSAFP, CBSA_NAME = NAME)

# County -> Metro CBSA crosswalk (NO duplicates) 
# Use point-on-surface to guarantee one CBSA per county (if any)
county_pts <- st_point_on_surface(counties_sf) %>%
  select(GEOID)

county_cbsa_crosswalk <- st_join(
  county_pts,
  cbsa_metro,
  join = st_within,                      # safer for points
  left = TRUE
) %>%
  st_drop_geometry() %>%
  filter(!is.na(CBSAFP)) %>%
  distinct(GEOID, .keep_all = TRUE)

# State-level metro population sum ----
msa_pop_by_state <- pop_county %>%
  left_join(county_cbsa_crosswalk, by = "GEOID") %>%
  filter(!is.na(CBSAFP)) %>%             # only metro counties
  group_by(STATEFP) %>%
  summarise(population_msa = sum(population, na.rm = TRUE), .groups = "drop")

msa_pop_by_state
data("fips_codes")

msa_pop_by_state_named <- msa_pop_by_state %>%
  left_join(
    fips_codes %>% 
      select(state_code, state = state_name) %>%
      distinct(),
    by = c("STATEFP" = "state_code")
  )
msa_pop_by_state_named


####################################################
####################################################

county_state <- counties_sf %>%
  st_drop_geometry() %>%
  transmute(GEOID = GEOID, STATEFP = STATEFP)

msa_count_by_state <- county_cbsa_crosswalk %>%
  left_join(county_state, by = "GEOID") %>%
  group_by(STATEFP) %>%
  summarise(n_msa = n_distinct(CBSAFP), .groups = "drop")

msa_count_by_state <- msa_count_by_state %>%
  left_join(fips_codes %>% 
              distinct(state_code , state_name), 
            by = c("STATEFP" = "state_code")) %>%
  select(state = state_name, n_msa) 


all_metric2 <- read.csv("data_joint/forecasting_metrics_overall.csv")
all_metric <- all_metric2 %>%
  left_join(msa_count_by_state, by = "state")


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
           log_density_hsa = log(density_hsa))
}

tmp1 <- tmp_horizon(ss = '2023/24', h = 1)
tmp1 <- tmp1 %>%
  mutate(log_diff_wis_overall = log(diff_wis_overall + 0.00171825 + 0.01))
tmp2 <- tmp_horizon(ss = '2023/24', h = 2)
tmp2 <- tmp2 %>%
  mutate(log_diff_wis_overall = log(diff_wis_overall + 0.06268538 + 0.01))

tmp3 <- tmp_horizon(ss = '2023/24', h = 3)
tmp3 <- tmp3 %>%
  mutate(log_diff_wis_overall = log(diff_wis_overall + 0.1324503 + 0.01))

tmp4 <- tmp_horizon(ss = '2023/24', h = 4)
tmp4 <- tmp4 %>%
  mutate(log_diff_wis_overall = log(diff_wis_overall + 0.2452104 + 0.01))

print(dim(tmp1))


cal_cv_R2 <- function(tmp, n_k, fit_fun, predict_fun = NULL, seed = 2025, target_var ="diff_wis_overall"){
  set.seed(seed)
  
  # 1. response와 fold 정의
  y <- tmp[[target_var]]
  n <- nrow(tmp)
  K <- n_k
  fold <- sample(rep(1:K, length.out = n))
  
  # 2. CV 예측값 저장 벡터
  yhat_cv <- rep(NA, n)
  
  # predict_fun이 없으면 기본은 predict(model, newdata=) 사용
  if (is.null(predict_fun)) {
    predict_fun <- function(model, newdata) predict(model, newdata = newdata)
  }
  
  # 3. K-fold CV loop
  for (k in 1:K) {
    train_idx <- fold != k
    test_idx  <- fold == k
    
    fit_k <- fit_fun(tmp[train_idx, ], target_var = target_var)
    yhat_cv[test_idx] <- predict_fun(fit_k, tmp[test_idx, ])
  }
  
  # 4. CV R² 계산 공식
  SSE <- sum((y - yhat_cv)^2)            # out-of-sample SSE
  SST <- sum((y - mean(y))^2)            # 전체 변동
  R2_cv <- 1 - SSE / SST
  
  RMSE_cv <- sqrt(mean((y - yhat_cv)^2))
  
  # 3) CV MAE
  MAE_cv <- mean(abs(y - yhat_cv))
  
  return(c(R2_cv = R2_cv, RMSE_cv = RMSE_cv, MAE_cv = MAE_cv))
}

lm_fit <- function(d, target_var = "log_diff_wis_overall") {
  fml <- as.formula(paste0(
    target_var, " ~ log_pop_ratio * log_pct_urban + n_msa"
  ))
  lm(fml, data = d)
}

lm_fit <- function(d, target_var = "log_diff_wis_overall") {
  fml <- as.formula(paste0(
    target_var, " ~ log_pop_ratio + n_msa"
  ))
  lm(fml, data = d)
}


cal_cv_R2(tmp1, n_k = 5, fit_fun = lm_fit, target_var ="log_diff_wis_overall")
cal_cv_R2(tmp2, n_k = 5, fit_fun = lm_fit, target_var ="log_diff_wis_overall")
cal_cv_R2(tmp3, n_k = 5, fit_fun = lm_fit, target_var ="log_diff_wis_overall")
cal_cv_R2(tmp4, n_k = 5, fit_fun = lm_fit, target_var ="log_diff_wis_overall")



library(mgcv)


gam_fit <- function(d, target_var = "log_diff_wis_overall") {
  fml <- as.formula(paste0(
    target_var,
    " ~ s(pop_ratio) + s(n_msa)"
  ))
  gam(fml, data = d, method = "REML")
}

gam_fit <- function(d, target_var = "log_diff_wis_overall") {
  fml <- as.formula(paste0(
    target_var,
    " ~ te(log_pop_ratio, log_pct_urban) + s(n_msa)"
  ))
  gam(fml, data = d, method = "REML")
}
cal_cv_R2(tmp1, n_k = 5, fit_fun = gam_fit, target_var = "log_diff_wis_overall")
cal_cv_R2(tmp2, n_k = 5, fit_fun = gam_fit, target_var = "log_diff_wis_overall")
cal_cv_R2(tmp3, n_k = 5, fit_fun = gam_fit, target_var = "log_diff_wis_overall")
cal_cv_R2(tmp4, n_k = 5, fit_fun = gam_fit, target_var = "log_diff_wis_overall")


library(dbarts)

# 공통으로 쓸 model.matrix 포뮬러 (lm, gam, bart 모두 동일 predictor 구조)
bart_mm_formula <- ~ log_pop_ratio + log_pct_urban + log_area_ratio + n_hsa
bart_mm_formula <- ~ log_pop_ratio + log_pct_urban + n_hsa
bart_mm_formula <- ~ pop_ratio + pct_urban + n_hsa
bart_mm_formula <- ~ log_pop_ratio + n_hsa
bart_mm_formula <- ~ log_pop_ratio + n_msa
bart_mm_formula <- ~ log_pop_ratio + log_pct_urban + n_msa

bart_fit <- function(d, target_var) {
  x <- model.matrix(bart_mm_formula, data = d)[, -1, drop = FALSE]
  y <- d[[target_var]]
  
  dbarts::bart(
    x.train   = x,
    y.train   = y,
    keeptrees = TRUE,
    verbose   = FALSE
  )
}

bart_predict <- function(model, newdata) {
  x_new <- model.matrix(bart_mm_formula, data = newdata)[, -1, drop = FALSE]
  
  # 여기서는 newdata 라는 이름을 써야 함
  pred_mat <- predict(model, newdata = x_new)
  as.numeric(colMeans(pred_mat))
}

cal_cv_R2(tmp1, n_k = 5, fit_fun = bart_fit, predict_fun = bart_predict, target_var = "log_diff_wis_overall")
cal_cv_R2(tmp2, n_k = 5, fit_fun = bart_fit, predict_fun = bart_predict, target_var = "log_diff_wis_overall")
cal_cv_R2(tmp3, n_k = 5, fit_fun = bart_fit, predict_fun = bart_predict, target_var = "log_diff_wis_overall")
cal_cv_R2(tmp4, n_k = 5, fit_fun = bart_fit, predict_fun = bart_predict, target_var = "log_diff_wis_overall")


#####################################################################
#####################################################################
############ Diagnostics plots
#####################################################################
#####################################################################
source("code/Analysis/fitting_diff_WIS_figures_ftn.R")

aa <- lm(log_diff_wis_overall~log_population_hsa +log(population_state)+n_msa+log_pop_ratio+log_pct_urban, data =tmp1)

lm_res1 <- lm(formula = log_diff_wis_overall ~ log_pop_ratio + n_msa, data = tmp1)
lm_res2 <- lm(formula = log_diff_wis_overall ~ log_pop_ratio + n_msa, data = tmp2)
lm_res3 <- lm(formula = log_diff_wis_overall ~ log_pop_ratio + n_msa, data = tmp3)
lm_res4 <- lm(formula = log_diff_wis_overall ~ log_pop_ratio + n_msa, data = tmp4)


lm_res1 <- lm(formula = log_diff_wis_overall ~ log_pop_ratio * log_pct_urban + n_msa, data = tmp1)
lm_res2 <- lm(formula = log_diff_wis_overall ~ log_pop_ratio * log_pct_urban + n_msa, data = tmp2)
lm_res3 <- lm(formula = log_diff_wis_overall ~ log_pop_ratio * log_pct_urban + n_msa, data = tmp3)
lm_res4 <- lm(formula = log_diff_wis_overall ~ log_pop_ratio * log_pct_urban + n_msa, data = tmp4)

for(i in 1:4){
  pdf(paste0("paper_figures/LM_fitting2_log_wis_var3_nmsa_interaction_h",i,".pdf"), 
      width = 4, height = 4, useDingbats = FALSE)
  
  
  lm_diag_figs(get(paste0("lm_res",i)), 
               ylabs = rep(expression("Partial effect on " * Delta * " WIS"), 4),
               xlabs = c("HSA-state population ratio (log)",
                         "Proportion of urban population (log)",
                         "Number of MSAs state"),
               target_var = "log_diff_wis_overall")
  dev.off()
  
}

### create interaction graph, choosed the percentiles
z_vals <- quantile(tmp1$log_pct_urban, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)

# x grid
x_seq <- seq(min(tmp1$log_pop_ratio, na.rm = TRUE),
             max(tmp1$log_pop_ratio, na.rm = TRUE),
             length.out = 100)

nd_lm <- expand.grid(
  log_pop_ratio = x_seq,
  log_pct_urban = as.numeric(z_vals),
  n_msa = median(tmp1$n_msa, na.rm = TRUE)
)

pr_lm <- predict(lm_res1, newdata = nd_lm, interval = "confidence", level = 0.95)

nd_lm$fit <- pr_lm[, "fit"]
nd_lm$lo  <- pr_lm[, "lwr"]
nd_lm$hi  <- pr_lm[, "upr"]

nd_lm$urban_level <- factor(
  nd_lm$log_pct_urban,
  levels = as.numeric(z_vals),
  labels = c("Low (10th)", "Medium (50th)", "High (90th)")
)


cols <- c("#1f78b4", "#fdbf6f", "#33a02c")
labs <- c("Low (10th)", "Medium (50th)", "High (90th)")

png("paper_figures/LM_fitting2_log_wis_var3_interaction_term_nmsa_paper_h1.png",
    width = 5, height = 4, units = "in", res = 600)

ggplot(nd_lm, aes(x = log_pop_ratio, y = fit,
                  color = urban_level, fill = urban_level)) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(aes(ymin = lo, ymax = hi),
              alpha = 0.15, color = NA) +
  scale_color_manual(
    values = cols,
    labels = labs,
    name   = "Proportion of \nurban population \n(log)"
  ) +
  scale_fill_manual(
    values = cols,
    labels = labs,
    name   = "Proportion of \nurban population \n(log)"
  ) +
  labs(
    x = "HSA-to-state population ratio (log)",
    y = "Predicted log(shifted ΔWIS)"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 7),
    legend.text  = element_text(size = 5)
  )

dev.off()


library(mgcv) 
gam_res1 <- gam(formula = log_diff_wis_overall  ~  te(log_pop_ratio,log_pct_urban) + s(n_msa) , data = tmp1) 
gam_res2 <- gam(formula = log_diff_wis_overall  ~  te(log_pop_ratio,log_pct_urban) + s(n_msa) , data = tmp2) 
gam_res3 <- gam(formula = log_diff_wis_overall  ~  te(log_pop_ratio,log_pct_urban)+ s(n_msa) , data = tmp3) 
gam_res4 <- gam(formula = log_diff_wis_overall  ~  te(log_pop_ratio,log_pct_urban) + s(n_msa) , data = tmp4) 


for(i in 1:4){
  pdf(paste0("paper_figures/GAM_fitting2_log_wis_var3_nmsa_interaction_h",i,".pdf"), width = 4, height = 4)
  gam_diag_figs(get(paste0("gam_res",i)), 
                ylabs = rep(expression("Partial Effect on " * Delta * " WIS"), 4),
                xlabs = c("HSA-to-state population ratio (Log)",
                          # "Proportion of urban population (log)",
                          # "HSA-to-state area ratio (log)",
                          "Number of MSAs per state"))
  dev.off()
  
}


## interaction plot for GAM, same as GLM
nd <- expand.grid(
  log_pop_ratio = x_seq,
  log_pct_urban = as.numeric(z_vals),
  n_msa = median(tmp1$n_msa, na.rm = TRUE)
)


pr <- predict(gam_res1, newdata = nd, type = "link", se.fit = TRUE)
nd$fit <- pr$fit
nd$lo  <- pr$fit - 1.96 * pr$se.fit
nd$hi  <- pr$fit + 1.96 * pr$se.fit

nd$urban_level <- factor(
  nd$log_pct_urban,
  levels = as.numeric(z_vals),
  labels = c("Low (10th)", "Medium (50th)", "High (90th)")
)

cols <- c("#1f78b4", "#fdbf6f", "#33a02c" )
labs <- c("Low (10th)", "Medium (50th)", "High (90th)")

png("paper_figures/GAM_fitting2_log_wis_var3_interaction_term_nmsa_paper_h1.png", 
    width = 5, height = 4, units = "in", res = 600)

ggplot(nd, aes(x = log_pop_ratio, y = fit,
               color = urban_level, fill = urban_level)) +
  geom_line(linewidth = 1.1) +
  geom_ribbon(aes(ymin = lo, ymax = hi),
              alpha = 0.15, color = NA) +
  scale_color_manual(
    values = cols,
    labels = labs,
    name   = "Proportion of \nurban population \n(log)"
  ) +
  scale_fill_manual(
    values = cols,
    labels = labs,
    name   = "Proportion of \nurban population \n(log)"
  ) +
  labs(
    x = "HSA-to-state population ratio (log)",
    y = "Predicted log(shifted ΔWIS)"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 7),
    legend.text  = element_text(size = 5)
  )

dev.off()

### BART 

library(dbarts)
predictors <- c("log_pop_ratio", "log_pct_urban", "log_area_ratio", "n_hsa")
predictors <- c("log_pop_ratio", "log_pct_urban", "n_msa")
predictors <- c("log_pop_ratio", "n_msa")

target_var <- "log_diff_wis_overall"
bart_res1 <- bart(
  x.train   = tmp1[, predictors, drop = FALSE],
  y.train   = tmp1[[target_var]],
  verbose   = FALSE,
  keeptrees = TRUE
)

bart_res2 <- bart(
  x.train   = tmp2[, predictors, drop = FALSE],
  y.train   = tmp2[[target_var]],
  verbose   = FALSE,
  keeptrees = TRUE
)

bart_res3 <- bart(
  x.train   = tmp3[, predictors, drop = FALSE],
  y.train   = tmp3[[target_var]],
  verbose   = FALSE,
  keeptrees = TRUE
)

bart_res4 <- bart(
  x.train   = tmp4[, predictors, drop = FALSE],
  y.train   = tmp4[[target_var]],
  verbose   = FALSE,
  keeptrees = TRUE
)


predictor_multiline_names = c("HSA-to-state \n population \n ratio (log)",
                              "Percent \n urbanization \n (log)",
                              "Number \n of MSAs \n per state")

predictors = c("log_pop_ratio",  "n_msa")
predictors = c("log_pop_ratio", "log_pct_urban",  "n_msa")


X <- tmp1[, predictors, drop=FALSE]

ylabs = rep(expression("Partial Effect on log(shifted " * Delta * " WIS"), 4)
xlabs = c("HSA-to-state population ratio (log)",
          "Proportion of urban population (log)",
          "Number of MSAs per state")
bart_diag_figs(bart_res1, xlabs, ylabs, predictor_multiline_names, predictors)

for(i in 1:4){
  pdf(paste0("paper_figures/BART_fitting2_log_wis_var3_nmsa_interaction_h",i,".pdf"), width = 4, height = 4)
  bart_diag_figs(get(paste0("bart_res",i)), 
                 ylabs = rep(expression("Partial Effect on " * Delta * " WIS"), 4),
                 xlabs = c("HSA-to-state population ratio (Log)",
                           "Proportion of urban population (log)",
                           "Number of MSAs per state"),
                 predictor_multiline_names, predictors)
  dev.off()
  
}



