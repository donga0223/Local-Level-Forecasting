
source("Local-Level-Forecasting/code/Analysis/fitting_diff_WIS_figures_ftn.R")
df_all3 <- readRDS("Local-Level-Forecasting/data/hsa_pct_urban.rds")
df_all3 <- df_all3 %>%
  dplyr::ungroup()
sfc_cols <- names(df_all3)[sapply(df_all3, inherits, what = "sfc")]

sfc_cols
df_all3_nogeo <- df_all3 %>%
  dplyr::select(-dplyr::all_of(sfc_cols))

head(df_all3_nogeo)
library(mgcv)

df_all3$y_h1 <- log(df_all3$diff_wis_overall_h1 - min(df_all3$diff_wis_overall_h1) + 0.01)
df_all3$y_h2 <- log(df_all3$diff_wis_overall_h2 - min(df_all3$diff_wis_overall_h2) + 0.01)
df_all3$y_h3 <- log(df_all3$diff_wis_overall_h3 - min(df_all3$diff_wis_overall_h3) + 0.01)
df_all3$y_h4 <- log(df_all3$diff_wis_overall_h4 - min(df_all3$diff_wis_overall_h4) + 0.01)


lm_fit <- function(d, target_var) {
  fml <- as.formula(paste0(
    target_var, " ~ log_pop_ratio * log_urban_pop_share + n_msa"
  ))
  lm(fml, data = d)
}

cal_cv_R2(tmp = df_all3, n_k = 5, fit_fun = lm_fit, target_var ="y_h1")
cal_cv_R2(tmp = df_all3, n_k = 5, fit_fun = lm_fit, target_var ="y_h2")
cal_cv_R2(tmp = df_all3, n_k = 5, fit_fun = lm_fit, target_var ="y_h3")


gam_fit <- function(d, target_var) {
  fml <- as.formula(paste0(
    target_var,
    " ~ te(log_pop_ratio, log_urban_pop_share, k=c(12,12)) + s(n_msa)"
  ))
  gam(fml, data = d, method = "REML")
}
cal_cv_R2(tmp = df_all3, n_k = 5, fit_fun = gam_fit, target_var = "y_h1")
cal_cv_R2(tmp = df_all3, n_k = 5, fit_fun = gam_fit, target_var = "y_h2")
cal_cv_R2(tmp = df_all3, n_k = 5, fit_fun = gam_fit, target_var = "y_h3")

bart_mm_formula <- ~ log_pop_ratio + log_urban_pop_share + n_hsa

cal_cv_R2(tmp = df_all3, n_k = 5, fit_fun = bart_fit, predict_fun = bart_predict, target_var = "y_h1")
cal_cv_R2(tmp = df_all3, n_k = 5, fit_fun = bart_fit, predict_fun = bart_predict, target_var = "y_h2")
cal_cv_R2(tmp = df_all3, n_k = 5, fit_fun = bart_fit, predict_fun = bart_predict, target_var = "y_h3")

lm_res1 <- lm_fit(d = df_all3, target_var = "y_h1")
summary(lm_res1)
lm_res2 <- lm_fit(d = df_all3, target_var = "y_h2")
summary(lm_res2)
lm_res3 <- lm_fit(d = df_all3, target_var = "y_h3")
summary(lm_res3)

gam_res1 <- gam_fit(d = df_all3, target_var = "y_h1")

pdf("Local-Level-Forecasting/paper_figures/lm_res_pct_share.pdf", width = 4, height = 4)
lm_diag_figs(lm_res1, ylabs = rep(expression("Partial effect on " * Delta * " WIS"), 4),
             xlabs = c("HSA-state population ratio (log)",
                       "Proportion of urban population (log)",
                       "Number of MSAs state")
             , target_var = "y_h1")
dev.off()

pdf("Local-Level-Forecasting/paper_figures/gam_res_pct_share.pdf", width = 4, height = 4)
gam_diag_figs(gam_res1, 
              ylabs = rep(expression("Partial Effect on " * Delta * " WIS"), 4),
              xlabs = c("HSA-to-state population ratio (Log)",
                        # "Proportion of urban population (log)",
                        # "HSA-to-state area ratio (log)",
                        "Number of MSAs per state"))

dev.off()



### create interaction graph, choosed the percentiles
z_vals <- quantile(df_all3$log_urban_pop_share, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)
# x grid
x_seq <- seq(min(df_all3$log_pop_ratio, na.rm = TRUE),
             max(df_all3$log_pop_ratio, na.rm = TRUE),
             length.out = 100)

## ---- 1) Prediction grid (nd) ----
nd <- expand.grid(
  log_pop_ratio = x_seq,
  log_urban_pop_share = as.numeric(z_vals),
  n_msa         = median(df_all3$n_msa, na.rm = TRUE)
)

## ---- 2) Predict on link scale + 95% CI ----
pr <- predict(lm_res1, newdata = nd, interval = "confidence", level = 0.95)
nd$fit <- pr[, "fit"]
nd$lo  <- pr[, "lwr"]
nd$hi  <- pr[, "upr"]

pr <- predict(gam_res1, newdata = nd, type = "link", se.fit = TRUE)
nd$fit <- pr$fit
nd$lo  <- pr$fit - 1.96 * pr$se.fit
nd$hi  <- pr$fit + 1.96 * pr$se.fit


nd$urban_level <- factor(
  nd$log_urban_pop_share,
  levels = as.numeric(z_vals),
  labels = c("10 percentile", "50 percentile", "90 percentile")
)

# 색과 라벨을 effect plot과 동일하게 정의
cols <- c("#1f78b4", "#fdbf6f", "#33a02c" )
labs <- c("10 percentile", "50 percentile", "90 percentile")
y_col = "y_h1"

## ---- 3) Raw data for sanity check overlay ----
## Create urban percentile bands (0–20th, 80–100th, Other)
tmp_plot <- df_all3 %>%
  ungroup() %>%
  filter(
    !is.na(log_pop_ratio),
    !is.na(log_urban_pop_share),
    !is.na(.data[[y_col]])
  ) %>%
  mutate(
    urban_pct = percent_rank(log_urban_pop_share) * 100,
    urban_band = case_when(
      urban_pct <= 20 ~ "0-20 percentile",
      urban_pct >= 80 ~ "80-100 percentile",
      TRUE ~ "Other"
    ),
    urban_band = factor(
      urban_band,
      levels = c("0-20 percentile", "80-100 percentile", "Other")
    )
  )

## ---- 4) Plot ----
out_png <- "Local-Level-Forecasting/paper_figures/GAM_fitting4_log_wis_var3_interaction_term_nmsa_paper_h1.png"
out_png <- "Local-Level-Forecasting/paper_figures/LM_fitting4_log_wis_var3_interaction_term_nmsa_paper_h1.png"

png(out_png, width = 5.5, height = 3.5, units = "in", res = 600)
library(ggnewscale)

p <- ggplot(nd, aes(x = log_pop_ratio, y = fit,
                    color = urban_level, fill = urban_level)) +
  
  geom_line(linewidth = 1.1) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, color = NA) +
  
  scale_color_manual(
    values = cols,
    labels = labs,
    name   = "Estimated"
  ) +
  scale_fill_manual(
    values = cols,
    labels = labs,
    name   = "Estimated"
  ) +
  ggnewscale::new_scale_color() +
  
  geom_point(
    data = tmp_plot,
    aes(x = log_pop_ratio, y = .data[[y_col]], color = urban_band, shape = urban_band),
    inherit.aes = FALSE,
    size  = 2
  ) +
  scale_color_manual(
    values = c("0-20 percentile"   = "#1f78b4",
               "80-100 percentile" = "#33a02c",
               "Other"          = "grey70"), 
    #name = "Urbanization\npercentile"
    guide = "none"
  ) +
  scale_shape_manual(
    values = c("0-20 percentile" = 16, "80-100 percentile" = 17, "Other" = 1),
    name   = "Observed"
  ) +
  guides(
    shape = guide_legend(
      override.aes = list(
        color = c("#1f78b4", "#33a02c", "grey70")
      )
    )
  ) +
  labs(
    x = "HSA-state population ratio (log)",
    y = expression("" * Delta * " MWIS")
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.title = element_text(size = 15),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 10)
  ) 
print(p)
dev.off()

### BART
predictors <- c("log_pop_ratio", "log_urban_pop_share", "n_msa")
target_var <- "y_h1"

bart_res1 <- bart(
  x.train   = df_all3[, predictors, drop = FALSE],
  y.train   = df_all3[[target_var]],
  verbose   = FALSE,
  keeptrees = TRUE
)

predictor_multiline_names = c("HSA-to-state \n population \n ratio (log)",
                              "Percent \n urbanization \n (log)",
                              "Number \n of MSAs \n per state")

X <- df_all3[, predictors, drop=FALSE]

ylabs = rep(expression("Partial Effect on log(shifted " * Delta * " WIS"), 4)
xlabs = c("HSA-to-state population ratio (log)",
          "Proportion of urban population (log)",
          "Number of MSAs per state")
bart_diag_figs(bart_res1, xlabs, ylabs, predictor_multiline_names, predictors)

pdf("Local-Level-Forecasting/paper_figures/bart_res_pct_share.pdf", width = 4, height = 4)
bart_diag_figs(bart_res1, 
               ylabs = rep(expression("Partial Effect on " * Delta * " WIS"), 4),
               xlabs = c("HSA-to-state population ratio (Log)",
                         "Proportion of urban population (log)",
                         "Number of MSAs per state"),
               predictor_multiline_names, predictors)
dev.off()






m_add <- gam(
  y_h1 ~ s(log_pop_ratio, k = 6) +
    s(log_urban_pop_share, k = 6) +
    s(n_msa, k = 5),
  data = df_all3,
  method = "REML"
)

m_int <- gam(
  y_h1 ~ s(log_pop_ratio, k = 6) +
    s(log_urban_pop_share, k = 6) +
    ti(log_pop_ratio, log_urban_pop_share, k = c(6, 6)) +
    s(n_msa, k = 5),
  data = df_all3,
  method = "REML",
  select = TRUE
)

AIC(m_add, m_int)
anova(m_add, m_int, test = "F")
summary(m_int)

m_add <- lm(
  y_h1 ~ log_pop_ratio * log_urban_pop_share + n_msa,
  data = df_all3
)

m_int <- lm(
  y_h1 ~ log_pop_ratio + log_urban_pop_share + n_msa,
  data = df_all3
)

AIC(m_add, m_int)
anova(m_add, m_int, test = "F")
summary(m_int)
summary(m_add)

##############################################################################
##############################################################################
m_lin <- lm(
  y_h1 ~ log_pop_ratio * log_urban_pop_share + n_msa,
  data = df_all3
)

nmsa_ref <- median(df_all3$n_msa, na.rm = TRUE)
z_vals <- quantile(
  df_all3$log_urban_pop_share,
  probs = c(0.1, 0.5, 0.9),
  na.rm = TRUE
)

x_seq <- seq(
  min(df_all3$log_pop_ratio, na.rm = TRUE),
  max(df_all3$log_pop_ratio, na.rm = TRUE),
  length.out = 100
)

nd_ref <- expand.grid(
  log_pop_ratio = x_seq,
  log_urban_pop_share = as.numeric(z_vals),
  n_msa = nmsa_ref
)

pr <- predict(m_lin, newdata = nd_ref, se.fit = TRUE)

nd_ref$fit <- pr$fit
nd_ref$lo  <- pr$fit - 1.96 * pr$se.fit
nd_ref$hi  <- pr$fit + 1.96 * pr$se.fit

nd_ref$urban_level <- factor(
  nd_ref$log_urban_pop_share,
  levels = as.numeric(z_vals),
  labels = c("10 percentile", "50 percentile", "90 percentile")
)

b_nmsa <- coef(m_lin)["n_msa"]

df_plot <- df_all3 %>%
  mutate(
    y_adj = y_h1 - b_nmsa * (n_msa - nmsa_ref)
  ) %>%
  ungroup()

library(ggplot2)
library(dplyr)

ggplot() +
  geom_point(
    data = df_plot,
    aes(x = log_pop_ratio, y = y_adj),
    color = "grey70", alpha = 0.5, size = 1.8
  ) +
  geom_ribbon(
    data = nd_ref,
    aes(x = log_pop_ratio, ymin = lo, ymax = hi, fill = urban_level),
    alpha = 0.18, color = NA
  ) +
  geom_line(
    data = nd_ref,
    aes(x = log_pop_ratio, y = fit, color = urban_level),
    linewidth = 1.2
  ) +
  scale_color_manual(values = c("#1f78b4", "#fdbf6f", "#33a02c")) +
  scale_fill_manual(values = c("#1f78b4", "#fdbf6f", "#33a02c")) +
  labs(
    x = "HSA–state population ratio (log)",
    y = expression(Delta~MWIS*" (adjusted for n_msa)"),
    color = "Urban population share",
    fill = "Urban population share"
  ) +
  theme_bw(base_size = 13)


df_plot <- df_plot %>%
  mutate(
    urban_pct = percent_rank(urban_pop_share) * 100,
    urban_band = case_when(
      urban_pct <= 20 ~ "Low",
      urban_pct >= 80 ~ "High",
      TRUE ~ "Other"
    )
  )



ggplot() +
  # 1️⃣ 전체 점 (배경)
  geom_point(
    data = df_plot,
    aes(x = log_pop_ratio, y = y_adj),
    color = "grey80", alpha = 0.4, size = 1.5
  ) +
  
  # 2️⃣ low urban (파란 점)
  geom_point(
    data = df_plot %>% filter(urban_band == "Low"),
    aes(x = log_pop_ratio, y = y_adj),
    color = "#1f78b4", size = 2.2
  ) +
  
  # 3️⃣ high urban (초록 삼각형 ⭐)
  geom_point(
    data = df_plot %>% filter(urban_band == "High"),
    aes(x = log_pop_ratio, y = y_adj),
    color = "#33a02c", shape = 17, size = 2.8
  ) +
  
  # 4️⃣ CI
  geom_ribbon(
    data = nd_ref,
    aes(x = log_pop_ratio, ymin = lo, ymax = hi, fill = urban_level),
    alpha = 0.18, color = NA
  ) +
  
  # 5️⃣ fitted lines
  geom_line(
    data = nd_ref,
    aes(x = log_pop_ratio, y = fit, color = urban_level),
    linewidth = 1.2
  ) +
  
  scale_color_manual(
    values = c("#1f78b4", "#fdbf6f", "#33a02c"),
    name = "Urban population share"
  ) +
  
  scale_fill_manual(
    values = c("#1f78b4", "#fdbf6f", "#33a02c"),
    name = "Urban population share"
  ) +
  
  labs(
    x = "HSA–state population ratio (log)",
    y = expression(Delta~MWIS*" (adjusted for n_msa)")
  ) +
  
  theme_bw(base_size = 13) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )


library(mgcv)

m_gam <- gam(
  y_h1 ~ te(log_pop_ratio , log_urban_pop_share) + s(n_msa),
  data = df_all3,
  method = "REML"
)

nmsa_ref <- median(df_all3$n_msa, na.rm = TRUE)

z_vals <- quantile(
  df_all3$log_urban_pop_share,
  probs = c(0.1, 0.5, 0.9),
  na.rm = TRUE
)

x_seq <- seq(
  min(df_all3$log_pop_ratio, na.rm = TRUE),
  max(df_all3$log_pop_ratio, na.rm = TRUE),
  length.out = 100
)

nd_ref <- expand.grid(
  log_pop_ratio = x_seq,
  log_urban_pop_share = as.numeric(z_vals),
  n_msa = nmsa_ref
)

pr <- predict(m_gam, newdata = nd_ref, se.fit = TRUE)

nd_ref$fit <- pr$fit
nd_ref$lo  <- pr$fit - 1.96 * pr$se.fit
nd_ref$hi  <- pr$fit + 1.96 * pr$se.fit

nd_ref$urban_level <- factor(
  nd_ref$log_urban_pop_share,
  levels = as.numeric(z_vals),
  labels = c("10 percentile", "50 percentile", "90 percentile")
)

terms_pred <- predict(m_gam, type = "terms")

df_plot <- df_all3 %>%
  ungroup() %>%
  mutate(
    s_nmsa = terms_pred[, "s(n_msa)"],
    y_adj = y_h1 - s_nmsa + mean(s_nmsa, na.rm = TRUE)
  )


df_plot <- df_plot %>%
  mutate(
    urban_pct = percent_rank(urban_pop_share) * 100,
    urban_band = case_when(
      urban_pct <= 20 ~ "Low",
      urban_pct >= 80 ~ "High",
      TRUE ~ "Other"
    )
  )


#####################################################################

b <- coef(lm_res1)
V <- vcov(lm_res1)

z_grid <- seq(
  min(df_all3$log_urban_pop_share, na.rm = TRUE),
  max(df_all3$log_urban_pop_share, na.rm = TRUE),
  length.out = 200
)

me_df <- data.frame(
  log_urban_pop_share = z_grid
) %>%
  mutate(
    slope = b["log_pop_ratio"] + b["log_pop_ratio:log_urban_pop_share"] * log_urban_pop_share,
    se = sqrt(
      V["log_pop_ratio", "log_pop_ratio"] +
        log_urban_pop_share^2 * V["log_pop_ratio:log_urban_pop_share", "log_pop_ratio:log_urban_pop_share"] +
        2 * log_urban_pop_share * V["log_pop_ratio", "log_pop_ratio:log_urban_pop_share"]
    ),
    lo = slope - 1.96 * se,
    hi = slope + 1.96 * se
  )


png("paper_figures/lm_marginal_pct_urban.png", 
    width = 5, height = 5, units = "in", res = 600)
ggplot(me_df, aes(x = log_urban_pop_share, y = slope)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2) +
  geom_line(linewidth = 1.1) +
  labs(
    x = "Proportion of urban population (log)",
    y = "Marginal effect of \n HSA-state population ratio (log)"
  ) +
  theme_bw(base_size = 13)
dev.off()


x_grid <- seq(
  min(df_all3$log_pop_ratio, na.rm = TRUE),
  max(df_all3$log_pop_ratio, na.rm = TRUE),
  length.out = 200
)

me_df <- data.frame(
  log_pop_ratio = x_grid
) %>%
  mutate(
    slope = b["log_urban_pop_share"] + 
      b["log_pop_ratio:log_urban_pop_share"] * log_pop_ratio,
    
    se = sqrt(
      V["log_urban_pop_share", "log_urban_pop_share"] +
        log_pop_ratio^2 * V["log_pop_ratio:log_urban_pop_share", "log_pop_ratio:log_urban_pop_share"] +
        2 * log_pop_ratio * V["log_urban_pop_share", "log_pop_ratio:log_urban_pop_share"]
    ),
    
    lo = slope - 1.96 * se,
    hi = slope + 1.96 * se
  )

png("paper_figures/lm_marginal_pop_ratio.png", 
    width = 5, height = 5, units = "in", res = 600)
ggplot(me_df, aes(x = log_pop_ratio, y = slope)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2) +
  geom_line(linewidth = 1.1) +
  labs(
    x = "HSA-to-state population ratio (log)",
    y = "Marginal effect of \n proportion of urban population (log)"
  ) +
  theme_bw(base_size = 13)
dev.off()

########################################################################
## main effect figure
# grid


x_grid <- seq(
  min(df_all3$log_pop_ratio, na.rm = TRUE),
  max(df_all3$log_pop_ratio, na.rm = TRUE),
  length.out = 200
)

# reference values (고정값)
urban_ref <- mean(df_all3$log_urban_pop_share, na.rm = TRUE)
msa_ref   <- mean(df_all3$n_msa, na.rm = TRUE)

# new data
main_df <- data.frame(
  log_pop_ratio = x_grid,
  log_urban_pop_share = urban_ref,
  n_msa = msa_ref
)

main_df <- data.frame(
  log_pop_ratio = df_all3$log_pop_ratio,
  log_urban_pop_share = urban_ref,
  n_msa = msa_ref
)

# prediction
pred_obj <- predict(lm_res1, newdata = main_df, se.fit = TRUE)

main_df <- main_df %>%
  mutate(
    pred = pred_obj$fit,
    se   = pred_obj$se.fit,
    lo   = pred_obj$fit - 1.96 * pred_obj$se.fit,
    hi   = pred_obj$fit + 1.96 * pred_obj$se.fit
  )


png("paper_figures/lm_main_pop_ratio.png", 
    width = 5, height = 5, units = "in", res = 600)
ggplot(main_df, aes(x = log_pop_ratio, y = pred)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2) +
  geom_line(linewidth = 1.1) +
  geom_point(aes(x = log_pop_ratio, y = lm_res1$model$y_h1)) +
  labs(
    x = "HSA-to-state population ratio (log)",
    y = expression(paste("Main effect on ", Delta, "MWIS"))
  ) +
  theme_bw(base_size = 13)

dev.off()

###########################################
### main effect with observations figure
# grid
x_grid <- seq(
  min(df_all3$log_pop_ratio, na.rm = TRUE),
  max(df_all3$log_pop_ratio, na.rm = TRUE),
  length.out = 200
)

# reference values
urban_ref <- mean(df_all3$log_urban_pop_share, na.rm = TRUE)
msa_ref   <- mean(df_all3$n_msa, na.rm = TRUE)

# new data
main_df <- data.frame(
  log_pop_ratio = x_grid,
  log_urban_pop_share = urban_ref,
  n_msa = msa_ref
)

# prediction
pred_obj <- predict(lm_res1, newdata = main_df, se.fit = TRUE)

main_df <- main_df %>%
  mutate(
    pred = pred_obj$fit,
    se   = pred_obj$se.fit,
    lo   = pred_obj$fit - 1.96 * pred_obj$se.fit,
    hi   = pred_obj$fit + 1.96 * pred_obj$se.fit
  )

# adjusted points for plotting
coef_vec <- coef(lm_res1)

plot_df <- lm_res1$model %>%
  mutate(
    partial_ratio = resid(lm_res1) +
      coef_vec["log_pop_ratio"] * log_pop_ratio +
      coef_vec["log_pop_ratio:log_urban_pop_share"] * log_pop_ratio * log_urban_pop_share
  )

png("paper_figures/lm_main_pop_ratio.png", 
    width = 5, height = 5, units = "in", res = 600)

ggplot() +
  geom_point(
    data = plot_df,
    aes(x = log_pop_ratio, y = partial_ratio),
    alpha = 0.25, size = 1
  ) +
  geom_ribbon(
    data = main_df,
    aes(x = log_pop_ratio, ymin = lo, ymax = hi),
    alpha = 0.2
  ) +
  geom_line(
    data = main_df,
    aes(x = log_pop_ratio, y = pred),
    linewidth = 1.1
  ) +
  labs(
    x = "HSA-to-state population ratio (log)",
    y = expression(paste("Main effect on ", Delta, "MWIS"))
  ) +
  theme_bw(base_size = 13)

dev.off()


###############3

library(dplyr)
library(ggplot2)

# 1. 모델 계수 추출
b0 <- coef(lm_res1)["(Intercept)"]
b_pop <- coef(lm_res1)["log_pop_ratio"]
b_urban <- coef(lm_res1)["log_urban_pop_share"]
b_msa <- coef(lm_res1)["n_msa"]
b_inter <- coef(lm_res1)["log_pop_ratio:log_urban_pop_share"]

# 2. 데이터 처리 및 '조정된 Y값' 계산
d_plot <- lm_res1$model %>%
  mutate(
    # n_msa의 효과만 제거 (다른 변수들은 interaction을 위해 남겨둠)
    # 이것이 Nick이 말한 'Partial Residual'의 본질입니다.
    y_adj = y_h1 - (b_msa * n_msa),
    
    # 도시화율에 따른 3분위수 분할
    urban_group = ntile(log_urban_pop_share, 3),
    urban_label = factor(urban_group, labels = c("Less Urban (0-33%)", "Moderately Urban (33-67%)", "More Urban (67-100%)"))
  )

# 3. 각 그룹별 중앙값에서 예측 선 생성
global_x_range <- range(d_plot$log_pop_ratio, na.rm = TRUE)
plot_lines <- d_plot %>%
  group_by(urban_label) %>%
  do({
    med_urban <- median(.$log_urban_pop_share)
    
    # 2. 개별 그룹의 범위가 아니라, 미리 구한 'global_x_range'를 사용합니다.
    x_seq <- seq(global_x_range[1], global_x_range[2], length.out = 100)
    
    # 예측값 계산 (동일한 식)
    y_pred <- b0 + (b_pop * x_seq) + (b_urban * med_urban) + (b_inter * x_seq * med_urban)
    
    data.frame(log_pop_ratio = x_seq, pred = y_pred)
  })


# 4. 그래프 그리기
png("paper_figures/partial_ratio_by_urban_tercile.png", 
    width = 9, height = 5, units = "in", res = 600)

# (plot_lines 계산 부분은 위에서 만든 로직 그대로 사용)
ggplot(d_plot, aes(x = log_pop_ratio, y = y_adj)) +
  # 점 스타일: alpha 0.4, size 1.5로 메인 그래프와 통일
  geom_point(alpha = 0.4, color = "black", size = 1.5) +  
  # 선 스타일: linewidth 1.2, color firebrick으로 통일
  geom_line(data = plot_lines, aes(y = pred), color = "firebrick", linewidth = 1.2) + 
  facet_wrap(~urban_label) +
  labs(
    x = "HSA-state population ratio (log)",
    y = "Adjusted forecast improvement (Δ MWIS)"
  ) +
  theme_bw(base_size = 14) + # 폰트 크기 14 통일
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"), # 패널 제목 배경 하얗게 (깔끔함)
    axis.title = element_text(size = 14)
  )

dev.off()


####################
# 1. 잔차 계산
d_plot$res <- residuals(lm_res1)


pdf("paper_figures/lm_res_residuals.pdf", width = 4, height = 4)

# 2. Residuals vs. log_pop_ratio 그래프
ggplot(d_plot, aes(x = log_pop_ratio, y = res)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # 기준선 (0)
  #geom_smooth(method = "loess", color = "blue") + # 잔차의 패턴을 보기 위한 부드러운 곡선
  labs(
    x = "HSA-to-state population ratio (log)",
    y = "Residuals",
    #title = "Residuals vs. Predictor (HSA-to-state population ratio)",
    #subtitle = "Checking for systematic patterns or non-linearity"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"), # 패널 제목 배경 하얗게 (깔끔함)
    axis.title = element_text(size = 14)
  )

ggplot(d_plot, aes(x = log_urban_pop_share, y = res)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # 기준선 (0)
  #geom_smooth(method = "loess", color = "blue") + # 잔차의 패턴을 보기 위한 부드러운 곡선
  labs(
    x = "Proportion of urban population (log)",
    y = "Residuals",
    #title = "Residuals vs. Predictor (Proportion of urban population)",
    #subtitle = "Checking for systematic patterns or non-linearity"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"), # 패널 제목 배경 하얗게 (깔끔함)
    axis.title = element_text(size = 14)
  )
dev.off()






library(dplyr)
library(ggplot2)

# 1. 계수 추출
b0 <- coef(lm_res1)["(Intercept)"]
b_pop <- coef(lm_res1)["log_pop_ratio"]
b_urban <- coef(lm_res1)["log_urban_pop_share"]
b_msa <- coef(lm_res1)["n_msa"]
b_inter <- coef(lm_res1)["log_pop_ratio:log_urban_pop_share"]

# 2. 모델 데이터 + partial residual 계산
d_plot <- lm_res1$model %>%
  mutate(
    resid = resid(lm_res1),
    urban_group = ntile(log_urban_pop_share, 3),
    urban_label = factor(
      urban_group,
      labels = c("Less urban HSAs",
                 "Moderately urban HSAs",
                 "More urban HSAs")
    ),
    pr_log_pop = resid +
      b_pop * log_pop_ratio +
      b_inter * log_pop_ratio * log_urban_pop_share
  )

# 3. 각 urban tercile의 median urban 값에서 직선 생성
global_x_range <- range(d_plot$log_pop_ratio, na.rm = TRUE)

plot_lines <- d_plot %>%
  group_by(urban_label) %>%
  summarise(
    med_urban = median(log_urban_pop_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  do({
    med_urban <- .$med_urban
    this_label <- .$urban_label
    
    x_seq <- seq(global_x_range[1], global_x_range[2], length.out = 100)
    
    # partial residual plot 위에 올릴 "effect line"
    # intercept는 포함하지 않고, x 관련 contribution만 그림
    pr_line <- b_pop * x_seq + b_inter * x_seq * med_urban
    
    data.frame(
      urban_label = this_label,
      log_pop_ratio = x_seq,
      pr_line = pr_line
    )
  }) %>%
  ungroup()

# 4. plot
png("Local-Level-Forecasting/paper_figures/partial_residual_ratio_by_urban_tercile.png",
    width = 9, height = 5, units = "in", res = 600)

ggplot(d_plot, aes(x = log_pop_ratio, y = pr_log_pop)) +
  geom_point(alpha = 0.4, color = "black", size = 1.5) +
  geom_line(
    data = plot_lines,
    aes(x = log_pop_ratio, y = pr_line),
    color = "firebrick",
    linewidth = 1.2
  ) +
  facet_wrap(~urban_label) +
  labs(
    x = "Log HSA-state population ratio",
    y = expression("Partial residual for " * Delta * "MWIS")
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(size = 14)
  )

dev.off()


# 1. n_msa의 부분 잔차 계산
# 다른 모든 변수의 효과를 제거하고 n_msa의 효과만 남깁니다.
d_plot_msa <- lm_res1$model %>%
  mutate(
    resid = resid(lm_res1),
    # n_msa의 부분 잔차: 잔차 + (n_msa의 계수 * n_msa의 실제값)
    pr_n_msa = resid + b_msa * n_msa
  )

# 2. 회귀선 생성을 위한 데이터 (나머지 변수는 평균으로 고정)
# 상호작용 항이 있는 경우, n_msa의 기울기는 다른 변수에 의해 변하지 않으므로 
# 단순하게 b_msa * x_seq 로 그릴 수 있습니다.
msa_range <- range(d_plot_msa$n_msa, na.rm = TRUE)
msa_seq <- seq(msa_range[1], msa_range[2], length.out = 100)
msa_line <- data.frame(
  n_msa = msa_seq,
  pr_line = b_msa * msa_seq  # b0를 더하지 않으면 0을 지나는 직선이 됩니다.
)

# 3. Plot
png("Local-Level-Forecasting/paper_figures/partial_residual_n_msa.png",
    width = 5, height = 4, units = "in", res = 600)

ggplot(d_plot_msa, aes(x = n_msa, y = pr_n_msa)) +
  geom_point(alpha = 0.4, color = "black", size = 1.5) +
  geom_line(data = msa_line, aes(x = n_msa, y = pr_line), 
            color = "firebrick", linewidth = 1.2) +
  labs(
    x = "Number of MSAs per state",
    y = expression("Partial residual for " * Delta * "MWIS")
    #title = "Partial Residual Plot for n_msa"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(size = 14)
  )

dev.off()

library(interactions)

# log_urban_pop_share의 값에 따른 log_pop_ratio의 효과 시각화
interact_plot(lm_res1, 
              pred = log_pop_ratio, 
              modx = log_urban_pop_share, 
              partial.residuals = TRUE) # 부분 잔차 표시 옵션


## GAM centered partial residuals
gam_res1 <- gam_fit(d = df_all3, target_var = "y_h1")

term_mat <- predict(gam_res1, type = "terms")
term_names <- colnames(term_mat)

term_pop <- term_names[grepl("log_pop_ratio", term_names)]
term_msa <- term_names[grepl("n_msa", term_names)]

## A. log_pop_ratio by urban tercile

d_plot <- gam_res1$model %>%
  dplyr::mutate(
    resid = residuals(gam_res1, type = "response"),
    urban_group = dplyr::ntile(log_urban_pop_share, 3),
    urban_label = factor(
      urban_group,
      labels = c(
        "Less urban HSAs",
        "Moderately urban HSAs",
        "More urban HSAs"
      )
    ),
    pop_term = term_mat[, term_pop],
    partial_resid = resid + pop_term
  )

global_x_range <- range(d_plot$log_pop_ratio, na.rm = TRUE)

plot_grid <- d_plot %>%
  dplyr::group_by(urban_label) %>%
  dplyr::summarise(
    log_urban_pop_share = median(log_urban_pop_share, na.rm = TRUE),
    n_msa = median(n_msa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::crossing(
    log_pop_ratio = seq(global_x_range[1], global_x_range[2], length.out = 100)
  )

grid_terms <- predict(
  gam_res1,
  newdata = plot_grid,
  type = "terms"
)

plot_grid$pop_term <- grid_terms[, term_pop]

png(
  "Local-Level-Forecasting/paper_figures/partial_residual_gam_ratio_by_urban_tercile_centered.png",
  width = 9,
  height = 5,
  units = "in",
  res = 600
)

ggplot(d_plot, aes(x = log_pop_ratio, y = partial_resid)) +
  geom_point(alpha = 0.4, color = "black", size = 1.5) +
  geom_line(
    data = plot_grid,
    aes(x = log_pop_ratio, y = pop_term),
    color = "firebrick",
    linewidth = 1.2
  ) +
  facet_wrap(~ urban_label) +
  labs(
    x = "Log HSA-state population ratio",
    y = expression("Partial residual for " * Delta * "MWIS")
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(size = 14)
  )

dev.off()


## B. n_msa centered partial residual

d_plot_msa <- gam_res1$model %>%
  dplyr::mutate(
    resid = residuals(gam_res1, type = "response"),
    msa_term = term_mat[, term_msa],
    partial_resid = resid + msa_term
  )

msa_range <- range(d_plot_msa$n_msa, na.rm = TRUE)

msa_line <- data.frame(
  n_msa = seq(msa_range[1], msa_range[2], length.out = 100),
  log_pop_ratio = median(d_plot_msa$log_pop_ratio, na.rm = TRUE),
  log_urban_pop_share = median(d_plot_msa$log_urban_pop_share, na.rm = TRUE)
)

msa_terms <- predict(
  gam_res1,
  newdata = msa_line,
  type = "terms"
)

msa_line$msa_term <- msa_terms[, term_msa]

png(
  "Local-Level-Forecasting/paper_figures/partial_residual_n_msa_gam_centered.png",
  width = 5,
  height = 4,
  units = "in",
  res = 600
)

ggplot(d_plot_msa, aes(x = n_msa, y = partial_resid)) +
  geom_point(alpha = 0.4, color = "black", size = 1.5) +
  geom_line(
    data = msa_line,
    aes(x = n_msa, y = msa_term),
    color = "firebrick",
    linewidth = 1.2
  ) +
  labs(
    x = "Number of MSAs per state",
    y = expression("Partial residual for " * Delta * "MWIS")
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(size = 14)
  )

dev.off()



