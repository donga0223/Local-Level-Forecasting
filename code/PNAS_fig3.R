
library(dplyr)
library(ggplot2)
library(stringr)
library(dplyr)
library(epidatr)


source("code/Analysis/fitting_diff_WIS_figures_ftn.R")
df_all3 <- readRDS("data/hsa_pct_urban.rds")
df_all3 <- df_all3 %>%
  dplyr::ungroup()

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


lm_res1 <- lm_fit(d = df_all3, target_var = "y_h1")
summary(lm_res1)


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


p3A <- ggplot(d_plot, aes(x = log_pop_ratio, y = pr_log_pop)) +
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
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9)
  )



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


p3B <- ggplot(d_plot_msa, aes(x = n_msa, y = pr_n_msa)) +
  geom_point(alpha = 0.4, color = "black", size = 1.5) +
  geom_line(data = msa_line, aes(x = n_msa, y = pr_line), 
            color = "firebrick", linewidth = 1.2) +
  labs(
    x = "Number of MSAs per state",
    y = expression("Partial residual for " * Delta * "MWIS")
    #title = "Partial Residual Plot for n_msa"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9)
  )




hsa4 <- readRDS("data/all_us_3var_geo.rds")
diff_wis <- read.csv("data/GBQR_diff_wis.csv")
unique(diff_wis$hsa_nci_id)

hsa5 <- hsa4 %>%
  filter(population_hsa >= 250000) %>%
  mutate()

df_all3 <- readRDS("data/hsa_pct_urban.rds")
head(df_all3)


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


lm_res1 <- lm_fit(d = df_all3, target_var = "y_h1")
summary(lm_res1)


predictors <- names(lm_res1$model)[-1]
target_var <- names(lm_res1$model)[1]

newdata <- hsa5[, predictors, drop = FALSE]

pred <- predict(
  lm_res1,
  newdata = newdata,
  type = "response",
  se.fit = TRUE
)

df_pred <- hsa5 %>%
  ungroup() %>%
  mutate(
    pred_mean = as.numeric(pred$fit),
    pred_tran_mean = exp(pred_mean) - 0.00171825 - 0.01,
    se_hat          = as.numeric(pred$se.fit),
    lwr95           = pred_mean - 1.96 * se_hat,
    upr95           = pred_mean + 1.96 * se_hat
  )



df_pred_us <- sf::st_as_sf(df_pred)
df_pred_us <- df_pred_us %>%
  sf::st_transform(4326)

us_bbox <- sf::st_bbox(
  c(
    xmin = -125,
    xmax = -66,
    ymin = 24,
    ymax = 50
  ),
  crs = 4326
)

df_pred_us <- sf::st_crop(df_pred_us, us_bbox)
st_sf <- states(cb = TRUE, year = 2022)

st_sf_us <- st_sf %>%
  sf::st_as_sf() %>%
  sf::st_transform(4326) %>%
  dplyr::filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))

county_sf <- counties(cb = TRUE, year = 2022)

hsa <- readxl::read_xls("data/Health.Service.Areas.xls") 
#View(hsa %>% filter(str_detect(`State-county`, "^VA:")))

hsa <- hsa %>%
  mutate(`State-county` = str_remove(`State-county`, "\\s*\\(\\d+\\)$")) %>%
  tidyr::extract(`State-county`,
                 into = c("state_abbr", "county_name"),
                 regex = "^([A-Z]{2}):\\s*(.*)$" ) %>%
  dplyr::mutate(
    county_name = stringr::str_remove(county_name, " County$")
  ) 

names(hsa) <- c("hsa_nci_id", "HSA Description", "state_abbr", "county_name", "fips")

hsa_sf <- county_sf %>%
  left_join(hsa, by = c("STUSPS" = "state_abbr", "GEOID" = "fips")) %>%
  filter(!is.na(hsa_nci_id)) %>%
  group_by(STATE_NAME, STUSPS, STATEFP, hsa_nci_id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")


hsa_sf_us <- hsa_sf %>%
  sf::st_as_sf() %>%
  sf::st_transform(4326) %>%
  dplyr::filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))

df_pred_us <- df_pred_us %>%
  sf::st_as_sf() %>%
  sf::st_transform(4326)

fill_var = "pred_tran_mean"
selected_ids <- unique(diff_wis$hsa_nci_id)

hsa_highlight <- hsa_sf_us |>
  dplyr::filter(hsa_nci_id %in% selected_ids)

p3 <- ggplot() +
  geom_sf(
    data = df_pred_us,
    aes(fill = .data[[fill_var]]),
    color = "gray20"
  ) +
  geom_sf(
    data = hsa_sf_us,
    fill = NA,
    color = "grey20",
    linewidth = 0.25
  ) +
  geom_sf(
    data = st_sf_us,
    fill = NA,
    color = "gray20"
  ) +
  coord_sf(
    xlim = c(-125, -66),
    ylim = c(24, 50),
    expand = FALSE
  ) +
  theme_void() +
  labs(title = "") +
  theme(
    legend.title = element_text(size = 15),
    legend.text  = element_text(size = 15)
  )

p4 <- p3 +
  scale_fill_viridis_c(option = "turbo", direction = 1,na.value = "white",
                       name = expression(atop("Predicted", Delta * " MWIS"))
                       #limits = c(min(df_pred[[target_var]], na.rm = TRUE),
                       #            max(df_pred[[target_var]], na.rm = TRUE))
  )

p3C <- p4 +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    plot.margin = margin(5, 5, 5, 5)
  )


library(patchwork)

# Top row: A = 65%, B = 35%
top_row <- p3A + p3B +
  plot_layout(widths = c(0.65, 0.35))

# Bottom row: C = full width
fig3 <- (top_row / p3C) +
  plot_layout(heights = c(1, 1.4)) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(
        size = 15,
        face = "bold"
      )
    )
  )

print(fig3)


dir.create(
  "paper_figures",
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  filename = "paper_figures/Figure3_wide.pdf",
  plot = fig3,
  device = "pdf",
  width = 10,
  height = 8,
  units = "in",
  bg = "white",
  useDingbats = FALSE
)
