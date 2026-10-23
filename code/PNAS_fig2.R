library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(grid)
library(forcats)
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
obs <- hsa_state_inc1 %>%
  mutate(location = paste(state, hsa_nci_id, sep = "_"),
         target_end_date = as.Date(week_end),
         hsa_nci_id = as.character(hsa_nci_id)) %>%
  select(target_end_date, location, state, hsa_nci_id, inc_hsa, inc_state)



all_df <- function(ref_date, i){
  out <- read.csv(paste0("/Users/dk29776/Dropbox/UTAustin/Forecasting/Local-Level-Forecasting/model_output/US_NSSP_public_Joint_250K_pct/", ref_date, "-GBQR.csv"))
  out1 <- out %>%
    separate(col = location,
             into = c("state", "hsa_nci_id"),
             sep = "_") %>%
    mutate(target_end_date = as.Date(target_end_date))
  
  out_hsa <- out1 %>%
    filter(hsa_nci_id != "All") 
  
  out_hsa_wide <- out_hsa %>%
    select(-reference_date, -target, -output_type) %>%
    filter(output_type_id %in% c(0.025, 0.5, 0.975)) %>%
    pivot_wider(
      names_from  = output_type_id,
      values_from = value,
      names_prefix = "output_type_id_"
    ) 
  
  
  out_state <- out1 %>%
    filter(hsa_nci_id == "All") 
  
  out_state_wide <- out_state %>%
    select(-reference_date, -target, -output_type, -hsa_nci_id) %>%
    filter(output_type_id %in% c(0.025, 0.5, 0.975)) %>%
    pivot_wider(
      names_from  = output_type_id,
      values_from = value,
      names_prefix = "output_type_id_"
    ) 
  
  out_wide <- out_hsa_wide %>%
    left_join(out_state_wide, 
              by = c("state", "target_end_date", "horizon"),
              suffix = c(".hsa", ".state")) %>%
    mutate(no = i)
  return(out_wide)
}

ref_dates <- c("2023-10-14", "2023-11-25", "2024-01-06", "2024-02-17", "2024-03-30")

for(i in 1:length(ref_dates)){
  assign(paste0("out", i), all_df(ref_dates[i], i))
}
outs_all <- bind_rows(out1, out2, out3, out4)

all <- obs %>%
  left_join(outs_all, by = c("state", "hsa_nci_id", "target_end_date"))


all <- all %>%
  mutate(no = as.factor(no))

fig_all <- all %>%
  filter(
    hsa_nci_id %in% c(405, 538, 434, 170, 826, 865),
    target_end_date <= as.Date(max(ref_dates)) + 30,
    target_end_date >= as.Date(min(ref_dates)) - 7,
    horizon != 4
  ) %>%
  mutate(
    facet_title = case_when(
      #hsa_nci_id == 289 ~ "Duluth, MN",
      hsa_nci_id == 826 ~ "Cherokee, GA",
      hsa_nci_id == 865 ~ "Rockland, NY",
      hsa_nci_id == 170 ~ "Durham, NC",
      hsa_nci_id == 405 ~ "Amarillo, TX",
      hsa_nci_id == 434 ~ "Fort Worth, TX",
      hsa_nci_id == 538 ~ "Laredo, TX",   
      TRUE ~ NA_character_
    ),
    facet_title = fct_relevel(
      facet_title,
      "Cherokee, GA", "Rockland, NY", "Durham, NC", #"Duluth, MN", 
      "Amarillo, TX", "Laredo, TX", "Fort Worth, TX"
    )
  ) 



fig_obs <- all %>%
  filter(
    hsa_nci_id %in% c(405, 538, 434, 170, 826, 865),
    target_end_date <= as.Date(max(ref_dates)) + 30,
    target_end_date >= as.Date(min(ref_dates)) - 7
  ) %>%
  mutate(
    facet_title = case_when(
      #hsa_nci_id == 289 ~ "Duluth, MN",
      hsa_nci_id == 826 ~ "Cherokee, GA",
      hsa_nci_id == 865 ~ "Rockland, NY",
      hsa_nci_id == 170 ~ "Durham, NC",
      hsa_nci_id == 405 ~ "Amarillo, TX",
      hsa_nci_id == 434 ~ "Fort Worth, TX",
      hsa_nci_id == 538 ~ "Laredo, TX",   
      TRUE ~ NA_character_
    ),
    facet_title = fct_relevel(
      facet_title,
      "Cherokee, GA", "Rockland, NY", "Durham, NC", #"Duluth, MN", 
      "Amarillo, TX", "Laredo, TX", "Fort Worth, TX"
    )
  ) 


facet_map <- c(
  "826" = "Cherokee, GA",
  "865" = "Rockland, NY",
  "170" = "Durham, NC",
  "405" = "Amarillo, TX",
  "538" = "Laredo, TX",
  "434" = "Fort Worth, TX"
)

facet_order <- c(
  "Cherokee, GA", "Rockland, NY", "Durham, NC",
  "Amarillo, TX", "Laredo, TX", "Fort Worth, TX"
)

ref_dates <- c(
  "2023-10-14",
  "2023-11-25",
  "2024-01-06",
  "2024-02-17",
  "2024-03-30"
)

theme_fig2 <- theme_minimal(base_size = 12) +
  theme(
    text = element_text(colour = "black"),
    axis.text = element_text(size = 10, colour = "black"),
    axis.title = element_text(size = 12, colour = "black"),
    strip.text = element_text(size = 11, colour = "black"),
    legend.text = element_text(size = 10, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.spacing.x = unit(0.15, "cm"),
    axis.title.y = element_text(margin = margin(r = 6)),
    legend.position = "bottom",
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.3, "cm"),
    plot.margin = margin(5, 5, 5, 5)
  )


x_scale_fig2 <- scale_x_date(
  breaks = as.Date(c(
    "2023-12-01",
    "2024-02-01",
    "2024-04-01"
  )),
  date_labels = "%b",
  limits = as.Date(c(
    "2023-10-01",
    "2024-04-30"
  ))
)
# ---------- Figure 2A ----------
# Selected forecast origins; keep original forecast trajectories

a_df <- fig_all %>%
  mutate(
    facet_title = factor(
      as.character(facet_title),
      levels = facet_order
    )
  )

a_obs <- fig_obs %>%
  distinct(facet_title, target_end_date, inc_hsa) %>%
  mutate(
    facet_title = factor(
      as.character(facet_title),
      levels = facet_order
    )
  )

p2A <- ggplot(a_df, aes(x = target_end_date)) +
  geom_ribbon(
    aes(
      ymin = output_type_id_0.025.state,
      ymax = output_type_id_0.975.state,
      group = no,
      fill = "State forecast"
    ),
    alpha = 0.25, na.rm = TRUE
  ) +
  geom_ribbon(
    aes(
      ymin = output_type_id_0.025.hsa,
      ymax = output_type_id_0.975.hsa,
      group = no,
      fill = "Local forecast"
    ),
    alpha = 0.25, na.rm = TRUE
  ) +
  geom_line(
    aes(
      y = output_type_id_0.5.state,
      group = no,
      colour = "State forecast"
    ),
    linewidth = 0.6, na.rm = TRUE
  ) +
  geom_point(
    aes(
      y = output_type_id_0.5.state,
      colour = "State forecast"
    ),
    size = 0.3,
    na.rm = TRUE
  ) +
  
  geom_line(
    aes(
      y = output_type_id_0.5.hsa,
      group = no,
      colour = "Local forecast"
    ),
    linewidth = 0.6, na.rm = TRUE
  ) +
  geom_point(
    aes(
      y = output_type_id_0.5.hsa,
      colour = "Local forecast"
    ),
    size = 0.3,
    na.rm = TRUE
  ) +
  geom_point(
    data = a_obs,
    aes(
      y = inc_hsa,
      shape = "Observed HSA"
    ),
    colour = "red",
    size = 0.9,
    inherit.aes = TRUE
  ) +
  scale_shape_manual(
    name = NULL,
    values = c("Observed HSA" = 16)
  ) +
  scale_color_manual(
    values = c(
      "Local forecast" = "orange",
      "State forecast" = "gray30"
    ),
    name = NULL
  ) +
  scale_fill_manual(
    values = c(
      "Local forecast" = "orange",
      "State forecast" = "gray30"
    ),
    name = NULL
  ) +
  facet_wrap(~facet_title, nrow = 1) +
  x_scale_fig2 +
  labs(x = NULL, y = "Percent influenza ED visits") +
  theme_fig2
p2A 
# ---------- Figure 2B ----------
# Horizon 3 only

source("code/Forecast_summary/summary_plot_ftn.R")
source("code/Forecast_summary/summary_plot_forpaper_ftn.R")
all_forecasting_metric <- read.csv("data_joint/forecasting_metrics_3seasons.csv")
forecasting_metrics_overall <- read.csv("data_joint/forecasting_metrics_overall.csv")
## This is for paper
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



hsa_state_inc_selected <- hsa_state_inc %>%
  filter(hsa_nci_id %in% c(405, 538, 434, 170, 826, 865)) #170, 289))
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

mymodel = "US_NSSP_public_Joint_250K_pct"

US_state_NSSP_250K_2324_selected <- cal_metrics(obs_data = hsa_state_inc_selected, mystate = NULL, 
                                                my_abb_state = mymodel,
                                                state_level = NULL, 
                                                date_list = seq.Date(from = as.Date("2023-10-07"), 
                                                                     to = as.Date("2024-03-30"), 
                                                                     by = "week"), 
                                                pop_size = 250000,
                                                pdfname = paste(mymodel, "2324_selected", sep = "_"))



b_df <- US_state_NSSP_250K_2324_selected %>%
  filter(horizon == 3) %>%
  mutate(
    target_end_date = as.Date(target_end_date),
    facet_title = factor(
      facet_map[as.character(hsa_nci_id)],
      levels = facet_order
    )
  ) %>%
  filter(!is.na(facet_title)) %>%
  select(
    reference_date, target_end_date, facet_title,
    inc, est_median, est_low, est_high,
    est_median.state, est_low.state, est_high.state
  ) %>%
  pivot_longer(
    cols = c(
      est_median, est_low, est_high,
      est_median.state, est_low.state, est_high.state
    ),
    names_to = c(".value", "level"),
    names_pattern = "^(est_(?:median|low|high))(?:\\.(state))?$"
  ) %>%
  mutate(
    level = if_else(
      is.na(level) | level == "",
      "Local forecast", "State forecast"
    )
  )

b_obs <- b_df %>%
  distinct(facet_title, target_end_date, inc)

p2B <- ggplot(
  b_df,
  aes(x = target_end_date, group = level)
) +
  geom_ribbon(
    aes(ymin = est_low, ymax = est_high, fill = level),
    alpha = 0.25,
    na.rm = TRUE
  ) +
  geom_line(
    aes(y = est_median, colour = level),
    linewidth = 0.6,
    na.rm = TRUE
  ) +
  geom_point(
    data = b_obs,
    aes(
      x = target_end_date,
      y = inc,
      shape = "Observed HSA"
    ),
    inherit.aes = FALSE,
    colour = "red",
    size = 0.9
  ) +
  scale_shape_manual(
    name = NULL,
    values = c("Observed HSA" = 16)
  ) + 
  scale_color_manual(
    values = c(
      "Local forecast" = "orange",
      "State forecast" = "gray30"
    ),
    name = NULL
  ) +
  scale_fill_manual(
    values = c(
      "Local forecast" = "orange",
      "State forecast" = "gray30"
    ),
    name = NULL
  ) +
  facet_grid(
    rows = vars(level),
    cols = vars(facet_title),
    scales = "free_y"
  ) +
  x_scale_fig2 +
  labs(x = NULL, y = "Percent influenza ED visits") +
  theme_fig2

p2B
# ---------- Combine and save ----------

fig2 <- p2A / p2B +
  plot_layout(heights = c(1, 2)) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(size = 15, face = "bold")
    )
  )

print(fig2)

ggsave(
  "paper_figures/Figure2_wide.pdf",
  plot = fig2,
  device = "pdf",
  width = 10,
  height = 7,
  units = "in",
  bg = "white",
  useDingbats = FALSE
)
