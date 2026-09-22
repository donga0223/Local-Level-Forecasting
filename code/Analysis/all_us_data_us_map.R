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



state_pop <- get_acs(
  geography = "state",
  variables = "B01003_001",  # total population
  year = 2023,
  survey = "acs5",
  output = "wide"
) %>%
  select(state = NAME, population = B01003_001E)

county_pop <- get_acs(
  geography = "county",
  variables = "B01003_001",  # total population
  year = 2023,
  survey = "acs5",
  output = "wide"
) %>%
  separate(NAME, into = c("county", "state"), sep = ", ") %>%
  mutate(
    county = str_remove(county, " County$")  # remove " County" in county variable
  ) %>%
  select(fips = GEOID, county, state, population = B01003_001E)

######### Note!! #################################
### Since Connecticut’s FIPS changed in 2022, 
### we continue using the old county FIPS to stay consistent with the NSSP data.
##################################################################

county_pop_CT <- get_acs(
  geography = "county",
  variables = "B01003_001",  # total population
  year = 2021,
  survey = "acs5",
  state = "CT",
  output = "wide"
) %>%
  separate(NAME, into = c("county", "state"), sep = ", ") %>%
  mutate(
    county = str_remove(county, " County$")  # remove " County" in county variable
  ) %>%
  select(fips = GEOID, county, state, population = B01003_001E)



county_pop <- county_pop %>% 
  filter(state != "Connecticut") %>%
  bind_rows(county_pop_CT)

state_county_pop <- county_pop %>%
  full_join(state_pop, by = "state", suffix = c("_county", "_state"))

state_abb <- read.csv("Local-Level-Forecasting/data/state-abbrevs.csv")
state_county_pop2 <- state_county_pop %>%
  left_join(state_abb, by = "state")

#hsa <- read.csv("Local-Level-Forecasting/data/Health.Service.Areas.xls")
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

head(hsa)

hsa1 <- hsa %>%
  dplyr::select(-"HSA Description") %>%
  left_join(state_county_pop2, by = c("state_abbr" = "abbreviation", "fips")) %>%
  group_by(hsa_nci_id, state_abbr, state, population_state) %>%
  summarise(population_hsa = sum(population_county, na.rm = TRUE)) %>%
  filter(!is.na(state)) %>%
  mutate(pop_ratio = population_hsa/population_state) 
  
####################################################################################
library(sf)
library(dplyr)
library(tigris)
library(tidycensus)

# 1) County polygons
county_sf <- counties(cb = TRUE, year = 2022)

hsa_sf <- county_sf %>%
  left_join(hsa, by = c("STUSPS" = "state_abbr", "GEOID" = "fips")) %>%
  filter(!is.na(hsa_nci_id)) %>%
  group_by(STATE_NAME, STUSPS, STATEFP, hsa_nci_id) %>%
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

out_dir <- "/Users/dk29776/Dropbox/UTAustin/Forecasting/Local-Level-Forecasting/data/hsa_pop_by_state"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

library(readr)
files <- list.files(out_dir, pattern = "^hsa_pop_.*\\.csv$", full.names = TRUE)

hsa_pop_urban <- bind_rows(lapply(files, read_csv, show_col_types = FALSE)) %>%
  group_by(hsa_nci_id) %>%
  summarise(
    total_pop = sum(total_pop, na.rm = TRUE),
    urban_pop = sum(urban_pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    #urban_pop = ifelse(urban_pop == 0, 1, urban_pop),
    urban_pop_share = ifelse(total_pop > 0, urban_pop / total_pop, NA_real_)
  )

hsa2 <- hsa1 %>%
  left_join(hsa_pop_urban, by = "hsa_nci_id") %>%
  filter(hsa_nci_id != 1022)



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

hsa3 <- hsa2 %>%
  left_join(msa_count, by = c("state" = "NAME"))

hsa4 <- hsa3 %>%
  
  mutate(log_pop_ratio = log(pop_ratio),
         log_urban_pop_share = log(urban_pop_share)) %>%
  left_join(hsa_sf, by = c("state" = "STATE_NAME", "state_abbr" = "STUSPS", 
                           "hsa_nci_id", "STATEFP"))

saveRDS(hsa4, "data/all_us_3var_geo.rds")
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

st_sf_us <- st_sf %>%
  sf::st_as_sf() %>%
  sf::st_transform(4326) %>%
  dplyr::filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))


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
  
  # 마지막에 강조
  geom_sf(
    data = hsa_highlight,
    fill = NA,
    color = "#D55E00",
    linewidth = 1,
    linejoin = "round"
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

p4

ggsave("paper_figures/GLM_us_map_250K.png",
       p4, width = 10, height = 5, dpi = 300)



