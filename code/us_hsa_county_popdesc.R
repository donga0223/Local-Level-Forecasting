library(tidycensus)
library(dplyr)
library(tidyr)
library(stringr)
 

##########################################################
## state, county, hsa population size
##########################################################

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

####### Note!############################
## CT county fips changed at 2022, so CT state, we are using the 2021 data.

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


state_hsa_county <- state_county_pop %>%
  left_join(hsa, by = "fips") 

hsa_pop <- state_hsa_county %>%
  group_by(state, hsa_nci_id) %>%
  summarise(population_hsa = sum(population_county))

state_hsa_county_pop <- state_hsa_county %>%
  left_join(hsa_pop, by = c("state", "hsa_nci_id"))

state_hsa_county_pop %>% filter(is.na(hsa_nci_id))


##########################################################
## Urbanization (get_acs not available)
##########################################################

urban_sf1 <- get_decennial(
  geography = "county",
  variables = c(
    total = "P1_001N",   # Total population
    urban = "P2_002N",   # Urban population
    rural = "P2_005N"    # Rural population
  ),
  year    = 2020,
  sumfile = "pl"
)


# calculate ratio
urban_pct <- urban_sf1 %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  dplyr::select(fips = GEOID, population_county_2020 = total, urban, rural) %>%
  left_join(hsa %>% dplyr::select(hsa_nci_id, fips), by = "fips") %>%
  group_by(hsa_nci_id) %>%
  summarise(urban_hsa = sum(urban), population_county_2020_hsa = sum(population_county_2020)) %>%
  mutate(pct_urban = urban_hsa/population_county_2020_hsa)

head(urban_pct)

state_hsa_county_pop_urban <- state_hsa_county_pop %>% 
  left_join(urban_pct, by = "hsa_nci_id") 


##########################################################
## Load sf data
##########################################################

######### Note!! #################################
### Since Connecticut’s FIPS changed in 2022, 
### we continue using the old county FIPS to stay consistent with the NSSP data.
##################################################################

us_states <- states(cb = TRUE, year = 2023, class = "sf")
us_counties <- counties(cb = TRUE, year = 2023, class = "sf")

us_counties_CT <- counties(cb = TRUE, year = 2021, class = "sf") %>%
  filter(STATE_NAME == "Connecticut")

us_counties <- us_counties %>%
  filter(STATE_NAME != "Connecticut") %>%
  bind_rows(us_counties_CT)


##########################################################
## State and HSA density
##########################################################


us_states_density <- us_states %>%
  mutate(area_km2_state = as.numeric(st_area(.) / 1e6))  %>% # st_area m²
  select(state = NAME, area_km2_state)

us_states_density <- st_drop_geometry(us_states_density)
class(us_states_density)   
head(us_states_density)

us_counties_density <- us_counties %>%
  mutate(area_km2_county = as.numeric(st_area(.) / 1e6))  %>% # st_area m²
  select(state = STATE_NAME, fips= GEOID, area_km2_county)
us_counties_density <- st_drop_geometry(us_counties_density)
class(us_counties_density)   
head(us_counties_density)


state_hsa_pop <- state_hsa_county_pop_urban %>% 
  select(state, hsa_nci_id, county, fips, population_state, population_hsa) %>% 
  distinct()

state_hsa_pop1 <- state_hsa_pop %>% 
  left_join(us_states_density, by = "state") %>%
  mutate(density_state = as.numeric(population_state) / as.numeric(area_km2_state)) %>%
  left_join(us_counties_density, by = c("state", "fips")) 



state_hsa_pop2 <- state_hsa_pop1 %>% group_by(state, hsa_nci_id) %>%
  distinct() %>%
  summarise(area_km2_hsa = sum(area_km2_county, na.rm = TRUE))

state_hsa_pop3 <- state_hsa_pop1 %>%
  left_join(state_hsa_pop2, by = c("state", "hsa_nci_id")) %>%
  mutate(density_hsa = as.numeric(population_hsa) / as.numeric(area_km2_hsa)) 


state_hsa_county_pop_urban_density <- state_hsa_county_pop_urban %>%
  left_join(state_hsa_pop3 %>% 
              select(state, hsa_nci_id, area_km2_state, area_km2_hsa, density_state, density_hsa) %>%
              distinct(),
            by = c("state", "hsa_nci_id"), relationship = "many-to-many")


state_hsa_county_pop_urban_density1 <- state_hsa_county_pop_urban_density %>%
  dplyr::select(-`State-county`, -hsa_counties) %>%
  filter(!is.na(hsa_nci_id)) %>%
  mutate(pop_ratio = population_hsa/population_state) %>%
  mutate(log_density_hsa = log(density_hsa))

write.csv(state_hsa_county_pop_urban_density1, "Local-Level-Forecasting/data/us_hsa_county_popdesc.csv", row.names = FALSE)


us_map_pop <- state_hsa_county_pop_urban_density1 %>%
  left_join(us_states %>%
              select(state = NAME, geometry), by = "state")

colSums(is.na(us_map_pop))

us_hsa <- us_counties %>%
  select(state = STATE_NAME, fips = GEOID, geometry) %>%
  inner_join(state_hsa_county_pop_urban_density1 %>% 
               select(state, fips, hsa_nci_id) %>% distinct(),
             by = c("state", "fips")) %>%
  group_by(state, hsa_nci_id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

 

us_map_pop1 <- us_map_pop %>%
  left_join(us_hsa, by = c("state", "hsa_nci_id"),
            suffix = c("", "_hsa"))

us_map_pop2 <- us_map_pop1 %>%
  left_join(us_counties %>%
              select(state = STATE_NAME, fips = GEOID, county = NAME, geometry), 
            by = c("state", "fips", "county"),
            suffix = c("", "_county"))







