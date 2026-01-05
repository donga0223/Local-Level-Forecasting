# Code

This directory contains all code for the GBQR model, data cleaning, generating summary figures, and calculating all metrics.
- Data cleaning 
    - `us_hsa_county_popdesc.R`
        - Harmonizes geographies (state / HSA / county); using Connecticut’s 2021 county boundaries (pre-2022 change) to preserve legacy FIPS codes.
        - Join ACS 2023 total population and 2020 PL urban/rural splits.
        - Compute areas (st_area), densities, pop_ratio, and log_density_hsa.
        - Outputs
            - `data/us_hsa_county_popdesc.csv` — population/urban-density descriptors by state, HSA, and county.
            - `data/us_map_pop_sf.rds` — sf object that includes state, HSA, and county geometries. Not committed due to size; recreate locally with the R script.
- [Forecasts](../Forecasts/)
    - for state level fitting : run GBQR_US_NSSP_state.ipynb
    - for HSA level fitting : run GBQR_US_NSSP_HSA.ipynb
    - Supporting files for model fitting:
        - loader.py
        - preprocess_and_plot.py
        - forecast_model.py
- [Analysis](../Analysis/)
  - contains all code used to fit models to diff_WIS, as well as the corresponding result figures.
- summary figures
- calculate metrics 

