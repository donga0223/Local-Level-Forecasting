# Code

This directory contains all code for the GBQR model, data cleaning, generating summary figures, and calculating all metrics.
- Data cleaning 
    - `us_hsa_county_popdesc.R`
        - Harmonizes geographies (state / HSA / county); using Connecticut’s 2021 county boundaries (pre-2022 change) to preserve legacy FIPS codes.
        - Join ACS 2023 total population and 2020 PL urban/rural splits.
        - Compute areas (st_area), densities, pop_ratio, and log_density_hsa.
        - Outputs
            - `data/us_hsa_county_popdesc.csv` — population/urban-density descriptors by state, HSA, and county.
            - `data/us_map_pop_sf.rds` — sf object that includes state, HSA, and county geometries. 
- GBQR 
    - for state level fitting : run GBM_US_NSSP_public_state_pct.ipynb
    - for HSA level fitting : run GBM_US_NSSP_public_multiCouny_pct_retro_500K.ipynb
    - Supporting files for model fitting:
        - loader.py
        - preprocess_and_plot.py
        - forecast_model.py
- summary figures
- calculate metrics 

