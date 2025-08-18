# Code

This directory contains all code for the GBQR model, data cleaning, generating summary figures, and calculating all metrics.
- Data cleaning 
    - `us_hsa_county_popdesc.csv`
        - Harmonize geographies (state / HSA / county); use CT 2022 counties to maintain old FIPS.
        - Join ACS 2023 total population and 2020 PL urban/rural splits.
        - Compute areas (st_area), densities, pop_ratio, and log_density_hsa.
        - Write a short run script, `code/us_hsa_county_popdesc.R`
- GBQR 
    - for state level fitting : run GBM_US_NSSP_public_state_pct.ipynb
    - for HSA level fitting : run GBM_US_NSSP_public_multiCouny_pct_retro_500K.ipynb
    - Supporting files for model fitting:
        - loader.py
        - preprocess_and_plot.py
        - forecast_model.py
- summary figures
- calculate metrics 

