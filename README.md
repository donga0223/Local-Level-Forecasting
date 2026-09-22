# Local-Level-Forecasting

This repository accompanies the paper Kim, D., et al. (2026+), Local Influenza Forecasts Outperform State-Level Forecasts in the United States.
The paper compares the performance of local-level influenza forecasting models with state-level forecasting models across the United States.
This repository contains all code used to fit the forecasting models, conduct analyses, and generate the figures presented in the paper.
All analyses rely exclusively on publicly available data.

## Repository organization

This repository has the following directories:

- `code/`: all code for GBQR forecasting and its component models, exploratory data analyses, and so on. See the readme in that folder for further information.
- `data/`: raw data pulled from CDC NSSP. See the readme in that folder for further information.


## Data Availability

The original influenza emergency department visit data used in this study are publicly available from the CDC National Syndromic Surveillance Program (NSSP) through the [CDC NSSP Emergency Department Visits dataset](https://data.cdc.gov/Public-Health-Surveillance/NSSP-Emergency-Department-Visits-COVID-19-Flu-RSV-/7xva-uux8/about_data).

Health Service Area (HSA) definitions and county-to-HSA assignments were obtained from the [National Cancer Institute's Surveillance, Epidemiology, and End Results (SEER) Program](https://seer.cancer.gov/seerstat/variables/countyattribs/hsa.html).

Population data were obtained from the [U.S. Census Bureau Decennial Census of Population and Housing](https://www.census.gov/programs-surveys/decennial-census/data.html). Metropolitan Statistical Area (MSA) delineations were obtained from the [U.S. Census Bureau Metropolitan and Micropolitan Statistical Area Delineation Files](https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html), using the July 2023 delineations.

The derived, analysis-ready datasets used in this study are available on [Zenodo](https://doi.org/10.5281/zenodo.22903162) (DOI: **10.5281/zenodo.22903162**). These datasets include:

- `forecasting_metrics_3seasons.csv`: forecast evaluation data for the 2022/23, 2023/24, and 2024/25 influenza seasons, including observed influenza emergency department visit percentages, forecast estimates, forecast horizons, coverage, errors, and weighted interval scores (WIS) for HSA- and state-level forecasts.
- `hsa_urban_analysis_ready.csv`: HSA-level analysis-ready data used to examine associations between forecast performance differences and population and urbanization characteristics.