# Raw CDC NSSP Data

This directory contains the **raw CDC NSSP data** used for influenza emergency department visit analysis.

## How to Obtain the Data

You can obtain the data in two ways:

1. **Manual Download**  
   Visit the [CDC NSSP Data Portal](https://data.cdc.gov/Public-Health-Surveillance/NSSP-Emergency-Department-Visit-Trajectories-by-St/rdmq-nq56/about_data) and download the dataset manually.

2. **Programmatic Access in R**  
   You can download the data directly using the **`epidatr`** package:

   ```r
   ## state level data 
   epidatr::pub_covidcast(
     source    = "nssp",
     signals   = "pct_ed_visits_influenza",
     geo_type  = "state",
     geo_values = "sc",
     time_type = "week"
   )
   
   ## HSA level data 
   epidatr::pub_covidcast(
     source    = "nssp",
     signals   = "pct_ed_visits_influenza",
     geo_type = "hsa_nci",
     geo_values = "sc",
     time_type = "week"
   )


# Health Service Areas (HSA)
   The HSA data were downloaded from [SEER](https://seer.cancer.gov/seerstat/variables/countyattribs/hsa.html). The dataset includes:
   - HSA ID (numeric code)
   - HSA Description
   - State–County mapping
   - FIPS codes

   The dataset has also been uploaded to this folder as an `Health.Service.Areas.xls` file and is available for download.
   