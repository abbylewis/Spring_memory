# Ecological memory of spring air temperature drives summer water quality dynamics across temperate lakes

## Summary

An analysis of how spring air temperatures influence summer bottom-water temperature and dissolved oxygen across many widespread lakes.

## Personnel

-   Abigail Lewis, Virginia Tech, [aslewis\@vt.edu](mailto:aslewis@vt.edu)
-   Cayelan Carey, Virginia Tech, [cayelan\@vt.edu](mailto:cayelan@vt.edu)

## Data availability

-   In-lake data for this study published on the Environmental Data Initative data portal: <https://doi.org/10.6073/pasta/2cd6628a942de2a8b12d2b19962712a0>
-   Additional data are presented in `./External data`:
    -   `files_gotm/` includes modeled water temperature for n = 43 lakes in our dataset that have been modeled through the Intersectoral Model Intercomparison Project lakes sector (ISIMIP global lakes v3a). For this analysis we used the General Ocean Turbulence Model (GOTM) hydrodynamic model output and the ERA5 climate reanalysis.
    -   `lake_tas/` includes air temperature data from ERA5 that corresponds with ISIMIP outputs.

## Repo content information

### `./Data processing`

Data loading/processing scripts:

-   `01 - Load climate - ERA5 open meteo`: Load global climate data and output a csv with meteorological records at each lake
-   `02 - Temp and DO interpolation.Rmd`: Interpolate temperature and oxygen profiles to a 1 m depth resolution
-   `04 - Summer avgs.Rmd`: Calculate surface and bottom water means during the late-summer period of each year
-   `03 - Stratified avgs.Rmd`: Calculate epilimnetic and hypolimnetic means during the entire stratified period of each year
-   `05 - VW DO Demand - based on strat dur.Rmd`: Calculate volume-weighted hypolimnetic oxygen demand for each lake-year
-   `06 - Compile data.Rmd`: Combine summer means with oxygen demand. Output a file for following analyses
-   `07 - ISIMIP_v3.Rmd`: Compile ISIMIP modeled water temperature

### `./Data analysis`

All data analysis scripts:

-   `01 - Calculate rolling mean correlations surf bot.Rmd`: Calculate the correlation between focal variables and rolling mean air temperature across all lakes
-   `02 - Data characterization.Rmd`: Characterize the full, synthesized dataset. Output map of all sites
-   `03 - Example plots - Taylor Pond.Rmd`: Create plots to describe the process of calculating seasonal ecological memory using Taylor Pond as an example lake
-   `04 - Calculate memory and plot rolling means.Rmd`: Calculate and plot seasonal ecological memory. Plot rolling correlations for hypolimnetic variables
-   `05 - Memory driver analysis - RF.Rmd`: Analyze the drivers of seasonal ecological memory. Output figure for paper
-   `06 - Calculate trends all met limit years.Rmd`: Calculate trends in air temperature and water quality
-   `07 - Plot trends all met.Rmd`: Plot association between air temperature trends and water temperature trends
-   `08 - ISIMIP_correlations`: Calculate seasonal ecological memory using ISIMIP-modeled water temperature
-   `correlations_doy.R`: Helper function for calculating correlations by day of year
-   `sen_slope_custom.R`: Helper functions to calculate trends over time
-   `run_rf_analysis.R`: Helper functions to run and plot random forest regressions

### `./External data`

Downloaded data (unmodified from original sources)

### `./Compiled data`

Compiled datasets, created by the scripts in `./Data processing` and `./Data analysis`

### `./Illustrator files`

Adobe illustrator files used to create conceptual figure and annotated figures for manuscript
