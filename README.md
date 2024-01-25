---
editor_options: 
  markdown: 
    wrap: 72
---

# Ecological memory of spring air temperature drives summer water quality dynamics across temperate lakes

## Summary

An analysis of how spring air temperatures influence summer bottom-water
temperature and dissolved oxygen across many widespread lakes.

## Data availability

-   In-lake data for this study published on the Environmental Data
    Initative data portal:
    <https://doi.org/10.6073/pasta/2cd6628a942de2a8b12d2b19962712a0>
-   Additional data are presented in `./External data`:
    -   `files_gotm/` includes modeled water temperature for n = 42
        lakes in our dataset that have been modeled through the
        Intersectoral Model Intercomparison Project lakes sector (ISIMIP
        global lakes v3a). For this analysis we used the General Ocean
        Turbulence Model (GOTM) hydrodynamic model output and the ERA5
        climate reanalysis.
    -   `lake_tas/` includes air temperature data from ERA5 that
        corresponds with ISIMIP outputs.

## Repo content information

### `./Data processing`

Data loading/processing scripts:

-   `01 - Load climate - ERA5.Rmd`
    -   Load netcdf of global climate data and output a csv with air
        temperature records at each lake
-   `02 - Temp and DO interpolation.Rmd`
    -   Interpolate temperature and oxygen profiles to a 1 m depth
        resolution
-   `03 - Stratified avgs.Rmd`
    -   Calculate epilimnetic and hypolimnetic means during the
        stratified period of each year. Add in additional chlorophyll-a
        data using "chla_harmonizer.csv"
-   `04 - Summer avgs.Rmd`
    -   Calculate epilimnetic and hypolimnetic means during the
        late-summer period of each year
-   `05 - Spring avgs.Rmd`
    -   Calculate surface and bottom means during the spring period of
        each year
-   `06 - hydrolakes.Rmd`
    -   Load all hydrolakes data and output a csv with hydrolakes
        metadata for each lake
-   `07 - VW DO Demand - based on strat dur.Rmd`
    -   Calculate volume-weighted hypolimnetic oxygen demand for each
        lake-year
-   `08 - Temp change - based on strat dur.Rmd`
    -   Calculate hypolimnetic warming rate for each lake-year
-   `09 - Compile data.Rmd`
    -   Combine late-summer and stratified means with oxygen demand, and
        climate data. Output a file for following analyses.
-   `10 - Anoxic factor.Rmd`
    -   Calculate anoxic factor for the entire hypolimnion
-   `11 - Anoxic factor layers.Rmd`
    -   Calculate anoxic factor in two hypolimnetic layers

### ./Data analysis

All data analysis scripts:

-   `01 - Data characterization.Rmd`
    -   Characterize the full, synthesized dataset. Output summary
        figures
-   `Air temp analysis.Rmd`
    -   Analyze the effects of changing monthly air temperatures on
        hypolimnetic temperature and oxygen dynamics
-   `DOC vs warming.Rmd`
    -   Analyze the associations between DOC and hypolimnetic warming
        rates

### ./External data

Downloaded data (unmodified from original sources)

### ./Compiled data

Compiled datasets, created by the scripts in `./Data analysis`

### ./Figures

Figures created by the scripts in `./Data analysis`

### ./Illustrator files

Adobe illustrator files used to create conceputal figure, graphical
abstract, and annotated figures for manuscript

## References

Filazzola, A., Mahdiyan, O., Shuvo, A., Ewins, C., Moslenko, L., Sadid,
T., Blagrave, K., Gray, D., Quinlan, R., O'Reilly, C., & Sharma, S.
(2020). A global database of chlorophyll and water chemistry in
freshwater lakes. <https://doi.org/10.5063/F1JH3JKZ>

Hersbach, H., Bell, B., Berrisford, P., Biavati, G., Horányi, A., Muñoz
Sabater, J., Nicolas, J., Peubey, C., Radu, R., Rozum, I., Schepers, D.,
Simmons, A., Soci, C., Dee, D., & Thépaut, J.-N. (2019). ERA5 monthly
averaged data on single levels from 1979 to present [Data set].
Copernicus Climate Change Service (C3S) Climate Data Store (CDS).
<https://doi.org/10.24381/CDS.F17050D7>

Messager, M. L., Lehner, B., Grill, G., Nedeva, I., & Schmitt, O.
(2016). Estimating the volume and age of water stored in global lakes
using a geo-statistical approach. Nature Communications, 7(1),
Article 1. <https://doi.org/10.1038/ncomms13603>
