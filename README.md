
## Kampoosa Bog Data

<img src="kampoosa_map/location.png" width="700px" style="display: block; margin: auto;" />

Kampoosa Bog is a 70ha wetland complex located in Stockbridge-Lee MA
that is currently being impacted by road salt pollution. In a effort to
understand the impacts of road salt on the water quality and vegetation
in the wetland, the Mass DOT and Department of Fish and Wildlife have
collaborated with engineers (Erich Hinlein and Camelia Rotaru) at the
University of Massachusetts to monitor the water quality at gauge
stations (KB100, KB150, KB300) within the Kampoosa watershed. At these
gauge stations water flow, water temperature and specific conductance
are recorded at a 15 minute interval as of Nov 2017.

## Procedure

### Packages and Dependancies

``` r
install.packages(c("tidyverse", "readxl", "lubridate", "anytime", 
"gtools", "zoo", "stringr", "forecast", "fable", "kernlab", "ggthemr",
"ggthemes", "ggpubr", "rrandomcoloR",  "gridExtra", "ggpattern",
"glue", "ggtext", "tibble", "cowplot",  "readr", "grid", "directlabels",
"cowplot", "ggpmisc", "ggrepel", "scales", "sf", "usmap", "maps", "maptools",
"rgdal", "latex2exp"))
```

### Analysis

Steps for each analysis are explained as commented code in all the
documents.

To reproduce analysis, follow these these guidelines:

1)  Run
    [**stream_weather**](https://github.com/wndlovu/kampoosa_paper/blob/main/analysis/stream_weather.R)
    to upload raw data and calculate daily, monthly and annual summaries
    for weather and stream gauges.

2)  Run
    [**groundwater_wells.R**](https://github.com/wndlovu/kampoosa_paper/blob/main/analysis/groundwater_wells.R)
    to extract grab sample chloride concentrations at wells A, B and C.

3)  Run
    [**salt_application_full.R**](https://github.com/wndlovu/kampoosa_paper/blob/main/analysis/salt_application_full.R)
    to calculate chloride mass balance for 2012 - 2020.

4)  Run
    [**long_term_chloride_steady_state.R**](https://github.com/wndlovu/kampoosa_paper/blob/main/analysis/long_term_chloride_steady_state.R)
    to calculate changes in chloride steady state concentration (2012 - 2019)
    
5)  Run
    [**visuals.R**](https://github.com/wndlovu/kampoosa_paper/blob/main/analysis/visuals.R)
    to reproduce all visuals in manuscript.

Results from running **visuals.R** are stored in the **results** folder
as either
[**tables**](https://github.com/wndlovu/kampoosa_paper/tree/main/results/tables)
or
[**visuals**](https://github.com/wndlovu/kampoosa_paper/tree/main/results/visuals)

### Additional Analysis

Supplementary data analysis can be found in the
[**additional_analysis**](https://github.com/wndlovu/kampoosa_paper/tree/main/results/tables)
folder.
