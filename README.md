
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
install.packages(c("tidyverse", "readxl", "lubridate", 
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

1)  Run [**Discharge with area and
    precip+evap.R**](https://github.com/wndlovu/kampoosa_paper/blob/main/analysis/Discharge%20with%20area%20and%20precip%2Bevap.R)
    to upload raw data and calculate daily, monthly and annual summaries
    for weather and stream gauges.

2)  Run
    [**groundwater_wells.R**](https://github.com/wndlovu/kampoosa_paper/blob/main/analysis/groundwater_wells.R)
    to extract grab sample chloride concentrations at wells A, B and C.

3)  Run
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
