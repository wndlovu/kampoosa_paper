library(tidyverse)
library(readxl)
library(lubridate)
library(gtools)
library(zoo)
library(stringr)
options(scipen=999)

## GROUNDWATER CHEMISTRY
# read in monthly chemistry data files
ionDataSummary <- read_excel("data/ion_data.xlsx", 
                             sheet = "GW Summary by sample location", 
                             col_types = c("date", "text", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric"))




# use UMass montly chemistry data to see any changes in concentrations
ionDataStreams <- ionDataSummary %>% 
  filter(WellID %in% c("KB-100","KB-150", "KB-300", "MB-100")) %>% 
  mutate(Date = as.Date(Date))

# create a df showing the Cl in the fen area (use well data). AS, BS and CS are surface water - welldepth = 0
wells <- ionDataSummary %>% 
  #filter(Date <= 2020-04-31) %>% 
  filter(WellID %in% c("AA", "AB", "AC", "BA", "BB", "BC", "CA", "CB", "CC", "AS", "BS", "CS")) %>% # remove surface water samples
  mutate(IDgroup = case_when(WellID == "AA" |WellID == "AB"|WellID ==  "AC"|WellID ==  "AS"  ~ "A",
                             WellID == "BA" |WellID == "BB"|WellID ==  "BC"|WellID ==  "BS" ~ "B",
                             WellID == "CA" |WellID == "CB"|WellID ==  "CC"|WellID ==  "CS" ~ "C"),
         Date = as.Date(Date), # change from .dttm to date
         depth = paste0(WellDepth_ft, "ft")) %>% 
  filter(Date <= as.Date("2020-03-17"))
  


# df to the trends over the yrs for Na, Ca, Cl, Mg and K
wellsPivoted <- wells %>% 
  pivot_longer(cols = c("Na_mgL", "Ca_mgL", "Cl_mgL", "Mg_mgL", "SO42_mgL", "K_mgL"), 
               names_to = "type", values_to = "concentration") 


# save files as .csv
write_csv(wellsPivoted, "results/tables/wellsPivoted.csv")

