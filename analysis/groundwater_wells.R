library(tidyverse)
library(readxl)
library(lubridate)
library(gtools)
library(zoo)
library(stringr)
library(Kendall)
library(trend)
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


# reproduce model fit equation for streams
stream_wells <- ionDataSummary %>% 
  filter(WellID %in% c("KB-100", "KB-150", "KB-300", "MB100")) %>% #
  mutate(Date = as.Date(Date)) %>% 
  filter(Date > as.Date("2017-05-17") & Date < as.Date("2019-04-18")) %>% 
  mutate(SC_sqrd = CalcConductance_mS^2,
         SC_sqrd2 = SC_mS^2)


sc_cl_model<- lm(Cl_mgL ~ CalcConductance_mS + SC_sqrd, data=stream_wells)

m1 <- lm(Cl_mgL ~ poly(CalcConductance_mS, 2), data = stream_wells)
m1 <- lm(Cl_mgL ~ CalcConductance_mS + I(CalcConductance_mS^2), data = stream_wells)


summary(m1)

#view model summary
summary(sc_cl_model)


# create a df showing the Cl in the fen area (use well data). AS, BS and CS are surface water - welldepth = 0
wells <- ionDataSummary %>% 
  filter(WellID %in% c("AA", "AB", "AC", "BA", "BB", "BC", "CA", "CB", "CC", "AS", "BS", "CS", "KB-100", "KB-150", "KB-300", "MB-100")) %>% # remove surface water samples
  mutate(IDgroup = case_when(WellID == "AA" |WellID == "AB"|WellID ==  "AC"|WellID ==  "AS"  ~ "A",
                             WellID == "BA" |WellID == "BB"|WellID ==  "BC"|WellID ==  "BS" ~ "B",
                             WellID == "CA" |WellID == "CB"|WellID ==  "CC"|WellID ==  "CS" ~ "C",
                             WellID ==  "KB-100"  ~ "KB-100",
                             WellID ==  "KB-150"  ~ "KB-150",
                             WellID ==  "KB-300"  ~ "KB-300",
                             WellID ==  "MB-100"  ~ "MB-100"),
         Date = as.Date(Date), # change from .dttm to date
         depth = paste0(WellDepth_ft, "ft")) %>% 
  filter(Date <= as.Date("2020-03-17") & Date >= as.Date("2018-01-24")) %>% 
  group_by(WellID, Date) %>%
  summarise(Cl_mgL = mean(Cl_mgL, na.rm = TRUE),
            IDgroup = IDgroup,
            depth = depth,
            WellDepth_ft) %>% 
  drop_na(Cl_mgL)

# calculate the % increase in cl concentrations at the 15ft wells
well_a <- wells %>% 
  filter(WellID == "AC")

well_b <- wells %>% 
  filter(WellID == "BC")

well_c <- wells %>% 
  filter(WellID == "CC")

initial_a <- well_a$Cl_mgL[1]
final_a <- tail(well_a$Cl_mgL, 1)

initial_b <- well_b$Cl_mgL[1]
final_b <- tail(well_b$Cl_mgL, 1)

initial_c <- well_c$Cl_mgL[1]
final_c <- tail(well_c$Cl_mgL, 1)

# Calculate percent increase
per_change_a <- ((final_a - initial_a) / initial_a) * 100
per_change_b <- ((final_b - initial_b) / initial_b) * 100
per_change_c <- ((final_c - initial_c) / initial_c) * 100


# calculate average cl concentration at the 5, 10 and 15ft depths

mean_conc <- wells %>% 
  filter(WellDepth_ft %in% c(5,10,15)) %>% 
  group_by(IDgroup) %>% 
  summarise(ave_conc = mean(Cl_mgL))



#calculate percent difference at the beginning and end of sampling period for 15ft wells
a_diff <- wells %>% 
  filter(WellID == "AC")  

a_diff <- ((a_diff$Cl_mgL[length(a_diff$Cl_mgL)] - a_diff$Cl_mgL[1])/a_diff$Cl_mgL[1])*100
  
b_diff = wells %>% 
  filter(WellID == "BC")  

b_diff <- ((b_diff$Cl_mgL[length(b_diff$Cl_mgL)] - b_diff$Cl_mgL[1])/b_diff$Cl_mgL[1])*100

c_diff = wells %>% 
  filter(WellID == "CC")  

c_diff <- ((c_diff$Cl_mgL[length(c_diff$Cl_mgL)] - c_diff$Cl_mgL[1])/c_diff$Cl_mgL[1])*100



# check trends
# site A


SeasonalMannKendall(ts(wells$Cl_mgL[wells$WellID == 'AA']))  # 5ft

SeasonalMannKendall(ts(wells$Cl_mgL[wells$WellID == 'AC'])) # 15ft

# site B
SeasonalMannKendall(ts(wells$Cl_mgL[wells$WellID == 'BA']))  # 5ft

SeasonalMannKendall(ts(wells$Cl_mgL[wells$WellID == 'BC'])) # 15ft


# site C
SeasonalMannKendall(ts(wells$Cl_mgL[wells$WellID == 'CA']))  # 5ft

SeasonalMannKendall(ts(wells$Cl_mgL[wells$WellID == 'CC'])) # 15ft


# save files as .csv
write_csv(wells, "results/tables/wellsPivoted.csv")

