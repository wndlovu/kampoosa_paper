library(tidyverse)
library(readxl)
library(lubridate)
library(gtools)
library(zoo)
library(stringr)
library(forecast)
library(fable)
library(kernlab)
library(anytime)
library(dplyr)
library(Kendall)
library(trend)
#library(e1071)
options(scipen=999)


# read in the probe data files
KB100_2017_2020 <- read_excel("data/KB100_2017_2020.xlsx") %>% 
  distinct(Standard_Date_Time, .keep_all= TRUE)

KB150_2017_2020 <- read_excel("data/KB150_2017_2020.xlsx") %>% 
  distinct(Standard_Date_Time, .keep_all= TRUE)

KB300_2017_2020 <- read_excel("data/KB300_2017_2020.xlsx") %>% 
  distinct(Date_Time, .keep_all= TRUE)

MB100_2017_2020 <- read_excel("data/MB100_2017_2020.xlsx") %>% 
  distinct(Standard_Date_Time, .keep_all= TRUE) %>% 
  mutate(Standard_Date_Time = as_datetime(Standard_Date_Time))

# read airTemp data 
dailyAirTemp <- read_csv("data/airTemp.csv") %>% 
  mutate(date = mdy(date)) %>% 
  distinct(date, .keep_all= TRUE) %>% 
  dplyr::rename(airtempr = DMtemp) %>% 
  select(date, airtempr)


# read in rainfall data
rainfall2017 <- read_excel("data/KB100_2017_rainfall.xlsx", 
                           col_types = c("date", "date", "numeric", 
                                         "numeric", "numeric"))  
rainfall2018 <- read_excel("data/KB100_2018_Rainfall.xlsx", 
                           col_types = c("date", "date", "numeric", 
                                         "numeric", "numeric"))
rainfall2019 <- read_excel("data/KB100_2019_Rainfall.xlsx", 
                           col_types = c("date", "date", "numeric", 
                                         "numeric", "numeric"))
rainfall2020 <- read_excel("data/KB100_2020_Rainfall.xlsx", 
                           col_types = c("date", "date", "numeric", 
                                         "numeric", "numeric"))


# read in water temperature for one site and calculate the average. Use average in analysis
kb100Temperature <- read_csv("data/kb100Temperature.csv") %>% 
  select(dateTime, tempC) %>% 
  dplyr::rename(tempC_kb100 = tempC) %>% 
  distinct(dateTime, .keep_all= TRUE)

kb150Temperature <- read_csv("data/kb150Temperature.csv") %>% 
  select(dateTime, tempC)%>% 
  dplyr::rename(tempC_kb150 = tempC) %>% 
  distinct(dateTime, .keep_all= TRUE)

kb300Temperature <- read_csv("data/kb300Temperature.csv") %>% 
  select(dateTime, tempC)%>% 
  dplyr::rename(tempC_kb300 = tempC) %>% 
  distinct(dateTime, .keep_all= TRUE)

mb100Temperature <- read_csv("data/mb100Temperature.csv") %>% 
  select(dateTime, tempC)%>% 
  dplyr::rename(tempC_mb100 = tempC) %>% 
  distinct(dateTime, .keep_all= TRUE)

water_temp <- kb100Temperature %>% 
  full_join(kb150Temperature, by = 'dateTime') %>% 
  full_join(kb300Temperature, by = 'dateTime') %>% 
  #full_join(mb100Temperature, by = 'dateTime') %>% 
  rowwise() %>% 
  mutate(avg_temp = mean(c(tempC_kb100, tempC_kb150, tempC_kb300), na.rm = FALSE)) 




# read in road salt application data
## add salt application data for I-90

roadname1 <- "I-90"
roadname2 <- "Rt7"

kampoosaI90 <- read_excel("data/Kampoosa_I-90.xlsx", 
                          sheet = "2017-2018", col_types = c("date", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric")) %>% 
  rbind(read_excel("data/Kampoosa_I-90.xlsx", 
                   sheet = "2018-2019", col_types = c("date", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric"))) %>% 
  rbind(read_excel("data/Kampoosa_I-90.xlsx", 
                   sheet = "2019-2020", col_types = c("date", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric"))) %>% 
  mutate(roadname = roadname1) %>% 
  dplyr::rename(date = `DATE OF EVENT`,
                total_saltI90 = `TOTAL SALT FOR 20.8 MILES OF I-90 (TONS)`) %>% 
  select(date, total_saltI90, roadname)



kampoosaRt7 <- read_excel("data/Kampoosa_Rte-7.xlsx", 
                          sheet = "2017-2018", col_types = c("date", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric")) %>% 
  rbind(read_excel("data/Kampoosa_Rte-7.xlsx", 
                   sheet = "2018-2019", col_types = c("date", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric"))) %>%
  rbind(read_excel("data/Kampoosa_Rte-7.xlsx", 
                   sheet = "2019-2020", col_types = c("date", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric"))) %>% 
  mutate(roadname = roadname2) %>% 
  dplyr::rename(date = `DATE OF EVENT`,
                total_saltRt7 = `TOTAL SALT FOR 19.27 MILES OF LEE DEPOT SECTION (TONS)`) %>% 
  select(date, total_saltRt7, roadname)


# road lengths for each watershed

# I90
wetland_I90 = mean(c(1338.47, 1375.86))/1609.344 # in miles
kb150_I90 = mean(c(1384.86, 1405.51))/1609.344 # in miles
kb300_I90 = mean(c(376, 180))/1609.344


#Rt 7
kb150_Rt7 = 1337.63/1609.344 # in miles
kb100_Rt7 = (1337.63 + (0.5*518.005))/1609.344 # in miles


# salt applied to each watershed from I-90
salt_fromI90 <- kampoosaI90 %>% 
  mutate(wetland_salt_I90 = ((wetland_I90/20.8) * total_saltI90) *907.185, # salt applied in tonnes to kg
         kb150_salt_I90 = ((kb150_I90/20.8) * total_saltI90) *907.185,
         kb300_salt_I90 = ((kb300_I90/20.8) * total_saltI90) *907.185,
         kb100_salt_I90 = (wetland_salt_I90 + kb150_salt_I90 + kb300_I90)) %>% 
  mutate(wetland_cl_I90 = ((35.45/(22.99+35.45))*wetland_salt_I90), # chloride applied kg
         kb150_cl_I90 = ((35.45/(22.99+35.45))*kb150_salt_I90),
         kb300_cl_I90 = ((35.45/(22.99+35.45))*kb300_salt_I90),
         kb100_cl_I90 = ((35.45/(22.99+35.45))*kb100_salt_I90)) %>% 
  select(date, roadname, wetland_salt_I90, kb150_salt_I90, kb300_salt_I90, kb100_salt_I90, wetland_cl_I90, kb150_cl_I90, kb300_cl_I90, kb100_cl_I90)

# salt applied to each watershed from Rt7
salt_fromRt7 <- kampoosaRt7 %>% 
  mutate(kb150_salt_Rt7 = ((kb150_Rt7/19.27) * total_saltRt7)*907.185, # salt applied in tonnes to kg
         kb100_salt_Rt7 = ((kb100_Rt7/19.27) * total_saltRt7)*907.185) %>% 
  mutate(kb150_cl_Rt7 = ((35.45/(22.99+35.45))*kb150_salt_Rt7),
         kb100_cl_Rt7 = ((35.45/(22.99+35.45))*kb100_salt_Rt7)) %>% 
  select(date, roadname, kb150_salt_Rt7, kb100_salt_Rt7, kb150_cl_Rt7, kb100_cl_Rt7)

# total cl for each watershed
total_applied_cl <- salt_fromI90 %>% 
  full_join(salt_fromRt7, by = "date") %>% 
  rowwise() %>% 
  mutate(total_salt_wetland = wetland_salt_I90,
         total_salt_kb300 = kb300_salt_I90,
         total_salt_kb150 = sum(kb150_salt_I90, kb150_salt_Rt7, na.rm = TRUE),
         total_salt_kb100 = sum(kb100_salt_I90, kb100_salt_Rt7, na.rm = TRUE),
         total_cl_wetland = wetland_cl_I90,
         total_cl_kb300 = kb300_cl_I90,
         total_cl_kb150 = total_salt_kb150*(35.45/(22.99+35.45)),
         #total_cl_kb175 = total_cl_wetland, # kb175 used to reference the fen region in later calcs be
         total_cl_kb100 = total_salt_kb100*(35.45/(22.99+35.45)))

total_applied_cl$roadname.x <- "I-90"
total_applied_cl$roadname.y <- "Rt7"


# salt from each road
salt_fromRt7_v3 <- salt_fromRt7 %>% 
  mutate(wetland_salt_Rt7 = 0,
         kb300_salt_Rt7 = 0) %>% 
  select(date, roadname, wetland_salt_Rt7, kb150_salt_Rt7, kb300_salt_Rt7, kb100_salt_Rt7) %>% 
  mutate(wetland_cl_Rt7 = ((35.45/(22.99+35.45))*wetland_salt_Rt7), # chloride applied kg
         kb150_cl_Rt7 = ((35.45/(22.99+35.45))*kb150_salt_Rt7),
         kb300_cl_Rt7 = 0,
         kb100_cl_Rt7 = ((35.45/(22.99+35.45))*kb100_salt_Rt7)) %>% 
  select(date, roadname, wetland_salt_Rt7, kb150_salt_Rt7, kb300_salt_Rt7, kb100_salt_Rt7,
         wetland_cl_Rt7, kb150_cl_Rt7, kb300_cl_Rt7, kb100_cl_Rt7)

salt_by_road <- as.data.frame(mapply(c, salt_fromI90 ,salt_fromRt7_v3, SIMPLIFY=FALSE)) %>% 
  mutate(date = as.Date(date),
         year = lubridate::year(date),
         month = lubridate::month(date),
         monthYear = as.yearmon(paste(year, month), "%Y %m")) %>% 
  group_by(monthYear, roadname) %>% 
  summarise(month_salt_wetland = sum(wetland_salt_I90, na.rm = TRUE),
            month_salt_kb150 = sum(kb150_salt_I90, na.rm = TRUE),
            month_salt_kb300 = sum(kb300_salt_I90, na.rm = TRUE),
            month_salt_kb100 = sum(kb100_salt_I90, na.rm = TRUE))


monthly_applied_salt <- total_applied_cl %>% 
  mutate(date = as.Date(date),
         year = lubridate::year(date),
         month = lubridate::month(date),
         monthYear = as.yearmon(paste(year, month), "%Y %m")) %>% 
  group_by(monthYear) %>% 
  summarise(month_salt_wetland = sum(total_salt_wetland, na.rm = TRUE),
            month_salt_kb150 = sum(total_salt_kb150, na.rm = TRUE),
            month_salt_kb300 = sum(total_salt_kb300, na.rm = TRUE),
            month_salt_kb100 = sum(total_salt_kb100, na.rm = TRUE))


# total cl from each road

# create new Rt7 df with a wetland column to allow joining
#salt_fromRt7_v2 <- salt_fromRt7 %>% 
#mutate(wetland_cl_Rt7 = 0) %>% 
#select(date, roadname, wetland_cl_Rt7, kb150_cl_Rt7, kb100_cl_Rt7)

cl_by_road <- as.data.frame(mapply(c, salt_fromI90 ,salt_fromRt7_v3, SIMPLIFY=FALSE)) %>% 
  mutate(date = as.Date(date),
         year = lubridate::year(date),
         month = lubridate::month(date),
         monthYear = as.yearmon(paste(year, month), "%Y %m")) %>% 
  group_by(monthYear, roadname) %>% 
  summarise(month_cl_wetland = sum(wetland_cl_I90, na.rm = TRUE),
            month_cl_kb150 = sum(kb150_cl_I90, na.rm = TRUE),
            month_cl_kb300 = sum(kb300_cl_I90, na.rm = TRUE),
            month_cl_kb100 = sum(kb100_cl_I90, na.rm = TRUE))




monthly_applied_cl <- total_applied_cl %>% 
  mutate(date = as.Date(date),
         year = lubridate::year(date),
         month = lubridate::month(date),
         monthYear = as.yearmon(paste(year, month), "%Y %m")) %>% 
  group_by(monthYear) %>% 
  summarise(month_cl_wetland = sum(total_cl_wetland, na.rm = TRUE),
            month_cl_kb150 = sum(total_cl_kb150, na.rm = TRUE),
            month_cl_kb300 = sum(total_cl_kb300, na.rm = TRUE),
            month_cl_kb100 = sum(total_cl_kb100, na.rm = TRUE))

# compare annual salt application values from report with my current calculation (values are similar except for 2018-19)
# calculate total cl for the 2018 and 2019 years 
#total_applied_cl <- total_applied_cl %>% 
  #filter(date >= '2018-01-05' & date <= '2019-12-31')

annual_cl_watershed <- total_applied_cl %>% 
  mutate(period = case_when(date >= "2018-01-05" & date < "2018-12-28" ~ "2018",
                            date  > "2019-01-01" & date < "2019-03-22" ~ "2019",
                            date > "2020-01-05" & date < "2020-04-18" ~ "2020")) %>% 
  group_by(period) %>% 
  summarise(annual_cl_wetland = sum(total_cl_wetland, na.rm = TRUE),
            annual_cl_kb150 = sum(total_cl_kb150, na.rm = TRUE),
            annual_cl_kb300 = sum(total_cl_kb300, na.rm = TRUE),
            annual_cl_kb100 = sum(total_cl_kb100, na.rm = TRUE))

# annual cl input by road
annual_cl_roadwatershed <- as.data.frame(mapply(c, salt_fromI90 ,salt_fromRt7_v3, SIMPLIFY=FALSE)) %>%  
  mutate(period = case_when(date >= "2018-01-05" & date < "2018-12-28" ~ "2018",
                            date  > "2019-01-01" & date < "2019-03-22" ~ "2019")) %>% 
  group_by(period, roadname) %>% 
  summarise(annual_cl_wetland = sum(wetland_cl_I90, na.rm = TRUE),
            annual_cl_kb150 = sum(kb150_cl_I90, na.rm = TRUE),
            annual_cl_kb300 = sum(kb300_cl_I90, na.rm = TRUE),
            annual_cl_kb100 = sum(kb100_cl_I90, na.rm = TRUE))




# dataframe with all the 

# Create potential evapotranspiration df. (pet) Data from http://www.nrcc.cornell.edu/wxstation/pet/pet.html
# create df with the monthly mean Potential Evapotranspiration (PET) Estimates (inches). 
# Use Albany and Hartford averages as an estimate for Kampoosa.
month <- c(1:12)
albany <- c(0.31,	0.49,	1.17,	2.21,	3.58,	4.05,	4.45,	3.80,	2.47,	1.37,	0.57,	0.30)
hartford <- c(0.41,	0.61,	1.34,	2.32,	3.57,	4.03,	4.50,	3.86,	2.60,	1.53,	0.71,	0.41)

pet <- data.frame(month, albany, hartford) # potential evapotranspiration df

pet <- pet %>%
  group_by(month) %>% 
  mutate(avg_pet_inch = ((albany+hartford)/2),
         avg_pet = avg_pet_inch*25.4) %>% #convert to mm
  select(month, avg_pet) %>% 
  tidyr::pivot_wider(names_from = month, values_from = avg_pet) %>% 
  mutate(jan_daily_avgPET = (`1`/31),
         feb_daily_avgPET = (`2`/28),
         feb_daily_avgPET_leapyear = (`2`/29),
         march_daily_avgPET = (`3`/31),
         april_daily_avgPET = (`4`/30),
         may_daily_avgPET = (`5`/31),
         june_daily_avgPET = (`6`/30),
         july_daily_avgPET = (`7`/31),
         aug_daily_avgPET = (`8`/31),
         sept_daily_avgPET = (`9`/30),
         oct_daily_avgPET = (`10`/31),
         nov_daily_avgPET = (`11`/30),
         dec_daily_avgPET = (`12`/31)) %>% 
  select(13:25) %>% 
  pivot_longer(1:13, values_to = "avg_PET", names_to = "month") %>% # create a df where values are pivoted longer
  mutate(month = c(1, 2, 2, 3:12))  # change format of months from text to number (note that th leap year value is also month 2)

pet_NonLeapYear <- pet %>% 
  filter(row_number() != 3) # remove leap year value

# create df with the avgPET for leap year (2020)
pet_LeapYear <- pet %>% 
  filter(row_number() == 3)


# area  data for the wetland and (brook + pond) calcuted in ArcGIS
#wetland <- 1756680.467356 m^2
#brook <- 36546.560753 m^2

# Rhodes Jan 2023
#wetland <- (1571250 * 10.76391041671) #ft^2 updated wetland area
#brook <- (36546.560753 * 10.76391041671) #ft^2


# Rhodes Feb 2023
wetland <- (1880640 * 10.76391041671) #ft^2 updated wetland area
brook <- (36546.560753 * 10.76391041671) #ft^2

## RAINFALL - create one df

# since all dataframes have the same columns, rename them all to be similar to those of Rainfall2017 to make merging easier
names(rainfall2018)<-names(rainfall2017)
names(rainfall2019)<- names(rainfall2017)
names(rainfall2020) <- names(rainfall2017)

# create one dataframe with all rainfall data
rainfall_full <- rbind(rainfall2017, rainfall2018, rainfall2019, rainfall2020) %>% #merge the 4 dfs
  #distinct(`Adjusted Date/Time`, .keep_all = TRUE) %>% # remove duplicated date/time
  mutate(date = as.Date(`Adjusted Date/Time`)) %>%  # create a datetime variable
  dplyr::rename(rain_inch = `Period Rain \r\n(in)`) %>% 
  distinct(date, .keep_all= TRUE) %>% 
  mutate(rain_mm = rain_inch*25.4,
         rain_mm= coalesce(rain_mm, 0)) %>% 
  select(date , rain_mm) 


# add waterYear and calculate total precipitation for each year
cumulRainfall <- rainfall_full %>% 
  mutate(wydates = date + 92, #  # Shift dates forward 92 days, such that 10-01 becomes 01-01 (this gives you the correct WY)
         wyYear = year(wydates)) %>% 
  group_by(wyYear) %>% 
  summarize(totalPrecip = sum(rain_mm))


## WATER TEMPERATURE
# daily water temperatures 
dailyWaterTemp <- water_temp %>% 
  distinct(dateTime, .keep_all= TRUE) %>% 
  mutate(date = as.Date(dateTime)) %>% 
  group_by(date) %>% 
  dplyr::summarise(daily_ave_water_temp = mean(avg_temp))




## DISCHARGE and CHLORIDE 

# select the date/time and flow variables for KB100 and KB150
KB100 <- KB100_2017_2020 %>% 
  mutate(periodNakg_KB100 = Period_NaCl_kg- Period_Cl_kg,
         site_nameKB100 = "KB100") %>% 
  dplyr::rename(periodClkg_KB100 = Period_Cl_kg,
                spec_cond_KB100 = `Spec.Cond.(mS/cm)`) %>% 
  mutate(cfs_KB100 = Flow_GPM*0.00222800925915, # discharge in cfs
         m3s_KB100 = Flow_GPM*6.309e-5,
         m3_15min_KB100 = m3s_KB100 * 900,
         stageKB100_m = `Level.(ft)`/3.281) %>%  
  select('Standard_Date_Time', 'site_nameKB100',  'cfs_KB100', 'm3s_KB100',  'm3_15min_KB100', 'stageKB100_m', 'periodClkg_KB100', 'spec_cond_KB100')



KB150 <- KB150_2017_2020 %>% 
  mutate(site_nameKB150 = "KB150") %>% 
  dplyr::rename(periodClkg_KB150 = Period_Cl_kg,
                spec_cond_KB150 = `Spec.Cond.(mS/cm)`) %>% 
  mutate(shifted_time =  Standard_Date_Time + hms("06:00:00")) %>% # time off by 6hrs
  dplyr::mutate(Flow_GPM = if_else(shifted_time >= "2017-12-26 00:00:00" & shifted_time <= "2017-12-30 23:45:00", as.numeric(NA), Flow_GPM),# interpolate for Dec 26 - Dec 30
                Flow_GPM = if_else(shifted_time >= "2018-01-01 00:00:00" & shifted_time <= "2018-01-11 04:00:00", as.numeric(NA), Flow_GPM),# interpolate the stage/flow between 01/01 and 01/10 @ 10:00AM, or even better 01/11 @ 3-4 AM
                Flow_GPM = if_else(shifted_time >= "2018-02-02 18:30:00" & shifted_time <= "2018-02-03 10:30:00", 400, Flow_GPM),# interpolate the stage/flow. This is a very short period of time so, using an average of 400gpm is relatively accurate.
                Flow_GPM = if_else(shifted_time >= "2019-03-04 19:00:00" & shifted_time <= "2019-03-07 11:00:00", as.numeric(NA), Flow_GPM)) %>% # interpolate the stage/flow. 
  mutate(Flow_GPM = na.approx(Flow_GPM)) %>% # interpolate
  dplyr::mutate(`Flow.Volume.(gal)` = if_else(shifted_time >= "2017-12-26 00:00:00" & shifted_time <= "2017-12-30 23:45:00", ((Flow_GPM+lag(Flow_GPM, default = 0))/2)*15, `Flow.Volume.(gal)`),# interpolate for Dec 26 - Dec 30
                `Flow.Volume.(gal)` = if_else(shifted_time >= "2018-01-01 00:00:00" & shifted_time <= "2018-01-11 04:00:00", ((Flow_GPM+lag(Flow_GPM, default = 0))/2)*15, `Flow.Volume.(gal)`),# interpolate the stage/flow between 01/01 and 01/10 @ 10:00AM, or even better 01/11 @ 3-4 AM
                `Flow.Volume.(gal)` = if_else(shifted_time >= "2018-02-02 18:30:00" & shifted_time <= "2018-02-03 10:30:00", ((Flow_GPM+lag(Flow_GPM, default = 0))/2)*15, `Flow.Volume.(gal)`),# interpolate the stage/flow. This is a very short period of time so, using an average of 400gpm is relatively accurate.
                `Flow.Volume.(gal)` = if_else(shifted_time >= "2019-03-04 19:00:00" & shifted_time <= "2019-03-07 11:00:00", ((Flow_GPM+lag(Flow_GPM, default = 0))/2)*15, `Flow.Volume.(gal)`)) %>% 
  mutate(cfs_KB150 = Flow_GPM*0.00222800925915, # discharge in cfs.      D12*((51.237*E12^2)+(189.985*E12)-47.102)/F2/F3
         m3s_KB150 = Flow_GPM*6.309e-5,
         m3_15min_KB150 = m3s_KB150 * 900,
         stageKB150_m = `Level.(ft)`/3.281) %>%  
  dplyr::mutate(periodClkg_KB150 = if_else(shifted_time >= "2017-12-26 00:00:00" & shifted_time <= "2017-12-30 23:45:00",  (`Flow.Volume.(gal)`)*(51.237*spec_cond_KB150^2 + 189.985*spec_cond_KB150-47.102)/0.264/1000000, periodClkg_KB150),# interpolate for Dec 26 - Dec 30
                periodClkg_KB150 = if_else(shifted_time >= "2018-01-01 00:00:00" & shifted_time <= "2018-01-11 04:00:00", (`Flow.Volume.(gal)`)*(51.237*spec_cond_KB150^2 + 189.985*spec_cond_KB150-47.102)/0.264/1000000, periodClkg_KB150),# interpolate the stage/flow between 01/01 and 01/10 @ 10:00AM, or even better 01/11 @ 3-4 AM
                periodClkg_KB150 = if_else(shifted_time >= "2018-02-02 18:30:00" & shifted_time <= "2018-02-03 10:30:00", (`Flow.Volume.(gal)`)*(51.237*spec_cond_KB150^2 + 189.985*spec_cond_KB150-47.102)/0.264/1000000, periodClkg_KB150),# interpolate the stage/flow. This is a very short period of time so, using an average of 400gpm is relatively accurate.
                periodClkg_KB150 = if_else(shifted_time >= "2019-03-04 19:00:00" & shifted_time <= "2019-03-07 11:00:00", (`Flow.Volume.(gal)`)*(51.237*spec_cond_KB150^2 + 189.985*spec_cond_KB150-47.102)/0.264/1000000, periodClkg_KB150)) %>% 
  select('site_nameKB150', 'Standard_Date_Time', 'cfs_KB150', 'm3s_KB150',  'm3_15min_KB150', 'stageKB150_m', 'periodClkg_KB150', 'spec_cond_KB150')

KB300 <- KB300_2017_2020 %>% 
  dplyr::rename(site_nameKB300 = Site,
                Standard_Date_Time = Date_Time,
                periodClkg_KB300 = Period_Cl_kg,
                spec_cond_KB300 = Spec_Cond) %>% 
  mutate(spec_cond_KB300 = if_else(Standard_Date_Time >= "2019-10-09 10:00:00" & Standard_Date_Time <= "2019-10-21 09:15:00", 0.315, spec_cond_KB300),
         spec_cond_KB300 = if_else(Standard_Date_Time >= "2019-11-24 11:00:00" & Standard_Date_Time <= "2019-12-13 11:00:00", 0.315, spec_cond_KB300)) %>% 
  mutate(cfs_KB300 = Flow_GPM*0.00222800925915,  # discharge in cfs
         m3s_KB300 = Flow_GPM*6.309e-5,
         m3_15min_KB300 = m3s_KB300 * 900,
         stageKB300_m = `Level_ft`/3.281) %>%
  select('site_nameKB300', 'Standard_Date_Time', 'cfs_KB300', 'm3s_KB300',  'm3_15min_KB300', 'stageKB300_m', 'periodClkg_KB300', 'spec_cond_KB300')


MB100 <- MB100_2017_2020 %>% 
  mutate(periodNakg_MB100 = Period_NaCl_kg- Period_Cl_kg,
         site_nameMB100 = "MB100") %>% 
  dplyr::rename(periodClkg_MB100 = Period_Cl_kg,
                spec_cond_MB100 = `Spec.Cond.(mS/cm)`) %>% 
  mutate(cfs_MB100 = Flow_GPM*0.00222800925915, # discharge in cfs
         m3s_MB100 = Flow_GPM*6.309e-5,
         m3_15min_MB100 = m3s_MB100 * 900,
         stageMB100_m = `Level.(ft)`/3.281) %>%
  select('Standard_Date_Time', 'site_nameMB100',  'cfs_MB100', 'm3s_MB100',  'm3_15min_MB100', 'stageMB100_m', 'periodClkg_MB100', 'spec_cond_MB100') 

library("hydroTSM")

# one df with discharge, chloride flux and specific conductance for KB100, KB150 and KB300 gauges
# filter for dates < April 2020
gauges_full <- KB100 %>% 
  left_join(KB150, by = 'Standard_Date_Time') %>% 
  left_join(., KB300, by = 'Standard_Date_Time') %>% 
  left_join(., MB100, by = 'Standard_Date_Time') %>% 
  mutate(year = lubridate::year(Standard_Date_Time),
         month = lubridate::month(Standard_Date_Time),
         day = as.Date(Standard_Date_Time),
         #dayMonth = paste(2020, month, day),
         #ymd(),
         monthYear = as.yearmon(paste(year, lubridate::month(Standard_Date_Time)), "%Y %m"),
         season = case_when(month >= as.numeric("6") & month <= as.numeric("8") ~ "summer",
                            month >= as.numeric("3") & month <= as.numeric("5") ~ "spring",
                            month == as.numeric("12") | month == as.numeric("1") | month == as.numeric("2")  ~ "winter",
                            month >= as.numeric("9") & month <= as.numeric("11") ~ "fall")) %>%   # create season variable
                            
  filter(monthYear <= "April 2020") %>% 
  mutate(season2 = time2season(day,                # Convert dates to seasons
                     out.fmt = "seasons")) %>% 
  arrange(Standard_Date_Time)


# create df for KB175 by assuming values for KB175 can be obtained by subtracting (KB150+KB300) from KB100)
gauges_full <- gauges_full %>% 
  mutate(periodClkg_KB175 = (periodClkg_KB100 - (periodClkg_KB150 + periodClkg_KB300)),
         periodClkg_KB175_corrected = if_else(periodClkg_KB175<0, 0, periodClkg_KB175),# replace negative cl values with 0
         m3s_KB175 = m3s_KB100 - (m3s_KB150 + m3s_KB300), # calculte flow at KB175 (assume KB175 values can be obtained by subtracting KB150 from KB100)
         m3_15min_KB175 = m3s_KB175 * 900,
         m3s_KB175_corrected = if_else(m3s_KB175<0, 0, m3s_KB175), # replace negative discharge values with 0
         site_name175 = 'KB175')  #add variable with the site names fill down with name of the site


# calculate daily discharge values at KB175 and KB300
# create dataframe with all the daily averages for flux, temp, salt application
daily_vals_full <- gauges_full %>% 
  mutate(date = as.Date(Standard_Date_Time)) %>% 
  dplyr::group_by(date) %>%  
  dplyr::summarize(
            totalDischarge_KB100 = sum(m3_15min_KB100),
            totalDischarge_KB150 = sum(m3_15min_KB150),
            totalDischarge_KB300 = sum(m3_15min_KB300),
            totalDischarge_KB175 = sum(m3_15min_KB175),
            dailyDischarge_KB100 = totalDischarge_KB100/86400, # m3/s
            dailyDischarge_KB150 = totalDischarge_KB150/86400, # m3/s
            dailyDischarge_KB175 = totalDischarge_KB175/86400, # m3/s
            dailyDischarge_KB300 = totalDischarge_KB300/86400, # m3/s
            dailyStage_KB100 = mean(stageKB100_m),
            dailyStage_KB150 = mean(stageKB150_m),
            dailyStage_KB300 = mean(stageKB300_m),
            dailyStage_MB100 = mean(stageMB100_m),
            totalClkg_KB100 = sum(periodClkg_KB100),
            totalClkg_KB150 = sum(periodClkg_KB150),
            totalClkg_KB175 = sum(periodClkg_KB175),
            totalClkg_KB175_corrected = sum(periodClkg_KB175_corrected),
            totalClkg_KB300 = sum(periodClkg_KB300),
            totalClkg_MB100 = sum(periodClkg_MB100),
            fluxClKB100 = ((totalClkg_KB100/totalDischarge_KB100)*(1000000)*(1/1000)),
            fluxClKB300 = ((totalClkg_KB300/totalDischarge_KB300)*(1000000)*(1/1000)),
            fluxClKB150 = ((totalClkg_KB150/totalDischarge_KB150)*(1000000)*(1/1000)),
            season = season2) %>% # flux concentration
  dplyr::distinct(date, .keep_all = TRUE) %>% # remove duplicated date/time 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date)) %>% 
  left_join(rainfall_full, by = c("date" = "date")) %>%  #use right left because rainfall data is collected from 2017-11-01 and gauge data from 2017-11-15
  left_join(pet_NonLeapYear, by = c("month" = "month")) %>%   # join PEt df 
  mutate(avg_PET=ifelse((date >= 	as.Date("2020-02-01") & date <= as.Date("2020-02-29")), 0.4817241, avg_PET)) %>%  # replace Feb 2020 AvgPET with calculated leap year value
         #periodRain_ft_s = (rain_inch* (9.645061728395 * (10^-7))), # daily rainfall ft/s
         #AvgPET_ft_s = (avg_PET* (9.645061728395 * (10^-7)))) %>% #daily potential evapotranspiration ft/s
  left_join(dailyAirTemp, by = 'date') %>% 
  left_join(dailyWaterTemp, by = 'date') %>% 
  dplyr::distinct(date, .keep_all = TRUE)



## Assume water and salt from the fen move into the brook (kb300 channel in the wetland), so split the wetland into the brook and the fen. 
# This allows us to calculate the water and salt storages for both the brook and fen at different times of the year and see when there is movement from fen to wetland and wetland to fen

# Sb = storage of water in brook 
# Sf = storage of water in fen
# I = precipitation
# +Qfen = flow of water from fen to brook
# Ab = area of brook + pond
# M = mass flux of Cl = (Q * C) where C is concentration of Cl in mg/L

# Storage equations


# storage in the brook 
# dSb/dt = KB300(cfs) + (KB150 - KB100)(cfs) +/- Qfen + (Ib - ETb)Ab                     (KB150 - KB100 = -KB175)

# since brook is very small assume dSb/dt ~= 0, then solve for Qfen

# water storage in fen
# dSf/dt = (If - ETf)Af +/- Qfen


# mass flux of chloride equations 

# chloride in brook
# dMb/dt = M300 + (M150 - M100) +/- Mfen


# chloride in fen 
# dMf/dt = +/- Mfen + M.add      (M.add = salt applied on I-90)


# calculate the storage in brook (Qfen) 

# first all NA values with 0, so that mean values are correctly calculated

kb100_area <- 4653086.5 # m2
kb150_area <- 1667472.1 # m2
kb300_area <- 964544.3 # m2
kb175_area <- 2941527.9


daily_vals_full <- daily_vals_full %>% 
  ungroup() %>% 
  dplyr::mutate(
    monthYear = as.yearmon(paste(year, lubridate::month(date)), "%Y %m"),
    waterYear = case_when(monthYear > "Sep 2017" & monthYear < "Oct 2018" ~ "2018",
                          monthYear  > "Sep 2018" & monthYear < "Oct 2019" ~ "2019",
                          monthYear > "Sep 2019" & monthYear < "Oct 2020" ~ "2020")) %>% 
  dplyr::filter(monthYear >= 'Jan 2018' & monthYear <= 'Apr 2020') %>%
  mutate(rownum = row_number()) %>% 
  dplyr::filter(monthYear >= 'Jan 2018' & monthYear <= 'Apr 2020') %>% 
  mutate(flowRatio = ifelse(totalDischarge_KB300  > totalDischarge_KB100, "true", "false"))
  

  
#######################

## MONTHLY SCALE
monthlyFull <- daily_vals_full %>% 
  left_join(total_applied_cl, by = 'date') %>% 
  mutate(monthYear = as.yearmon(paste(year, month), "%Y %m")) %>% 
  group_by(monthYear) %>% 
  summarise(totalDischargeKB100 = sum(totalDischarge_KB100, na.rm = TRUE),
            totalDischargeKB150 = sum(totalDischarge_KB150, na.rm = TRUE),
            totalDischargeKB300 = sum(totalDischarge_KB300, na.rm = TRUE),
            totalDischargeKB175 = sum(totalDischarge_KB175, na.rm = TRUE),
            monthlyCl_kb100 = sum(totalClkg_KB100), # monthlyCl
            monthlyCl_kb150 = sum(totalClkg_KB150),
            monthlyCl_kb175 = sum(totalClkg_KB175),
            monthlyCl_kb175_corrected = sum(totalClkg_KB175_corrected),
            monthlyCl_kb300 = sum(totalClkg_KB300),
            monthlyCl_mb100 = sum(totalClkg_MB100),
            monthSaltappliedKB175 = sum(total_cl_wetland, na.rm = TRUE),
            monthSaltappliedKB100 = sum(total_cl_kb100, na.rm = TRUE),
            monthSaltappliedKB150 = sum(total_cl_kb150, na.rm = TRUE),
            monthSaltappliedKB300 = sum(total_cl_kb300, na.rm = TRUE),
            totalmonthPrecip = sum(rain_mm, na.rm = TRUE),
            monthETP = sum(avg_PET, na.rm = TRUE))


monthlyFull <- monthlyFull %>% 
  mutate(
    dMfenKB100 = monthSaltappliedKB100 - monthlyCl_kb100,
    dMfenKB150 = monthSaltappliedKB150 - monthlyCl_kb150,
    dMfenKB300 = monthSaltappliedKB300 - monthlyCl_kb300,
    dMfenKB175 = monthSaltappliedKB175 - monthlyCl_kb175,
    dMfenKB175_corrected = monthSaltappliedKB175 - monthlyCl_kb175_corrected,
    water_storage_kb100 = ((totalmonthPrecip/1000 - monthETP/1000)*kb100_area) - totalDischargeKB100,
    rownum = row_number(),
    Month = as.Date(monthYear),
    Month = month(Month),
    Season = case_when(Month > as.numeric("5") & Month < as.numeric("9") ~ "summer",
                       Month > as.numeric("2") & Month < as.numeric("6") ~ "spring",
                       Month == as.numeric("12") | Month == as.numeric("1") | Month == as.numeric("2")  ~ "winter",
                       Month > as.numeric("8") & Month < as.numeric("12") ~ "fall"
    )) 



# create function to calculate the amount of salt stored in the wetland (mfen). Initial mfen = 0
saltFluxKB100 = monthlyFull$dMfenKB100 # can replace with dMfen_corrected
saltFluxKB150 = monthlyFull$dMfenKB150
saltFluxKB300 = monthlyFull$dMfenKB300
saltFluxKB175 = monthlyFull$dMfenKB175


#water flux
waterFluxKB100 = monthlyFull$water_storage_kb100


saltStored <- function(q){
  mfen <- 0
  for (i in 1:length(q)){
    mfen[i+1] <- q[i] + mfen[i]
  }
  return(mfen)
}


waterStored <- function(q){
  mfen <- 8e+5
  for (i in 1:length(q)){
    mfen[i+1] <- q[i] + mfen[i]
  }
  return(mfen)
}

cumulSaltKB100 <- as_tibble(saltStored(q = saltFluxKB100)) %>% 
  mutate(rownum = row_number()) %>% 
  dplyr::rename(MfenKB100 = value)

cumulSaltKB150 <- as_tibble(saltStored(q = saltFluxKB150)) %>% 
  mutate(rownum = row_number()) %>% 
  dplyr::rename(MfenKB150 = value)

cumulSaltKB300 <- as_tibble(saltStored(q = saltFluxKB300)) %>% 
  mutate(rownum = row_number()) %>% 
  dplyr::rename(MfenKB300 = value)


cumulSaltKBKB175 <- as_tibble(saltStored(q = saltFluxKB175)) %>% 
  mutate(rownum = row_number()) %>% 
  dplyr::rename(MfenKB175 = value)


# water accumulation
cumulWaterKB100 <- as_tibble(waterStored(q = waterFluxKB100)) %>% 
  mutate(rownum = row_number()) %>% 
  dplyr::rename(waterKB100 = value)


# join the cumulSalt df with the monthly full
monthlyFull <- monthlyFull %>% 
  right_join(cumulSaltKB100, by = c("rownum" = "rownum")) %>% 
  right_join(cumulSaltKB150, by = c("rownum" = "rownum")) %>% 
  right_join(cumulSaltKB300, by = c("rownum" = "rownum")) %>% 
  right_join(cumulSaltKBKB175, by = c("rownum" = "rownum")) %>% 
  right_join(cumulWaterKB100, by = c("rownum" = "rownum")) %>% 
  mutate(precip_input_kb100 = (totalmonthPrecip/1000)*kb100_area,
         discharge_kb100_mm = (totalDischargeKB100/kb100_area)*1000,
         discharge_kb150_mm = (totalDischargeKB150/kb150_area)*1000,
         discharge_kb300_mm = (totalDischargeKB300/kb300_area)*1000)
  

total_precip_kb100 <- sum(monthlyFull$precip_input_kb100, na.rm = TRUE)


# run seasonal mann kendall on the monthly accumulation data
trend_kb100 <- SeasonalMannKendall(ts(monthlyFull$dMfenKB100))
trend_kb150 <- SeasonalMannKendall(ts(monthlyFull$dMfenKB150))
trend_kb300 <- SeasonalMannKendall(ts(monthlyFull$dMfenKB300))


monthlyFull <- head(monthlyFull, -1) %>% # drop last column with na
  mutate(reteff_KB150 = (dMfenKB150/monthSaltappliedKB150)*100, # retention efficiency
         reteff_KB175 = (dMfenKB175/monthSaltappliedKB175)*100)

monthlyFull$reteff_KB150 <- replace(monthlyFull$reteff_KB150, is.infinite(monthlyFull$reteff_KB150), NA)
monthlyFull$reteff_KB175 <- replace(monthlyFull$reteff_KB175, is.infinite(monthlyFull$reteff_KB175), NA)

# calculate retention rates
retention_kb150 <- median(monthlyFull$reteff_KB150, na.rm = TRUE)
retention_kb175 <- median(monthlyFull$reteff_KB175, na.rm = TRUE)


ret_table <- monthlyFull %>% 
  filter(monthSaltappliedKB175 != 0) %>% 
  select(monthYear, monthSaltappliedKB175, dMfenKB175, reteff_KB175, monthSaltappliedKB150, dMfenKB150, reteff_KB150) %>% 
  dplyr::rename(`Chloride Accumulated \r\nWetland Region \r\n(kg/month)` = dMfenKB175,
                `Chloride Accumulated \r\n \r\n(kg/month)` = dMfenKB150, 
                `Retention Effeciency\r\nWetland Region (%)` = reteff_KB175,
                `Retention Effeciency \r\nKB150 (%)` = reteff_KB150,
                `Chloride Applied \r\nWetland Region \r\n(kg/month)` = monthSaltappliedKB175,
                `Chloride Applied \r\nKB150 \r\n(kg/month)` = monthSaltappliedKB150,
                `Period` = monthYear) 



## annual summary

# miles of road in each watershed
kb100_road <- 14797/1609.344
kb150_road <- 8256/1609.344
kb300_road <- 1112/1609.344
wetland_road <- 5428.66/1609.344

annualAccumulation <- monthlyFull %>% 
  mutate(Year = case_when(monthYear >= "Jan 2018" & monthYear < "Jan 2019" ~ "2018",
                               monthYear  >= "Jan 2019" & monthYear <= "Dec 2019" ~ "2019")) %>% 
  filter(Year != 2020) %>% 
  group_by(Year) %>% 
  summarise(`Total Precipitation (mm)` = sum(totalmonthPrecip),
            `Total PET (mm)` = sum(monthETP),
            `Average Discharge - KB300 (m3/year)` = sum(totalDischargeKB300),
            `Average Discharge - KB175 (m3/year)` = sum(totalDischargeKB175),
            `Average Discharge - KB100 (m3/year)` = sum(totalDischargeKB100),
            `Average Discharge - KB150 (m3/year)` = sum(totalDischargeKB150),
            `Average Discharge - KB300 (m3/s)` = `Average Discharge - KB300 (m3/year)`/3.154e+7,
            `Average Discharge - KB100 (m3/s)` = `Average Discharge - KB100 (m3/year)`/3.154e+7,
            `Average Discharge - KB150 (m3/s)` = `Average Discharge - KB150 (m3/year)`/3.154e+7,
            `Average Discharge - KB175 (m3/s)` = `Average Discharge - KB175 (m3/year)`/3.154e+7,
            `Discharge - KB300 (mm)` = (`Average Discharge - KB300 (m3/year)`/kb300_area)*1000,
            `Discharge - KB100 (mm)` = (`Average Discharge - KB100 (m3/year)`/kb100_area)*1000,
            `Discharge - KB150 (mm)` = (`Average Discharge - KB150 (m3/year)`/kb150_area)*1000,
            `Discharge - KB175 (mm)` = (`Average Discharge - KB175 (m3/year)`/kb175_area)*1000,
            `Total Applied Cl KB175 (kg)` = sum(monthSaltappliedKB175),
            `Total Applied Cl KB150 (kg)` = sum(monthSaltappliedKB150),
            `Total Applied Cl KB300 (kg)` = sum(monthSaltappliedKB300),
            `Total Applied Cl KB100 (kg)` = sum(`Total Applied Cl KB175 (kg)`, `Total Applied Cl KB150 (kg)`, `Total Applied Cl KB300 (kg)`),
            `KB300 Cl (kg)` = round(sum(monthlyCl_kb300)),
            `KB175 Cl (kg)` = round(sum(monthlyCl_kb175)),
            `KB175 Cl (kg)_corrected` = round(sum(monthlyCl_kb175_corrected)),
            
            `MB100 Cl (kg)` = round(sum(monthlyCl_mb100)),
            `KB150 Cl (kg)` = round(sum(monthlyCl_kb150)),
            `KB100 Cl (kg)` = round(sum(`KB300 Cl (kg)`, `KB175 Cl (kg)`, `KB150 Cl (kg)`)),
            `Total Cl Stored KB100` = `Total Applied Cl KB100 (kg)` - `KB100 Cl (kg)`,
            `Total Cl Stored KB150` = `Total Applied Cl KB150 (kg)` - `KB150 Cl (kg)`, 
            `Total Cl Stored KB175` = `Total Applied Cl KB175 (kg)` - `KB175 Cl (kg)`,
            `Total Cl Stored KB300` = `Total Applied Cl KB300 (kg)` - `KB300 Cl (kg)`) %>% 
  mutate(`Total Applied Cl KB100 - Normalised (kg)`= `Total Applied Cl KB100 (kg)`/kb100_road,
         `Total Applied Cl KB150 - Normalised (kg)`= `Total Applied Cl KB150 (kg)`/kb150_road,
         `Total Applied Cl KB300 - Normalised (kg)`= `Total Applied Cl KB300 (kg)`/kb300_road,
         `Total Applied Cl KB175 - Normalised (kg)`= `Total Applied Cl KB175 (kg)`/wetland_road) %>% 
          mutate(`KB175 retention` = round((`Total Cl Stored KB175`/`Total Applied Cl KB175 (kg)`)*100),
         `KB150 retention` = round((`Total Cl Stored KB150`/`Total Applied Cl KB150 (kg)`)*100),
         `KB300 retention` = round((`Total Cl Stored KB300`/`Total Applied Cl KB300 (kg)`)*100),
         `KB100 retention` = round((`Total Cl Stored KB100`/`Total Applied Cl KB100 (kg)`)*100))#    # thousands of kgs
 

# annual chloride steady state concentrations
kb100_ss = (mean(annualAccumulation$`Total Applied Cl KB100 (kg)`)/mean(annualAccumulation$`Average Discharge - KB100 (m3/year)`))*1000
kb150_ss = (mean(annualAccumulation$`Total Applied Cl KB150 (kg)`)/mean(annualAccumulation$`Average Discharge - KB150 (m3/year)`))*1000
kb300_ss = (mean(annualAccumulation$`Total Applied Cl KB300 (kg)`)/mean(annualAccumulation$`Average Discharge - KB300 (m3/year)`))*1000


# save annual accumulation, monthlyFull and daily flux
write_csv(annualAccumulation, "results/tables/annualAccumulation.csv")
write_csv(monthlyFull, "results/tables/monthly_values.csv")
write_csv(daily_vals_full, "results/tables/daily_values.csv")


