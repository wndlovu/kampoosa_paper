library(tidyverse)
library(readxl)
library(lubridate)
library(gtools)
library(zoo)
library(stringr)
options(scipen=999)


# read in the probe data files
KB100_2017_2020 <- read_excel("data/KB100_2017_2020.xlsx") 
KB150_2017_2020 <- read_excel("data/KB150_2017_2020.xlsx") 
KB300_2017_2020 <- read_excel("data/KB300_2017_2020.xlsx")

# read airTemp data 
airTemp <- read_csv("data/airTemp.csv") %>% 
  dplyr::rename(date = dateTime) %>% 
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
  dplyr::rename(tempC_kb100 = tempC)

kb150Temperature <- read_csv("data/kb150Temperature.csv") %>% 
  select(dateTime, tempC)%>% 
  dplyr::rename(tempC_kb150 = tempC)

kb300Temperature <- read_csv("data/kb300Temperature.csv") %>% 
  select(dateTime, tempC)%>% 
  dplyr::rename(tempC_kb300 = tempC)

water_temp <- kb100Temperature %>% 
  full_join(kb150Temperature, by = 'dateTime') %>% 
  full_join(kb300Temperature, by = 'dateTime') %>% 
  rowwise() %>% 
  mutate(avg_temp = mean(c(tempC_kb100, tempC_kb150, tempC_kb300), na.rm = TRUE)) 
  



# read in road salt application data
## add salt application data for I-90

roadname1 <- "I-90"

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
                applied_cl = `Cl (kg)`) %>% 
  select(date, applied_cl, roadname)




# Create potential evapotranspiration df. (pet) Data from http://www.nrcc.cornell.edu/wxstation/pet/pet.html
# create df with the monthly mean Potential Evapotranspiration (PET) Estimates (inches). 
# Use Albany and Hartford averages as an estimate for Kampoosa.
month <- c(1:12)
albany <- c(0.31,	0.49,	1.17,	2.21,	3.58,	4.05,	4.45,	3.80,	2.47,	1.37,	0.57,	0.30)
hartford <- c(0.41,	0.61,	1.34,	2.32,	3.57,	4.03,	4.50,	3.86,	2.60,	1.53,	0.71,	0.41)

pet <- data.frame(month, albany, hartford) # potential evapotranspiration df

pet <- pet %>%
  group_by(month) %>% 
  summarise(avg_pet = ((albany+hartford)/2)) %>% 
  pivot_wider(names_from = month, values_from = avg_pet) %>% 
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

wetland <- (1756680.467356 * 10.76391041671) #ft^2
brook <- (36546.560753 * 10.76391041671) #ft^2



## RAINFALL - create one df

# since all dataframes have the same columns, rename them all to be similar to those of Rainfall2017 to make merging easier
names(rainfall2018)<-names(rainfall2017)
names(rainfall2019)<- names(rainfall2017)
names(rainfall2020) <- names(rainfall2017)

# create one dataframe with all rainfall data
rainfall_full <- rbind(rainfall2017, rainfall2018, rainfall2019, rainfall2020) %>% #merge the 4 dfs
  distinct(`Adjusted Date/Time`, .keep_all = TRUE) %>% # remove duplicated date/time
  mutate(date = as.Date(`Adjusted Date/Time`)) %>%  # create a datetime variable
  dplyr::rename(rain_inch = `Period Rain \r\n(in)`) %>% 
  select(date , rain_inch) 
        

# add waterYear and calculate total precipitation for each year
cumulRainfall <- rainfall_full %>% 
  mutate(wydates = date + 92, #  # Shift dates forward 92 days, such that 10-01 becomes 01-01 (this gives you the correct WY)
         wyYear = year(wydates)) %>% 
  group_by(wyYear) %>% 
  summarize(totalPrecip = sum(rain_inch))


## WATER TEMPERATURE
# daily water temperatures 
dailyWaterTemp <- water_temp %>% 
  mutate(date = as.Date(dateTime)) %>% 
  group_by(date) %>% 
  dplyr::summarise(daily_ave_water_temp = mean(avg_temp))


## AIR TEMPERATURE
# daily air temperatures 
dailyAirTemp <- airTemp %>% 
  distinct(date, .keep_all = TRUE) %>% # remove duplicated date/time
  mutate(airtempr = if_else(date >= "2019-06-10 13:30:00" & date <= "2019-08-12 13:00:00",(airtempr-32)/1.8, airtempr),
         airtempr = if_else(date >= "2019-01-19 14:15:00" & date <= "2019-02-19 15:00:00", (airtempr-32)/1.8, airtempr),
         airtempr = if_else(date >= "2020-06-01 00:00:00" & date <= "2020-08-01 00:00:00", as.numeric(NA), airtempr)) %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(date) %>% 
  summarize(daily_ave_air_temp = mean(airtempr, na.rm = TRUE)) #DailyMeanTemp



## DISCHARGE and CHLORIDE 

# select the date/time and flow variables for KB100 and KB150
KB100 <- KB100_2017_2020 %>% 
  mutate(periodNakg_KB100 = Period_NaCl_kg- Period_Cl_kg,
         site_nameKB100 = "KB100") %>% 
  dplyr::rename(periodClkg_KB100 = Period_Cl_kg,
                spec_cond_KB100 = `Spec.Cond.(mS/cm)`) %>% 
  mutate(cfs_KB100 = Flow_GPM*0.00222800925915) %>%  # discharge in cfs
  select('Standard_Date_Time', 'site_nameKB100',  'cfs_KB100', 'periodClkg_KB100', 'spec_cond_KB100')

KB150 <- KB150_2017_2020 %>% 
  mutate(site_nameKB150 = "KB150") %>% 
  dplyr::rename(periodClkg_KB150 = Period_Cl_kg,
                spec_cond_KB150 = `Spec.Cond.(mS/cm)`) %>% 
  mutate(cfs_KB150 = Flow_GPM*0.00222800925915) %>%  # discharge in cfs
  select('site_nameKB150', 'Standard_Date_Time', 'cfs_KB150', 'periodClkg_KB150', 'spec_cond_KB150')


KB300 <- KB300_2017_2020 %>% 
  dplyr::rename(site_nameKB300 = Site,
                Standard_Date_Time = Date_Time,
                periodClkg_KB300 = Period_Cl_kg,
                spec_cond_KB300 = Spec_Cond) %>% 
  mutate(cfs_KB300 = Flow_GPM*0.00222800925915) %>%  # discharge in cfs
  select('site_nameKB300', 'Standard_Date_Time', 'cfs_KB300', 'periodClkg_KB300', 'spec_cond_KB300')


# one df with discharge, chloride flux and specific conductance for KB100, KB150 and KB300 gauges
gauges_full <- KB100 %>% 
  left_join(KB150, by = 'Standard_Date_Time') %>% 
  left_join(., KB300, by = 'Standard_Date_Time') %>% 
  mutate(year = lubridate::year(Standard_Date_Time),
         month = lubridate::month(Standard_Date_Time),
         #day = lubridate::day(Standard_Date_Time),
         #dayMonth = paste("2020", month, day),
          #ymd(),
         monthYear = as.yearmon(paste(year, lubridate::month(Standard_Date_Time)), "%Y %m"),
         season = case_when(month > as.numeric("5") & month < as.numeric("9") ~ "summer",
                            month > as.numeric("2") & month < as.numeric("6") ~ "spring",
                            month == as.numeric("12") | month == as.numeric("1") | month == as.numeric("2")  ~ "winter",
                            month > as.numeric("8") & month < as.numeric("12") ~ "fall")) %>%   # create season variable
  filter(monthYear != "Dec 2020") %>% 
  arrange(Standard_Date_Time)
  

# create df for KB175 by assuming values for KB175 can be obtained by subtracting KB150 from KB100)
gauges_full <- gauges_full %>% 
  mutate(periodClkg_KB175 = (periodClkg_KB100 - periodClkg_KB150),
         periodClkg_KB175_corrected = if_else(periodClkg_KB175<0, 0, periodClkg_KB175),# replace negative cl values with 0
         cfs_KB175 = cfs_KB100 - cfs_KB150, # calculte flow at KB175 (assume KB175 values can be obtained by subtracting KB150 from KB100)
         cfs_KB175_corrected = if_else(cfs_KB175<0, 0, cfs_KB175), # replace negative discharge values with 0
         site_name175 = 'KB175')  #add variable with the site names fill down with name of the site


# calculate daily discharge values at KB175 and KB300
# create dataframe with all the daily averages for flux, temp, salt application
daily_vals_full <- gauges_full %>% 
  mutate(date = as.Date(Standard_Date_Time)) %>% 
  group_by(date) %>%  
  summarize(dailyCFS_KB100 = mean(cfs_KB100), 
            dailyCFS_KB150 = mean(cfs_KB150), 
            dailyCFS_KB175 = mean(cfs_KB175),  #  DailyMeanFlow
            dailyCFS_KB175_corrected = mean(cfs_KB175_corrected),
            dailyCFS_KB300 = mean(cfs_KB300), 
            totalClkg_KB100 = sum(periodClkg_KB100),
            totalClkg_KB150 = sum(periodClkg_KB150),
            totalClkg_KB175 = sum(periodClkg_KB175),
            totalClkg_KB175_corrected = sum(periodClkg_KB175_corrected),
            totalClkg_KB300 = sum(periodClkg_KB300)) %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>% 
  left_join(rainfall_full, by = c("date" = "date")) %>%  #use right left because rainfall data is collected from 2017-11-01 and gauge data from 2017-11-15
  left_join(pet_NonLeapYear, by = c("month" = "month")) %>%   # join PEt df 
  mutate(avg_PET=ifelse((date >= 	as.Date("2020-02-01") & date <= as.Date("2020-02-29")), pet_LeapYear$avg_PET, avg_PET), # replace Feb 2020 AvgPET with calculated leap year value
         periodRain_ft_s = (rain_inch* (9.645061728395 * (10^-7))), # daily rainfall ft/s
         AvgPET_ft_s = (avg_PET* (9.645061728395 * (10^-7)))) %>% #daily potential evapotranspiration ft/s
  left_join(dailyAirTemp, by = 'date') %>% 
  left_join(dailyWaterTemp, by = 'date')


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

daily_vals_full <- daily_vals_full %>% 
  mutate(Qfen = (dailyCFS_KB175 - (dailyCFS_KB300 + ((periodRain_ft_s - AvgPET_ft_s)*brook))),  # equation to calculate Qfen
         Qfen_corrected = (dailyCFS_KB175_corrected - (dailyCFS_KB300 + ((periodRain_ft_s - AvgPET_ft_s)*brook))),
         dSf_dt = (((periodRain_ft_s - AvgPET_ft_s)*wetland) - Qfen),
         dSf_dt_corrected = (((periodRain_ft_s - AvgPET_ft_s)*wetland) - Qfen_corrected),
         monthYear = as.yearmon(paste(year, lubridate::month(date)), "%Y %m"),
         waterYear = case_when(monthYear > "Sep 2017" & monthYear < "Oct 2018" ~ "2018",
                               monthYear  > "Sep 2018" & monthYear < "Oct 2019" ~ "2019",
                               monthYear > "Sep 2019" & monthYear < "Oct 2020" ~ "2020"))





## MONTHLY SCALE
monthlyFull <- daily_vals_full %>% 
  left_join(kampoosaI90, by = 'date') %>% 
  mutate(monthYear = as.yearmon(paste(year, month), "%Y %m")) %>% 
  group_by(monthYear) %>% 
  summarise(monthDischargeKB100 = mean(dailyCFS_KB100, na.rm = TRUE),
            monthDischargeKB150 = mean(dailyCFS_KB150, na.rm = TRUE),
            monthDischargeKB175 = mean(dailyCFS_KB175, na.rm = TRUE),
            monthDischargeKB175_corrected = mean(dailyCFS_KB175_corrected, na.rm = TRUE),
            monthDischargeKB300 = mean(dailyCFS_KB300, na.rm = TRUE),
            monthlyCl_kb100 = sum(totalClkg_KB100), # monthlyCl
            monthlyCl_kb150 = sum(totalClkg_KB150),
            monthlyCl_kb175 = sum(totalClkg_KB175),
            monthlyCl_kb175_corrected = sum(totalClkg_KB175_corrected),
            monthlyCl_kb300 = sum(totalClkg_KB300),
            monthSaltapplied = sum(applied_cl, na.rm = TRUE),
            totalmonthPrecip = sum(rain_inch, na.rm = TRUE),
            monthETP = sum(avg_PET, na.rm = TRUE),
            monthlyQfen = mean(Qfen, na.rm = TRUE)) %>% 
  mutate(
         dMfen = (((monthSaltapplied*0.91) + monthlyCl_kb300) - monthlyCl_kb175), # change back to monthlyClkg_KB175 
         dMfen_corrected = (((monthSaltapplied*0.91) + monthlyCl_kb300) - monthlyCl_kb175_corrected), # change back to monthlyClkg_KB175 
         rownum = row_number(),
         Month = as.Date(monthYear),
         Month = month(Month),
         Season = case_when(Month > as.numeric("5") & Month < as.numeric("9") ~ "summer",
                            Month > as.numeric("2") & Month < as.numeric("6") ~ "spring",
                            Month == as.numeric("12") | Month == as.numeric("1") | Month == as.numeric("2")  ~ "winter",
                            Month > as.numeric("8") & Month < as.numeric("12") ~ "fall"
         )) %>% 
  drop_na()



# create function to calculate the amount of salt stored in the wetland (mfen). Initial mfen = 0
saltFlux = monthlyFull$dMfen
saltStored <- function(q){
  mfen <- 0
  for (i in 1:length(q)){
    mfen[i+1] <- q[i] + mfen[i]
  }
  return(mfen)
}

cumulSalt <- as_tibble(saltStored(q = saltFlux)) %>% 
  mutate(rownum = row_number())

# join the cumulSalt df with the monthly full
monthlyFull <- monthlyFull %>% 
  right_join(cumulSalt, by = c("rownum" = "rownum")) %>% 
  rename(Mfen = value) 



## calculate the amount of salt accumlated in the fen and total discharge and precipitation
annualAccumulation <- monthlyFull %>% 
  mutate(waterYear = case_when(monthYear > "Sep 2017" & monthYear < "Oct 2018" ~ "2018",
                            monthYear  > "Sep 2018" & monthYear < "Oct 2019" ~ "2019",
                            monthYear > "Sep 2019" & monthYear < "Oct 2020" ~ "2020")) %>% 
  group_by(waterYear) %>% 
  summarise(`Total Precipitation (inch)` = sum(totalmonthPrecip),
            `Average Discharge - KB-300 (cfs)` = mean(monthDischargeKB300),
            `Average Discharge - KB-175 (cfs)` = mean(monthDischargeKB175),
            #`Average Discharge - KB100 (cfs)` = mean(monthDischargeKB100),
            #`Average Discharge - KB150 (cfs)` = mean(monthDischargeKB150),
            `Total Applied Cl - I90 (kg)` = sum(monthSaltapplied),
            `KB300 Cl (kg)` = round(sum(monthlyCl_kb300)),
            `KB175 Cl (kg)` = round(sum(monthlyCl_kb175)),
            `KB175 Cl (kg)_corrected` = round(sum(monthlyCl_kb175_corrected)),
            `Ave Flux Concentration - KB175 (mg/L)` =  ((`KB175 Cl (kg)`*1000000)/(`Average Discharge - KB-175 (cfs)`*31536000))/28.316,
            `Ave Flux Concentration - KB175 (mg/L)_corrected` =  ((`KB175 Cl (kg)_corrected`*1000000)/(`Average Discharge - KB-175 (cfs)`*31536000))/28.316,
            `Total Cl Stored in fen` = sum(dMfen)) %>%  # calculated from KB175 Cl (kg)_corrected 
  mutate(`Fen area` = round(`Total Cl Stored in fen`)) %>%  # thousands of kgs
  drop_na()        

# save annual accumulation, monthlyFull and daily flux
#write_csv(annualAccumulation, "results/tables/annualAccumulation.csv")
#write_csv(monthlyFull, "results/tables/monthly_values.csv")
#write_csv(daily_vals_full, "results/tables/daily_values.csv")




