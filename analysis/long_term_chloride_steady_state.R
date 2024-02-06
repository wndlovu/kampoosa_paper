library(tidyverse)
library(readr)

# annaul cl application from 2012 - 2019
cl_applied <- read_csv("data/historic_salt_application_v2.csv")
precip <- read_csv("data/Kampoosa_Precip_2012_2020.csv")
monthly_values <- read_csv("results/tables/monthly_values.csv")

# create et df
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

# watershed areas
kb100_area <- 4653086.5
kb150_area <- 1667472.1 # m2
kb300_area <- 964544.3 # m2

# create precip df
precip <- precip %>% 
  dplyr::rename(date = ObservationDate,
                rain_inch = TotalPrecipAmt) %>% 
  mutate(rain_inch = if_else(rain_inch == "T", 0, as.numeric(rain_inch))) %>% 
  mutate(date = mdy(date)) %>%  # create a datetime variable
  mutate(rain_mm = rain_inch*25.4,
         rain_mm= coalesce(rain_mm, 0)) %>% 
  arrange(date) %>% 
  select(date , rain_mm) 


# fill in missing dates and assume precip is 0
complete_dates <- seq(min(precip$date), max(precip$date), by = "1 day")

complete_df <- data.frame(date = complete_dates)

#merge precip with full dates
precip <- complete_df %>%
  left_join(precip, by = "date") %>%
  arrange(date)

# Fill missing values with 0
precip$rain_mm[is.na(precip$rain_mm)] <- 0


# water balance
monthly_values <- monthly_values %>% 
  mutate(date = dmy(paste("01", monthYear)),
         year = lubridate::year(lubridate::mdy(paste("01", monthYear)))) %>% 
  #drop_na() %>% 
  #mutate(monthYear = as.yearmon(paste("01", monthYear))) %>% 
  select(date, year, waterKB100, totalDischargeKB100, totalDischargeKB150, totalDischargeKB300, totalmonthPrecip, monthETP) %>% 
  mutate(precip_vol_kb100 = totalmonthPrecip/1000*kb100_area,
         precip_vol_kb150 = totalmonthPrecip/1000*kb150_area,
         precip_vol_kb300 = totalmonthPrecip/1000*kb300_area,
         et_vol_kb100 = monthETP/1000*kb100_area,
         et_vol_kb150 = monthETP/1000*kb150_area,
         et_vol_kb300 = monthETP/1000*kb300_area)



# since water balance is closed, assume ds/dt = 0. Simulate streamflow as the difference between precip and ET
test_yearly <- monthly_values %>% 
  group_by(year) %>% 
  summarise(`Total Precipitation (mm)` = sum(totalmonthPrecip),
            `Total PET (mm)` = sum(monthETP),
            flow_kb300 = sum(totalDischargeKB300),
            flow_kb100 = sum(totalDischargeKB100),
            flow_kb150 = sum(totalDischargeKB150),
            simul_flow_kb300 = sum(precip_vol_kb300 - et_vol_kb300),
            simul_flow_kb100 = sum(precip_vol_kb100 - et_vol_kb100),
            simul_flow_kb150 = sum(precip_vol_kb150 - et_vol_kb150))


# for those two years calculate the r2, rmse, kge and nse
# model performance evaluation (KB100)
rsq <- function(x, y) summary(lm(y~x))$r.squared


print('KB100')
cat('r2:', rsq(test_yearly$flow_kb100, test_yearly$simul_flow_kb100), '\n')
cat('NSE:', NSE(sim=test_yearly$simul_flow_kb100, obs=test_yearly$flow_kb100), '\n')
cat('KGE:', KGE(sim=test_yearly$simul_flow_kb100, obs=test_yearly$flow_kb100), '\n')
cat('RMSE:', rmse(sim=test_yearly$simul_flow_kb100, obs=test_yearly$flow_kb100), '\n')
print(' ')
print('KB150')
cat('r2:', rsq(test_yearly$flow_kb150, test_yearly$simul_flow_kb150), '\n')
cat('NSE:', NSE(sim=test_yearly$simul_flow_kb150, obs=test_yearly$flow_kb150), '\n')
cat('KGE:', KGE(sim=test_yearly$simul_flow_kb150, obs=test_yearly$flow_kb150), '\n')
cat('RMSE:', rmse(sim=test_yearly$simul_flow_kb150, obs=test_yearly$flow_kb150), '\n')
print(' ')
print('KB300')
cat('r2:', rsq(test_yearly$flow_kb300, test_yearly$simul_flow_kb300), '\n')
cat('NSE:', NSE(sim=test_yearly$simul_flow_kb300, obs=test_yearly$flow_kb300), '\n')
cat('KGE:', KGE(sim=test_yearly$simul_flow_kb300, obs=test_yearly$flow_kb300), '\n')
cat('RMSE:', rmse(sim=test_yearly$simul_flow_kb300, obs=test_yearly$flow_kb300), '\n')


# predict streamflow for the 2012 - 2017 period
flow_predict <- precip %>% 
  distinct(date, .keep_all = TRUE) %>% 
  mutate(month = lubridate::month(date),
         year = lubridate::year(date)) %>% 
  mutate(month2 =  as.numeric(format(date, "%m"))) %>% #,
  #year = format(date, "%Y")) %>% 
  mutate(monthYear = as.yearmon(paste(year, month), "%Y %m")) %>% 
  
  #filter(date <= as.Date("2019-01-31")) %>%    # up to April 2020
  left_join(pet_NonLeapYear, by = c("month" = "month")) %>% 
  mutate(monthYear = as.yearmon(paste(year, lubridate::month(date)), "%Y %m"),
         avg_PET = ifelse((monthYear == as.yearmon('Feb 2012') | monthYear == as.yearmon('Feb 2016')), 0.4817241, avg_PET)) %>%
  dplyr::group_by(year, month) %>% 
  dplyr::summarise(totalmonthPrecip = sum(rain_mm, na.rm = TRUE),
                   monthETP = sum(avg_PET)) %>% 
  mutate(#date = dmy(paste("01", monthYear)),
    #year = lubridate::year(date),
    precip_vol_kb100 = totalmonthPrecip/1000*kb100_area,
    precip_vol_kb150 = totalmonthPrecip/1000*kb150_area,
    precip_vol_kb300 = totalmonthPrecip/1000*kb300_area,
    et_vol_kb100 = monthETP/1000*kb100_area,
    et_vol_kb150 = monthETP/1000*kb150_area,
    et_vol_kb300 = monthETP/1000*kb300_area) 

# calculate the annaul discharge
h_annual_flows <- flow_predict %>% 
  #filter(year < 2020) %>% 
  group_by(year) %>% 
  summarise(`Total Precipitation (mm)` = sum(totalmonthPrecip),
            `Total PET (mm)` = sum(monthETP),
            simul_flow_kb300 = sum(precip_vol_kb300 - et_vol_kb300),
            simul_flow_kb100 = sum(precip_vol_kb100 - et_vol_kb100),
            simul_flow_kb150 = sum(precip_vol_kb150 - et_vol_kb150))



cl_applied <- cl_applied %>% 
  filter(year > 2011 & year < 2020) # calculate the historic steady state values


# merge cl applied and flow data
h_full <- cl_applied %>% 
  left_join(h_annual_flows, by = c('year' = 'year')) #%>% 
#drop_na() # use when using 2012 - 2017


# long-term chloride steady state
h_kb100_ss = (mean(h_full$`Total Applied Cl KB100 (kg)`)/mean(h_full$simul_flow_kb100))*1000
h_kb150_ss = (mean(h_full$`Total Applied Cl KB150 (kg)`)/mean(h_full$simul_flow_kb150))*1000
h_kb300_ss = (mean(h_full$`Total Applied Cl KB300 (kg)`)/mean(h_full$simul_flow_kb300))*1000


