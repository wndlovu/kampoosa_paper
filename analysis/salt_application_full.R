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
library(data.table)
#library(e1071)
options(scipen=999)

roadname1 <- "I-90"
roadname2 <- "Rt7"

kampoosaI90_full <- read_excel("data/Kampoosa I-90_2011_2023.xlsx", 
                                                     sheet = "2011-2012", col_types = c("date", 
                                                                                        "numeric", "numeric")) %>% 
  rbind(read_excel("data/Kampoosa I-90_2011_2023.xlsx", 
             sheet = "2012-2013", col_types = c("date", 
                                                "numeric", "numeric"))) %>%
  rbind(read_excel("data/Kampoosa I-90_2011_2023.xlsx", 
             sheet = "2013-2014", col_types = c("date", 
                                                "numeric", "numeric"))) %>%
  rbind(read_excel("data/Kampoosa I-90_2011_2023.xlsx", 
             sheet = "2014-2015", col_types = c("date", 
                                                "numeric", "numeric"))) %>%
  rbind(read_excel("data/Kampoosa I-90_2011_2023.xlsx", 
             sheet = "2015-2016", col_types = c("date", 
                                                "numeric", "numeric"))) %>%
  rbind(read_excel("data/Kampoosa I-90_2011_2023.xlsx", 
             sheet = "2016-2017", col_types = c("date", 
                                                "numeric", "numeric"))) %>%
  rbind(read_excel("data/Kampoosa I-90_2011_2023.xlsx", 
             sheet = "2017-2018", col_types = c("date", 
                                                "numeric", "numeric"))) %>%
  rbind(read_excel("data/Kampoosa I-90_2011_2023.xlsx", 
             sheet = "2018-2019", col_types = c("date", 
                                                "numeric", "numeric"))) %>%
  rbind(read_excel("data/Kampoosa I-90_2011_2023.xlsx", 
             sheet = "2019-2020", col_types = c("date", 
                                                "numeric", "numeric"))) %>%
  rbind(read_excel("data/Kampoosa I-90_2011_2023.xlsx", 
             sheet = "2021-2022", col_types = c("date", 
                                                "numeric", "numeric"))) %>%
  rbind(read_excel("data/Kampoosa I-90_2011_2023.xlsx", 
             sheet = "2022-2023", col_types = c("date", 
                                                "numeric", "numeric"))) %>% 
  mutate(roadname = roadname1) %>% 
  dplyr::rename(date = `DATE OF EVENT`,
                total_saltI90 = `TOTAL SALT FOR 20.8 MILES OF I-90 (TONS)`) %>% 
  select(date, total_saltI90, roadname)



kampoosaRt7_full <- read_excel("data/Kampoosa Rte 7_2011_2023.xlsx", 
                                       sheet = "2011-2012", col_types = c("date", 
                                                                          "numeric", "numeric")) %>% 
  rbind(read_excel("data/Kampoosa Rte 7_2011_2023.xlsx", 
                   sheet = "2012-2013", col_types = c("date", 
                                                      "numeric", "numeric"))) %>% 
  rbind(read_excel("data/Kampoosa Rte 7_2011_2023.xlsx", 
                   sheet = "2013-2014", col_types = c("date", 
                                                      "numeric", "numeric"))) %>% 
  rbind(read_excel("data/Kampoosa Rte 7_2011_2023.xlsx", 
                   sheet = "2014-2015", col_types = c("date", 
                                                      "numeric", "numeric"))) %>% 
  rbind(read_excel("data/Kampoosa Rte 7_2011_2023.xlsx", 
                   sheet = "2015-2016", col_types = c("date", 
                                                      "numeric", "numeric"))) %>% 
  rbind(read_excel("data/Kampoosa Rte 7_2011_2023.xlsx", 
                   sheet = "2016-2017", col_types = c("date", 
                                                      "numeric", "numeric"))) %>% 
  rbind(read_excel("data/Kampoosa Rte 7_2011_2023.xlsx", 
                   sheet = "2017-2018", col_types = c("date", 
                                                      "numeric", "numeric"))) %>% 
  rbind(read_excel("data/Kampoosa Rte 7_2011_2023.xlsx", 
                   sheet = "2018-2019", col_types = c("date", 
                                                      "numeric", "numeric"))) %>% 
  rbind(read_excel("data/Kampoosa Rte 7_2011_2023.xlsx", 
                   sheet = "2019-2020", col_types = c("date", 
                                                      "numeric", "numeric"))) %>% 
  rbind(read_excel("data/Kampoosa Rte 7_2011_2023.xlsx", 
                   sheet = "2022-2023", col_types = c("date", 
                                                      "numeric", "numeric"))) %>% 
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
salt_fromI90 <- kampoosaI90_full %>% 
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
salt_fromRt7 <- kampoosaRt7_full %>% 
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



monthly_applied_cl <- total_applied_cl %>% 
  mutate(date = as.Date(date),
         year = lubridate::year(date),
         month = lubridate::month(date),
         monthYear = as.yearmon(paste(year, month), "%Y %m")) %>% 
  group_by(monthYear, year) %>% 
  summarise(month_cl_wetland = sum(total_cl_wetland, na.rm = TRUE),
            month_cl_kb150 = sum(total_cl_kb150, na.rm = TRUE),
            month_cl_kb300 = sum(total_cl_kb300, na.rm = TRUE),
            month_cl_kb100 = sum(total_cl_kb100, na.rm = TRUE))



kb100_road <- 14797/1609.344
kb150_road <- 8256/1609.344
kb300_road <- 1112/1609.344
wetland_road <- 5428.66/1609.344

annual_cl_application <- monthly_applied_cl %>% 
  group_by(year) %>% 
  summarise(
  `Total Applied Cl KB175 (kg)` = sum(month_cl_wetland),
`Total Applied Cl KB150 (kg)` = sum(month_cl_kb150),
`Total Applied Cl KB300 (kg)` = sum(month_cl_kb300),
`Total Applied Cl KB100 (kg)` = sum(`Total Applied Cl KB175 (kg)`, `Total Applied Cl KB150 (kg)`, `Total Applied Cl KB300 (kg)`)) %>% 
  mutate(`Total Applied Cl KB100 - Normalised (kg)`= `Total Applied Cl KB100 (kg)`/kb100_road,
         `Total Applied Cl KB150 - Normalised (kg)`= `Total Applied Cl KB150 (kg)`/kb150_road,
         `Total Applied Cl KB300 - Normalised (kg)`= `Total Applied Cl KB300 (kg)`/kb300_road,
         `Total Applied Cl KB175 - Normalised (kg)`= `Total Applied Cl KB175 (kg)`/wetland_road)




annual_cl_application_v2 <- annual_cl_application %>% 
  mutate(across(where(is.numeric) & !matches("year"), ~ round(./1000) * 1000)) 

annual_cl_application_v3 <- annual_cl_application_v2 %>% 
  filter(year < 2020 & year > 2011)

setDT(annual_cl_application_v3)  

annual_cl_application_v3[, percentage_difference := (`Total Applied Cl KB100 - Normalised (kg)` - shift(`Total Applied Cl KB100 - Normalised (kg)`, fill = `Total Applied Cl KB100 - Normalised (kg)`[1])) / shift(`Total Applied Cl KB100 - Normalised (kg)`, fill = `Total Applied Cl KB100 - Normalised (kg)`[1]) * 100]

# calculate the percent increase in application between (2012-2017 mean) and 2018 & 2019
mean_2012_2017 <- annual_cl_application_v3 %>% 
  filter(year < 2018) 


mean_2012_2019 <- annual_cl_application_v3 %>% 
  filter(year < 2020) 

mean_2018_2019 <- annual_cl_application_v3 %>% 
  filter(year < 2020 & year > 2017)

kb100_mean_2012_2017 <- mean(mean_2012_2017$`Total Applied Cl KB100 (kg)`)
kb100_mean_2012_2019 <-  mean(mean_2012_2019$`Total Applied Cl KB100 (kg)`)
kb100_mean_2018_2019 <-  mean(mean_2018_2019$`Total Applied Cl KB100 (kg)`)
kb100_2018 <- annual_cl_application_v3$`Total Applied Cl KB100 (kg)`[annual_cl_application_v3$year == 2018]
kb100_2019 <- annual_cl_application_v3$`Total Applied Cl KB100 (kg)`[annual_cl_application_v3$year == 2019]


# Calculate percent increase
per_change_2018 <- ((kb100_2018 - kb100_mean_2012_2017) / kb100_mean_2012_2017) * 100
per_change_2019 <- ((kb100_2019 - kb100_mean_2012_2017) / kb100_mean_2012_2017) * 100
  
# percent change between the 2012 - 2019 and 2018 - 2019                                     
per_change_full <- ((kb100_mean_2018_2019 - kb100_mean_2012_2017) / kb100_mean_2012_2017) * 100




application_plot <- annual_cl_application %>% 
  select(year, `Total Applied Cl KB175 (kg)`, `Total Applied Cl KB150 (kg)`, `Total Applied Cl KB300 (kg)`, `Total Applied Cl KB100 (kg)`) %>% 
  pivot_longer(!year, names_to = "watershed", values_to = "cl_mass") %>% 
  filter(year < 2020 & year > 2011)

path = here::here()

pdf(file = paste(path,'/results/visuals/', 'Fig5.pdf',sep=""),
    width = 8.5,
    height = 5.5)


ggplot(application_plot, aes(x = year, y = cl_mass, fill = watershed), color = 'black') +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5, color = "black") +
  geom_bar(stat = "identity", position = "dodge", alpha = 0, color = "black", size = 0.5) +
  scale_fill_manual(values = c("red4", "grey40",  "#000435", "darkgoldenrod"), labels = c("KB100", "KB150", "KB300", "Wetland Region"))+ 
  labs(title = "",
       x = "Year",
       y = "Annual Chloride Application (kg)") +
  #scale_fill_discrete(name = "Watershed")+
  scale_x_discrete(limits = unique(application_plot$year))+
  scale_y_continuous(breaks = seq(0,800000,200000))+
  theme_clean()+
  theme(axis.title.x = element_blank(),
        axis.text = element_text(size = 14,  color = "black"),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_text(size = 15),
        legend.text = element_text(size=14),
        #legend.position=c(.15,.91),
        strip.background = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white", colour = "white", size =.2),
        panel.background = element_rect(fill = "white", colour = "black", size =1))

dev.off()

write_csv(annual_cl_application_v2, "results/tables/historic_salt_application_v2.csv")




