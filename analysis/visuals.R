library(tidyverse)
#library(ggthemr)
library(ggthemes)
library("ggsci")
library(ggpubr)
library(patchwork)
library(plotly)
library(randomcoloR)
library(zoo)
library(gridExtra)
library(ggpattern)
library(glue)
library(ggtext)
library(tibble)
library(grid)
library(directlabels)
library(cowplot)
library(ggpmisc)
library(ggrepel)
library(scales)
library(sf)
library(usmap)
library(maps)
library(maptools)
library(rgdal)
library(latex2exp)
latex2exp_examples(cex = 1.3)
options(scipen=999)

# flux and salt application data
dailyScaleFull <- read_csv("results/tables/daily_values.csv") %>% 
  #replace_na(list(cumulAppCl = 0)) %>% 
  distinct(date, .keep_all = TRUE) %>% 
  filter(date < "2020-10-01") %>% 
  mutate(flowRatio = ifelse(dailyCFS_KB300  > dailyCFS_KB175_corrected, "true", "false"))


monthlyFull <- read_csv("results/tables/monthly_values.csv") %>% 
  mutate(monthYear = as.yearmon(monthYear)) # change monthYear back to yearmon variable


# groundwater chemistry data
wellsPivoted <- read_csv("results/tables/wellsPivoted.csv")


safe_colorblind_palette <- c("#88CCEE",  "grey", "navy", "#117733", "#999933", "#AA4499", "black", 
                             "#44AA99", "#999933", "#882255", "#661100", "orange")



monthlyFull_tve_export <- monthlyFull %>% 
  pivot_longer(cols = c(monthSaltapplied, monthlyCl_kb175_corrected), names_to = 'salt_name', values_to = 'salt_value')

### graph showing salt application i_90, export (KB175) and monthly accum
monthyear <- as.data.frame(monthlyFull$monthYear) %>% 
  filter(row_number() %% 2 != 1) %>% 
  dplyr::rename(monthYear = `monthlyFull$monthYear`)


# CHLORIDE ACCUMALATION, APPLICATION AND EXPORT

clAppl_exp_accum <- ggplot(monthlyFull_tve_export, aes(x = monthYear))+
  geom_bar(aes(y=salt_value, fill = salt_name),stat = "identity", position = position_dodge(width =0.035), width =.1, color = 'black')+
  geom_vline(xintercept =as.yearmon(c("Oct 2019", "Oct 2018", "May 2020")), colour = "grey20", alpha = .9, size = .5, linetype = "dashed") +
  geom_line(aes(y = Mfen_corrected/2.5), color = "brown4", alpha = .9, size=.9)+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 24),
        axis.text.y = element_text(size = 14),
        legend.text =element_text(size = 13), 
        legend.position=c(.9,.943),
        legend.background = element_rect(fill = "white", colour = "grey", size =.2),
        panel.background = element_rect(fill = "white", colour = "black", size =1))+
  scale_color_manual(values = "brown4", labels = '')+
  scale_fill_manual(values = c("gray90", "steelblue3"), labels = c("Outflux (KB175)", "Applied to I-90"))+ ##56B4E9
  scale_x_yearmon(breaks = monthyear$monthYear, expand = expansion(mult = c(0, 0)))+
  scale_y_continuous(breaks = seq(0,140000,20000), 
                     expand = expansion(mult = c(0, .15),add = c(0, 0)),
                     sec.axis = sec_axis(~.*2.5, breaks = seq(0,350000,50000), name = "Chloride Accumulation in fen (kg)"))+
  annotate("text", as.yearmon("Apr 2018"), y = 105000, label = "2018", colour = "black", size = 6)+
  annotate("text", as.yearmon("Apr 2019"), y = 105000, label =  "2019", colour = "black", size = 6)+
  annotate("text", as.yearmon("Feb 2020"), y = 105000, label =  "2020", colour = "black", size = 6)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.text.y.right = element_text(size = 15, color = "brown4"),
        axis.line.y.right = element_line(color = "brown4"),
        axis.ticks.y.right = element_line(color = "brown4"),
        axis.title.y.right=element_text(colour="brown4", angle = 90))+
  labs(y = "Chloride Mass (kg)",
       x = "")


## SEASON CHANGES IN WATER MOVEMENT BETWEEN FEN AND BROOK

## discharge graph to explain movement of water between fen and brook
## graph showing daily mean discharge values - explian the idea of qfen

discharge_direction<- ggplot(dailyScaleFull, aes(x= date))+
  annotation_logticks()+
  geom_line(aes(y = dailyCFS_KB175, color = "KB-175"), size = .9)+
  geom_point(aes(y = dailyCFS_KB300, color = flowRatio), size = 1.5)+
  scale_color_manual(values = c("darkgoldenrod",  "gray80","steelblue3"), labels = c("KB-300 < KB-175",  "KB-175", "KB-300 > KB-175"))+
  scale_x_date(labels = scales::date_format("%b %Y"), date_breaks = "2 month")+
  scale_y_log10(limits = c(0.01,100),
                expression("Discharge (log"["10"]*"(cfs))"),
                breaks= trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme(legend.title = element_blank(),
        axis.text = element_text(size=14),
        axis.title = element_text(size=20),
        axis.text.x = element_text(angle = 90),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        legend.position=c(.9,.92),
        legend.background = element_rect(fill = "white", color = "grey"),
        legend.key = element_rect(fill = "white"),
        legend.key.size = unit(.5, 'cm'),#,
        legend.text = element_text(size=13))+
  theme(panel.background = element_rect(fill = "white", colour = "black", size =1))+
  labs(x = "")


# GROUNDWATER CHEMISTRY - DISCHARGE RELATIONSHIP

## How does groundwater chemistry vary spatial and with time?
## Is there a relationship between discharge at the exit point and groundwater chemistry
wellsPivoted_cl <- wellsPivoted %>% 
  filter(type == "Cl_mgL",
         WellDepth_ft > 0,
         !is.na (concentration),
         Date <= "2020-09-30")  

gw_cl_discharge <- ggplot()+
  geom_point(data = wellsPivoted_cl, aes(x= Date, y = concentration, color = as.factor(WellDepth_ft)), size = 1.5)+ #used size=2.9
  geom_line(data = dailyScaleFull, aes(x = date, y = dailyCFS_KB100/.9), color = "black")+ # set scale for second y axis #grey90
  scale_color_manual(values=c("steelblue3","#E69F00",  "#999999"), labels = c("5ft", "10ft", "15ft"))+
  geom_smooth(data = wellsPivoted_cl, aes(x= Date, y = concentration, color = as.factor(WellDepth_ft)), method = "lm", size = .6, se = FALSE)+
  geom_line(size = 1)+ #used size =2
  #annotate("label", as.Date("2020-01-01"), y = 32, label = "Discharge KB-300 (cfs)", colour = "black", size = 3.8, face = "bold")+
  geom_line(data = wellsPivoted_cl, aes(x= Date, y = concentration, color = as.factor(WellDepth_ft)),size = .5)+ #used size =2
  facet_wrap(~IDgroup, nrow = 1, scales = "free")+
  scale_y_continuous(limits = c(0, 230), 
                     breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200, 225),
                     sec.axis = sec_axis(~.*.9, name = "Discharge KB-100 (cfs)", breaks = c(0,20,40,60)))+ #, breaks = c(0,20,40,60)
  scale_x_date(breaks = "4 month")+
  theme(
    axis.text.x = element_text(angle=90, size = 12),
    axis.text.y = element_text(size = 14),
    legend.key.size = unit(1, 'cm'),#,
    legend.text = element_text(size=13),
    panel.spacing = unit(1.2, "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor=element_blank(),
    axis.title.y.right = element_text(angle = 90, hjust = .02, size = 14, color = 'black'),
    axis.text.y.right = element_text(size = 15, color = "black"),
    axis.line.y.right = element_line(color = "black"),
    axis.ticks.y.right = element_line(color = "black"),
    #axis.title.y.right=element_text(colour="brown4", angle = 90))+
    #axis.title.y.left = element_text(angle = 90, hjust = .2),
    legend.title =  element_text(size=15),
    axis.title = element_text(size = 18),
    strip.background = element_blank(),
    legend.key = element_rect(fill = "white"),
    strip.text = element_text(size=18),
    legend.position=c(.95,.86),
    legend.background = element_rect(fill = "white", colour = "grey", size =.2),
    panel.background = element_rect(fill = "white", colour = "black", size =1))+
  labs(y =  expression("Cl"^-{}*" (mg/L)"), #expression("Cl^-{}* concentration (mg/L)")
       x = "",
       color = "Well depth")


## How do average mean concentration change with depth? (Compare to Rhodes and Guswa (2016))
wellsPivoted_cl_mean <- wellsPivoted %>% 
  filter(!is.na (concentration),
         Date <= "2020-09-30") %>% 
  group_by(IDgroup, depth) %>% 
  summarise(avg_cl = mean(concentration)) %>% 
  mutate(WellDepth_ft = case_when(depth == "0ft"   ~ 0,
                                  depth == "5ft"   ~ 5,
                                  depth == "10ft"   ~ 10,
                                  depth == "15ft"   ~ 15),
         avg_cl_umolL = ((avg_cl/35.453)*1000), # convert to mols, but we'll use mg/L if visual is included in paper (supplement?)
         depth_m = WellDepth_ft*0.3048)


ggplot(wellsPivoted_cl_mean, aes(x = -depth_m, y = avg_cl_umolL, color = IDgroup))+
  geom_point(size = 7)+
  geom_line(size = 2)+
  labs(y = 'Cl concentration (umol/L)',
       x = 'Well depth (m)')+
  coord_flip()

# show general trend in Cl concentrations at all the 


# DISCHARGE - MASS FLUX RELATIONSHIPS
# What drives chloride flusing/outflux?
# Is there a relationship between mass flux and discharge?
## monthly discharge and cl export
monthlyFull <- monthlyFull %>% 
  drop_na(monthYear)
labs <- as.character(monthlyFull$monthYear) # month labels


# equation of line
formula1 <- y~x

# corrected kb100 vals

dischFlux_labels <- monthlyFull %>% 
  filter(monthYear <= "Sep 2020") %>% 
  ggplot(aes(x = monthDischargeKB100, y= monthlyCl_kb100)) +
  geom_point(aes(color = Season), alpha = .7, size = 2)+
  geom_smooth(aes(fill = monthlyCl_kb100), method = "lm", color = "black",se = TRUE, formula = formula1)+
  geom_smooth(aes(fill = monthlyCl_kb100), method=lm, se=FALSE, formula=y~x-1, color = 'navy', linetype = "dashed",  alpha = .3, size = .7)+
  scale_color_manual(values = c("black", "#88CCEE", "darkgoldenrod", "red4"))+
  scale_y_continuous(breaks = seq(0,50000,10000))+
  scale_x_continuous(breaks = seq(0,8,2))+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        legend.position=c(.9,.2),
        legend.background = element_rect(fill = "white", colour = "grey", size =.2))+
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 20),
    legend.key.size = unit(.8, 'cm'),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15))+
  theme(panel.background = element_rect(fill = "white", colour = "black", size =1))+
  geom_label_repel(data = monthlyFull , aes(label = labs, color = Season), size = 4.5, show.legend = FALSE)+
  labs(x = "Monthly Discharge at KB100 (cfs)",
       y = "Monthly Chloride Mass Flux at KB100 (kilograms)")


monthlyFull %>% 
  filter(monthYear <= "Sep 2020") %>% 
  ggplot(aes(x = monthDischargeKB150, y= monthlyCl_kb150)) +
  geom_point(aes(color = Season), alpha = .7, size = 2)+
  geom_smooth(aes(fill = monthlyCl_kb150), method = "lm", color = "black",se = TRUE, formula = formula1)+
  geom_smooth(aes(fill = monthlyCl_kb150), method=lm, se=FALSE, formula=y~x-1, color = 'navy', linetype = "dashed",  alpha = .3, size = .7)+
  scale_color_manual(values = c("black", "#88CCEE", "darkgoldenrod", "red4"))+
  scale_y_continuous(breaks = seq(0,50000,10000))+
  scale_x_continuous(breaks = seq(0,8,2))+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        legend.position=c(.9,.2),
        legend.background = element_rect(fill = "white", colour = "grey", size =.2))+
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 20),
    legend.key.size = unit(.8, 'cm'),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15))+
  theme(panel.background = element_rect(fill = "white", colour = "black", size =1))+
  geom_label_repel(data = monthlyFull , aes(label = labs, color = Season), size = 4.5, show.legend = FALSE)+
  labs(x = "Discharge at KB150 (cfs)",
       y = "Chloride Mass Flux at KB150 (kilograms)")




dischFlux_nolabels <- monthlyFull %>% 
  filter(monthYear <= "Sep 2020") %>% 
  ggplot(aes(x = monthDischargeKB100, y= monthlyCl_kb100)) +
  geom_point(alpha = .7, size = 2)+
  geom_smooth(method = "lm", color = "black",se = TRUE, formula = formula1)+
  scale_y_continuous(breaks = seq(0,50000,10000))+
  scale_x_continuous(breaks = seq(0,8,2))+
  stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = 0.15,
               eq.with.lhs = "italic(hat(y))~`=`~",
               eq.x.rhs = "~italic(x)",
               formula = formula1, parse = TRUE, size = 7) +
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = "bottom",
               formula = formula1, parse = TRUE, size = 7) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        legend.position=c(.9,.2),
        legend.background = element_rect(fill = "white", colour = "grey", size =.2))+
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 20),
    legend.key.size = unit(.8, 'cm'),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15))+
  theme(panel.background = element_rect(fill = "white", colour = "black", size =1))+
  labs(x = "Monthly Discharge at KB100 (cfs)",
       y = "Monthly Chloride Mass Flux at KB100 (kilograms)")

# discharge no labels kb100, but highlight March and Nov 2018
monthlyFull_outliers <- monthlyFull %>% 
  filter(monthYear == "Mar 2018" | monthYear == "Nov 2018") 

labs2 <- as.character(monthlyFull_outliers$monthYear)

dischFlux_nolabels_outliers <- monthlyFull %>% 
  filter(monthYear <= "Sep 2020") %>% 
  ggplot(aes(x = monthDischargeKB100, y= monthlyCl_kb100)) +
  geom_point(alpha = .7, size = 2)+
  geom_point(data = monthlyFull_outliers, aes(x = monthDischargeKB100, y= monthlyCl_kb100), size = 2.5, color = "darkgoldenrod")+
  scale_y_continuous(breaks = seq(0,50000,10000))+
  scale_x_continuous(breaks = seq(0,8,2))+
  geom_label_repel(data = monthlyFull_outliers, aes(label = labs2), size = 6, show.legend = FALSE, nudge_y = -500,
                   nudge_x = .5)+
  geom_smooth(data = monthlyFull, method = "lm", color = "black",se = TRUE, formula = formula1)+
  stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = 0.15,
               eq.with.lhs = "italic(hat(y))~`=`~",
               eq.x.rhs = "~italic(x)",
               formula = formula1, parse = TRUE, size = 7) +
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = "bottom",
               formula = formula1, parse = TRUE, size = 7) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        legend.position=c(.9,.2),
        legend.background = element_rect(fill = "white", colour = "grey", size =.2))+
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 20),
    legend.key.size = unit(.8, 'cm'),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15))+
  theme(panel.background = element_rect(fill = "white", colour = "black", size =1))+
  labs(x = "Monthly Discharge at KB100 (cfs)",
       y = "Monthly Chloride Mass Flux at KB100 (kilograms)")



# AIR AND WATER TEMP, PRECIPITATION
## temperature graphs

monthlyCoeff <- 0.13
monthlyMaxRange  <- 60


air_temp <- function(water_yr){
  dailyScaleFull %>% 
    filter(waterYear == as.character(water_yr)) %>% 
    ggplot(aes(x= date))+
    geom_line(aes(y = daily_ave_air_temp, color = "Air Temp"), size = 1.5)+
    geom_line(aes(y = daily_ave_water_temp, color = "Water Temp"), size = 1.5)+
    geom_tile(aes(y = monthlyMaxRange - rain_inch/monthlyCoeff/2, # set scale for second y axis
                  height = rain_inch/monthlyCoeff, 
                  fill = 'PColor'), fill = "black")+
    scale_y_continuous(name = "Temperature (°C)",
                       limit = c(-25, monthlyMaxRange),
                       expand = c(0, 0),
                       sec.axis = sec_axis(trans = ~(.-monthlyMaxRange)*-monthlyCoeff,
                                           name = "Precipitation (inches)", breaks = c(0,1,2,3,4)))+
    theme_clean()+
    theme_classic(base_size = 24)+
    scale_color_manual(values = c("black", "grey"))+
    scale_x_date(date_breaks = "4 week")+
    theme(
      legend.title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y.right = element_text(angle = 90, hjust = 1.05),
      axis.title.y.left = element_text(angle = 90, hjust = .2),
      axis.title.y = element_text(),
      axis.text.x = element_text(angle = 90),
      legend.position = "",
      plot.title = element_text(hjust = 0.5))+
    labs(title = water_yr)
}

airtemp18 <- air_temp(water_yr = 2018)
airtemp19 <- air_temp(water_yr = 2019)

airtemp20 <- air_temp(water_yr = 2020)+
  theme(legend.justification=c(1,0),legend.position=c(1,0),
        axis.text.x = element_text(angle = 90),
        #legend.background = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(1.5, 'cm'),#,
        legend.text = element_text(size=19))


air_temp_grid <- plot_grid(airtemp18, airtemp19,airtemp20, ncol = 3, nrow=1, align = "v")

air_temp_time_series <- ggplot(dailyScaleFull, aes(x= date))+
  geom_line(aes(y = daily_ave_air_temp, color = "Air Temp"), size = 1.5)+
  geom_line(aes(y = daily_ave_water_temp, color = "Water Temp"), size = 1.5)+
  geom_tile(aes(y = monthlyMaxRange - rain_inch/monthlyCoeff/2, # set scale for second y axis
                height = rain_inch/monthlyCoeff, 
                fill = 'PColor'), fill = "black", width = 2.5)+
  scale_y_continuous(name = "Temperature (°C)",
                     limit = c(-25, monthlyMaxRange),
                     expand = c(0, 0),
                     sec.axis = sec_axis(trans = ~(.-monthlyMaxRange)*-monthlyCoeff,
                                         name = "Precipitation (inches)", breaks = c(0,1,2,3,4)))+
  scale_x_date(labels = scales::date_format("%b %Y"), date_breaks = "2 month")+
  theme_clean()+
  theme_classic(base_size = 24)+
  scale_color_manual(values = c("black", "grey"))+
  #scale_x_date(date_breaks = "4 week")+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y.right = element_text(angle = 90, hjust = 1.05),
    axis.title.y.left = element_text(angle = 90, hjust = .2),
    axis.title.y = element_text(),
    legend.justification=c(1,0),legend.position=c(1,0),
    axis.text.x = element_text(angle = 90),
    #legend.background = element_rect(fill = "white", color = "black"),
    legend.key.size = unit(1.5, 'cm'),#,
    legend.text = element_text(size=19),
    plot.title = element_text(hjust = 0.5))+
  labs(title = "")



### LOCATION MAP
## create blank MA map with dot at Kampoosa Bog

site <- data.frame(lat = 42.308301, lon = -73.323011) 

transf <- usmap_transform(site)

MainStates <- map_data("state") %>% 
  filter(region == "massachusetts",
         subregion == "main")

location <- ggplot() + 
  geom_polygon(data=MainStates, aes(x=long, y=lat), fill = "white", color = "black", size = 1.1)+
  geom_point(data = transf,
             aes(x = lon, y= lat),
             color = "steelblue3", shape = 18, fill = "black", size =18)+
  theme_bw()+
  theme_classic()+
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())


# save all figures
img <- list(air_water_temp_precipitation_time_series = air_temp_time_series,
  air_water_temp_precipitation_grid = air_temp_grid,
            location=location, 
  discharge_chloride_flux_KB100_nolabels_outliers=dischFlux_nolabels_outliers,
  discharge_chloride_flux_KB100_nolabels=dischFlux_nolabels,
  discharge_chloride_flux_KB100_labels=dischFlux_labels,
  groundwater_chloride_concentration_discharge_KB100=gw_cl_discharge, 
  discharge_direction_mvt_of_water_between_fen_and_brook=discharge_direction, 
            monthly_chloride_application_export_accumulation=clAppl_exp_accum)


to_print <- tibble(
  name = names(img), 
  img = img,
  filename = paste0(name, ".jpg"),
  path = fs::path(here::here('results/visuals', filename))
)


walk2(
  to_print$img, to_print$path,
  ~ggsave(filename = .y, plot = .x,  # width 12 and height 8
          width = 14, height = 8, dpi = 400, limitsize = FALSE) # width = 22, height = 8 for graph temp graphs
  # width = 12, height = 8 for other graphs
)
