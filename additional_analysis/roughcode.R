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
library(rgdal)
options(scipen=999)

# add monthl data df
monthlyFull <- read_csv("results/tables/monthly_values.csv") %>% 
  mutate(monthYear = as.yearmon(monthYear)) %>% # change monthYear back to yearmon variable
  filter(monthYear <= "Sep 2020") 

# flux and salt application data
dailyScaleFull <- read_csv("results/tables/daily_values.csv") %>% 
  #replace_na(list(cumulAppCl = 0)) %>% 
  distinct(date, .keep_all = TRUE) %>% 
  filter(date < "2020-10-01") %>% 
  mutate(flowRatio = ifelse(dailyCFS_KB300  > dailyCFS_KB175_corrected, "true", "false"))



safe_colorblind_palette <- c("#88CCEE",  "grey", "navy", "#117733", "#999933", "#AA4499", "black", 
                             "#44AA99", "#999933", "#882255", "#661100", "orange")

# to get a better understanding of the high discharge at KB175 in May 2020, make Kb100 discharge-flux graphs,
# kb150 graphs. High discharge at kb175 could be due to extremely low discharge values at kb150

  
# best fit line equation
formula1 <- y~x 

# flux discharge at KB 100 (discharge at kb100 is relatively high in May 2020)
dischFlux_labels_kb100 <- 
  ggplot(monthlyFull, aes(x = monthDischargeKB100, y= monthlyCl_kb100)) +
  geom_point(aes(color = Season), alpha = .7, size = 2)+
  geom_smooth(aes(fill = monthlyCl_kb100), method = "lm", color = "black",se = TRUE, formula = formula1)+
  geom_smooth(aes(fill = monthlyCl_kb100), method=lm, se=FALSE, formula=y~x-1, color = 'navy', linetype = "dashed",  alpha = .3, size = .7)+
  scale_color_manual(values = c("black", "#88CCEE", "darkgoldenrod", "red4"))+
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
  labs(x = "Discharge at KB100 (cfs)",
       y = "Chloride Mass Flux at KB100 (kilograms)")


dischFlux_nolabels_kb100 <- ggplot(monthlyFull, aes(x = monthDischargeKB100, y= monthlyCl_kb100)) +
  geom_point(alpha = .7, size = 2)+
  geom_smooth(aes(fill = monthlyCl_kb100), method = "lm", color = "black",se = TRUE, formula = formula1)+
  geom_smooth(aes(fill = monthlyCl_kb100), method=lm, se=FALSE, formula=y~x-1, color = 'navy', linetype = "dashed",  alpha = .3, size = .7)+
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
  labs(x = "Discharge at KB100 (cfs)",
       y = "Chloride Mass Flux at KB100 (kilograms)")


## make graphs showing discharge-flux at KB175 original values with negatives 

dischFlux_kb175_negativeVals <-  ggplot(monthlyFull, aes(x = monthDischargeKB175, y= monthlyCl_kb175)) +
  geom_point(alpha = .7, size = 2)+
  geom_smooth(aes(fill = monthlyCl_kb175), method = "lm", color = "black",se = TRUE, formula = formula1)+
  geom_smooth(aes(fill = monthlyCl_kb175), method=lm, se=FALSE, formula=y~x-1, color = 'navy', linetype = "dashed",  alpha = .3, size = .7)+
  theme(panel.background = element_rect(fill = "white", colour = "black", size =1))+
  stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = 0.15,
               eq.with.lhs = "italic(hat(y))~`=`~",
               eq.x.rhs = "~italic(x)",
               formula = formula1, parse = TRUE, size = 4) +
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = "bottom",
               formula = formula1, parse = TRUE, size = 4) +
  labs(x = "Discharge at KB175 (cfs)",
       y = "Chloride Mass Flux at KB175 (kilograms)",
       title = "Original Calculated Values")

# make graphs showing discharge-flux at KB175 replace negatives with 0s
dischFlux_kb175_replace_negatives <- ggplot(monthlyFull, aes(x = monthDischargeKB175_corrected, y= monthlyCl_kb175_corrected))+
  geom_point(alpha = .7, size = 2)+
  geom_smooth(aes(fill = monthlyCl_kb175), method = "lm", color = "black",se = TRUE, formula = formula1)+
  geom_smooth(aes(fill = monthlyCl_kb175), method=lm, se=FALSE, formula=y~x-1, color = 'navy', linetype = "dashed",  alpha = .3, size = .7)+
  theme(panel.background = element_rect(fill = "white", colour = "black", size =1))+
  stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = 0.15,
               eq.with.lhs = "italic(hat(y))~`=`~",
               eq.x.rhs = "~italic(x)",
               formula = formula1, parse = TRUE, size = 4) +
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = "bottom",
               formula = formula1, parse = TRUE, size = 4) +
  labs(x = "Discharge at KB175 (cfs)",
       y = "",
       titles = "Replaced Negatives with 0")

# make graphs showing discharge-flux at KB175 drop negatives
dischFlux_kb175_drop_negatives <- monthlyFull %>% 
  filter(monthDischargeKB175 > 0,
         monthlyCl_kb175 > 0) %>% 
  ggplot(aes(x = monthDischargeKB175, y= monthlyCl_kb175)) +
  geom_point(alpha = .7, size = 2)+
  geom_smooth(aes(fill = monthlyCl_kb175), method = "lm", color = "black",se = TRUE, formula = formula1)+
  geom_smooth(aes(fill = monthlyCl_kb175), method=lm, se=FALSE, formula=y~x-1, color = 'navy', linetype = "dashed",  alpha = .3, size = .7)+
  theme(panel.background = element_rect(fill = "white", colour = "black", size =1))+
  stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = 0.15,
               eq.with.lhs = "italic(hat(y))~`=`~",
               eq.x.rhs = "~italic(x)",
               formula = formula1, parse = TRUE, size = 4) +
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               label.x.npc = "right", label.y.npc = "bottom",
               formula = formula1, parse = TRUE, size = 4) +
  labs(x = "Discharge at KB175 (cfs)",
       y = "",
       title = "Dropped Negatives")


discharge_flux_kb175_3_simulations <- dischFlux_kb175_negativeVals + dischFlux_kb175_replace_negatives + dischFlux_kb175_drop_negatives

# kb150 graphs (May 2020 discharge is extremely low)
dischFlux_labels_kb150 <- 
  ggplot(monthlyFull, aes(x = monthDischargeKB150, y= monthlyCl_kb150)) +
  geom_point(aes(color = Season), alpha = .7, size = 2)+
  geom_smooth(aes(fill = monthlyCl_kb150), method = "lm", color = "black",se = TRUE, formula = formula1)+
  geom_smooth(aes(fill = monthlyCl_kb150), method=lm, se=FALSE, formula=y~x-1, color = 'navy', linetype = "dashed",  alpha = .3, size = .7)+
  scale_color_manual(values = c("black", "#88CCEE", "darkgoldenrod", "red4"))+
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

# kb150 graphs (May 2020 discharge is extremely low)
dischFlux_labels_kb300 <- 
  ggplot(monthlyFull, aes(x = monthDischargeKB300, y= monthlyCl_kb300)) +
  geom_point(aes(color = Season), alpha = .7, size = 2)+
  geom_smooth(aes(fill = monthlyCl_kb300), method = "lm", color = "black",se = TRUE, formula = formula1)+
  geom_smooth(aes(fill = monthlyCl_kb300), method=lm, se=FALSE, formula=y~x-1, color = 'navy', linetype = "dashed",  alpha = .3, size = .7)+
  scale_color_manual(values = c("black", "#88CCEE", "darkgoldenrod", "red4"))+
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
  labs(x = "Discharge at KB300 (cfs)",
       y = "Chloride Mass Flux at KB300 (kilograms)")



## plot SC and discharge at a daily scale to see if there is anything happening in May 2020
daily_specific_conductance <- dailyScaleFull %>% 
  #filter(waterYear == "2018") %>% 
  ggplot(aes(x= date))+
  geom_line(aes(y = dailySC_KB100, color = "KB100"))+
  geom_line(aes(y = dailySC_KB150, color = "KB150"))+
  geom_line(aes(y = dailySC_KB300, color = "KB300"))+
  scale_color_manual(values = safe_colorblind_palette, labels = c("KB100", "KB150", "KB300"))+
  #scale_fill_tron()+
  geom_vline(xintercept = as.numeric(as.Date("2020-05-01")), colour = "red", alpha = .9, size = .5, linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2020-06-01")), colour = "red", alpha = .9, size = .5, linetype = "dashed") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 12),
        #axis.ticks.x = element_blank(),
        legend.position = "")+
  scale_x_date(date_breaks = "1 month")+
  #ylim(0.2,1.8)+
  labs(x = "",
       y = "Average Daily Specific Conductance (mS)")


daily_discharge <- dailyScaleFull %>% 
  #filter(waterYear == "2018") %>% 
  ggplot(aes(x= date))+
  geom_line(aes(y = dailyCFS_KB100, color = "KB100"))+
  geom_line(aes(y = dailyCFS_KB150, color = "KB150"))+
  geom_line(aes(y = dailyCFS_KB300, color = "KB300"))+
  scale_color_manual(values = safe_colorblind_palette, labels = c("KB100", "KB150", "KB300"))+
  #scale_fill_tron()+
  geom_vline(xintercept = as.numeric(as.Date("2020-05-01")), colour = "red", alpha = .9, size = .5, linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2020-06-01")), colour = "red", alpha = .9, size = .5, linetype = "dashed") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size =12),
        legend.title = element_blank(),
        #axis.ticks.x = element_blank(),
        #legend.position = ""
        legend.justification=c(1,1),legend.position=c(1,1),
        #legend.background = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(1.5, 'cm'),#,
        legend.text = element_text(size=19))+
  scale_x_date(date_breaks = "1 month")+
  #ylim(0.2,1.8)+
  labs(x = "",
       y = "Daily Discharge (cfs)")

daily_sc_discharge <- plot_grid(daily_specific_conductance, daily_discharge, ncol = 1, align = "v")



daily_precip <- dailyScaleFull %>% 
  #filter(waterYear == "2018") %>% 
  ggplot(aes(x= date))+
  geom_bar(aes(y = rain_inch),stat = "identity", position = position_dodge(width =0.035), color = "steelblue3")+
  geom_vline(xintercept = as.numeric(as.Date("2020-05-01")), colour = "red", alpha = .9, size = .5, linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date("2020-06-01")), colour = "red", alpha = .9, size = .5, linetype = "dashed") +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.justification=c(1,1),legend.position=c(1,1),
        #legend.background = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(1.5, 'cm'),#,
        legend.text = element_text(size=19))+
  scale_x_date(date_breaks = "1 month")+
  #ylim(0.2,1.8)+
  labs(x = "",
       y = "Daily Precipitation (inches)")



## show changes of concentration with depth
# calculate avargae concentration at each depth

#don't remove the 0ft reading
ionDataSummary <- read_excel("data/ion_data.xlsx", 
                             sheet = "GW Summary by sample location", 
                             col_types = c("date", "text", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric"))


wells2 <- ionDataSummary %>% 
  filter(WellID %in% c("AA", "AB", "AC", "BA", "BB", "BC", "CA", "CB", "CC", "AS", "BS", "CS")) %>% # remove surface water samples
  mutate(IDgroup = case_when(WellID == "AA" |WellID == "AB"|WellID ==  "AC"|WellID ==  "AS"  ~ "A",
                             WellID == "BA" |WellID == "BB"|WellID ==  "BC"|WellID ==  "BS" ~ "B",
                             WellID == "CA" |WellID == "CB"|WellID ==  "CC"|WellID ==  "CS" ~ "C"),
         Date = as.Date(Date), # change from .dttm to date
         depth = paste0(WellDepth_ft, "ft"))


wellsPivoted_cl2 <- wells2 %>% 
  pivot_longer(cols = c("Na_mgL", "Ca_mgL", "Cl_mgL", "Mg_mgL", "SO42_mgL", "K_mgL"), 
               names_to = "type", values_to = "concentration") %>% 
  filter(type == "Cl_mgL",
         !is.na (concentration),
         Date <= "2020-09-30") 


wellsPivoted_cl_mean <- wellsPivoted_cl2 %>% 
  group_by(IDgroup, depth) %>% 
  summarise(avg_cl = mean(concentration)) %>% 
  mutate(WellDepth_ft = case_when(depth == "0ft"   ~ 0,
                                  depth == "5ft"   ~ 5,
                                  depth == "10ft"   ~ 10,
                                  depth == "15ft"   ~ 15),
         avg_cl_umolL = ((avg_cl/35.453)*1000),
         depth_m = WellDepth_ft*0.3048)

ggplot(wellsPivoted_cl_mean, aes(x = -depth_m, y = avg_cl_umolL, color = IDgroup))+
  geom_point(size = 7)+
  geom_line(size = 2)+
  labs(y = 'Cl concentration (umol/L)',
       x = 'Well depth (m)')+
  coord_flip()


monthlyFull %>% 
  filter(monthYear <= "Sep 2020") %>% 
  ggplot(aes(x = monthDischargeKB100, y= monthDischargeKB300)) +
  geom_point(aes(color = Season), alpha = .7, size = 2)+
  geom_smooth(method = "lm", color = "black",se = TRUE)+
  scale_color_manual(values = c("black", "#88CCEE", "darkgoldenrod", "red4"))+
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
  labs(x = "Discharge at KB100 (cfs)",
       y = "Discharge at KB300 (cfs)")



img_add <- list(dischFlux_labels_kb300=dischFlux_labels_kb300, dischFlux_labels_kb150=dischFlux_labels_kb150, discharge_flux_kb175_3_simulations=discharge_flux_kb175_3_simulations,
                dischFlux_labels_kb100=dischFlux_labels_kb100, 
                daily_sc_discharge=daily_sc_discharge, daily_precip = daily_precip)





to_print <- tibble(
  name = names(img_add), 
  img = img_add,
  filename = paste0(name, ".jpg"),
  path = fs::path(here::here('additional_analysis/visuals', filename))
)


walk2(
  to_print$img, to_print$path,
  ~ggsave(filename = .y, plot = .x,  # width 12 and height 8
          width = 12, height = 8, dpi = 300, limitsize = FALSE) # width = 20, height = 8 for graph temp graphs
  # width = 20, height = 8 for other graphs
)
