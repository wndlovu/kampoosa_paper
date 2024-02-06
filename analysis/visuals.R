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
library(gridExtra)
library(egg)
library(hydroTSM)
#library(latex2exp)
#latex2exp_examples(cex = 1.3)
options(scipen=999)

path = here::here()


# flux and salt application data
daily_vals_full <- read_csv("results/tables/daily_values.csv") %>% 
  distinct(date, .keep_all = TRUE) 

monthlyFull <- read_csv("results/tables/monthly_values.csv") %>% 
  mutate(monthYear = as.yearmon(monthYear)) # change monthYear back to yearmon variable


# groundwater chemistry data
wellsPivoted <- read_csv("results/tables/wellsPivoted.csv")



monthlyFull_tve_export <- monthlyFull %>% 
  pivot_longer(cols = c(monthSaltappliedKB100, monthlyCl_kb100), names_to = 'salt_name', values_to = 'salt_value')

monthlyFull_tve_exportKB150 <- monthlyFull %>% 
  pivot_longer(cols = c(monthSaltappliedKB150, monthlyCl_kb150), names_to = 'salt_name', values_to = 'salt_value')


monthlyFull_tve_exportKB300 <- monthlyFull %>% 
  pivot_longer(cols = c(monthSaltappliedKB300, monthlyCl_kb300), names_to = 'salt_name', values_to = 'salt_value')


monthlyFull_tve_exportKB175 <- monthlyFull %>% 
  pivot_longer(cols = c(monthSaltappliedKB175, monthlyCl_kb175), names_to = 'salt_name', values_to = 'salt_value')


### graph showing salt application i_90, export (KB175) and monthly accum
monthyear <- as.data.frame(monthlyFull$monthYear) %>% 
  filter(row_number() %% 5 == 0) %>% 
  dplyr::rename(monthYear = `monthlyFull$monthYear`) #%>% 
#filter(row_number() <= n()-1)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Fig 2
# Meteorology Plot

monthlyCoeff <- 2.5
monthlyMaxRange  <- 60

pdf(file = paste(path,'/results/visuals/', 'Fig2.pdf',sep=""),
    width = 5.5,
    height = 4.5)
ggplot(daily_vals_full, aes(x= date))+
  geom_line(aes(y = airtempr, color = "Air (°C)"), size = 0.6)+
  geom_line(aes(y = daily_ave_water_temp, color = "Water (°C)"), size = 0.6)+
  geom_tile(aes(y = monthlyMaxRange - rain_mm/monthlyCoeff/2, # set scale for second y axis
                height = rain_mm/monthlyCoeff, 
                fill = 'PColor'), fill = "black", width = 2.5)+
  scale_y_continuous(name = "Temperature (°C)",
                     limit = c(-25, monthlyMaxRange),
                     expand = c(-0, 0),
                     sec.axis = sec_axis(trans = ~(.-monthlyMaxRange)*-monthlyCoeff,
                                         name = "Precipitation (mm)", breaks = c(0,20,40,60,80)))+
  scale_x_date(labels = scales::date_format("%b %Y"), date_breaks = "6 month")+
  scale_color_manual(values = c("grey24", "grey"))+
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y.right = element_text( size = 14, angle = 90, hjust = 1.05),
    axis.title.y.left = element_text(size = 14, angle = 90, hjust = .2),
    axis.title.y = element_text(),
    legend.justification=c(1,0),legend.position=c(1,0),
    panel.grid.major = element_blank(),
    panel.grid.minor=element_blank(),
    axis.text = element_text(color = 'black', size = 12),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(0.5, 'cm'),#,
    legend.text = element_text(size=13),
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "white", colour = "black", size = 1.2),
    axis.ticks = element_line(color = "black"))+
  labs(title = "")

dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Fig. 3
# Compare MB100 and KB300 to see the background chemistry

bck_chem <- wellsPivoted %>% 
  filter(!is.na (Cl_mgL),
         IDgroup %in% c('MB-100', 'KB-300', 'KB-100', 'KB-150')) 


# reorder the sites
bck_chem$IDgroup <- factor(bck_chem$IDgroup, levels = c('KB-150', 'KB-100', 'KB-300', 'MB-100'))


pdf(file = paste(path,'/results/visuals/', 'Fig3.pdf',sep=""),
    width = 6.5,
    height = 5.)

ggplot(data = bck_chem)+
  scale_color_manual(values=c('grey30', "red4", "#000435", 'goldenrod3'), labels = c("KB150", "KB100", "KB300", 'MB100'))+ 
  geom_line(aes(x= Date, y = Cl_mgL, color = IDgroup), method = "lm", size = 0.8, se = FALSE)+
  scale_y_continuous(breaks = seq(0,260,50))+
  scale_x_date(labels = scales::date_format("%b %Y"), breaks = "6 months") +
  theme(
    axis.text.x = element_text(size = 14, color = 'black'),
    axis.text.y = element_text(size = 14, color = 'black'),
    legend.spacing.y = unit(-0.5, 'cm'),
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(1.7,"line"),
    legend.text = element_text(size=14),
    legend.title =  element_blank(),
    axis.title = element_text(size = 15),
    legend.key = element_rect(fill = "white"),
    legend.box.background = element_blank(),
    axis.text = element_text(color = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor=element_blank(),
    legend.background = element_rect(fill = "white", colour = "white", size =.2),
    panel.background = element_rect(fill = "white", colour = "black", size =1))+
  labs(y =  expression("Chloride Concentration (mg/L)"), #expression("Cl^-{}* concentration (mg/L)")
       x = "",
       color = " ")

dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Fig.4
# GROUNDWATER CHEMISTRY - DISCHARGE RELATIONSHIP

## How does groundwater chemistry vary spatial and with time? and how is it different at the wells and KB100 
## Is there a relationship between discharge at the exit point and groundwater chemistry
wellsPivoted_cl <- wellsPivoted %>% 
  filter(
    !depth %in% c("0ft", "10ft"),
    !is.na (Cl_mgL),
    !IDgroup %in% c('MB-100','KB-150', 'KB-300', 'KB-100')) %>% 
  mutate(WellDepth_ft = coalesce(WellDepth_ft, 0)) # make surface water depths be 0 instead of NA

kb100_gw <- wellsPivoted %>% 
  filter(IDgroup == 'KB-100',
         #type == "Cl_mgL",
         !is.na (Cl_mgL)) %>% 
  select(!WellDepth_ft) 


gw_plots <- function(df){
  plot <- ggplot()+
    geom_line(data = df, aes(x= Date, y = Cl_mgL, color = IDgroup, linetype=as.factor(WellDepth_ft)), method = "lm", size = 0.8, se = FALSE)+ #add linetype to aes to include in legend
    geom_smooth(data = df, aes(x= Date, y = Cl_mgL, color = IDgroup, linetype=as.factor(WellDepth_ft)), method = "lm", size = 0.8, se = FALSE)+
    geom_line(data = kb100_gw, aes(x= Date, y = Cl_mgL, color = IDgroup), method = "lm", size = 0.8, se = FALSE, linetype = "solid")+ #method = "lm",
    scale_color_manual(values=c("#000435","goldenrod3", "grey35", "red4"), labels = c("A", "B", "C", 'KB100'))+
    geom_point(data = df, aes(x= Date, y = Cl_mgL, color = IDgroup), size = 1.5)+ #used size=2.9
    geom_point(data = kb100_gw, aes(x= Date, y = Cl_mgL, color = IDgroup), size = 1.5)+
    scale_linetype_manual(values = c("longdash", "longdash", "longdash"), guide = "none")+ # remove guide for one plot
    geom_line(size = 1)+ #used size =2
    facet_wrap(~WellDepth_ft, nrow = 1, #scales = "free",
               labeller = labeller(WellDepth_ft = 
                                     c(
                                       "5" = "5 ft",
                                       "15" = "15 ft")))+
    scale_y_continuous(limits = c(25, 230), 
                       breaks = c(50,  100, 150,  200, 250))+
    #scale_x_date(breaks = "6 month")+
    scale_x_date(labels = scales::date_format("%b %Y"), breaks = "6 months") +  # Change to scale_x_date
    
    theme(
      strip.background = element_rect(
        color="black", fill="grey"
      ))+
    theme(
      axis.text.x = element_text(size = 11, color = 'black'),
      axis.text.y = element_text(size = 14, color = 'black'),
      legend.spacing.y = unit(-0.5, 'cm'),
      legend.key.size = unit(0.9, "cm"),
      legend.key.width = unit(1.7,"line"),
      panel.spacing = unit(1.2, "lines"),
      panel.grid.major = element_blank(),
      panel.grid.minor=element_blank(),
      legend.text = element_text(size=14),
      axis.title = element_text(size = 15),
      legend.key = element_rect(fill = "white"),
      #axis.text = element_text(color = 'black'),
      strip.text = element_text(size=23),
      legend.background = element_rect(fill = "white", colour = "white", size =.2),
      panel.background = element_rect(fill = "white", colour = "black", size =1))+
    labs(y =  expression("Chloride Concentration (mg/L)"), #expression("Cl^-{}* concentration (mg/L)")
         x = "",
         color = "Site")+
    guides(color = guide_legend(override.aes = list(size = c(2.5, 2.5, 2.5, 2.5))))
  
  return(plot)
}

# 5ft and 15ft wells
pdf(file = paste(path,'/results/visuals/', 'Fig4.pdf',sep=""),
    width = 9.3,
    height = 5.5)

gw_plots(wellsPivoted_cl)

dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Fig.6 
# CHLORIDE ACCUMALATION, APPLICATION AND EXPORT

monthlyFull_tve_export$kb100_title <- "KB100"

clAppl_exp_accumKB100 <- ggplot(monthlyFull_tve_export, aes(x = monthYear))+
  
  geom_bar(aes(y=salt_value, fill = salt_name),stat = "identity", position = position_dodge(width =0.035), width =.1, color = 'black', guide = FALSE)+
  geom_line(aes(y = MfenKB100/2.5), color = "black", alpha = .9, size = .7)+ #goldenrod3 gray40
  geom_vline(xintercept =as.yearmon(c("Jan 2019")), colour = "grey20", alpha = .9, size = .5, linetype = "dashed") +
  geom_vline(xintercept =as.yearmon(c("Jan 2020")), colour = "grey20", alpha = .9, size = .5, linetype = "dashed") +
  facet_grid(. ~ kb100_title) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_text(size = 12),
        legend.key = element_rect(fill = "white", color = ''),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        legend.position = "none", 
        panel.background = element_rect(fill = "white", colour = "black", size =1))+
  scale_fill_manual(values = c("gray40", "gray90"), labels = c("Outflux", "Applied"))+ 
  scale_x_yearmon(breaks = monthyear$monthYear, expand = expansion(mult = c(0, 0)))+
  scale_y_continuous(limits = c(0, 170000),
                     breaks = c(50000, 100000, 150000),
                     expand = expansion(mult = c(0, .15),add = c(0, 0)),
                     sec.axis = sec_axis(~.*2.5,  breaks = c(100000,200000,300000,400000), name = ""))+#expression(Cl^{"-"}~"Accumulation (kg)")))+
  annotate("text", as.yearmon("Aug 2019"), y = 158000, label =  "Net Accumulation", colour = "black", size = 4.5)+
  annotate("text", as.yearmon("Feb 2018"), y = 147000, label =  ".", colour = "white", size = 0.5)+
  geom_curve(data = data.frame(x = as.yearmon("Aug 2019"), yend = 87000, xend = as.yearmon("Aug 2019"), y = 145000),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
             curvature = 0, arrow = arrow(30L, unit(0.1, "inches"),
                                          "last", "closed"),
             alpha = 1, inherit.aes = FALSE)+
  theme(axis.text = element_text(color = 'black'),
        legend.key=element_blank(),
        strip.text.x = element_text(size = 14),
        axis.title.y.right=element_text( angle = 90),
        legend.background = element_rect(colour = "transparent", fill = "white"),
        legend.key.size = unit(.7, 'cm'))+
  theme(axis.text.x = element_text(size = 12), #, angle = 45, vjust =0.9, hjust = .9
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        strip.background = element_rect(colour="black"),
        panel.background = element_rect(fill = "white", colour = "black", size = 1),
        axis.ticks = element_line(color = "black"))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 17))+
  xlab("")+
  ylab("")



# KB150
monthlyFull_tve_exportKB150$kb150_title <- "KB150"

clAppl_exp_accumKB150 <- ggplot(monthlyFull_tve_exportKB150, aes(x = monthYear))+
  geom_bar(aes(y=salt_value, fill = salt_name),stat = "identity", position = position_dodge(width =0.035), width =.1, color = 'black')+
  geom_line(aes(y = MfenKB150/2.5), color = "black", alpha = .9, size = .7)+ #goldenrod3 gray40
  geom_vline(xintercept =as.yearmon(c("Jan 2019")), colour = "grey20", alpha = .9, size = .5, linetype = "dashed") +
  geom_vline(xintercept =as.yearmon(c("Jan 2020")), colour = "grey20", alpha = .9, size = .5, linetype = "dashed") +
  facet_grid(. ~ kb150_title) +
  scale_x_yearmon(breaks = monthyear$monthYear, expand = expansion(mult = c(0, 0)))+
  scale_y_continuous(#breaks = seq(0,150000,50000), 
    limits = c(0, 170000),
    breaks = c(50000, 100000, 150000),
    expand = expansion(mult = c(0, .15),add = c(0, 0)),
    sec.axis = sec_axis(~.*2.5,  breaks = c(100000,200000,300000,400000), name = ""))+ #breaks = seq(0,350000,100000)
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        #axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, color = 'black'),
        legend.text = element_text(size=10),
        #strip.background = element_blank(),
        strip.text.x = element_text(size = 14),
        legend.key = element_rect(fill = "white", color = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        legend.position = "none", 
        panel.background = element_rect(fill = "white", colour = "black", size =1))+
  scale_fill_manual(values = c("gray40", "gray90"), labels = c("Outflux", "Applied"))+ ##56B4E9
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, color = 'white'),
        axis.ticks.x = element_blank(),
        legend.key=element_blank(),
        axis.text= element_text(size = 12),
        strip.background = element_rect(colour="black"),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.background = element_rect(fill = "transparent"),
        axis.title.y.right=element_text( angle = 90))+ 
  theme(plot.title = element_text(hjust = 0.5, size = 17))+
  xlab("")+
  ylab("")


#kb300
monthlyFull_tve_exportKB300$kb300_title <- "KB300"

clAppl_exp_accumKB300 <- ggplot(monthlyFull_tve_exportKB300, aes(x = monthYear))+
  geom_bar(aes(y=salt_value, fill = salt_name),stat = "identity", position = position_dodge(width =0.035), width =.1, color = 'black')+
  geom_line(aes(y = MfenKB300/2.5), color = "black", alpha = .9, size = .7)+ #goldenrod3 gray40
  geom_vline(xintercept =as.yearmon(c("Jan 2019")), colour = "grey20", alpha = .9, size = .5, linetype = "dashed") +
  geom_vline(xintercept =as.yearmon(c("Jan 2020")), colour = "grey20", alpha = .9, size = .5, linetype = "dashed") +
  facet_grid(. ~ kb300_title) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        #axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 12, color = 'black'),
        #legend.text =element_text(size = 13), 
        legend.text = element_text(size=12),
        #strip.background = element_blank(),
        legend.key = element_rect(fill = "white", color = 'white'),
        #legend.key.size = 
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        legend.key.size = unit(.5, 'cm'),
        strip.text.x = element_text(size = 14),
        strip.background = element_rect(colour="black"),
        legend.position=c(.1,.925),
        panel.background = element_rect(fill = "white", colour = "black", size =1))+
  scale_fill_manual(values = c("gray40", "gray90"), labels = c("Outflux", "Applied"))+ ##56B4E9
  scale_x_yearmon(breaks = monthyear$monthYear, expand = expansion(mult = c(0, 0)))+
  scale_y_continuous(#breaks = seq(0,150000,50000), 
    limits = c(0, 170000),
    breaks = c(50000, 100000, 150000),
    expand = expansion(mult = c(0, .15),add = c(0, 0)),
    sec.axis = sec_axis(~.*2.5,  breaks = c(100000,200000,300000,400000), name = ""))+ #name = expression(Cl^{"-"}~"Accumulation (kg)")))+
  #annotate("text", as.yearmon("July 2018"), y = 80000, label = "2018", colour = "black", size = 4)+
  #annotate("text", as.yearmon("July 2019"), y = 80000, label =  "2019", colour = "black", size = 4)+
  annotate("text", as.yearmon("Feb 2018"), y = 147000, label =  ".", colour = "white", size = 0.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, color = 'white'),
        axis.ticks.x = element_blank(),
        legend.key=element_blank(),
        axis.text= element_text(size = 10.5),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.background = element_rect(fill = "transparent"),
        
        axis.title.y.right=element_text( angle = 90))+ #colour="gray40",
  
  theme(plot.title = element_text(hjust = 0.5, size = 17))+
  xlab("")+
  ylab("")




pdf(file = paste(path,'/results/visuals/', 'Fig6.pdf',sep=""),
    width = 6.9, #7
    height = 10) #10


cl_accum <- plot_grid(clAppl_exp_accumKB300, NULL, clAppl_exp_accumKB150, NULL, clAppl_exp_accumKB100,
                      ncol = 1, nrow=5, align = 'vh', rel_heights = c(1, -0.15 , 1, -.15, 1)) #, 
y.grob <- textGrob(expression('Chloride Applied and Outflux (kg/month)'), 
                   gp=gpar(col="black", fontsize=18), rot=90)
y2.grob <- textGrob(expression("Chloride Accumulation (kg)"), 
                    gp=gpar(col="black", fontsize=18), rot=90)
grid.arrange(arrangeGrob(cl_accum, left = y.grob, right = y2.grob ))

dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Fig. 7 
# monthly change in water storage for each watershed

pdf(file = paste(path,'/results/visuals/', 'Fig7.pdf',sep=""),
    width = 6.5, #7
    height = 4.5)

ggplot(monthlyFull, aes(x = monthYear))+
  geom_line(aes(y = waterKB100,  color = 'monthWaterStorage_kb100'), size = 0.7)+
  geom_point(aes(x = as.yearmon("Jan 2018"), y = 800000), shape = 21, fill = "red4", color = "black", size = 4) +
  scale_x_yearmon(breaks = monthyear$monthYear, expand = expansion(mult = c(0, 0)))+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = c("black"), labels = c("KB100"))+
  #annotate("text", as.yearmon("May 2018"), y = 800000, label =  "Abitrary initial storage", colour = "red4", fill = 'green', size = 2.5)+
  geom_label(aes( x=as.yearmon("June 2018"), y=800000, label='Arbitrary initial storage'),            
             color="red4", 
             size=3.8 , fontface="bold" )+
  theme(axis.text = element_text(size = 14,  color = "black"),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_text(size = 15),
        legend.text = element_text(size=15),
        legend.position=c(.89,.1),
        strip.background = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white", colour = "white", size =.2),
        panel.background = element_rect(fill = "white", colour = "black", size =1))+
  ylab(expression('Water Storage (m3)')) 

dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Fig. 8 
# flow duration curves
kb100_area <- 4653086.5 # m2
kb150_area <- 1667472.1 # m2
kb300_area <- 964544.3 # m2

meankb100 <- mean(((daily_vals_full$totalDischarge_KB100)/kb100_area)*1000)
medkb100 <- median(((daily_vals_full$totalDischarge_KB100)/kb100_area)*1000)

meankb150 <- mean(((daily_vals_full$totalDischarge_KB150)/kb150_area)*1000)
medkb150 <- median(((daily_vals_full$totalDischarge_KB150)/kb150_area)*1000)

meankb300 <- mean(((daily_vals_full$totalDischarge_KB300)/kb300_area)*1000)
medkb300 <- median(((daily_vals_full$totalDischarge_KB300)/kb300_area)*1000)

# save figure 
pdf(file = paste(path,'/results/visuals/', 'Fig8.pdf',sep=""),
    width = 9,
    height = 8)
kb100fdc = fdc(((daily_vals_full$totalDischarge_KB100)/kb100_area)*1000, new=TRUE,
               # lQ.thr=0.9, hQ.thr=0.1,
               yat=c(0.01, 0.1, 1, 10), 
               plot=TRUE, log="y",col="red4",
               main = ' ',
               ylab="Daily Discharge (mm/day)",
               lty=0.1, cex=1.2,
               cex.axis=1.5, cex.lab=1.5, 
               leg.cex=0, leg.pos="bottom",
               verbose=FALSE,
               thr.shw=FALSE) 
kb150fdc = fdc(((daily_vals_full$totalDischarge_KB150)/kb150_area)*1000, new=FALSE,
               col="grey40",
               lty=.1, cex=1.2, 
               #title = ' ',
               cex.axis=1.5, cex.lab=1.5, 
               leg.cex=1, leg.pos="bottom",
               verbose=FALSE,
               thr.shw=FALSE)  
kb300fdc = fdc(((daily_vals_full$totalDischarge_KB300)/kb300_area)*1000, new=FALSE, 
               col="#000435",
               lty=.1, cex=1.2,
               #title = ' ',
               cex.axis=1.5, cex.lab=1.5, #leg.txt="KB130",
               leg.cex=1, leg.pos="bottom",
               verbo.se=FALSE,
               thr.shw=FALSE)
grid(nx = NULL, ny = NULL, col = "grey80", lty =21, lwd=0.1, equilogs =
       TRUE)
#axis(side = 2, lty = 0, lwd = 0, cex=1.5, at = c(0.01, 0.1, 1, 10))

legend(0.25, 30,  c(paste("KB150: mean =", round(meankb150, 2), "mm/day, median =", round(medkb150, 1), "mm/day"),
                    paste("KB100: mean =", round(meankb100, 1), "mm/day, median =", round(medkb100, 1), "mm/day"),
                    paste("KB300: mean =", round(meankb300, 1), "mm/day, median =", round(medkb300, 1), "mm/day")),
       col=c("grey40","red4","blue4"), y.intersp=0.9,cex=1.25,
       lty = c(1, 1, 1),  box.lwd = 1,box.col = "white",bg = "white",
       lwd=c(3,3,3),
       seg.len = c(2, 2, 2))

dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Fig. 9, Fig. S1, Fig. S7
# plot the flow - concentration relationship for KB100 
season_labels <- c(`autumm` = "Autumn",
                   `spring` = "Spring",
                   `summer` = "Summer",
                   `winter` = "Winter")

daily_vals_full <- daily_vals_full %>% 
  mutate(year = as.numeric(year),
         season = as.character(season))

faceted_plots <- function(x, y){
  plot <- ggplot(daily_vals_full, aes(x = x, y = y))+
    geom_point(aes(color = season),size = 2.5)+
    geom_point(shape = 1, size = 2.5, colour = "black", alpha = .9)+
    #scale_color_manual(values = c("darkgoldenrod", "#000435", "red4", "grey50"))+
    scale_color_manual(values = c("grey40", "grey40", "grey40", "grey40"))+
    facet_wrap(~season, labeller = as_labeller(season_labels))+ # labeller = as_labeller(season_labels),
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~")), color = "black", geom = "label", size = 2.8,
             p.accuracy = 0.05, r.accuracy = 0.01, label.x = -0.5,
             label.y.npc="top", label.x.npc = "right")+
    theme(legend.position = "none",
          strip.text.x  = element_text(
            size = 12
          ),
          axis.text = element_text(color = 'black', size = 12),
          axis.title = element_text(size = 15),
          panel.grid.major = element_blank(),
          panel.grid.minor=element_blank(),
          panel.background = element_rect(fill = "white", colour = "black", size =1),
          plot.title = element_text(hjust = 0.5, size=12))+
    theme(
      strip.background = element_rect(
        color="black", fill="grey"
      )
    )+
    ylab('Chloride flux concentration (mg/L)')
  
  return(plot)
}


kb100_conc <- faceted_plots(daily_vals_full$dailyDischarge_KB100, y = daily_vals_full$fluxClKB100)+
  geom_smooth(method = 'lm', color = 'grey30', se = FALSE, size = 0.5)+
  scale_x_log10(limits = c(0.001,10),
                expression("Discharge (log"["10"]*"m"^{"3"}~"/s)"),
                breaks= trans_breaks("log10", function(x) 10^x, n = 4),
                labels = trans_format("log10", math_format(10^.x)))+
  ggtitle("KB100 Flux Concentration")

kb150_conc <- faceted_plots(daily_vals_full$dailyDischarge_KB150, y = daily_vals_full$fluxClKB150)+
  geom_smooth(method = 'lm', color = 'grey30', se = FALSE, size = 0.5)+
  scale_x_log10(limits = c(0.01,10),
                expression("Discharge (log"["10"]*"m"^{"3"}~"/s)"),
                breaks= trans_breaks("log10", function(x) 10^x, n = 4),
                labels = trans_format("log10", math_format(10^.x)))+
  ggtitle("KB150 Flux Concentration")

kb300_conc <- faceted_plots(daily_vals_full$dailyDischarge_KB300, y = daily_vals_full$fluxClKB300)+
  geom_smooth(method = 'lm', color = 'grey30', se = FALSE, size = 0.5)+
  scale_x_log10(limits = c(0.01,10),
                expression("Discharge (log"["10"]*"m"^{"3"}~"/s)"),
                breaks= trans_breaks("log10", function(x) 10^x, n = 4),
                labels = trans_format("log10", math_format(10^.x)))+
  ggtitle("KB300 Flux Concentration")



kb100_flux_f <- faceted_plots(daily_vals_full$dailyDischarge_KB100, y = daily_vals_full$totalClkg_KB100)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~")), color = "black", geom = "label", size = 3.2,
           p.accuracy = 0.05, r.accuracy = 0.01, label.x = 0.85,
           label.y.npc="top", label.x.npc = "left")+
  scale_x_continuous(limits = c(0,1.6))+
  scale_y_continuous(limits = c(0,(max(daily_vals_full$totalClkg_KB100)+1000)))+
  ylab('Daily Chloride Mass Flux (kg)') +
  xlab("Daily Discharge (m"^{"3"}~"/s)")


kb150_flux_f <- faceted_plots(daily_vals_full$dailyDischarge_KB150, y = daily_vals_full$totalClkg_KB150)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~")), color = "black", geom = "label", size = 3.2,
           p.accuracy = 0.05, r.accuracy = 0.01, label.x = 0.85,
           label.y.npc="top", label.x.npc = "left")+
  scale_x_continuous(limits = c(0,1.6))+
  ylab('Chloride Mass Flux (kg)') +
  xlab("Discharge (m"^{"3"}~"/s)")+
  ggtitle("KB150 Mass Flux")


kb300_flux_f <- faceted_plots(daily_vals_full$dailyDischarge_KB300, y = daily_vals_full$totalClkg_KB300)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~")), color = "black", geom = "label", size = 3.2,
           p.accuracy = 0.05, r.accuracy = 0.01, label.x = 0.85,
           label.y.npc="top", label.x.npc = "left")+
  scale_x_continuous(limits = c(0,1.6))+
  ylab('Chloride Mass Flux (kg)') +
  xlab("Discharge (m"^{"3"}~"/s)")+
  ggtitle("KB300 Mass Flux")



img <- list(FigS7_KB100 = kb100_conc,
            FigS7_KB150 = kb150_conc,
            FigS7_KB300 = kb300_conc,
            Fig9 = kb100_flux_f,
            FigS1_KB150 = kb150_flux_f,
            FigS1_KB300 = kb300_flux_f
)


to_print <- tibble(
  name = names(img), 
  img = img,
  filename = paste0(name, ".pdf"),
  path = fs::path(here::here('results/visuals', filename))
)


walk2(
  to_print$img, to_print$path,
  ~ggsave(filename = .y, plot = .x,  # width 12 and height 8
          width = 6, height = 5.5, limitsize = FALSE) # width = 22, height = 8 for graph temp graphs
  # width = 12, height = 8 for other graphs
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Fig. 10
# flux concentration in the 3 watershed

kb100conc_mean = mean(daily_vals_full$fluxClKB100, na.rm = TRUE)
kb100conc_med = median(daily_vals_full$fluxClKB100, na.rm = TRUE)

kb150conc_mean = mean(daily_vals_full$fluxClKB150, na.rm = TRUE )
kb150conc_med = median(daily_vals_full$fluxClKB150, na.rm = TRUE)

kb300conc_mean = mean(daily_vals_full$fluxClKB300, na.rm = TRUE)
kb300conc_med = median(daily_vals_full$fluxClKB300, na.rm = TRUE)


pdf(file = paste(path,'/results/visuals/', 'Fig10.pdf',sep=""),
    width = 7,
    height = 5.5)

ggplot(daily_vals_full)+
  geom_line(aes(x = date, y = fluxClKB300 , color = 'KB300'), size = 0.8)+
  
  geom_line(aes(x = date, y = fluxClKB100, color = 'KB100'), size = 0.8)+
  geom_line(aes(x = date, y = fluxClKB150 , color = 'KB150'), size = 0.8)+
  
  scale_color_manual(values = c("red4","grey40","#000435"),
                     labels = c( paste("KB100: mean =", round(kb100conc_mean, 0), "mg/L, median =", round(kb100conc_med, 0), "mg/L"),
                                 paste("KB150: mean =", round(kb150conc_mean, 0), "mg/L, median =", round(kb150conc_med, 0), "mg/L"),
                                 
                                 paste("KB300: mean =", round(kb300conc_mean, 0), "mg/L, median =", round(kb300conc_med, 0), "mg/L")
                     ))+
  scale_x_date(labels = scales::date_format("%b %Y"), date_breaks = "6 month") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        axis.text = element_text(size = 14, color = 'black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size=14),
        legend.position=c(.6,.9),
        strip.background = element_blank(),
        legend.key = element_rect(fill = "white", size = 4),
        legend.background = element_rect(fill = "white", colour = "white", size =.2),
        legend.spacing.y = unit(-0.1, 'cm'),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(1.7,"line"),
        panel.background = element_rect(fill = "white", colour = "black", size =1))+
  ylab('Chloride Flux Concentration (mg/L)')
dev.off() 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Fig. 11 
# Cl mass flux normalised by salted lane meters
# road lengths in m
kb100_road_m <- 14797
kb150_road_m <- 8256
kb300_road_m <- 1112


kb100norm_mean = mean(daily_vals_full$totalClkg_KB100/kb100_road_m)
kb100norm_med = median(daily_vals_full$totalClkg_KB100/kb100_road_m)

kb150norm_mean = mean(daily_vals_full$totalClkg_KB150/kb150_road_m)
kb150norm_med = median(daily_vals_full$totalClkg_KB150/kb150_road_m)

kb300norm_mean = mean(daily_vals_full$totalClkg_KB300/kb300_road_m)
kb300norm_med = median(daily_vals_full$totalClkg_KB300/kb300_road_m)

pdf(file = paste(path,'/results/visuals/', 'Fig11.pdf',sep=""),
    width = 7,
    height = 5)

ggplot(daily_vals_full)+
  geom_line(aes(x = date, y = totalClkg_KB150/kb150_road_m , color = 'blue'), size = 0.6)+
  geom_line(aes(x = date, y = totalClkg_KB100/kb100_road_m, color = 'red'), size = 0.6, linetype = 1342)+
  
  #geom_line(aes(x = date, y = totalClkg_KB300/kb300_road_m , color = 'yellow'), size = 0.5)+
  scale_color_manual(values = c("grey40", "red4", "blue4"), labels = c(paste("KB150: mean =", round(kb150norm_mean, 3), "kg/day/m, median =", round(kb150norm_med, 3), "kg/day/m"),
                                                                       paste("KB100: mean =", round(kb100norm_mean, 3), "kg/day/m, median =", round(kb100norm_med, 3), "kg/day/m"),
                                                                       
                                                                       paste("KB300: mean =", round(kb300norm_mean, 3), "kg/day/m, median =", round(kb300norm_med, 3), "kg/day/m")))+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        axis.text = element_text(size = 14, color = 'black'),
        axis.title = element_text(size = 15),
        legend.text = element_text(size=14),
        legend.position=c(.55,.9),
        strip.background = element_blank(),
        legend.key = element_rect(fill = "white", size = 4),
        legend.background = element_rect(fill = "white", colour = "white", size =.2),
        legend.spacing.y = unit(-0.1, 'cm'),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(1.7,"line"),
        panel.background = element_rect(fill = "white", colour = "black", size =1))+
  ylab('Chloride Mass Flux per Lane-Length (kg/day/m)')

dev.off()    

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Fig. S2
# SEASON CHANGES IN WATER MOVEMENT BETWEEN FEN AND BROOK

## discharge graph to explain movement of water between fen and brook
## graph showing daily mean discharge values - explian the idea of qfen

# kb300> kb100 df
ratios_df <- daily_vals_full %>% 
  filter(flowRatio == 'TRUE')


pdf(file = paste(path,'/results/visuals/', 'FigS2.pdf',sep=""),
    width = 6.5, #7
    height = 5)


ggplot(daily_vals_full, aes(x = date)) +
  annotation_logticks() +
  geom_line(aes(y = totalDischarge_KB100, color = "KB-100"), size = 0.6, linetype=1) +
  geom_line(aes(y = totalDischarge_KB300, color = flowRatio), size = 0, alpha = 0.001) +
  geom_line(aes(y = totalDischarge_KB300), linetype=1, size = 0.6, color = '#000435') +
  geom_point(data = ratios_df, aes(x = date, y = totalDischarge_KB300), size = 2.2, color = "black") +
  geom_point(data = ratios_df, aes(x = date, y = totalDischarge_KB300, color = flowRatio), size = 1.9) +
  scale_color_manual(values = c("#000435", "red4", "grey"), labels = c("KB300",  "KB100", "KB300 > KB100")) +
  scale_x_date(labels = scales::date_format("%b %Y"), date_breaks = "6 month") +
  scale_y_log10(limits = c(100, 350000),
                name = expression("Discharge (log"[10]*"m"^{"3"}~"/day)"),
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme(legend.title = element_blank(),
        axis.text = element_text(size = 14, color = 'black'),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(hjust = 0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.spacing.y = unit(-0.8, 'cm'),
        legend.key.width = unit(1,"cm"),
        legend.text = element_text(size = 14),
        legend.position = c(.8, .85),
        strip.background = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white", colour = "white", size = 0.2),
        panel.background = element_rect(fill = "white", colour = "black", size = 1)) +
  labs(x = "") +
  guides(color = guide_legend(override.aes = list(size = c(0, 0, 2.5))))


dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Fig. S4
# Surface wells
# surface water concentrations at the wells
surface_wells<- wellsPivoted %>% 
  filter(depth  == "0ft")


pdf(file = paste(path,'/results/visuals/', 'FigS4.pdf',sep=""),
    width = 6,
    height = 5)

gw_plots(surface_wells)+
  facet_wrap(~'')+
  facet_wrap(~WellDepth_ft, nrow = 1, #scales = "free",
             labeller = labeller(WellDepth_ft = 
                                   c(
                                     "0" = "Surface")))+
  scale_y_continuous(limits = c(0, 350), 
                     breaks = c(50,  100, 150,  200, 250, 300))

dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Fig. S6 
#Monthly precipitation, ET, discharge, and water in storage for KB100

pdf(file = paste(path,'/results/visuals/', 'FigS6.pdf',sep=""),
    width = 6.5, #7
    height = 5)

ggplot(monthlyFull, aes(x = monthYear))+
  geom_bar(aes(y=totalmonthPrecip),stat = "identity",  fill = 'grey80', color = 'black', width = 0.06)+
  geom_line(aes(y = discharge_kb100_mm, color = 'monthDischargeKB100'), size = 0.9)+
  #geom_point(aes(y = discharge_kb100_mm), shape=21, fill="red4", color="black", size=2)+ 
  geom_line(aes(y = discharge_kb150_mm, color = 'monthDischargeKB150'),  size = 0.9)+
  #geom_point(aes(y = discharge_kb150_mm), shape=21, fill="grey20", color="black", size=2)+ 
  geom_line(aes(y = discharge_kb300_mm, color = 'monthDischargeKB300'), size = 0.9)+
  #geom_point(aes(y = discharge_kb300_mm), shape=21, fill="blue4", color="black", size=2)+ 
  #goldenrod3
  scale_x_yearmon(breaks = monthyear$monthYear, expand = expansion(mult = c(0, 0)))+
  scale_color_manual(values = c("red4", "grey40","blue4", "black"), labels = c("KB100", "KB150", "KB300"))+
  theme(axis.text = element_text(size = 14,  color = "black"),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_text(size = 15),
        legend.text = element_text(size=15),
        legend.position=c(.88,.86),
        legend.key.width = unit(1,"cm"),
        strip.background = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white", colour = "white", size =.2),
        panel.background = element_rect(fill = "white", colour = "black", size =1))+
  ylab(expression('Precipitation and Discharge (mm/month)'))+
  guides(color = guide_legend(override.aes = list(size = 6.5)))


dev.off()

