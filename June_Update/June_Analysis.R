
# loading Libs ------------------------------------------------------------


library(tidyverse)
library(lubridate)
library(readr)
library(scales)
library(cowplot)
library(gganimate)
library(magick)
library(maps)
library(zoo)
library(patchwork)
library(ggrepel)
windowsFonts(`Roboto Condensed` = windowsFont("Roboto Condensed"))


# Loading Data ------------------------------------------------------------


NYS_Thruway_Data <- readRDS("data/NYS_Thruway_Data_Clean.rds")
locations <- read_rds("data/locations.rds")
NY <- map_data("state") %>% as_tibble() %>% filter(region=="new york")
NYS_Thruway_Data_lockdown <- NYS_Thruway_Data %>% 
  filter(month %in% c("January", "February", "March", "April", "May")) %>%
  mutate(lockdownperiod=case_when(year==2020 & Date>= "2020-03-21" ~ "Post-Lockdown Period",
                                  year==2019 & Date>= "2019-03-21" ~ "Post-Lockdown Period",
                                  TRUE~"Pre-Lockdown Period"))



# Overall traffic compared to 2019 ----------------------------------------

traffic2019vs2020 <- 
  NYS_Thruway_Data %>%
  filter((Date<="2020-07-07" & Date>="2020-01-01" | Date<="2020-07-07" & Date>="2019-01-01")) %>% 
  group_by(Date) %>% 
  summarise(Total=sum(Vehicle.Count)) %>%
  ungroup() %>% 
  mutate(day=day(Date),
         month=month(Date),
         year=year(Date)) %>% na.omit()


traffic2019vs20202019 <- traffic2019vs2020 %>% filter(year==2019)
traffic2019vs20202020 <- traffic2019vs2020 %>% filter(year==2020)
traffic2019vs2020 <- 
  left_join(traffic2019vs20202019,traffic2019vs20202020, by=c("month","day")) %>% na.omit()


traffic2019vs2020 <- 
  traffic2019vs2020 %>% 
  mutate(total.x.mean=rollmean(Total.x,k = 7,fill = NA, align = "right"),
         total.y.mean=rollmean(Total.y,k = 7,fill = NA, align = "right"),
         index=(total.y.mean/total.x.mean)*100)

(traffic_graph <- ggplot(traffic2019vs2020) +
  geom_hline(yintercept = 100, lwd=.3, linetype="dashed", color="grey70") +
  geom_point(aes(x=Date.y, y=index),size=1.24,alpha=0) +
  geom_line(aes(x=Date.y, y=index),lwd=1.25, color="#C29000") +
  geom_vline(xintercept = as.numeric(traffic2019vs2020$Date.y[59]), 
             color="grey50", lwd=1, linetype="dashed") +
  geom_vline(xintercept = as.numeric(traffic2019vs2020$Date.y[77]), 
             color="grey50", lwd=1, linetype="dashed") +
  scale_color_manual(values = c("#C29000","#0A8CC2")) +
  labs(title = "Traffic had plateaued as cases rose across the country,\nbut appears to be back on its upward trajectory",
       subtitle = "Vertical lines represent national and statewide lockdowns, respectively",
       x=NULL,
       y= "Percent of 2019 Traffic", 
       color="Vehicle Type",
       caption = "Plot: @jakepscott2020 | Data: data.ny.gov/Transportation") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = rel(1.5)),
        plot.subtitle = element_text(face = "plain", size = rel(1.1), color = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        legend.title = element_text(size = rel(1.1), face="bold"),
        legend.text = element_text(size=rel(.9)),
        legend.position = "bottom",
        plot.title.position = "plot", 
        axis.title.y = element_text(vjust = .85)))
#ggsave("June_Update/Overall2019vs2020.png", dpi=600)



# Heatmap ---------------------------------------------------------

sum_cars_year_month_day <- NYS_Thruway_Data %>% 
  group_by(year, month, day) %>% 
  summarize(sum_cars = sum(Vehicle.Count)) 

ggplot() +
  geom_tile(data= sum_cars_year_month_day,
            aes(x = day, y = fct_rev(month), fill = sum_cars)) +
  facet_wrap(~year, ncol=1) +
  # Add viridis colors
  scale_fill_viridis_c(option = "inferno", labels = comma) + 
  scale_x_discrete(breaks=seq(1,31,by=2)) +
  # Add nice labels
  labs(x = "Day of the month", y = NULL,
       title = "Total Vehicles Entering Thruway Per Day",
       fill = "Total\nNumber \nof Vehicles",
       caption = "Plot: @jakepscott2020 | Data: data.ny.gov/Transportation") +
  # Force all the tiles to have equal widths and heights
  coord_equal() +
  # Use a cleaner theme
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(panel.grid = element_blank(),
        # Bold, bigger title
        plot.title = element_text(face = "bold", size = rel(1.2)),
        # Plain, slightly bigger subtitle that is grey
        plot.subtitle = element_text(face = "plain", size = rel(.9), color = "grey70"),
        # Italic, smaller, grey caption that is left-aligned
        plot.caption = element_text(face = "italic", size = rel(0.9), 
                                    color = "grey70"),
        # Bold legend titles
        legend.title = element_text(size = rel(.8)),
        # Bold, slightly larger facet titles that are left-aligned for the sake of repetition
        strip.text = element_text(face = "bold", size = rel(.7), hjust = 0),
        strip.background.x = element_blank(),
        axis.title = element_blank(),
        # Add a light grey background to the facet titles, with no borders
        strip.background = element_blank(),
        # Add a thin grey border around all the plots to tie in the facet titles
        panel.border = element_rect(color = "grey90", fill = NA),
        plot.title.position = "plot",
        legend.key = element_rect(fill = "white", colour = "white"))

#ggsave("June_Update/HeatMap.png", dpi=600)


# Overall Commercial Versus non Commercial ------------------------

  
##Comparing to week last year
com_verus_noncom_total_index <- 
  NYS_Thruway_Data %>%
  filter((Date<="2020-07-07" & Date>="2020-01-01" | Date<="2019-07-07" & Date>="2019-01-01")) %>% 
  group_by(year, week, commercial_vehicle) %>% 
  summarise(Total=sum(Vehicle.Count)) %>%
  ungroup() 


year2019 <- com_verus_noncom_total_index %>% filter(year==2019)
year2020 <- com_verus_noncom_total_index %>% filter(year==2020)
com_verus_noncom_total_index <- 
  left_join(year2019,year2020, by=c("week", "commercial_vehicle"))

com_verus_noncom_total_index <- 
  com_verus_noncom_total_index %>% 
  mutate(index=(Total.y/Total.x)*100) 


ggplot(com_verus_noncom_total_index) +
  geom_hline(yintercept = 100, lwd=.3, linetype="dashed", color="grey70") +
  geom_line(aes(x=week, y=index,
                color=commercial_vehicle,
                group=commercial_vehicle),
            lwd=1.25) +
  geom_vline(xintercept = 9,color="grey50", lwd=1, linetype="dashed") +
  geom_vline(xintercept = 12,color="grey50", lwd=1, linetype="dashed") +
  geom_hline(yintercept = 100, lwd=.3, linetype="dashed", color="grey70") +
  scale_color_manual(values = c("#C29000","#0A8CC2")) +
  labs(title = "After a steady recovery between April and mid June, both\ncommercial and non-commerical traffic are plateauing",
       subtitle = "Vertical lines represent national and statewide lockdowns, respectively",
       x="Week",
       y= "Percent of 2019 Traffic", 
       color="Vehicle Type",
       caption = "Plot: @jakepscott2020 | Data: data.ny.gov/Transportation") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = rel(1.5)),
        plot.subtitle = element_text(face = "plain", size = rel(1.1), color = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        legend.title = element_text(size = rel(1.1), face="bold"),
        legend.text = element_text(size=rel(.9)),
        legend.position = "bottom",
        plot.title.position = "plot", 
        axis.title.y = element_text(vjust = .85)) 

#ggsave("June_Update/Com_V_NonCom.png", dpi=600)



# Commercial Versus non Commercial by Exit --------------------------------


com_verus_noncom_total_index_by_exit <- 
  NYS_Thruway_Data %>%
  filter((Date<="2020-07-07" & Date>="2020-01-01" | Date<="2019-07-07" & Date>="2019-01-01")) %>% 
  group_by(Exit, year, week, commercial_vehicle) %>% 
  summarise(Total=sum(Vehicle.Count)) %>%
  ungroup() 


year2019 <- com_verus_noncom_total_index_by_exit %>% filter(year==2019)
year2020 <- com_verus_noncom_total_index_by_exit %>% filter(year==2020)
com_verus_noncom_total_index_by_exit <- 
  left_join(year2019,year2020, by=c("Exit","week", "commercial_vehicle"))

com_verus_noncom_total_index_by_exit <- 
  com_verus_noncom_total_index_by_exit %>% 
  mutate(index=(Total.y/Total.x)*100) %>% 
  filter(week<=26)



ggplot(com_verus_noncom_total_index_by_exit) +
  geom_hline(yintercept = 100, lwd=.3, linetype="dashed", color="grey70") +
  geom_point(aes(x=week, y=index,color=commercial_vehicle), alpha=0) +
  geom_line(aes(x=week, y=index,
                color=commercial_vehicle,
                group=commercial_vehicle),
            lwd=.5) +
  facet_wrap(~Exit)+
  scale_color_manual(values = c("#C29000","#0A8CC2")) +
  scale_y_continuous(breaks = NULL) +
  labs(title = "There is heterogeneity in the recovery across exits",
       x="Week",
       y= "Percent of 2019 Traffic", 
       color="Vehicle Type",
       caption = "Plot: @jakepscott2020 | Data: data.ny.gov/Transportation") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = rel(1.2)),
        plot.subtitle = element_text(face = "plain", size = rel(.9), color = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        legend.title = element_text(face="bold",size = rel(.8)),
        legend.text = element_text(size=rel(.7)),
        legend.position = c(.85,.05),
        plot.title.position = "plot", 
        axis.title.y = element_text(vjust = .85),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) +
  guides(color=guide_legend(title.position = "left"))

#ggsave("June_Update/Commercial_vs_nonCommercial_by_Exit.png", dpi=600)





# July 4th and Memorial Day and St. Patricks and new years -----------------------------------------------

traffic2019vs2020 %>% filter(day(Date.x)=="4" & month(Date.x)=="7")
traffic2019vs2020 %>% filter(day(Date.x)=="27" & month(Date.x)=="5")
traffic2019vs2020 %>% filter(day(Date.x)=="25" & month(Date.x)=="5")
traffic2019vs2020 %>% filter(day(Date.x)=="17" & month(Date.x)=="3")
traffic2019vs2020 %>% filter(day(Date.x)=="1" & month(Date.x)=="1")

#New YEars
256146/268075*100
#St. Patrick's Day 2019 vs 2020
306801/336926*100
#Memorial day 2019 2020
215990/382485*100
#Fourth 2019 vs 2020
254736/363191*100

holidays <- tibble(holiday=c("New Years Day","St. Patrick's Day","Memorial Day","4th of July"),
       percent=c(95.55013,91.05887,56.47019,70.1383))

holidays$holiday <- factor(holidays$holiday,
          labels=c("New Years Day","St. Patrick's Day","Memorial Day","4th of July"),
          levels=c("New Years Day","St. Patrick's Day","Memorial Day","4th of July"))
set.seed(1)
ggplot(holidays,aes(x=(holiday),y=percent, group=1)) + 
  geom_point(color="#0A8CC2",size=5) +
  coord_cartesian(ylim=c(50,100)) +
  geom_text_repel(aes(label=holiday),direction = "y",box.padding = .75,point.padding = 1,segment.color = "grey70") +
  geom_line(color="#0A8CC2",lwd=2) +
  labs(title = "Holiday traffic on the 4th was still below last year's levels,\nbut was a sharp improvement from Memorial Day",
       x=NULL,
       y= "Percent of 2019 Traffic", 
       caption = "Plot: @jakepscott2020 | Data: data.ny.gov/Transportation") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = rel(1.5)),
        plot.subtitle = element_text(face = "plain", size = rel(1.1), color = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        legend.title = element_text(size = rel(1.1), face="bold"),
        legend.text = element_text(size=rel(.9)),
        legend.position = "bottom",
        plot.title.position = "plot", 
        axis.title.y = element_text(vjust = .85),
        axis.text.x=element_blank()) 
#ggsave("June_Update/Holidays2.png", dpi=600)
