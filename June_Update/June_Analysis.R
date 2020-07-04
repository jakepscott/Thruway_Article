
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
  filter((Date<="2020-07-02" & Date>="2020-01-01" | Date<="2020-07-02" & Date>="2019-01-01")) %>% 
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
  labs(title = "Traffic had been rising since late April, but has plateaued \nas cases have risen across the country",
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
ggsave("June_Update/Overall2019vs2020.png", dpi=600)



# Vehicles by Weekday ---------------------------------------------

vehicles_by_weekday_2019vs2020 <- 
  NYS_Thruway_Data_lockdown %>% 
  group_by(lockdownperiod,Entrance,Date) %>% 
  mutate(Total_By_Date=sum(Vehicle.Count)) %>% ungroup() %>%
  group_by(lockdownperiod,year, weekday) %>% 
  summarise(Mean_Vehicles=mean(Total_By_Date))

vehicles_by_weekday_2019vs2020 <- 
  vehicles_by_weekday_2019vs2020 %>% 
  mutate(year_label=case_when(lockdownperiod=="Pre-Lockdown Period" &
                                weekday=="Monday" & year==2019~"2019",
                              lockdownperiod=="Pre-Lockdown Period" &
                                weekday=="Monday" & year==2020~"2020",
                              lockdownperiod=="Post-Lockdown Period" &
                                weekday=="Monday" & year==2019~"2019",
                              lockdownperiod=="Post-Lockdown Period" &
                                weekday=="Monday" & year==2020~"2020")) 


ggplot(vehicles_by_weekday_2019vs2020) +
  geom_col(aes(x=weekday,y=Mean_Vehicles, fill=year),
           position = position_dodge()) +
  geom_text(aes(x=weekday,y=Mean_Vehicles, color=year, label=year_label),
            position = position_dodge(width = 0.9),vjust=-.2,
            family="Roboto Condensed") +
  facet_wrap(~fct_rev(factor(lockdownperiod)), ncol = 1) +
  scale_color_manual(values = c("grey70", "#C29000")) +
  scale_fill_manual(values = c("grey70", "#C29000")) +
  labs(title = "Mean Number of Vehicles Passing Entry Points by Day of Week",
       subtitle = "There is a clear fall in the number of vehicles entering toll booths following lockdown",
       y="Mean Number\nof Vehicles",
       x=NULL,
       fill=NULL,
       caption = "Plot: @jakepscott2020 | Data: data.ny.gov/Transportation") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = rel(1.2)),
        plot.subtitle = element_text(face = "plain", size = rel(.9), color = "grey70"),
        plot.caption = element_text(size = rel(0.9), face="italic", 
                                    color = "grey70"),
        strip.text = element_text(face="bold",size = rel(1)),
        legend.title = element_text(size = rel(.8)),
        plot.title.position = "plot", 
        axis.title.y = element_blank(),
        legend.position = "none") +
  coord_cartesian(ylim=c(0,15000))

#ggsave("July_Update//Vehicles_by_Weekday.png", dpi=600)



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


# Location Map ------------------------------------------------------------


#Getting the number of vehicles by exit for each date
Entrance_Data_2020 <- NYS_Thruway_Data %>% 
  group_by(Date, Entrance) %>%
  summarise(total=sum(Vehicle.Count)) %>% 
  ungroup() %>%
  rename(Exit=Entrance) %>%
  mutate(Date=ymd(Date),
         year=year(Date),
         month=month(Date),
         day=day(Date),
         weekday=weekdays(as.Date(Date))) %>%
  filter(year==2020 & month>=3)

#Getting the latitude and longitude for the exits
Locations <- locations %>% select(Exit, Latitude, Longitude)

#Combining the number of vehicles and the lat and long
Entrance_Data_2020 <- left_join(Entrance_Data_2020,Locations,by="Exit")

#Standardizing number of cars
Entrance_Data_2020 <- 
  Entrance_Data_2020 %>% 
  group_by(Exit,weekday) %>% 
  mutate(mean=mean(total), sd=sd(total)) %>%
  ungroup() %>% 
  mutate(z_score=(total-mean)/sd)

#2020 Gif using gganimate
gif_2020 <- ggplot() + 
  geom_polygon(data=NY, aes(x=long, y=lat, group=group),
               color="black", fill="lightblue") +
  geom_point(data=Entrance_Data_2020, aes(x=Longitude, y=Latitude, size=z_score,
                                          color=z_score)) +
  scale_color_viridis_c(option = "inferno", labels = comma) + 
  scale_size(range = c(.1, 10)) +
  labs(title="Number of Vehicles at a Given Exit on {frame_time}",
       subtitle = "Traffic in 2020 mirrors that of 2019... until the stay-at-home order was announced on March 21st",
       x="Longitude",
       y="Latitude",
       color= "Z Score",
       size="Z Score",
       caption = "Plot: @jakepscott2020 | Data: data.ny.gov/Transportation") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = rel(1.2)),
        plot.subtitle = element_text(face = "plain", size = rel(.9), color = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.9), 
                                    color = "grey70"),
        legend.title = element_text(size = rel(.8)),
        plot.title.position = "plot", 
        axis.title.y = element_text(angle=0, vjust = .5)) +
  transition_time(Date)

#Animating the gif
gif_2020 <- animate(gif_2020, renderer = gifski_renderer(), 
                    nframes=length(unique(Entrance_Data_2020$Date)),
                    end_pause = 5,
                    width = 500, 
                    height = 500)

#Getting the number of vehicles by exit for each date for 2019
Entrance_Data_2019 <- NYS_Thruway_Data %>% 
  group_by(Date, Entrance) %>%
  summarise(total=sum(Vehicle.Count)) %>% 
  ungroup() %>%
  rename(Exit=Entrance) %>%
  mutate(Date=ymd(Date),
         year=year(Date),
         month=month(Date),
         day=day(Date),
         weekday=weekdays(as.Date(Date))) %>% 
  filter(year==2019 & month>=3 & Date<="2019-06-29")


#Combining the number of vehicles and the lat and long
Entrance_Data_2019 <- left_join(Entrance_Data_2019,Locations,by="Exit")

#Standardizing number of cars
Entrance_Data_2019 <- 
  Entrance_Data_2019 %>% 
  group_by(Exit,weekday) %>% 
  mutate(mean=mean(total), sd=sd(total)) %>%
  ungroup() %>% 
  mutate(z_score=(total-mean)/sd)

##Making gif with gganimate
gif_2019 <- ggplot() + 
  geom_polygon(data=NY, aes(x=long, y=lat, group=group),
               color="black", fill="lightblue") +
  geom_point(data=Entrance_Data_2019, aes(x=Longitude, y=Latitude, size=z_score,
                                          color=z_score)) +
  scale_color_viridis_c(option = "inferno", labels = comma) + 
  scale_size(range = c(.1, 10)) +
  labs(title="Number of Vehicles at a Given Exit on {frame_time}",
       subtitle = "Besides some drops (likely due to weather), traffic in 2019 is fairly consistent",
       x="Longitude",
       y="Latitude",
       color= "Z Score",
       size="Z Score") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = rel(1.2)),
        plot.subtitle = element_text(face = "plain", size = rel(.9), color = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.3), 
                                    color = "grey70", hjust = 0),
        legend.title = element_text(size = rel(.8)),
        plot.title.position = "plot", 
        axis.title.y = element_text(angle=0, vjust = .5)) +
  transition_time(Date)

#animating
gif_2019 <- animate(gif_2019, renderer = gifski_renderer(), 
                    nframes=length(unique(Entrance_Data_2019$Date)),
                    end_pause = 5,
                    width = 500, 
                    height = 500)
#turning the gifs into a series of images using the magik package
a_mgif <- image_read(gif_2019)
b_mgif <- image_read(gif_2020)

#Making a new gif with the two next to each other, again using magik
  new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
  for(i in 1:length(unique(Entrance_Data_2019$Date))){
    combined <- image_append(c(a_mgif[i], b_mgif[i]))
    new_gif <- c(new_gif, combined)
  }
  
  #Saving. For some reason I Cannot get it to save in the Figures Folder
  #image_write(new_gif, "NYS_Locations.gif", quality = 100)
  image_write(new_gif, "NYS_Locations_630.gif", quality = 100)



# Overall Commercial Versus non Commercial ------------------------

  
##Comparing to week last year
com_verus_noncom_total_index <- 
  NYS_Thruway_Data %>%
  filter((Date<="2020-07-02" & Date>="2020-01-01" | Date<="2020-07-02" & Date>="2019-01-01")) %>% 
  group_by(year, week, commercial_vehicle) %>% 
  summarise(Total=sum(Vehicle.Count)) %>%
  ungroup() 


year2019 <- com_verus_noncom_total_index %>% filter(year==2019)
year2020 <- com_verus_noncom_total_index %>% filter(year==2020)
com_verus_noncom_total_index <- 
  left_join(year2019,year2020, by=c("week", "commercial_vehicle"))

com_verus_noncom_total_index <- 
  com_verus_noncom_total_index %>% 
  mutate(index=(Total.y/Total.x)*100) %>% 
  filter(week<=26)


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
  filter((Date<="2020-07-02" & Date>="2020-01-01" | Date<="2019-07-02" & Date>="2019-01-01")) %>% 
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
  labs(title = "Across the state, traffic of all types is starting to level off",
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
