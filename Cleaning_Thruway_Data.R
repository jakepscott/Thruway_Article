library(tidyverse)
library(lubridate)
library(readr)

Thruway_2019 <- read_csv("data/NYS_Thruway_Data_2019.csv",
                         col_types = "cccccdc")
saveRDS(Thruway_2019, "data/NYS_Thruway_Data_2019.rds")

Thruway_2020 <- read_csv("data/NYS_Thruway_Data_2020_6_12.csv",
                         col_types = "cccccdc")
saveRDS(Thruway_2020, "data/NYS_Thruway_Data_2020.rds")

Thruway_2019 <- readRDS("data/NYS_Thruway_Data_2019.rds")
Thruway_2020 <- readRDS("data/NYS_Thruway_Data_2020.rds")



NYS_Thruway_Data_Raw <- rbind(Thruway_2020,Thruway_2019)
saveRDS(NYS_Thruway_Data_Raw, "data/NYS_Thruway_Data_Raw.rds")

NYS_Thruway_Data <- readRDS("data/NYS_Thruway_Data_Raw.rds")

NYS_Thruway_Data <- NYS_Thruway_Data %>% mutate(Date=mdy(Date),
                                                year=year(Date),
                                                month=month(Date),
                                                day=day(Date),
                                                weekday=weekdays(as.Date(Date)),
                                                week=week(Date),
                                                commercial_vehicle = case_when(`Vehicle Class`=="2L"~"Non-commercial",
                                                                               TRUE~"Commercial"),
                                                NY_Lockdown=case_when(Date>="2020-03-20"~"Lockdown in Place",
                                                                      TRUE~"Lockdown not in Place"),
                                                US_Lockdown=case_when(Date>="2020-03-01"~TRUE,
                                                                      TRUE~FALSE),
                                                National_Emergency=case_when(Date>="2020-01-29"~TRUE,
                                                                             TRUE~FALSE)) 

##Renaming factors
month_names <- c("January", "February", "March", "April", "May", "June", "July",
                 "August", "September", "October", "November", "December")

day_names <- c("Monday", "Tuesday", "Wednesday", 
               "Thursday", "Friday", "Saturday", "Sunday")

NYS_Thruway_Data <- NYS_Thruway_Data %>% mutate(month=factor(month, labels = month_names),
                                                weekday=factor(weekday, labels = day_names), 
                                                day=factor(day),
                                                year=factor(year)) %>%
  rename("Vehicle.Class"=`Vehicle Class`,
         "Vehicle.Count"=`Vehicle Count`)
saveRDS(NYS_Thruway_Data, "data/NYS_Thruway_Data_Clean.rds")
