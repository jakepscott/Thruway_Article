# Coronavirus and New York State Thruway Traffic 
I wrote the code in this repo to generate figures for two Medium blog posts on how thruway traffic in NY has been affected by Coronavirus and the subsequent lockdowns/reopenings. They are [New Yorkers Cut Back On Driving Following Stay-At-Home Order](https://medium.com/@jakepscott16/new-yorkers-cut-back-on-driving-following-stay-at-home-order-72b40585992) and [Slow and Steady: Traffic Growth Has Recovered Modestly Since March Shutdowns](https://medium.com/@jakepscott16/slow-and-steady-traffic-growth-recovered-modestly-since-march-shutdowns-d442defc7506).

The code is "living" in that I still fairly regularly will tweak and rerun it, so the code at any given point will not exactly replicate the figures in the posts in terms of things like date range, but the basic structure will always be the same. 

![image](https://user-images.githubusercontent.com/56490913/88095926-e0169a80-cb63-11ea-8496-e991ada52f3f.png)

## Getting Started

To replicate the figures in the article, first download the folders and code in this repo. For the first article, New Yorkers Cut Back On Driving Following Stay-At-Home Order, use the Full_Thruway_Analysis.R file. Follow the comments in the code to make the figure you want to replicate. For the second article, Slow and Steady: Traffic Growth Has Recovered Modestly Since March Shutdowns, open the June_Update folder and use the June_Analysis.R file. Again, comments in the code will guide you to the figure you want to replicate. 

The Cleaning_Thruway_Data.R file only needs to be run if the data in the data folder is updated. For example, if you download more recent 2020 data, you will need to run the code in that file to get the full dataset up to spead. 

### Prerequisites

To run the code on your computer you just need R and the following packages and fonts installed and loaded:

```
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
```

### Datasets
Thruway data comes from data.NY.gov: [2019](https://data.ny.gov/Transportation/NYS-Thruway-Origin-and-Destination-Points-for-All-/chzq-388p), [2020](https://data.ny.gov/Transportation/NYS-Thruway-Origin-and-Destination-Points-for-All-/r8tn-bjyq)

## Author

* **Jake Scott** - [Twitter](https://twitter.com/jakepscott2020), [Medium](https://medium.com/@jakepscott16)
