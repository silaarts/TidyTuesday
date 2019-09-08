#TidyTuesday
#===============================================================================
#Women soccer 
#@sil_aarts
#===============================================================================

#Install library
install.packages("remotes")
remotes::install_github("daranzolin/d3rain")

#Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(d3rain)

#Load file
wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")

#Check if NL is in there. Yes: NED.
wwc_outcomes$team

#Select top 4 countries this WK
wwc <- wwc_outcomes %>%
  filter(team=="NED" |team=="USA" | team=="SWE" | team=="ENG")

#Select columns first, then select wins only. 
wwc2 <- wwc %>%
  select(c(1,2,7)) %>%
  filter(win_status=="Won")

#Add ID columns
wwc2$ID <- 1:nrow(wwc2)

#Change data.frame: make vectors of team > columns: spread the data from long to wide
wwc3 <- wwc2 %>% 
  mutate(AUX = 1) %>%
  spread(team, AUX, fill = 0)

#Select only 4 columns
wwc4 <- wwc3 %>%
  select(c(1,4:7)) 

#Include a total column (87), and compute total per country, resp: 42, 7, 23, 15
wwc4 <- wwc4 %>%
  mutate(Total=sum(cbind(ENG, NED, SWE,USA)))

wwc4 <- wwc4 %>%
  mutate(Total_U=sum(cbind(USA)))

wwc4 <- wwc4 %>%
  mutate(Total_N=sum(cbind(NED)))

wwc4 <- wwc4 %>%
  mutate(Total_S=sum(cbind(SWE)))

wwc4 <- wwc4 %>%
  mutate(Total_E=sum(cbind(ENG)))

#Change to logical levels for drop
wwc4$Total <- as.logical(wwc4$Total)
wwc4$ENG <- as.logical(wwc4$ENG)
wwc4$NED <- as.logical(wwc4$NED)
wwc4$SWE <- as.logical(wwc4$SWE)
wwc4$USA <- as.logical(wwc4$USA)

#Let it rain, let it rain!
p <- wwc4 %>% 
  d3rain_hist(x = year, 
              levels = c("Total", "USA", "NED", "SWE", "ENG"),
              title = "Women's wins for the top 4 countries of the world-cup in France (2019)") %>% 
  hist_chart_settings(annotations = c("Total number: 87 wins", "42 wins", "7 wins","23 wins","15 wins"),
                      levelLabelLocation = "right") %>% 
  hist_drip_settings(colors = c("grey","darkblue", "orange", "gold", "red"),
                     transitionIntervals = 2000,
                     dripSpeed = 700,
                     dripSize = 5)

#Run it!
p
