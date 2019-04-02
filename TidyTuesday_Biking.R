#TidyTuesday
#Biking

#Install new packages for specific themes GGplot
install.packages("ggthemes")

#Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggmap)
library(zipcode)
library(ggthemes)
library(extrafont)

#Read file
bike <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")

#Select one crossing: of course > Broadway!
bike2 <- bike %>%
  filter(bike$crossing == "Broadway Cycle Track North Of E Union St")

#Make column 1 only days, not times, substr char <10
bike2$date <- str_sub(bike2$date,1,10)

#Sum bike counts per day
bike3 <- aggregate(bike2$bike_count ~ date + crossing + direction, data=bike2, sum)

#Change colname
colnames(bike3) [c(4)] <- c("bike_count")

#Filter 2019: first change character tot date
bike3$date <- as.Date(bike3$date, "%m/%d/%Y")
bike4 <- bike3 %>%
  filter(date > "2019-01-01" & date < "2019-12-31") 

#Make a date for 2019-01-14
x <- as.Date("2019-01-14")

#GGplot: North
p <-bike4 %>%
  filter (direction== "North") %>%
    ggplot(aes(x=date)) + 
    geom_line(aes(y=bike_count), color="gold4", size=1.5) + 
    labs(title= "TidyTuesday: Biking in Seattle | Broadway Cycle Track North Of E Union St.", 
        subtitle= "TOTAL DAILY BIKE TRAFFIC | Direction North | January 2019",
        caption="Source: Seattle Gov. | Plot by: @sil_aarts")+
  annotate(geom="label", x=x, y = 205, label = "250 bikes on 14th, January 2019", family= "Courier", vjust=0.1, size=5, fontface="bold")+
  theme_wsj()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.x = element_line(color="darkgrey"),
    axis.line.y = element_blank(),
    axis.text.x = element_text(color="darkgrey", size=15),
    axis.text.y = element_text(color="darkgrey", size=14),
    panel.grid.major.y = element_line(colour="darkgrey"),
    plot.title=element_text(size=16, face="bold", hjust=0, color="black"),
    plot.subtitle=element_text(size=15, hjust=0, color="black"),
    plot.caption=element_text(size=12, color="darkgrey"))
    
#Run it!
p

#Make a date for 2019-01-14
x2 <- as.Date("2019-01-29")

#GGplot: South
p2 <-bike4 %>%
filter (direction== "South") %>%
  ggplot(aes(x=date)) + 
  geom_line(aes(y=bike_count), color="gold4", size=1.5) + 
  labs(title= "TidyTuesday: Biking in Seattle | Broadway Cycle Track North Of E Union St.", 
       subtitle= "TOTAL DAILY BIKE TRAFFIC | Direction South | January 2019",
       caption="Source: Seattle Gov. | Plot by: @sil_aarts")+
  annotate(geom="label", x=x2, y = 241, label = "241 bikes on 14th, January 2019", family="Courier", hjust=0.8, vjust=0.1, size=5, fontface="bold")+
  theme_wsj()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.x = element_line(color="darkgrey"),
    axis.line.y = element_blank(),
    axis.text.x = element_text(color="darkgrey", size=15),
    axis.text.y = element_text(color="darkgrey", size=14),
    panel.grid.major.y = element_line(colour="darkgrey"),
    plot.title=element_text(size=16, face="bold", hjust=0, color="black"),
    plot.subtitle=element_text(size=15, hjust=0, color="black"),
    plot.caption=element_text(size=12, color="darkgrey"))    

#Run it!
p2
