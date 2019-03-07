#TidyTuesday -february 26
#French train delays
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gganimate)
library(gapminder)
library(RColorBrewer)
library(extrafont)

#Read file
trains <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/full_trains.csv")
summary(trains)

#Select only a few variabeles
trains2 <- select(trains, 1:4,7,8, 10,24, 26,27)

#Filter only national services
trains3 <- filter(trains2, trains2$service == "National")

#Summarise some variables
trains4 <- trains3 %>% 
  group_by(departure_station, year) %>% 
  summarise(total_num_trips = sum(total_num_trips, na.rm=T),
            num_of_canceled_trains=sum(num_of_canceled_trains, na.rm=T),
            num_late_at_departure=sum(num_late_at_departure, na.rm=T),
            num_greater_15_min_late = sum(num_greater_15_min_late, na.rm=T),
            num_greater_30_min_late = sum(num_greater_30_min_late, na.rm=T),
            num_greater_60_min_late = sum(num_greater_60_min_late, na.rm=T)) %>% 
  ungroup(trains4)



# Scatterplot
p <- ggplot(trains4, aes(x=num_of_canceled_trains, y=total_num_trips)) + 
  geom_point(aes(col=year)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 1500)) + 
  ylim(c(0, 75000))+
  geom_text(aes(label=ifelse(total_num_trips>20000,as.character(departure_station),'')),hjust=0,vjust=0)+
  labs(subtitle="The relation between total number of trips & number of canceled trains", 
       y="total # of trips", 
       x="# of canceled trains", 
       title="Scatterplot", 
       caption = "Source: SCNF, plot by: @sil_aarts")+
  theme_set(theme_bw())+
  theme(legend.position = "none",
        text=element_text(family="Courier"),
        plot.title=element_text(size=12, face='bold'),
        plot.subtitle=element_text(size=11),
        plot.caption=element_text(size=10),
        axis.text.x = element_text(size=10, hjust=0.1, face='bold'))

p
