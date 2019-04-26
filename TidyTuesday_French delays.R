#TidyTuesday
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

#Make a new variable regarding percentage of total trips
trains4$percentage_late <- (trains4$num_late_at_departure/trains4$total_num_trips)*100

#Order data file by percentage_late to see top of stations
trains5 <-  trains4[order(-trains4$percentage_late),] 

#Select those trainstations of which one year is >25 percentage delays
trains6 <- trains5 %>%
  group_by(departure_station) %>%
  filter(any(percentage_late > 20))%>%
  ungroup(trains6)

#Change one rowname
trains6$departure_station[trains6$departure_station=="CHAMBERY CHALLES LES EAUX"] <- "CHAMBERY CHALLES"

#Make sure every bar has a distinct colour: we need 59 bars!
nb.cols <- 12
mycolors <- colorRampPalette(brewer.pal(12, "Blues"))(nb.cols)

#GGplot with animation
p <- ggplot(data=trains6, aes(x=departure_station, y=percentage_late)) + 
  geom_col(aes(fill=departure_station), width = 0.8)+
  coord_polar()+
  scale_fill_manual(values = mycolors)+
  transition_time(year)+
  labs(title="Stations with >20% 'late at departure' in at least one year", 
       subtitle="% late at departure (of total number of trips) in {round(frame_time)}",
       x="", 
       y="",
       caption="Source: SNCF OPEN DATA, Plot by @sil_aarts")+
  theme_minimal(10) +
  theme(legend.position = "none",
        text=element_text(family="Courier"),
        plot.title=element_text(size=12, hjust=0.2, face='bold'),
        plot.subtitle=element_text(size=11, hjust=0.2),
        plot.caption=element_text(size=9, hjust=0.2),
        axis.text.x = element_text(size=9, hjust=0.1, face='bold'),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank())

#Run it!
p

#GGplot save
animate(p, height = 700, width =700)
anim_save("Desktop/R/TidyTuesday/French_delaysII.gif")

