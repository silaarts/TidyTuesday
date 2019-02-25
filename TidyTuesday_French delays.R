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

#Select only a few variabeles
trains2 <- select(trains, 1:4,7,24)

#Filter only national services
trains3 <- filter(trains2, trains2$service == "National")

#Summarise some variables
trains4 <- trains3 %>% 
  group_by(departure_station, year) %>% 
  summarise(total_num_trips = sum(total_num_trips, na.rm=T),
            num_greater_15_min_late = sum(num_greater_15_min_late, na.rm=T)) %>% 
ungroup()

#Make a new variable regarding percentage of total trips
trains4$percentage <- (trains4$num_greater_15_min_late/trains4$total_num_trips)*100

#Make sure every bar has a distinct colour: we need 59 bars!
nb.cols <- 59
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)

#GGplot with animation
p <- ggplot(data=trains4, aes(x=departure_station, y=percentage)) + 
  geom_col(aes(fill=departure_station), width = 0.8) + 
  geom_text(aes(label=departure_station), hjust="left", nudge_y = 0.5, size = 3, family="Comic Sans MS") +
  coord_flip()+
  scale_fill_manual(values = mycolors)+
  transition_time(year)+
  labs(title="Are you on time? Delays >15 minutes per departure station", 
       subtitle="Percentage of total number of trips by year: {round(frame_time)}\n", 
       x="", 
       y="",  
       caption="Source: SNCF OPEN DATA, Plot by @sil_aarts")+ 
  theme_minimal(10) +
  theme(legend.position = "none",
        text=element_text(family="Comic Sans MS"),
        plot.title=element_text(size=13, hjust=0.3, face='bold'),
        plot.subtitle=element_text(size=12, hjust=0.3, face='italic'),
        plot.caption=element_text(size=8, hjust=1),
        axis.text.x = element_text(size=10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
p

#Save GGplot animation
anim_save("Desktop/R/TidyTuesday/French_delays.gif")

