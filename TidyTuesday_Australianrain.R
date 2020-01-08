#TidyTuesday
#===============================================================================
#Australian raindfall
#@sil_aarts
#===========================================================================

#Load libraries
library(gganimate)
library(tidyverse)
library(ggthemes)

#Load file
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
data1 <- unique(data)

#Summarize rainfall 
data2 <- data1 %>%
  group_by(city_name, year, month, long, lat)%>%
  summarize(rainfall_month = sum(rainfall))

#Check na's
data3 <- data2 %>%
  drop_na()

#Set a theme
theme <- 
  theme_wsj() +
    theme(
      text = element_text(family = "Georgia", color = "black"),
      legend.position = "none",
      axis.ticks.y = element_line(size=12),
      axis.text.x = element_text(size=16),
      axis.text.y = element_text(size=16),
      plot.title=element_text(size=32, hjust=0.5, face='bold', colour="black", lineheight = 1),
      plot.caption=element_text(size=13, hjust=0.5, colour="black", family="mono"),
      plot.subtitle=element_text(size=19, hjust=0.5, colour="black", family="mono"))

#Plot
p <- ggplot(data3, aes(x=city_name, y=rainfall_month, fill=rainfall_month)) +
  annotate("text", x = "Brisbane", y = 250, label = "AU", color="bisque3", alpha=0.2)+
  geom_boxplot(aes(fill=city_name), size=1.5, alpha=0.5) +
  scale_y_reverse()+
  scale_x_discrete(position = "top") +
  scale_fill_manual(values=c("#0492F7", "#004ED2", "#004BA4", "#00A0EC", "#00C5D9", "#00DFE9"))+
  transition_time(year)+
labs(x = NULL, 
       y = "Rainfall(millimeters)", 
       caption = "Source: Bureau of Meterology (BoM) | Plot by @sil_aarts",
       title = "Rainfall in Australia in {round(frame_time)}", 
       subtitle = "In millimeters per month")+
  theme

#Save it!
animate(p, nframes = 130, fps=2, height = 700, width =700)
#GGplot save
anim_save("Desktop/RainfallAustralia.gif")
