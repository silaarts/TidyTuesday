#TidyTuesday
#===============================================================================
#Nuclear Explotions
#@sil_aarts
#===========================================================================

#Load libraries
library(ggplot2)
library(dplyr)
library(zoo)
library(tidyverse)
library(ggtext)
library(extrafont)
library(ggforce)
library(showtext)
library(showtextdb)

#Load file
data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")
summary(data)
data1 <- data_frame(unique(data$id_no))

#Sort 
data1 <- data[order(-data$magnitude_body),]
#How many explotions with mb=0?
data2 <- data1 %>%
filter(magnitude_body == 0)

#Choose font
font_add_google("Anton", "Anton")
showtext_auto()

#Make theme
theme_sil <- theme_void() +
  theme(
    plot.background = element_rect(fill = "grey80"),
    text= element_text(family="Anton", face="bold"),
    legend.text = element_text(margin = margin(0, 20, 0, 0)),
    legend.title = element_text(margin = margin(0, 20, 0, 0)),
    legend.text.align = 0,
    strip.text = element_blank(),
    panel.spacing = unit(2, "points"),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(size = 40, hjust = 0, colour="darkred"),
    plot.subtitle = element_text(size = 25, hjust = 0),
    plot.caption = element_text(hjust = 1, size = 6)) 

#Run quartz for showtext
quartz()

#Make drawing
p <- ggplot()+
  #Circle 1: 7.4
  geom_hline(yintercept = 9, colour="black", size=0.5)+
  geom_circle(aes(x0 = -10, y0 = -1, r = 5), fill="black")+
  geom_rect(aes(xmin = -12.5, ymin = 1, xmax = -8.5, ymax = 2), fill= "orange",color = "darkred")+
  geom_text(aes(x = -11, y = 1.5), label="MB: 7.4", color="white", size=6, family="Anton")+
  geom_label(aes(x = -11, y = -0.5), label="Year: 1987\nCountry: USSR\n Type: SHAFT", color="black", size=3, family="Anton")+
  #Circle 2: 7.3
  geom_circle(aes(x0 = -5, y0 = -1, r = 4), colour="black", fill="grey15")+
  geom_rect(aes(xmin = -7, ymin = 0, xmax = -3, ymax = 1), fill= "orange", color = "darkred")+
  geom_text(aes(x = -5, y = 0.5), label="MB: 7.3", color="white", size=6, family="Anton")+
  geom_label(aes(x = -5, y = -1.5), label="Year: 1987\nCountry: USSR\nType: SHAFT", color="black", size=3, family="Anton")+
  #Circle 3: 7.2
  geom_circle(aes(x0 = 0, y0 = -1, r = 3), colour="black", fill="grey25")+
  geom_rect(aes(xmin = -2, ymin = -1, xmax = 2, ymax = 0), fill= "orange", color = "darkred")+
  geom_text(aes(x = 0, y = -0.5), label="MB: 7.2** ", color="white", size=6, family="Anton")+
  geom_label(aes(x = 0, y = -2.5), label="Year: 1985\nCountry: USSR\nType: SHAFT", color="black", size=3, family="Anton")+
  #Circle 4: 0
  geom_circle(aes(x0 = 5, y0 = -1, r = 0.5), colour="black", fill="grey45")+
  annotate("segment", x= 5, xend = 5, y = 3, yend = -1, size=0.5)+
  annotate(geom="label", x= 5, y = 3, label = "1.217 explosions\nhave an 'MB' of zero.", vjust=0.1, size=4, family="Anton", fontface="bold")+
  #Extra text
  annotate(geom="label", x= 10, y = -9, label = 
  "* 'Biggest' is expressed in 'Body wave magnitude of explosion' (MB).\nBody-waves consist of P-waves or S-waves, or reflections of either. Body-waves travel through rock directly. Source: Wikipedia.
  ** Two explotions have an MB of 7.2. Based on magnitude surface, this explosion was chosen as the 3th biggest.", size=2.5, vjust=0.1, hjust=1, family="Anton")+ 
  labs(title="NUCLEAR EXPLOTIONS", 
       subtitle="The three biggest explosions and the smallest ones.*",
       caption="Source: Stockholm International Peace Research Institute, SIPRI | Plot by: @sil_aarts",
       y="Age of death (z-scores)")+
  coord_fixed()+
  theme_sil


#Run it!
p
