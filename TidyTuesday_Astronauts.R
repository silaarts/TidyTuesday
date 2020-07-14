
#TidyTuesday
#===============================================================================
#TidyTuesday
#Astronauts
#@sil_aarts
#===============================================================================
#Load libraries
library(tidyverse)
library(extrafont)
library(showtext)
library(showtextdb)
library(ggridges)
library(grid)
library(gridExtra)
library(magick)
library(ggforce)
library(ggtext)
library(cowplot)
library(glue)
library(ggsci)
library(magick)

#Read file
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')
#Check 
data1 <- unique(data)

#Get some facts!
sex <- data%>%
  filter(sex == "female")%>%
  distinct(name)
#How many missions in total
unique(data$selection)
#Most missions? 7
#Occupation
occ2 <- data%>%
  filter(occupation == "Other (Journalist)")
#Hours spend in space
hours <- data%>%
  summarize(hours=sum(hours_mission))
#Youngest person
data$age <- data$year_of_mission - data$year_of_birth

#Choose font
font_add_google("Monofett", "M")
font_add_google("Gugi", "G")
font_add_google("Anton", "S")
showtext.auto()

#aAd image
sun <- image_read("/Users/silaarts/Desktop/sun.png")
image2 <- grid::rasterGrob(sun, interpolate = T) 

#GGplot
p <- ggplot()+
  #annotation_custom(image, xmin= -30, xmax= 30, ymin= -40, ymax= 40)+
  #Create ellipses
  #geom_ellipse(aes(x0 = -16, y0 = 0, a = 5, b = 4.5, angle = 0), color="white", fill="transparent", size=0.1)+
  geom_curve(aes(x = -10, y = -15, xend = -10, yend = 15), color = "white")+
  geom_curve(aes(x = -8, y = -15, xend = -8, yend = 15), color = "white")+
  geom_curve(aes(x = -4, y = -15, xend = -4, yend = 15), color = "white")+
  geom_curve(aes(x = -1, y = -15, xend = -1, yend = 15), color = "white")+
  geom_curve(aes(x = 3, y = -15, xend = 3, yend = 15), color = "white")+
  geom_curve(aes(x = 5, y = -15, xend = 5, yend = 15), color = "white")+
  #Create some planets
  geom_circle(aes(x0 = 1.5, y0 = -13, r = 2.2), fill="#624D7C")+
  geom_circle(aes(x0 = 10.1, y0 = 8, r = 2.3), fill="#700000")+
  geom_circle(aes(x0 = -1, y0 = 4.3, r = 2), fill="#3D87A3")+
  geom_circle(aes(x0 = -6.3, y0 = 11, r = 1.5), fill="#B0A8B9")+
  geom_circle(aes(x0 = 3, y0 = -3.8, r = 2.8), fill="#5C3A00")+
  geom_circle(aes(x0 = 9.5, y0 = -3.8, r = 2), fill="#D78374")+
  #Add images
  annotation_custom(image2, xmin= -25, xmax= 0, ymin= -7, ymax= 7)+
 #Add text
  geom_text(aes(x=1.5, y=-13, label= "Toyohiro Akiyama:\nthe only journalist"), color="white", size=2, inherit.aes = FALSE, family="G")+ 
  geom_text(aes(x=10.15, y=8, label= "7\nThe highest\nnumber of\nmissions by\nFranklin R.Chang-Diaz\n&\n Jerry Ross"), color="white", size=1.7, inherit.aes = FALSE, family="G")+ 
  geom_text(aes(x=-1, y=4.5, label= "They made\n230 missions\nsince 1961"), color="white", size=2.2, family="G")+ 
  geom_text(aes(x=-6.25, y=11.1, label= "64\nfemales"), color="white", size=2.8, inherit.aes = FALSE, family="G")+ 
  geom_text(aes(x=3, y=-3.65, label= "Gherman Titov\n&\nValentina Tereshkova\nwere the youngest\nto go on a mission:\n26 years"), color="white", size=2, family="G")+ 
  geom_text(aes(x=9.5, y=-3.6, label= "1.341.979\nhours\nthey spend\nin space"), color="white", size=2, family="G")+ 
  #Aesthetics
  labs(title= "Astronauts",
       subtitle = "Who are they?",  
       caption = "Source: Mariya Stavnichuk and Tatsuya Corlett &\n'Population analysis of space travelers'\nPlot by @sil_aarts")+
  coord_fixed()+
  theme_void()+
  theme(
    #legend.position = c(0.1, 0.4),
    #legend.title = element_text(size=30, family="S", colour="white"),
    #legend.text = element_text(size=30, family="S", colour="white"),
    strip.background = element_rect(fill = "black", colour = "black"),
    plot.background = element_rect(fill = "black", colour = "black"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.x = element_blank(),
    plot.title = element_text(color="white", size=50, hjust=0.5, margin = margin(15, 0, 0, 0), family="M"),
    plot.subtitle = element_text(colour = "white", size = 20, hjust= 0.5, margin = margin(5, 0, 0, 0), family = "G"),
    plot.caption = element_text(colour = "white", size = 8, hjust= 0.5, margin = margin(0, 5, 5, 0), family = "M"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
    #plot.margin = unit(c(0,3,3,0), "cm"))
quartz()

#Run it!
p
