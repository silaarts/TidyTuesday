
#TidyTuesday
#===============================================================================
#TidyTuesday
#Chopped
#@sil_aarts
#===============================================================================
#Load libraries
library(tidyverse)
library(showtext)
library(showtextdb)
library(ggforce)
library(magick)
library(magick)

#Read file
data <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

#Make some colours
col_numb <- 45
mycols <- c("#A06D20","#41493B","#799422","#7B9972","#F3FCF0")
colors <- colorRampPalette(mycols)(col_numb) 

#Choose font
font_add_google("Russo One", "D")
font_add_google("Lexend Peta", "L")
showtext_auto()

#GGplot: boxplot
p1 <- ggplot(data, aes(x=season, y=episode_rating))+
  geom_boxplot(aes(fill=factor(season)))+
  geom_point(aes(color=factor(season)))+
  scale_colour_manual(values=colors)+
  scale_fill_manual(values=colors)+
  labs(x= "Season #",
       y= "Rating from 0 - 10")+
  scale_x_continuous(breaks=c(5,10,15,20,25,30,35,40,45))+
  scale_y_continuous(breaks=c(6,8,10), limits = c(6, 10))+
  theme_light()+
  theme(
    text = element_text(family="L"),
    legend.position = "none",
    plot.background = element_rect(fill = "gray80", colour = "gray80"),
    panel.background = element_rect(fill= "gray80"),
    axis.ticks.length=unit(-0.3, "cm"),
    axis.text.x = element_text(size=18, colour="black", face="bold"),
    axis.text.y = element_text(size=18, colour="black", face="bold"),
    axis.title.y = element_text(size=25, colour="black", face="bold"),
    axis.title.x = element_text(size=25, colour="black", face="bold"),
    plot.margin = unit(c(1,1,1,1), "cm"))

#Check it
p1
#Save as png
ggsave("/Users/silaarts/Desktop/p1.png", width = 18, height=8)

#Add image
image <- image_read("/Users/silaarts/Desktop/p1.png")
pic <- grid::rasterGrob(image, interpolate = T) 

#GGplot total plot
p <- ggplot(data)+
  #Make a knife
  geom_circle(aes(x0 = -53, y0 = 35, r = 10), fill="gray80", colour="gray80")+
  geom_rect(xmin = -55, xmax = -10, ymin = 25, ymax = 45,  colour="black", fill = "black", size=1)+ 
  geom_circle(aes(x0 = -40, y0 = 35, r = 2), fill="gray80", colour="gray80")+
  geom_circle(aes(x0 = -20, y0 = 35, r = 2), fill="gray80", colour="gray80")+
  #Add axis titles
  geom_text(aes(x= -53, y= 34, label="Source: Chopped\nPlot by @sil_aarts"), color="gray80", size=1.2, family="L", alpha=0.5, angle=90)+ 
  #Add pic of boxplot
  annotation_custom(pic, xmin=-10, xmax=80, ymin= -40, ymax= 65)+
  #Some vegatables
  geom_circle(aes(x0 = 50, y0 = -45, r = 4), fill="transparent", colour="yellow4", size=1.5)+
  geom_circle(aes(x0 = 20, y0 = -35, r = 4), fill="transparent", colour="yellow4", size=1.5)+
  geom_circle(aes(x0 = 30, y0 = -31, r = 3), fill="transparent", colour="yellow4",size=1.7)+
  geom_circle(aes(x0 = 40, y0 = -45, r = 2), fill="transparent", colour="yellow4",size=1.7)+
  geom_circle(aes(x0 = 60, y0 = -40, r = 3), fill="transparent", colour="yellow4",size=1.6)+
  geom_circle(aes(x0 = 70, y0 = -32, r = 3), fill="transparent", colour="yellow4",size=1.6)+
  #Cosmetics
  labs(title= "CHOPPED",
       subtitle= "Season Ratings")+
  scale_y_continuous(limits=c(-50,70))+
  scale_x_continuous(limits=c(-70,80))+
  theme_void()+
  theme(
    plot.background = element_rect(fill = "brown4", colour = "brown4"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.x = element_blank(),
    plot.title = element_text(color="black", size=55, hjust=0.5, margin = margin(5, 0, 0, 0), family="D"),
    plot.subtitle = element_text(colour = "black", size = 30, hjust= 0.5, margin = margin(0, 0, 0, 0), family = "L"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1,1,1,1), "cm"))

#Run quartz
quartz()

#Run it!
p
