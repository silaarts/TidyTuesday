#TidyTuesday
#===============================================================================
#Abducted by Aliens
#@sil_aarts
#===============================================================================

#load libraries
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(htmlwidgets)
library(ggplot2)
library(ggmap)
library(ggimage)
library(magick)
library(grid)
library(gridExtra)
library(showtext)
library(showtextdb)

#Load data
ufo <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

#Only unique rows
ufo2 <- unique(ufo)

#Select several columns
ufo3 <- ufo2 %>%
  select(3:5,10,11)

#Only non-missing data
ufo4 <- na.omit(ufo3)

#Only select disk ufo's: you know, those special ones
ufo5 <- ufo4 %>%
  filter(ufo_shape== "disk")

#Change colnames ease of use
ufo5$long <- ufo5$longitude
ufo5$lat <- ufo5$latitude

#Add font
font_add_google("Rubik Mono One", "Rubik")
font_add_google("Press Start 2P", "Game")
showtext_auto()

#Add an image
image <- image_read("Desktop/alien2.png")
alien <- grid::rasterGrob(image, interpolate = T) 

#Set a theme
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Game", color = "black"),
      plot.background = element_rect(fill = "black", color = NA), 
      panel.background = element_rect(fill = "black", color = NA), 
      legend.background = element_rect(fill = "black", color = NA),
      plot.title=element_text(size=14, hjust=0, face='bold', colour="white", lineheight = 1),
      plot.caption=element_text(size=9, hjust=1, colour="white"),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
     )
}

#Run quartz for showtext
quartz()

#World data
world_map <- map_data("world")

#GGplot: map
p <- ggplot(world_map, aes(x=long, y=lat)) +
  geom_polygon(aes(group = group), fill = "grey16", color = "lightgrey")+ 
  geom_point(data = ufo5, aes(x=long, y=lat), colour = "green", size = 0.3)+
  annotation_custom(alien, xmin=162, xmax=182, ymin=30, ymax=50)+
  annotation_custom(alien, xmin=80, xmax=100, ymin=-40, ymax=-60)+
  annotation_custom(alien, xmin=-45, xmax=-65, ymin=40, ymax=30)+
  annotation_custom(alien, xmin=-120, xmax=-140, ymin=-60, ymax=-40)+
  annotation_custom(alien, xmin=160, xmax=145, ymin=20, ymax=30)+
  annotate("label", x = 0, y = 0, label = "WARNING", color = "red", family = "Rubik", size = 40) +
  annotate("label", x = +70, y = +80, label = "INCOMING MESSAGE [22|07|2053 : 00.00]:\n%ˆ(*_(//(%#ˆ(&)%ˆ!}%$&˜˜@#@!%)@oo%))ˆˆ"   ,
           color = "black", family = "Game", size = 2, hjust = 0, fill="white", fontface = "bold") +
  annotate("label", x = +70, y = +65, label = "INCOMING MESSAGE [22|07|2053 : 00.00]:\n4.320th Disk UFO since 1949. WE ARE COMING!", 
           color = "black", family = "Game", size = 2, hjust = 0, fill="white", fontface = "bold")+
  labs(x = NULL, 
       y = NULL, 
       title= "!!!ALIEN INVASION: OUTER SPACE IS COMING FOR YOU, USA!!!",
       caption = "Data: NUFORC. Person abducted: @sil_aarts") +
  scale_x_continuous(breaks = seq(-200, 200, by = 25)) +
  scale_y_continuous(breaks = seq(-100, 100, by = 25)) +
  scale_size_continuous(range = c(0.4, 1)) +
  coord_fixed(xlim = c(-165, 180), ylim = c(-76, 82))+
  theme_map()



#Run it!
p


