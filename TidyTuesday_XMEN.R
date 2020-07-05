
#TidyTuesday
#===============================================================================
#TidyTuesday
#X-Men Do Cry
#@sil_aarts
#===============================================================================

#Load libraries
library(tidyverse)
library(showtext)
library(showtextdb)
library(grid)
library(gridExtra)
library(magick)
library(ggforce)
library(ggtext)
library(cowplot)

#Read file
characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/characters.csv')

#Filter & select
data <- characters %>%
  filter(visible_tears_number_of_panels > 1)%>%
  select(2, contains("tear"))%>%#select column with name it in
  group_by(character)%>%
  mutate(totalcry=sum(visible_tears_number_of_intances))%>%
  distinct(character, .keep_all=T)

#Change names
data$character[data$character=="Rogue = Name Unknown"] <- "Rogue"
data$character[data$character=="Longshot = (unknown real name)"] <- "Longshot"

#Add image
image <- image_read("/Users/silaarts/Desktop/eye.png")
eye <- grid::rasterGrob(image, interpolate = T) 

#Calculate drops (top of it)
data1 <- data %>%
  rowwise() %>% 
  mutate(
    #Triangle 'falling'
    x1 = list(c(-totalcry/5.8, 0.15, totalcry/5.74)),
    y1 = list(c(0.2, totalcry/3, 0.2))
  ) %>% 
  unnest(c(x1, y1))

#Choose font
font_add_google("Notable", "N")
font_add_google("Jacques Francois Shadow", "J")
font_add_google("Quantico", "Q")
showtext_auto()

#Theme for plots
theme <- theme_void() +
  theme(
    strip.text = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "gray87", colour = "gray87"),
    #strip.background = element_rect(fill = "grey28"),
    #panel.background = element_rect(fill = "grey28", color = "grey28"),
    plot.caption = element_text(colour = "black", size = 9, hjust = 0.5, margin = margin(5, 10, 15, 5), family = "Q")
  )

#

#Explanation
title <- ggplot() +
  labs(x = NULL, y = NULL,
       title = "X-MEN DO CRY",
       subtitle = "Top 'crybabies': bigger teardrops are indicative for more 'showers'.\nWith 10 instances, Kitty Pryde cries most often.")+
  theme_void() +
  theme(
    strip.text = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "gray87", colour = "gray87"),
   #strip.background = element_rect(fill = "gray90"),
    #plot.margin = unit(c(2,2,2,2), "cm"),
    plot.title = element_text(color="black", size=50, hjust=0.5, margin = margin(15, 0, 5, 0), family="J"),
    plot.subtitle = element_text(colour = "black", size = 20, hjust= 0.5, margin = margin(5, 0, 5, 0), family = "O"),
)

#GGplot
p1 <- ggplot(data1) +
  #polygons
  geom_polygon(aes(x1, y1), fill = "black")+
  geom_circle(aes(x0=0, y0=0, r=totalcry/6), fill="black", color="black")+
  annotation_custom(eye, xmin= -8, xmax= 8, ymin= 5, ymax= 7)+
  geom_text(aes(0,-3, label = paste0(character)), hjust = 0.5,  colour = "black", size = 2.5, family="N") +
  scale_x_continuous(limits=c(-10,10))+
  scale_y_continuous(limits=c(-5,8))+
  facet_wrap(~ character, nrow = 3, ncol=3)+
  labs(caption = "Source: Claremont Run | Wikipedia - Uncanny X-Men | Plot: @sil_aarts")+
  theme

#Run quartz
quartz()

#Combine plots
plots <- plot_grid(title, p1, nrow=2, ncol=1, rel_heights = c(1.5,6), rel_widths = c(6,6))

#Run it!
plots
