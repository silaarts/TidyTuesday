
#TidyTuesday
#===============================================================================
#Coffee
#@sil_aarts
#===============================================================================
#Load libraries
library(tidyverse)
library(LaCroixColoR)
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
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')
names(data)[4] <- "country"

unique(data$harvest_year)

#Calculate coffee per country
data1 <- data %>%
  group_by(country)%>%
  mutate(mean_points=mean(total_cup_points))%>%
  mutate(rounded = round(mean_points, 1))%>%
  distinct(country,.keep_all = T)%>%
  drop_na(country)


#Make the coffee
data2 <- data1 %>%
  rowwise() %>% 
  mutate(
    x1 = list(c(-20, -20, 20, 20)),
    y1 = list(c(0, mean_points, mean_points, 0)),
  ) %>% 
  unnest(c(x1, y1))

#Change names
data2$country[data2$country== "Cote d?Ivoire"] <- "Cote de Ivoire"
data2$country[data2$country== "Tanzania, United Republic Of"] <- "Tanzania"

#Choose font
font_add_google("Coiny", "R")
font_add_google("Nova Mono", "N")
showtext_auto()

#GGplot
p <- ggplot(data2)+
  geom_polygon(aes(x1, y1), fill = "chocolate4", alpha = 0.7)+
  geom_rect(xmin = -20, xmax = 20, ymin = 0, ymax = 100,  colour="white", fill = "transparent", size=1)+ 
  annotate("curve", x= 20, xend= 20, y=0, yend=95, colour="white", size=1.5)+
  geom_text(aes(x=0, y = 50, label = paste(rounded)), family= "R", vjust=0.1, size=4, fontface="bold")+
  geom_text(aes(x=0, y = -20, label = paste(country)), family= "R", vjust=0.1, size=3, fontface="bold")+
  scale_y_continuous(limits=c(-30, 110))+
  scale_x_continuous(limits=c(-70,70))+
  labs(x = NULL, y = NULL,
       title = "Where to go for a coffee?",
       subtitle = "Countries & their average total cup points (0-100 scale) in 2009-2018.\nThe amount of 'coffee in a cup' is related to the acquired points.",
       caption = "Source: James LeDoux & Coffee Quality Database - Yorgos Askalidis - TWD| Plot by @sil_aarts")+
  facet_wrap(~ country)+
  theme_void() +
  theme(
    legend.position = "none",
    #legend.text = element_text(colour="white", size=10),
    strip.text = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "gray71", colour = "gray71"),
    #panel.spacing.x = unit(1, "lines"),
    #plot.margin = margin(2, 2, 2, 2, "cm"),
    plot.caption = element_text(colour = "black", size = 8, hjust = 0.5, margin = margin(5, 10, 10, 5), family = "N"),
    plot.title = element_text(color="black", size=40, hjust=0.5, margin = margin(20, 0, 5, 0), family="R"),
    plot.subtitle = element_text(colour = "black", size = 15, hjust= 0.5, margin = margin(20, 0, 15, 0), family = "N"))

#Run quartz
quartz()

#Run it!
p
