#TidyTuesday
#===============================================================================
#Beer!
#@sil_aarts
#===============================================================================
library(tidyverse)
library(ggtext)
library(cowplot)
library(showtext)
library(showtextdb)
library(wesanderson)

#Read file
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')

#Total golden models > 5
data1 <- data %>%
  filter(medal=="Gold")%>%
  group_by(year, state)%>%
  mutate(count = n())%>%
  distinct(year, state, count)%>%
  filter(year > 1999)%>%
  filter(count > 5)

#Total golden medals per year
data2 <- data%>%
  filter(medal=="Gold" & year > 1999)%>%
  group_by(year)%>%
  mutate(count2 = n())%>%
  distinct(year, count2)

#Total golden medals 2000-2020
data3 <- data%>%
  filter(year >1999)%>%
  filter(medal=="Gold")
 
#Get some fonts
font_add_google("Unica One", "P")
font_add_google("Sigmar One", "S")
showtext_auto()

#Colors
col <- wes_palette("IsleofDogs2", 8, type = "continuous")

#Theme
theme_sil <- theme_void()+
  theme(
    legend.position = "bottom",
    legend.text = element_text(size=8, colour="black", family="P"),
    legend.key.size = unit(0.2,"line"),
    legend.key.width = unit(3,"line"),
    legend.background = element_rect(fill="#CEA912", colour ="#CEA912"),
    legend.title= element_blank(),
    strip.text.x = element_text(family="B", size= 7, colour="white", margin = margin(.1, 0, .1, 0, "cm")),
    strip.background = element_rect(fill="transparent", colour="black", size=1),
    plot.background = element_rect(fill = "#CEA912", colour = "#CEA912"),
    panel.background = element_rect(fill = "#CEA912", colour = "#CEA912"),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.length.y =unit(-0.3, "cm"),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(color="black", size=8, hjust=0.5, margin = margin(15, 1, 5, 1), family="P"),
    plot.title = element_text(color="black", size=20, hjust=0.5, margin = margin(5, 2, 2, 2), family="S"),
    plot.subtitle = element_markdown(color="black", size=8, hjust=0.5, family="P"),
    plot.margin = unit(c(1,1,0,1), "cm"))

#GGplot
p <- ggplot(data1, aes(x = year, y = count)) + 
  geom_col(aes(fill=state)) + 
  labs(title=" - BEER AWARDS -",
       subtitle= "The Professional Judge Panel awards gold, silver or bronze medals <br> that are recognized around the world as symbols of brewing excellence.<br> These awards are among the most coveted in the industry and heralded by the <br> winning brewers in their national advertising. <br> Below the gold medals for US states <span style='color:#474439'>**(total)**</span> are displayed.<br> Individual states with specific years that include more than <br> 5 golden medals are also displayed.",
       caption="Source: | Plot by @sil_aarts") +
  scale_fill_manual(values = col)+
  geom_text(data = data1, aes(x = year, y = 0, label = glue("{year}  ")), hjust=1, size = 1.8, colour="black", family="P") +
  #Add ticks
  geom_text(data = data1, aes(x = 2022, y = 0, label = "0"), hjust = 0.8, size = 2, colour="black", family="P") +
  geom_text(data = data1, aes(x = 2022, y = 20, label = "20"), hjust = 0.8, size = 2, colour="black", family="P") +
  geom_text(data = data1, aes(x = 2022, y = 40, label = "40"), hjust = 0.8, size = 2, colour="black", family="P") +
  geom_text(data = data1, aes(x = 2010, y = 55, label = "1.323 Golden medals \nawarded since 2000"), hjust = 0.8, size = 3, colour="black", family="S") +
  #Add informayion
  coord_polar(theta = "y", start=0, clip="off")+
  scale_x_continuous(limits = c(1995, 2023), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 80)) +
  theme_sil

#Run it!
p

#GGplot2
p2 <- ggplot(data2, aes(x = year, y = count2)) + 
  geom_col(width = 1, colour="#CEA912",  fill="#474439")+
  scale_y_reverse(breaks=c(0,40,80), position = "right")+
  coord_flip()+
  theme_void()+
  theme(
    plot.background = element_rect(fill = "#CEA912", colour = "#CEA912"),
    panel.background = element_rect(fill = "#CEA912", colour = "#CEA912"),
    axis.text.x = element_text(size=6, colour="black", family="P"),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

#Run it!
p2

#Combine plots
p3 <- ggdraw(p) + 
  draw_plot(p2, x=0.305, y=0.465, width = 0.17, height = 0.23) 

#Run it!
p3 
