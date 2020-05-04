#TidyTuesday
#===============================================================================
#Broadway
#@sil_aarts
#===========================================================================

#Load libraries
library(ggplot2)
library(dplyr)
library(ggalt)
library(showtext)
library(showtextdb)
library(ggimage)
library(magick)
library(ggtext)
library(grid)
library(gridExtra)

#Load file 
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)

#Filter
data1 <- data %>%
  filter(show =="Harry Potter and the Cursed Child, Parts One and Two")

#If two entrees per week, select the average
data2 <- data1 %>%
  group_by(show,week_ending,week_number)%>%
  summarise(mean_avg_ticket_price = mean(avg_ticket_price, na.rm=T))
#Check unique
unique(data1$week_number)

#Choose font
font_add_google("Abril Fatface", "A")
font_add_google("Amatic SC", "C")
showtext_auto()

#Run quartz for showtext
quartz()

#Add theme
theme <- theme_minimal() +
  theme(
    text = element_text(family="C"),
    plot.background = element_rect(fill = "black", color = NA),
    axis.text = element_text(siz=15, face="bold", colour="white"),
    axis.title.x = element_text(siz=15, colour="white"),
    axis.title.y = element_text(siz=15, colour="white"),
    panel.grid.major.y = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 45, colour="darkgoldenrod4", face="bold", hjust=0.5, family="A"),
    plot.subtitle = element_markdown(size = 20, colour="darkgoldenrod4", hjust=0.5),
    plot.caption = element_text(hjust = 1, size = 9, colour="darkgoldenrod4", family="A"))

#GGplot: dumbbell
p <- ggplot(data2, aes(x=week_ending, y=mean_avg_ticket_price)) + 
  geom_segment( aes(x=week_ending, xend=week_ending, y=0, yend=mean_avg_ticket_price), color="white", linetype=6) +
  geom_point(color="darkgoldenrod4", size=4, shape=23, fill="#4C8076") +
  labs(x="",  y="Average ticket price per week ($)", title="\n*** BROADWAY ***", 
       subtitle="<span style='color:#4C8076'>**Harry Potter and the Cursed Child, Parts One and Two**</span> <br> 
       <span style='color:#F7F4FC'>Average ticket price per week since the premiere at  Lyric Theatre </span>", 
       caption= "Source: Playbill | Plot by @sil_aarts") +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")+
  coord_flip()+
  #Annotation: premiere
  annotate(geom="label", x=as.Date("2018-04-08"), y = 190, label = "Lyric Theatre's\nfirst week of HP show\nended on March 25th, 2018.", family= "C", size=3.5)+
  annotate("segment", x=as.Date("2018-03-25"), xend = as.Date("2018-03-25"), y = 160, yend = 190 ,size=0.2, colour= "white", arrow=arrow(length=unit(.3, "cm")))+
  annotate(geom="label", x=as.Date("2019-12-29"), y = 190, label = "Week after christmas 2019", family= "C", size=3.5)+
  annotate("segment", x=as.Date("2019-12-295"), xend = as.Date("2019-12-29"), y = 160, yend = 190 ,size=0.1, colour= "white", arrow=arrow(length=unit(.3, "cm")))+
  
theme


#Run it!        
p
