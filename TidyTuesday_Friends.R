
#TidyTuesday
#===============================================================================
#TidyTuesday
#Friends
#@sil_aarts
#===============================================================================
#Load libraries
library(tidyverse)
library(showtext)
library(showtextdb)
library(ggforce)
library(ggtext)
library(magick)
library(cowplot)
library(grid)

#Read file
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
unique(friends_emotions$emotion)

#Count emotions
data_fr <- friends_emotions %>%
  group_by(season, episode, scene) %>%
  count(emotion)%>%
  ungroup() %>%
  select(1,4:5)%>%
  group_by(season, emotion)%>%
  mutate(som_season = sum(n))%>%
  select(-3)%>%
  unique()%>%
  ungroup()

#Add groups
data_fr$som_season <- as.numeric(data_fr$som_season)
data_fr$season <- as.factor(c( rep('A', 7), rep('B', 7), rep('C', 7), rep('D', 7)))

#Create some room so groups are more apparent
empty_bar <- 2
to_add <- data.frame(matrix(NA, empty_bar * nlevels(data_fr$season), ncol(data_fr)))
colnames(to_add) <- colnames(data_fr)
to_add$season <- rep(levels(data_fr$season), each=empty_bar)
friends <- rbind(data_fr, to_add)
friends <- friends %>% 
  as.data.frame %>%
  arrange(season)
friends$id <- seq(1, nrow(friends))

#Get some text
label_datafr <- friends

#Make the angles for the text so that the text runs around the polar
number_of_bar <- nrow(label_datafr)
angle <- 90 - (360*(label_datafr$id-0.3) /number_of_bar)     
label_datafr$hjust <- ifelse(angle < -90, 1, 0)
label_datafr$angle <- ifelse(angle < -90, angle-180, angle)

#Choose font
font_add_google("Caveat", "M")
font_add_google("Architects Daughter", "A")
showtext_auto()

#Theme
theme_sil <- theme_void()+
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black", color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.x = element_blank(),
    plot.title = element_text(color="white", size=60, hjust=0.5, margin = margin(15, 0, 0, 0), family="M"),
    plot.subtitle = element_markdown(color = "white", size = 18, hjust=0.5, margin = margin(10, 0, 0, 0)),
    plot.caption = element_text(color = "white", size = 7, hjust= 0.5, margin = margin(0, 0, 0, 0), family = "A"),
    plot.margin = unit(c(0,0,0.5,0), "cm"))

#GGplot: bar with coord_polar
p <- ggplot(friends, aes(x=as.factor(id), y=som_season, fill=season))+
  geom_bar(stat="identity", width = 1, alpha=0.8, colour= "white")+
  #Add text
  geom_text(data=label_datafr, aes(x=id, y=som_season+320, label= paste(emotion), hjust=hjust), color="white", size=3.7, angle= label_datafr$angle, inherit.aes = FALSE, family="A")+ 
  geom_text(data=label_datafr, aes(x=id, y=som_season+70, label= paste(som_season), hjust=hjust), color="white", size=2, angle= label_datafr$angle, inherit.aes = FALSE, family="A")+ 
  #Cosmetics
  labs(title= "Friends",
       subtitle = "Emotions uttered in season <span style='color:yellow1'>**1**</span> , <span style='color:green'>**2**</span> , <span style='color:deepskyblue2'>**3**</span> and <span style='color:red'>**4**</span>.",
       caption= "Source: Friends (R package) | Plot by @sil_aarts")+
  coord_polar(start=0)+
  scale_y_continuous(limits=c(-1000, 2000))+
  scale_fill_manual(values=c("yellow1", "green", "deepskyblue2", "red"))+
  theme_sil

#Run quartz
quartz()
  
#Run it!
p

#Add image
image <- image_read("/Users/silaarts/Desktop/friends.png")

#Put image on plot
p2 <- ggdraw(p) + 
  draw_image(image, x=0.435, y=0.35, width = 0.13, height = 0.2)

#Run it!
p2          
