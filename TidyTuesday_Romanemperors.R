#TidyTuesday
#===============================================================================
#Roman Emperors
#@sil_aarts
#===========================================================================

#install package for BC time
install.packages("zoo")

#Load libraries
library("shiny")
library("ggplot2")
library("scales")
library("DT")
library("dplyr")
library("shinydashboard")
library("zoo")
library("tidyverse")

#Load file
data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

#Calculate time between dates
data$birth_y <- as.numeric(as.yearmon(data$birth))
data$death_y <- as.numeric(as.yearmon(data$death))
data$age_death <- data$death_y - data$birth_y

#4 dates are BC, so: death+birth
data$age_death <- ifelse(data$index== 1 | data$index==2 | data$index==4 | data$index==6, data$death_y + data$birth_y, data$death_y - data$birth_y)
data$age_death <- round(data$age_death,1)

#Make z-scores
data$age_zscore <- ave(data$age_death, FUN=scale)
#Make a 0/1 variable: above & below average
data$age_groups <- ifelse(data$age_zscore < 0, "below", "above")

#Check mean age of death: 68
data %>%
  summarise(mean_age = mean(age_death), n=n())

#Own theme
theme_sil <- theme_light() + 
  theme(
    text = element_text(size = 10, family="Courier", colour="white"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "black"),
    plot.title=element_text(size=18, hjust=0, face='bold', colour="white", lineheight = 1),
    plot.subtitle=element_text(size=18, hjust=0, colour="white"),
    plot.caption=element_text(size=8, hjust=1, colour="white"),
    axis.text = element_text(size = 12, colour="white", face="bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.y = element_blank())


#GGplot
p <- data %>%
  ggplot(aes(x= name, y= age_zscore, label= age_zscore)) + 
  geom_segment(aes(y = 0, 
                   x = name, 
                   yend = age_zscore, 
                   xend = name))+
  geom_point(stat= "identity", aes(colour= age_groups), size=6)+ 
  scale_colour_manual(name="Average age of death: 68", 
                      labels = c("Above Average","Below Average", "Average"), 
                      values = c("darkred","pink", "red"))+
  annotate(geom="label", x="Marcus Aurelius", y = -1.6, label = "Gordian III\nAt the age of 13\nruler of Rome (238-244).\nDied in the Roman-Sassanidische war\nat the age of 19.", family= "Courier", vjust=0.1, size=3, fontface="bold")+
  annotate("segment", x="Macrinus", xend = "Gordian III", y = -1.83, yend = -1.83 ,size=0.5, colour= "white", arrow=arrow(length=unit(.2, "cm")))+
  annotate(geom="label", x="Philip I", y = 1.6, label = "Gordian I\nRuler of Rome for only 21 days \n during 238. He committed suicide at\n the Battle of Carthage.\nHe was 79 years old.", family= "Courier", vjust=0.1, size=3, fontface="bold")+
  annotate("segment", x="Pertinax", xend = "Gordian I", y = 1.69, yend = 1.69 ,size=0.5, colour= "white", arrow=arrow(length=unit(.2, "cm")))+
  annotate(geom="label", x="Hadrian", y = 0, label = "Average age of death\n68 years of age.", family= "Courier", vjust=0.1, size=3, fontface="bold")+
  annotate(geom="label", x="Titus", y = -1.4, label = "Valentinian II\nWas found hanged in 392 in his house in Vienna.\nHe was 21 years old.", family= "Courier", vjust=0.1, size=3, fontface="bold")+
  annotate("segment", x="Trajan", xend = "Valentinian II", y = -1.7, yend = -1.7 ,size=0.5, colour= "white", arrow=arrow(length=unit(.2, "cm")))+
  annotate(geom="label", x="Galerius", y = 0.65, label = "Claudius\nKilled by a poisonous feather\nbecause his wife wanted him death.\nHe was 64 years old.", family= "Courier", vjust=0.1, size=3, fontface="bold")+
  annotate("segment", x="Geta", xend = "Claudius", y = 0.81, yend = 0.81 ,size=0.5, colour= "white", arrow=arrow(length=unit(.2, "cm")))+
  guides(colour ="none")+
  labs(title="TidyTuesday: Roman Emperors", 
       subtitle="To live or die?",
       caption="Source: Wikipedia | Plot by: @sil_aarts",
       y="Age of death (z-scores)")+
  ylim(-2,2)+
  coord_flip()+
  theme_sil

#Run it!
p
