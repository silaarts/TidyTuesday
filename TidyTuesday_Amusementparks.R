#TidyTuesday
#===============================================================================
#Amusement parks
#@sil_aarts
#===========================================================================

#Load libraries
library(ggplot2)
library(dplyr)
library(harrypotter)
library(tidyverse)
library(showtext)
library(showtextdb)
library(ggtext)

#Load file
injuries <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/tx_injuries.csv")

#Check data
unique(injuries$injury_report_rec)
unique(injuries$name_of_operation)

#Count injuries per operation
injuries$name_of_operation <- as.factor(injuries$name_of_operation)
data1 <- injuries %>%
  group_by(name_of_operation) %>%
  mutate(count_op = n())

#Calculate Females
data2 <- data1 %>%
  filter(gender == "F") %>%
  group_by(name_of_operation, gender) %>%
  mutate(count_gender = n())

#Select operations with the most incidents
data3 <- data2 %>%
  filter(name_of_operation== "Splashtown - Spring, TX" | 
          name_of_operation=="Wonderland Amusement Park" |
          name_of_operation== "Great Wolf Lodge" |
          name_of_operation== "Six Flags - Hurricane Harbor" |
          name_of_operation== "Six Flags Over Texas")

#Select some rows
data4 <- data3[c(1,2,8,9,15), ]

#Make some colours
colors <- "#084D49FF"
colors2 <- "#830042FF"

#Theme
theme_sil <- theme_minimal() +
  theme(
    text = element_text(family="serif"),
    line = element_blank(),
    plot.background = element_rect(fill = "black", color = NA),
    axis.text = element_text(size = 15, color="grey70"), 
    panel.grid.major.y = element_line(color="grey70"),
    panel.grid = element_blank(),
    plot.caption = element_markdown(size = 13, color = "grey70", hjust=0)
  )

#GGplot: point
p <- ggplot(data4, aes(x= name_of_operation, y= count_op))+
  geom_point(colour = colors, shape= 11, size=10)+
  scale_y_continuous(breaks=seq(0.0, 100, 20))+
  geom_text(aes(x=name_of_operation, y= count_op, label= "T"), size=5, color="white", fontface="bold") +
  #Females 
  geom_point(aes(x=name_of_operation, y= count_gender), shape=11, color=colors2, size=10)+
  #Insert the first letter in geom_point
  geom_text(aes(x=name_of_operation, y= count_gender, label=substr(gender, 1, 1)), size=5, color="white", fontface="bold")+
  #Title
  annotate("rect", xmin = 5.8, xmax = 6, ymin = 0, ymax = 100, alpha = .8, colour= colors, fill=colors)+
  geom_text(aes(x= 5.9, y= 50, label= "Top 5 amusement parks at which the most injuries happened"), alpha= 0.8, angle=90, fontface="bold", size=6, colour="white", family= "serif")+
  #Line
  annotate("rect", xmin = 1, xmax = 2.5, ymin = 110, ymax = 110, alpha = .2, colour= "white")+
  annotate("rect", xmin = 3.2, xmax = 4.8, ymin = 110, ymax = 110, alpha = .2, colour= "white")+
  #Info 1
  annotate(geom="text", x= 0.9, y = 118, label = "92", size= 25, fontface="bold", color=colors2, alpha=0.8)+
  geom_text(aes(x= 1, y = 118), label = "In total, 92 incidents involving injuries\nhappened at Six Flags Over Texas",  alpha= 0.2, fontface="bold", size=6, family= "serif", hjust=0, color="white")+
  #Info 2
  annotate(geom="text", x= 3.1, y = 118, label = "50%", size=25, fontface="bold", color=colors2, alpha=0.8)+
  geom_text(aes(x= 3.2, y = 118), label = "Of all 24 incidents at Splashtown - Spring, TX\n50% involved females", alpha= 0.2, fontface="bold", size=6, family= "serif", hjust=0, color="white")+
  #Extra info: T
  annotate(geom="text", x= "Splashtown - Spring, TX", y = 85, label = "Total number of incidents", size= 6, family= "serif", alpha= 0.2, fontface="bold", color="white")+
  annotate("segment", x= "Splashtown - Spring, TX", xend = "Six Flags Over Texas", y = 92, yend = 92, size= 0.5, linetype="dashed", color=colors)+
  geom_point(aes(x="Splashtown - Spring, TX", y= 92), shape=21, size=5, color=colors, fill="black", stroke = 5)+
  #Extra info: F
  annotate(geom="text", x= "Splashtown - Spring, TX", y = 55, label = "Total number of incidents including females", alpha= 0.2, size= 6, family= "serif", fontface="bold", color="white")+
  annotate("segment", x= "Splashtown - Spring, TX", xend = "Six Flags Over Texas", y = 47, yend = 47, size= 0.5, linetype="dashed", color=colors2)+
  geom_point(aes(x="Splashtown - Spring, TX", y= 47), shape=21, size=5, color=colors2, fill="black", stroke = 5)+
  #labs 
  labs(caption = "Source: Saferparks | Plot by: <span style='color:white'>**@sil_aarts**</span>" )+
  theme_sil

#Run it!
p 
