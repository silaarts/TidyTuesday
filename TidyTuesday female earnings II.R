#TidyTuyesday
#Female earnings II

#Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gganimate)
library(extrafont)
library(readr)

#Read file
female <- read.csv("Desktop/employed_gender.csv")

#Subset
female2<- subset(female, year %in% c(1970, 1980, 1990, 2000, 2010))

#Create new data.frame
female3 <- data.frame(part_time = c(female2$part_time_female, female2$part_time_male))
female3$sex <- ifelse(female3$part_time < 20,  c("male"), c("female")) 
female3$year <- c(1970, 1980, 1990, 2000, 2010, 1970, 1980, 1990, 2000, 2010)

#Create colours
fill <- c("darkmagenta", "darkkhaki")

#GGplot- bar
p <- ggplot(female3, aes(x=year, y=part_time, fill=sex)) + 
  geom_col(position="dodge")+
  scale_fill_manual(values=fill)+
  ggtitle(label = "TidyTuesday: women in the workplace", subtitle="Are you working part-time?")+
  xlab("")+ ylab("% of employed")+labs(caption="Source: Bureau of Labor,  Plot by @sil_aarts")+
  theme(panel.background = element_rect(fill = "white"),axis.line = element_line(size=1, colour = "black"),
        plot.title = element_text(color="black", face="bold", size=14, hjust=0),
        plot.subtitle=element_text(size=13, hjust=0, face="italic", color="black"),
        plot.caption= element_text(size=10, hjust=1, color="azure4"),
        legend.title = element_blank(),
        axis.text.x= element_text(size=10,angle = 90, vjust = 0.5, hjust=1, face='bold', colour='black'),
        axis.text.y= element_text(size=10,face='bold', colour='black'),
        axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = 0.5, vjust = 1, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = 0.5, vjust = 1, face = "bold")
  )

#Check it
p

#Animate it
p+ transition_states(year,wrap=F) +
  shadow_mark()
  
#Save it!
animate(p, height = 600, width =600)
anim_save("Desktop/R/TidyTuesday/Female_earningsII.gif")