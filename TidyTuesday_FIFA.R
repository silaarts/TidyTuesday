#TidyTuesday
#FIFA!
#Why? Because I love soccer

#Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggmap)
library(zipcode)
library(ggthemes)
library(extrafont)
library(emoGG)

#Read file
fifa <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-06-12/week11_fifa_audience.csv")

#Delete first column
fifa$X1 <- NULL

#Select multiple countries which the Dutch still have matches against in 2019
selection <- c("Netherlands", "United Kingdom", "Germany", "Estonia", "Belarus")
fifa2 <- filter(fifa, country %in% selection)

#Estonia is all NULL so delete that row!
fifa3 <- fifa3[-c(5),]

#GGplot
p <- ggplot(fifa3, aes (country, tv_audience_share)) + geom_emoji(emoji="26bd")+
  ggtitle(label = "TidyTuesday (2018): World Cup 2010 TV audience", subtitle="The Dutch team and some countries we still have to play against in 2019")+
  xlab("")+ ylab("Tv viewership (% of population)")+labs(caption="Source: fivethirtyteight.com | Plot by: @sil_aarts")+
  coord_flip()+
  theme_tufte()+
  theme(panel.background = element_rect(fill = "darkgreen"),
        text=element_text(family="Comic Sans MS"),
        plot.title = element_text(color="black", face="bold", size=16, hjust=0),
        plot.subtitle=element_text(size=14, hjust=0, face="italic", color="black"),
        plot.caption= element_text(size=12, hjust=1, color="black"),
        legend.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x= element_text(size=10, vjust = 0.5, hjust=1, face='bold', colour='black'),
        axis.text.y= element_text(size=14,face='bold', colour='black'),
        axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = 0.5, vjust = 1, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = 0.5, vjust = 1, face = "bold"))
  
#Run it!
p
