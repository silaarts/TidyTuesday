#TidyTuyesday
#Female earnings
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gganimate)
library(gapminder)
library(RColorBrewer)
library(extrafont)
library(readr)

#Read file
female <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/earnings_female.csv")

#Year to unique
year2 <- unique(female$Year)

#GGplot - geom_area
p <- ggplot(female, aes(x = Year, y = percent, group=group, color=group))+
  geom_line(aes(x=Year, y=percent))+
  scale_color_manual(values = c("grey", "grey", "pink", rep("gray", 4), "black"))+
  geom_point(aes(colour=group), size=2)+   
  ggtitle(label = "TidyTuesday: Women in workplace", subtitle="Female salary percent of male salary")+
  xlab("Year")+ ylab("Percentage")+labs(caption="Source: Bureau of Labor,  Plot by @sil_aarts")+
  theme(panel.background = element_rect(fill = "white"),axis.line = element_line(size=1, colour = "black"),
        plot.title = element_text(color="black", face="bold", size=16, hjust=0),
        legend.title = element_blank(),
        plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black"),
        plot.caption= element_text(size=8, hjust=1, color="azure4"),
        axis.text.x= element_text(size=10,angle = 90, vjust = 0.5, hjust=1, face='bold', colour='black'),
        axis.text.y= element_text(size=10,face='bold', colour='black'),
        axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = 0.5, vjust = 1, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = 0.5, vjust = 1, face = "bold"),
  )

#Check it
p 

#GGplot save
ggsave("Desktop/R/TidyTuesday/Female_earnings.png")
