#TidyTuesday
#===============================================================================
#Movie Revenues
#@sil_aarts
#===============================================================================

#Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)
library(LaCroixColoR)

#Read data
media <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

#Select some of the best movies!
media2 <- media %>%
  filter(franchise=="Shrek" | franchise=="Frozen" | franchise=="Cars") 

#Change colnames for ease of use and clearity plotly
colnames(media2) [1:3] <- c("Title","Category","Revenue")

#Colours
col2 <- lacroix_palette("PassionFruit", n = 4, type = "continuous")

#Start using my own theme
theme_sil <- theme_void() + 
  theme(
    text = element_text(size = 10, family="Courier", colour="white"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "black"),
    plot.title=element_text(size=18, hjust=0, face='bold', colour="white", lineheight = 1),
    plot.subtitle=element_text(size=18, hjust=0, colour="white"),
    plot.caption=element_text(size=8, hjust=1, colour="white"),
    axis.text = element_text(size = 12, colour="white", face="bold"))


#Plotly
p <- ggplot(media2, aes(x=Title, y=Revenue, fill=Category)) +
  geom_col(alpha=0.8) +
  labs(title= "TidyTuesday: revenues of three great movies (in Billion $)")+
  scale_fill_manual(values=col2)+
  coord_flip()+
  theme_sil


#Run it!
p1 <- ggplotly(p) %>% 
  layout(annotations = 
           list(x = 1, y = -0.05, text = "Source: Wikipedia, Media Frenchise Revenues | Plot by @sil_aarts.", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=12, color="white"))
  )
p1
