#TidyTuesday
#Anime-animated

#Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gganimate)
library(extrafont)

#Load data
anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

#Delete some columns to make some space and delete all NA
anime$synopsis <- NULL
anime$background <- NULL
anime2 <- na.omit(anime)

#Filter only unique names
anime3 <- unique(anime2[c("name", "score","rank","start_date", "members","studio")])

#Make start_date, year only
anime3$year <- format(anime3$start_date, "%Y")
anime3$year <- as.numeric(anime3$year)

#Change members variabele
anime3$members <- anime3$members/1000

#GGplot: scatter
p <- ggplot(anime3, 
  aes(x = members, y=score, size = rank, colour =studio)
) +
  geom_point(show.legend = F, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 10)) +
  labs(x = "# of members (x1000)", y = "Score", caption = "Source: MyAnimeList | Plot by @sil_aarts",
       title = "TidyTuesday: Anime scatterplot \n Relation between # of members and score (by rank and studio) in {round(frame_time)}")+
  shadow_mark(colour="black", size = 1)+ 
  theme_bw()+
  theme(text=element_text(family="Times New Roman", face="bold", size=12))+
  transition_time(year) 
  
#Run animation
p2 <- animate(p, nframes = 150, fps=3)

#Save it!
anim_save("Desktop/R/TidyTuesday/Anime.gif", p2)
