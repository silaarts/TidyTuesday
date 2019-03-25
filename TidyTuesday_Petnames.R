#TidyTuesday
#Pet names!

#Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(readr)

#Read file
seattle_pets <- read.csv2("Desktop/Seattle_Pet.csv", stringsAsFactors=F, header=T)

#Keep only one col> name
pets <- seattle_pets[c(3)]

#Change colname
colnames(pets) <- c("name")

#Insert count
pets2 <- pets %>% count(name, sort=T)

#If vector is empty: insert "Nameless pet"
for (i in 1:nrow(pets2))
{
  if (pets2$name[i]== "") { pets2$name[i] <- "Nameless pet :-("}
}

#Filter top 10 names in Seattle
pets3 <- pets2 %>%
  filter(n>227)

#Get percentages
pets4 <- pets3 %>%
  group_by(name) %>%
  summarise(n) %>%
  mutate(percentages = n/sum(n)*100)

#Percentage based on one decimal (round)
pets5 <- pets4 %>% 
  mutate_if(is.numeric, round, digits = 1)

#GGplot: pie chart 
p <- ggplot(pets5, aes(x = "", y=n , fill = factor(name))) + 
    geom_col(aes(fill = name), width = 1) +
    geom_text(aes(label = percentages), position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette="Set3")+
    coord_polar("y")+
    labs(fill="Pet name", 
       x=NULL, 
       y=NULL, 
       title="TidyTuesday: 'Sleepless (pets) in Seattle'", 
       subtitle="The 10 most used pet names in %",
       caption="Source: Seattle.gov | Plot by: @sil_aarts")+
    theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x=element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      plot.title=element_text(size=20, face="bold"),
      plot.subtitle=element_text(size=18, face='italic')
  )

#Run it!
p

