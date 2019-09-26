#TidyTuesday
#===============================================================================
#School diversity
#@sil_aarts
#===========================================================================


#Load libraries
library(ggplot2)
library(dplyr)
library(harrypotter)
library(ggalt)
library(showtext)
library(showtextdb)
library(ggimage)
library(magick)
library(ggtext)
library(grid)
library(gridExtra)

#Load file 
data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")

#Filter file
data1 <- data %>%
  filter(SCHOOL_YEAR== "2016-2017")

data2 <- data1 %>%
  filter(Asian > 10 & Black > 10 & Hispanic >10 & White > 10 & Multi > 5)

#Round up!
data3 <-  data2 %>%
  mutate_if(is.numeric, round)
#Add colum: difference
data3$diff <- (data3$White - data2$Black)
data3$diff <- round(data3$diff)

#Making some colours: #084D49FF #73C1C4FF #830042FF #6D0036FF #852E19FF #FFA000FF
colors <- hp(3, option = "RonWeasley")

#Choose font
font_add_google("Shadows into Light", "Tr")
showtext_auto()

#Add theme
theme <- theme_minimal() +
  theme(
    text = element_text(family="Tr"),
    plot.background = element_rect(fill = "grey44", color = NA),
    axis.text = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 32, hjust = 0, colour="Black", face="bold"),
    plot.subtitle = element_markdown(size = 22, hjust = 0),
    plot.caption = element_text(hjust = 1, size = 9, colour="black"))
  
#Run quartz for showtext
quartz()

#GGplot: dumbbell
p <- ggplot(data3, aes(x=White, xend=Black, y=LEA_NAME, group=LEA_NAME)) + 
  geom_dumbbell(aes(x=White, xend=Black, y=LEA_NAME), color="#73C1C4FF", colour_x = "#852E19FF",colour_xend = "#6D0036FF", size=6) + 
  scale_x_continuous(expand=c(0,0), limits=c(0, 60))+
  labs(x=NULL,  y=NULL, title="Top 35 schools in the U.S. with the most diversity (2016-2017)*", 
       subtitle="Even in schools with a lot of diversity, there are rather large <br>
       <span style='color:#73C1C4FF'>**differences**</span> in percentage of <span style='color:#852E19FF'>**white**</span> and <span style='color:#6D0036FF'>**black**</span> students", 
       caption= "Source: National Center for Education Statistics (NCES) | Plot by @sil_aarts") +
  #Extra information: labels 
  geom_text(aes(x=White, label=White), color="black", size=4, hjust=0.4,  fontface="bold", family="Tr")+
  geom_text(aes(x=Black, label=Black), color="black", size=4, hjust=0.4, fontface="bold", family= "Tr")+
  #Extra information: difference 
  geom_point(aes(x= 4, y= LEA_NAME), size= 7, colour="#73C1C4FF")+
  geom_text(aes(label=diff, x=4, y=LEA_NAME), fontface="bold", size=5, family="Tr", colour="black")+
  geom_rect(aes(xmin=2, xmax=6, ymin=-Inf, ymax=Inf), fill="transparent", colour="black", size=1)+
  #Extra information: title
  geom_text(aes(x=1, y=13, label="*Top 35 is based on at least 10% White, Black, Hispanic and Asian students and 5% of students of multiple ethnicities."), size=4, color="black", family="Tr")+
  coord_flip()+
  theme

#Run it!        
p


