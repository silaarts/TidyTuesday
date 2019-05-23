#TidyTuesday
#===============================================================================
#Mismanaged waste around the worls; waste no more...
#@sil_aarts
#===============================================================================

#Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(extrafont)

#Load data
data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")

#Take only with per capita plastic waste: omit the rest, so leaving only data for 2010
data1 <- na.omit(data)

#Change colnames for ease of use: per capita plastic waste, kg per person per day | gpd in $
colnames(data1)[4] <- c("capita_mis")
colnames(data1)[5] <- c("gdp_capita")

#Order the data
data2 <- data1[order(-data1$capita_mis),] 

#Make a variabele regarding percentage of Sri Lanka
data2$perc <- NA
data2$perc <- (data2$capita_mis/0.299)*100

#Select countries with highest waste < selecting by percentage of Sri Lanka
data3 <- data2 %>%
  top_n(36)

#Change some country names for ease of use
data3$Entity[5] <- "Trinidad & Tobago"
data3$Entity[33] <- "Sao Tome"

#Order countries based on percentage
data3$Entity <- factor(data3$Entity, levels=data3$Entity[order(-data3$perc)])

#GGplot: multiple piecharts
p <- data3 %>%
  ggplot(aes(x="", y=perc, fill=perc)) +
  geom_bar(width = 1, stat = "identity", fill = "darkgoldenrod3", colour = "black") +
  coord_polar("y", start=0) + facet_wrap(~ Entity) +
  labs(title = "TidyTuesday: Mismanaged plastic waste",
       subtitle="Countries based on kg per person per day as % of Sri Lanka.\nSri Lanka is, with 0.299kg per person per day, the country on 'top'.",
       caption = "Source: Our World in Data | Plot by @sil_aarts")+
  theme_minimal()+
  theme(
    plot.background = element_rect(fill = "grey54"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid=element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    plot.title=element_text(size=18, face="bold", family="sans"),
    plot.subtitle=element_text(size=15, family="sans", face="bold", colour="white"),
    plot.caption=element_text(size=10, face="bold", family="sans"))


#Run it! 'See those pacmans!'.
p
