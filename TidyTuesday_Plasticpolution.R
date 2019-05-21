#TidyTuesday
#===============================================================================
#Plastic waste; waste no more...
#@sil_aarts
#===============================================================================

#Load libraries
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(extrafont)

#Load data
data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

#Take only with per capita plastic waste
data1 <- na.omit(data)

#Change colnames for ease of use: per capita plastic waste, kg per person per day | gpd in $
colnames(data1)[4] <- c("capita_waste")
colnames(data1)[5] <- c("gdp_capita")

#Order the data
data2 <- data1[order(-data1$capita_waste),] 

#Check the mean of all countries=0.198
data2 %>%
  summarise((mean_capita=mean(capita_waste)))

#Select 20 countries with most waste per capita
data3 <- data2 %>%
  filter(capita_waste > 0.296)

#Check the mean of top 20 countries capita_waste=0.605
data3 %>%
  summarise((mean_capita=mean(capita_waste)))

#Check the median of top 20 countries capita_waste=0.411
data3 %>%
  summarise((median_capita=median(capita_waste)))

#GGplot: Barchart using annotations and curves
p <- data3 %>%
  ggplot(aes(x = Entity, y = capita_waste))+
  geom_hline(yintercept = 0.605, colour="red", size=0.8, linetype="dashed")+
  geom_hline(yintercept = 0.411, colour="red", size=0.8)+
  geom_bar(stat = "identity", color="darkslategray3")+
  coord_flip()+
  labs(title="TidyTuesday: Plastic polution in 2010", 
       subtitle="Global plastic waste: 20 countries with the most waste in kg per person per day",
       x="", 
       y="Plastic waste in kg per person per day",
       caption="Source: Our World in Data | Plot by @sil_aarts")+
  annotate(geom="label", x="Saint Lucia", y = 3.3, label = "Tinidad and Tobago:\n 3.6kg per person/day", family= "Courier", vjust=0.1, size=4, fontface="bold")+
  annotate("curve", x="Seychelles", xend = "Trinidad and Tobago", y = 3.6, yend = 3.6 ,size=0.5, arrow=arrow(length=unit(.2, "cm")))+
  annotate(geom="label", x="Netherlands", y = 1.5, label = "Netherlands:\n 0.424kg per person/day", family= "Courier", vjust=0.1, size=4, fontface="bold")+
  annotate("curve", x="Netherlands", xend = "Netherlands", y = 1, yend = 0.424, size=0.5, arrow=arrow(length=unit(.2, "cm")))+
  annotate(geom="label", x="Ireland", y = 1.5, label = "Mean of top 20 countries:\n 0.605kg per person/day", family= "Courier", vjust=0.1, size=4, fontface="bold")+
  annotate("curve", x="Ireland", xend = "Ireland", y = 1, yend = 0.605, size=0.5, arrow=arrow(length=unit(.2, "cm")))+
  annotate(geom="label", x="Grenada", y = 1.55, label = "Median of top 20 countries:\n 0.411kg per person/day", family= "Courier", vjust=0.1, size=4, fontface="bold")+
  annotate("curve", x="Grenada", xend = "Grenada", y = 1, yend = 0.411, size=0.5, arrow=arrow(length=unit(.2, "cm")))+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "grey30"),
        plot.title=element_text(size=18, hjust=0, family="Courier New",face='bold', colour="white"),
        plot.subtitle=element_text(size=14, hjust=0, family="Courier New", colour="white"),
        plot.caption=element_text(size=10, hjust=1,family="Courier New", colour="white"),
        axis.text.x= element_text(size=10, family="Courier", face="bold", colour="white"),
        axis.text.y= element_text(size=10, family="Courier", face="bold", colour="white"),
        axis.title.x = element_text(size=10, family="Courier", face="bold", colour="white"))

#Run it
p
