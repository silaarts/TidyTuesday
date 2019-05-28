#TidyTuesday
#===============================================================================
#Have a drink! Wine!
#@sil_aarts
#===============================================================================

#Load libraries
library("ggplot2")
library("cowplot")
library("dplyr")
library("extrafont")

#Load data
data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

#Keep 3 columns only (x1 is useless and contains duplicate rows)
data1 <- data %>%
  select(2:14)

#Only unique rows
data2 <- unique(data1)

#Select only 3 columns we need
data2 <- data2 %>%
  select(1,4,5)

#Only select wines with not an exorbitant price: so top is 30euro!
data3 <- data2 %>%
  filter(price <30)

#Check levels of country
data3$country <- as.factor(data3$country)
levels(data3$country)

#Select only the top 3 countries
data3 <- data3 %>%
  filter(country=="Spain" | country=="France" | country=="Italy")

#Only non-missing data
data4 <- na.omit(data3)

#Count n per country for annotation
data5 <- data4 %>%
  count(country)

#GGPlot: Scatter plot
pscatter <- ggplot(data4, aes(price, points, color=country, shape=country)) + 
  geom_point(alpha=.8)+
  scale_color_manual(values=c("violetred", "darkseagreen4","darkgoldenrod"))+
  xlim(0, 31)+
  ylim(75, 100)+
  ylab("Wine rating")+
  xlab("Wine price (€)")+
  annotate(geom="label", x= 25, y = 75, label = "Total N= 22.193\nFrance= 9.696\nItaly= 8.001\nSpain= 4.496", family= "Courier", vjust=0.1, size=4, fontface="bold", colour="black", fill="transparent")+
  theme_classic()+
  theme(
    axis.title.x = element_text(size=13, family="Courier", face="bold", colour="white"),
    axis.title.y = element_text(size=13, family="Courier", face="bold", colour="white"),
    axis.text= element_text(size=8, family="Courier", face="bold", colour="white"),
    legend.position="bottom", legend.box = "horizontal",
    legend.background = element_rect(fill="grey30", size=0.5, linetype="solid", colour ="black"),
    legend.text = element_text(family="Courier", colour="white", size=12),
    legend.title = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "grey30"))
  
#GGplot: Density Price
p1_dist <- ggplot(data4, aes(price, fill=country)) + 
  geom_density(alpha=.5)+
  ylab("Wine price (€)")+
  xlim(0, 31)+
  scale_fill_manual(values=c("violetred", "darkseagreen4","darkgoldenrod"))+
  theme_classic()+
  theme(
  legend.position = "none",
  axis.text=element_blank(),
  axis.line=element_blank(),
  axis.ticks=element_blank(),
  axis.title.x=element_blank(),
  axis.title.y = element_text(size=13, family="Courier", face="bold", colour="white"),
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "grey30"))

#GGplot: Density Rating
p2_dist <- ggplot(data4, aes(points, fill=country)) + 
  geom_density(alpha=.5)+
  ylab("Wine rating")+
  xlim(75,100)+
  scale_fill_manual(values=c("violetred", "darkseagreen4","darkgoldenrod"))+
  theme_classic()+
  theme(
    legend.position = "none",
    axis.title.y=element_blank(),
    axis.line=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x= element_text(size=13, family="Courier", face="bold", colour="white"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "grey30"))

#Combine the 3 plots
plot_grid(pscatter, p1_dist, p2_dist, nrow=1)

#Flip P2_dist
p2_dist <- p2_dist + coord_flip()

#Add title
title <- ggplot(data.frame(x = 1:2, y = 1:10))+
  labs(x = NULL, y = NULL,
       title = "TidyTuesday: Wine" ,
       subtitle = "Relation between \nprice & ratings for\ntop 3 wine countries:\nFrance, Italy & Spain.\nSelection of wines \nbelow 30 euro.\nRatings:\nmin=80; max=100" ,
       caption="Source: Kaggle \nPlot by @Sil_aarts")+
  theme(line = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "grey30"),
        panel.border = element_rect(color = NA),
        plot.title=element_text(size=18, hjust=0, family="Courier New",face='bold', colour="white"),
        plot.subtitle=element_text(size=15, hjust=0, family="Courier New", colour="white"),
        plot.caption=element_text(size=12, hjust=0, family="Courier New",face='bold', colour="white"),
        axis.text = element_blank())

#Change margin to reduce the distance between plots: c(top, right, bottom, left)
pscatter <- pscatter + theme(plot.margin = unit(c(0, 0, 0.5, 0.5), "cm"))
p1_dist <- p1_dist + theme(plot.margin = unit(c(0.5, 0, 0, 0.7), "cm"))
p2_dist <- p2_dist + theme(plot.margin = unit(c(0, 0.5, 0.5, 0), "cm"))
#Make sure the title is added to in the right top corner
p_title <- title + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

#Combine all plots together and crush graph density with rel_heights
p1 <-  plot_grid(p1_dist, pscatter, ncol = 1, rel_heights = c(1, 3))
p2 <-  plot_grid(p_title, p2_dist, ncol = 1, rel_heights = c(1, 3))
p <-  plot_grid(p1, p2, ncol = 2, rel_widths = c(3, 1))

#Run it!
p
