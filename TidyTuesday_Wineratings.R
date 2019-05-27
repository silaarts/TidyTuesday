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

#Keep 3 columns only
data1 <- data %>%
  select(2,5,6)

#Only select wines with not an exorbitant price: so top is 30e
data2 <- data1 %>%
  filter(price <30)

#Check levels of country
data1$country <- as.factor(data1$country)
levels(data1$country)

#Select only the top 3 countries
data3 <- data2 %>%
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
  xlim(0, 35)+
  ylim(75, 100)+
  ylab("Rating")+
  xlab("Price")+
  annotate(geom="label", x= 25, y = 75, label = "Total N= 24.262\nFrance= 10.636\nItaly= 8754\nSpain= 4872", family= "Courier", vjust=0.1, size=4, fontface="bold", colour="black", fill="transparent")+
  theme_classic()+
  theme(
    plot.caption=element_text(size=10, hjust=0.8,family="Courier New", colour="white"),
    axis.title.x = element_text(size=10, family="Courier", face="bold", colour="white"),
    axis.title.y = element_text(size=10, family="Courier", face="bold", colour="white"),
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
  ylab("Wine price")+
  xlim(0, 35)+
  scale_fill_manual(values=c("violetred", "darkseagreen4","darkgoldenrod"))+
  theme_classic()+
  theme(
  legend.position = "none",
  axis.text=element_blank(),
  axis.line=element_blank(),
  axis.ticks=element_blank(),
  axis.title.x=element_blank(),
  axis.title.y = element_text(size=10, family="Courier", face="bold", colour="white"),
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "grey30"))

#GGplot: Density Points
p2_dist <- ggplot(data4, aes(points, fill=country)) + 
  geom_density(alpha=.5)+
  ylab("Wine rating")+
  scale_fill_manual(values=c("violetred", "darkseagreen4","darkgoldenrod"))+
  theme_classic()+
  theme(
    legend.position = "none",
    axis.title.y=element_blank(),
    axis.line=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x= element_text(size=10, family="Courier", face="bold", colour="white"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "grey30"))

#Combine the 3 plots
plot_grid(pscatter, p1_dist, p2_dist, nrow=1)

#Avoid displaying duplicated legend #Flip axis of P2_dist
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
        plot.title=element_text(size=15, hjust=0, family="Courier New",face='bold', colour="white"),
        plot.subtitle=element_text(size=13, hjust=0, family="Courier New", colour="white"),
        plot.caption=element_text(size=12, hjust=0, family="Courier New",face='bold', colour="white"),
        axis.text = element_blank())

#Change margin to reduce the distance between plots: c(top, right, bottom, left)
#Align G1 density with the scatterplot
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
