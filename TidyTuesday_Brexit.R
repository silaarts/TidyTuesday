#TidyTuesday
#Brexit
#The Economist
#@sil_aarts

#Load libraries
library(tidyverse)
library(RColorBrewer)
library(gganimate)
library(dplyr)
library(cowplot)
library(ggplot2)

#Load data
brexit <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/brexit.csv")

#Change colnames for ease of use
colnames(brexit) <- c("date","perc_right","perc_wrong")

#Merge columns
brexit2 <- data.frame(brexit$date, percent = c(brexit$perc_right,brexit$perc_wrong))

#Add ID and then >85 = wrong
brexit2$ID <- seq.int(nrow(brexit2))

#New column right/wrong
brexit2$Choice <- NA
for (i in 1:nrow(brexit2))
{
  if (brexit2$ID[i] > 85) { brexit2$Choice[i] <- "wrong"} else {brexit2$Choice="right"}
}
brexit2$ID <- NULL

#Change date format
brexit2$date <- as.Date(brexit2$brexit.date, format="%d/%m/%y")

#GGplot: right
p <- brexit2 %>%
  filter(Choice=="right") %>%
  ggplot(aes(x = date, y = percent)) + 
  geom_line(aes(color = Choice), size = 1) +
  scale_color_manual(values = c("navy")) +
  scale_x_date(date_labels="%m-%y")+
  theme_cowplot()+
  theme(
  axis.text.x = element_text(angle = 90, hjust = 1),
  axis.title.x=element_blank())+
  labs(caption="Source: The Economist | Plot by: @sil_aarts",
       y="%")
    
#Including line (mean)
p1 <- p + stat_smooth(
  color = "tomato", fill = "red",
  method = "loess")

p1

#GGplot: wrong
p2 <- brexit2 %>%
  filter(Choice=="wrong") %>%
  ggplot(aes(x = date, y = percent)) + 
  geom_line(aes(color = Choice), size = 1) +
  scale_color_manual(values = c("skyblue")) +
  scale_x_date(date_labels="%m-%y")+
  theme_cowplot()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x=element_blank())+
  labs(y="%")

#Including line (mean)
p3 <- p2 + stat_smooth(
  color = "tomato", fill = "red",
  method = "loess")

p1
#Put both plots into one plot
p4 <- ggdraw() +
    draw_plot(p1+theme(legend.position="none"), 0, 0, 1, 1) +
    draw_plot(p3+theme(legend.position="none"), 0.5, 0.6, 0.45, 0.4) +
    draw_plot_label(c("A.Right", "B.Wrong"), c(0.07, 0.56), c(1, 1), size = 10)

#Run finale plot!
p4
