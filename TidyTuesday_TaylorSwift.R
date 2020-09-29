#TidyTuesday
#===============================================================================
#Taylor Swift
#@sil_aarts
#===============================================================================
library(tidyverse)
library(cowplot)
library(showtext)
library(showtextdb)
library(stringr)
library(reshape2)
library(gghighlight)

#Read file
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv')

#Get date
data1 <- data %>%
  separate(released, ",", into = c("month", "year"), remove = FALSE)%>%
  separate(year, " ", into = c("extra", "year2"), remove = FALSE)

#Choose font
font_add_google("Faster One", "D")
font_add_google("Just Another Hand", "K")
font_add_google("Cabin Sketch", "L")
showtext_auto()

#Theme
theme_sil <- theme_classic()+
  theme(
    legend.background = element_rect(fill="#AE7EA8", linetype="dashed", colour ="#AE7EA8"),
    legend.title= element_blank(),
    plot.background = element_rect(fill = "#AE7EA8", colour = "#AE7EA8"),
    panel.background = element_rect(fill = "#AE7EA8", colour = "#AE7EA8"),
    plot.caption = element_text(colour = "black", size = 7, hjust= 0.7, margin = margin(4, 0, 0, 0), family="L"),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(colour="black", size=17, family="L"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.y = element_line(colour="#FFE6FF"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(0.5,1,1,1), "cm")) #trbl

#Title
title <- ggplot() +
  labs(title = "Taylor Swift",
       subtitle = "Record sales in $ per year.        Bigger 'records' are indicative for more $.\nHighlighted are the records with more than 1 million $ in sales.")+
  theme_sil+
  theme(
    plot.background = element_rect(fill = "#AE7EA8", colour = "#AE7EA8"),
    panel.background = element_rect(fill = "#AE7EA8", colour = "#AE7EA8"),
    plot.title = element_text(color="black", size=60, hjust=0.5, margin = margin(2, 2, 3, 0), family="K"),
    plot.subtitle = element_text(color="black", size=20, hjust=0.6, margin = margin(0, 0, 0, 0), family="L"),
    plot.margin = unit(c(1,0,0,0), "cm")) #trbl

#Make some colours
col_numb <- 14
mycols <- c("#B85208","#B93947","#9A3C6E","#3E4C72", "#00937D")
mycolors <- colorRampPalette(mycols)(col_numb)

#GGplot
p <- ggplot(data1, aes(x=year2, y=sales)) +
  geom_point(aes(size = sales, colour=title), alpha=0.9,  stroke=12, colour="black")+
  geom_point(aes(size = sales, colour=title), alpha=0.8, fill="gray70", colour="gray70", size=8)+
  geom_point(aes(size = sales, colour=title), alpha=0.7, fill="black", colour="white", size=0.7)+
  gghighlight(sales > 1000000, unhighlighted_params = list(colour = alpha("gray70", 0.7)))+
  labs(caption = "Source: Rosie Baillie & Dr. Sara Stoudt  | Plot by @sil_aarts")+
  scale_size_continuous(range = c(2,20))+
  guides(size = FALSE)+
  coord_flip(clip = "off")+
  theme_sil

#Quartz
quartz(type="Cocoa")

#Run it
p

#Combine plots
plots <- plot_grid(title, p, nrow=2, ncol=1, rel_widths=c(15,15), rel_heights=c(8,25))
plots
