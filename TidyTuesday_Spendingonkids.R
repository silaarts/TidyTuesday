#TidyTuesday
#===============================================================================
#Tidykids
#@sil_aarts
#===============================================================================
library(ggstream)
library(tidyverse)
library(grid)
library(gridExtra)
library(ggforce)
library(cowplot)
library(wesanderson)
library(extrafont)

#font_import()
fonts()
#Read file
kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')

#Select
kids2 <- kids %>%
  filter(variable=="other_health") %>%
  filter(state=="Rhode Island" | state== "New York" | state=="New Jersey" | state=="Delaware" |
           state=="Maryland" | state=="District of Columbia"  | state== "Maine" |
state=="New Hampshire" | state== "Massachusetts"  | state== "Connecticut" | state== "Virginia" |  
  state== "North Carolina" | state== "South Carolina" | state=="Georgia"   | state== "Florida")%>%
  select(1,3,6)

#Theme
theme_sil <- theme_classic()+
  theme(
    text=element_text(family="mono"),
    legend.position = "bottom",
    legend.text = element_text(size=8),
    legend.key.size = unit(0.1,"line"),
    legend.key.width = unit(2.8,"line"),
    legend.background = element_rect(fill="gray87", linetype="solid", colour ="gray70"),
    legend.title= element_blank(),
    plot.background = element_rect(fill = "gray87", colour = "gray87"),
    panel.background = element_rect(fill = "gray87", colour = "gray87"),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(colour="gray70", size=10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(colour="gray70"),
    panel.grid.major.x = element_line(colour="gray70"),
    plot.title = element_text(color="black", size=55, hjust=0.5, margin = margin(5, 0, 0, 0)),
    plot.caption = element_text(colour = "black", size = 10, hjust= 0.5, margin = margin(0, 0, 0, 0)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1,1,1,1), "cm"))

#Make some colours
col_numb <- 15
mycols <- c("#FFA28A","#7E61B4","#A75E00","#00887B")
mycolors <- colorRampPalette(mycols)(col_numb)

#Create some info to explain the plot
data = tibble(
  x = c(-1.5, -1, 0, 1, 1.2, 0), 
  y = c(-5, -3, 0, -3, -5, -7)
)
#Info
info <- ggplot()+
  geom_bspline_closed(data=data,aes(x = x, y = y), fill = "#FFA28A", colour="black", size=0.5, alpha=0.5)+
  geom_segment(data=data, aes(x=0, xend = 0, y=-3, yend = -2), size = 0.6, arrow = arrow(length = unit(0.2, "inches")))+
  geom_segment(data=data, aes(x=0, xend = 0, y=-5, yend = -6), size = 0.6, arrow = arrow(length = unit(0.2, "inches")))+
  geom_text(data=data, aes(x=0, y=-4, label="The height\ndisplays\nthe changes\nin data\n(over time)"), size=3, family="mono")+
  scale_y_continuous(limits=c(-8,2))+
  theme_classic()+
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "gray87", colour = "gray87"),
    panel.background = element_rect(fill = "gray87", colour = "gray87"),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1,0.5,1,0), "cm")) #trbl

#GGplot
p <- ggplot(kids2, aes(x=year, y=inf_adj_perchild, fill =state)) +
  geom_stream(method = "density", color = "black", size = 0.5, bw = 1, alpha=0.5)+
  #Title & caption
  geom_text(aes(x=2001, y=71, label="Spending on Kids"), size=13, family="Impact")+
  geom_text(aes(x=2002, y=56, label="US East Coast"), size=8, family="mono")+
  geom_text(aes(x=2004, y=61, label="Source: Census Bureau's annual State and Local Government Finance Survey | Plot by @sil_aarts"), size=1.5, family="mono", colour="black")+
  annotate(geom="label", x=1997.5, y=-62, label="Public spending on\nhealth vendor payments &\npublic hospitals,\nexcluding Medicaid,\nby state and year,\nin $1,000.", size=3, family="mono", fill="gray87")+
  #Explanation plot
  geom_segment(aes(x=2010, xend = 2010, y=20, yend = 30), size = 0.4, arrow = arrow(length = unit(0.1, "inches")))+
  geom_text(aes(x=2012, y=27, label="The height displays\nthe changes\nin data (over time)"), size=2, family="mono")+
  scale_y_continuous(limits=c(-75,75))+
  scale_x_continuous(breaks=seq(from = 1995, to = 2020, by = 5)) +
  scale_fill_manual(values=mycolors)+
  theme_sil

#Run it
p

#Add plots together
#plots <- plot_grid(p, info, nrow=1, ncol=2, rel_widths=c(20, 7), rel_heights=c(15, 15))
#plots
