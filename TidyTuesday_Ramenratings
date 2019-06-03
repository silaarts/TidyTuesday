#TidyTuesday
#===============================================================================
#Food!!! Ramen ratings.
#@sil_aarts
#===============================================================================

#Install packages
install.packages("packcircles")
install.packages("ggiraph")
install.packages("packcircles")
install.packages("htmlwidgets")

#load libraries
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)
library(dplyr)
library(RColorBrewer)
library(htmlwidgets)

#Load data
data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

#Check unique rows
data1 <- data %>%
  select(2:6)

#Only unique rows
data2 <- unique(data1)

#Only non-missing data
data3 <- na.omit(data2)

#Order by ratings (=stars)
data4 <- data3[order(-data3$stars),]

#See what kind of countries there are: we are there too! 
unique(data4$country)

#Select only European countries: Let's check European Ramen!
data5 <- data4 %>%
 filter(country=="Holland" | country=="Netherlands" | country=="Italy" | country=="Sweden" | country=="Finland" | country=="Poland" | country=="France" | country=="Estonia")

#Change some names for ease of use/plot
data5$brand[c(29,37,38,41)] <- "Koh \n Thai"

#Add a new column with the text you want to display when you press a specific bubble/circle
data5$text <- paste("Brand: ",data5$brand, "\n", "Variety:", data5$variety, "\n", "Origin country:", data5$country, "\n", "Style of container:", data5$style,"\n","Rating:", data5$stars)

#Generate the lay-out (thanks to: https://www.r-graph-gallery.com/308-interactive-circle-packing/)
circles <- circleProgressiveLayout(data5$stars, sizetype='area')
data6 <- cbind(data5, circles)
data_gg <- circleLayoutVertices(circles, npoints=25)

#Make interval to factor for use of colours!
data_gg$breaks <- as.factor(data_gg$id)

#Make some colours using a self-made colour_palette (thanks to my son): we need 41
col_numb <- 41
mycols <- c("#04BEBF","#006169","#FFA28A","#9B0A12")
mycolors <- colorRampPalette(mycols)(col_numb)

#GGplot: bubbles!
p <- ggplot() + 
  geom_polygon_interactive(data = data_gg, aes(x, y, group = id, fill=breaks, tooltip = data6$text[id], data_id = id), colour = "white", alpha = 0.7, size=2) +
  scale_fill_manual(values=mycolors)+
  geom_text(data = data6, aes(x, y, label = data6$brand, size=3)) +
  labs(title = "TidyTuesday: Ramen ratings " ,
       subtitle = "Can we serve nice ramen in the EU? Please press a brand for the info you need!\n* 0: Not so yummy | 5: Give me more!" ,
       caption="Source: TheRamenRater.com | Plot by @sil_aarts")+
  theme_void() + 
  theme(
    legend.position="none", plot.margin=unit(c(0,0,0,0),"cm"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "black"),
    plot.title=element_text(size=30, hjust=0, family="Georgia",face='bold', colour="white", lineheight = 1),
    plot.subtitle=element_text(size=22, hjust=0, family="Georgia", colour="white"),
    plot.caption=element_text(size=18, hjust=0.9, family="Georgia",face='bold', colour="white"))+
  coord_equal()

#Run it
p

#With interaction
p_int <- ggiraph(ggobj = p, width_svg = 15, height_svg = 15)

#Run it!
p_int
