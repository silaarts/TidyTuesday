#TidyTuesday
#Women in research
#SankeyNetwork
#@sil_aarts

#Load libraries
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(gganimate)
library(dplyr)
library(networkD3)

#Load data
women <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")

#Make nodes
nodes <- data.frame(name=c(as.character(women$country), as.character(women$field)) %>% unique())

#Make Id needed nfor NetworkD3
women$IDcountry=match(women$country, nodes$name)-1 
women$IDfield=match(women$field, nodes$name)-1

#Provide group variable
nodes$ID <- seq.int(nrow(nodes))
nodes$group <- NA
for (i in 1:nrow(nodes))
{
  if (nodes$ID[i] > 12) { nodes$group[i] <- "b"} else {nodes$group="a"}
}
nodes$ID <- NULL

#Provide colours to the nodes
my_color <- 'd3.scaleOrdinal() .domain(["a","b"]).range(["violet","black"])'

#Make the Network
p <- sankeyNetwork(Links = as.data.frame(women), Nodes = nodes,
                   Source = "IDcountry", Target = "IDfield",
                   Value = "percent_women", NodeID = "name", NodeGroup="group", 
                   colourScale =my_color, fontSize = 14, sinksRight=F, fontFamily = "sans-serif", 
                   nodeWidth = 20, nodePadding=20,
                   width= 800, height=600)

p1 <- htmlwidgets::prependContent(p, htmltools::tags$h1("TidyTuesday: women researchers throughout the world")) 
p2 <- htmlwidgets::appendContent(p1, htmltools::tags$p("Source: The Economist | Plot by @sil_aarts"))

#Run it!
p2


