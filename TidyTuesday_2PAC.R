#TidyTuesday
#===============================================================================
#TidyTuesday
#2 Pac
#ChordDiagram
#@sil_aarts
#===============================================================================

#Load libraries
library(tidyverse)
library(circlize)
library(dplyr)
library(LaCroixColoR)
library(extrafont)

#Read file
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

#Check unique rows
data1 <- data %>%
  filter(artist=="2Pac" | artist=="2Pac ft. Dr Dre" | artist=="2Pac ft. Talent")

#Order count (already done)
data2 <- data1[order(-data1$points),]

#Select columns
data3 <- data2 %>%
  select(2,3,6)
data4 <- data3 %>%
  select("artist", everything())

#Change colnames
colnames(data4) <- c("names", "key", "value")



#Make some colours
col <- lacroix_palette("PassionFruit", n = 11, type = "continuous")

#Set some parameters: insert gap to see difference between col and rows
circos.par(start.degree = 0, track.margin = c(-0.1, 0.15), points.overflow.warning = FALSE)
par(mar = rep(0, 3), par(bg = "black"), par(family = "mono"))

#ChordDiagram plot
chordDiagram(
  x = data4, 
  grid.col = col,
  transparency = 0.1,
  big.gap = 10, 
  small.gap = 1.5,
  directional = 1,
  direction.type = c("arrows"), 
  diffHeight  = -0.05,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.05),
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE,
  link.border="grey",
  link.lwd= 0.5,
  link.arr.length = 0.05)


#Add axis all around and text using the labels
circos.trackPlotRegion(track.index = 1, 
                       bg.border = NA, 
                       panel.fun = function(x, y) 
                         {
                         xlim = get.cell.meta.data("xlim")
                         ylim = get.cell.meta.data("ylim")
                         sector.index = get.cell.meta.data("sector.index")
                        
                         #Add names to the sector, size letters=cex
                         circos.text(x = mean(xlim), y = 1.3, labels = sector.index, facing = "clockwise", adj=c(0,0.2), cex = 0.6, col="white", niceFacing = TRUE)
                         
                         #Add ticks on axis
                         circos.axis(h = "bottom", labels.niceFacing = T, labels.col="white", labels.cex=0.4, labels.facing = "clockwise", direction="inside", col="white") 
                         }
)

circos.clear()

#Add title
#text(0, 0.1,"TidyTuesday", cex=3, col="black")
text(-1.7, 0.8, "2Pac", pos=4, cex = 6, col="white", family="mono")

#Add read it info
text(1.8,-0.55,c("Tupac Shakur"), pos=2, cex=2, col="white")

text(1.8,-0.70,c("Arrows represent the # of points for the\nfavourite hip-hop tracks list.\nA ticker arrow is indicative for more points."), pos=2, cex=0.9, col="white", family="mono")
text(1.8,-1,c("With 42 points, 'Dear Mama' holds\nthe 10th position of the favourite hip-hop tracks list.\n(#1 has 140 points)"), pos=2, cex=0.9, col="white", family="mono")

#Add caption
text(-1.7,-1.00,c("Plot by @sil_aarts"), pos=4, cex=0.6, col="white",family="mono")
text(-1.7,-1.05,c("Source: BBC Music | Simon Jockers at Datawrapper"), pos=4, cex=0.6, col="white",family="mono")

