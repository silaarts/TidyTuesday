#TidyTuesday
#===============================================================================
#TidyTuesday
# Birdsspotting @Christmas.
#ChordDiagram
#@sil_aarts
#===============================================================================

#Load libraries
library(tidyverse)
library(circlize)
library(dplyr)
library(chorddiag)  
library(LaCroixColoR)
library(extrafont)

#Read file
birds <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

#Check what is in the data! Yep normal names and Latin have the same n.
unique(birds$species)
unique(birds$species_latin)

#Check unique rows
birds2 <- birds %>%
  select(2:6)

#Order count
birds2 <- birds2[order(-birds2$how_many_counted),]

#Sum count
birds3 <- birds2 %>% 
  group_by(species, species_latin) %>% 
  summarise(total = sum(how_many_counted, na.rm=T))

#Order count
birds4 <- birds3[order(-birds3$total),]

#Select top 10
birds5 <- birds4 %>%
  subset(total > 155000)

#Divide by 1000 for ease of use
birds5$total <- birds5$total/1000

#Change colnames
colnames(birds5) <- c("names", "key", "value")

#Make some colours
col <- lacroix_palette("Pamplemousse", n = 10, type = "continuous")

#Set some parameters: insert gap to see difference between col and rows
circos.par(start.degree = 0, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4), par(bg = "grey30"), par(family = "serif"))

#ChordDiagram plot
chordDiagram(
  x = birds5, 
  grid.col = col,
  transparency = 0.2,
  directional = 1,
  direction.type = c("arrows"), 
  diffHeight  = -0.05,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.3),
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE,
  link.border="white",
  link.lwd=2,
  link.arr.length = 0.1)

#Add axis all around and text using the labels
circos.trackPlotRegion(track.index = 1, 
                       bg.border = NA, 
                       panel.fun = function(x, y) {
                        xlim = get.cell.meta.data("xlim")
                        ylim = get.cell.meta.data("ylim")
                        sector.index = get.cell.meta.data("sector.index")
                        
#Tick white line all the way around to get the axis.ticks in the white 
circos.rect(xleft=xlim[1], ybottom=3, xright=xlim[2], ytop=1, col = "white")

#Add names to the sector, make it facing inside, size letters=cex
circos.text(x = mean(xlim), y = 4.5, labels = sector.index, facing = "inside", cex = 0.9, col="white")

#Add ticks on axis
circos.axis(major.at = c(0, 200, 400, 600, 800, 1000, 1200, 1400, 1600), direction = "outside",  col="black") }
)

circos.clear()

#Add title
text(0, 0.1,"TidyTuesday", cex=3, col="black")
text(0, 0, "Birdspotting since 1921", cex = 2, col="black")

#Add read it info
text(-1.1,-0.92,c("How to read this plot?"), pos=4, cex=1.2, col="black")
text(-1.1,-0.96,c("Arrows represent names of bird species (bottom) "), pos=4, cex=1, col="black")
text(-1.1,-1.00,c("and their Latin names (top). Arrows are indicative"), pos=4, cex=1, col="black")
text(-1.1,-1.04,c("for the top 5 most spotted birds (x1000) around Christmas"), pos=4, cex=1, col="black")

#Add caption
text(1.1,-1.00,c("Plot by @sil_aarts"), pos=2, cex=1, col="black")
text(1.1,-1.04,c("Source: Bird Studies Canada | Hamilton area of Ontario"), pos=2, cex=1, col="black")
