#TidyTuesday
#Stanford Open Policing project

#Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gganimate)
library(extrafont)
library(RColorBrewer)
library(readr)

#Read file
combined_data <- readr::read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv")
sort(combined_data$stops_per_year, decreasing=T)

#Alter stops per year for esthetics plot
combined_data$stops_per_year <- combined_data$stops_per_year/100

#GGplot: scatterplot: search rate by arrest rate
#Make colors for the states
nb.cols <- 17
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)

#Make the plot (in my tweet about this plot I used geom_count, however, geom_point gives you all the states)
p4 <- ggplot(combined_data, aes(x=search_rate, y=arrest_rate)) + 
  geom_point(aes(col=state, size=stops_per_year)) + 
  xlim(c(0, 0.18)) + 
  labs(subtitle="Stanford Open Policing Project: relating search rate(%) to arrest rate(%)", 
       y="Arrest rate %", 
       x="Search rate %", 
       title="TidyTuesday", 
       size="Stops per year*100", 
       col="State",
       caption = "Source: Stanford Open Policing Project | Plot by @sil_aarts")+
  scale_color_manual(values = mycolors)+
  scale_size_continuous(breaks = 1:4, labels = c("<1","1000","2000","3000"), limits = NULL, trans = "identity")+
  guides(size = guide_legend(override.aes = list(size = c(2:5))))+  
  theme(legend.background = element_rect(fill="lightgrey", size=0.5, linetype="solid", colour="black"),
        panel.background = element_rect(fill = "lightgrey"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.x =element_blank(),
        panel.grid.major.y =element_blank(),
        panel.grid.minor.x =element_blank(),
        panel.grid.minor.y =element_blank(),
        plot.title = element_text(color="black", face="bold", size=14, hjust=0),
        plot.subtitle=element_text(size=13, hjust=0, face="italic", color="black"),
        plot.caption= element_text(size=10, hjust=1, color="azure4"),
        legend.title = element_text(size=10, hjust=1, color="black", face="bold"),
        axis.text.x= element_text(size=10, hjust=1, face='bold', colour='black'),
        axis.text.y= element_text(size=10,face='bold', colour='black'),
        axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = 0.6, vjust = 1, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = 0.5, vjust = 1, face = "bold"))

#Run it
p4

#Make the plot: ZOOM IN!
p5 <- ggplot(combined_data, aes(x=search_rate, y=arrest_rate)) + 
  geom_point(aes(col=state, size=stops_per_year)) + 
  xlim(c(0, 0.05)) + 
  labs(subtitle="Stanford Open Policing Project: relating search rate(%) to arrest rate(%): ZOOM IN", 
       y="Arrest rate %", 
       x="Search rate %", 
       title="TidyTuesday", 
       size="Stops per year*100", 
       col="State",
       caption = "Source: Stanford Open Policing Project | Plot by @sil_aarts")+
  scale_color_manual(values = mycolors)+
  scale_size_continuous(breaks = 1:4, labels = c("<1","1000","2000","3000"), limits = NULL, trans = "identity")+
  guides(size = guide_legend(override.aes = list(size = c(2:5))))+  
  theme(legend.background = element_rect(fill="lightgrey", size=0.5, linetype="solid", colour="black"),
        panel.background = element_rect(fill = "lightgrey"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major.x =element_blank(),
        panel.grid.major.y =element_blank(),
        panel.grid.minor.x =element_blank(),
        panel.grid.minor.y =element_blank(),
        plot.title = element_text(color="black", face="bold", size=14, hjust=0),
        plot.subtitle=element_text(size=13, hjust=0, face="italic", color="black"),
        plot.caption= element_text(size=10, hjust=1, color="azure4"),
        legend.title = element_text(size=10, hjust=1, color="black", face="bold"),
        axis.text.x= element_text(size=10, hjust=1, face='bold', colour='black'),
        axis.text.y= element_text(size=10,face='bold', colour='black'),
        axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = 0.6, vjust = 1, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = 0.5, vjust = 1, face = "bold"))

#Run it
p5
