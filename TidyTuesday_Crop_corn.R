#TidyTuesday
#===============================================================================
#TidyTuesday
#Corn!
#@sil_aarts
#===============================================================================
#Load libraries
library(tidyverse)
library(ggforce)
library(cowplot)
library(showtext)
library(showtextdb)

#Load data
crop <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

#Change colname for ease of use
colnames(crop)[6] <- "Maize"

#Change rownames for ease of use
crop$Entity[crop$Entity == "Bosnia and Herzegovina"] <- "Bosnia &\nHerzegovina"
crop$Entity[crop$Entity == "Saint Vincent and the Grenadines"] <- "Saint Vincent\n&\nGrenadines"
crop$Entity[crop$Entity == "United Arab Emirates"] <- "United Arab\nEmirates"
crop$Entity[crop$Entity == "New Caledonia"] <- "New\nCaledonia"


#Select
data <- crop %>%
  select(1:3,6)%>%
  filter(Year==2018)%>%
  drop_na(Code)%>%
  top_n(50, Maize)

#Choose sme nice fonts
font_add_google("Calligraffitti", "P")
font_add_google("Bad Script", "B")
showtext_auto()

#Make some corn
p2 <- ggplot(data, aes(fill=Entity, alpha = Maize)) +
  #Corn
  geom_ellipse(aes(x0 = 0, y0 = 9, a = 7, b = 3, angle = pi / 2, m1 = 3), fill= "gold3",  color = "coral4") + 
  #Leaves
  geom_ellipse(aes(x0 = 6.1, y0 = 6.1, a = 5, b = 1, angle = pi / 4, m1 = 1), fill="olivedrab4",  color = "transparent", alpha=1) + 
  geom_ellipse(aes(x0 = 6.2, y0 = 9.2, a = 3, b = 1, angle = pi / 3, m1 = 1), fill="olivedrab4",  color = "transparent", alpha=1) + 
  geom_ellipse(aes(x0 = -6.1, y0 = 6.1, a = 5, b = 1, angle = 3*pi / 4, m1 = 1), fill="olivedrab4",  color = "transparent", alpha=1) +
  geom_ellipse(aes(x0 = -6.2, y0 = 9.2, a = 3, b = 1, angle = 2*pi / 3, m1 = 1), fill="olivedrab4",  color = "transparent", alpha=1) + 
  facet_wrap(~ Entity, nrow=5, ncol=10)+
  theme_void()+
  theme(
    legend.position = "none",
    strip.text.x = element_text(family="B", size= 7, colour="white", margin = margin(.1, 0, .1, 0, "cm")),
    strip.background = element_rect(fill="transparent", colour="black", size=1),
    plot.background = element_rect(fill = "gray64", colour = "gray64"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1,1,1,1), "cm"))
#Run it!
p2

#Create a legend
legend <- ggplot(data) +
  #Corn
  geom_ellipse(aes(x0 = 0, y0 = 10, a = 7, b = 3, angle = pi / 2, m1 = 3), fill= "gold3",  color = "coral4") + 
  #Leaves
  geom_ellipse(aes(x0 = 5.7, y0 = 6.2, a = 5, b = 1, angle = pi / 4, m1 = 1), fill="olivedrab4",  color = "transparent") + 
  geom_ellipse(aes(x0 = 6.1, y0 = 10.2, a = 4, b = 1, angle = pi / 3, m1 = 1), fill="olivedrab4",  color = "transparent") + 
  geom_ellipse(aes(x0 = -5.7, y0 = 6.2, a = 5, b = 1, angle = 3*pi / 4, m1 = 1), fill="olivedrab4",  color = "transparent") + 
  geom_ellipse(aes(x0 = -6.1, y0 = 10.2, a = 4, b = 1, angle = 2*pi / 3, m1 = 1), fill="olivedrab4",  color = "transparent") + 
  #Mark
  geom_mark_circle(aes(x=0, y = 15, label = "A lighter color is indicative for\nless tonnes per hectare"), label.fill=  "transparent", label.colour = "white", label.buffer = unit(20, "mm"), label.family = c("C"), expand = unit(1, "mm"), con.type="elbow", con.colour= "black") +
  labs(caption="Source: Global Crop Yields & Our World in Data | Plot by @sil_aarts")+
  #Costum
  theme_void()+
  theme(
    legend.position = "none",
    text = element_text(family="B", colour="white", size=10),
    plot.background = element_rect(fill = "gray64", colour = "gray64"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line.x = element_blank(),
    plot.caption = element_text(colour = "white", size = 6, hjust=0.5, family="B"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1,1,1,1), "cm"))

#Create title
title <- ggplot() +
  #geom_hline(yintercept = 3.5, colour="black", size=2)+
  labs(x = NULL, y = NULL,
       title = "CORN",
       subtitle = "Top 50 countries with the most tonnes of corn (maize) per hectares in 2018")+
  theme_void(10)+
  theme(
    #text = element_text(family="C"),
    plot.title = element_text(color="white", size=40, hjust=0.5, margin = margin(5, 2, 2, 2), famil="P"),
    plot.subtitle = element_text(color="white", size=25, hjust=0.5, margin = margin(2, 2, 2, 2), family="B"),
    axis.text.y = element_blank(),
    axis.ticks.y =  element_blank(),
    plot.background = element_rect(fill = "gray64", color = "gray64"),
    panel.background = element_rect(fill = "gray64", color = "gray64"),
    panel.border = element_rect(fill = NA, color = NA),
    strip.background = element_rect(fill = NA, color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"))

#Run quartz for showtext
quartz()

#Combine plots
pf <- plot_grid(p2, legend, rel_heights = c(15,15), rel_widths = c(15,10))
pf

pf2 <- plot_grid(title, pf, nrow=2, ncol=1, rel_heights = c(5,23), rel_widths = c(18,18))
pf2
                                                        
