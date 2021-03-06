#TidyTuesday
#===============================================================================
#THE HORROR!
#@sil_aarts
#===========================================================================
#Load libraries
library(tidyverse)
library(showtext)
library(showtextdb)
library(ggmap)
library(ggthemes)
library(grid)
library(gridExtra)
library(magick)

#Load files
data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

#Select only rows with non-missing filming_locations
data1 <- data %>%
  drop_na(filming_locations)

#String split on filming_locations
data2 <- data1 %>%
  separate(filming_locations, c("place", "city", "state"), ",")

#Change value of a column to another column on conditon
data3 <- mutate(data2, city = ifelse(city== "USA", city, place))
data3$city[data3$city == "Wellington"] <- "Florida"

#Select 20 most horrible movies!
data4 <- data3 %>%
  top_n(-20, review_rating)

#Merge long & lat
data5 <- data4 %>%
  drop_na(city)
register_google(key = "Your key here") 
geo <- geocode(data5$city) 
data6 <- merge(data5, geo, by.x = 0, by.y = 0)

#Add an image
image <- image_read("https://cdn.pixabay.com/photo/2015/06/22/23/21/filmklappe-818198_960_720.jpg")
image2 <- image_read("https://www.nicepng.com/png/full/439-4394330_free-blood-drip-png-real-blood-effect-png.png")
movie <- grid::rasterGrob(image, interpolate = T) 
blood <- grid::rasterGrob(image2, interpolate = T)

#Pixels!
#Low resolution is lot of dots. High is 'dotless'.
resolution <- 1.5
lat <- tibble(lat = seq(-90, 90, by = resolution))
long <- tibble(long = seq(-180, 180, by = resolution))
#Lakes are optional
pixels <- merge(lat, long, all = TRUE) %>%
  mutate(country = maps::map.where("world", long, lat),
         lakes = maps::map.where("lakes", long, lat)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>%
  select(-lakes)

#Choose font
font_add_google("Barrio", "C")
showtext_auto()

#Theme
theme <-  theme_map() +
  theme(
    text = element_text(family="C"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA), 
    plot.title= element_text(size = 50, color="red", hjust=0.5),
    plot.subtitle= element_text(size = 14, color="black", hjust=0.5),
    plot.caption = element_text(size = 10, color="red"),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())
    

#GGplot: ggmap
pixelsmap <- ggplot() + 
    geom_point(data = pixels, aes(x = long, y = lat), color = "grey30", size = 1)

#Run quartz for showtext
quartz()

#GGplot: ggmap
p <- pixelsmap + 
  geom_point(data =data6, aes(x = lon, y = lat), color = "red", size = 2) +
  geom_point(data =data6, aes(x = lon, y = lat), color = "red", size = 4, alpha=0.4)+
  #Peru
  annotation_custom(movie, xmin= -110, xmax= -150, ymin= -40, ymax= -10)+
  geom_text(aes(x = -130, y = -30), label="Lima, Peru (2017)\nUna Comedia\nMarcabia\nRating 1.0", color="white", size=2.5, family="C")+
  annotation_custom(blood, xmin= -110, xmax= -150, ymin= -50, ymax= -40)+
  #Russia
  annotation_custom(movie, xmin= 130, xmax=170, ymin=10, ymax=40)+
  geom_text(aes(x = 150, y = 20), label="Pereslavl-Zalessky\nRussia (2017)\nInterstelar 2:\nOperation Terra 2040\nRating 1.6", color="white", size=2.1, family="C")+
  annotation_custom(blood, xmin= 130, xmax= 170, ymin= 0, ymax= 10)+
  labs(title= "THE HORROR", 
       subtitle="Where were the most 'horrible' movies shot? Locations where movies with ratings <2.0 were filmed*.", 
       caption="*Scale 1 (low) - 10 (high) rating | Source: IMDB | Plot by: @sil_aarts")+
  coord_sf(clip = "on",
           ylim = c(-75, 85),
           xlim = c(-160, 170))+
  theme

#Run it!
p
