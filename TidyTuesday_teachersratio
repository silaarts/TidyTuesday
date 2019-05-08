#TidyTuesday
#Student-teacher ratio teaching

#Load libraries
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggplot2)  
library(readr)
library(ggmap)
library(ggplot2)
library(tidytext)
library(stringr)
library(gganimate)
library(extrafont)

#Read file
data <- read.csv2("Desktop/Data1.csv", stringsAsFactors = F)
colnames(data) [1] <- c("country")
 
#Tranpose data: make countries colnames
data1 <- add_rownames(data) %>% 
  gather(year, grade, X2012:X2017, na.rm=TRUE) %>% 
  arrange(rowname, country) %>% 
  select(-rowname)
data1$year <- gsub("X", "", data1$year)

#Make the countries data.frame
countries <- as.data.frame(data$country)
colnames(countries) [1] <- c("country")
countries$ID <- seq.int(nrow(countries))

#API key google
register_google(key = "Enter your key here")

#Make the geocode data.frame
europe <- geocode(c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Denmark", "Estonia","Finland","France", "Germany", "Greece", "Hungary", "Ireland",
                    "Italy", "Latvia", "Lithunia", "Luxembourg", "Malta", "Netherlands",
                    "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain","Sweden"))
europe$ID <- seq.int(nrow(europe))

#Combine the two datafiles
data3 <- merge(x = europe, y = countries, by = "ID", all = TRUE)
#Combine with original file
data4 <- merge(x = data3, y = data1, by = "country", all = TRUE)

#Delete last decimals for ease of use
data4$grade <- substr(data4$grade, 1, nchar(data4$grade)-4)

#Make year = date
data4$year <- as.Date(data4$year, "%Y")
#Make grading numeric
data4$grade  <- as.numeric(gsub('![[:alnum:]]*[[:space:]]|[[:punct:]]', '', data4$grade))
data4$grade <- data4$grade/10.

#Select 2016
data6 <- data5 %>%
  filter(year=="2016-05-07")

#Get ggmap Europe
europe_map<- get_map(location='Europe', zoom=4)

#Ready to make the ggmap
p <- ggmap(europe_map)+
  geom_point(data = data6, aes(x = lon, y = lat, color = grade), size=5) + 
  scale_colour_gradient(name = 'Ratio', low="turquoise1", high="slateblue")+
  labs(title ="TidyTuesday: Tertiary education 2016", 
       subtitle="Pupil-teacher ratio in tertiary education", 
       caption="Source: UNESCO | Plot by: @sil_aarts")+
  theme(
        plot.title= element_text(family="Times New Roman", face="bold", size=18),
        plot.subtitle= element_text(family="Times New Roman", size=16),
        plot.caption= element_text(family="Times New Roman", size=12),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

#Run it
p
