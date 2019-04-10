#TidyTuesday
#Tennis
#Grand Slams!

#Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(extrafont)
library(emoGG)

#Load File
tennis <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")

#Delete NA
tennis2 <- tennis[!is.na(tennis$grand_slam), ]

#Add ID, check data and then >48 is male
tennis2$ID <- seq.int(nrow(tennis2))

#Create new column
tennis2$sex <- NA
#Loop for male and female
for (i in 1:nrow(tennis2))
{
  if (tennis2$ID[i] > 48) { tennis2$sex[i] <- "Male"} else {tennis2$sex="Female"}
}

#Age in years
tennis2$age_years <- (tennis2$age/365)

#Make z-scores
tennis2$age_zscore <- ave(tennis2$age_years, tennis2$sex, FUN=scale)
#Make a 0/1 variabele
tennis2$age_groups <- ifelse(tennis2$age_zscore < 0, "below", "above")

#Check mean age for Females and Male
tennis2 %>%
  group_by(sex) %>%
  summarise(mean_age= mean(age_years))

#Dicimals check 
tennis3 <- tennis2 %>% 
  mutate_if(is.numeric, round, digits = 1)

#Oeps, we see that some 0.0 are below and some above. 
#If mean age_zscore is around 0.0 (0.05 more or less) > age_groups=mean=22.8
for (i in 1:nrow(tennis3))
{
  if (tennis3$age_zscore[i] > -0.05 && tennis3$age_zscore[i] < 0.05) { tennis3$age_groups[i] <- "mean"} 
}

#GGplot: divergrent lollipop plot
p <- tennis3 %>%
  filter(sex=="Female") %>%
  ggplot(aes(x=name, y=age_zscore, label=age_zscore)) + 
  geom_segment(aes(y = 0, 
                   x =name, 
                   yend =age_zscore, 
                   xend = name)) +
  geom_point(stat="identity", aes(colour=age_groups), size=6)+ 
  scale_colour_manual(name="Average age: 22.8", 
                    labels = c("Above Average","Below Average", "Mean"), 
                    values = c("darkgreen","limegreen", "springgreen"))+
  add_emoji(emoji="1f3be")+
  geom_text(color="black", size=3, family="Helvetica") +
  labs(title="TidyTuesday: Female Tennis Grand Slams", 
       subtitle="Age at first Grand Slam title (z-scores)",
       caption="Source: Wikipedia | Plot by: @sil_aarts",
        y="age in years (z-scores)")+
  theme_light()+
  theme(
    axis.title.x = element_text(size=12, face="bold", family="Times"),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size=10, face="bold", family="Times"),
    plot.title=element_text(size=20, face="bold", family="Times"),
    plot.subtitle=element_text(size=18, family="Times"),
    plot.caption=element_text(size=10, family="Times"))+
  ylim(-3,3)+
  coord_flip()

#Run it
p
