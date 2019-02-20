#TidyTuesday 
#Week 7: FRD Spendings on NIH

#Load libraries
library(ggplot2)
library(devtools)
library(gganimate)

#Read data from github
Spendings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")

#Let's check National Institutes of Health where I worked for 4 months (NIA, Bethesda)
Spendings <- subset(Spendings, department=="NIH")

#Divide all by 1 miljoen
Spendings$FRD <- Spendings$total_outlays/100000000
Spendings$GDP <- Spendings$gdp/100000000

#GGplot - geom_line 
p <- ggplot(Spendings, aes(x = year, y = GDP))+
  geom_line(aes(x=year, y=FRD), size=1.2, alpha = 0.6, col="darkslateblue")+
  geom_label(data=Spendings[23,], aes(x=year, y=FRD, label="Total FRD Budget"), size =3, alpha=0, vjust=0.03, hjust= 1, col="darkblue")+
  geom_line(aes(x=year, y=GDP), size=1.2, alpha = 0.6, col="darkcyan")+
  geom_label(data=Spendings[23,], aes(x=year, y=GDP, label="Total US Gross Domestic Product"), size =3, alpha=0, vjust=1, hjust= 1.2, col="darkcyan")+
  ggtitle(label = "Federal R&D Budget and Total Gross Domestic Product", subtitle="Fiscal Years: from 1976 until 2017 (in millions of dollars)")+
  xlab("Year")+ 
  ylab("Spendings")+
  labs(caption="Source: Federal Research and Development Spending. Plot by sil_aarts")+
  transition_reveal(year)
    
#Run animation
p

#Save gif, using 80 frame (100=default)
anim_save("Desktop/R/TidyTuesday/TidyTuesday_week7.gif")
