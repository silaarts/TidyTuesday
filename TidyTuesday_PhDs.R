#TidyTuesday 18 februari 2019 - Doctorates U.S.
#Load library
library(ggplot2)
library(tidyr)
library(dplyr)

#Read file (original excel file: saved as csv)
data2 <- read.csv("Desktop/R/Data_doc.csv", header=T, sep = ";")

#Transpose all but the first column and keep them numeric
data3 <- as.data.frame(t(data2[,-1]))
colnames(data3) <- data2[, 1]
data3 <- data.frame(lapply(data3, function(x) as.numeric(as.character(x))))

#Create a new column with years
Year <- c(1997, 2002, 2007, 2012, 2017)
data4 <- cbind(Year, data3)

#Select Psychology and Social Sciences
data5 <- data4 %>% 
  select("Year", "Psychology.and.social.sciences.", "Hispanic.or.Latino.4","American.Indian.or.Alaska.Native.4"
  ,"Asiana.4", "Black.or.African.American.4", "White.4", "More.than.one.race.4","Other.race.or.race.not.reportedb.4")

#Change colnames for ease of use
colnames(data5) <- c("Year", "Total", "Hispanic/Latino", "American Indian/Alaska Native",
                     "Asian", "African American", "Caucasian", "Multiple", "Other")

#For two variables x1000 (to make sure they're thousands,'cause of the . as decimal)
data5 <- data5 %>% mutate(Total=Total*1000, Caucasian=Caucasian*1000)

#All ethnicities into one column
data6 <- data5 %>% gather(Ethnicity, count, 3:9)

#Alter the x-as for years using unique years; 1997, 2002, 2007, 2012, 2017
labels <- unique(data6$Year)

#GGplot bar
p <- ggplot(data6, aes(x=Year, y=count, fill=Ethnicity))+
  geom_bar(stat="identity", position=position_dodge())+
ggtitle(label = "Getting your PhD in the U.S.", subtitle="PhD awarded in Psychology & Social Sciences")+
  xlab("Year")+ ylab("Number of PhD awarded")+labs(caption="Source: NSF,  Plot by @sil_aarts")+
  scale_x_discrete(limits=labels)+
  theme(panel.background = element_rect(fill = "white"),axis.line = element_line(size=1, colour = "black"),
  plot.title = element_text(color="black", face="bold", size=16, hjust=0),
  plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black"),
  plot.caption= element_text(size=8, hjust=1, color="azure4"),
  axis.text.x= element_text(size=10,angle = 90, vjust = 0.5, hjust=1, colour='black'),
  axis.text.y= element_text(size=10, colour='black'),
  axis.title.x = element_text(color = "black", size = 18, angle = 0, hjust = 0.5, vjust = 1),
  axis.title.y = element_text(color = "black", size = 18, angle = 90, hjust = 0.5, vjust = 1)
)

#Run GGplot
p
