#TidyTueasday
#How long does in take to get your Phd? Median PhD Years
#Does it differ per sex?
install.packages("datapasta")

#Load library
library(ggplot2)
library(dplyr)
library(datapasta)

#Datapaste, so less cleaning is necessary
data <- data.frame(
          V1 = c(58, 59),
          V2 = c(26.305, 22.397),
          V3 = c(57, 58),
          V4 = c(5.168, 6.335),
          V5 = c(57, 56),
          V6 = c(3.723, 1.831),
          V7 = c(57, 58),
          V8 = c(2.614, 836),
          V9 = c(60, 60),
         V10 = c(3.245, 4.614),
         V11 = c(53, 53),
         V12 = c(6.681, 2.086),
         V13 = c(63, 65),
         V14 = c(1.352, 2.878),
         V15 = c(70, 73),
         V16 = c(2.301, 2.4),
         V17 = c(54, 60),
         V18 = c(1.221, 1.417)
) 

#Delete columns 
data2 <- data[, c(1,3,9)]

#Add colnames
colnames(data2) <- c("All fields", "Life Sciences", "Psychology and Social Sciences")

#Add one variabele gender
sex <- c("Male", "Female")
data3 <- cbind(sex, data2) 

#All fields into one column
data4 <- data3 %>% gather(Field, Count, 2:4 )

#Divide by 10 to get median years
data4$Count <- data4$Count/10
  
#Labels
labels <- unique(data4$Field)

#GGplot
p <- ggplot(data4, aes(x=Field, y=Count, fill=sex))+
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle(label = "Getting your PhD in the U.S.", subtitle="Median years to doctorate")+
  labs(caption="Source: National Science Foundation,  Plot by @sil_aarts")+
  scale_x_discrete(limits=labels)+
  scale_fill_manual(values = c("Male" = "powderblue", "Female"="rosybrown1"))+
  geom_text(aes(label=Count), position=position_dodge(width=0.9), vjust=-0.25)+
  coord_flip()+ 
  theme_minimal(14) +
  theme(legend.position = "bottom",
        plot.title = element_text(size=18, hjust=0, color="black", face="bold"),
        plot.subtitle=element_text(size=16, hjust=0, face="italic", color="black"),
        plot.caption= element_text(size=8, hjust=1, color="black"),
        axis.text.x= element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
p

#Save plot
ggsave("Desktop/R/TidyTuesday/TidyTuesday_PhDtime.png", width = 20, height = 20)
