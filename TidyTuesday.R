#TidyTuesday
#Board Games: love it! Especially, Monopoly and 'Colonisten van Catan'!

#Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(wordcloud)
library(RColorBrewer)
library(tidyr)
library(tidytext)
library(stopwords)
library(tm)

#Read file
games<- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")

#Select only one variabele
games_des <- select(games, select="description")

#Column name
colnames (games_des) <- c("games")

#Every word one vector
games_des2 <- games_des %>% 
  unnest_tokens(word, games) 

#Delete stopwords
games_des3 <- games_des2 %>% 
anti_join(get_stopwords(language="en", source="snowball"))
  
#Do some cleaning!
games_des3$word <- removeNumbers(games_des3$word, ucp = FALSE)
games_des3$word <- str_replace(games_des3$word, ";", "")
games_des3$word <- str_replace(games_des3$word, "x", "")
games_des3$word <- str_replace(games_des3$word, "s", "")
games_des3$word <- str_replace(games_des3$word, "i", "")
games_des4 <- games_des3[games_des3$word!="",]

#Frequency words
games_des5 <- games_des4 %>% count(word, sort=T)
sort(games_des5$n)
games_des5$n <- as.numeric(games_des5$n)

#Wordcloud
set.seed(1234)
wordcloud(words = games_des5$word, freq = games_des5$n, min.freq = 1,scale=c(2,0.5),
          max.words=100, random.order=FALSE, use.r.layout=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#Make some colors (10 instead of 6 from Dark2)
nb.cols <- 10
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)

#Filter (n>1000) 
p <- games_des5 %>% 
  filter (n > 5000) %>%
#Make GGplot
  ggplot(aes(x = word, y = n)) +
  geom_bar(stat = "identity", fill=mycolors)+
  geom_text(aes(label=word), hjust="left", nudge_y = 0.5, size = 4, family="Comic Sans MS")+
  ggtitle(label = "TidyTuesday", subtitle="Wanna play a (board) game?")+
  xlab("")+ ylab("# of occurences")+labs(caption="Source: Board Game Geeks, Plot by @sil_aarts")+
  theme_minimal(10) +
  theme(legend.position = "none",
        text=element_text(family="Comic Sans MS"),
        plot.title=element_text(size=14, hjust=0, face='bold'),
        plot.subtitle=element_text(size=13, hjust=0, face='italic'),
        plot.caption=element_text(size=8, hjust=1),
        axis.text.x = element_text(size=10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  coord_flip()

#Run it!
p

