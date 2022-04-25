rm(list=ls())

### Importing datasets  

library(readr)
library(ggplot2)
library(dplyr)
games <- read_csv("C:/Users/leonk/Downloads/Ligue1 Championship.csv")


# less games before 2002/2003 season (only 18 teams before and 20 after)
table(games$Season)

#total goals/games for every season

games = games %>% 
  group_by(Season) %>%  
  mutate(goals= sum(Score))

n <- nlevels(as.factor(games$Season))
year <- levels(as.factor(games$Season))
tgoals <- c()
for (i in 1:n){
  tgoals[i] <- games$goals[(i-1)*380 + 1]
}

k <- cbind(year,tgoals)
k<- data.frame(k)
k$tgoals <- as.numeric(k$tgoals)
k$tgoals[1] <- k$tgoals[1]/306
k$tgoals[2] <- k$tgoals[2]/306
k$tgoals[3] <- k$tgoals[3]/306
for (j in 4:20){
  k$tgoals[j] <- k$tgoals[j]/380
}


graphic1<-ggplot(data=k, aes(x=year, y=tgoals)) +  
  geom_bar(stat="identity",fill = 'black',width=0.5) +
  theme_minimal() +
  scale_y_continuous(name = 'Goals per game')


graphic1


#home/draw/away win between seasons (barplot?)
#faire un barplot pour la saison 2018/2019 puis ensuite barplot discrétisé


# goals scored by teams between seasons


# ranking bewteen seasons vs goals bewteen seasons


#rankings






