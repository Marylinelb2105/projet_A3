x = read.csv("C:/Users/maryl/OneDrive/Documents/études supérieures/cours/2022-2023/projet_A3_2/projet_A3/stat_acc_V3.csv", header = TRUE, sep = ";")

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)


#Nombre d'accidents en fonction des conditions athmosphériques
count <- x  %>% count(descr_athmo, sort = TRUE)
p <- ggplot(count, mapping = aes(x = descr_athmo, y = n)) + geom_point(size = 3,color = "red")
p <- p  + labs(title    = "Nombre d'accidents en fonction des conditions athmosphériques",
  subtitle = "",
  x        = "Conditions athmosphériques",
  y        = "Nombre d'accidents") + theme_bw()
p
ggsave("accidents-cond_athmo.png", p)


#Nombre d'accidents en fonction de la description de la surface
count <- x  %>% count(descr_etat_surf, sort = TRUE)
p <- ggplot(count, mapping = aes(x = descr_etat_surf, y = n)) + geom_point(size = 3,color = "red")
p <- p  + labs(title    = "Nombre d'accidents en fonction de la description de la surface",
  subtitle = "",
  x        = "Description de la surface",
  y        = "Nombre d'accidents") + theme_bw() 
p
ggsave("accidents-descr_surf.png", p)


#Nombre d'accidents selon la gravité
count <- x  %>% count(descr_grav, sort = TRUE)
p <- ggplot(count, mapping = aes(x = descr_grav, y = n)) + geom_point(size = 3,color = "red")
p <- p  + labs(title    = "Nombre d'accidents selon la gravité",
  subtitle = "",
  x        = "Gravité",
  y        = "Nombre d'accidents") + theme_bw() 
p
ggsave("accidents-gravité.png", p)


#Nombre d'accidents par tranches d'heure
x$date<-ymd_hms(x$date)
count <- x  %>% count(hour(date), sort = TRUE)
p <- ggplot(count, mapping = aes(x = `hour(date)`, y = n)) + geom_point(size = 3,color = "red")
p <- p  + labs(title    = "Nombre d'accidents par tranches d'heure",
               subtitle = "",
               x        = "Heure",
               y        = "Nombre d'accidents") + theme_bw() 
p
ggsave("accidents-heure.png", p)


#Nombre d'accidents par ville (supérieurs à 500)
ville <- x[, c("id_usa", "ville")]
ville$ville <- sub("[[:digit:]]*$", "", ville$ville)
count <-ville %>% count(ville, sort = TRUE)
ville <- subset(count, count$n >= 500 )
p <- ggplot(ville, mapping = aes(x = ville, y = n)) + geom_point(size = 3,color = "red")
p <- p  + labs(title    = "Nombre d'accidents par ville",
  subtitle = "",
  x        = "Ville",
  y        = "Nombre d'accidents") + theme_bw() 
p
ggsave("accidents-ville.png", p)



#Nombre d'accidents par jours
count <- x  %>% count(date(date), sort = FALSE)
lgn=length(count$date)
jour<-count$n
i=1
for(i in 1:lgn){
  jour[i]<-(i+2)%%7
  i=i+1
}
count<-cbind(count,jour)
grp <- count %>% group_by(jour)
agg <- grp %>% summarise(sum(n))
agg$jour[1]<-"A"
agg$jour[2]<-"B"
agg$jour[3]<-"C"
agg$jour[4]<-"D"
agg$jour[5]<-"E"
agg$jour[6]<-"F"
agg$jour[7]<-"G"
p <- ggplot(agg, mapping = aes(x = jour, y = `sum(n)`)) + geom_point(size = 3,color = "red") + labs(
  title    = "Nombre d'accidents par jour",
  subtitle = "",
  x        = "Jour",
  y        = "Nombre d'accidents") + theme_bw() + scale_x_discrete("Jour", 
  labels=c("A"="Lundi","B"="Mardi","C"="Mercredi","D"="Jeudi","E"="Vendredi","F"="Samedi","G"="Dimanche")) + theme_bw() 
p
ggsave("accidents-jour.png", p)

