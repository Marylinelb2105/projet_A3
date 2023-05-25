library(leaflet)
library(geojsonio)
library(geojson)
library(dplyr)
library(stringr)
library(readxl)
library(ggplot2)
library(tidyr)
library(lubridate)
#recuperation du fichier csv
x = read.csv("stat_acc_V3.csv", header = TRUE, sep = ";")

#### NETTOYAGE DES DONNEES ####

#nettoyage des valeurs NULL et NA
x <- subset(x, age != "NULL" & place != "NULL" & id_code_insee != "NULL" & an_nais != "NULL")
x <- x[!is.null(x$id_code_insee) & !is.null(x$an_nais) & !is.null(x$age) & !is.null(x$place), ]
x <- x[!is.na(x$id_code_insee) & !is.na(x$an_nais) & !is.na(x$age) & !is.na(x$place), ]

#numerisation des données
x$Num_Acc <- as.numeric(x$Num_Acc)
x$id_usa <- as.numeric(x$id_usa)
x$id_code_insee <- as.character(x$id_code_insee)
x$latitude <- as.numeric(x$latitude)
x$longitude <- as.numeric(x$longitude)
x$an_nais <- as.numeric(x$an_nais)
x$age <- as.numeric(x$age)
x$place <- as.numeric(x$place)
x$ville <- as.character(x$ville)
x$date <- ymd_hms(x$date)

#on retire 14 pour obtenir l'age des perosnnes en 2009 et pas à l'heure actuelle
x <- x %>% mutate(age = age - 14)

#suppresion des mauvaises lat et lon
x <- subset(x, latitude < 90 & latitude >-90)
x <- subset(x, longitude < 180 & longitude >-180)


#nettoyage des données
x$descr_cat_veh <- as.numeric(factor(x$descr_cat_veh))
x$descr_agglo <- as.numeric(factor(x$descr_agglo))
x$descr_athmo <- as.numeric(factor(x$descr_athmo))
x$descr_lum <- as.numeric(factor(x$descr_lum))
x$descr_etat_surf <- as.numeric(factor(x$descr_etat_surf))
x$description_intersection <- as.numeric(factor(x$description_intersection))
x$descr_dispo_secu <- as.numeric(factor(x$descr_dispo_secu))
x$descr_grav <- as.numeric(factor(x$descr_grav))
x$descr_motif_traj <- as.numeric(factor(x$descr_motif_traj))
x$descr_type_col <- as.numeric(factor(x$descr_type_col))


#### séries chronologiques sur l’évolution du nombre d’accidents par mois et semaines sur l’ensemble de la période ####

#création des tableau
accidents_par_mois <- aggregate(x$Num_Acc, by = list(Mois = format(x$date, "%Y-%m")), FUN = length)
accidents_par_semaine <- aggregate(x$Num_Acc, by = list(Semaine = format(x$date, "%Y-%U")), FUN = length)

#affichage des accidents par mois et semaines 
barplot(accidents_par_mois$x, names.arg = accidents_par_mois$Mois, xlab = "Mois", ylab = "Nombre d'accidents", main = "Évolution du nombre d'accidents par mois")
barplot(accidents_par_semaine$x, names.arg = accidents_par_semaine$Semaine, xlab = "Semaine", ylab = "Nombre d'accidents", main = "Évolution du nombre d'accidents par semaine")

serie_mois <- ts(accidents_par_mois$x, start = c(2009, 0), frequency = 12)

# Affichage de la série chronologique des accidents par mois
plot(serie_mois, xlab = "Mois", ylab = "Nombre d'accidents", main = "Évolution du nombre d'accidents par mois")

# Création de la série chronologique des accidents par semaine
serie_semaine <- ts(accidents_par_semaine$x, start = c(2009, 1), frequency = 52)

# Affichage de la série chronologique des accidents par semaine
plot(serie_semaine, xlab = "Semaine", ylab = "Nombre d'accidents", main = "Évolution du nombre d'accidents par semaine")



####Construire un jeu de données avec le nombre d’accidents selon la gravité pour 100.000 habitants par région ####

correspondance= read_excel("code-2009.xls" , sheet = 1)
# Garder les colonnes 
correspondancebis <- correspondance[, c("CODGEO", "REG")]


# Récupérer les noms de colonnes existants
noms_colonnes <- colnames(correspondancebis)

# Trouver l'indice de la colonne à renommer
indice_colonne <- which(noms_colonnes == "CODGEO")

# Renommer la colonne
noms_colonnes[indice_colonne] <- "id_code_insee"

# Réattribuer les noms de colonnes modifiés au tableau
colnames(correspondancebis) <- noms_colonnes

# Récupérer les noms de colonnes existants
noms_colonnes <- colnames(correspondancebis)

# Trouver l'indice de la colonne à renommer
indice_colonne <- which(noms_colonnes == "REG")

# Renommer la colonne
noms_colonnes[indice_colonne] <- "code"

# Réattribuer les noms de colonnes modifiés au tableau
colnames(correspondancebis) <- noms_colonnes

#Ajout des regions dans la base 
x <- merge(x, correspondancebis, by = "id_code_insee")

pop= read.csv("Regions.csv", header = TRUE, sep ="," ,encoding = "UTF-8")

popbis <- pop[, c("PMUN", "code")]


#ajout des populations des regions
x <- merge(x, pop, by = "code")

accidents_par_region_gravite <- aggregate(Num_Acc ~ code + descr_grav + code, x, FUN = length)
data_final_regions <- merge(accidents_par_region_gravite, pop, by = "code")

###le jeu de données pour les régions
data_final_regions$accidents_par_100k_total <- (data_final_regions$Num_Acc / data_final_regions$PMUN) * 100000


deptest <- read.csv("correspondance-code-insee-code-postal.csv", header = TRUE, sep = ";")
deppop <- read.csv("Departements.csv", header = TRUE, sep = ",")

# Supprimer les colonnes 
deptestbis <- deptest[, c("Code.INSEE", "Département","Code.Département")]
deptestbis <- deptestbis %>% rename(id_code_insee = Code.INSEE)

x <- merge(x, deptestbis, by = "id_code_insee")

deppop <- deppop[, c("Population","X.Code.Département")]
deppop <- deppop %>% rename(Code.Département = X.Code.Département)


accidents_par_departement_gravite <- aggregate(Num_Acc ~ Département + descr_grav + Code.Département, x, FUN = length)
data_final_departement <- merge(accidents_par_departement_gravite, deppop, by = "Code.Département")

###le jeu de données pour les départements
data_final_departement$accidents_par_100k_departement <- (data_final_departement$Num_Acc / data_final_departement$Population) *100000


#### LES REPRESENTATIONS GRAPHIQUES ####
#recuperation du fichier csv
x2 = read.csv("stat_acc_V3.csv", header = TRUE, sep = ";")
#suppresion des mauvaises lat et lon
x2 <- subset(x2, latitude < 90 & latitude >-90)
x2 <- subset(x2, longitude < 180 & longitude >-180)

#### NETTOYAGE DES DONNEES ####

#nettoyage des valeurs NULL et NA
x2 <- subset(x2, age != "NULL" & place != "NULL" & id_code_insee != "NULL" & an_nais != "NULL")
x2 <- x2[!is.null(x2$id_code_insee) & !is.null(x2$an_nais) & !is.null(x2$age) & !is.null(x2$place), ]
x2 <- x2[!is.na(x2$id_code_insee) & !is.na(x2$an_nais) & !is.na(x2$age) & !is.na(x2$place), ]

#Nombre d'accidents en fonction des conditions athmosphériques
count <- x2  %>% count(descr_athmo, sort = TRUE)
p <- ggplot(count, mapping = aes(x = descr_athmo, y = n)) + geom_point(size = 3,color = "red")
p <- p  + labs(title    = "Nombre d'accidents en fonction des conditions athmosphériques",
               subtitle = "",
               x        = "Conditions athmosphériques",
               y        = "Nombre d'accidents") + theme_bw()
p
ggsave("img/accidents-cond_athmo.png", p)


#Nombre d'accidents en fonction de la description de la surface
count <- x2  %>% count(descr_etat_surf, sort = TRUE)
p <- ggplot(count, mapping = aes(x = descr_etat_surf, y = n)) + geom_point(size = 3,color = "red")
p <- p  + labs(title    = "Nombre d'accidents en fonction de la description de la surface",
               subtitle = "",
               x        = "Description de la surface",
               y        = "Nombre d'accidents") + theme_bw() 
p
ggsave("img/accidents-descr_surf.png", p)


#Nombre d'accidents selon la gravité
count <- x2  %>% count(descr_grav, sort = TRUE)
p <- ggplot(count, mapping = aes(x = descr_grav, y = n)) + geom_point(size = 3,color = "red")
p <- p  + labs(title    = "Nombre d'accidents selon la gravité",
               subtitle = "",
               x        = "Gravité",
               y        = "Nombre d'accidents") + theme_bw() 
p
ggsave("img/accidents-gravité.png", p)


#Nombre d'accidents par tranches d'heure

count <- x2  %>% count(hour(date), sort = TRUE)
p <- ggplot(count, mapping = aes(x = `hour(date)`, y = n)) + geom_point(size = 3,color = "red")
p <- p  + labs(title    = "Nombre d'accidents par tranches d'heure",
               subtitle = "",
               x        = "Heure",
               y        = "Nombre d'accidents") + theme_bw() 
p
ggsave("img/accidents-heure.png", p)


#Nombre d'accidents par ville (supérieurs à 500)
ville <- x2[, c("id_usa", "ville")]
ville$ville <- sub("[[:digit:]]*$", "", ville$ville)
count <-ville %>% count(ville, sort = TRUE)
ville <- subset(count, count$n >= 500 )
p <- ggplot(ville, mapping = aes(x = ville, y = n)) + geom_point(size = 3,color = "red")
p <- p  + labs(title    = "Nombre d'accidents par ville",
               subtitle = "",
               x        = "Ville",
               y        = "Nombre d'accidents") + theme_bw() 
p
ggsave("img/accidents-ville.png", p)



#Nombre d'accidents par jours
count <- x2  %>% count(date(date), sort = FALSE)
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
ggsave("img/accidents-jour.png", p)

#### LES HISTOGRAMMES ####

#nbre de tranche d'age
nb_tranche <- 10

#on recupere les ages mais separés par tranches d'age
new_age <- cut(as.numeric(x$age), breaks = nb_tranche, labels = c("0-14", "14-25","25-36","36-47", "47-58", "58-69", "69-80", "80-90", "90-101", "101-112"))

#deign de l'histogramme
accident <- ggplot(x, aes(x=new_age, fill = new_age)) + labs(
  title = "Nombre d’accidents par tranche d'âge en 2009",
  x = "tranche d'âge",
  y = "Nombre d'accidents",
  
) + geom_bar(na.rm = TRUE)

#on sauvegarde l'image en png
ggsave("img/nbre_accident_tranche_age_2009.png", accident)

#moyenne mensuel des accidents
age1 <- x$age
mois1 <- format(x$date, format="%m")
donnees <- data.frame(age = age1, mois = mois1)
resultat <- aggregate(age1 ~ mois1, x = donnees, FUN = mean)

#on fait 12 tranches pour les 12 mois de l'année
nb_tranches <- 12

#on repartit par mois
new_mois <- cut(as.numeric(mois1), breaks = nb_tranches, labels = c("janv", "fevr", "mars", "avril", "mai", "juin", "juillet", "aout", "sept", "oct", "nov", "dec"))

#design de l'histogramme
moyenne <- ggplot(x, aes(x=new_mois, fill = new_mois)) + labs(
  title = "Moyenne des accidents mensuels",
  x = "mois",
  y = "moyenne des accidents",
  
) + geom_bar(na.rm = TRUE)

#on sauvegarde l'image en png
ggsave("img/moyenne_accidents_en_2009.png", moyenne)


####une représentation sous formes de carte de la quantité d’accidents enregistrés par région puis par départements ####

#1 carte par regions

# Aggréger les accidents par 100k habitants au total par région
accidents_par_region_total <- aggregate(accidents_par_100k_total ~ nom_region + code, data_final_regions, FUN = sum)


regions <- geojsonio::geojson_read("regions.geojson", what = "sp")

regions$Num_Acc <- accidents_par_region_total$accidents_par_100k_total[match(regions$code, accidents_par_region_total$code)]
regions$Num_Acc <- as.numeric(regions$Num_Acc)


bins <- c(0,20,40,60,80,100,120,140,160)
pal <- colorBin("YlOrRd", domain = regions$Num_Acc, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%s accidents pour 100000 habitants",
  regions$nom, regions$Num_Acc
) %>% lapply(htmltools::HTML)

leaflet(regions) %>%
  setView(2.5, 46.5, zoom = 5) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    data = regions,
    fillColor = ~pal(Num_Acc),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~Num_Acc, opacity = 0.7, title = "Nombres d'accidents par régions par 100k habitants",
            position = "bottomright")


# Aggréger les accidents par 100k habitants au total par départements
accidents_par_départment_total <- aggregate(accidents_par_100k_departement ~ Département + Code.Département, data_final_departement, FUN = sum)


dep <- geojsonio::geojson_read("https://france-geojson.gregoiredavid.fr/repo/departements.geojson", what = "sp")

dep$Num_Acc <- accidents_par_départment_total$accidents_par_100k_departement[match(dep$code, accidents_par_départment_total$Code.Département)]
dep$Num_Acc <- as.numeric(dep$Num_Acc)

bins <- c(0,50,100,150,200,250)
pal <- colorBin("YlOrRd", domain = dep$Num_Acc, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%s accidents pour 100000 habitants",
  dep$nom, dep$Num_Acc
) %>% lapply(htmltools::HTML)

leaflet(dep) %>%
  setView(2.5, 46.5, zoom = 5) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal(Num_Acc),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~Num_Acc, opacity = 0.7, title = "Nombres d'accidents par départements pour 100000 habitants",
            position = "bottomright")


####CARTE avec les taux d’accidents graves ####

# Filtrer les données pour ne conserver que les lignes avec gravite = 4
data_final_regions_filtre <- filter(data_final_regions, descr_grav == 4)


regions2 <- geojsonio::geojson_read("regions.geojson", what = "sp")

regions2$Num_Acc <- data_final_regions_filtre$Num_Acc[match(regions2$code, data_final_regions_filtre$code)]
regions2$Num_Acc <- as.numeric(regions2$Num_Acc)


bins <- c(0,50,100,150,200)
pal <- colorBin("YlOrRd", domain = regions2$Num_Acc, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%s accidents",
  regions2$nom, regions2$Num_Acc
) %>% lapply(htmltools::HTML)

leaflet(regions2) %>%
  setView(2.5, 46.5, zoom = 5) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    data = regions2,
    fillColor = ~pal(Num_Acc),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~Num_Acc, opacity = 0.7, title = "Accidents mortel par régions",
            position = "bottomright")


#### Etude des relations entre variables qualitatives ####

#Tableaux croisés et test du chi2 du type de vehicule par rapport aux descriptions temps, agglomeration...
table_veh_agglo <- table(x$descr_agglo, x$descr_cat_veh)
chi2_test <- chisq.test(table_veh_agglo)
print(chi2_test)

table_veh_athmo <- table(x$descr_athmo, x$descr_cat_veh)
chi2_test1 <- chisq.test(table_veh_athmo)
print(chi2_test1)

table_veh_lum <- table(x$descr_lum, x$descr_cat_veh)
chi2_test2 <- chisq.test(table_veh_lum)
print(chi2_test2)

table_veh_surf <- table(x$descr_etat_surf, x$descr_cat_veh)
chi2_test3 <- chisq.test(table_veh_surf)
print(chi2_test3)

table_veh_inter <- table(x$description_intersection, x$descr_cat_veh)
chi2_test4 <- chisq.test(table_veh_inter)
print(chi2_test4)

table_veh_sec <- table(x$descr_dispo_secu, x$descr_cat_veh)
chi2_test5 <- chisq.test(table_veh_sec)
print(chi2_test5)

table_veh_grav <- table(x$descr_grav, x$descr_cat_veh)
chi2_test6 <- chisq.test(table_veh_grav)
print(chi2_test6)

table_veh_motif <- table(x$descr_motif_traj, x$descr_cat_veh)
chi2_test7 <- chisq.test(table_veh_motif)
print(chi2_test7)

table_veh_col <- table(x$descr_type_col, x$descr_cat_veh)
chi2_test8 <- chisq.test(table_veh_col)
print(chi2_test8)


#Graphique avec mosaicplot du type de vehicule par rapport aux descriptions temps, agglomeration...

#Tableaux croisés et test du chi2 de la gravite en fonction de la lumiere, de la surface et de l'âge
table_grav_lum <- table(x$descr_grav, x$descr_lum)
chi2_test9 <- chisq.test(table_grav_lum)
print(chi2_test9)

table_grav_surf <- table(x$descr_grav, x$descr_etat_surf)
chi2_test10 <- chisq.test(table_grav_surf)
print(chi2_test10)

table_grav_age <- table(x$descr_grav, x$age)
chi2_test11 <- chisq.test(table_grav_age)
print(chi2_test11)

table_grav_secu <- table(x$descr_grav, x$descr_dispo_secu)
chi2_test12 <- chisq.test(table_grav_secu)
print(chi2_test12)


#sauvegarde des mosaicplot en png
#catégorie de véhicule
im1 <- paste0("img/cat_vs_agglo.png")
png(im1)
mosaicplot(table_veh_agglo, main = "Test du chi2 entre descr_cat_veh et descr_agglo", xlab = "desc_agglo", ylab = "descr_cat_veh", shade=TRUE)
dev.off()

im2 <- paste0("img/cat_vs_athmo.png")
png(im2)
mosaicplot(table_veh_athmo, main = "Test du chi2 entre descr_cat_veh et descr_athmo", xlab = "descr_athmo", ylab = "descr_cat_veh", shade=TRUE)
dev.off()

im3 <- paste0("img/cat_vs_lum.png")
png(im3)
mosaicplot(table_veh_lum, main = "Test du chi2 entre descr_cat_veh et descr_lum", xlab = "descr_lum", ylab = "descr_cat_veh", shade=TRUE)
dev.off()

im4 <- paste0("img/cat_vs_surf.png")
png(im4)
mosaicplot(table_veh_surf, main = "Test du chi2 entre descr_cat_veh et descr_surf", xlab = "descr_surf", ylab = "descr_cat_veh", shade=TRUE)
dev.off()

#gravite
im5 <- paste0("img/grav_vs_lum.png")
png(im5)
mosaicplot(table_grav_lum, main = "Gravite vs luminosité", xlab = "Gravite", ylab = "Luminosité", shade=TRUE)
dev.off()

im6 <- paste0("img/grav_vs_surf.png")
png(im6)
mosaicplot(table_grav_surf, main = "Gravité vs état de la surface", xlab = "Gravite", ylab = "Etat de la surface", shade=TRUE)
dev.off()

im7 <- paste0("img/grav_vs_age.png")
png(im7)
mosaicplot(table_grav_age, main = "Gravité vs age", xlab = "Gravité", ylab = "Age", shade=TRUE)
dev.off()

im8 <- paste0("img/grav_vs_secu.png")
png(im8)
mosaicplot(table_grav_secu, main = "Gravité vs dispositif de sécurité", xlab = "Gravité", ylab = "Dispositif de sécurité", shade=TRUE)
dev.off()


#### régressions linéaires de l’évolution du nombre d’accidents par mois, puis par semaine ####

#split.screen(1:3)

#Regression linéaire de l'évolution du nombre d'accidents par mois
x$date <- as.POSIXct(x$date)
mois <- aggregate(x$Num_Acc, by = list(Mois = format(x$date, "%m")), FUN = length)
mois$Mois <- as.numeric(mois$Mois)
mois$x <- cumsum(mois$x)
X<- mois$x
y<- mois$Mois

reg_mois <- lm(X~y, data=mois)
screen(1);plot(X~y,pch=16,data=mois,main="Evolution du nombre d'accidents par mois",
               xlab="Mois",
               ylab="Somme cumulée des accidents") + abline(reg_mois,col="red",lwd=2)


#Regression linéaire de l'évolution du nombre d'accidents par semaine
semaine <- aggregate(x$Num_Acc, by = list(Semaine = format(x$date, "%U")), FUN = length)
semaine$Semaine <- as.numeric(semaine$Semaine)
semaine$x <- cumsum(semaine$x)
X<- semaine$x
y<- semaine$Semaine

reg_semaine <- lm(X~y, data=semaine)
screen(2);plot(X~y,pch=16,data=semaine,main="Evolution du nombre d'accidents par semaine",
               xlab="Semaine",
               ylab="Somme cumulée des accidents") + abline(reg_semaine,col="red",lwd=2) + abline(reg_semaine,col="red",lwd=2)

#Analyse
print("Performances de la régression (proportion due aux résidus): ")
cat("par mois: ", sum(reg_mois$residuals^2),"\n")
cat("par semaine: ", sum(reg_semaine$residuals^2),"\n")

print("Erreurs types associées aux estimateurs: ")
cat("par mois: ", summary(reg_mois)$coefficient[,"Std. Error"],"\n")
cat("par semaine: ", summary(reg_semaine)$coefficient[,"Std. Error"],"\n")

print("Intervales de confiance à 95% pour ces estimateurs: ")
cat("par mois: ", confint(reg_mois),"\n")
cat("par semaine: ", confint(reg_semaine),"\n")

print("Calcul du R² et R² ajusté: ") 
cat("par mois: R²:", summary(reg_mois)$r.squared,"R² ajusté: ",summary(reg_mois)$adj.r.squared,"\n")
cat("par semaine: R²:", summary(reg_semaine)$r.squared,"R² ajusté: ",summary(reg_semaine)$adj.r.squared,"\n")