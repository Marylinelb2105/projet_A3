x= read.csv("C:/Users/theog/OneDrive/Documents/ISEN/A3/Big-Data/projet_A3/stat_acc_V3.csv", header = TRUE, sep =";" )

correspondance= read.csv("C:/Users/theog/OneDrive/Documents/ISEN/A3/Big-Data/projet_A3/communes-departement-region.csv", header = TRUE, sep ="," )

# Supprimer les colonnes 
correspondancebis <- correspondance[, c("code_commune_INSEE", "nom_region")]


# Récupérer les noms de colonnes existants
noms_colonnes <- colnames(correspondancebis)

# Trouver l'indice de la colonne à renommer
indice_colonne <- which(noms_colonnes == "code_commune_INSEE")

# Renommer la colonne
noms_colonnes[indice_colonne] <- "id_code_insee"

# Réattribuer les noms de colonnes modifiés au tableau
colnames(correspondancebis) <- noms_colonnes

x <- merge(x, correspondancebis, by = "id_code_insee")

pop= read.csv("C:/Users/theog/OneDrive/Documents/ISEN/A3/Big-Data/projet_A3/Regions.csv", header = TRUE, sep =";" )
# Supprimer les colonnes 
popbis <- pop[, c("REG","PMUN")]

# Récupérer les noms de colonnes existants
noms_colonnes <- colnames(popbis)

# Trouver l'indice de la colonne à renommer
indice_colonne <- which(noms_colonnes == "REG")

# Renommer la colonne
noms_colonnes[indice_colonne] <- "nom_region"

# Réattribuer les noms de colonnes modifiés au tableau
colnames(popbis) <- noms_colonnes

x <- merge(x, popbis, by = "nom_region")

#date format date
x$date <- as.POSIXct(x$date)

#valeurs numériques et character
x$Num_Acc <- as.numeric(x$Num_Acc)
x$num_veh <- as.character(x$num_veh)
x$id_usa <- as.numeric(x$id_usa)
x$ville <- as.character(x$ville)
x$id_code_insee <- as.numeric(x$id_code_insee)
x$latitude <- as.numeric(x$latitude)
x$longitude <- as.numeric(x$longitude)
x$an_nais <- as.numeric(x$an_nais)
x$age <- as.numeric(x$age)
x$place <- as.numeric(x$place)

#suppresion des lignes avec lat et lon supérieur a 90°
x <- subset(x, latitude < 90 & longitude < 90)

head(x)

accidents_par_mois <- aggregate(x$Num_Acc, by = list(Mois = format(x$date, "%Y-%m")), FUN = length)
accidents_par_semaine <- aggregate(x$Num_Acc, by = list(Semaine = format(x$date, "%U")), FUN = length)

barplot(accidents_par_mois$x, names.arg = accidents_par_mois$Mois, xlab = "Mois", ylab = "Nombre d'accidents", main = "Évolution du nombre d'accidents par mois")
barplot(accidents_par_semaine$x, names.arg = accidents_par_semaine$Semaine, xlab = "Semaine", ylab = "Nombre d'accidents", main = "Évolution du nombre d'accidents par semaine")

accidents_par_region_gravite <- aggregate(Num_Acc ~ nom_region + descr_grav, x, FUN = length)
data_final <- merge(accidents_par_region_gravite, popbis, by = "nom_region")
data_final$accidents_par_100k <- (data_final$Num_Acc / data_final$PMUN) *100000






