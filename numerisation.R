x = read.csv("C:/Users/maryl/OneDrive/Documents/études supérieures/cours/2022-2023/projet_A3_2/projet_A3/stat_acc_V3.csv", header = TRUE, sep = ";")
library(dplyr)

x$descr_cat_veh[x$descr_cat_veh == "Autobus"] <- 1
x$descr_cat_veh[x$descr_cat_veh == "Autocar"] <- 2
x$descr_cat_veh[x$descr_cat_veh == "Autre véhicule"] <- 3
x$descr_cat_veh[x$descr_cat_veh == "Bicyclette"] <- 4
x$descr_cat_veh[x$descr_cat_veh == "Cyclomoteur <50cm3"] <- 5
x$descr_cat_veh[x$descr_cat_veh == "Engin spécial"] <- 6
x$descr_cat_veh[x$descr_cat_veh == "Motocyclette > 125 cm3"] <- 7
x$descr_cat_veh[x$descr_cat_veh == "Motocyclette > 50 cm3 et <= 125 cm3"] <- 8
x$descr_cat_veh[x$descr_cat_veh == "PL > 3,5T + remorque"] <- 9
x$descr_cat_veh[x$descr_cat_veh == "PL seul 3,5T <PTCA <= 7,5T"] <- 10
x$descr_cat_veh[x$descr_cat_veh == "PL seul > 7,5T"] <- 11
x$descr_cat_veh[x$descr_cat_veh == "Quad lourd > 50 cm3 (Quadricycle à moteur non carrossé)"] <- 12
x$descr_cat_veh[x$descr_cat_veh == "Quad léger <= 50 cm3 (Quadricycle à moteur non carrossé)"] <- 13
x$descr_cat_veh[x$descr_cat_veh == "Scooter < 50 cm3"] <- 14
x$descr_cat_veh[x$descr_cat_veh == "Scooter > 125 cm3"] <- 15
x$descr_cat_veh[x$descr_cat_veh == "Scooter > 50 cm3 et <= 125 cm3"] <- 16
x$descr_cat_veh[x$descr_cat_veh == "Tracteur agricole"] <- 17
x$descr_cat_veh[x$descr_cat_veh == "Tracteur routier + semi-remorque"] <- 18
x$descr_cat_veh[x$descr_cat_veh == "Tracteur routier seul"] <- 19
x$descr_cat_veh[x$descr_cat_veh == "Train"] <- 20
x$descr_cat_veh[x$descr_cat_veh == "Tramway"] <- 21
x$descr_cat_veh[x$descr_cat_veh == "VL seul"] <- 22
x$descr_cat_veh[x$descr_cat_veh == "VU seul 1,5T <= PTAC <= 3,5T avec ou sans remorque "] <- 23
x$descr_cat_veh[x$descr_cat_veh == 'Voiturette (Quadricycle à moteur carrossé) (anciennement "voiturette ou tricycle à moteur")'] <- 24

x$descr_agglo[x$descr_agglo == "En agglomération"] <- 1
x$descr_agglo[x$descr_agglo == "Hors agglomération"] <- 2

x$descr_athmo[x$descr_athmo == "Autre"] <- 1
x$descr_athmo[x$descr_athmo == "Brouillard – fumée"] <- 2
x$descr_athmo[x$descr_athmo == "Neige – grêle"] <- 3
x$descr_athmo[x$descr_athmo == "Normale"] <- 4
x$descr_athmo[x$descr_athmo == "Pluie forte"] <- 5
x$descr_athmo[x$descr_athmo == "Pluie légère"] <- 6
x$descr_athmo[x$descr_athmo == "Temps couvert"] <- 7
x$descr_athmo[x$descr_athmo == "Temps éblouissant"] <- 8
x$descr_athmo[x$descr_athmo == "Vent fort – tempête"] <- 9

x$descr_lum[x$descr_lum == "Crépuscule ou aube"] <- 1
x$descr_lum[x$descr_lum == "Nuit avec éclairage public allumé"] <- 2
x$descr_lum[x$descr_lum == "Nuit avec éclairage public non allumé"] <- 3
x$descr_lum[x$descr_lum == "Nuit sans éclairage public"] <- 4
x$descr_lum[x$descr_lum == "Plein jour"] <- 5

x$descr_etat_surf[x$descr_etat_surf == "Autre"] <- 1
x$descr_etat_surf[x$descr_etat_surf == "Boue"] <- 2
x$descr_etat_surf[x$descr_etat_surf == "Corps gras – huile"] <- 3
x$descr_etat_surf[x$descr_etat_surf == "Enneigée"] <- 4
x$descr_etat_surf[x$descr_etat_surf == "Flaques"] <- 5
x$descr_etat_surf[x$descr_etat_surf == "Inondée"] <- 6
x$descr_etat_surf[x$descr_etat_surf == "Mouillée"] <- 7
x$descr_etat_surf[x$descr_etat_surf == "Normale"] <- 8
x$descr_etat_surf[x$descr_etat_surf == "Verglacée"] <- 9

x$description_intersection[x$description_intersection == "Autre intersection"] <- 1
x$description_intersection[x$description_intersection == "Giratoire"] <- 2
x$description_intersection[x$description_intersection == "Hors intersection"] <- 3
x$description_intersection[x$description_intersection == "Intersection en T"] <- 4
x$description_intersection[x$description_intersection == "Intersection en X"] <- 5
x$description_intersection[x$description_intersection == "Intersection en Y"] <- 6
x$description_intersection[x$description_intersection == "Intersection à plus de 4 branches"] <- 7
x$description_intersection[x$description_intersection == "Passage à niveau"] <- 8
x$description_intersection[x$description_intersection == "Place"] <- 9

x$descr_dispo_secu[x$descr_dispo_secu == "Autre - Non déterminable"] <- 1
x$descr_dispo_secu[x$descr_dispo_secu == "Autre - Non utilisé"] <- 2
x$descr_dispo_secu[x$descr_dispo_secu == "Autre - Utilisé"] <- 3
x$descr_dispo_secu[x$descr_dispo_secu == "Présence d'un casque - Utilisation non déterminable"] <- 4
x$descr_dispo_secu[x$descr_dispo_secu == "Présence d'un casque non utilisé "] <- 5
x$descr_dispo_secu[x$descr_dispo_secu == "Présence d'un dispositif enfant non utilisé"] <- 6
x$descr_dispo_secu[x$descr_dispo_secu == "Présence d'un équipement réfléchissant non utilisé"] <- 7
x$descr_dispo_secu[x$descr_dispo_secu == "Présence d'une ceinture de sécurité - Utilisation non déterminable"] <- 8
x$descr_dispo_secu[x$descr_dispo_secu == "Présence de ceinture de sécurité non utilisée "] <- 9
x$descr_dispo_secu[x$descr_dispo_secu == "Présence dispositif enfant - Utilisation non déterminable"] <- 10
x$descr_dispo_secu[x$descr_dispo_secu == "Présence équipement réfléchissant - Utilisation non déterminable"] <- 11
x$descr_dispo_secu[x$descr_dispo_secu == "Utilisation d'un casque "] <- 12
x$descr_dispo_secu[x$descr_dispo_secu == "Utilisation d'un dispositif enfant"] <- 13
x$descr_dispo_secu[x$descr_dispo_secu == "Utilisation d'un équipement réfléchissant "] <- 14
x$descr_dispo_secu[x$descr_dispo_secu == "Utilisation d'une ceinture de sécurité "] <- 15

x$descr_grav[x$descr_grav == "Blessé hospitalisé"] <- 1
x$descr_grav[x$descr_grav == "Blessé léger"] <- 2
x$descr_grav[x$descr_grav == "Indemne"] <- 3
x$descr_grav[x$descr_grav == "Tué"] <- 4

x$descr_motif_traj[x$descr_motif_traj == "Autre"] <- 1
x$descr_motif_traj[x$descr_motif_traj == "Courses – achats"] <- 2
x$descr_motif_traj[x$descr_motif_traj == "Domicile – travail"] <- 3
x$descr_motif_traj[x$descr_motif_traj == "Domicile – école"] <- 4
x$descr_motif_traj[x$descr_motif_traj == "Promenade – loisirs"] <- 5
x$descr_motif_traj[x$descr_motif_traj == "Utilisation professionnelle"] <- 6

x$descr_type_col[x$descr_type_col == "Autre collision"] <- 1
x$descr_type_col[x$descr_type_col == "Deux véhicules - Frontale"] <- 2
x$descr_type_col[x$descr_type_col == "Deux véhicules – Par le coté"] <- 3
x$descr_type_col[x$descr_type_col == "Deux véhicules – Par l’arrière"] <- 4
x$descr_type_col[x$descr_type_col == "Sans collision"] <- 5
x$descr_type_col[x$descr_type_col == "Trois véhicules et plus – Collisions multiples"] <- 6
x$descr_type_col[x$descr_type_col == "Trois véhicules et plus – En chaîne"] <- 7