x = read.csv("C:/Users/maryl/OneDrive/Documents/études supérieures/cours/2022-2023/projet_A3_2/projet_A3/stat_acc_V3.csv", header = TRUE, sep = ";")

library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)

split.screen(1:3)

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
