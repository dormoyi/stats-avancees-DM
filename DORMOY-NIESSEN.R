#### DM de DORMOY et NIESSEN
rm(list=objects())
graphics.off()

############################
#### A COMPLETER:
grp_id = 'T' #à remplacer par le nom de votre groupe (voir sur Edunao)
setwd("~/Documents/projets_git/stats-avancees-DM") # pour Ines
# mets ici ta ligne nat

############################
#### A NE PAS TOUCHER:
data_poll<-read.table("pollution.txt",sep=" ",header=T,dec=".")
dict = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'AA', 'AB', 'AC', 'AD', 'AE', 'AF', 'AG', 'AH', 'AI', 'AJ', 'AK', 'AL', 'AM' )
set.seed(which(dict==grp_id))
idx = sample(1:length(data_poll$pollution))[1:80]
data_poll = data_poll[idx, ]

############################
#### SUITE DU FICHIER A COMPLETER:
############################
#### 1. Faire une ou des représentation(s) graphique(s) unidimensionnelle(s) des données et commenter
df = data_poll
dim(df) # 80 13
summary(df)

# we replace the character values by numbers 
# pluie = 1, sec  = 0
df['pluie'][df['pluie'] == 'Sec'] <- as.numeric(0)
df['pluie'][df['pluie'] == 'Pluie'] <- as.numeric(1)
df['pluie'] <- lapply(df['pluie'] , function(x) if(is.character(x)) as.numeric(x) else x)
# Nord=1, Ouest=2, Sud=3, Est=4 (maybe we could one hot encode that later)
df['vent'][df['vent'] == 'Nord'] <- as.numeric(1)
df['vent'][df['vent'] == 'Ouest'] <- as.numeric(2)
df['vent'][df['vent'] == 'Sud'] <- as.numeric(3)
df['vent'][df['vent'] == 'Est'] <- as.numeric(4)
df['vent'] <- lapply(df['vent'] , function(x) if(is.character(x)) as.numeric(x) else x)
summary(df)

par(mfrow=c(1,2),oma=c(0,0,3,0))    
pluie.table=table(df$pluie)
pie(pluie.table,col=rainbow(4),main="Ratio des jours de pluie")
title(main="Plots des jours de vent et de",
      outer=TRUE)

vent.table=table(df$vent)
pie(vent.table,col=rainbow(4),main="Ratio des jours de vent")

plot(df$pollution, main="Pollution en fonction du point dans le dataset")

plot(df$T9, main="Pollution en fonction du point dans le dataset")

plot(df$T9, type = "l", col = 1)  # Plot with Base R
lines(df$T12, type = "l", col = 2)
lines(df$T15, type = "l", col = 3)
title(main="Températures aux différentes heures de la jounée pour chaque point",
      outer=FALSE)




############################
#### 2.
pairs(df) #trace tous les graphes à 2 variables possibles

# les variables de température semblent assez corrélées entre elles
# pareil pour les variables Vx


############################
#### 3.

############################
#### 5.

############################
#### 6.

############################
#### 7.

############################
#### 8.

############################
#### 9.

############################
#### 11.

############################
#### 12.

