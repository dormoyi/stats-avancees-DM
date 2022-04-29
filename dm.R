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
#### 1.

############################
#### 2.

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

