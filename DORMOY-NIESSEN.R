#### DM de DORMOY et NIESSEN
rm(list=objects())
graphics.off()

############################
#### A COMPLETER:
grp_id = 'T' #à remplacer par le nom de votre groupe (voir sur Edunao)
setwd("~/Documents/projets_git/stats-avancees-DM") # pour Ines
#setwd("C:/Users/natan/Documents/2A/Statistiques avanc?es/DM/stats-avancees-DM") #pour Natascha

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
head(df)
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
labels <- c("Sec", "Pluie")
pie(pluie.table,labels, col=rainbow(4),main="Ratio des jours de pluie", radius = 0.9)
title(main="Plots des jours de vent et de pluie",
      outer=TRUE)

vent.table=table(df$vent)
labels2 <- c("Nord", "Ouest", "Sud", "Est")
pie(vent.table, labels2, col=rainbow(4),main="Ratio des jours de vent", radius = 0.9)

par(mfrow=c(1,1),oma=c(0,0,3,0))
plot(df$pollution, main="Pollution en fonction du point dans le dataset")

library(ggplot2)
library(reshape2)
df2=df[c("pollution","T9", "T12", "T15")]
df2 = melt(df2, id.vars = "pollution", measure.vars = c("T9", "T12", "T15"))

ggplot(df2,aes(x=variable,y=value,group=pollution,colour=pollution)) + 
  geom_line() + ggtitle("Temp?ratures mesur?es aux diff?rentes heures de la journ?e pour chaque point") + xlab('Horaire de la mesure') + ylab('Temperature')

df3=df[c("pollution","Ne9","Ne12","Ne15")]
df3=melt(df3, id.vars = "pollution", measure.vars = c("Ne9","Ne12","Ne15"))
par(mfrow=c(1,1),oma=c(0,0,3,0))
ggplot(df3,aes(x=variable,y=value,group=pollution,colour=pollution)) + 
  geom_line() + ggtitle("N?bulosit? mesur?e aux diff?rentes heures de la journ?e pour chaque point") + xlab('Horaire de la mesure') + ylab('N?bulosit?')

#-----------? voir si on garde ce plot------------
plot(df$T9, type = "l", col = 1)  # Plot with Base R
lines(df$T12, type = "l", col = 2)
lines(df$T15, type = "l", col = 3)
title(main="Températures aux différentes heures de la jounée pour chaque point",
      outer=FALSE)



############################
#### 2. Faire une ou des représentation(s) graphique(s) illustrant les corrélations entre les variables et
#commenter.

pairs(df, main = "Nuages de points - corr?lation entre les variables") #trace tous les graphes à 2 variables possibles
# les variables de température semblent assez corrélées entre elles
# pareil pour les variables Vx

library(corrplot)
par(mfrow=c(1,1),oma=c(0,0,1,0))
corrplot(cor(df), main = "Correlation entre les variables", outer = TRUE)



############################
#### 3. Faites une figure représentant la pollution en fonction de la variable Vx12 et en fonction de la
#direction du vent. Que remarquez-vous ?

plot(df$vent,df$pollution, main="Pollution en fonction du vent")

# la pollution semble élevée quand le vent vient de l'est
# mais il y a moins de réalisations dans le dataset de vent venant de l'est, donc c'est moins significatif
# la pollution semble basse quand le vent vient du nord, et moyenne quand le vent vient de l'ouest ou du sud

############################
# 4. Décrire le modèle de régression linéaire pour expliquer la corrélation entre la variable pollution et
#toutes les autres variables. Définir précisément le modèle et ses hypothèses. (c'est dans le cours) Quelle est la dimension
#du paramètre à estimer ? (pas besoin de code pour cette question)

# dimension -> 12+1=13 car on compte l'intercept


############################
#### 5. Faire tourner ce modèle lm1 sur R. Commenter

lm1=lm(pollution~.,data=df)
summary(lm1)

# R^2 ajusté a une valeur de 0.74, ce qui est convenable sans être très élevé
# vpollution semble être une variable très significative, ce qui est cohérent avec ce que l'on peut penser intuitivement
# les p-values de nombreuses variables de notre modèle sont très largement supérieures à 0.05, ce qui laisse à croire que 
# nous supprimerons de nombreuses variables de notre modèle dans la suite de notre étude
# le vent a une p-value très élevée, alors que selon notre graphique de la question 3, cette variable aurait pu être significative


############################ AA
#### 6. Que vaut l’estimation du paramètre associé à la variable Vx12 ? Que vaut son écart-type ? Poser
#précisément un test pour savoir si cette variable a une influence dans le modèle ou pas. Que vaut
#la statistique de test observée ? Quelle décision prenez-vous et à quel risque ?

param_estim = lm1$coefficients[9]
param_estim # 1.21385

# ecart-type = 1.24266 (affiché dans le summary)

confint(lm1)
lm1$coefficients 

# IC pour Vx12 -1.2665077  3.6942075, beta est est dans l'IC, on conclut donc que cette variable a une influence
# dans le modèle au risque 5%
# Que vaut la statistique de test observée ? -> à voir


############################ AA
#### 7. La variable Vx15 est-elle significative au risque 5% ? Expliquer pourquoi ce résultat peut paraître
#étonnant.

param_estim = lm1$coefficients[10]
param_estim # 0.1501186 

confint(lm1)

# IC pour Vx15 -2.0102089  2.3104461, beta est est dans l'IC, on conclut donc que cette variable est significative
# dans le modèle au risque 5%
# ce résultat peut paraître étonnant car Vx15 est fortement corrélée à Vx12


############################ AA
#### 8. Construire le test de significativité globale de la régression . Quelle est votre décision ici ?

summary(lm1)
# p-value: < 2.2e-16, la régression est significative
# sinon on peut faire avec fisher à la question 1.7 


############################
#### 9. Ajuster un sous-modèle lmfin pas à pas en prenant en compte uniquement certaines variables de
#votre choix. Justifier succinctement la stratégie adoptée pour la sélection des variables.

# Méthode descendante (backward) : on part du modèle complet, on
#enlève successivement la variable la moins influente selon le critère
#choisi (ici on va prendre la p-value), on s’arrête dès que le fait de retirer une nouvelle variable
#diminue les performances du modèle.

summary(lm1) # Adjusted R-squared:  0.7395 
lm2=lm(pollution~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+vpollution+pluie,data=df) # on enlève vent
summary(lm2) # Adjusted R-squared:  0.7433 
lm3=lm(pollution~T9+T12+T15+Ne9+Ne15+Vx9+Vx12+Vx15+vpollution+pluie,data=df) # on enlève Ne12
summary(lm3) # Adjusted R-squared:  0.7469 
lm4=lm(pollution~T9+T12+T15+Ne9+Ne15+Vx9+Vx12+vpollution+pluie,data=df) # on enlève Vx15
summary(lm4) # Adjusted R-squared:  0.7504 
lm5=lm(pollution~T9+T12+T15+Ne9+Ne15+Vx12+vpollution+pluie,data=df) # on enlève Vx9
summary(lm5) # Adjusted R-squared:  0.7537 
lm6=lm(pollution~T9+T12+Ne9+Ne15+Vx12+vpollution+pluie,data=df) # on enlève T15
summary(lm6) # Adjusted R-squared:  0.7567 
lm7=lm(pollution~T9+T12+Ne9+Vx12+vpollution+pluie,data=df) # on enlève Ne15
summary(lm7) # Adjusted R-squared:  0.7573 
lm8=lm(pollution~T9+T12+Ne9+Vx12+vpollution,data=df) # on enlève pluie
summary(lm8) # Adjusted R-squared:  0.7579 
lm9=lm(pollution~T12+Ne9+Vx12+vpollution,data=df) # on enlève T9
summary(lm9) # Adjusted R-squared:  0.7541 
# en enlevant T9, on fait diminuer la métrique R^2 ajusté. On décide donc de garder le modèle lm8

# à la fin on conserve les variables
# T9, T12, Ne9, Vx12, vpollution

lmfin = lm8

############################
# 10. Quelle indication supplémentaire apporte le coefficient de détermination ? Commenter sa valeur
#dans le cas des modèles étudiés précédemment.

# on a justement utilisé le coefficient de détermination dans la quetion précédente, peut-être qu'on devrait utiliser une 
# autre métrique ?
# par ex on aurait ou utiiser les résidus (dcp il faut refaire la question précédente)


############################
#### 11. Effectuer une procédure de sélection de modèle avec la commande stepAIC du package MASS. Rap-
#peler son principe. Quel modèle choisit-elle ? Utiliser la même méthode pour sélectionner un modèle
#par le critère BIC. Commenter. Choisir le modèle que vous allez finalement conserver en justifiant
#votre choix.

Y=as.matrix(df$pollution,ncol=1) # vraies observations de lpsa
n=length(Y) # 80
n


library(MASS)
stepAIC(lm1,direction='both',trace=1) # backward ou forward ou both, trace=1 on affiche toutes les étapes
# AIC fait conerver les variables T9, T12, Ne9, Vx12, vpollution
# c'est aussi ce que l'on a trouvé précédement !
stepAIC(lm1,k=log(n),trace=0)
# BIC pénalise plus le b de variables
# BIC fait conerver les variables T12, Ne9, Vx12, vpollution
# BIC supprime T9 par rapport à AIC. Cela est cohérent car BIC pénalise davantage le grand nombre de variables par rapport à AIC
# aussi il semble logique que ce soit T9 qui soit supprimée, car elle est en quelque sorte redondante avec T12, et T12 semblait davantage significative 
# depuis le début

# globalement, les deux modèles ont des bonnes performances et leurs Residual standard error et Adjusted R-squared sont relativement proches
# nous décidons de garder T9 dans le modèle, car en fonction des saisons, la différence entre T9 et T12 peut plus ou moins varier
# T9 permet donc un ajout d'information au modèle, même s'il n'est pas aussi significatif que d'autres variables

############################
#### 12. Valider ou invalider ce dernier modèle.

lmf = lm8


# fenetre en 2x2,  marge haute pour titre general
par(mfrow=c(2,2), oma=c(0,0,2,0)) 

plot(lmf$fitted,df$pollution,main="ajustes/observes") # pour voir si la variation est aléatoire (bien répartie sur la droite, et de meme variance)
abline(0,1)
# ici les résidus sont bien répartis de part et d'autre de la droite, la distribution semble normale


plot(lmf$fitted,lmf$residuals,main="differents residus",ylim=c(-3,3))
abline(h=0,lty=2)

library(MASS)
plot(lmf$fitted,stdres(lmf), col=2,pch=2 ,ylim=c(-3,3),
     main="résidus standardisés et studentisés")
points(lmf$fitted,studres(lmf),col=3,pch=3) 
abline(h=2); abline(h=-2); abline(h=0,lty=2)
legend("bottomright",c( "stdres", "studres"),col=2:3, pch=2:3,cex=0.5)

# 
qqnorm(studres(lmf),main="quantile-quantile plot")
qqline(studres(lmf))

# 
title(main="Validation en régression linéaire multiple", outer=TRUE)

# 2.par(mfrow=c(2,2))
plot(lmf) # residus en fonction y chapeau, quantiles, distance de cook et points leviers (ici pas de points avec des distances trop grandes)

# il va falloir commenter ces graphiques, il y aura sûrement plus de pistes dans le corrigé du tp2
