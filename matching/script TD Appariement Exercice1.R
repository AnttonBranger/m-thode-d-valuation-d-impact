
# Appariement sur le score de propension
#########################################

library(ggplot2)
library(ggthemes)
library(dplyr)
library(stargazer)
library(MatchIt)
library(haven) # pour read_dta
library(tidyverse)

#############################
# Exercice 1. Evaluation du Family Health Program au Brésil
#############################

# 1. 
load("FHP_Bresil.RData")
str(MEIfinal)

stargazer(MEIfinal, 
          type = "text", 
          title = "Statistiques descriptives",
          digits = 2,
          style = "aer")

trait <- MEIfinal |> filter(Year==2007) |> group_split(FHP)
for(i in seq_along(trait)) {
  cat("Annee 2007")
  stargazer(
    data.frame(trait[[i]]),
    type = "text", 
    title = paste("Statistiques descriptives pour le groupe", unique(trait[[i]]$FHP)),
    summary = TRUE, 
    digits = 2
  )
}

# selection des observations de 2007
boxplot(hosp_inf_5y~FHP,data=MEIfinal[MEIfinal$Year==2007,])
t.test(hosp_inf_5y~FHP,data=MEIfinal[MEIfinal$Year==2007,])

# ou t.test(MEIfinal[MEIfinal$Year==2007 & MEIfinal$FHP==0,]$hosp_inf_5y,MEIfinal[MEIfinal$Year==2007 & MEIfinal$FHP==1,]$hosp_inf_5y)

# creation du sous échantillon
MEI07 <- MEIfinal[MEIfinal$Year==2007,]
MEI07 <- MEI07 |> mutate(FHP=factor(FHP))
ggplot(MEI07, aes(x=FHP, y= hosp_inf_5y, color = FHP)) + geom_boxplot()

ggplot(MEI07, aes(x=FHP, y= Beds, color = FHP)) + geom_boxplot()
ggplot(MEI07, aes(x=FHP, y= Income, color = FHP)) + geom_boxplot()
ggplot(MEI07, aes(x=FHP, y= Lifeexp, color = FHP)) + geom_boxplot()
ggplot(MEI07, aes(x=FHP, y= Poverty, color = FHP)) + geom_boxplot()

# les groupes "ne sont pas statistiquement comparables

# 2. calcul du score de propension à l'aide d'un modèle de régression logisitique
ps <- glm(FHP ~ Income + Poverty + Illiteracy + MHDI + Lifeexp + Beds, family = binomial(), MEI07)
summary(ps) 
# exp(coef(ps)) # odd ratio
# Régression logistique si le nb de facteurs est grand, il est difficile d'apparier et si le nb est trop faible,il peut y avoir 1 biais.

step(ps,direction="both")

# selection de variables 
ps <- glm(FHP ~ Income + Poverty + Illiteracy + MHDI + Beds, family = binomial(), MEI07)
summary(ps)
# avec ou sans beds, ça ne change pas grand chose
# Attacher les scores de propension à la base de données
MEI07$psvalue <- predict(ps, type = "response")
# évaluation du score de propension
# matrice de confusion 
plot(MEI07$psvalue~MEI07$FHP,bty="l",xlab="MCC School",
     ylab="Probabilité estimée",cex.lab=1.25,
     cex.axis=1.25,cex.main="1.25",pch=16,
     main="Probabilité de d'être dans le programme FHP")
pred.class = ifelse(MEI07$psvalue>=0.5,"Traitement","Contrôle")
confusion = table(MEI07$FHP,pred.class,dnn=list("Observé","Predit"))
confusion

# 3. Histogramme
ggplot(MEI07)+
  geom_histogram(aes(psvalue, group=factor(FHP),col=factor(FHP))) # empilement des barres

ggplot(MEI07, aes(psvalue)) + 
  geom_histogram(data = subset(MEI07,FHP == 0), fill = "green", alpha = 0.2) +
  geom_histogram(data = subset(MEI07,FHP == 1), fill = "red", alpha = 0.2) 


ggplot()+
  geom_density(aes(psvalue), data=MEI07[MEI07$FHP==0,], col="green")+
  geom_density(aes(psvalue), data=MEI07[MEI07$FHP==1,], col="red")

summary(MEI07[MEI07$FHP==0,]$psvalue)
summary(MEI07[MEI07$FHP==1,]$psvalue)

# support commun : s'assurer que pour chaque individu du groupe traité, on peut trouver, au moins, un participant du groupe témoin ayant les mêmes caractéristiques (selon le score de propension).
# zone de support commun assez étendue ici, signe d’une bonne comparabilité des groupes traitement et contrôle.
# Le score de propension n'est pas vraiment équilibré (ie: les scores de propension des individus du groupe de traitement et contrôle comparables). 
# Il y a peu d'individus du groupe contrôle avec une valeur de score de propension élevée, autrement dit avec une forte proba d'être traité. 
# les variables retenues ne sont pas suffisamment exogènes ou non suffisantes en nombre. 

# 4. appariement
ps_match <- matchit(FHP ~ Income + Poverty + Illiteracy + MHDI + Beds, data = MEI07)
# algo MatchIt basé sur de l'aléatoire
ps_match
summary(ps_match)

# Numéro des lignes des individus appariés
IndApp<-subset(as.data.frame(ps_match$subclass), !is.na(as.data.frame(ps_match$subclass)))
# Affiche les six premiers individus appariés
head(IndApp)
# Tous les individus appariés
match.final <- match.data(ps_match)

# QQ plot touche entrée pour changer de variables
plot(ps_match) # les points en dehors de l'intervalle posent pb. ici seule la variable Beds est correcte. 

# ici le jdd apparié s'écarte encore plus

# histogramme des score des propension 1ere colonne données brutes, 2eme colonne données appariées
# on veut que les histogrammes soient identiques traités/controle pour la colonne apparié, ce qui n'est pas du tout le cas
plot(ps_match,type="hist") 

# support commun peut être visualisé avec l'argument type = jitter
plot(ps_match,type="jitter")
# on cherche une répartition similaire entre traités et controle du jdd apparié

# graphe diagnostic
cobalt::love.plot(ps_match, drop.distance = TRUE)
# extraire les données 
match.data <- match.data(ps_match) 

# pour récupérer les correspondances ou avec subclass de match.data 
matches<-data.frame(ps_match$match.matrix) 

# 5. Effet traitement
t.test(match.data[match.data$FHP==1,]$hosp_inf_5y,match.data[match.data$FHP==0,]$hosp_inf_5y, paired=TRUE)
# ou 
# pairwise.t.test(match.data$hosp_inf_5y,match.data$FHP,paired=TRUE, p.adjust.method ="bonferroni")

# comparaison de moyenne sur données appariées
# ici j'utilise un t.test avec l'option paired 
# les résultats montrent un effet non significatif mais la méthode n'est pas "correctement" utilisée ici
# les résultats de l'appariement ne sont pas bons 
