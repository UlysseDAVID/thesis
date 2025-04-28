#---------------------------------INSTALLATION DES PACKAGE-----------------
install.packages("FactoMineR")
install.packages("Factoshiny")
install.packages("shiny")
install.packages("FactoInvestigate")
install.packages("ggplot2")
install.packages("cluster")
install.packages("mlogit")    # For multinomial logit
install.packages("nnet")
install.packages("GGally")
install.packages('Hmisc')
install.packages("factoextra")
install.packages("guideR")
install.packages("performance")
install.packages("tidyverse")
install.packages("labelled")
install.packages("leaps")
install.packages("modelsummary")
install.packages("knitr", dependencies = TRUE)
install.packages("formattable")
install.packages("gtsummary")
install.packages("explor")
install.packages("factoextra")

#--------------------------OUVERTURE DES LIBRARY------------------------------
library(mlogit)
library(shiny)
library(FactoInvestigate)
library(ggplot2)
library(FactoMineR)
library(Factoshiny)
library(cluster)
library(tidyr)
library(dplyr)
library(Hmisc)
library(factoextra)
library(guideR)
library(performance)
library(modelsummary)
library(knitr)
library(explor)
library(factoextra)

#----------------------ETAPE 1-----------------------------------
#choix de la base de données 
data_raw <- MCA_vhors_endettement #BDD des 276 méthas avec typologie actionnariat sans endettement
  #OU
data_raw <- MCA_vavec_endettement #BDD des 216 méthas avec endettement et typologie actionnaire
#transformation taux d'endettement en valeur absolue
data_raw$`dernier ratio d'endettement` <- abs(data_raw$`dernier ratio d'endettement`)

# définition des données contenant les ratios financiers triée sans les métha avec comme seul exercice celui de leur mise en service
data_fi <- GRDF_ratios_fi_normalisés_trié 

# corrélation entre variables et sélection des variables ?


#------------------------ETAPE 2--------------------------------------------------------------
#classification, méthode 1 ascendante hiérarchique 
#suppression centrale montaigu
data_raw <- data_raw[-94,] #BDD hors endettement
data_raw <- data_raw[-c(194,28,209,185,205),] #BDD avec endettement supp 	Méthanisère car taux endettement extrêmement bas et CELLES SUR BELLE BIOGAZ


#sélection des données pour les classes la base de données hors taux d'endettement
tri_CAH <- data.frame(data_raw[,c(1,7:13,5,15,3)], row.names = 1, check.names = FALSE)
tri_CAH <- data.frame(data_raw[,c(1,8:14,6,3,16,4)], row.names = 1, check.names = FALSE) #BDD avec endettement

#centrer réduire la variable Ksocial/nb actionnaires et taux d'endettement
tri_CAH[,8] <- scale(tri_CAH[,8]) #ksocial

#valeur absolue puis normalisation logarithmique du taux d'endettement 
tri_CAH[,9] <- abs(tri_CAH[,9])
tri_CAH[,9] <- log(tri_CAH[,9])
#normalisation taux endettement logarithmique 
tri_CAH[,9] <- scale(tri_CAH[,9]) #BDD taux endettement

#construction de la classification
classif <- agnes(scale(tri_CAH[,1:8]), method = "ward") #pour endettement 1:9 et 1:8 sans
classif <- agnes(scale(tri_PCA[,1:16]), method = "ward") #pour endettement 1:9 et 1:8 sans
#classification avec fonction hclust
#classif <- hclust(dist(scale(tri_CAH[,1,8])), method = "ward.D2)
#affichage de la classfication
plot(classif,xlab="Individu",which.plot=2,main="Dendogramme", cex = 0.2)
#affichage des hauteurs de coupes
classif2 <- as.hclust(classif)
plot(rev(classif2$height),type = "h", ylab = "hauteurs")
#coupe de l'arbre de hauteur k
clust <- cutree(classif, k=7) 
#description des classes
classes <- cbind.data.frame(tri_CAH,clust=as.factor(clust))
#explication des classes 
catdes(classes, num.var = 10) 
#affichage des effectifs par classe
library(gtsummary)
theme_gtsummary_language("fr", decimal.mark = ",")
classes |> 
  tbl_summary(include = clust)

#représentation des variables 
library(GGally)
tri_CAH$cluster <- classes$clust
tri <- tri_CAH[,-c(9,10)] #9:10 sans dette et 10:11 avec
ggtable(
  tri_CAH, 
  columnsX = "cluster", 
  columnsY = names(tri),
  cells = "resid",
  fill = "none"
) + 
  labs(fill = "Résidus standardizés du Chi²") +
  theme(legend.position = "bottom")

ggduo(
  tri_CAH, 
  columnsX = "cluster", 
  columnsY = names(tri),
  columnLabelsY = c("agriculteurs","personnes","énergéticien","financier","industriel","autres", "collectivité","Ksocial"),
) + 
  labs(fill = "Résidus standardizés du Chi²") +
  theme(legend.position = "bottom")


tri |> 
  tbl_summary(
    include =  names(tri[,-9]), #9 hors endettement et 10 avec
    by = cluster,
    type = c(`Part actionnaire collectivité`,`Part actionnaire industriel`,`montant capital social`) ~ "continuous",
    digits = all_continuous() ~ 2,
    statistic = all_continuous() ~ "{mean} ({sd})"
  )

tri |> 
  tbl_summary(
    include =   names(tri[,-9]), #9 hors endettement et 10 avec
    type = c(`Part actionnaire collectivité`,`Part actionnaire industriel`,`montant capital social`) ~ "continuous",
    digits = all_continuous() ~ 2,
    by = cluster
  ) 




#classification avec PCA 
#PCA sans endettement 
#sélection des données pour le PCA depuis la base de données hors taux d'endettement
tri_PCA <- data.frame(data_fi[,c(2,7,14:21,26:34)],ROW.NAMES =1, check.names = FALSE)
tri_PCA <- tri_PCA[-c(77,61,146),]
tri_PCA$part_agri <- tri_PCA$`Part actionnaire agricole`+tri_PCA$`Part actionnaire personnes`+tri_PCA$`Part actionnaire sociétés autres`
tri_PCA <- tri_PCA[,-c(4,5,8)]
tri_PCA <- tri_PCA[tri_PCA$EBE_normalise != 0,]
tri_PCA[]<- lapply(tri_PCA, function(x) as.numeric(as.character(x)))
tri_PCA <- scale(tri_PCA)
#centrer réduire la variable Ksocial/nb actionnaires et taux d'endettement
tri_PCA[,7] <- scale(tri_PCA[,7]) #ksocial/actionnaire
tri_PCA[,9] <- scale(tri_PCA[,9]) #BDD taux endettement

#affichage des variables pour identifier si elles sont qualitatives ou non
summary(tri_PCA)
#réalisation de la PCA, variable K social/nb actionnaires est supplémentaire, tout comme le type de métha et la part de déchets agricoles
res.pca <- PCA(tri_PCA,scale.unit = TRUE) #pour BDD hors endettement
#affichage des premières dimensions et leur inertie
barplot(res.pca$eig[,2],names=paste("Dim",1:nrow(res.pca$eig)))
#résumé de l'inertie des 2 premières dimensions et la contribution des principales variables
summary(res.pca, ncp=2, nbelements = 3)
# graphique des individus sur axes 1 et 2 avec habillage sur la part de déchets agricoles
plot(res.pca, choix = "ind", habillage = 10, cex = 0.5, select="cos2 0.6", title = "Graphe des individus PCA")
# graphique des individus sur axes 1 et 2 avec habillage sur la part de déchets agricoles
plot(res.pca, choix = "ind", habillage = 10, axes = 3:4, cex = 0.5, select="cos2 0.6", title = "Graphe des individus PCA axes 3 et 4")
# graphique des variables sur axes 3 et 4 avec habillage sur la part de déchets agricoles
plot(res.pca, choix = "var", habillage = 10, axes = 3:4, cex = 0.5, title = "Graphe des variables PCA axes 3 et 4")
#Afffichage des contributions des variables pour chaque dimension
dimdesc(res.pca, proba = 0.2)
#visualisation avec le package Factoshiny
PCAshiny(res.pca)
#affichage des ellipses de confiance pour les variables qualitatives supplémentaires
plotellipses(res.pca)
#classification à partir de l'ACP, min nombre de classes minimum
res.hcpc <- HCPC(res.pca, consol=FALSE,nb.clust	= 3, min = 4, cex = 0.2) #si TRUE alors étape de consolidation par k-means après découpage de l'arbre
#visualisation du dendogramme 
res.hcpc |> 
  factoextra::fviz_dend(show_labels = FALSE)
#Inertie des classes
res.hcpc |> 
  guideR::plot_inertia_from_tree()
#description des classes 
res.hcpc$desc.var
res.hcpc$desc.axes
res.hcpc$desc.ind
#affichage des paramètres des classes 
res.hcpc$desc.var |> plot()
#récupération des classes 
classes <- res.hcpc$data.clust


#classification à partir de l'AFM
#Analyse factorielle multiple
#sélection des données pour le PCA depuis la base de données hors taux d'endettement
tri_MFA <- data.frame(data_raw[,c(1,8:13,5,15,3:4)], row.names = 1, check.names = FALSE)
#centrer réduire toutes les variables
for(i in 1:8){
tri_MFA[,i] <- scale(tri_MFA[,i])
}
#centrer réduire toutes la variable Ksocial/nb actionnaires et ratio endettement
tri_MFA[,7] <- scale(tri_MFA[,7]) #Ksocial/actionnaire
tri_MFA[,9] <- scale(tri_MFA[,9]) #taux endettement
#affichage des variables pour identifier si elles sont qualitatives ou non
summary(tri_MFA)
#réalisation de la MFA, les 7 premières variables ne sont pas centrées, 2 groupes sont constitués (un sur l'actionnariat et l'autre sur son montant)
res.mfa <- MFA(tri_MFA, group=c(6,1,3), type=c("c","s","n"), name.group = c("Typologie actionnariat","part K social par actionnaire","groupe témoin"), num.group.sup = 3) #hors taux endettement
res.mfa <- MFA(tri_MFA, group=c(7,2,1), type=c("c","s","c"), name.group = c("Typologie actionnariat","part K social par actionnaire","groupe témoin"), num.group.sup = 3) #avec taux endettement
#affichage des dimensions
barplot(res.mfa$eig[,2], names=paste("Dim",1:nrow(res.mfa$eig)))
#analyse des résultats pour les 2 premières dimensions
summary(res.mfa, nbelement=10,nb.dec=2,ncp=2)
#lien entre les variables par groupe
round(res.mfa$group$RV, 2)
#Afffichage des contributions des variables pour chaque dimension
dimdesc(res.mfa, proba = 0.2)
#visualisation avec le package Factoshiny
MFAshiny(res.mfa)
#classification à partir de la FMA
res.hcpc2 <- HCPC(res.mfa, consol=TRUE,nb.clust	= 4, min = 4) #si TRUE alors étape de consolidation par k-means après découpage de l'arbre
#visualisation du dendogramme 
res.hcpc2 |> 
  factoextra::fviz_dend(show_labels = FALSE)
#Inertie des classes
res.hcpc2 |> 
  guideR::plot_inertia_from_tree()
#description des classes 
res.hcpc2$desc.var
res.hcpc2$desc.axes
res.hcpc2$desc.ind
classes <- res.hcpc2$data.clust
#affichage des paramètres des classes 
res.hcpc2$desc.var |> plot()
#récupération des classes 
classes <- res.hcpc2$data.clust

#lien endettement et actionnariat métha 
#régression classes fonction endettement
library(tidyverse)
library(labelled)
#si la régression est sur la même BDD que celle pour les classes
classes$'nom installation' <- data_raw$`Nom de l'installation`
data_regr$cluster <- classes$clust
#récupération des classes si la BDD régression n'est pas la même que les classes
data_regr <- MCA_vavec_endettement
data_regr$`dernier ratio d'endettement` <- abs(data_regr$`dernier ratio d'endettement`)
data_regr$cluster <- ""
classes$'nom installation' <-data_raw$`Nom de l'installation`
for (i in data_raw$`Nom de l'installation`){
  data_regr$cluster[data_regr$`Nom de l'installation` == i] <- classes$clust[classes$'nom installation' == i]
}
#enlever métha montaigu et metha avec endettement nul 
data_regr <- data_regr[-c(28,185,194,205,209),]


#proportion par classes
summary(data_regr)
#conversion cluster en type factor pour la régression
data_regr$cluster <- as.factor(data_regr$cluster)
#transformation du taux d'endettement avec logarithme 
data_regr$`dernier ratio d'endettementlog` <- log1p(data_regr$`dernier ratio d'endettement`)
data_regr$`dernier ratio d'endettementlog2` <- abs(data_regr$`dernier ratio d'endettement`)
data_regr$`dernier ratio d'endettementlog2` <- log(data_regr$`dernier ratio d'endettementlog2`)

#régression linéaire avec contraste de type somme, sans référence
contrasts(data_regr$cluster) <- contr.sum
mod1_sum <- lm(`dernier ratio d'endettementlog` ~ cluster, data = data_regr)
mod1_sum
#calcul de la grande moyenne, à savoir la moyenne des moyennes de chaque sous-groupe.
moy_groupe <-
  data_regr |> 
  dplyr::group_by(cluster) |> 
  dplyr::summarise(moyenne_endettement = mean(`dernier ratio d'endettementlog`, na.rm = TRUE))
moy_groupe
mean(moy_groupe$moyenne_endettement)
contrasts(data_regr$cluster)

#affichages tableau et IC
mod1_sum |>
  tbl_regression(
    intercept = TRUE, 
    add_estimate_to_reference_rows = TRUE
  ) |> 
  bold_labels()
ggstats::ggcoef_model(mod1_sum)
ggstats::ggcoef_table(mod1_sum)
#affichage selon les économistes
mod1_sum |> modelsummary::modelsummary(stars = TRUE)
mod1_sum |> modelsummary::modelplot()
#analyse de la significativité des variables
car::Anova(mod1_sum)


#lien endettement et année mise en service 
#conversion annee mise en service en type factor pour la régression
data_regr$`Annee mise en service` <- as.factor(data_regr$`Annee mise en service`)
#régression linéaire avec contraste de type somme, sans référence
contrasts(data_regr$`Annee mise en service`) <- contr.sum
mod1_sum <- lm(`dernier ratio d'endettementlog` ~ `Annee mise en service`, data = data_regr)
mod1_sum
#calcul de la grande moyenne, à savoir la moyenne des moyennes de chaque sous-groupe.
moy_groupe <-
  data_regr |> 
  dplyr::group_by(`Annee mise en service`) |> 
  dplyr::summarise(moyenne_endettement = mean(`dernier ratio d'endettementlog`, na.rm = TRUE))
moy_groupe
mean(moy_groupe$moyenne_endettement)
contrasts(data_regr$`Annee mise en service`)
#affichage tableau et IC
mod1_sum |>
  tbl_regression(
    intercept = TRUE, 
    add_estimate_to_reference_rows = TRUE
  ) |> 
  bold_labels()
ggstats::ggcoef_model(mod1_sum)
ggstats::ggcoef_table(mod1_sum)

#-----------------------------ETAPE 3----------------------------------------------------------------
#analyse des ratios financiers 
#sélection des données et ajout des classes à la base de données
data_raw$cluster <- classes$clust #à compléter en fonction des résultats précédents utilisés ci-dessus
#ajout des classes à la BDD contenant l'ensemble des informations financières 
data_analysis <- merge(data_fi, data_raw[, c("Nom de l'installation", "cluster")], by = "Nom de l'installation", all.x = TRUE)
#suppression montaigu
data_analysis <- data_analysis[-c(164,77),] #BDD_endettement suppression Méthanisère car taux endettement extrêmement bas et CELLES SUR BELLE BIOGAZ et montaigu
data_analysis <- data_analysis[-c(70,77,164),] #BDD_endettement suppression Méthanisère car taux endettement extrêmement bas et CELLES SUR BELLE BIOGAZ et montaigu
#valeur absolue taux endettement 
data_analysis$`dernier ratio d'endettementabs` <- abs(data_analysis$`dernier ratio d'endettement`)
#normalisation logarithmique taux endettement 
data_analysis$`dernier ratio d'endettementlog1`<- log1p(data_analysis$`dernier ratio d'endettement`)
data_analysis$`dernier ratio d'endettementlogabs` <- log(data_analysis$`dernier ratio d'endettementabs`)

#régression des variables financières (EBE et RNE normalisés) à partir des variables utilisées pour les classes
#régression RNE
data_regr <- data_analysis[data_analysis$RNE_normalise != 0,] #RNE
data_regr <- data_analysis[data_analysis$EBE_normalise != 0,] #EBE
data_regr <- data_analysis[data_analysis$`CAF/CA` != 0,]
#sélection des variables 
data_regr <- data_regr[,c(1,4,11,14:24,39,26:36)]

#enlever NA
data_regr <- na.omit(data_regr)
data_regr[data_regr == -Inf] <- 0

library(nnet)
summary(data_regr)
#conversion numérique
data_regr[,2] <- as.factor(data_regr[,2])
data_regr[,14] <- as.factor(data_regr[,14])
data_regr[,17] <- as.numeric(data_regr[,17])
data_regr[,18] <- as.numeric(data_regr[,18])
data_regr[,19] <- as.numeric(data_regr[,19])
data_regr[,20] <- as.numeric(data_regr[,20])
data_regr[,21] <- as.numeric(data_regr[,21])
data_regr[,22] <- as.numeric(data_regr[,22])
data_regr[,23] <- as.numeric(data_regr[,23])
data_regr[,24] <- as.numeric(data_regr[,24])


#centrage et réduction de certaines variables
data_regr[,c(4,5,13,15:25)] <- scale(data_regr[,c(4,5,13,15:25)])
#création de nouvelles colonnes pour les actionnaires en fonction de leur présence 
data_regr$"présence énergéticien" <- ""
#énergéticiens
for (i in data_regr$`Nom de l'installation`){
  if (data_regr$`Part actionnaire énergéticien`[data_regr$`Nom de l'installation`==i] == 0 ){
    data_regr$`présence énergéticien`[data_regr$`Nom de l'installation`==i] <- 0
  } else {
    data_regr$`présence énergéticien`[data_regr$`Nom de l'installation`==i] <- 1
  }
}
#data_regr$`présence énergéticien` <- as.character(data_regr$`présence énergéticien`)
#agriculteurs 
data_regr$"présence agriculteurs" <- ""
for (i in data_regr$`Nom de l'installation`){
  if (data_regr$`Part actionnaire agricole`[data_regr$`Nom de l'installation`==i] == 0 ){
    data_regr$`présence agriculteurs`[data_regr$`Nom de l'installation`==i] <- 0
  } else {
    data_regr$`présence agriculteurs`[data_regr$`Nom de l'installation`==i] <- 1
  }
}
#data_regr$`présence agriculteurs` <- as.character(data_regr$`présence agriculteurs`)

#personnes 
data_regr$"présence personnes" <- ""
for (i in data_regr$`Nom de l'installation`){
  if (data_regr$`Part actionnaire personnes`[data_regr$`Nom de l'installation`==i] == 0 ){
    data_regr$`présence personnes`[data_regr$`Nom de l'installation`==i] <- 0
  } else {
    data_regr$`présence personnes`[data_regr$`Nom de l'installation`==i] <- 1
  }
}
#data_regr$`présence personnes` <- as.character(data_regr$`présence personnes`)

#sociétés financières 
data_regr$"présence financier" <- ""
for (i in data_regr$`Nom de l'installation`){
  if (data_regr$`Part actionnaire financières`[data_regr$`Nom de l'installation`==i] == 0 ){
    data_regr$`présence financier`[data_regr$`Nom de l'installation`==i] <- 0
  } else {
    data_regr$`présence financier`[data_regr$`Nom de l'installation`==i] <- 1
  }
}
#data_regr$`présence financier` <- as.character(data_regr$`présence financier`)

#sociétés industrielles 
data_regr$"présence industriel" <- ""
for (i in data_regr$`Nom de l'installation`){
  if (data_regr$`Part actionnaire industriel`[data_regr$`Nom de l'installation`==i] == 0 ){
    data_regr$`présence industriel`[data_regr$`Nom de l'installation`==i] <- 0
  } else {
    data_regr$`présence industriel`[data_regr$`Nom de l'installation`==i] <- 1
  }
}
#data_regr$`présence financier` <- as.character(data_regr$`présence financier`)

#collectivité 
data_regr$"présence collectivité" <- ""
for (i in data_regr$`Nom de l'installation`){
  if (data_regr$`Part actionnaire collectivité`[data_regr$`Nom de l'installation`==i] == 0 ){
    data_regr$`présence collectivité`[data_regr$`Nom de l'installation`==i] <- 0
  } else {
    data_regr$`présence collectivité`[data_regr$`Nom de l'installation`==i] <- 1
  }
}
#data_regr$`présence collectivité` <- as.character(data_regr$`présence collectivité`)

#sociétés autres 
data_regr$"présence autres" <- ""
for (i in data_regr$`Nom de l'installation`){
  if (data_regr$`Part actionnaire sociétés autres`[data_regr$`Nom de l'installation`==i] == 0 ){
    data_regr$`présence autres`[data_regr$`Nom de l'installation`==i] <- 0
  } else {
    data_regr$`présence autres`[data_regr$`Nom de l'installation`==i] <- 1
  }
}
#data_regr$`présence autres` <- as.character(data_regr$`présence autres`)

#methanisation agricole 
data_regr$"methanisation agricole" <- ""

for (i in data_regr$`Nom de l'installation`){
  if (data_regr$`Part actionnaire personnes`[data_regr$`Nom de l'installation`==i]+ data_regr$`Part actionnaire agricole`[data_regr$`Nom de l'installation`==i] >= 0.5 ){
    data_regr$`methanisation agricole`[data_regr$`Nom de l'installation`==i] <- 1
  } else {
    data_regr$`methanisation agricole`[data_regr$`Nom de l'installation`==i] <- 0
  }
}

#sélection de la colonne poyur la régression
column_regr <- data_regr$RNE_normalise
column_regr <- data_regr$RNE_parK
column_regr <- data_regr$EBE_normalise
column_regr <- data_regr$EBE_parK
column_regr <- data_regr$EBIT_normalise
column_regr <- data_regr$EBIT_parK
column_regr <- data_regr$evolution_cap_prod
column_regr <- data_regr$`Capacite de production (GWh/an)`
column_regr <- data_regr$`dernier ratio d'endettementlogabs`
#choix de variables 
library(leaps)
#liste de paramètres : `montant capital social` + `Capital social par actionnaire`+`présence autres`+`présence collectivité` +`présence industriel` + `présence financier` + `présence énergéticien` + `présence personnes`+`présence agriculteurs` + `Capacite de production (GWh/an)` + evolution_cap_prod  +`Part actionnaire agricole` + `Part actionnaire personnes` + `Part actionnaire énergéticien` + `Part actionnaire financières`+  `Part actionnaire industriel` + `Part actionnaire sociétés autres` + `Part actionnaire collectivité` +`dernier ratio d'endettement` + `Annee mise en service`
choix <- regsubsets(  column_regr ~ `montant capital social` + `Capacite de production (GWh/an)` + evolution_cap_prod  +`Part actionnaire agricole` + `Part actionnaire personnes` + `Part actionnaire énergéticien` + `Part actionnaire financières`+  `Part actionnaire industriel` + `Part actionnaire sociétés autres` + `Part actionnaire collectivité` + `dernier ratio d'endettement` + `Annee mise en service`,
                    data = data_regr,
                    nbest=1,
                    nvmax = 12
                    )
plot(choix, scale="bic") #chaque ligne correspond à un modèle et plus le bic est faible plus le modèle est bon, si la variable est colorée ça signifit qu'elle est significative
#plus le bic estfaible, plus la variable est significative, pareil pour AIC
#sélection des variables significatives pour la régression
data_regr$`présence autres` <- as.factor(data_regr$`présence autres`)
data_regr$`methanisation agricole` <- as.factor(data_regr$`methanisation agricole`)
data_regr$`présence collectivité` <- as.numeric(data_regr$`présence collectivité`) #non utilisé car toujours à 0
data_regr$`présence financier` <- as.factor(data_regr$`présence financier`)
data_regr$`présence personnes` <- as.factor(data_regr$`présence personnes`)
data_regr$`présence industriel` <- as.factor(data_regr$`présence industriel`)
data_regr$`présence agriculteurs` <- as.factor(data_regr$`présence agriculteurs`)
data_regr$`présence énergéticien` <- as.factor(data_regr$`présence énergéticien`)
data_regr$`Annee mise en service` <- as.factor(data_regr$`Annee mise en service`)
#`montant capital social` +
choix <- lm(column_regr ~ `nombre d'actionnaire` + `Augmentation prévue 1` + `Capacite de production (GWh/an)` +`présence autres`+`présence industriel` + `présence financier`+ `présence énergéticien` + `présence personnes`+`présence agriculteurs` + evolution_cap_prod   +`dernier ratio d'endettementlogabs` + `methanisation agricole`,
            data = data_regr)
#choix <- lm(column_regr ~ `dernier ratio d'endettement`,data = data_regr) et `montant capital social` +
AIC(choix)
choix2 <- step(choix)
anova(choix, choix2, test = "Chisq")
#Affichage comparaison des modèles
library(ggstats)
ggcoef_compare(
  list("modèle complet" = choix, "modèle réduit" = choix2),
  significance = 0.05,
  type = "faceted"
)
ggcoef_model(choix2)
#variables à enlever :
#modèle de régression retenu :    `Annee mise en service`+  `dernier ratio d'endettementlogabs` + 
model <- lm(column_regr ~ `Capacite de production (GWh/an)`  +`nombre d'actionnaire` + `Augmentation prévue 1`+ `présence autres`+`présence industriel` + `présence financier`+ `présence énergéticien` + `présence personnes`+`présence agriculteurs`  + `montant capital social` +`methanisation agricole`,
            data = data_regr)

#test de multicolinéarité
mc <- model |> performance::check_collinearity()
mc
# affichage de la colinéarité
plot(mc)
#analyse résidu 
res.m <- rstudent(model)
plot(res.m, pch=15,cex=.5,ylab="résidus",main="",ylim=c(-10,10))
abline(h=c(-2,0,2),lty=c(2,1,2))
#affichage régression
library(gtsummary)
model %>%
  tbl_regression(intercept = TRUE)
library(ggstats)
ggcoef_model(model)
#affichage selon les économistes
model |> modelsummary::modelsummary(stars = TRUE)
model |> modelsummary::modelplot()
#analyse de la significativité des variables
car::Anova(model)


#Analyse lien entre classes et variables multiples 
#définition de la colonne régression
library(nnet)
column_regr <- data_regr$cluster
contrasts(data_regr$cluster) <- contr.sum
model <- multinom(column_regr ~ `Augmentation prévue 1` + Typologie + `dernier ratio d'endettementlogabs` + `nombre d'actionnaire`, data = data_regr)
#calcul de la grande moyenne, à savoir la moyenne des moyennes de chaque sous-groupe.
moy_groupe <-
  data_regr |> 
  dplyr::group_by(cluster) |> 
  dplyr::summarise(moyenne_endettement = mean(`dernier ratio d'endettementlogabs`, na.rm = TRUE))
moy_groupe
mean(moy_groupe$moyenne_endettement)
contrasts(data_regr$cluster)
#affichages tableau et IC
model |>
  tbl_regression(
    intercept = TRUE, 
    add_estimate_to_reference_rows = TRUE
  ) |> 
  bold_labels()
ggstats::ggcoef_model(mod1_sum)
ggstats::ggcoef_table(mod1_sum)
#affichage selon les économistes
mod1_sum |> modelsummary::modelsummary(stars = TRUE)
mod1_sum |> modelsummary::modelplot()
#analyse de la significativité des variables
car::Anova(mod1_sum)
#affichage réduit
multinom_pivot_wider <- function(x) {
  # check inputs match expectatations
  if (!inherits(x, "tbl_regression") || !inherits(x$inputs$x, "multinom")) {
    stop("`x=` must be class 'tbl_regression' summary of a `nnet::multinom()` model.")
  }
  
  # create tibble of results
  df <- tibble::tibble(outcome_level = unique(x$table_body$groupname_col))
  df$tbl <- 
    purrr::map(
      df$outcome_level,
      function(lvl) {
        gtsummary::modify_table_body(
          x, 
          ~ dplyr::filter(.x, .data$groupname_col %in% lvl) |> 
            dplyr::ungroup() |> 
            dplyr::select(-dplyr::any_of("groupname_col"))
        )
      }
    )
  
  tbl_merge(df$tbl, tab_spanner = paste0("**", df$outcome_level, "**"))
}
#affichage du résultat
library(gtsummary)
theme_gtsummary_language("fr",decimal.mark = ",")
tbl <- model |> 
  tbl_regression(exponentiate = TRUE)
tbl
tbl |> multinom_pivot_wider()
# Predict probability for each class
pred_probs <- predict(model, type = "probs")


#Analyse lien entre volonté d'augmenter la capacité et variables
#définition de la colonne régression
library(nnet)
column_regr <- data_regr$`Augmentation prévue 1`
model <- glm(column_regr ~ cluster +RNE_parK + RNE_normalise + `dernier ratio d'endettementlogabs`+ `Capacite de production (GWh/an)` +`présence autres`+`présence industriel` + `présence financier`+ `présence énergéticien` + `présence personnes`+`présence agriculteurs`   + evolution_cap_prod+ `montant capital social` +`methanisation agricole`,
             family = binomial, 
             data = data_regr)

#affichage du résultat
library(gtsummary)
theme_gtsummary_language("fr",decimal.mark = ",")
tbl <- model |> 
  tbl_regression(exponentiate = TRUE)
tbl
# Predict probability for each class
pred_probs <- predict(model, type = "probs")








# PCA avec différents indicateurs fi et de productivité en intégrant les classes et dates comme valeurs supplémentaires
#sélection des données pour le PCA depuis la base de données hors taux d'endettement (préciser les numéros des colonnes à retenir pour la PCA)
PCA_analysis <- data.frame(data_analysis[,c(1,13:24,11,26:36)], row.names = 1, check.names = FALSE)
#centrer réduire certaines variables
PCA_analysis[,11] <- as.numeric(PCA_analysis[,11])
PCA_analysis[,14] <- as.numeric(PCA_analysis[,14])
PCA_analysis[,15] <- as.numeric(PCA_analysis[,15])
PCA_analysis[,16] <- as.numeric(PCA_analysis[,16])
PCA_analysis[,17] <- as.numeric(PCA_analysis[,17])
PCA_analysis[,18] <- as.numeric(PCA_analysis[,18])
PCA_analysis[,19] <- as.numeric(PCA_analysis[,19])
PCA_analysis[,20] <- as.numeric(PCA_analysis[,20])
#choix de garder ou non tous les ratios financiers 
PCA_analysis <- PCA_analysis[PCA_analysis$RNE_normalise !=0,]
PCA_analysis <- PCA_analysis[PCA_analysis$EBE_normalise !=0,]

PCA_analysis[,c(2,11,13:19)] <- scale(PCA_analysis[,c(2,11,13:19)])
#affichage des variables pour identifier si elles sont qualitatives ou non
summary(PCA_analysis)

#réalisation de la PCA avec sélection des variables supplémentaires, EBE comme quanti supplémentaire pour éviter les 0 (17:18)
res_PCA_analysis <- PCA(PCA_analysis,scale.unit = FALSE,quanti.sup = c(1:11,13,17:18), quali.sup =21 )
#Affichage avec shiny et parcours des possibilités
library(explor)
res_PCA_analysis |> explor::explor()
#affichage des premières dimensions et leur inertie
barplot(res_PCA_analysis$eig[,2],names=paste("Dim",1:nrow(res_PCA_analysis$eig)))
#résumé de l'inertie des 2 premières dimensions et la contribution des principales variables
summary(res_PCA_analysis, ncp=2, nbelements = 3)
#sélection du numéro de colonne cluster 
column_nb <- which(names(data_analysis) == "cluster")
# graphique des individus sur axes 1 et 2 avec habillage sur les clusters
plot(res_PCA_analysis, choix = "ind", habillage = column_nb, cex = 0.5, select="cos2 0.6", title = "Graphe des individus PCA")
# graphique des individus sur axes 1 et 2 avec habillage sur les clusters
plot(res_PCA_analysis, choix = "ind", habillage = column_nb, axes = 3:4, cex = 0.5, select="cos2 0.6", title = "Graphe des individus PCA axes 3 et 4")
# graphique des variables sur axes 3 et 4 avec habillage sur les clusters
plot(res_PCA_analysis, choix = "var", habillage = column_nb, axes = 3:4, cex = 0.5, title = "Graphe des variables PCA axes 3 et 4")
#Afffichage des contributions des variables pour chaque dimension
dimdesc(res_PCA_analysis, proba = 0.2)
#visualisation avec le package Factoshiny
PCAshiny(res_PCA_analysis)
#affichage des ellipses de confiance pour les variables qualitatives supplémentaires
plotellipses(res_PCA_analysis)


# FMA avec différents indicateurs fi et de productivité en intégrant les classes et dates comme valeurs supplémentaires

#sélection des données pour le MFA depuis la base de données hors taux d'endettement
MFA_analysis <- data.frame(data_analysis[,c(1,4,16:22,14:15,39,27:32,35,33,34,26,24,11,36)], row.names = 1, check.names = FALSE)
#choix de garder ou non tous les ratios financiers 
MFA_analysis <- MFA_analysis[MFA_analysis$RNE_normalise !=0,]
MFA_analysis <- MFA_analysis[MFA_analysis$EBE_normalise !=0,]
MFA_analysis <- MFA_analysis[MFA_analysis$`CAF/CA` !=0,]
#enlever NA
MFA_analysis <- na.omit(MFA_analysis)
MFA_analysis[MFA_analysis == -Inf] <- 0


#centrer réduire certaines variables
MFA_analysis[,9] <- as.numeric(MFA_analysis[,9])
MFA_analysis[,10] <- as.numeric(MFA_analysis[,10])
MFA_analysis[,11] <- as.numeric(MFA_analysis[,11])
MFA_analysis[,12] <- as.numeric(MFA_analysis[,12])
MFA_analysis[,13] <- as.numeric(MFA_analysis[,13])
MFA_analysis[,14] <- as.numeric(MFA_analysis[,14])
MFA_analysis[,15] <- as.numeric(MFA_analysis[,15])
MFA_analysis[,16] <- as.numeric(MFA_analysis[,16])
MFA_analysis[,17] <- as.numeric(MFA_analysis[,17])
MFA_analysis[,18] <- as.numeric(MFA_analysis[,18])
MFA_analysis[,19] <- as.numeric(MFA_analysis[,19])
MFA_analysis[,20] <- as.numeric(MFA_analysis[,20])
MFA_analysis[,21] <- as.numeric(MFA_analysis[,21])
MFA_analysis[,23] <- as.factor(MFA_analysis[,23])
MFA_analysis[,22] <- as.factor(MFA_analysis[,22])
MFA_analysis[,1] <- as.factor(MFA_analysis[,1])
MFA_analysis[,c(9:21)] <- scale(MFA_analysis[,c(9:21)])
MFA_analysis <- MFA_analysis[,-c(8,14:19)]
#suppression methabiovalor et methaenergie trop grosses contributions aux axes 
MFA_analysis <- MFA_analysis[!rownames(MFA_analysis) %in% c("PANAIS ENERGIE", "METHABIOVALOR", "Biogasconha", "AGRIOPALE SERVICES", "AGRIGAZ"), ]

#affichage des variables pour identifier si elles sont qualitatives ou non
summary(MFA_analysis)

#vérification que certaines variables n'aient pas une modalité rare 
for (i in 1:ncol(MFA_analysis)) {
  par(ask=TRUE)
  plot(tri_MFA[,i])
}
#old
res_MFA_analysis <- MFA(MFA_analysis, group=c(9,1,2,2,2,1,3,1,1), type=c("m","s","s","s","s","s","m","n","n"), name.group = c("Typologie actionnariat","stratégie financière","performances économiques RNE","performances économiques EBE","performances économiques EBIT","CAF","productivisme","année", "cluster"), num.group.sup = c(1,4,5,6,8,9))
res_PCA_analysis <- PCA(MFA_analysis,scale.unit = FALSE,quanti.sup = c(2:9,11:12), quali.sup =c(1,20:22) )

#réalisation de la MFA,précision du type de variable et des groupes pour l'analyse
fULL_PCA <- PCA(MFA_analysis,scale.unit = FALSE,quanti.sup = c(2:7), quali.sup =c(1,21,22,23) )
#modèles réduits
res_MFA_analysis <- MFA(MFA_analysis, group=c(7,3,2,3,1,1), type=c("m","s","s","m","n","n"), name.group = c("Typologie actionnariat","stratégie financière","performances économiques RNE","productivisme","année", "cluster"), num.group.sup = c(1,2,5,6))
res_PCA_analysis <- PCA(MFA_analysis,scale.unit = FALSE,quanti.sup = c(2:10), quali.sup =c(1,15:17) )
#enlever annee 
MFA_analysis2 <- MFA_analysis[,-16]
res_MFA_analysis <- MFA(MFA_analysis2, group=c(7,3,2,3,1), type=c("m","s","s","m","n"), name.group = c("Typologie actionnariat","stratégie financière","performances économiques RNE","productivisme", "cluster"), num.group.sup = c(1,2,5))
#affichage des dimensions
barplot(res_MFA_analysis$eig[,2], names=paste("Dim",1:nrow(res_MFA_analysis$eig)))
#analyse des résultats pour les 2 premières dimensions
summary(res_MFA_analysis, nbelement=10,nb.dec=2,ncp=2)
#lien entre les variables par groupe
round(res_MFA_analysis$group$RV, 2)
#Afffichage des contributions des variables pour chaque dimension
dimdesc(res_MFA_analysis, proba = 0.2)
#visualisation avec le package Factoshiny
#enlever les annees des variables
MFAshiny(res_MFA_analysis)
PCAshiny(res_PCA_analysis)
res_PCA_analysis |> explor::explor()
fULL_PCA |> explor::explor()

res <- res_MFA_analysis |>
  factoextra::get_mfa_ind()
print(res)
data_coord <- data.frame(as_tibble(res$coord))
data_cos2 <- data.frame(as_tibble(res$cos2))
data_MFA <- data.frame(data_analysis[,c(1,4,16:22,14:15,39,27:32,35,33,34,26,24,11,36)], row.names = 1, check.names = FALSE)
data_MFA <- data_MFA[data_MFA$RNE_normalise !=0,]
data_MFA <- na.omit(data_MFA)
data_MFA[data_MFA == -Inf] <- 0
#suppression methabiovalor et methaenergie trop grosses contributions aux axes 
data_MFA<- data_MFA[!rownames(data_MFA) %in% c("PANAIS ENERGIE", "METHABIOVALOR", "Biogasconha", "AGRIOPALE SERVICES", "AGRIGAZ"), ]

data_MFA$"coord.axe1" <- data_coord$Dim.1
data_MFA$"coord.axe2" <- data_coord$Dim.2
data_MFA$"coord.axe3" <- data_coord$Dim.3
data_MFA$"cos2.axe1" <- data_cos2$Dim.1
data_MFA$"cos2.axe2" <- data_cos2$Dim.2
data_MFA$"cos2.axe3" <- data_cos2$Dim.3
#sélection des individus pour les exemples 
data_MFA<- data_MFA[rownames(data_MFA) %in% c("AGRI METHA ENERGY","Bioagrienergies","BLB GAZ (Saconin-et-Breuil)","METHA DES COTEAUX","BIOGAZ BEAUCE","METHACONFOLENTAIS","BIOCROPS","LETANG BIOGAZ","Biogaz Val de Seine","Centrale Biogaz du Dunois","EQUIMETH"), ]
library(writexl)  # For exporting Excel files
write_xlsx(data_MFA,"Memoire/ACM/résultats/version définitive/AFM finale/individus_exemples2.xlsx")
write.csv2(data_MFA, "Memoire/ACM/résultats/version définitive/AFM finale/individus2.csv",row.names = row.names(data_MFA))

#inertie par dimension
res_MFA_analysis |>
  factoextra::fviz_screeplot()
#variables par dimension
res_MFA_analysis |>
  factoextra::fviz_contrib(choice = "partial.axes", axes = 1)
res_MFA_analysis |>
  factoextra::fviz_contrib(choice = "partial.axes", axes = 2)
res_MFA_analysis |>
  factoextra::fviz_contrib(choice = "partial.axes", axes = 3)
res_MFA_analysis |>
  factoextra::fviz_contrib(choice = "partial.axes", axes = 4)

#évolution des classes par année 
# Comptage du nombre de méthanisation par classe et par année
data_raw$Annee <- data_raw$`Annee mise en service`
data_evolution <- data_raw %>%
  group_by(Annee, cluster) %>%
  summarise(new_units = n(), .groups = "drop") 
# Accmululation du nombre de métha par année
data_evolution <- data_evolution %>%
  group_by(cluster) %>%
  arrange(Annee) %>%
  mutate(cumulative_units = cumsum(new_units))
#affichage du graphique
ggplot(data_evolution, aes(x = Annee, color = cluster)) +
  geom_line(aes(y = new_units, linetype = "New Units Created"), size = 1) +  # First curve
  geom_line(aes(y = cumulative_units, linetype = "Cumulative Units"), size = 1) +  # Second curve
  labs(title = "Nombre d'unité de méthanisation mise en service par classe chaque année",
       x = "Year", y = "Number of Units",
       linetype = "Legend") +
  theme_minimal()

ggplot(data_evolution, aes(x = Annee, y = Count, color = as.factor(cluster), group = cluster)) +
  geom_line(size = 1) +  # Line plot
  geom_point(size = 3) +  # Add points for clarity
  labs(title = "Evolution du nombre de méthanisation par classe selon les années",
       x = "Année de mise en service",
       y = "Nombre de méthanisation",
       color = "Classe") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(data_evolution$Annee), max(data_evolution$Annee), 1))  # Show all years

# Proportion par classe
group_proportion <- data_raw %>%
  group_by(cluster) %>%
  summarise(total_units = n(), .groups = "drop") %>%
  # Compute overall total and proportion
  mutate(overall_total = sum(total_units),
         proportion = total_units / overall_total)
# Affichage sous forme de tableau
kable(group_proportion, digits = 2, col.names = c("Group", "Total Units", "Overall Total", "Proportion"))