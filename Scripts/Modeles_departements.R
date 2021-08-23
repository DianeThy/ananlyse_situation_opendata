
###############################################################################################################################

################################################ Modèles départements #########################################################

###############################################################################################################################

library(tidyverse)

# On importe la base des départements
departements <- read_csv("Data/process/dep_analyse_explo.csv")
departements_sans_outliers <- read_csv("Data/process/dep_analyse_explo_sans_outliers.csv")

# Bon format les variables catégorielles
departements_sans_outliers[,c("part_diplomes_5.65","code_region","CSP_chef","niveau_rural","niveau_densite","partis_po_chef")] <- lapply(departements_sans_outliers[,c("part_diplomes_5.65","code_region","CSP_chef","niveau_rural","niveau_densite","partis_po_chef")], as.factor)
departements[,c("code_region","CSP_chef","niveau_rural","niveau_densite","partis_po_chef")] <- lapply(departements[,c("code_region","CSP_chef","niveau_rural","niveau_densite","partis_po_chef")], as.factor)



                    #    A) Données sans outliers




#-----------------------  Sélection de variables 


# Méthode bacward, forward et both pour trouver les variables qu'il faut garder pour le modèle
  # Forward
modele <- glm((nb_publi ~ 1), data = departements_sans_outliers)
modele.forward <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data = departements_sans_outliers, direction = "forward")
summary(modele.forward)

  # Bacward
modele <- glm((nb_publi ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data= departements_sans_outliers)
modele.backward <- step(modele,scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data = departements_sans_outliers, direction = "backward")
summary(modele.backward)  #choix vble car 4 sont correlées : on prend la plus significative

  # Both
modele <- glm((nb_publi ~ 1), data = departements_sans_outliers)
modele.both <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data = departements_sans_outliers, direction = "both")
summary(modele.both)

 


#---  Premier MCO (variables sélectionnées par méthodes)


library(car)

library(lmtest)
mco1 = lm(nb_publi ~ part_diplomes_5.65 + niveau_densite + nb_crea_entps + CSP_chef + partis_po_chef, data = departements_sans_outliers) 
summary(mco1)

#On vérifie que la forme linéaire retenue pour le modèle estimé soit appropriée (au seuil de risque de 5 %). 
library(zoo)
library(lmtest) 
reset(mco1)  #forme fonctionnelle linéaire

    # méthode 2
ncvTest(mco1)
spreadLevelPlot(mco1, smooth = FALSE) #On observe clairement une tendence positive, la courbe devrais être horizontal La suggested power Transformation est de 0.29. l'idéal serait de mettre Y à la puissance 0.29. Dans notre cas pour facilité les interprétation nous allons passer Y en logarithme et voir si cela résou le problème d'hétéroscédasticité.
    # plot entre Y et résidus et y'a tendance donc résidus pas homoscédastiques. Suggère une transformation à Y en exponentiel pour résoudre hétéroscédasticité (1 manière)
    # mais 0.29 trop compliqué donc log

#Pour la passer en log il faut transformer notre Y en strictement positif car log de 0 n'existe pas. 
departements_sans_outliers$nb_publi1 <- departements_sans_outliers$nb_publi + 1


mco2 = lm(log(nb_publi1) ~ part_diplomes_5.65 + niveau_densite + nb_crea_entps + CSP_chef + partis_po_chef, data = departements_sans_outliers) 

# On retire les variables non significatives
mco2 = lm(log(nb_publi1) ~ part_diplomes_5.65 + niveau_densite, data = departements_sans_outliers) 
summary(mco2)

#forme fonctionnelle
reset(mco2)  #forme adaptée

#VIF
vif(mco2) #pas de mutlicolinéarité

# Homoscédasticité
bptest(mco2)  #res homoscédastiques

# Distance de Cook (données influentes)
plot(cooks.distance(mco2),type="h") # <0.1
influenceIndexPlot(mco2)
outlierTest(mco2)

#homoscédasticité des erreurs
ncvTest(mco2)
spreadLevelPlot(mco2, smooth = FALSE) #résolution non parfaite mais plus horizontal que précédemment. Courbe bcp plus plate, pas de tendance à la hausse donc preuve pb résolu


### Etude des résidus : 
resMCO2=residuals(mco2)
        # Normalité
par(mfrow=c(1,3))
boxplot(resMCO2, main="Boxplot des résidus", col="#FF9966")
hist(resMCO2, xlab="Résidus", ylab="Fréquences", main="Histogramme des résidus", freq=TRUE, col=brewer.pal(n = 8, name = "Reds"))
qqnorm(resMCO2)
qqline(resMCO2, col="red")
ks.test(resMCO2, "pnorm", mean(resMCO2), sd(resMCO2)) #ou
shapiro.test(resMCO2)

        # Indépendance
acf(resMCO2, main="resMCO2")
durbinWatsonTest(resMCO2)

        # Homogénéité
plot(mco2, 3) #courbe plate donc homogènes mais res
ncvTest(mco2)


# on regarde quels départements sont le mieux et moins bien prédits
departements_sans_outliers$prediction <- predict(mco2)
departements_sans_outliers$residus <- resMCO2
departements_sans_outliers$log_nb_publi1 <- log(departements_sans_outliers$nb_publi1)
summary(departements_sans_outliers$residus)
res <- departements_sans_outliers[,c(1,2,29,30,31)]



#---  Deuxième MCO (variables de l'ACP)


# Estimation
modele = lm(nb_publi ~ dynamisme + pauvrete + age_chef, data = departements_sans_outliers) 
summary(modele) #aucune variable significative

# Diagnostic
reset(modele)  
vif(modele)
bptest(modele)
plot(cooks.distance(modele), type="h") 



#---  Troisième MCO (toutes les variables)



modele <- lm(nb_publi ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+part_diplomes_5.65, data = departements_sans_outliers)
summary(modele)

reset(modele)  
vif(modele)
bptest(modele)
plot(cooks.distance(modele), type="h") 

# Variables significatives et non correlées
modele <- lm(nb_publi ~ partis_po_chef + part_diplomes_5.65, data = departements_sans_outliers)
summary(modele)

reset(modele)  
vif(modele)
bptest(modele)
plot(cooks.distance(modele), type="h") 



                    #    B) Données avec outliers



#-----------------------  Sélection de variables 


# Méthode bacward, forward et both pour trouver les variables qu'il faut garder pour le modèle
  # Forward
modele <- glm((nb_publi ~ 1), data = departements)
modele.forward <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef), data = departements, direction = "forward")
summary(modele.forward)

  # Bacward
modele <- glm((nb_publi ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef), data= departements)
modele.backward <- step(modele,scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef), data = departements, direction = "backward")
summary(modele.backward)  #garde rien...

  # Both
modele <- glm((nb_publi ~ 1), data = departements)
modele.both <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef), data = departements, direction = "both")
summary(modele.both)



#---  Premier MCO (variables sélectionnées par méthodes)



modele = lm(nb_publi ~ niveau_rural+nb_etudiants+niveau_vie, data = departements) 
summary(modele)

reset(modele)  #forme fonctionnelle non linéaire = manque des variables pertinentes
vif(modele)
bptest(modele)  #res hétéroscédastiques
residualPlots(modele)
plot(cooks.distance(modele), type="h") 

# Transformation
departements$nb_publi1 <- departements$nb_publi + 1
mco2_so = lm(log(nb_publi1) ~ niveau_rural+nb_etudiants+niveau_vie, data = departements) 
mco2_so = lm(log(nb_publi1) ~ niveau_rural+nb_etudiants, data = departements) 
summary(mco2_so)

reset(mco2_so)  
vif(mco2_so)
bptest(mco2_so)
plot(cooks.distance(mco2_so), type="h") 

ncvTest(mco2_so)
spreadLevelPlot(mco2_so, smooth = FALSE) #On observe clairement une tendence positive, la courbe devrais être horizontal La 







#####################################################################################################

################################# Modèles Poisson ###########################################

##################################################################################################


departements_sans_outliers$nb_publi <- as.integer(departements_sans_outliers$nb_publi)
y <- departements_sans_outliers$nb_publi


#-----------------------  Sélection de variables 


# Méthode bacward, forward et both pour trouver les variables qu'il faut garder pour le modèle
  # Forward
modele <- glm((nb_publi ~ 1), data = departements_sans_outliers)
modele.forward <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data = departements_sans_outliers, direction = "forward")
summary(modele.forward)

  # Bacward
modele <- glm((nb_publi ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data= departements_sans_outliers)
modele.backward <- step(modele,scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data = departements_sans_outliers, direction = "backward")
summary(modele.backward)  #choix vble car 4 sont correlées : on prend la plus significative

  # Both
modele <- glm((nb_publi ~ 1), data = departements_sans_outliers)
modele.both <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data = departements_sans_outliers, direction = "both")
summary(modele.both)

 


#---  Premier modèle (variables sélectionnées par méthodes)

# Poisson
library(stats)
modele <- glm(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65+nb_crea_entps, data = departements_sans_outliers, family = poisson(link=log))
summary(modele)
AIC(modele)


# Verif conditions
mean(departements_sans_outliers$nb_publi)
set.seed(1234) # pour simuler tjs les mêmes comptages
theoretic_count <-rpois(72, 13.44)

# on incorpore ces comptages théoriques dans un data frame
tc_df <-data.frame(theoretic_count)

# on plot simultanémaent les comptages observés et les comptages théoriques
library(ggplot2)
ggplot(departements_sans_outliers, aes(nb_publi))+
   geom_bar(fill="#1E90FF")+
   geom_bar(data=tc_df, aes(theoretic_count,fill="#1E90FF", alpha=0.5))+
    labs(title="Comptages théoriques et comptages observés",
        x ="Nombre de publications", y = "Fréquence")+
   theme_classic()+
   theme(legend.position="none")  #surdispersion et inflation en zero


# Autre test disp
var(departements_sans_outliers$nb_publi)
mean(departements_sans_outliers$nb_publi)

# Dispersion
disp<-modele_poiss$deviance/modele_poiss$df.residual
disp  #sur dispersion car disp >1

# Quasi poisson
modele_qp <- glm(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65, data = departements_sans_outliers, family = quasipoisson(link=log))
summary(modele_qp)

# Binomiale négative
library(MASS)
nb_fit <- glm.nb(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65, data = departements_sans_outliers)
summary(nb_fit)
AIC(nb_fit)

# Distrib Y
table(departements_sans_outliers$nb_publi) #26 Y=0 soit 36%



#---  Modèles à inflation de zéro 


# Sélection de variables pour la partie binaire


# Méthode bacward, forward et both pour trouver les variables qu'il faut garder pour le modèle
  # Forward
modele <- glm((ouvre_data ~ 1), data = departements_sans_outliers)
modele.forward <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data = departements_sans_outliers, direction = "forward")
summary(modele.forward)

  # Bacward
modele <- glm((ouvre_data ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data= departements_sans_outliers)
modele.backward <- step(modele,scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data = departements_sans_outliers, direction = "backward")
summary(modele.backward)  #choix vble car correlées

  # Both
modele <- glm((ouvre_data ~ 1), data = departements_sans_outliers)
modele.both <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data = departements_sans_outliers, direction = "both")
summary(modele.both)


library(pscl)
    # pour loi de poisson
fm_zip <- zeroinfl(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65 | CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65+age_chef, data=departements_sans_outliers)
summary(fm_zip) #on retire les variables non significatives
fm_zip2 <- zeroinfl(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65 | part_diplomes_5.65+age_chef, data=departements_sans_outliers)
summary(fm_zip2)
fm_zip3 <- zeroinfl(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65 | part_diplomes_5.65, data=departements_sans_outliers)
summary(fm_zip3)
AIC(fm_zip3)
    # pour loi binomiale négative
fm_zinb <- zeroinfl(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65 | CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65+age_chef, dist="negbin", data=departements_sans_outliers)
summary(fm_zinb) #rien n'est significatif
fm_zinb2 <- zeroinfl(nb_publi ~ niveau_densite+part_diplomes_5.65 | 1, dist="negbin", data=departements_sans_outliers)
summary(fm_zinb2) #rien n'est significatif
AIC(fm_zinb2)


#--- Modèle double hurdle


# Modèle poisson
library(pscl)
mod.hurdle<-hurdle(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65 | CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65+age_chef, data=departements_sans_outliers, dist="poisson", zero.dist="binomial")
summary(mod.hurdle)

mod.hurdle2 <- hurdle(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65 | part_diplomes_5.65+age_chef, data=departements_sans_outliers, dist="poisson", zero.dist="binomial")
summary(mod.hurdle2)

mod.hurdle3 <- hurdle(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65 | part_diplomes_5.65, data=departements_sans_outliers, dist="poisson", zero.dist="binomial")
summary(mod.hurdle3)
AIC(mod.hurdle3)

# Modèle négative binomial
mod.hurdle.nb <- hurdle(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65 | CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65+age_chef, data= departements_sans_outliers, dist="negbin")
summary(mod.hurdle.nb) 

mod.hurdle.nb2 <- hurdle(nb_publi ~ niveau_densite+part_diplomes_5.65 | CSP_chef+part_diplomes_5.65+age_chef, data= departements_sans_outliers, dist="negbin")
summary(mod.hurdle.nb2) 

mod.hurdle.nb3 <- hurdle(nb_publi ~ niveau_densite+part_diplomes_5.65 | part_diplomes_5.65+age_chef, data= departements_sans_outliers, dist="negbin")
summary(mod.hurdle.nb3) #mieux Poisson car log(tetha) pas significatif

mod.hurdle.nb4 <- hurdle(nb_publi ~ niveau_densite+part_diplomes_5.65 | part_diplomes_5.65, data= departements_sans_outliers, dist="negbin")
summary(mod.hurdle.nb4) #mieux Poisson car log(tetha) pas significatif
AIC(mod.hurdle.nb4)



#---  Comparaisons

# Comparaison modèles
odTest(nb_fit)  #binomial négatif plus adapté


# Test de Vuong
vuong(modele, fm_zip3) #z-stat>2 dc ZIP mieux que poisson
vuong(nb_fit, fm_zinb2)  #mitigé

vuong(modele, mod.hurdle3) #loi de Poisson
vuong(nb_fit, mod.hurdle.nb4) #loi binomiale négative

vuong(fm_zip3, mod.hurdle3) #loi de Poisson
vuong(fm_zinb2, mod.hurdle.nb4) #loi binomiale négative



# Résultats
    # significativité log(theta)
summary(fm_zinb2) #log(theta) pas signif donc fm_zip mieux  
summary(mod.hurdle.nb4) 
    # valeurs des coefs
summary(fm_zip3) 
summary(mod.hurdle3) 




#---  Modèle final



# Pseudo R2 fm_zip3
rd <- ifelse(y==0,0,y*log(y/fm_zip3$fitted.values))-(y-fm_zip3$fitted.values) 
print(rd) 
DS = 2 * sum(rd) 
print(DS)
a0 <- log(mean(y)) 
print(a0) 
rd0 <- ifelse(y==0,0,y *log(y)) - y * a0 - (y - exp(a0)) 
D0 <- 2 * sum(rd0) 
print(D0)
R2 <- (D0-DS)/D0 
print(R2)

# Pseudo R2 mod.hurdle3
rd <- ifelse(y==0,0,y*log(y/mod.hurdle3$fitted.values))-(y-mod.hurdle3$fitted.values) 
print(rd) 
DS = 2 * sum(rd) 
print(DS)
a0 <- log(mean(y)) 
print(a0) 
rd0 <- ifelse(y==0,0,y *log(y)) - y * a0 - (y - exp(a0)) 
D0 <- 2 * sum(rd0) 
print(D0)
R2 <- (D0-DS)/D0 
print(R2)




# Interpretations des résultats
exp(coef((mod.hurdle3)))


# Résidus
res <- as.data.frame(residuals(fm_zip2))
res$nom <- departements_sans_outliers$nom
res$abs_res <- abs(res$re)
