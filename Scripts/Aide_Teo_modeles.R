###############################################################################################################################

################################################ Modèles départements #########################################################

###############################################################################################################################

library(tidyverse)

# On importe la base des départements
departements <- read_csv("Data/process/dep_analyse_explo.csv")
departements_sans_outliers <- read_csv("Data/process/dep_analyse_explo_sans_outliers.csv")

# Bon format les variables créées
departements[,c(25:27)] <- lapply(departements[,c(25:27)], as.factor)
departements_sans_outliers[,c(29,30)] <- lapply(departements_sans_outliers[,c(29,30)], as.factor)



                    #    A) Données avec outliers




#-----------------------  Sélection de variables 


# Méthode bacward, forward et both pour trouver les variables qu'il faut garder pour le modèle
  # Forward
modele <- glm((nb_publi ~ 1), data = departements)
modele.forward <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+is_big_nb_etu+feuilles_CART+is_extreme), data = departements, direction = "forward")
summary(modele.forward)

  # Bacward
modele <- glm((nb_publi ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+inactivite+densite+vie_chill+is_big_nb_etu+feuilles_CART+is_extreme), data= departements)
modele.backward <- step(modele,scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+is_big_nb_etu+feuilles_CART+is_extreme), data = departements, direction = "backward")
summary(modele.backward)  #garde rien...

  # Both
modele <- glm((nb_publi ~ 1), data = departements)
modele.both <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+is_big_nb_etu+feuilles_CART+is_extreme), data = departements, direction = "both")
summary(modele.both)



#----------------------- MCO 1 (variables sélectionnées par méthodes et non corrélées)


library(lmtest)
mco1 = lm(nb_publi ~ feuilles_CART+niveau_vie+is_extreme+part_etudiants, data = departements) 
summary(mco1)

#On vérifie si la forme linéaire retenue pour le modèle estimé est appropriée (au seuil de risque de 5 %). 
library(zoo)
library(lmtest) 
reset(mco1)  #forme fonctionnelle non linéaire = manque des variables pertinentes

#On vérifie ensuite le VIF du modèle
library(car)
vif(mco1)

#On vérifie maintenant l'homoscédasticité des erreurs
bptest(mco1)  #res hétéroscédastiques

#On cherche les coupables de l'hétéroscédasticité 
residualPlots(mco1)










                    #    B) Données sans outliers




#-----------------------  Sélection de variables 


# Méthode bacward, forward et both pour trouver les variables qu'il faut garder pour le modèle
  # Forward
modele <- glm((nb_publi ~ 1), data = departements_sans_outliers)
modele.forward <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+chef_age+faible_dynamisme+dense+is_big_part_diplomes+feuilles_CART), data = departements_sans_outliers, direction = "forward")
summary(modele.forward)

  # Bacward
modele <- glm((nb_publi ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+chef_age+faible_dynamisme+dense+is_big_part_diplomes+feuilles_CART), data= departements_sans_outliers)
modele.backward <- step(modele,scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+chef_age+faible_dynamisme+dense+is_big_part_diplomes+feuilles_CART), data = departements_sans_outliers, direction = "backward")
summary(modele.backward)

  # Both
modele <- glm((nb_publi ~ 1), data = departements_sans_outliers)
modele.both <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+chef_age+faible_dynamisme+dense+is_big_part_diplomes+feuilles_CART), data = departements_sans_outliers, direction = "both")
summary(modele.both)

 


#-----------------------   MCO 1 (variables sélectionnées par méthodes)



library(lmtest)
mco1_so = lm(nb_publi ~ feuilles_CART+is_big_part_diplomes+population+partis_po_chef+faible_dynamisme+dense, data = departements_sans_outliers) 
summary(mco1_so)

# Sans outliers méthodes sélectionnent plus de variables éco et moins celles créées à partir des ACP etc, mais plus de 10% en moins ds R2

#On vérifie si la forme linéaire retenue pour le modèle estimé est appropriée (au seuil de risque de 5 %). 
library(zoo)
library(lmtest) 
reset(mco1_so)  #forme fonctionnelle non linéaire = manque des variables pertinentes

#On vérifie ensuite le VIF du modèle
library(car)
vif(mco1_so)

#On vérifie maintenant l'homoscédasticité des erreurs
bptest(mco1_so)  #res hétéroscédastiques

#On cherche les coupables de l'hétéroscédasticité 
residualPlots(mco1_so)  # pas de vble ressort pour voir coupable, on va plus loin avec ncvTest




#----------------------- MCO 2 (observation plus pousser de l'hétéroscédasticité avec le nvcTest)
ncvTest(mco1_so)
spreadLevelPlot(mco1_so, smooth = FALSE) #On observe clairement une tendence positive, la courbe devrais être horizontal La suggested power Transformation est de 0.29. l'idéal serait de mettre Y à la puissance 0.29. Dans notre cas pour facilité les interprétation nous allons passer Y en logarithme et voir si cela résou le problème d'hétéroscédasticité.
    # plot entre Y et résidus et y'a tendance donc résidus pas homoscédastiques. Suggère une transformation à Y en exponentiel pour résoudre hétéroscédasticité (1 manière)
    # mais 0.29 trop compliqué donc log

#Pour la passer en log il faut transformer notre Y en strictement positif car log de 0 n'existe pas. 
departements_sans_outliers$nb_publi <- departements_sans_outliers$nb_publi + 1


mco2_so = lm(log(nb_publi) ~ feuilles_CART+is_big_part_diplomes+population+partis_po_chef+faible_dynamisme+dense, data = departements_sans_outliers) 
summary(mco2_so)

#forme fonctionnelle
reset(mco2_so)  

#VIF
vif(mco2_so)

#homoscédasticité des erreurs
ncvTest(mco2_so)
spreadLevelPlot(mco2_so, smooth = FALSE) #résolution non parfaite mais plus horizontal que précédemment. Courbe bcp plus plate, pas de tendance à la hausse donc preuve pb résolu
bptest(mco2_so)  #res homoscédastiques


