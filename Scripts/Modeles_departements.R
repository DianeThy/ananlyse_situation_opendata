
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



#-----------------------  Premier MCO (variables sélectionnées par méthodes)



library(lmtest)
modele = lm(nb_publi ~ feuilles_CART+depenses_hab+niveau_vie+is_extreme+niveau_rural, data = departements) 
summary(modele)

#On vérifie si la forme linéaire retenue pour le modèle estimé est appropriée (au seuil de risque de 5 %). 
library(zoo)
library(lmtest) 
reset(modele)  #forme fonctionnelle non linéaire = manque des variables pertinentes

#On vérifie ensuite le VIF du modèle
library(car)
vif(modele)

#On vérifie maintenant l'homoscédasticité des erreurs
bptest(modele)  #res hétéroscédastiques

#On cherche les coupables de l'hétéroscédasticité 
residualPlots(modele)








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

 


#-----------------------  Premier MCO (variables sélectionnées par méthodes)



library(lmtest)
modele = lm(nb_publi ~ feuilles_CART+is_big_part_diplomes+percent_pop_rurale+population+partis_po_chef+faible_dynamisme+dense, data = departements_sans_outliers) 
summary(modele)

# Sans outliers méthodes sélectionnent plus de variables éco et moins celles créées à partir des ACP etc, mais plus de 10% en moins ds R2