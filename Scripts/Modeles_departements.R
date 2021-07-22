
###############################################################################################################################

################################################ Modèles départements #########################################################

###############################################################################################################################

library(tidyverse)

# On importe la base des départements
departements <- read_csv("Data/process/dep_analyse_explo.csv")



#-----------------------  Sélection de variables


# Méthode bacward, forward et both pour trouver les variables qu'il faut garder pour le modèle
  # Forward
modele <- glm((nb_publi ~ 1), data = departements)
modele.forward <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural_mode+niveau_rural_insee+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+pop_insee+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+inactivite+densite+vie_chill+is_big_nb_etu+feuilles_CART+is_extreme), data = departements, direction = "forward")
summary(modele.forward)

  # Bacward
modele <- glm((nb_publi ~ taux_chomage+CSP_chef+niveau_rural_mode+niveau_rural_insee+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+pop_insee+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+inactivite+densite+vie_chill+is_big_nb_etu+feuilles_CART+is_extreme), data= departements)
modele.backward <- step(modele,scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural_mode+niveau_rural_insee+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+pop_insee+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+inactivite+densite+vie_chill+is_big_nb_etu+feuilles_CART+is_extreme), data = departements, direction = "backward")
summary(modele.backward)

  # Both
modele <- glm((nb_publi ~ 1), data = departements)
modele.both <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural_mode+niveau_rural_insee+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+pop_insee+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+inactivite+densite+vie_chill+is_big_nb_etu+feuilles_CART+is_extreme), data = departements, direction = "both")
summary(modele.both)



#-----------------------  Premier MCO (variables sélectionnées par méthodes)



library(lmtest)
modele = lm(nb_publi ~ feuilles_CART+depenses_hab+densite+part_etudiants+is_extreme+part_plus65+partis_po_chef, data = departements) 
summary(modele)

#On vérifie si la forme linéaire retenue pour le modèle estimé est appropriée (au seuil de risque de 5 %). 
library(zoo)
library(lmtest) 
reset(modele)  #forme fonctionnelle non linéaire

#On vérifie ensuite le VIF du modèle
library(car)
vif(modele)

#On vérifie maintenant l'homoscédasticité des erreurs
bptest(modele)  #res hétéroscédastiques

#On cherche les coupables de l'hétéroscédasticité 
residualPlots(modele)







 