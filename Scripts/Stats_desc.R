
###############################################################################################################################

############################################ STATISTIQUES DESCRIPTIVES ########################################################

###############################################################################################################################

library(tidyverse)

# Import des bases finalisées
regions <- read_csv("./Data/process/regions.csv")
departements <- read_csv("Data/process/departements.csv")
communes <- read_csv("Data/process/communes.csv")
epci <- read_csv("Data/process/epci.csv")


        # VARIABLES QUANTI
          # continues
            # - taux_chomage
            # - primaire_VA
            # - secondaire_VA
            # - tertiaire_marchand_VA
            # - tertiaire_non_mar_VA
            # - part_plus65
            # - part_diplomes
            # - depenses_hab
            # - part_etudiants
            # - percent_pop_rurale
          # discrètes
            # - nb_ptf
            # - nb_datagouv
            # - pop_insee
            # - age_chef
            # - PIB_habitant
            # - niveau_vie
            # - nb_crea_entps
            # - nb_nuitees_hotels
            # - nb_etudiants
        # VARIABLES QUALI
          # modalités
            # - ouvre_data (2)
            # - niveau_rural_mode (6)
            # - niveau_rural_insee (4)
            # - flux_migration_res (departements français)
          # string
            # - nom
            # - partis_po_chef
            # - CSP_chef

vbles_quanti <- c("nb_publi","nb_ptf","nb_datagouv","taux_chomage","primaire_VA","secondaire_VA","tertiaire_marchand_VA","tertiaire_non_mar_VA", "part_plus65","part_diplomes","depenses_hab","part_etudiants","percent_pop_rurale","pop_insee","age_chef","PIB_habitant","niveau_vie","nb_crea_entps","nb_nuitees_hotels","nb_etudiants")
vbles_quali <- c("ouvre_data","niveau_rural_mode","niveau_rural_insee","flux_migration_res","nom","partis_po_chef","CSP_chef")





                ### A) Régions




# VARIABLES QUANTI


#----------- Points atypiques

  # Boxplots
library(rAmCharts)  # ATTENTION : individual = n° d'obs, est différent de "number of outliers" 
amBoxplot(regions$nb_ptf, xlab=" ", ylab=" ", main="Nombre de données publiées sur la plateforme locale") #4
amBoxplot(regions$nb_publi, xlab=" ", ylab=" ", main="Nombre total de données publiées") #4
amBoxplot(regions$nb_datagouv, xlab=" ", ylab=" ", main="Nombre de données publiées sur datagouv") #4
amBoxplot(regions$pop_insee, xlab=" ", ylab=" ", main="Population") #4
amBoxplot(regions$age_chef, xlab=" ", ylab=" ", main="Âge du chef de l'exécutif") #2
amBoxplot(regions$taux_chomage, xlab=" ", ylab=" ", main="Taux de chômage") #4
amBoxplot(regions$PIB_habitant, xlab=" ", ylab=" ", main="PIB par habitant")  #5
amBoxplot(regions$primaire_VA, xlab=" ", ylab=" ", main="Part du secteur primaire dans la VA")
amBoxplot(regions$secondaire_VA, xlab=" ", ylab=" ", main="Part du secteur secondaire dans la VA")
amBoxplot(regions$tertiaire_marchand_VA, xlab=" ", ylab=" ", main="Part du secteur tertiaire marchand dans la VA") #4
amBoxplot(regions$tertiaire_non_mar_VA, xlab=" ", ylab=" ", main="Part du secteur tertiaire non marchand dans la VA") #7
amBoxplot(regions$part_plus65, xlab=" ", ylab=" ", main="Part des plus de 65 ans dans la population") #1
amBoxplot(regions$niveau_vie, xlab=" ", ylab=" ", main="Médiane du niveau de vie") #7
amBoxplot(regions$part_diplomes, xlab=" ", ylab=" ", main="Part des diplômés dans la population") #4
amBoxplot(regions$nb_crea_entps, xlab=" ", ylab=" ", main="Nombre de création d'entreprises") #4
amBoxplot(regions$nb_nuitees_hotels, xlab=" ", ylab=" ", main="Nombre de nuitées dans des hôtels de tourisme") #4
amBoxplot(regions$depenses_hab, xlab=" ", ylab=" ", main="Dépenses totales par habitant") #6
amBoxplot(regions$nb_etudiants, xlab=" ", ylab=" ", main="Nombre d'étudiants dans la population") #4
amBoxplot(regions$part_etudiants, xlab=" ", ylab=" ", main="Part des étudiants dans la population") #4
amBoxplot(regions$percent_pop_rurale, xlab=" ", ylab=" ", main="Pourcentage de population vivant en zone rurale")


  # Test de Rosner pour les points atypiques
library("EnvStats")
    # nb_publi
rosnerTest(regions$nb_publi, k = 4, alpha = 0.05) #outlier quand nb de jeux ouverts sur datagouv = 861, obs° 6 à 9 càd Ile de France
    # nb_datagouv
rosnerTest(regions$nb_datagouv, k = 4, alpha = 0.05) #outlier quand nb de jeux ouverts sur datagouv >= 861, Ile de France aussi
    # pop_insee
rosnerTest(regions$pop_insee, k = 4, alpha = 0.05) #outlier quand pop = 12.258.425, Ile de France
    # age_chef
rosnerTest(regions$age_chef, k = 2, alpha = 0.05) # finalement pas d'outlier : 'FALSE'
    # taux_chomage
rosnerTest(regions$taux_chomage, k = 4, alpha = 0.05) #outlier quand taux >= 16.1, Guadeloupe, Guyane, Réunion
    # PIB_habitant
rosnerTest(regions$PIB_habitant, k = 5, alpha = 0.05) #outlier quand PIB/hab >= 34117, Ile de France
    # tertiaire_marchand_VA
rosnerTest(regions$tertiaire_marchand_VA, k = 4, alpha = 0.05) #outlier quand part = 71.9%, Ile de France
    # tertiaire_non_mar_VA
rosnerTest(regions$tertiaire_non_mar_VA, k = 7, alpha = 0.05) # finalement pas d'outlier
    # part_plus65
rosnerTest(regions$part_plus65, k = 1, alpha = 0.05) #outlier quand part = 5.3, Guyane
    # niveau_vie
rosnerTest(regions$niveau_vie, k = 7, alpha = 0.05) #outlier quand niveau =< 17880 et = 23860, Martinique, Réunion, Ile de France
    # part_diplomes
rosnerTest(regions$part_diplomes, k = 4, alpha = 0.05) #outlier quand part = 19.9%, Ile de France
    # nb_crea_entps
rosnerTest(regions$nb_crea_entps, k = 4, alpha = 0.05) #outlier quand nb = 251781, Ile de France
    # nb_nuitees_hotels
rosnerTest(regions$nb_nuitees_hotels, k = 4, alpha = 0.05) #outlier quand nb = 70736, Ile de France
    # depenses_hab
rosnerTest(regions$depenses_hab, k = 6, alpha = 0.05) #outlier quand dépenses > 1158, Guyane, Martinique, Guadeloupe, Réunion, Corse
    # nb_etudiants
rosnerTest(regions$nb_etudiants, k = 4, alpha = 0.05) #outlier quand nb = 723217, Ile de France
    # part_etudiants
rosnerTest(regions$part_etudiants, k = 4, alpha = 0.05) #outlier quand part >= 5.9%, Ile de France


# On retire les points atypiques dans une nouvelle base pour faire une double analyse par la suite
regions_sans_outliers <- regions[-c(1:9,27),]


#----------- Distribution


  # Histogrammes de distribution
library(RColorBrewer)
par(mfrow=c(5,2))
hist(regions$taux_chomage, main="Taux de chômage", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 6, name = "YlGn"))
hist(regions_sans_outliers$taux_chomage, main="Taux de chômage", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "YlGn"))
hist(regions$primaire_VA, main="Part du secteur primaire", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 5, name = "PuRd"))
hist(regions_sans_outliers$primaire_VA, main="Part du secteur primaire", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "PuRd"))
hist(regions$secondaire_VA, main="Part du secteur secondaire", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 8, name = "Reds"))
hist(regions_sans_outliers$secondaire_VA, main="Part du secteur secondaire", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 6, name = "Reds"))
hist(regions$tertiaire_marchand_VA, main="Part du secteur tertiaire marchand", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 8, name = "Blues"))
hist(regions_sans_outliers$tertiaire_marchand_VA, main="Part du secteur tertiaire marchand", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "Blues"))
hist(regions$tertiaire_non_mar_VA, main="Part du secteur tertiaire non marchand", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 5, name = "Greys"))
hist(regions_sans_outliers$tertiaire_non_mar_VA, main="Part du secteur tertiaire non marchand", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "Greys"))

par(mfrow=c(5,2))
hist(regions$part_plus65, main="Part des plus de 65 ans", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 4, name = "Purples"))
hist(regions_sans_outliers$part_plus65, main="Part des plus de 65 ans", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "Purples"))
hist(regions$part_diplomes, main="Part des diplômés", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Oranges"))
hist(regions_sans_outliers$part_diplomes, main="Part des diplômés", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Oranges"))
hist(regions$depenses_hab, main="Dépenses par habitant", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Greens"))
hist(regions_sans_outliers$depenses_hab, main="Dépenses par habitant", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Greens"))
hist(regions$part_etudiants, main="Part des étudiants", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Spectral"))
hist(regions_sans_outliers$part_etudiants, main="Part des étudiants", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "Spectral"))
hist(regions$percent_pop_rurale, main="Pourcentage de population rurale", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 6, name = "PuBu"))
hist(regions_sans_outliers$percent_pop_rurale, main="Pourcentage de population rurale", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "PuBu"))


  # Test de Spearman des bases sans outliers
shapiro.test(regions_sans_outliers$taux_chomage)
shapiro.test(regions_sans_outliers$primaire_VA)  #normal à 5%
shapiro.test(regions_sans_outliers$secondaire_VA)  #normal
shapiro.test(regions_sans_outliers$tertiaire_marchand_VA)  #normal
shapiro.test(regions_sans_outliers$tertiaire_non_mar_VA)  #tout juste normal
shapiro.test(regions_sans_outliers$part_plus65)  #normal
shapiro.test(regions_sans_outliers$part_diplomes)
shapiro.test(regions_sans_outliers$depenses_hab)  #normal
shapiro.test(regions_sans_outliers$part_etudiants)  #normal
shapiro.test(regions_sans_outliers$percent_pop_rurale)


  # Matrice de corrélation de Spearman car variables ne suivent pas toutes une loi normale (on exclu les Y)
cor1 <- cor(regions_sans_outliers[,c("taux_chomage","primaire_VA","secondaire_VA","tertiaire_marchand_VA","tertiaire_non_mar_VA", "part_plus65","part_diplomes","depenses_hab","part_etudiants","percent_pop_rurale","pop_insee","age_chef","PIB_habitant","niveau_vie","nb_crea_entps","nb_nuitees_hotels","nb_etudiants")], use="complete.obs", method=c("spearman"))
library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor1, method="color", col=col(200), 
             type="upper",
             addCoef.col = "black")
# corrélations moyennes (0.5<x<0.6) :
    # - taux_chomage et depenses_hab/pop_insee/nb_etudiants
    # - primaire_VA et part_diplomes/nb_nuitees_hotels/nb_etudiants
    # - secondaire_VA et depenses_hab/nb_cre_entps
    # - tertiaire_marchand_VA et nb_crea_entps
    # - tertiaire_non_mar_VA et depenses_hab/
    # - part_plus65 et part_etudiants/age_chef
    # - part_etudiants et nb_crea_entps
    # - percent_pop_rurale et pop_insee/nb_etudiants
# corrélations fortes (x>0.6) :
    # - taux_chomage et tertaire_non_mar_VA/percent_pop_rurale/niveau_vie
    # - primaire_VA et tertiaire_marchand_VA/percent_pop_rurale/nb_crea_entps
    # - secondaire_VA et tertiaire_marchand_VA/part_diplomes
    # - tertiaire_marchand_VA et part_diplomes/depenses_hab/PIB_habitant
    # - tertaire_non_mar_VA et PIB_habitant/niveau_vie
    # - part_diplomes et depenses_hab/PIb_habitant/nb_crea_entps/nb_nuitees_hotels
    # - depenses_hab et PIB_habitant/niveau_vie
    # - part_etudiants et pop_insee/age_chef/nb_etudiants
    # - percent_pop_rurale et nb_crea_entps/nb_nuitees_hotels
    # - pop_insee et nb_crea_entps/nb_nuitees_hotels/nb_etudiants
    # - PIB_habitant et niveau_vie
    # - nb_crea_entps et nb_nuitees_hotels/nb_etudiants
    # - nb_nuitees_hotels et nb_etudiants


  # Statistiques
summary(regions) 
NA_reg <- as.data.frame(apply(is.na(regions), 2, sum)) %>% rename(nb_NA = `apply(is.na(regions), 2, sum)`) %>%
                        mutate(percent_NA = nb_NA/nrow(regions)*100) %>% mutate(percent_NA = round(percent_NA, 2)) #max 3/26 NA







# ACP, arbre décisionnel, clusters quand Y défini(s)
library(FactoMineR)
library(factoextra)
basepauvreté1.actifs=basepauvreté1[,c(4:12)]
basepauvreté1.illus=basepauvreté1[,3]
basepauvreté2=basepauvreté1.actifs
View(basepauvreté2)

par(mfrow=c(1,1))
fviz_pca_var (res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)



# VARIABLES QUALI

#Fréquences of categorical levels
#cluster partis po



# CORSE / Gilles SImeoni, Professions libérales, 20 avril 67, Femu a Corsica
# GUYANE : Gabriel SERville, Professions Intermédiaires, 27 septembre 1959, Péyi Guyane
# MARTINIQUE: Alfred Marie-Jeanne, Professions Intermédiaires, 15 novembre 1936, Mouvement indépendantiste martiniquais


                ### B) Départements




# VARIABLES QUANTI


summary(departements) 
NA_dep <- as.data.frame(apply(is.na(departements), 2, sum)) %>% rename(nb_NA = `apply(is.na(departements), 2, sum)`) #/119


# VARIABLES QUALI






                ### C) Communes




# VARIABLES QUANTI


summary(communes) 
NA_com <- as.data.frame(apply(is.na(communes), 2, sum)) %>% rename(nb_NA = `apply(is.na(communes), 2, sum)`) #/35684


# VARIABLES QUALI






                ### D) EPCI




# VARIABLES QUANTI

summary(epci) 
NA_epci <- as.data.frame(apply(is.na(epci), 2, sum)) %>% rename(nb_NA = `apply(is.na(epci), 2, sum)`) #/1318



# VARIABLES QUALI




#------------------
test <- epci[,c("nom","pop_insee","partis_po_chef")] %>% filter(stringr::str_detect(nom, "Métropole"))
test <- test %>% distinct(c("nom","pop_insee"))





