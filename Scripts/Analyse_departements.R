
###############################################################################################################################

################################################ ANALYSE DEPARTEMENTS #########################################################

###############################################################################################################################

library(tidyverse)

# On importe la base des départements
departements <- read_csv("Data/process/departements.csv")

# Les variables :
vbles_quanti <- c("nb_publi","nb_ptf","nb_datagouv","taux_chomage","part_plus65","part_diplomes","depenses_hab","part_etudiants","percent_pop_rurale","pop_insee","age_chef","niveau_vie","nb_crea_entps","nb_nuitees_hotels","nb_etudiants")
vbles_quali <- c("nom","ouvre_data","niveau_rural_mode","niveau_rural_insee","flux_migration_res","partis_po_chef","CSP_chef")

# On met au bon format les variables qualis
departements[,c("ouvre_data","niveau_rural_mode","niveau_rural_insee","flux_migration_res","partis_po_chef","CSP_chef")] <- lapply(departements[,c("ouvre_data","niveau_rural_mode","niveau_rural_insee","flux_migration_res","partis_po_chef","CSP_chef")], as.factor)



#----------------------- Harmonisation couleur politique


# On regarde tous les partis po de la base
as.data.frame(table(departements$partis_po_chef)) %>% arrange(desc(Freq))

# On les regroupe en 2 modalités principales : droite / gauche et on laisse les sans étiquettes dans une autre modalité
departements <- departements %>% mutate(partis_po_chef = case_when(partis_po_chef == "Parti socialiste" ~ "Gauche",
                                                                   partis_po_chef == "Les Républicains" ~ "Droite",
                                                                   partis_po_chef == "Union pour un mouvement populaire" ~ "Droite",
                                                                   partis_po_chef == "Union des démocrates et indépendants" ~ "Droite",
                                                                   partis_po_chef == "Union pour la démocratie française" ~ "Droite",
                                                                   partis_po_chef == "divers droite" ~ "Droite",
                                                                   partis_po_chef == "Rassemblement pour la République" ~ "Droite",
                                                                   partis_po_chef == "Démocratie libérale" ~ "Droite",
                                                                   partis_po_chef == "Parti républicain" ~ "Droite",
                                                                   partis_po_chef == "sans étiquette" ~ "sans étiquette",
                                                                   partis_po_chef == "Union des démocrates pour la République" ~ "Droite",
                                                                   partis_po_chef == "Centre des démocrates sociaux" ~ "Droite",
                                                                   partis_po_chef == "Libres" ~ "Droite",
                                                                   partis_po_chef == "Objectif France" ~ "Droite",
                                                                   partis_po_chef == "Occident" ~ "Droite",
                                                                   partis_po_chef == "Parti communiste français" ~ "Gauche",
                                                                   partis_po_chef == "Parti radical de gauche" ~ "Gauche"
                                                                   ))

# partis po harmonisés, on supprime les doublons (quand qq1 avait plusieurs partis renseignés, dû aux noms qui changent)
departements <- departements %>% unique()




#----------------------- Harmonisation CSP du chef de l'exécutif


# On regarde tous les partis po de la base
as.data.frame(table(departements$CSP_chef)) %>% arrange(desc(Freq))

# Même chose base avec outliers
departements <- departements %>% mutate(CSP_chef = case_when(CSP_chef == "Fonctionnaires de catégorie A" ~ "3",
                                                             CSP_chef == "Permanents politiques" ~ "3",
                                                             CSP_chef == "Autres professions" ~ "8",
                                                             CSP_chef == "Retraités de l'enseignement" ~ "7",
                                                             CSP_chef == "Retraités des professions libérales" ~ "7",
                                                             CSP_chef == "Retraités fonct.publique (sf enseig.)" ~ "7",
                                                             CSP_chef == "Autres cadres (secteur privé)" ~ "3",
                                                             CSP_chef == "Pharmaciens" ~ "3",
                                                             CSP_chef == "Retraités agricoles" ~ "7",
                                                             CSP_chef == "Retraités salariés privés" ~ "7",
                                                             CSP_chef == "Sans profession déclarée" ~ "8",
                                                             CSP_chef == "Agriculteurs propriétaires exploit." ~ "1",
                                                             CSP_chef == "Autres professions libérales" ~ "3",
                                                             CSP_chef == "Avocats" ~ "3",
                                                             CSP_chef == "Enseignants 1er deg.-directeurs école" ~ "4",
                                                             CSP_chef == "Grands corps de l'état" ~ "3",
                                                             CSP_chef == "Industriels-Chefs entreprise" ~ "2",
                                                             CSP_chef == "Ingénieurs" ~ "3",
                                                             CSP_chef == "Professeurs du secondaire et techn." ~ "4",
                                                             CSP_chef == "Professions rattachées à enseignt." ~ "4",
                                                             CSP_chef == "Retr.artis.commerc.chefs d'entrep." ~ "7",
                                                             CSP_chef == "Agents technique et techniciens" ~ "4",
                                                             CSP_chef == "Autres retraités" ~ "7",
                                                             CSP_chef == "Cadres supérieurs (secteur privé)" ~ "3",
                                                             CSP_chef == "Employés (secteur privé)" ~ "5",
                                                             CSP_chef == "Journalistes et autres médias" ~ "3",
                                                             CSP_chef == "Professeurs de faculté" ~ "3",
                                                             CSP_chef == "Représentants de commerce" ~ "4",
                                                             CSP_chef == "Retraités des entreprises publiques" ~ "7",
                                                             CSP_chef == "Vétérinaires" ~ "3",
                                                             CSP_chef == "Agents généraux d'assurances" ~ "2",
                                                             CSP_chef == "Médecins" ~ "3",
                                                             CSP_chef == "Fonctionnaires de catégorie B" ~ "4"
                                                             ))





########################################################################
## Dataframe dictionnaire des variables
########################################################################


# Description niveau_rural_mode
niveau_rural_mode <- as.data.frame(table(departements$niveau_rural_mode)) %>% rename(num = Var1) %>% select(-Freq)
niveau_rural_mode <- niveau_rural_mode %>% mutate(description = c("urbain dense","urbain densité intermédiaire", "rural sous forte influence d'un pôle", "rural sous faible influence d'un pôle", "rural autonome peu dense", "rural autonome très peu dense"),
                                                  variable = "niveau_rural_mode") %>% select(variable,num,description)
# Description niveau_rural_insee
niveau_rural_insee <- as.data.frame(table(departements$niveau_rural_insee)) %>% rename(num = Var1) %>% select(-Freq)
niveau_rural_insee <- niveau_rural_insee %>% mutate(description = c("très dense", "dense", "peu dense"),
                                                    variable = "niveau_rural_insee") %>% select(variable,num,description)
# Description CSP_chef
CSP_chef <- as.data.frame(table(departements$CSP_chef)) %>% rename(num = Var1) %>% select(-Freq)
CSP_chef <- CSP_chef %>% mutate(description = c("Agriculteurs exploitants","Artisans, commerçants et chefs d'entreprise","Cadres et professions intellectuelles supérieures","Professions Intermédiaires", "Employés","Retraités","Autres personnes sans activité professionnelle"),
                                variable = "CSP_chef") %>% select(variable,num,description)
# Description flux_migration_res
flux_migration_res <- as.data.frame(table(departements$flux_migration_res)) %>% rename(num = Var1) %>% select(-Freq) 
  # on va recouper avec le nom du département (import base)
infos_dep <- read_csv("Data/raw/infos_departements.csv")
infos_dep$COG <- as.character(infos_dep$COG)
flux_migration_res <- left_join(flux_migration_res, infos_dep[,c(1,3)], by = c("num" = "COG")) %>% rename(description = nom)
flux_migration_res <- flux_migration_res %>% mutate(variable = "flux_migration_res") %>% select(variable,num,description)
flux_migration_res[20,]$description <- "Corse"  # pour la Corse on met à la main car pas dans infos_dep

# Description code_region
code_region <- as.data.frame(table(departements$code_region)) %>% rename(num = Var1) %>% select(-Freq) 
  # on va recouper avec le nom de la région (import base)
infos_reg <- read_csv("Data/raw/infos_regions.csv")
infos_reg$COG <- as.character(infos_reg$COG)
code_region <- left_join(code_region, infos_reg[,c(1,3)], by = c("num" = "COG")) %>% rename(description = nom)
code_region <- code_region %>% mutate(variable = "code_region") %>% select(variable,num,description)


# On met tout ça ensemble pour avoir un dictionnaire des variables
dico_variables <- rbind(niveau_rural_mode, niveau_rural_insee, CSP_chef, flux_migration_res, code_region)





                ### A) Visualisations des variables une à une





#----------------------- Statistiques descriptives


# Statistiques
library(summarytools)
view(dfSummary(departements))

# Répartitions des variables qualis
library(inspectdf)
inspect_cat(departements[,c("ouvre_data","code_region","niveau_rural_mode","niveau_rural_insee","flux_migration_res","CSP_chef","partis_po_chef")]) %>% show_plot() 




#----------------------- Points atypiques 


ggplot(departements, aes(x=nb_publi, y=frequency(nb_publi))) + #10 potentiels outliers 
  labs(title="Distribution et box du nombre de publications open data", x="Nombe de publications", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements, aes(x=taux_chomage, y=frequency(taux_chomage))) +  #3
  labs(title="Distribution et box du taux de chômage", x="Taux de chômage", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements, aes(x=part_plus65, y=frequency(part_plus65))) +  
  labs(title="Distribution et box de la part des plus de 65 ans", x="Part des plus de 65 ans", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements, aes(x=part_diplomes, y=frequency(part_diplomes))) +  #6
  labs(title="Distribution et box de la part des diplômés dans la population", x="Part des diplômés", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements, aes(x=depenses_hab, y=frequency(depenses_hab))) +  #3
  labs(title="Distribution et box des dépenses par habitant", x="Dépenses par habitant", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements, aes(x=part_etudiants, y=frequency(part_etudiants))) +  #2
  labs(title="Distribution et box de la part des étudiants dans la population", x="Part des étudiants", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements, aes(x=nb_etudiants, y=frequency(nb_etudiants))) +  #8
  labs(title="Distribution et box du nombre d'étudiants", x="Nombre d'étudiants", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements, aes(x=percent_pop_rurale, y=frequency(percent_pop_rurale))) +  
  labs(title="Distribution et box de la part de le population vivant en zone rurale", x="Part de population", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements, aes(x=pop_insee, y=frequency(pop_insee))) +  #3
  labs(title="Distribution et box du nombre d'habitants", x="Nombre d'habitants", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements, aes(x=age_chef, y=frequency(age_chef))) +  
  labs(title="Distribution et box de l'âge du chef de l'exécutif", x="Âge du chef", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements, aes(x=niveau_vie, y=frequency(niveau_vie))) +  #5
  labs(title="Distribution et box de la médiane du niveau de vie", x="Médiane du niveau de vie", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements, aes(x=nb_crea_entps, y=frequency(nb_crea_entps))) +  #? 
  labs(title="Distribution et box du nombre de créations d'entreprises", x="Créations d'entreprises", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements, aes(x=nb_nuitees_hotels, y=frequency(nb_nuitees_hotels))) +  #6
  labs(title="Distribution et box du nombre de nuitées recensées dans des hôtels de tourisme", x="Nombre de nuitées", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()


# Pour savoir le nombre de points potentiellement atypiques sur le violin plot pour nb_crea_entps
  #on compte le nombre d'observations supérieures au 9è decile
departements %>% filter(nb_crea_entps > (quantile(nb_crea_entps, probs = .9))) %>% count()  #10


# Test de Rosner quand plus d'1 point potentiellement atypique
library(EnvStats) 
    # nb_publi
rosnerTest(departements$nb_publi, k = 10, alpha = 0.05) #outlier quand publi ≥ 88 
    # taux_chomage
rosnerTest(departements$taux_chomage, k = 3, alpha = 0.05) #outliers quand taux ≥ 13.8%, càd Pyrénées Orientales
    # part_diplomes
rosnerTest(departements$part_diplomes, k = 6, alpha = 0.05) #outlier quand part ≥ 15.7%
    # depenses_hab
rosnerTest(departements$depenses_hab, k = 3, alpha = 0.05) #outlier quand dépenses ≥ 1770.413
    # part_etudiants
rosnerTest(departements$part_etudiants, k = 2, alpha = 0.05) #outlier quand part ≥ 16.5%
    # nb_etudiants
rosnerTest(departements$nb_etudiants, k = 8, alpha = 0.05) #outlier quand nombre ≥ 98938
    # pop_insee
rosnerTest(departements$pop_insee, k = 3, alpha = 0.05) #outlier quand nombre ≥ 2639070
    # niveau_vie
rosnerTest(departements$niveau_vie, k = 5, alpha = 0.05) #outlier quand niveau ≥ 26600
    # nb_crea_entps
rosnerTest(departements$nb_crea_entps, k = 10, alpha = 0.05) #outlier quand nombre ≥ 32480
    # nb_nuitees_hotels
rosnerTest(departements$nb_nuitees_hotels, k = 6, alpha = 0.05) #outlier quand nombre ≥ 6724


# Dans une nouvelle base on retire les observations atypiques
departements_sans_outliers <- departements %>% filter(nb_publi < 88,
                                                      taux_chomage < 13.8,
                                                      part_diplomes < 15.7,
                                                      depenses_hab < 1770.413,
                                                      part_etudiants < 16.5,
                                                      nb_etudiants < 98938,
                                                      pop_insee < 2639070,
                                                      niveau_vie < 26600,
                                                      nb_crea_entps < 32480,
                                                      nb_nuitees_hotels < 6724)

nrow(departements)-nrow(departements_sans_outliers) #22 obs perdues
outliers <- anti_join(departements, departements_sans_outliers)  # dep d'Ile de France x4, Pays de la Loire x3, Occitanie x3 etc.



#----------------------- Distributions avec et sans outliers


# Variable à expliquer
par(mfrow=c(1,2))
hist(departements$nb_publi, prob=T, ylim=c(0, 0.07), 
     xlab="Avec outliers", ylab="Densité", main="Nombre de publications open data")
lines(density(departements$nb_publi), col="red", lwd=2)
curve(dnorm(x, mean=mean(departements$nb_publi), sd=sd(departements$nb_publi)),col="#0033CC", lwd=2, add=TRUE, yaxt="n")
hist(departements_sans_outliers$nb_publi, prob=T, ylim=c(0, 0.07), 
     xlab="Sans outliers", ylab="Densité", main="Nombre de publications open data")
lines(density(departements_sans_outliers$nb_publi), col="red", lwd=2)
curve(dnorm(x, mean=mean(departements_sans_outliers$nb_publi), sd=sd(departements_sans_outliers$nb_publi)), 
      col="#0033CC", lwd=2, add=TRUE, yaxt="n")


# Variables explicatives
library(RColorBrewer)

par(mfrow=c(4,2))
hist(departements$taux_chomage, main="Taux de chômage", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Greens"))
hist(departements_sans_outliers$taux_chomage, main="Taux de chômage", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "Greens"))
hist(departements$part_plus65, main="Part des plus de 65 ans", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "PuRd"))
hist(departements_sans_outliers$part_plus65, main="Part des plus de 65 ans", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "PuRd"))
hist(departements$part_diplomes, main="Part des diplômés", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Blues"))
hist(departements_sans_outliers$part_diplomes, main="Part des diplômés", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Blues"))
hist(departements$depenses_hab, main="Dépenses par habitant", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Reds"))
hist(departements_sans_outliers$depenses_hab, main="Dépenses par habitant", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Reds"))

par(mfrow=c(4,2))
hist(departements$part_etudiants, main="Part des étudiants", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Greens"))
hist(departements_sans_outliers$part_etudiants, main="Part des étudiants", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "Greens"))
hist(departements$nb_etudiants, main="Nombre d'étudiants", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 4, name = "PuRd"))
hist(departements_sans_outliers$nb_etudiants, main="Nombre d'étudiants", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "PuRd"))
hist(departements$percent_pop_rurale, main="Pourcentage de population rurale", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Blues"))
hist(departements_sans_outliers$percent_pop_rurale, main="Pourcentage de population rurale", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Blues"))
hist(departements$pop_insee, main="Nombre d'habitants", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 6, name = "Reds"))
hist(departements_sans_outliers$pop_insee, main="Nombre d'habitants", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Reds"))

par(mfrow=c(4,2))
hist(departements$age_chef, main="Âge du chef", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Greens"))
hist(departements_sans_outliers$age_chef, main="Âge du chef", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Greens"))
hist(departements$niveau_vie, main="Niveau de vie", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 4, name = "PuRd"))
hist(departements_sans_outliers$niveau_vie, main="Niveau de vie", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "PuRd"))
hist(departements$nb_crea_entps, main="Créations d'entreprises", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Blues"))
hist(departements_sans_outliers$nb_crea_entps, main="Créations d'entreprises", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Blues"))
hist(departements$nb_nuitees_hotels, main="Nombre de nuitées en hotels", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 6, name = "Reds"))
hist(departements_sans_outliers$nb_nuitees_hotels, main="Nombre de nuitées en hotels", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Reds"))



#-----------------------  Corrélations et dépendances



# Corrélations (matrice de Spearman pour non loi normale) des variables quantis
library(corrplot)
cor1 <- cor(departements[,c("nb_publi","taux_chomage","part_plus65","part_diplomes","depenses_hab","part_etudiants","percent_pop_rurale","pop_insee","age_chef","niveau_vie","nb_crea_entps","nb_nuitees_hotels","nb_etudiants")], use="complete.obs", method=c("spearman"))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor1, method="color", col=col(200), 
             type="upper",
             addCoef.col = "black")
corrplot(cor1, type="upper", order="hclust", tl.col="black", tl.srt=45, addCoef.col = "black")

      # corrélations moyennes (0.5<x<0.6) :
          # - taux_chomage et niveau_vie
          # - part_plus65 et niveau_vie/nb_crea_entps/nb_etudiants
          # - part_diplomes et pop_insee/nb_etudiants
          # - depenses_hab et nb_crea_entps/nb_etudiants
          # - part_etudiants et nb_crea_entps
          # - niveau_vie et nb_nuitees_hotels
      # corrélations fortes (x≥0.6) :
          # - part_plus65 et depenses_hab/percent_pop_rurale/pop_insee
          # - part_diplomes et part_etudiants/percent_pop_rurale/niveau_vie/nb_crea_entps/nb_nuitees_hotels/nb_etudiants
          # - depenses_hab et pop_insee/niveau_vie
          # - part_etudiants et percent_pop_rurale/nb_nuitees_hotels/nb_etudiants
          # - percent_pop_rurale et pop_insee/nb_crea_entps/nb_nuitees_hotels/nb_etudiants
          # - pop_insee et nb_crea_entps/nb_nuitees_hotels/nb_etudiants
          # - nb_crea_entps et nb_nuitees_hotels/nb_etudiants
          # - nb_nuitees_hotels et nb_etudiants


# Dépendances des variables qualis
# ouvre_data avec les autres qualis
chisq.test(departements$ouvre_data, departements$niveau_rural_mode) #indépendant à 5%...
chisq.test(departements$ouvre_data, departements$niveau_rural_insee) #indépendant
chisq.test(departements$ouvre_data, departements$flux_migration_res) #indépendant
chisq.test(departements$ouvre_data, departements$partis_po_chef) #indépendant
chisq.test(departements$ouvre_data, departements$CSP_chef) #indépendant

# niveau_rural_mode
chisq.test(departements$niveau_rural_mode, departements$niveau_rural_insee) # dépendant
chisq.test(departements$niveau_rural_mode, departements$flux_migration_res) # dépendant
chisq.test(departements$niveau_rural_mode, departements$partis_po_chef) #indépendant
chisq.test(departements$niveau_rural_mode, departements$CSP_chef) #indépendant

# niveau_rural_insee
chisq.test(departements$niveau_rural_insee, departements$flux_migration_res) # dépendant
chisq.test(departements$niveau_rural_insee, departements$partis_po_chef) #indépendant
chisq.test(departements$niveau_rural_insee, departements$CSP_chef) #indépendant

# flux_migration_res
chisq.test(departements$flux_migration_res, departements$partis_po_chef) #indépendant
chisq.test(departements$flux_migration_res, departements$CSP_chef) #indépendant

# partis_po_chef
chisq.test(departements$partis_po_chef, departements$CSP_chef) #indépendant




                ### B) Relations des variables explicatives avec Y




    ## 1. VARIABLES QUANTI et QUALI





#----------------------- CART 


library(rpart)
library(caret)
library(rpart.plot)
library(randomForest)

# On définit les paramètres de contrôle
ctrl=rpart.control(cp=0.01, xval=5, maxdepth=3, minsplit = 5)
#tuneRF(x=departements, y = departements$nb_publi, mtryStart=3, ntreeTry = 500, stepFactor = 2, improve=0.001)  #mtry=31

# Arbre
arbre_nb.publi <- rpart(nb_publi ~ taux_chomage+CSP_chef+niveau_rural_mode+niveau_rural_insee+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+pop_insee+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef, data = departements, method="anova", control = )
summary(arbre_nb.publi)
# Plot
rpart.plot(arbre_nb.publi, box.palette = "Blues")

# Même chose mais sans Paris qui peut fausser les répartitions
arbre_nb.publi2 <- rpart(nb_publi ~ taux_chomage+CSP_chef+niveau_rural_mode+niveau_rural_insee+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+pop_insee+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef, data = test, method="anova")
summary(arbre_nb.publi2)
rpart.plot(arbre_nb.publi2, box.palette = "Blues")



#----------------------- ACP 


library(FactoMineR)
library(factoextra)
  # plot
res.pca_Y = PCA(departements_sans_outliers[,c("nb_publi","taux_chomage","part_plus65","part_diplomes","depenses_hab","part_etudiants","percent_pop_rurale","pop_insee","age_chef","niveau_vie","nb_crea_entps","nb_nuitees_hotels","nb_etudiants")], quanti.sup=1, graph=F)
fviz_pca_var(res.pca_Y, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

  # inertie de chaque axe fictif : % de la variance
round(res.pca_Y$eig,2)
fviz_eig(res.pca_Y, addlabels = TRUE) #axes 1 et 2 conservent 65% de l'info des 12 Xt

  # contributions des Xt aux axes 1 et 2
round(res.pca_Y$var$contrib,2)
fviz_contrib(res.pca_Y, choice = "var", axes = 1, col="black")  #axe 1 = dynamisme du département
fviz_contrib(res.pca_Y, choice = "var", axes = 2, col="black")  #axe 2 = manque d'activité (indirecte)
      # ajout CC

  # corrélations des Xt aux dimensions : voir relation po/neg entre vble et axe
corrplot(res.pca_Y$var$cor, is.corr=FALSE, method="circle", tl.srt=45, tl.col="#004400", col=brewer.pal(n=9, name="RdYlBu"), addCoef.col="black")  #plus % pop rurale augmente, moins la région est attractive

  # projections des départements sur le plan à 2 dimensions
fviz_pca_ind(res.pca_Y, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T) #noyau dur : bcp de dep avec peu d'attractivité (gauche du graph) et qualité de vie  autour de 0
fviz_pca_biplot(res.pca_Y, repel = TRUE,
                col.var = "#2E9FDF",
                col.ind = "red"
                )

# Projections des individus colorés en fonction du nombre de jeux ouverts
fviz_pca_ind(res.pca_Y, col.ind = departements_sans_outliers$nb_publi,
             gradient.cols = c("#FFCC00", "#FC4E07"),
             repel = T)



#----------------------- ACM


  # plot
res.mca <- MCA(departements[,c("niveau_rural_mode","niveau_rural_insee","partis_po_chef","CSP_chef")])
  # projection des variables
fviz_mca_var(res.mca, axe=c(1,2), invisible="ind",cex=0.8,autoLab="yes", jitter = list(what = "label", width = NULL, height = NULL))
  # projection des modalités
fviz_eig(res.mca,main="Pourcentage expliqué par chaque facteur")
  # contributions des variables aux axes
round(res.mca$var$coord,2)
fviz_contrib(res.mca, choice = "var", axes = 1, col="black")  #axe 1 = dynamisme du département
fviz_contrib(res.mca, choice = "var", axes = 2, col="black")  #axe 2 = manque d'activité (indirecte)
  # corrélations des Xt aux dimensions : voir relation po/neg entre vble et axe
corrplot(res.mca$var$coord, is.corr=FALSE, method="circle", tl.srt=45, tl.col="#004400", col=brewer.pal(n=9, name="RdYlBu"), addCoef.col="black")  #plus % pop rurale augmente, moins la région est attractive
  # projections des départements sur le plan à 2 dimensions
fviz_pca_ind(res.mca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T)



#----------------------- AFM 



# On remanie la base pour mettre les variables par groupes identifiés sur ACP et ACM
dep_reorder <- departements[,c(2,7,11:13,15,16,19,20,23,14,18,9,21,22,17,10,8)]

res.mfa <- MFA(dep_reorder, 
               group = c(1, 9, 2, 1, 2, 1, 2), 
               type = c("c","c","c","c","s","s","s"),
               name.group = c("Y","dynamisme","activité","age_chef", "urbanité","flux_migr","infos_chef"),
               graph = T)






    ## 2. VARIABLES QUALI



#----------------------- Diagrammes croisés



# Croisements Y (nb_publi) avec variables qualis
t1=tapply(departements$nb_publi, departements$niveau_rural_mode, mean)
t2=tapply(departements$nb_publi, departements$niveau_rural_insee, mean) %>% sort()
t3=tapply(departements$nb_publi, departements$flux_migration_res, mean) %>% sort()
t4=tapply(departements$nb_publi, departements$partis_po_chef, mean) %>% sort()
t5=tapply(departements$nb_publi, departements$CSP_chef, mean) %>% sort()


library(RColorBrewer)
barplot(t1, horiz=TRUE, xlim=c(0,230), legend = c("Urbain dense","Urbain densité intermédiaire","Rural sous forte influence d'un pôle","Rural sous faible influence d'un pôle","Rural autonome peu dense","Rural autonome très peu dense"), main="Nombre de jeux ouverts moyen selon le niveau de ruralité", col=rev(brewer.pal(n = 6, name = "Blues")), cex.names=.9, cex.main=.96, col.main="#0033CC")
table(departements$niveau_rural_mode)

barplot(t2, horiz=TRUE, xlim=c(0,80), legend.text = TRUE, args.legend = list(x = "bottomright", inset = c(.02, .05), legend = c("Dense","Peu dense","Très dense")), main="Nombre de jeux ouverts moyen selon le niveau de densité", col = brewer.pal(n = 3, name = "Reds"), cex.names=1, cex.main=1, col.main="#FF6600")
table(departements$niveau_rural_insee)

barplot(t3[36:44], horiz=TRUE, xlim=c(0,50), main="Top 9 des départements avec le plus grand nombre de jeux ouverts moyen, 
        selon le principal flux de migration résidentiel", col=brewer.pal(n = 9, name = "Greys"), cex.names=1, cex.main=1, col.main="black") #données pas fiables car pas assez d'obs par modalité (rappel : les numéros correspondent au COG du dep de destination des flux résidentiels)
table(departements$flux_migration_res)

barplot(t4, horiz=TRUE, xlim=c(0,55), main="Nombre de jeux ouverts moyen selon l'appartenance politique du chef de l'exécutif", col=brewer.pal(n = 3, name = "Purples"), cex.names=1, cex.main=1, col.main="#756BB1")
table(departements$partis_po_chef)

barplot(t5, horiz=TRUE, xlim=c(0,80), legend.text = TRUE, args.legend = list(x = "bottomright", inset = c(.02, .05), legend=c("Artisans, commerçants et chefs d'entreprise","Employés","Agriculteurs exploitants","Retraités","Autres personnes sans activité professionnelle","Cadres et professions intellectuelles supérieures","Professions Intermédiaires")), main="Nombre de jeux ouverts moyen selon la CSP du chef de l'exécutif", col=brewer.pal(n = 7, name = "Purples"), cex.names=1, cex.main=1, col.main="#756BB1") #Cat 2 sous représentées donc att aux conclus
table(departements$CSP_chef)



# Pour les flux migratoires on sélectionne les modalités avec plus de 3 obs pour que ce soit à peu près représentatif
dep_tri <- departements[departements$flux_migration_res %in%  names(table(departements$flux_migration_res))[table(departements$flux_migration_res) > 3] , ]
t3_bis=tapply(dep_tri$nb_publi, dep_tri$flux_migration_res, mean) %>% sort()

table(dep_tri$flux_migration_res)
barplot(t3_bis, horiz=TRUE, xlim=c(0,60), legend.text = TRUE, args.legend = list(x = "bottomright", inset = c(.02, .05), legend = c("Bouches du Rhônes","Haute Garonne","Marne")), main="Top 9 des départements avec le plus grand nombre de jeux ouverts moyen, 
        selon le principal flux de migration résidentiel", col=brewer.pal(n = 5, name = "Greys"), cex.names=1, cex.main=1, col.main="black")




#----------------------- Treemap


# On récupère les noms des régions
infos_reg <- read_csv("Data/raw/infos_regions.csv") %>% rename(region = nom)
departements <- left_join(departements, infos_reg[,c(1,3)], by = c("code_region" = "COG"))

# Data pour savoir les départements de quelles régions ouvrent le plus de données
t6=tapply(departements$nb_publi, departements$region, mean) 
t6 <- t6 %>% as.data.frame() %>% mutate(nom = rownames(t6))
t6$label <- paste(t6$nom, "-", round(t6$.,0), "publications", sep=" ")

# Plot
library(treemap)
treemap(t6, 
        index="label", 
        vSize=".", 
        type="index",                            
        palette = "Pastel2",                      
        title = "Nombre de jeux ouverts moyen selon la région",
        fontsize.title=17,
        fontcolor.labels = "black", 
        fontface.labels="plain",
        fontsize.labels=14,
        lowerbound.cex.labels = 0.4,
      )

table(departements$code_region)




#----------------------- Boxplots croisés


ggplot(departements, aes(x=niveau_rural_mode, y=nb_publi, fill=niveau_rural_mode)) + 
  geom_boxplot()+
  geom_point() +
  labs(title="Box du nombre de publications par niveau de ruralité", y = "Nombre de jeux ouverts", x = "") + 
  scale_fill_brewer(name="Niveau de ruralité", palette="Blues") +
  guides(fill = FALSE) +
  theme_classic() #plus il y a d'obs plus la moyenne peut être tirée vers le bas
ggplot(departements, aes(x=niveau_rural_insee, y=nb_publi, fill=niveau_rural_insee)) + 
  geom_boxplot()+
  geom_point() +
  labs(title="Box du nombre de publications par niveau de densité", y = "Nombre de jeux ouverts", x = "") + 
  scale_fill_brewer(name="Niveau de densité", palette="Blues")  +
  guides(fill = FALSE) +
  theme_classic() # cat 2 et 3 vraiment comparables
ggplot(departements, aes(x=partis_po_chef, y=nb_publi, fill=partis_po_chef)) + 
  geom_boxplot()+
  geom_point() +
  labs(title="Box du nombre de publications par couleur politique", y = "Nombre de jeux ouverts", x = "") + 
  scale_fill_brewer(name="Couleur politique", palette="Blues") +
  guides(fill = FALSE) +
  theme_classic()
ggplot(departements, aes(x=CSP_chef, y=nb_publi, fill=CSP_chef)) + 
  geom_boxplot()+
  geom_point() +
  labs(title="Box du nombre de publications par CSP du chef", y = "Nombre de jeux ouverts", x = "") + 
  scale_fill_brewer(name="CSP du chef", palette="Blues") +
  guides(fill = FALSE) +
  theme_classic()  #3 et 7 vraiment comparables (cadres / retraités)



#----------------------- Stacked barchart


  # Répartition de la variable binaire : ouvre des données ou non 
tY <- as.data.frame(table(departements$ouvre_data))
ggplot(tY, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(y=Freq/2, label=Freq), vjust=1.6, color="white", size=5)+
  labs(title="Répartition des départements selon qu'ils ouvrent ou non des données", y = "Nombre de jeux ouverts", x = "") + 
  theme_minimal()



  # Niveau de ruralité et ouvre_data
t7 <- departements %>% group_by(ouvre_data) %>% count(niveau_rural_insee)
  # plot
library(viridis)
library(hrbrthemes)
library(plotly)
ggp <- ggplot(t7, aes(fill=ouvre_data, y=n, x=niveau_rural_insee,  
                text = paste("Ouvre des données", ouvre_data, 
                             "\nNiveau de ruralité", niveau_rural_insee,
                              "\n", n,"départements"), group=niveau_rural_insee)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T, name="Ouvre des données?", option="E", direction=-1) +
    theme_ipsum() +
    xlab("") + ylab("Nombre de départements") + ggtitle("Ouverture de données selon le niveau de ruralité") +
    theme(axis.title.y = element_text(size=12))
ggplotly(ggp, tooltip=c("text"))


  # partis_po_chef et ouvre_data
t8 <- departements %>% group_by(ouvre_data) %>% count(partis_po_chef)
  # plot
ggp2 <- ggplot(t8, aes(fill=ouvre_data, y=n, x=partis_po_chef,  
                text = paste("Ouvre des données", ouvre_data, 
                             "\nCouleur politique :", partis_po_chef,
                              "\n", n,"départements"), group=partis_po_chef)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T, name="Ouvre des données?", option="E", direction=-1) +
    theme_ipsum() +
    xlab("") + ylab("Nombre de départements") + ggtitle("Ouverture de données selon la couleur politique") +
    theme(axis.title.y = element_text(size=12))
ggplotly(ggp2, tooltip=c("text"))


  # CSP_chef et ouvre_data
t9 <- departements %>% group_by(ouvre_data) %>% count(CSP_chef)
  # plot
ggp3 <- ggplot(t9, aes(fill=ouvre_data, y=n, x=CSP_chef,  
                text = paste("Ouvre des données", ouvre_data, 
                             "\nCSP du chef :", CSP_chef,
                              "\n", n,"départements"), group=ouvre_data)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T, name="Ouvre des données?", option="E", direction=-1) +
    theme_ipsum() +
    xlab("") + ylab("Nombre de départements") + ggtitle("Ouverture de données selon la CSP du chef") +
    theme(axis.title.y = element_text(size=12))
ggplotly(ggp3, tooltip=c("text"))





    ## 3. VARIABLES QUANTI




#----------------------- Nuages de points


ggplot(data = departements, mapping=aes(taux_chomage, nb_publi)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi)) + 
  geom_point(mapping=aes(part_plus65, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_diplomes, nb_publi)) + 
  geom_point(mapping=aes(part_diplomes, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(depenses_hab, nb_publi)) + 
  geom_point(mapping=aes(depenses_hab, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_etudiants, nb_publi)) + 
  geom_point(mapping=aes(part_etudiants, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(nb_etudiants, nb_publi)) + 
  geom_point(mapping=aes(nb_etudiants, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(percent_pop_rurale, nb_publi)) + 
  geom_point(mapping=aes(percent_pop_rurale, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(pop_insee, nb_publi)) + 
  geom_point(mapping=aes(pop_insee, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(age_chef, nb_publi)) + 
  geom_point(mapping=aes(age_chef, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(niveau_vie, nb_publi)) + 
  geom_point(mapping=aes(niveau_vie, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(nb_crea_entps, nb_publi)) + 
  geom_point(mapping=aes(nb_crea_entps, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(nb_nuitees_hotels, nb_publi)) + 
  geom_point(mapping=aes(nb_nuitees_hotels, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()




#----------------------- Identification de sous populations


# On essaye en colorant avec les variables catégorielles existantes

    # niveau_rural_mode
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi, col=niveau_rural_mode)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi, col=niveau_rural_mode)) + 
  geom_point(mapping=aes(part_plus65, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
    # niveau_rural_insee
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi, col=niveau_rural_insee)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi, col=niveau_rural_insee)) + 
  geom_point(mapping=aes(part_plus65, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
    # flux_migration_res
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi, col=flux_migration_res)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi, col=flux_migration_res)) + 
  geom_point(mapping=aes(part_plus65, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
    # partis_po_chef
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi, col=partis_po_chef)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi, col=partis_po_chef)) + 
  geom_point(mapping=aes(part_plus65, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
    # CSP_chef
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi, col=CSP_chef)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi, col=CSP_chef)) + 
  geom_point(mapping=aes(part_plus65, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()


# On créé des variables catégorielles à partir de l'analyse exploratoire
     #depuis ACP
departements$dynamisme <- res.pca_Y$ind$coord[,1]
departements$inactivite <- res.pca_Y$ind$coord[,2]
     #depuis ACM
departements$densite <- res.mca$ind$coord[,1]
departements$vie_chill <- res.mca$ind$coord[,2]
     #depuis CART
departements <- departements %>% mutate(is_big_nb_etu = case_when(nb_etudiants >= 56525 ~ 1,
                                                                  nb_etudiants < 56525 ~ 0))
departements <- departements %>% mutate(feuilles_CART = case_when(nb_etudiants >= 56525 ~ 4,
                                                                  nb_etudiants < 56525 & depenses_hab >=1102 ~ 1,
                                                                  nb_etudiants < 56525 & depenses_hab < 1102 & part_etudiants >=1.7 ~ 2,
                                                                  nb_etudiants < 56525 & depenses_hab < 1102 & part_etudiants < 1.7 ~ 3
                                                                  ))
     #depuis violin plots
departements <- departements %>% mutate(is_extreme = case_when(taux_chomage < 13.8 & part_diplomes < 15.7 & depenses_hab < 1770.413 & part_etudiants < 16.5 & nb_etudiants < 98938 & pop_insee < 2639070 & niveau_vie < 26600 & nb_crea_entps < 32480 & nb_nuitees_hotels < 6724 ~ 0,
                                                               TRUE ~ 1))



# on essaye les colorations des nuages de points
    # dynamisme
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi, col=dynamisme)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi, col=dynamisme)) + 
  geom_point(mapping=aes(part_plus65, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
    # inactivite
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi, col=inactivite)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi, col=inactivite)) + 
  geom_point(mapping=aes(part_plus65, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
    # densite
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi, col=densite)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi, col=densite)) + 
  geom_point(mapping=aes(part_plus65, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
    # vie_chill
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi, col=vie_chill)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi, col=vie_chill)) + 
  geom_point(mapping=aes(part_plus65, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
    # is_big_nb_etu
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi, col=is_big_nb_etu)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi, col=is_big_nb_etu)) + 
  geom_point(mapping=aes(part_plus65, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
    # feuilles_CART
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi, col=feuilles_CART)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi, col=feuilles_CART)) + 
  geom_point(mapping=aes(part_plus65, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
    # is_extreme
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi, col=is_extreme)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi, col=is_extreme)) + 
  geom_point(mapping=aes(part_plus65, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()



# On essaye en parametrant la taille des points en fonction d'une quanti

    # pop_insee
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi, size=pop_insee), col="#666666") + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi)) + 
  geom_point(mapping=aes(part_plus65, nb_publi, size=pop_insee), col="#666666") + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
    # nb_etudiants
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi, size=nb_etudiants), col="#666666") + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi)) + 
  geom_point(mapping=aes(part_plus65, nb_publi, size=nb_etudiants), col="#666666") + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
    # nb_crea_entps
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi, size=nb_crea_entps), col="#666666") + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi)) + 
  geom_point(mapping=aes(part_plus65, nb_publi, size=nb_crea_entps), col="#666666") + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
    # age_chef
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi, size=age_chef), col="#666666") + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi)) + 
  geom_point(mapping=aes(part_plus65, nb_publi, size=age_chef), col="#666666") + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
    # niveau_vie
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi, size=niveau_vie), col="#666666") + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi)) + 
  geom_point(mapping=aes(part_plus65, nb_publi, size=niveau_vie), col="#666666") + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
    # part_diplomes
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi, size=part_diplomes), col="#666666") + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi)) + 
  geom_point(mapping=aes(part_plus65, nb_publi, size=part_diplomes), col="#666666") + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
    # percent_pop_rurale
ggplot(data = departements, mapping=aes(taux_chomage, nb_publi)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi, size=percent_pop_rurale), col="#666666") + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements, mapping=aes(part_plus65, nb_publi)) + 
  geom_point(mapping=aes(part_plus65, nb_publi, size=percent_pop_rurale), col="#666666") + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()

           


     
          ### C) Relations entre les variables explicatives





#-----------------------  Dendrogramme


dep_rownames <- departements
rownames(dep_rownames) = departements$nom

dep_rownames %>%  select(nb_publi,nb_ptf,nb_datagouv,taux_chomage,part_plus65,part_diplomes,depenses_hab,part_etudiants,percent_pop_rurale,pop_insee,age_chef,niveau_vie,nb_crea_entps,nb_nuitees_hotels,nb_etudiants) %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram() -> dend
 
# Plot
par(mar=c(7,3,1,1))  # Increase bottom margin to have the complete label
plot(dend)

library(dendextend)
par(mar=c(1,1,1,7))
dend %>%
  set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
  plot(horiz=TRUE, axes=FALSE)
abline(v = 350, lty = 2)





rio::export(departements, "./Data/process/dep_analyse_explo.csv")



