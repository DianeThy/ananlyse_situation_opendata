
###############################################################################################################################

################################################ ANALYSE DEPARTEMENTS #########################################################

###############################################################################################################################

library(tidyverse)

# On importe la base des départements
departements <- read_csv("Data/process/departements.csv")

# Les variables :
vbles_quanti <- c("nb_publi","nb_ptf","nb_datagouv","taux_chomage","part_plus65","part_diplomes","depenses_hab","part_etudiants","percent_pop_rurale","pop_insee","age_chef","niveau_vie","nb_crea_entps","nb_nuitees_hotels","nb_etudiants")
vbles_quali <- c("nom","ouvre_data","niveau_rural_mode","niveau_rural_insee","flux_migration_res","partis_po_chef","CSP_chef")





                ### A) Variables une à une



    ## 1. VARIABLES QUANTI



#----------------------- Statistiques descriptives

# Lsq il y a plusieurs partis po pour 1 chef, l'observation est dupliquée. Pour ne pas fausser les stats desc des variables quantitatives on retire la colonne 'partis_po_chef' et on supprime les doublons
departements_unique <- departements %>% select(-partis_po_chef) %>% unique()

# Statistiques
library(summarytools)
view(dfSummary(departements_unique))



#----------------------- Points atypiques 


# Distrib nb_publi par type
ggplot(departements_unique, aes(x=nb_publi, y=frequency(nb_publi))) + #10 potentiels outliers 
  labs(title="Distribution et box du nombre de publications open data", x="Nombe de publications", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements_unique, aes(x=taux_chomage, y=frequency(taux_chomage))) +  #3
  labs(title="Distribution et box du taux de chômage", x="Taux de chômage", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements_unique, aes(x=part_plus65, y=frequency(part_plus65))) +  
  labs(title="Distribution et box de la part des plus de 65 ans", x="Part des plus de 65 ans", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements_unique, aes(x=part_diplomes, y=frequency(part_diplomes))) +  #6
  labs(title="Distribution et box de la part des diplômés dans la population", x="Part des diplômés", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements_unique, aes(x=depenses_hab, y=frequency(depenses_hab))) +  #3
  labs(title="Distribution et box des dépenses par habitant", x="Dépenses par habitant", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements_unique, aes(x=part_etudiants, y=frequency(part_etudiants))) +  #2
  labs(title="Distribution et box de la part des étudiants dans la population", x="Part des étudiants", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements_unique, aes(x=nb_etudiants, y=frequency(nb_etudiants))) +  #8
  labs(title="Distribution et box du nombre d'étudiants", x="Nombre d'étudiants", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements_unique, aes(x=percent_pop_rurale, y=frequency(percent_pop_rurale))) +  
  labs(title="Distribution et box de la part de le population vivant en zone rurale", x="Part de population", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements_unique, aes(x=pop_insee, y=frequency(pop_insee))) +  #3
  labs(title="Distribution et box du nombre d'habitants", x="Nombre d'habitants", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements_unique, aes(x=age_chef, y=frequency(age_chef))) +  
  labs(title="Distribution et box de l'âge du chef de l'exécutif", x="Âge du chef", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements_unique, aes(x=niveau_vie, y=frequency(niveau_vie))) +  #5
  labs(title="Distribution et box de la médiane du niveau de vie", x="Médiane du niveau de vie", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements_unique, aes(x=nb_crea_entps, y=frequency(nb_crea_entps))) +  #? 
  labs(title="Distribution et box du nombre de créations d'entreprises", x="Créations d'entreprises", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements_unique, aes(x=nb_nuitees_hotels, y=frequency(nb_nuitees_hotels))) +  #6
  labs(title="Distribution et box du nombre de nuitées recensées dans des hôtels de tourisme", x="Nombre de nuitées", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()


# Pour savoir le nombre de points potentiellement atypiques sur le violin plot pour nb_crea_entps
  #on compte le nombre d'observations supérieures au 9è decile
departements_unique %>% filter(nb_crea_entps > (quantile(nb_crea_entps, probs = .9))) %>% count()  #10


# Test de Rosner quand plus d'1 point potentiellement atypique
library(EnvStats) 
    # nb_publi
rosnerTest(departements_unique$nb_publi, k = 10, alpha = 0.05) #outlier quand publi ≥ 88 
    # taux_chomage
rosnerTest(departements_unique$taux_chomage, k = 3, alpha = 0.05) #outliers quand taux ≥ 13.8%, càd Pyrénées Orientales
    # part_diplomes
rosnerTest(departements_unique$part_diplomes, k = 6, alpha = 0.05) #outlier quand part ≥ 15.7%
    # depenses_hab
rosnerTest(departements_unique$depenses_hab, k = 3, alpha = 0.05) #outlier quand dépenses ≥ 1770.413
    # part_etudiants
rosnerTest(departements_unique$part_etudiants, k = 2, alpha = 0.05) #outlier quand part ≥ 16.5%
    # nb_etudiants
rosnerTest(departements_unique$nb_etudiants, k = 8, alpha = 0.05) #outlier quand nombre ≥ 98938
    # pop_insee
rosnerTest(departements_unique$pop_insee, k = 3, alpha = 0.05) #outlier quand nombre ≥ 2639070
    # niveau_vie
rosnerTest(departements_unique$niveau_vie, k = 5, alpha = 0.05) #outlier quand niveau ≥ 26600
    # nb_crea_entps
rosnerTest(departements_unique$nb_crea_entps, k = 10, alpha = 0.05) #outlier quand nombre ≥ 32480
    # nb_nuitees_hotels
rosnerTest(departements_unique$nb_nuitees_hotels, k = 6, alpha = 0.05) #outlier quand nombre ≥ 6724


# Dans une nouvelle base on retire les observations atypiques
departements_unique_sans_outliers <- departements_unique %>% filter(nb_publi < 88,
                                                                    taux_chomage < 13.8,
                                                                    part_diplomes < 15.7,
                                                                    depenses_hab < 1770.413,
                                                                    part_etudiants < 16.5,
                                                                    nb_etudiants < 98938,
                                                                    pop_insee < 2639070,
                                                                    niveau_vie < 26600,
                                                                    nb_crea_entps < 32480,
                                                                    nb_nuitees_hotels < 6724)

# Même chose pour la base avec les doublons liés aux partis politiques
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

nrow(departements_unique)-nrow(departements_unique_sans_outliers) #22 obs perdues



#----------------------- Distributions


# Variable à expliquer
par(mfrow=c(1,2))
hist(departements_unique$nb_publi, prob=T, ylim=c(0, 0.07), 
     xlab="Avec outliers", ylab="Densité", main="Nombre de publications open data")
lines(density(departements_unique$nb_publi), col="red", lwd=2)
curve(dnorm(x, mean=mean(departements_unique$nb_publi), sd=sd(departements_unique$nb_publi)),col="#0033CC", lwd=2, add=TRUE, yaxt="n")
hist(departements_unique_sans_outliers$nb_publi, prob=T, ylim=c(0, 0.07), 
     xlab="Sans outliers", ylab="Densité", main="Nombre de publications open data")
lines(density(departements_unique_sans_outliers$nb_publi), col="red", lwd=2)
curve(dnorm(x, mean=mean(departements_unique_sans_outliers$nb_publi), sd=sd(departements_unique_sans_outliers$nb_publi)), 
      col="#0033CC", lwd=2, add=TRUE, yaxt="n")


# Variables explicatives
library(RColorBrewer)

par(mfrow=c(4,2))
hist(departements_unique$taux_chomage, main="Taux de chômage", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Greens"))
hist(departements_unique_sans_outliers$taux_chomage, main="Taux de chômage", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "Greens"))
hist(departements_unique$part_plus65, main="Part des plus de 65 ans", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "PuRd"))
hist(departements_unique_sans_outliers$part_plus65, main="Part des plus de 65 ans", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "PuRd"))
hist(departements_unique$part_diplomes, main="Part des diplômés", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Blues"))
hist(departements_unique_sans_outliers$part_diplomes, main="Part des diplômés", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Blues"))
hist(departements_unique$depenses_hab, main="Dépenses par habitant", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Reds"))
hist(departements_unique_sans_outliers$depenses_hab, main="Dépenses par habitant", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Reds"))

par(mfrow=c(4,2))
hist(departements_unique$part_etudiants, main="Part des étudiants", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Greens"))
hist(departements_unique_sans_outliers$part_etudiants, main="Part des étudiants", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "Greens"))
hist(departements_unique$nb_etudiants, main="Nombre d'étudiants", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 4, name = "PuRd"))
hist(departements_unique_sans_outliers$nb_etudiants, main="Nombre d'étudiants", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "PuRd"))
hist(departements_unique$percent_pop_rurale, main="Pourcentage de population rurale", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 6, name = "Blues"))
hist(departements_unique_sans_outliers$percent_pop_rurale, main="Pourcentage de population rurale", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Blues"))
hist(departements_unique$pop_insee, main="Nombre d'habitants", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 6, name = "Reds"))
hist(departements_unique_sans_outliers$pop_insee, main="Nombre d'habitants", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Reds"))

par(mfrow=c(4,2))
hist(departements_unique$age_chef, main="Âge du chef", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Greens"))
hist(departements_unique_sans_outliers$age_chef, main="Âge du chef", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "Greens"))
hist(departements_unique$niveau_vie, main="Niveau de vie", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 4, name = "PuRd"))
hist(departements_unique_sans_outliers$niveau_vie, main="Niveau de vie", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "PuRd"))
hist(departements_unique$nb_crea_entps, main="Créations d'entreprises", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 6, name = "Blues"))
hist(departements_unique_sans_outliers$nb_crea_entps, main="Créations d'entreprises", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Blues"))
hist(departements_unique$nb_nuitees_hotels, main="Nombre de nuitées en hotels", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 6, name = "Reds"))
hist(departements_unique_sans_outliers$nb_nuitees_hotels, main="Nombre de nuitées en hotels", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Reds"))



#-----------------------  Corrélations



# Matrice de corrélation de Spearman car variables ne suivent pas toutes une loi normale (on exclu les Y)
library(corrplot)
cor1 <- cor(departements_unique_sans_outliers[,c("taux_chomage","part_plus65","part_diplomes","depenses_hab","part_etudiants","percent_pop_rurale","pop_insee","age_chef","niveau_vie","nb_crea_entps","nb_nuitees_hotels","nb_etudiants")], use="complete.obs", method=c("spearman"))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor1, method="color", col=col(200), 
             type="upper",
             addCoef.col = "black")
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




    ## 2. VARIABLES QUALI



# On met au bon format les variables qualis
departements[,c("ouvre_data","niveau_rural_mode","niveau_rural_insee","flux_migration_res","partis_po_chef","CSP_chef")] <- lapply(departements[,c("ouvre_data","niveau_rural_mode","niveau_rural_insee","flux_migration_res","partis_po_chef","CSP_chef")], as.factor)
departements_unique[,c("ouvre_data","niveau_rural_mode","niveau_rural_insee","flux_migration_res","CSP_chef")] <- lapply(departements_unique[,c("ouvre_data","niveau_rural_mode","niveau_rural_insee","flux_migration_res","CSP_chef")], as.factor)
departements_sans_outliers[,c("ouvre_data","niveau_rural_mode","niveau_rural_insee","flux_migration_res","partis_po_chef","CSP_chef")] <- lapply(departements_sans_outliers[,c("ouvre_data","niveau_rural_mode","niveau_rural_insee","flux_migration_res","partis_po_chef","CSP_chef")], as.factor)
departements_unique_sans_outliers[,c("ouvre_data","niveau_rural_mode","niveau_rural_insee","flux_migration_res","CSP_chef")] <- lapply(departements_unique_sans_outliers[,c("ouvre_data","niveau_rural_mode","niveau_rural_insee","flux_migration_res","CSP_chef")], as.factor)




#-----------------------  Répartitions des catégories


library(inspectdf)
  # pour toutes les variable son prend la base unique
inspect_cat(departements_unique_sans_outliers[,c("ouvre_data","niveau_rural_mode","niveau_rural_insee","flux_migration_res","CSP_chef")]) %>% show_plot(high_cardinality = 1)  #données initiales pour voir répartition partis_po
  # pour la répartition dans les modalités de la couleur politique
inspect_cat(departements_sans_outliers[,c("partis_po_chef")]) %>% show_plot(high_cardinality = 1)  # PS et LR



#----------------------- Harmonisation couleur politique


# On regarde tous les partis po de la base
as.data.frame(table(departements_sans_outliers$partis_po_chef)) %>% arrange(desc(Freq))

# On rassemble les partis en 3 groupes : droite, gauche et sans étiquette
departements_sans_outliers <- departements_sans_outliers %>% mutate(partis_po_chef = 
                                                                      case_when(partis_po_chef == "Parti socialiste" ~ "Gauche",
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
departements_sans_outliers <- departements_sans_outliers %>% unique()

# Même chose pour les données avec outliers (base initiale)
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
departements <- departements %>% unique()



#----------------------- Harmonisation CSP du chef de l'exécutif



# On regarde tous les partis po de la base
as.data.frame(table(departements_sans_outliers$CSP_chef)) %>% arrange(desc(Freq))

# 30 valeurs différentes donc on harmonise grâce au fichier détaillé des CSP de l'INSEE
departements_sans_outliers <- departements_sans_outliers %>% mutate(CSP_chef = case_when(CSP_chef == "Fonctionnaires de catégorie A" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Permanents politiques" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Autres professions" ~ "Autres personnes sans activité professionnelle",
                                                             CSP_chef == "Retraités de l'enseignement" ~ "Retraités",
                                                             CSP_chef == "Retraités des professions libérales" ~ "Retraités",
                                                             CSP_chef == "Retraités fonct.publique (sf enseig.)" ~ "Retraités",
                                                             CSP_chef == "Autres cadres (secteur privé)" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Pharmaciens" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Retraités agricoles" ~ "Retraités",
                                                             CSP_chef == "Retraités salariés privés" ~ "Retraités",
                                                             CSP_chef == "Sans profession déclarée" ~ "Autres personnes sans activité professionnelle",
                                                             CSP_chef == "Agriculteurs propriétaires exploit" ~ "Agriculteurs exploitants",
                                                             CSP_chef == "Autres professions libérales" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Avocats" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Enseignants 1er deg.-directeurs école" ~ "Professions Intermédiaires",
                                                             CSP_chef == "Grands corps de l'état" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Industriels-Chefs entreprise" ~ "Artisans, commerçants et chefs d'entreprise",
                                                             CSP_chef == "Ingénieurs" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Professeurs du secondaire et techn." ~ "Professions Intermédiaires",
                                                             CSP_chef == "Professions rattachées à enseignt." ~ "Professions Intermédiaires",
                                                             CSP_chef == "Retr.artis.commerc.chefs d'entrep." ~ "Retraités",
                                                             CSP_chef == "Agents technique et techniciens" ~ "Professions Intermédiaires",
                                                             CSP_chef == "Autres retraités" ~ "Retraités",
                                                             CSP_chef == "Cadres supérieurs (secteur privé)" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Employés (secteur privé)" ~ "Employés",
                                                             CSP_chef == "Journalistes et autres médias" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Professeurs de faculté" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Représentants de commerce" ~ "Professions Intermédiaires",
                                                             CSP_chef == "Retraités des entreprises publiques" ~ "Retraités",
                                                             CSP_chef == "Vétérinaires" ~ "Cadres et professions intellectuelles supérieures"
                                                             ))
# Même chose base avec outliers
departements <- departements %>% mutate(CSP_chef = case_when(CSP_chef == "Fonctionnaires de catégorie A" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Permanents politiques" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Autres professions" ~ "Autres personnes sans activité professionnelle",
                                                             CSP_chef == "Retraités de l'enseignement" ~ "Retraités",
                                                             CSP_chef == "Retraités des professions libérales" ~ "Retraités",
                                                             CSP_chef == "Retraités fonct.publique (sf enseig.)" ~ "Retraités",
                                                             CSP_chef == "Autres cadres (secteur privé)" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Pharmaciens" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Retraités agricoles" ~ "Retraités",
                                                             CSP_chef == "Retraités salariés privés" ~ "Retraités",
                                                             CSP_chef == "Sans profession déclarée" ~ "Autres personnes sans activité professionnelle",
                                                             CSP_chef == "Agriculteurs propriétaires exploit." ~ "Agriculteurs exploitants",
                                                             CSP_chef == "Autres professions libérales" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Avocats" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Enseignants 1er deg.-directeurs école" ~ "Professions Intermédiaires",
                                                             CSP_chef == "Grands corps de l'état" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Industriels-Chefs entreprise" ~ "Artisans, commerçants et chefs d'entreprise",
                                                             CSP_chef == "Ingénieurs" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Professeurs du secondaire et techn." ~ "Professions Intermédiaires",
                                                             CSP_chef == "Professions rattachées à enseignt." ~ "Professions Intermédiaires",
                                                             CSP_chef == "Retr.artis.commerc.chefs d'entrep." ~ "Retraités",
                                                             CSP_chef == "Agents technique et techniciens" ~ "Professions Intermédiaires",
                                                             CSP_chef == "Autres retraités" ~ "Retraités",
                                                             CSP_chef == "Cadres supérieurs (secteur privé)" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Employés (secteur privé)" ~ "Employés",
                                                             CSP_chef == "Journalistes et autres médias" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Professeurs de faculté" ~ "Cadres et professions intellectuelles supérieures",
                                                             CSP_chef == "Représentants de commerce" ~ "Professions Intermédiaires",
                                                             CSP_chef == "Retraités des entreprises publiques" ~ "Retraités",
                                                             CSP_chef == "Vétérinaires" ~ "Cadres et professions intellectuelles supérieures"
                                                             ))


#----------------------- Dépendances


# ouvre_data avec les autres qualis
chisq.test(departements_sans_outliers$ouvre_data, departements_sans_outliers$niveau_rural_mode) #indépendant à 5%...
chisq.test(departements_sans_outliers$ouvre_data, departements_sans_outliers$niveau_rural_insee) #indépendant
chisq.test(departements_sans_outliers$ouvre_data, departements_sans_outliers$flux_migration_res) #indépendant
chisq.test(departements_sans_outliers$ouvre_data, departements_sans_outliers$partis_po_chef) #indépendant
chisq.test(departements_sans_outliers$ouvre_data, departements_sans_outliers$CSP_chef) #indépendant

# niveau_rural_mode
chisq.test(departements_sans_outliers$niveau_rural_mode, departements_sans_outliers$niveau_rural_insee) #dépendant
chisq.test(departements_sans_outliers$niveau_rural_mode, departements_sans_outliers$flux_migration_res) #dépendant
chisq.test(departements_sans_outliers$niveau_rural_mode, departements_sans_outliers$partis_po_chef) #indépendant
chisq.test(departements_sans_outliers$niveau_rural_mode, departements_sans_outliers$CSP_chef) #indépendant

# niveau_rural_insee
chisq.test(departements_sans_outliers$niveau_rural_insee, departements_sans_outliers$flux_migration_res) #indépendant à 5%...
chisq.test(departements_sans_outliers$niveau_rural_insee, departements_sans_outliers$partis_po_chef) #indépendant à 5%...
chisq.test(departements_sans_outliers$niveau_rural_insee, departements_sans_outliers$CSP_chef)

# flux_migration_res
chisq.test(departements_sans_outliers$flux_migration_res, departements_sans_outliers$partis_po_chef) #indépendant
chisq.test(departements_sans_outliers$flux_migration_res, departements_sans_outliers$CSP_chef) #indépendant

# partis_po_chef
chisq.test(departements_sans_outliers$partis_po_chef, departements_sans_outliers$CSP_chef) #dépendant





                ### B) Relations des variables explicatives avec Y




    ## 1. VARIABLES QUANTI



#----------------------- Nuages de points


ggplot(data = departements_sans_outliers, mapping=aes(taux_chomage, nb_publi)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements_sans_outliers, mapping=aes(part_plus65, nb_publi)) + 
  geom_point(mapping=aes(part_plus65, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements_sans_outliers, mapping=aes(part_diplomes, nb_publi)) + 
  geom_point(mapping=aes(part_diplomes, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements_sans_outliers, mapping=aes(depenses_hab, nb_publi)) + 
  geom_point(mapping=aes(depenses_hab, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements_sans_outliers, mapping=aes(part_etudiants, nb_publi)) + 
  geom_point(mapping=aes(part_etudiants, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements_sans_outliers, mapping=aes(percent_pop_rurale, nb_publi)) + 
  geom_point(mapping=aes(percent_pop_rurale, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements_sans_outliers, mapping=aes(pop_insee, nb_publi)) + 
  geom_point(mapping=aes(pop_insee, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements_sans_outliers, mapping=aes(age_chef, nb_publi)) + 
  geom_point(mapping=aes(age_chef, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements_sans_outliers, mapping=aes(niveau_vie, nb_publi)) + 
  geom_point(mapping=aes(niveau_vie, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements_sans_outliers, mapping=aes(nb_crea_entps, nb_publi)) + 
  geom_point(mapping=aes(nb_crea_entps, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements_sans_outliers, mapping=aes(nb_nuitees_hotels, nb_publi)) + 
  geom_point(mapping=aes(nb_nuitees_hotels, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = departements_sans_outliers, mapping=aes(nb_etudiants, nb_publi)) + 
  geom_point(mapping=aes(nb_etudiants, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()



                ### C) Relations entre les variables explicatives





#----------------------- ACP 


library(FactoMineR)
library(factoextra)
  # plot
res.pca = PCA(departements_unique_sans_outliers[,c("taux_chomage","part_plus65","part_diplomes","depenses_hab","part_etudiants","percent_pop_rurale","pop_insee","age_chef","niveau_vie","nb_crea_entps","nb_nuitees_hotels","nb_etudiants")], graph=F)
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

  # inertie de chaque axe fictif : % de la variance
round(res.pca$eig,2)
fviz_eig(res.pca, addlabels = TRUE) #axes 1 et 2 conservent 65% de l'info des 12 Xt

  # contributions des Xt aux axes 1 et 2
round(res.pca$var$contrib,2)
fviz_contrib(res.pca, choice = "var", axes = 1, col="black")  #axe 1 = dynamisme du département
fviz_contrib(res.pca, choice = "var", axes = 2, col="black")  #axe 2 = manque d'activité (indirecte)
      # ajout CC

  # corrélations des Xt aux dimensions : voir relation po/neg entre vble et axe
corrplot(res.pca$var$cor, is.corr=FALSE, method="circle", tl.srt=45, tl.col="#004400", col=brewer.pal(n=9, name="RdYlBu"),
addCoef.col="black")  #plus % pop rurale augmente, moins la région est attractive

  # projection de Y
res.pca1 = PCA(departements_unique_sans_outliers[,c("nb_publi","taux_chomage","part_plus65","part_diplomes","depenses_hab","part_etudiants","percent_pop_rurale","pop_insee","age_chef","niveau_vie","nb_crea_entps","nb_nuitees_hotels","nb_etudiants")], quanti.sup=1, graph=F)
fviz_pca_var (res.pca1, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) #permet de voir corrélations avec Xt : plus la région est dynamique plus elle a de chances d'ouvrir data, mais pas forcément une bonne qualité de vie (partie inférieure du plot)

  # projection de Y avec outliers
res.pca2 = PCA(departements_unique[,c("nb_publi","taux_chomage","part_plus65","part_diplomes","depenses_hab","part_etudiants","percent_pop_rurale","pop_insee","age_chef","niveau_vie","nb_crea_entps","nb_nuitees_hotels","nb_etudiants")], quanti.sup=1, graph=F)
fviz_pca_var (res.pca2, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) #favorise open data : dynamisme (nb_crea_entps, nb_etudiants)


  # projections des régions sur le plan à 2 dimensions
fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_pca_biplot(res.pca1, repel = TRUE,
                col.var = "#2E9FDF",
                col.ind = "red"
                )




#----------------------- Arbre de décision CART (https://www.guru99.com/r-decision-trees.html)


library(rpart)
library(caret)
library(rpart.plot)
# On définit les paramètres de contrôle
ctrl=rpart.control(cp=0.01, xval=5, maxdepth=3)

# Fit Theatre
rpart_thea <- rpart(nb_publi ~ taux_chomage+CSP_chef+niveau_rural_mode+niveau_rural_insee+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+pop_insee+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants, data = departements_sans_outliers, method="anova")
summary(rpart_thea)
# Plot
rpart.plot(rpart_thea, box.palette = "Blues")
plot(rpart_thea,uniform=F)
text(rpart_thea,all=T,use.n=T,cex=0.8)
# Voir erreur de prévision selon la taille de l'arbre
plotcp(rpart_thea)
printcp(rpart_thea)



#-----------------------  Dendrogramme


departements_sans_outliers %>% rownames(nom)
  select(nb_publi,nb_ptf,nb_datagouv,taux_chomage,part_plus65,part_diplomes,depenses_hab,part_etudiants,percent_pop_rurale,pop_insee,age_chef,niveau_vie,nb_crea_entps,nb_nuitees_hotels,nb_etudiants) %>% 
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


