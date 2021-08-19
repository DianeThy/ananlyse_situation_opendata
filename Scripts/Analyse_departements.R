
###############################################################################################################################

################################################ ANALYSE DEPARTEMENTS #########################################################

###############################################################################################################################

library(tidyverse)

# On importe la base des départements
departements <- read_csv("Data/process/departements.csv")

# Les variables :
vbles_quanti <- c("nb_publi","nb_ptf","nb_datagouv","taux_chomage","part_plus65","part_diplomes","depenses_hab","part_etudiants","percent_pop_rurale","population","age_chef","niveau_vie","nb_crea_entps","nb_nuitees_hotels","nb_etudiants")
vbles_quali <- c("nom","ouvre_data","niveau_rural","niveau_densite","flux_migration_res","partis_po_chef","CSP_chef")

# On met au bon format les variables qualis
departements[,c("ouvre_data","niveau_rural","niveau_densite","flux_migration_res","partis_po_chef","CSP_chef")] <- lapply(departements[,c("ouvre_data","niveau_rural","niveau_densite","flux_migration_res","partis_po_chef","CSP_chef")], as.factor)



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


# On regarde toutes les CSP de la base
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


# Description niveau_rural
niveau_rural <- as.data.frame(table(departements$niveau_rural)) %>% rename(num = Var1) %>% select(-Freq)
niveau_rural <- niveau_rural %>% mutate(description = c("urbain dense","urbain densité intermédiaire", "rural sous forte influence d'un pôle", "rural sous faible influence d'un pôle", "rural autonome peu dense", "rural autonome très peu dense"),
                                                  variable = "niveau_rural") %>% select(variable,num,description)
# Description niveau_densite
niveau_densite <- as.data.frame(table(departements$niveau_densite)) %>% rename(num = Var1) %>% select(-Freq)
niveau_densite <- niveau_densite %>% mutate(description = c("très dense", "dense", "peu dense"),
                                                    variable = "niveau_densite") %>% select(variable,num,description)
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
dico_variables <- rbind(niveau_rural, niveau_densite, CSP_chef, flux_migration_res, code_region)
View(dico_variables)




                ### A) Analyse non supervisée





#----------------------- Statistiques descriptives


# Statistiques
library(summarytools)
view(dfSummary(departements))

# Répartitions des variables qualis
library(inspectdf)
inspect_cat(departements[,c("ouvre_data","code_region","niveau_rural","niveau_densite","flux_migration_res","CSP_chef","partis_po_chef")]) %>% show_plot() 




#----------------------- Points atypiques 


g1 <- ggplot(departements, aes(x=nb_publi, y=frequency(nb_publi))) + #10 potentiels outliers 
  labs(title="Distribution et box du nombre de publications open data", x="Nombe de publications", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
g2 <- ggplot(departements, aes(x=taux_chomage, y=frequency(taux_chomage))) +  #1
  labs(title="Distribution et box du taux de chômage", x="Taux de chômage", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
g3 <- ggplot(departements, aes(x=part_plus65, y=frequency(part_plus65))) +  
  labs(title="Distribution et box de la part des plus de 65 ans", x="Part des plus de 65 ans", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
g4 <- ggplot(departements, aes(x=part_diplomes, y=frequency(part_diplomes))) +  #6
  labs(title="Distribution et box de la part des diplômés dans la population", x="Part des diplômés", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
g5 <- ggplot(departements, aes(x=depenses_hab, y=frequency(depenses_hab))) +  #3
  labs(title="Distribution et box des dépenses par habitant", x="Dépenses par habitant", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
g6 <- ggplot(departements, aes(x=part_etudiants, y=frequency(part_etudiants))) +  #2
  labs(title="Distribution et box de la part des étudiants", x="Part des étudiants", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
g7 <- ggplot(departements, aes(x=nb_etudiants, y=frequency(nb_etudiants))) +  #8
  labs(title="Distribution et box du nombre d'étudiants", x="Nombre d'étudiants", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
g8 <- ggplot(departements, aes(x=percent_pop_rurale, y=frequency(percent_pop_rurale))) +  
  labs(title="Distribution et box de la part de le pop rurale", x="Part de population", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
g9 <- ggplot(departements, aes(x=population, y=frequency(population))) +  #3
  labs(title="Distribution et box du nombre d'habitants", x="Nombre d'habitants", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
g10 <- ggplot(departements, aes(x=age_chef, y=frequency(age_chef))) +  
  labs(title="Distribution et box de l'âge du chef de l'exécutif", x="Âge du chef", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
g11 <- ggplot(departements, aes(x=niveau_vie, y=frequency(niveau_vie))) +  #5
  labs(title="Distribution et box de la médiane du niveau de vie", x="Médiane du niveau de vie", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
g12 <- ggplot(departements, aes(x=nb_crea_entps, y=frequency(nb_crea_entps))) +  #? 
  labs(title="Distribution et box des créations d'entreprises", x="Créations d'entreprises", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(departements, aes(x=nb_nuitees_hotels, y=frequency(nb_nuitees_hotels))) +  #6
  labs(title="Distribution et box du nombre de nuitées", x="Nombre de nuitées", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol=2, nrow = 2)
grid.arrange(g5, g6, g7, g8, g9, g10, g11, g12, g13, ncol=3, nrow = 3)

# Pour savoir le nombre de points potentiellement atypiques sur le violin plot pour nb_crea_entps
  #on compte le nombre d'observations supérieures au 9è decile
departements %>% filter(nb_crea_entps > (quantile(nb_crea_entps, probs = .9))) %>% count()  #10


# Test de Grubbs quand 1 point potentiellement atypique
library(outliers) 
    # taux_chomage
grubbs.test(departements$taux_chomage, type=10, two.sided = TRUE)
order(departements$taux_chomage)  #outlier qd taux ≥ 12.5, obs n°65 càd Pyrénées Orientales


# Test de Rosner quand plus d'1 point potentiellement atypique
library(EnvStats) 
    # nb_publi
rosnerTest(departements$nb_publi, k = 10, alpha = 0.05) #outlier quand publi ≥ 88 
    # part_diplomes
rosnerTest(departements$part_diplomes, k = 6, alpha = 0.05) #outlier quand part ≥ 16.7%
    # depenses_hab
rosnerTest(departements$depenses_hab, k = 3, alpha = 0.05) #outlier quand dépenses ≥ 1770.413
    # part_etudiants
rosnerTest(departements$part_etudiants, k = 2, alpha = 0.05) #outlier quand part ≥ 16.5%
    # nb_etudiants
rosnerTest(departements$nb_etudiants, k = 8, alpha = 0.05) #outlier quand nombre ≥ 98938
    # population
rosnerTest(departements$population, k = 3, alpha = 0.05) #outlier quand nombre ≥ 2639070
    # niveau_vie
rosnerTest(departements$niveau_vie, k = 5, alpha = 0.05) #outlier quand niveau ≥ 26600
    # nb_crea_entps
rosnerTest(departements$nb_crea_entps, k = 10, alpha = 0.05) #outlier quand nombre ≥ 32480
    # nb_nuitees_hotels
rosnerTest(departements$nb_nuitees_hotels, k = 6, alpha = 0.05) #outlier quand nombre ≥ 6724


# Dans une nouvelle base on retire les observations atypiques
departements_sans_outliers <- departements %>% filter(nb_publi < 88,
                                                      taux_chomage < 12.5,
                                                      part_diplomes < 16.7,
                                                      depenses_hab < 1770.413,
                                                      part_etudiants < 16.5,
                                                      nb_etudiants < 98938,
                                                      population < 2639070,
                                                      niveau_vie < 26600,
                                                      nb_crea_entps < 32480,
                                                      nb_nuitees_hotels < 6724)

nrow(departements)-nrow(departements_sans_outliers) #22 obs perdues
outliers <- anti_join(departements, departements_sans_outliers)  # dep d'Ile de France x6, Pays de la Loire x3, Occitanie x3 etc
View(outliers)



#----------------------- Distributions


# Variable à expliquer
par(mfrow=c(1,2))
hist(departements$nb_publi, prob=T, ylim=c(0, 0.025), col = "#CCCCCC", main = "",
     xlab="nombre de publications open data", ylab="densité")
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
hist(departements$taux_chomage, main="Taux de chômage", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Greens"), xlim = c(4,13))
hist(departements_sans_outliers$taux_chomage, main="Taux de chômage", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n =9, name = "Greens"), xlim = c(4,13))
hist(departements$part_plus65, main="Part des plus de 65 ans", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "PuRd"), xlim = c(10,32))
hist(departements_sans_outliers$part_plus65, main="Part des plus de 65 ans", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "PuRd"), xlim = c(10,32))
hist(departements$part_diplomes, main="Part des diplômés", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Blues"), xlim = c(0,40))
hist(departements_sans_outliers$part_diplomes, main="Part des diplômés", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Blues"), xlim = c(0,40))
hist(departements$depenses_hab, main="Dépenses par habitant", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Reds"), xlim = c(500,4000))
hist(departements_sans_outliers$depenses_hab, main="Dépenses par habitant", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Reds"), xlim = c(500,4000))

par(mfrow=c(4,2))
hist(departements$part_etudiants, main="Part des étudiants", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Greens"), xlim = c(0,40))
hist(departements_sans_outliers$part_etudiants, main="Part des étudiants", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "Greens"), xlim = c(0,40))
hist(departements$nb_etudiants, main="Nombre d'étudiants", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 4, name = "PuRd"), xlim = c(0, 400000))
hist(departements_sans_outliers$nb_etudiants, main="Nombre d'étudiants", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "PuRd"), xlim = c(0, 400000))
hist(departements$percent_pop_rurale, main="Pourcentage de population rurale", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Blues"), xlim = c(0, 90))
hist(departements_sans_outliers$percent_pop_rurale, main="Pourcentage de population rurale", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Blues"), xlim = c(0,90))
hist(departements$population, main="Nombre d'habitants", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 6, name = "Reds"), xlim = c(0,3000000))
hist(departements_sans_outliers$population, main="Nombre d'habitants", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Reds"), xlim = c(0,3000000))

par(mfrow=c(4,2))
hist(departements$age_chef, main="Âge du chef", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Greens"), xlim = c(40,86))
hist(departements_sans_outliers$age_chef, main="Âge du chef", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Greens"), xlim = c(40,86))
hist(departements$niveau_vie, main="Niveau de vie", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "PuRd"), xlim = c(16000,29000))
hist(departements_sans_outliers$niveau_vie, main="Niveau de vie", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "PuRd"), xlim = c(16000,29000))
hist(departements$nb_crea_entps, main="Créations d'entreprises", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Blues"), xlim = c(0,80000))
hist(departements_sans_outliers$nb_crea_entps, main="Créations d'entreprises", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Blues"), xlim = c(0,80000))
hist(departements$nb_nuitees_hotels, main="Nombre de nuitées en hotels", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 6, name = "Reds"), xlim = c(0,40000))
hist(departements_sans_outliers$nb_nuitees_hotels, main="Nombre de nuitées en hotels", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Reds"), xlim = c(0,40000))


# Test de Spearman pour les données sans outliers
shapiro.test(departements_sans_outliers$nb_publi) # pas normal
shapiro.test(departements_sans_outliers$taux_chomage)  # normal à 5%
shapiro.test(departements_sans_outliers$nb_crea_entps)  # pas normal
shapiro.test(departements_sans_outliers$part_plus65)  #normal
shapiro.test(departements_sans_outliers$part_diplomes)  # pas normal
shapiro.test(departements_sans_outliers$depenses_hab)  #normal
shapiro.test(departements_sans_outliers$part_etudiants)  # pas normal
shapiro.test(departements_sans_outliers$percent_pop_rurale)  #à 5% mais pas 10%
shapiro.test(departements_sans_outliers$nb_nuitees_hotels)  # pas normal
shapiro.test(departements_sans_outliers$niveau_vie)  #à 5% mais pas 10%
shapiro.test(departements_sans_outliers$population)  # pas normal
shapiro.test(departements_sans_outliers$nb_etudiants)  # pas normal
shapiro.test(departements_sans_outliers$age_chef)  # pas normal



#-----------------------  Corrélations et dépendances



# Corrélations (matrice de Spearman pour non loi normale) des variables quantis
library(corrplot)
cor1 <- cor(departements_sans_outliers[,c("nb_publi","taux_chomage","part_plus65","part_diplomes","depenses_hab","part_etudiants","percent_pop_rurale","population","age_chef","niveau_vie","nb_crea_entps","nb_nuitees_hotels","nb_etudiants")], use="complete.obs", method=c("spearman"))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor1, method="color", col=col(200), 
             type="upper",
             addCoef.col = "black")
      # 10 corrélations moyennes (0.5<x=<0.6) :
      # 24 corrélations fortes (x>0.6) :


# Dépendances des variables qualis
# ouvre_data avec les autres qualis
chisq.test(departements_sans_outliers$ouvre_data, departements_sans_outliers$niveau_rural) # dépendant 
chisq.test(departements_sans_outliers$ouvre_data, departements_sans_outliers$niveau_densite) # dépendant à +/- 10%
chisq.test(departements_sans_outliers$ouvre_data, departements_sans_outliers$flux_migration_res) #indépendant
chisq.test(departements_sans_outliers$ouvre_data, departements_sans_outliers$partis_po_chef) #indépendant
chisq.test(departements_sans_outliers$ouvre_data, departements_sans_outliers$CSP_chef) #indépendant

# niveau_rural
chisq.test(departements_sans_outliers$niveau_rural, departements_sans_outliers$niveau_densite) # dépendant
chisq.test(departements_sans_outliers$niveau_rural, departements_sans_outliers$flux_migration_res) # dépendant
chisq.test(departements_sans_outliers$niveau_rural, departements_sans_outliers$partis_po_chef) #indépendant
chisq.test(departements_sans_outliers$niveau_rural, departements_sans_outliers$CSP_chef) #indépendant

# niveau_densite
chisq.test(departements_sans_outliers$niveau_densite, departements_sans_outliers$flux_migration_res) # dépendant
chisq.test(departements_sans_outliers$niveau_densite, departements_sans_outliers$partis_po_chef) #indépendant
chisq.test(departements_sans_outliers$niveau_densite, departements_sans_outliers$CSP_chef) #indépendant

# flux_migration_res
chisq.test(departements_sans_outliers$flux_migration_res, departements_sans_outliers$partis_po_chef) #indépendant
chisq.test(departements_sans_outliers$flux_migration_res, departements_sans_outliers$CSP_chef) #indépendant

# partis_po_chef
chisq.test(departements_sans_outliers$partis_po_chef, departements_sans_outliers$CSP_chef) #indépendant



                ### B) Analyse supervisée




    ## 1. VARIABLES QUANTITATIVES





#----------------------- CART 


library(rpart)
library(caret)
library(rpart.plot)
library(randomForest)

# Arbre
arbre_nb.publi <- rpart(nb_publi ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef, data = departements_sans_outliers, method="anova")
summary(arbre_nb.publi)
# Plot
rpart.plot(arbre_nb.publi, box.palette = "Blues")




#----------------------- ACP 


# On change l'index de la base, pour avoir les numéros de départements sur la projection des individus 
infos_dep <- read_csv("https://www.data.gouv.fr/fr/datasets/r/4d8c420b-c412-4deb-a469-b722b195f9c7")
departements_sans_outliers <- left_join(departements_sans_outliers, infos_dep[,c(1,3)], by = "nom")
rownames(departements_sans_outliers) <- departements_sans_outliers$COG


library(FactoMineR)
library(factoextra)

  # ACP
res.pca_Y = PCA(departements_sans_outliers[,c("nb_publi","taux_chomage","part_plus65","part_diplomes","depenses_hab","part_etudiants","percent_pop_rurale","population","age_chef","niveau_vie","nb_crea_entps","nb_nuitees_hotels","nb_etudiants")], quanti.sup=1, graph=F)

  # plot
fviz_pca_var(res.pca_Y, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

  # inertie de chaque axe fictif : % de la variance
round(res.pca_Y$eig,2)
fviz_eig(res.pca_Y, addlabels = TRUE) #axes 1 et 2 conservent 65% de l'info des 12 Xt

  # contributions des Xt aux axes 1, 2 et 3
round(res.pca_Y$var$contrib,2)
fviz_contrib(res.pca_Y, choice = "var", axes = 1, col="black")  #axe 1 = dynamisme du département
fviz_contrib(res.pca_Y, choice = "var", axes = 2, col="black")  #axe 2 = pauvreté / manque d'activité 
fviz_contrib(res.pca_Y, choice = "var", axes = 3, col="black")  #axe 3 = chef age

  # corrélations des Xt aux dimensions : voir relation po/neg entre vble et axe
corrplot(res.pca_Y$var$cor, is.corr=FALSE, method="circle", tl.srt=45, tl.col="#004400", col=brewer.pal(n=9, name="RdYlBu"), addCoef.col="black")  #plus % pop rurale augmente, moins la région est attractive

  # projections des départements colorés en fonction du nombre de jeux ouverts
fviz_pca_ind(res.pca_Y, col.ind = departements_sans_outliers$nb_publi, 
             gradient.cols = c("#FFCC00", "#FC4E07"),
             repel = T)



#----------------------- Nuages de points colorés selon vbles catégorielles


        # CSP_chef
g1 <- ggplot(data = departements_sans_outliers, mapping=aes(taux_chomage, nb_publi, col=CSP_chef)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Nombre de publications open data", x="Taux de chômage") +
  theme_linedraw()
g2 <- ggplot(data = departements_sans_outliers, mapping=aes(part_plus65, nb_publi, col=CSP_chef)) + 
  geom_point(mapping=aes(part_plus65, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Nombre de publications open data", x="Part des plus de 65 ans") +
  theme_linedraw()
g3 <- ggplot(data = departements_sans_outliers, mapping=aes(part_diplomes, nb_publi, col=CSP_chef)) + 
  geom_point(mapping=aes(part_diplomes, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Nombre de publications open data", x="Part des diplômés") +
  theme_linedraw()
g4 <- ggplot(data = departements_sans_outliers, mapping=aes(depenses_hab, nb_publi, col=CSP_chef)) + 
  geom_point(mapping=aes(depenses_hab, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Nombre de publications open data", x="Dépenses par habitant") +
  theme_linedraw()
grid.arrange(g1, g2, g3, g4, ncol = 2, nrow = 2)

    # partis_po_chef
g1 <- ggplot(data = departements_sans_outliers, mapping=aes(part_etudiants, nb_publi, col=partis_po_chef)) + 
  geom_point(mapping=aes(part_etudiants, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Nombre de publications open data", x="Part des étudiants") +
  theme_linedraw()
g2 <- ggplot(data = departements_sans_outliers, mapping=aes(nb_etudiants, nb_publi, col=partis_po_chef)) + 
  geom_point(mapping=aes(nb_etudiants, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Nombre de publications open data", x="Nombre d'étudiants") +
  theme_linedraw()
g3 <- ggplot(data = departements_sans_outliers, mapping=aes(percent_pop_rurale, nb_publi, col=partis_po_chef)) + 
  geom_point(mapping=aes(percent_pop_rurale, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Nombre de publications open data", x="Pourcentage population rurale") +
  theme_linedraw()
g4 <- ggplot(data = departements_sans_outliers, mapping=aes(population, nb_publi, col=partis_po_chef)) + 
  geom_point(mapping=aes(population, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Nombre de publications open data", x="Nombre d'habitants") +
  theme_linedraw()
grid.arrange(g1, g2, g3, g4, ncol = 2, nrow = 2)

    # niveau_densite
g1 <- ggplot(data = departements_sans_outliers, mapping=aes(age_chef, nb_publi, col=niveau_densite)) + 
  geom_point(mapping=aes(age_chef, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Nombre de publications open data", x="Age du chef") +
  theme_linedraw()
g2 <- ggplot(data = departements_sans_outliers, mapping=aes(niveau_vie, nb_publi, col=niveau_densite)) + 
  geom_point(mapping=aes(niveau_vie, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Nombre de publications open data", x="Médiane du niveau de vie") +
  theme_linedraw()
g3 <- ggplot(data = departements_sans_outliers, mapping=aes(nb_crea_entps, nb_publi, col=niveau_densite)) + 
  geom_point(mapping=aes(nb_crea_entps, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Nombre de publications open data", x="Créations d'entreprises") +
  theme_linedraw()
g4 <- ggplot(data = departements_sans_outliers, mapping=aes(nb_nuitees_hotels, nb_publi, col=niveau_densite)) + 
  geom_point(mapping=aes(nb_nuitees_hotels, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Nombre de publications open data", x="Nombre de nuitées") +
  theme_linedraw()
grid.arrange(g1, g2, g3, g4, ncol = 2, nrow = 2)


# Pour le rapport: 2 de chaque
g1 <- ggplot(data = departements_sans_outliers, mapping=aes(taux_chomage, nb_publi, col=CSP_chef)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Publications", x="Taux de chômage") +
  theme_linedraw()
g2 <- ggplot(data = departements_sans_outliers, mapping=aes(part_plus65, nb_publi, col=CSP_chef)) + 
  geom_point(mapping=aes(part_plus65, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Publications", x="Part des plus de 65 ans") +
  theme_linedraw()
g3 <- ggplot(data = departements_sans_outliers, mapping=aes(percent_pop_rurale, nb_publi, col=partis_po_chef)) + 
  geom_point(mapping=aes(percent_pop_rurale, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Publications", x="Pourcentage population rurale") +
  theme_linedraw()
g4 <- ggplot(data = departements_sans_outliers, mapping=aes(population, nb_publi, col=partis_po_chef)) + 
  geom_point(mapping=aes(population, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Publications", x="Nombre d'habitants") +
  theme_linedraw()
g5 <- ggplot(data = departements_sans_outliers, mapping=aes(nb_crea_entps, nb_publi, col=niveau_densite)) + 
  geom_point(mapping=aes(nb_crea_entps, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Publications", x="Créations d'entreprises") +
  theme_linedraw()
g6 <- ggplot(data = departements_sans_outliers, mapping=aes(nb_nuitees_hotels, nb_publi, col=niveau_densite)) + 
  geom_point(mapping=aes(nb_nuitees_hotels, nb_publi), size=2) + 
  geom_quantile(quantiles=0.5, size=1, colour="black") +
  labs(title="",
       y="Publications", x="Nombre de nuitées") +
  theme_linedraw()
grid.arrange(g1, g2, g3, g4, g5, g6, ncol = 2, nrow = 3)
     


#----------------------- Création de variables


     #depuis ACP
departements_sans_outliers$dynamisme <- res.pca_Y$ind$coord[,1]
departements_sans_outliers$pauvrete <- res.pca_Y$ind$coord[,2]
     #depuis CART
departements_sans_outliers <- departements_sans_outliers %>% mutate(is_5.65_part_diplomes = case_when(part_diplomes >= 5.65 ~ 1,
                                                                                                     part_diplomes < 5.65 ~ 0))




    ## 2. VARIABLES QUALI




#----------------------- ACM


  # plot
res.mca <- MCA(departements_sans_outliers[,c("ouvre_data","niveau_rural","niveau_densite","partis_po_chef","CSP_chef")], quali.sup=1)
fviz_pca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
  # projection des variables
fviz_mca_var(res.mca, axe=c(1,2), invisible="ind",cex=0.8,autoLab="yes", jitter = list(what = "label", width = NULL, height = NULL))
  # projection des modalités
fviz_eig(res.mca,main="Pourcentage expliqué par chaque facteur")
  # contributions des variables aux axes
round(res.mca$var$coord,2)
fviz_contrib(res.mca, choice = "var", axes = 1, col="black")  #axe 1 = département urbain/dense
fviz_contrib(res.mca, choice = "var", axes = 2, col="black")  #axe 2 = faible dynamisme 
  # corrélations des Xt aux dimensions : voir relation po/neg entre vble et axe
corrplot(res.mca$var$coord, is.corr=FALSE, method="circle", tl.srt=45, tl.col="#004400", col=brewer.pal(n=9, name="RdYlBu"), addCoef.col="black") 
  # projections des départements sur le plan à 2 dimensions
fviz_pca_ind(res.mca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T)



#----------------------- Diagrammes croisés


departements_sans_outliers$pol2 <- str_replace_all(departements_sans_outliers$partis_po_chef, c("Gauche"="1", "Droite"="2", "sans étiquette" = "3"))

# Croisements Y (nb_publi) avec variables qualis
t1=tapply(departements_sans_outliers$nb_publi, departements_sans_outliers$niveau_rural, median) %>% sort
t2=tapply(departements_sans_outliers$nb_publi, departements_sans_outliers$niveau_densite, median) %>% sort()
t4=tapply(departements_sans_outliers$nb_publi, departements_sans_outliers$pol2, median) %>% sort()
t5=tapply(departements_sans_outliers$nb_publi, departements_sans_outliers$CSP_chef, median) %>% sort()

par(mfrow=c(2,2))
library(RColorBrewer)
barplot(t1[-1], horiz=TRUE, xlim=c(0,50), main="Nombre de jeux ouverts médian selon le 
        niveau de ruralité", col=brewer.pal(n = 5, name = "Blues"), cex.names=1, cex.main=1.1, col.main="#0033CC")

barplot(t2, horiz=TRUE, xlim=c(0,50), main="Nombre de jeux ouverts médian selon le 
        niveau de densité", col = brewer.pal(n = 3, name = "Reds"), cex.names=1, cex.main=1.1, col.main="#FF6600")

barplot(t4, horiz=TRUE, xlim=c(0,50), main="Nombre de jeux ouverts médian selon la 
        couleur politique", col=brewer.pal(n = 3, name = "Purples"), cex.names=1, cex.main=1.1, col.main="#756BB1")

barplot(t5, horiz=TRUE, xlim=c(0,50), main="Nombre de jeux ouverts médian selon la 
        CSP du chef de l'exécutif", col=brewer.pal(n = 7, name = "Greens"), cex.names=1, cex.main=1.1, col.main="#006600") #Cat 2 sous représentées donc att aux conclus

table(departements_sans_outliers$niveau_rural)
table(departements_sans_outliers$niveau_densite)
table(departements_sans_outliers$pol2)
table(departements_sans_outliers$CSP_chef)




#----------------------- Treemap


# On récupère les noms des régions
infos_reg <- read_csv("Data/raw/infos_regions.csv") %>% rename(region = nom)
departements_sans_outliers <- left_join(departements_sans_outliers, infos_reg[,c(1,3)], by = c("code_region" = "COG"))

# Data pour savoir les départements de quelles régions ouvrent le plus de données
t6=tapply(departements_sans_outliers$nb_publi, departements_sans_outliers$region, mean) 
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

table(departements_sans_outliers$code_region) #Nb d'obs trop différent d'une région à l'autre




#----------------------- Boxplots croisés


ggplot(departements_sans_outliers, aes(x=niveau_rural, y=nb_publi, fill=niveau_rural)) + 
  geom_boxplot()+
  geom_point() +
  labs(title="Box du nombre de publications par niveau de ruralité", y = "Nombre de jeux ouverts", x = "") + 
  scale_fill_brewer(name="Niveau de ruralité", palette="Blues") +
  guides(fill = FALSE) +
  theme_classic() #plus il y a d'obs plus la moyenne peut être tirée vers le bas
ggplot(departements_sans_outliers, aes(x=niveau_densite, y=nb_publi, fill=niveau_densite)) + 
  geom_boxplot()+
  geom_point() +
  labs(title="Box du nombre de publications par niveau de densité", y = "Nombre de jeux ouverts", x = "") + 
  scale_fill_brewer(name="Niveau de densité", palette="Blues")  +
  guides(fill = FALSE) +
  theme_classic() # cat 2 et 3 vraiment comparables
ggplot(departements_sans_outliers, aes(x=partis_po_chef, y=nb_publi, fill=partis_po_chef)) + 
  geom_boxplot()+
  geom_point() +
  labs(title="Box du nombre de publications par couleur politique", y = "Nombre de jeux ouverts", x = "") + 
  scale_fill_brewer(name="Couleur politique", palette="Blues") +
  guides(fill = FALSE) +
  theme_classic()
ggplot(departements_sans_outliers, aes(x=CSP_chef, y=nb_publi, fill=CSP_chef)) + 
  geom_boxplot()+
  geom_point() +
  labs(title="Box du nombre de publications par CSP du chef", y = "Nombre de jeux ouverts", x = "") + 
  scale_fill_brewer(name="CSP du chef", palette="Blues") +
  guides(fill = FALSE) +
  theme_classic()  #3 et 7 vraiment comparables (cadres / retraités)



#----------------------- Stacked barchart



  # Niveau de ruralité et ouvre_data
departements_sans_outliers$ouvre_data <- str_replace_all(departements_sans_outliers$ouvre_data, "1", "oui")
departements_sans_outliers$ouvre_data <- str_replace_all(departements_sans_outliers$ouvre_data, "0", "non")
t7 <- departements_sans_outliers %>% group_by(ouvre_data) %>% count(niveau_densite)
  # plot
library(viridis)
library(hrbrthemes)
library(plotly)
g1 <- ggplot(t7, aes(fill=ouvre_data, y=n, x=niveau_densite)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T, name="Ouvre ?", option="E", direction=1) +
    theme_ipsum() +
    xlab("") + ylab("Nombre de départements") + ggtitle("Ouverture de données selon 
le niveau de densité") +
    theme(axis.title.y = element_text(size=12))


  # partis_po_chef et ouvre_data
t8 <- departements_sans_outliers %>% group_by(ouvre_data) %>% count(pol2)
  # plot
g2 <- ggplot(t8, aes(fill=ouvre_data, y=n, x=pol2)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T, name="Ouvre ?", option="E", direction=1) +
    theme_ipsum() +
    xlab("") + ylab("Nombre de départements") + ggtitle("Ouverture de données selon 
la couleur politique") +
    theme(axis.title.y = element_text(size=12))


  # CSP_chef et ouvre_data
t9 <- departements_sans_outliers %>% group_by(ouvre_data) %>% count(CSP_chef)
  # plot
g3 <- ggplot(t9, aes(fill=ouvre_data, y=n, x=CSP_chef)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T, name="Ouvre ?", option="E", direction=1) +
    theme_ipsum() +
    xlab("") + ylab("Nombre de départements") + ggtitle("Ouverture de données selon 
la CSP du chef") +
    theme(axis.title.y = element_text(size=12))

grid.arrange(g1,g2,g3, ncol=3, nrow=1)






#-------------------- Export des bases

rio::export(departements, "./Data/process/dep_analyse_explo.csv")
rio::export(departements_sans_outliers[,1:27], "./Data/process/dep_analyse_explo_sans_outliers.csv")

