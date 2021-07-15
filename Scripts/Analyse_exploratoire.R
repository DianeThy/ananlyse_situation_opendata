
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



# On va travailler sur les observations complètes pour une analyse générale
  # on créé pour reg dep et com une colonne du type d'orga pour rbinder ensuite
regions$type <- "REG"
departements$type <- "DEP"
communes$type <- "COM"
  # on réordonne et sélectionne les colonnes communes aux 4 types d'orga
reg2 <- regions %>% select(nom,type,nb_publi,ouvre_data,pop_insee,CSP_chef,age_chef,partis_po_chef,part_plus65,nb_crea_entps,flux_migration_res,part_diplomes,depenses_hab)
dep2 <- departements %>% select(nom,type,nb_publi,ouvre_data,pop_insee,CSP_chef,age_chef,partis_po_chef,part_plus65,nb_crea_entps,flux_migration_res,part_diplomes,depenses_hab)
com2 <- communes %>% select(nom,type,nb_publi,ouvre_data,pop_insee,CSP_chef,age_chef,partis_po_chef,part_plus65,nb_crea_entps,flux_migration_res,part_diplomes,depenses_hab)
epci2 <- epci %>% select(nom,type,nb_publi,ouvre_data,pop_insee,CSP_chef,age_chef,partis_po_chef,part_plus65,nb_crea_entps,flux_migration_res,part_diplomes,depenses_hab)
  # on met le tout dans une même base et on garde les obs complètes
base <- rbind(reg2,dep2,com2,epci2) %>% na.omit()



                ### A) Variables une à une


    ## 1. VARIABLES QUANTI



# Quand plusieurs partis po pour 1 orga alors plusieurs obs. Pour ne pas fausser l'analyse exploratoire sur les autres variables (quanti) on retire la colonne du parti_po et on supprime les doublons : 1 orga = 1obs
base_unique <- base %>% select(-partis_po_chef) %>% unique()



#----------- Points atypiques et distribution


g1 <- ggplot(base_unique, aes(x=nb_publi, y=frequency(nb_publi))) +  
  labs(title="Distribution et box du nombre de publications", x="Nombe de publications", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
g2 <- ggplot(base_unique, aes(x=pop_insee, y=frequency(pop_insee))) +
  labs(title="Distribution et box du nombre d'habitants", x="Nombre d'habitants", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
g3 <- ggplot(base_unique, aes(x=age_chef, y=frequency(age_chef))) + #1 outlier
  labs(title="Distribution et box de l'âge du chef", x="Âge du chef de l'exécutif", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
g4 <- ggplot(base_unique, aes(x=part_plus65, y=frequency(part_plus65))) +  
  labs(title="Distribution et box de la part des plus de 65 ans", x="Part des plus de 65 ans", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(base_unique, aes(x=nb_crea_entps, y=frequency(nb_crea_entps))) +  
  labs(title="Distribution et box du nombre d'entreprises créées", x="Nombre d'entreprises créées", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(base_unique, aes(x=part_diplomes, y=frequency(part_diplomes))) +
  labs(title="Distribution et box de la part des diplômés", x="Part des diplômés", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(base_unique, aes(x=depenses_hab, y=frequency(depenses_hab))) + 
  labs(title="Distribution et box des dépenses par habitant", x="Dépenses par habitant", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()

library(gridExtra)
grid.arrange(g1,g2,g3,g4, ncol=2, nrow = 2)




  # Test de Grubbs quand 1 point atypique
library(outliers) 
    # age_chef
grubbs.test(base_unique$age_chef, type=10, two.sided = TRUE)
order(base_unique$age_chef)  #outlier qd age ≤ 33, obs n°297 càd commune de Fréjus


  # Test de Rosner quand plusieurs points atypiques
library(EnvStats) 

# Pour savoir le nombre de points potentiellement atypiques sur le violin plot (k du test de Rosner)
base_unique %>% filter(nb_publi > (summary(base_unique$nb_publi))[5]) %>% count() #132
base_unique %>% filter(pop_insee > (summary(base_unique$pop_insee))[5]) %>% count() #132
base_unique %>% filter(nb_crea_entps > (summary(base_unique$nb_crea_entps))[5]) %>% count() #132
base_unique %>% filter(part_diplomes > (summary(base_unique$part_diplomes))[5]) %>% count() #130
base_unique %>% filter(depenses_hab > (summary(base_unique$depenses_hab))[5]) %>% count() #132

    # nb_publi
rosnerTest(base_unique$nb_publi, k = 132, alpha = 0.05) #outlier quand taux ≥ 16.1, Guadeloupe, Guyane, Réunion
    # pop_insee
rosnerTest(base_unique$pop_insee, k = 132, alpha = 0.05) #outlier quand PIB/hab ≥ 59387 ou ≤ 14879, Ile de France et Guyane
    # part_plus65
rosnerTest(base_unique$part_plus65, k = 8, alpha = 0.05) #outlier quand PIB/hab ≥ 59387 ou ≤ 14879, Ile de France et Guyane
    # nb_crea_entps
rosnerTest(base_unique$nb_crea_entps, k = 132, alpha = 0.05) #outlier quand part ≤ 11.1%, Guyane et Réunion
    # part_diplomes
rosnerTest(base_unique$part_diplomes, k = 130, alpha = 0.05) #outlier quand niveau ≥ 23860 ou ≤ 17880, Ile de France, Martinique, Réunion
    # depenses_hab
rosnerTest(base_unique$depenses_hab, k = 132, alpha = 0.05) #outlier quand dépenses ≥ 2835, Martinique, Corse


# On retire les points atypiques dans une nouvelle base pour faire une double analyse par la suite
base_unique_sans_outliers <- base_unique[-c(1:5,17),]  # Ile de France et DROM



#----------- Stats


  # Statistiques descriptives
library(summarytools)
view(dfSummary(regions_unique)) # avec outliers
view(dfSummary(regions_unique_sans_outliers)) # sans outliers, toutes les variables sont homogènes (sauf nb_datagouv mais pas utile) = écart-type < moyenne



#----------- Distribution


  # Histogrammes de distribution

# Variable à expliquer
par(mfrow=c(1,2))
hist(regions_unique$nb_publi, prob=T, ylim=c(0, 0.015), 
     xlab="Avec outliers", ylab="Densité", main="Nombre de publications open data")
lines(density(regions_unique$nb_publi), col="red", lwd=2)
curve(dnorm(x, mean=mean(regions_unique$nb_publi), sd=sd(regions_unique$nb_publi)),col="#0033CC", lwd=2, add=TRUE, yaxt="n")
hist(regions_unique_sans_outliers$nb_publi, prob=T, ylim=c(0, 0.015), 
     xlab="Sans outliers", ylab="Densité", main="Nombre de publications open data")
lines(density(regions_unique_sans_outliers$nb_publi), col="red", lwd=2)
curve(dnorm(x, mean=mean(regions_unique_sans_outliers$nb_publi), sd=sd(regions_unique_sans_outliers$nb_publi)), 
      col="#0033CC", lwd=2, add=TRUE, yaxt="n")


# Variables explicatives
library(RColorBrewer)
par(mfrow=c(5,2))
hist(regions_unique$taux_chomage, main="Taux de chômage", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 6, name = "YlGn"))
hist(regions_unique_sans_outliers$taux_chomage, main="Taux de chômage", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "YlGn"))
hist(regions_unique$primaire_VA, main="Part du secteur primaire", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 5, name = "PuRd"))
hist(regions_unique_sans_outliers$primaire_VA, main="Part du secteur primaire", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "PuRd"))
hist(regions_unique$secondaire_VA, main="Part du secteur secondaire", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 8, name = "Reds"))
hist(regions_unique_sans_outliers$secondaire_VA, main="Part du secteur secondaire", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 6, name = "Reds"))
hist(regions_unique$tertiaire_marchand_VA, main="Part du secteur tertiaire marchand", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 8, name = "Blues"))
hist(regions_unique_sans_outliers$tertiaire_marchand_VA, main="Part du secteur tertiaire marchand", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "Blues"))
hist(regions_unique$tertiaire_non_mar_VA, main="Part du secteur tertiaire non marchand", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 5, name = "Greys"))
hist(regions_unique_sans_outliers$tertiaire_non_mar_VA, main="Part du secteur tertiaire non marchand", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "Greys"))

par(mfrow=c(5,2))
hist(regions_unique$part_plus65, main="Part des plus de 65 ans", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 4, name = "Purples"))
hist(regions_unique_sans_outliers$part_plus65, main="Part des plus de 65 ans", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "Purples"))
hist(regions_unique$part_diplomes, main="Part des diplômés", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Oranges"))
hist(regions_unique_sans_outliers$part_diplomes, main="Part des diplômés", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Oranges"))
hist(regions_unique$depenses_hab, main="Dépenses par habitant", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Greens"))
hist(regions_unique_sans_outliers$depenses_hab, main="Dépenses par habitant", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Greens"))
hist(regions_unique$part_etudiants, main="Part des étudiants", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "Spectral"))
hist(regions_unique_sans_outliers$part_etudiants, main="Part des étudiants", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 7, name = "Spectral"))
hist(regions_unique$percent_pop_rurale, main="Pourcentage de population rurale", xlab="Avec outliers", ylab="Fréquences", col=brewer.pal(n = 6, name = "PuBu"))
hist(regions_unique_sans_outliers$percent_pop_rurale, main="Pourcentage de population rurale", xlab="Sans outliers", ylab="Fréquences", col=brewer.pal(n = 9, name = "PuBu"))


  # Test de Spearman des bases sans outliers
shapiro.test(regions_unique_sans_outliers$nb_publi) #normal
shapiro.test(regions_unique_sans_outliers$taux_chomage)  #tout juste normal à 5%
shapiro.test(regions_unique_sans_outliers$primaire_VA)  #normal
shapiro.test(regions_unique_sans_outliers$secondaire_VA)  #normal
shapiro.test(regions_unique_sans_outliers$tertiaire_marchand_VA)  #normal
shapiro.test(regions_unique_sans_outliers$tertiaire_non_mar_VA)  #normal
shapiro.test(regions_unique_sans_outliers$part_plus65)  #normal
shapiro.test(regions_unique_sans_outliers$part_diplomes)
shapiro.test(regions_unique_sans_outliers$depenses_hab)  #normal
shapiro.test(regions_unique_sans_outliers$part_etudiants)  #normal
shapiro.test(regions_unique_sans_outliers$percent_pop_rurale)  #à 5% mais pas 10%


#----------- Corrélations



  # Matrice de corrélation de Spearman car variables ne suivent pas toutes une loi normale (on exclu les Y)
library(corrplot)
cor1 <- cor(regions_unique_sans_outliers[,c("nb_publi","taux_chomage","primaire_VA","secondaire_VA","tertiaire_marchand_VA","tertiaire_non_mar_VA", "part_plus65","part_diplomes","depenses_hab","part_etudiants","percent_pop_rurale","pop_insee","age_chef","PIB_habitant","niveau_vie","nb_crea_entps","nb_nuitees_hotels","nb_etudiants")], use="complete.obs", method=c("spearman"))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor1, method="color", col=col(200), 
             type="upper",
             addCoef.col = "black")
# corrélations moyennes (0.5<x<0.6) :
    # - taux_chomage et depenses_hab
    # - primaire_VA et part_etudiants/pop_insee/age_chef/nb_etudiants
    # - secondaire_VA et part_diplomes
    # - tertiaire_marchand_VA et nb_nuitees_hotels
    # - tertiaire_non_mar_VA et depenses_hab
    # - part_plus65 et age_chef
    # - part_diplomes et part_etudiants/nb_etudiants
    # - percent_pop_rurale et pop_insee/nb_etudiants
    # - PIB_habitant et nb_crea_entps/nb_nuitees_hotels
# corrélations fortes (x≥0.6) :
    # - taux_chomage et tertaire_non_mar_VA/percent_pop_rurale/niveau_vie
    # - primaire_VA et tertiaire_marchand_VA/percent_pop_rurale/nb_crea_entps/nb_nuitees_hotels
    # - secondaire_VA et tertiaire_marchand_VA
    # - tertiaire_marchand_VA et part_diplomes/depenses_hab/PIB_habitant/nb_crea_entps
    # - tertaire_non_mar_VA et PIB_habitant/niveau_vie
    # - part_diplomes et depenses_hab/PIb_habitant/nb_crea_entps/nb_nuitees_hotels
    # - depenses_hab et PIB_habitant/niveau_vie
    # - part_etudiants et pop_insee/age_chef/nb_crea_entps/nb_nuitees_hotels/nb_etudiants
    # - percent_pop_rurale et nb_crea_entps/nb_nuitees_hotels
    # - pop_insee et nb_crea_entps/nb_nuitees_hotels/nb_etudiants
    # - PIB_habitant et niveau_vie
    # - nb_crea_entps et nb_nuitees_hotels/nb_etudiants
    # - nb_nuitees_hotels et nb_etudiants



# ACP pour voir groupements de variables explicatives
library(FactoMineR)
library(factoextra)
  # plot
res.pca = PCA(regions_unique_sans_outliers[,c(6,8,10:26)], graph=F)
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

  # inertie de chaque axe fictif : % de la variance
round(res.pca$eig,2)
fviz_eig(res.pca, addlabels = TRUE) #axes 1 et 2 conservent 60% de l'info des 19 Xt

  # contributions des Xt aux axes 1 et 2
round(res.pca$var$contrib,2)
fviz_contrib(res.pca, choice = "var", axes = 1, col="black")  #axe 1 = dynamisme région / activité
fviz_contrib(res.pca, choice = "var", axes = 2, col="black")  #axe 2 = qualité de vie (indirecte)
      # ajout CC

  # corrélations des Xt aux dimensions : voir relation po/neg entre vble et axe
res <- get_pca_var(res.pca)
corrplot(res$cor, is.corr=FALSE, method="circle", tl.srt=45, tl.col="#004400", col=brewer.pal(n=9, name="RdYlBu"),
addCoef.col="black")  #plus l'age du chef/% pop rurale augmentent, moins la région est attractive

  # projection de Y
res.pca1=PCA(regions_unique_sans_outliers[,c(2,6,8,10:23,26)], quanti.sup=1)
fviz_pca_var (res.pca1, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) #permet de voir corrélations avec Xt : plus la région est dynamique plus elle a de chances d'ouvrir data, mais pas forcément une bonne qualité de vie (partie inférieure du plot)

  # projection de Y avec outliers
res.pca1=PCA(regions_unique[,c(2,6,8,10:23,26)], quanti.sup=1)
fviz_pca_var (res.pca1, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) #favorise open data : dynamisme (nb_cre_entps, nb_etudiants)


  # projections des régions sur le plan à 2 dimensions
fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_pca_biplot(res.pca1, repel = TRUE,
                col.var = "#2E9FDF",
                col.ind = "red"
                )

# regions les plus dynamiques :
# regions les moins dynamiques : 
# regions avec meilleure qualité de vie :
# regions avec moins bonne qualité de vie : 





# VARIABLES QUALI


# On repart de la base complète avec les partis politiques, à laquelle on retire les outliers
regions_sans_outliers <- regions[c(1:9,27:29),]

# On met au bon format les variables qualis
regions[,c(3,7,9,10,25,26)] <- lapply(regions[,c(3,7,9,10,25,26)], as.factor)
regions_unique[,c(3,7,9,24,25)] <- lapply(regions_unique[,c(3,7,9,24,25)], as.factor)
regions_sans_outliers[,c(3,7,9,10,25,26)] <- lapply(regions_sans_outliers[,c(3,7,9,10,25,26)], as.factor)
regions_unique_sans_outliers[,c(3,7,9,24,25)] <- lapply(regions_unique_sans_outliers[,c(3,7,9,24,25)], as.factor)



#----------- Fréquence des modalités des variables catégoriques


library(inspectdf)
inspect_cat(regions[,c(3,7,9,10,25,26)]) %>% show_plot(high_cardinality = 1)  #données initiales pour voir repartition partis_po
  # partis_po ressortent le plus sont UMP, PS et LR
inspect_cat(regions_unique[,c(3,7,9,24,25)]) %>% show_plot(high_cardinality = 1) #pour les autres c'est la base sans doublons à regarder
  # CSP_chef 12% autres professions et médecins
  # flux_migration 47% ds dep 11 càd Aude
inspect_cat(regions_unique_sans_outliers[,c(3,7,9,24,25)]) %>% show_plot(high_cardinality = 1) #same sans outliers


#----------- Dépendances


# CSP_chef avec les autres qualis
chisq.test(regions_unique_sans_outliers$CSP_chef, regions_unique_sans_outliers$ouvre_data)
chisq.test(regions_unique_sans_outliers$CSP_chef, regions_unique_sans_outliers$niveau_rural_mode)
chisq.test(regions_unique_sans_outliers$CSP_chef, regions_unique_sans_outliers$niveau_rural_insee)
chisq.test(regions_unique_sans_outliers$CSP_chef, regions_unique_sans_outliers$flux_migration_res)
chisq.test(regions_unique_sans_outliers$CSP_chef, regions_unique_sans_outliers$partis_po_chef)

# Ouvre_data avec les autres qualis
chisq.test(regions_unique_sans_outliers$ouvre_data, regions_unique_sans_outliers$niveau_rural_mode)
chisq.test(regions_unique_sans_outliers$ouvre_data, regions_unique_sans_outliers$niveau_rural_insee)
chisq.test(regions_unique_sans_outliers$ouvre_data, regions_unique_sans_outliers$flux_migration_res)
chisq.test(regions_unique_sans_outliers$ouvre_data, regions_unique_sans_outliers$partis_po_chef)

vbles_quali <- c("ouvre_data","niveau_rural_mode","niveau_rural_insee","flux_migration_res","nom","partis_po_chef","CSP_chef")



#cluster partis po
#clusters CSP

# ACM
res.mca <- MCA(regions_sans_outliers[,c(7,9,10,25,26)])
fviz_mca_var(res.mca, axe=c(1,2), invisible="ind",cex=0.8,autoLab="yes", jitter = list(what = "label", width = NULL, height = NULL))
fviz_eig(res.pca,main="Pourcentage expliqué par chaque facteur")


# 


    ## 2. Relations entre les variables explicatives 











    ## 3. Relations des variables explicatives avec Y



# VARIABLES QUANTI


#----------- Nuages de points



# Relations positives (CC > 0)

ggplot(data = regions_unique_sans_outliers, mapping=aes(pop_insee, nb_publi)) + 
  geom_point(mapping=aes(pop_insee, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le nombre d'habitants",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = regions_unique_sans_outliers, mapping=aes(taux_chomage, nb_publi)) + 
  geom_point(mapping=aes(taux_chomage, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le taux de chômage",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = regions_unique_sans_outliers, mapping=aes(tertiaire_marchand_VA, nb_publi)) + 
  geom_point(mapping=aes(tertiaire_marchand_VA, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part du tertiaire marchand",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = regions_unique_sans_outliers, mapping=aes(tertiaire_non_mar_VA, nb_publi)) + 
  geom_point(mapping=aes(tertiaire_non_mar_VA, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part du tertiaire non marchand",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = regions_unique_sans_outliers, mapping=aes(part_diplomes, nb_publi)) + 
  geom_point(mapping=aes(part_diplomes, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des diplomés",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = regions_unique_sans_outliers, mapping=aes(part_etudiants, nb_publi)) + 
  geom_point(mapping=aes(part_etudiants, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des étudiants",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = regions_unique_sans_outliers, mapping=aes(PIB_habitant, nb_publi)) + 
  geom_point(mapping=aes(PIB_habitant, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le PIB par habitant",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = regions_unique_sans_outliers, mapping=aes(niveau_vie, nb_publi)) + 
  geom_point(mapping=aes(niveau_vie, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le niveau de vie",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = regions_unique_sans_outliers, mapping=aes(nb_crea_entps, nb_publi)) + 
  geom_point(mapping=aes(nb_crea_entps, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le nombre d'entreprises créées",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = regions_unique_sans_outliers, mapping=aes(nb_nuitees_hotels, nb_publi)) + 
  geom_point(mapping=aes(nb_nuitees_hotels, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le nombre de nuitées en hotels",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = regions_unique_sans_outliers, mapping=aes(nb_etudiants, nb_publi)) + 
  geom_point(mapping=aes(nb_etudiants, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le nombre d'étudiants",
       y="Nombre de publications open data", x="") +
  theme_linedraw()


# Relations negatives (CC < 0)

ggplot(data = regions_unique_sans_outliers, mapping=aes(primaire_VA, nb_publi)) + 
  geom_point(mapping=aes(primaire_VA, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part du primaire dans la VA",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = regions_unique_sans_outliers, mapping=aes(secondaire_VA, nb_publi)) + 
  geom_point(mapping=aes(secondaire_VA, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part du secondaire dans la VA",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = regions_unique_sans_outliers, mapping=aes(part_plus65, nb_publi)) + 
  geom_point(mapping=aes(part_plus65, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et la part des plus de 65 ans",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = regions_unique_sans_outliers, mapping=aes(percent_pop_rurale, nb_publi)) + 
  geom_point(mapping=aes(percent_pop_rurale, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et le pourcentage de population rurale",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = regions_unique_sans_outliers, mapping=aes(age_chef, nb_publi)) + 
  geom_point(mapping=aes(age_chef, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et l'âge du chef",
       y="Nombre de publications open data", x="") +
  theme_linedraw()
ggplot(data = regions_unique_sans_outliers, mapping=aes(depenses_hab, nb_publi)) + 
  geom_point(mapping=aes(depenses_hab, nb_publi), size=3) + 
  geom_quantile(quantiles=0.5, size=1, colour="red") +
  labs(title="Relation entre le nombre de publications et les dépenses par habitant",
       y="Nombre de publications open data", x="") +
  theme_linedraw()



# VARIABLES QUALI



# Croisements Ouvre_data avec budget et age
t1=tapply(regions_sans_outliers$budget_random, regions_sans_outliers$ouvre_data, mean)
t2=tapply(regions_sans_outliers$age, regions_sans_outliers$ouvre_data, mean)

par(mfrow=c(1,2))
library(RColorBrewer)

barplot(t1, horiz=TRUE,xlim=c(0,50),legend=c("Jamais", "1 à 2 fois par an", "3 à 10 fois par an", "1 fois par mois", "Au moins 1 fois par semaine", "Ne sait pas"), main="Budget mensuel moyen alloué à la Culture selon 
les fréquences de sortie à des conférences", col=brewer.pal(n = 6, name = "Blues"), cex.names=.9, cex.main=.96, col.main="#0033CC")
barplot(t2, horiz=TRUE,xlim=c(0,30),legend=c("Jamais", "1 à 2 fois par an", "3 à 10 fois par an", "1 fois par mois", "Au moins 1 fois par semaine", "Ne sait pas"), main="Âge moyen des étudiants selon les 
fréquences de sortie à des conférences", col=brewer.pal(n = 6, name = "Reds"), cex.names=1, cex.main=1, col.main="#FF6600")



                ### B) Départements




# VARIABLES QUANTI


summary(departements) 
NA_dep <- as.data.frame(apply(is.na(departements), 2, sum)) %>% rename(nb_NA = `apply(is.na(departements), 2, sum)`) #/119




# Arbre de décision CART (https://www.guru99.com/r-decision-trees.html)
library(rpart)
library(caret)
library(rpart.plot)
# On définit les paramètres de contrôle
ctrl=rpart.control(cp=0.01, xval=5, maxdepth=3)

# Fit Theatre
rpart_thea <- rpart(nb_publi ~ CSP_chef+flux_migration_res+niveau_rural_mode+niveau_rural_insee+taux_chomage+primaire_VA+secondaire_VA+tertiaire_marchand_VA+tertiaire_non_mar_VA+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+pop_insee+age_chef+PIB_habitant+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants, data = regions_unique_sans_outliers, method="anova")
summary(rpart_thea)
# Plot
rpart.plot(rpart_thea, box.palette = "Blues")
X11()
par(xpd=NA)
plot(rpart_thea,uniform=F)
text(rpart_thea,all=T,use.n=T,cex=0.8)
# Voir erreur de prévision selon la taille de l'arbre
plotcp(rpart_thea)
printcp(rpart_thea)




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



### SELECTION DE VARIABLES MODELES


# Méthode bacward, forward et both pour trouver les variables qu'il faut garder pour le modèle
  # Forward
modele<-glm((Theatre_binaire~1),data=base_binaire,family=binomial(logit))
modele.forward<-step(modele,scope=list(lower=~1,upper=~freqConcert+freqConference+freqMusee+freqCinema+freqBiblio+freqRadio+freqTele+freqJeuxVid+freqLecture+participation_acti_univ+sensibilisation_culture+budget+boursier+travail+formation+genre+age),data=base_binaire,direction="forward")
summary(modele.forward)

  # Bacward
modele<-glm((Theatre_binaire~freqConcert+freqConference+freqMusee+freqCinema+freqBiblio+freqRadio+freqTele+freqJeuxVid+freqLecture+participation_acti_univ+sensibilisation_culture+budget+boursier+travail+formation+genre+age),data=base_binaire,family=binomial(logit))
modele.backward<-step(modele,scope=list(lower=~1,upper=~freqConcert+freqConference+freqMusee+freqCinema+freqBiblio+freqRadio+freqTele+freqJeuxVid+freqLecture+participation_acti_univ+sensibilisation_culture+budget+boursier+travail+formation+genre+age), data=base_binaire,direction="backward")
summary(modele.backward)

  # Both
modele<-glm((Theatre_binaire~1),data=base_binaire,family=binomial(logit))
modele.both<-step(modele,scope=list(lower=~1,upper=~freqConference+freqMusee+freqCinema+freqBiblio+freqRadio+freqTele+freqJeuxVid+freqLecture+participation_acti_univ+sensibilisation_culture+budget+boursier+travail+formation+genre+age), data=base_binaire,direction="both")
summary(modele.both)

















############### POUBELLE



  # Boxplots
library(rAmCharts)  # ATTENTION : individual = n° d'obs, est différent de "number of outliers" 
amBoxplot(regions_unique$nb_publi, xlab=" ", ylab=" ", main="Nombre de données publiées (local ou datagouv)") #1 outlier
amBoxplot(regions_unique$pop_insee, xlab=" ", ylab=" ", main="Population") 
amBoxplot(regions_unique$age_chef, xlab=" ", ylab=" ", main="Âge du chef de l'exécutif")
amBoxplot(regions_unique$taux_chomage, xlab=" ", ylab=" ", main="Taux de chômage") #3
amBoxplot(regions_unique$PIB_habitant, xlab=" ", ylab=" ", main="PIB par habitant")  #6
amBoxplot(regions_unique$primaire_VA, xlab=" ", ylab=" ", main="Part du secteur primaire dans la VA")
amBoxplot(regions_unique$secondaire_VA, xlab=" ", ylab=" ", main="Part du secteur secondaire dans la VA")
amBoxplot(regions_unique$tertiaire_marchand_VA, xlab=" ", ylab=" ", main="Part du secteur tertiaire marchand dans la VA") #1
amBoxplot(regions_unique$tertiaire_non_mar_VA, xlab=" ", ylab=" ", main="Part du secteur tertiaire non marchand dans la VA")
amBoxplot(regions_unique$part_plus65, xlab=" ", ylab=" ", main="Part des plus de 65 ans dans la population") #2
amBoxplot(regions_unique$niveau_vie, xlab=" ", ylab=" ", main="Médiane du niveau de vie") #3
amBoxplot(regions_unique$part_diplomes, xlab=" ", ylab=" ", main="Part des diplômés dans la population") #1
amBoxplot(regions_unique$nb_crea_entps, xlab=" ", ylab=" ", main="Nombre de création d'entreprises") #1
amBoxplot(regions_unique$nb_nuitees_hotels, xlab=" ", ylab=" ", main="Nombre de nuitées dans des hôtels de tourisme") #1
amBoxplot(regions_unique$depenses_hab, xlab=" ", ylab=" ", main="Dépenses totales par habitant") #2
amBoxplot(regions_unique$nb_etudiants, xlab=" ", ylab=" ", main="Nombre d'étudiants dans la population") #1
amBoxplot(regions_unique$part_etudiants, xlab=" ", ylab=" ", main="Part des étudiants dans la population") #1
amBoxplot(regions_unique$percent_pop_rurale, xlab=" ", ylab=" ", main="Pourcentage de population vivant en zone rurale")


  # Test de Grubbs quand 1 point atypique
library(outliers) 
    # nb_publi
grubbs.test(regions_unique$nb_publi, type=10, two.sided = TRUE)
order(regions_unique$nb_publi)  #outlier qd nb de jeux ouverts ≥ 303, obs n°5 càd Ile de France
    # tertiaire_marchand_VA
grubbs.test(regions_unique$tertiaire_marchand_VA, type=10, two.sided = TRUE)  #outlier quand part ≥ 71.9%, Ile de France
order(regions_unique$tertiaire_marchand_VA)
    # part_diplomes
grubbs.test(regions_unique$part_diplomes, type=10, two.sided = TRUE)  #outlier quand part ≥ 19.9%, Ile de France
order(regions_unique$part_diplomes)
    # nb_crea_entps
grubbs.test(regions_unique$nb_crea_entps, type=10, two.sided = TRUE)  #outlier quand nb ≥ 251781 , Ile de France
order(regions_unique$nb_crea_entps)
    # nb_etudiants
grubbs.test(regions_unique$nb_etudiants, type=10, two.sided = TRUE)  #outlier quand nb ≥ 723217, Ile de France
order(regions_unique$nb_etudiants)
    # part_etudiants
grubbs.test(regions_unique$part_etudiants, type=10, two.sided = TRUE)  #outlier quand part  ≥ 5.9%, Ile de France
order(regions_unique$part_etudiants)


  # Test de Rosner quand plusieurs points atypiques
library(EnvStats) 
    # taux_chomage
rosnerTest(regions_unique$taux_chomage, k = 3, alpha = 0.05) #outlier quand taux ≥ 16.1, Guadeloupe, Guyane, Réunion
    # PIB_habitant
rosnerTest(regions_unique$PIB_habitant, k = 6, alpha = 0.05) #outlier quand PIB/hab ≥ 59387 ou ≤ 14879, Ile de France et Guyane
    # part_plus65
rosnerTest(regions_unique$part_plus65, k = 2, alpha = 0.05) #outlier quand part ≤ 11.1%, Guyane et Réunion
    # niveau_vie
rosnerTest(regions_unique$niveau_vie, k = 3, alpha = 0.05) #outlier quand niveau ≥ 23860 ou ≤ 17880, Ile de France, Martinique, Réunion
    # nb_nuitees_hotels
rosnerTest(regions_unique$nb_nuitees_hotels, k = 4, alpha = 0.05) #outlier quand nb ≥ 70736, Ile de France
    # depenses_hab
rosnerTest(regions_unique$depenses_hab, k = 2, alpha = 0.05) #outlier quand dépenses ≥ 2835, Martinique, Corse


# On retire les points atypiques dans une nouvelle base pour faire une double analyse par la suite
regions_unique_sans_outliers <- regions_unique[-c(1:5,17),]  # Ile de France et DROM
