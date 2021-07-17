library(tidyverse)

# Import de la base
base_unique <- read_csv("./Data/process/base_unique.csv")




#----------- Points atypiques et distribution


ggplot(base_unique, aes(x=nb_publi, y=frequency(nb_publi))) +  
  labs(title="Distribution et box du nombre de publications", x="Nombe de publications", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(base_unique, aes(x=pop_insee, y=frequency(pop_insee))) +
  labs(title="Distribution et box du nombre d'habitants", x="Nombre d'habitants", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(base_unique, aes(x=age_chef, y=frequency(age_chef))) + #1 outlier
  labs(title="Distribution et box de l'âge du chef", x="Âge du chef de l'exécutif", y="Fréquence") +
  geom_violin(trim=FALSE, fill = "#CCCCCC") +
  geom_boxplot(width=0.1) + 
    theme_minimal()
ggplot(base_unique, aes(x=part_plus65, y=frequency(part_plus65))) +  
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



  # Test de Grubbs quand 1 point atypique
library(outliers) 
    # age_chef
grubbs.test(base_unique$age_chef, type=10, two.sided = TRUE)
order(base_unique$age_chef)  #outlier qd age ≤ 33, obs n°297 càd commune de Fréjus


  # Test de Rosner quand plusieurs points atypiques
library(EnvStats) 

# Pour savoir le nombre de points potentiellement atypiques sur le violin plot (k du test de Rosner)
base_unique %>% filter(nb_publi > (summary(base_unique$nb_publi))[5]) %>% count() #132 valeurs > 3è quantile
base_unique %>% filter(pop_insee > (summary(base_unique$pop_insee))[5]) %>% count() #132
base_unique %>% filter(nb_crea_entps > (summary(base_unique$nb_crea_entps))[5]) %>% count() #132
base_unique %>% filter(part_diplomes > (summary(base_unique$part_diplomes))[5]) %>% count() #130
base_unique %>% filter(depenses_hab > (summary(base_unique$depenses_hab))[5]) %>% count() #132

    # nb_publi
options(max.print=9999)
rosnerTest(base_unique$nb_publi, k = 132, alpha = 0.0001) #outlier quand publi ≥ 10
    # pop_insee
rosnerTest(base_unique$pop_insee, k = 132, alpha = 0.05) #outlier quand population ≥ 654829 
    # part_plus65
rosnerTest(base_unique$part_plus65, k = 8, alpha = 0.05) #outlier quand part ≥ 51.6%, obs n°172 càd Arcachon
    # nb_crea_entps
rosnerTest(base_unique$nb_crea_entps, k = 132, alpha = 0.05) #outlier quand créations ≥ 8136
    # part_diplomes
rosnerTest(base_unique$part_diplomes, k = 130, alpha = 0.05) #outlier quand part ≥ 32.4% 
    # depenses_hab
rosnerTest(base_unique$depenses_hab, k = 132, alpha = 0.05) #outlier quand dépenses ≥ 3756.05


# On retire les points atypiques dans une nouvelle base pour faire une double analyse par la suite
base_unique_sans_outliers <- base_unique %>% filter(age_chef > 33,
                                                    nb_publi < 10,
                                                    pop_insee < 654829,
                                                    part_plus65 < 51.6,
                                                    nb_crea_entps < 8136,
                                                    part_diplomes < 32.4,
                                                    depenses_hab < 3756.05)

nrow(base_unique)-nrow(base_unique_sans_outliers) #166 obs perdues
