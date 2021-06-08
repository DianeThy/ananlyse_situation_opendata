########### RAPPROCHEMENT CODES - COLLECTIVITES ########### 
library(tidyverse)



#----------------------------------------- REGIONS


# Import comptes consolidés
OFGL_region <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-regions-consolidee/download/?format=csv&disjunctive.reg_name=true&disjunctive.agregat=true&refine.agregat=D%C3%A9penses+totales&refine.exer=2019&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")

# On garde les champs qui nous intéressent 
OFGL_region <- OFGL_region[,c(4,5,7,8)]

# On les renomme
OFGL_region <- OFGL_region %>% rename(COG = `Code Insee 2020 Région`,
                                      nom = `Nom 2020 Région`,
                                      nom_minimise = `Libellé Budget`,
                                      SIREN = `Code Siren Collectivité`)

# On réordonne 
OFGL_region <- OFGL_region[,c(2,4,1,3)]

# On trie les observations avec COG par ordre croissant
OFGL_region <- OFGL_region %>% arrange(COG)





#----------------------------------------- DEPARTEMENTS


# Import comptes consolidés 
OFGL_departement <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-departements-consolidee/download/?format=csv&disjunctive.reg_name=true&disjunctive.dep_tranche_population=true&disjunctive.dep_name=true&disjunctive.agregat=true&refine.exer=2019&refine.agregat=D%C3%A9penses+totales&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")

# On garde les champs qui nous intéressent 
OFGL_departement <- OFGL_departement[,c(3,7,8,10,11)]

# On les renomme
OFGL_departement <- OFGL_departement %>% rename(COG = `Code Insee 2020 Département`,
                                                nom = `Nom 2020 Département`,
                                                nom_minimise = `Libellé Budget`,
                                                code_region = `Code Insee 2020 Région`,
                                                SIREN = `Code Siren Collectivité`)

# On réordonne 
OFGL_departement <- OFGL_departement[,c(3,5,2,4,1)]

# On trie les observations avec COG par ordre croissant
OFGL_departement <- OFGL_departement %>% arrange(COG)





#----------------------------------------- COMMUNES


# Import jeu des comptes consolidés des communes car numéro SIREN et INSEE y sont
OFGL_commune <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-communes-consolidee/download/?format=csv&disjunctive.reg_name=true&disjunctive.dep_name=true&disjunctive.epci_name=true&disjunctive.tranche_population=true&disjunctive.tranche_revenu_imposable_par_habitant=true&disjunctive.com_name=true&disjunctive.agregat=true&refine.exer=2019&refine.agregat=D%C3%A9penses+totales&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")

# On garde les champs qui nous intéressent 
OFGL_commune <- OFGL_commune[,c(3,5,15,16,18,20)]

# On les renomme
OFGL_commune <- OFGL_commune %>% rename(COG = `Code Insee 2020 Commune`,
                                              code_region = `Code Insee 2020 Région`,
                                              code_departement = `Code Insee 2020 Département`,
                                              nom = `Nom 2020 Commune`,
                                              nom_minimise = `Libellé Budget`,
                                              SIREN = `Code Siren Collectivité`)

# On réordonne 
OFGL_commune <- OFGL_commune[,c(4,6,3,5,1,2)]

# On trie les observations avec COG par ordre croissant
OFGL_commune <- OFGL_commune %>% arrange(COG)





#------------------------------------------- TOUTES COLL


OFGL_region$type <- "REG"
OFGL_departement$type <- "DEP"
OFGL_commune$type <- "COM"

# On regroupe ces 3 nieaux de collectivités pour avoir toutes les infos en une seule base
infos_coll <- rbind(OFGL_region,OFGL_departement[,c(1:4,6)],OFGL_commune[,c(1:4,7)])




#----------------------------------------- INTERCO (juste SIREN et noms)


# Import comptes consolidés 
OFGL_interco <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-gfp-consolidee/download/?format=csv&disjunctive.dep_name=true&disjunctive.gfp_tranche_population=true&disjunctive.nat_juridique=true&disjunctive.mode_financement=true&disjunctive.gfp_tranche_revenu_imposable_par_habitant=true&disjunctive.epci_name=true&disjunctive.agregat=true&refine.exer=2019&refine.agregat=D%C3%A9penses+totales&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")

# On garde les champs qui nous intéressent 
OFGL_interco <- OFGL_interco[,c(3,5,12,13,16)]

# On les renomme
OFGL_interco <- OFGL_interco %>% rename(nom = `Nom 2020 EPCI`,
                                              nom_minimise = `Libellé Budget`,
                                              code_region = `Code Insee 2020 Région`,
                                              code_departement = `Code Insee 2020 Département`,
                                              SIREN = `Code Siren 2020 EPCI`)

# On réordonne 
OFGL_interco <- OFGL_interco[,c(4,5,3,1,2)]

# On trie les observations avec COG par ordre croissant
OFGL_interco <- OFGL_interco %>% arrange(nom)




#------------------------------------------- EXPORT


# On exporte toutes ces bases qui aideront pour croiser des variables de différents jeux quand les variables pivot ne sont pas les mêmes
#rio::export(OFGL_commune,"./Data/external/infos_communes.csv")
#rio::export(OFGL_region,"./Data/external/infos_regions.csv")
#rio::export(OFGL_departement,"./Data/external/infos_departements.csv")
#rio::export(OFGL_interco,"./Data/external/infos_interco.csv")
#rio::export(infos_coll,"./Data/external/infos_collectivites.csv")







