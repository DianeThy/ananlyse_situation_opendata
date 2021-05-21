########### RAPPROCHEMENT CODES - COLLECTIVITES ########### 



#----------------------------------------- COMMUNE


# Import jeu des comptes consolidés des communes car numéro SIREN et INSEE y sont
comptes_commune <- read_delim("Data/external/comptes_communes.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# On garde les champs qui nous intéressent 
comptes_commune <- comptes_commune[,c(3,5,15,16,18,20)]

# On les renomme
comptes_commune <- comptes_commune %>% rename(COG = `Code Insee 2020 Commune`,
                                              code_region = `Code Insee 2020 Région`,
                                              code_departement = `Code Insee 2020 Département`,
                                              nom = `Nom 2020 Commune`,
                                              nom_minimise = `Libellé Budget`,
                                              SIREN = `Code Siren Collectivité`)

# On réordonne 
comptes_commune <- comptes_commune[,c(4,6,3,5,1,2)]

# On trie les observations avec COG par ordre croissant
comptes_commune <- comptes_commune %>% arrange(COG)




#----------------------------------------- REGION


# Import comptes consolidés
comptes_region <- read_delim("Data/external/comptes_regions.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# On garde les champs qui nous intéressent 
comptes_region <- comptes_region[,c(4,5,7,8)]

# On les renomme
comptes_region <- comptes_region %>% rename(COG = `Code Insee 2020 Région`,
                                            nom = `Nom 2020 Région`,
                                            nom_minimise = `Libellé Budget`,
                                            SIREN = `Code Siren Collectivité`)

# On réordonne 
comptes_region <- comptes_region[,c(2,4,1,3)]

# On trie les observations avec COG par ordre croissant
comptes_region <- comptes_region %>% arrange(COG)





#----------------------------------------- DEPARTEMENT


# Import comptes consolidés 
comptes_departement <- read_delim("Data/external/comptes_departements.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# On garde les champs qui nous intéressent 
comptes_departement <- comptes_departement[,c(3,7,8,10,11)]

# On les renomme
comptes_departement <- comptes_departement %>% rename(COG = `Code Insee 2020 Département`,
                                                      nom = `Nom 2020 Département`,
                                                      nom_minimise = `Libellé Budget`,
                                                      code_region = `Code Insee 2020 Région`,
                                                      SIREN = `Code Siren Collectivité`)

# On réordonne 
comptes_departement <- comptes_departement[,c(3,5,2,4,1)]

# On trie les observations avec COG par ordre croissant
comptes_departement <- comptes_departement %>% arrange(COG)





#----------------------------------------- INTERCO (juste SIREN et noms)


# Import comptes consolidés 
comptes_interco <- read_delim("Data/external/comptes_EPCI.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# On garde les champs qui nous intéressent 
comptes_interco <- comptes_interco[,c(3,5,12,13,16)]

# On les renomme
comptes_interco <- comptes_interco %>% rename(nom = `Nom 2020 EPCI`,
                                              nom_minimise = `Libellé Budget`,
                                              code_region = `Code Insee 2020 Région`,
                                              code_departement = `Code Insee 2020 Département`,
                                              SIREN = `Code Siren 2020 EPCI`)

# On réordonne 
comptes_interco <- comptes_interco[,c(4,5,3,1,2)]

# On trie les observations avec COG par ordre croissant
comptes_interco <- comptes_interco %>% arrange(nom)




#------------------------------------------- TOUTES COLL


comptes_region$type <- "REG"
comptes_departement$type <- "DEP"
comptes_commune$type <- "COM"

# On regroupe ces 3 nieaux de collectivités pour avoir toutes les infos en une seule base
infos_coll <- rbind(comptes_region,comptes_departement[,c(1:4,6)],comptes_commune[,c(1:4,7)])



#------------------------------------------- EXPORT


# On exporte toutes ces bases qui aideront pour croiser des variables de différents jeux quand les variables pivot ne sont pas les mêmes
#rio::export(comptes_commune,"./Data/external/infos_communes.csv")
#rio::export(comptes_region,"./Data/external/infos_regions.csv")
#rio::export(comptes_departement,"./Data/external/infos_departements.csv")
#rio::export(comptes_interco,"./Data/external/infos_interco.csv")
#rio::export(infos_coll,"./Data/external/infos_collectivites.csv")







