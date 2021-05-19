#-------------------------------  AJOUT DONNEES SOCIO ECO  -------------------------------#

library(tidyverse)
library(readxl)
library(stringdist)
library(fuzzyjoin)
library(tm)


    # PACKAGE INSEE
library(insee)
dataset_list <- get_dataset_list()


    # APPEL API INSEE 
    # (https://api.insee.fr/catalogue/site/themes/wso2/subthemes/insee/pages/item-info.jag?name=DonneesLocales&version=V0.1&provider=insee)
library(httr)
res = GET("https://api.insee.fr/donnees-locales/V0.1")
res
rawToChar(res$content)
repo_content <- content(res)
repo_content

username <- "DiaThy"
password <- "MaiJuin2018I"
base <- "https://api.insee.fr/donnees-locales/V0.1"

insee_query = file.path(insee_link)
data = get_insee(insee_query)




    # DOWNLOADED DATA
stats_locales_departement <- read_delim("Data/external/stats_locales_departement.csv", ";", escape_double = FALSE, trim_ws = TRUE, skip = 2)
stats_locales_region <- read_delim("Data/external/stats_locales_region.csv", ";", escape_double = FALSE, trim_ws = TRUE, skip = 2)
stats_locales_interco <- read_delim("Data/external/stats_locales_interco.csv", ";", escape_double = FALSE, trim_ws = TRUE, skip = 2)
stats_locales_commune <- read_delim("Data/external/stats_locales_commune.csv", ";", escape_double = FALSE, trim_ws = TRUE, skip = 2)

# On renomme les colonnes
    # départements
names(stats_locales_departement)
stats_locales_departement <- stats_locales_departement %>% 
    rename(nom = Libellé,
           part_plus65 = `Part des pers. âgées de 65 ans ou + 2017`,
           niveau_vie = `Médiane du niveau de vie 2018`,
           part_diplomes = `Part des diplômés d'un BAC+5 ou plus dans la pop. non scolarisée de 15 ans ou + 2017`,
           taux_chomage = `Taux de chômage annuel moyen 2019`,
           nb_crea_entps = `Nb créations d'entreprises 2020`,
           nb_nuitees_hotels = `Nb de nuitées dans les hôtels de tourisme 2019`,
           flux_migration_res = `Flux principal de migration résidentielle 2017`)

    # regions
names(stats_locales_region)
stats_locales_region <- stats_locales_region %>% 
    rename(nom = Libellé,
           part_plus65 = `Part des pers. âgées de 65 ans ou + 2017`,
           niveau_vie = `Médiane du niveau de vie 2018`,
           part_diplomes = `Part des diplômés d'un BAC+5 ou plus dans la pop. non scolarisée de 15 ans ou + 2017`,
           taux_chomage = `Taux de chômage annuel moyen 2019`,
           nb_crea_entps = `Nb créations d'entreprises 2020`,
           nb_nuitees_hotels = `Nb de nuitées dans les hôtels de tourisme 2019`,
           flux_migration_res = `Flux principal de migration résidentielle 2017`,
           PIB_habitant = `Produit intérieur brut par habitant 2018`,
           tertiaire_VA = `Part du tertiaire marchand dans la VA 2018`)

    # intercommunalitées
names(stats_locales_interco)
stats_locales_interco <- stats_locales_interco %>% 
    rename(nom = Libellé,
           part_plus65 = `Part des pers. âgées de 65 ans ou + 2017`,
           niveau_vie = `Médiane du niveau de vie 2018`,
           part_diplomes = `Part des diplômés d'un BAC+5 ou plus dans la pop. non scolarisée de 15 ans ou + 2017`,
           nb_crea_entps = `Nb créations d'entreprises 2020`,
           flux_migration_res = `Flux principal de migration résidentielle 2017`)

    # communes
names(stats_locales_commune)
stats_locales_commune <- stats_locales_commune %>% 
    rename(nom = Libellé,
           part_plus65 = `Part des pers. âgées de 65 ans ou + 2017`,
           part_diplomes = `Part des diplômés d'un BAC+5 ou plus dans la pop. non scolarisée de 15 ans ou + 2017`,
           nb_crea_entps = `Nb créations d'entreprises 2020`,
           flux_migration_res = `Flux principal de migration résidentielle 2017`)




#------------------------------- MATCH AVEC JEU RECENSEMENT + COULEUR PO (sauf CA et CC)



            ### A. Départements

# Import jeu recensement 
departement <- read_excel("Data/interim/Step3_recherche_manuelle/departement_pol2.xlsx")

# On met les noms des organisations en majuscules pour le match
stats_locales_departement$nom_upper <- toupper(stats_locales_departement$nom)
departement$nom_upper <- toupper(departement$nom)

# Match
departement <- left_join(departement, stats_locales_departement[,-c(1:2)], by="nom_upper", copy=FALSE)
departement <- departement[,-18]   # on retire la variable des noms en majuscule


            ### B. Régions

# Import jeu recensement 
region <- read_excel("Data/interim/Step3_recherche_manuelle/region_pol.xlsx")

# Noms en majuscules
stats_locales_region$nom_upper <- toupper(stats_locales_region$nom)
region$nom_upper <- toupper(region$nom)

# Match
region <- left_join(region, stats_locales_region[,-c(1:2)], by="nom_upper", copy=FALSE)
region <- region[,-18]


            ### C. CU

# Import jeux recensement 
CU <- read_excel("Data/interim/Step3_recherche_manuelle/communaute_urbaine.xlsx")

# Noms en majuscules
CU$nom_upper <- toupper(CU$nom)
stats_locales_interco$nom_upper <- toupper(stats_locales_interco$nom)

# Match inexact quand les noms d'organisation ne correspondent pas tout à fait dans les 2 bases
CU <- stringdist_left_join(CU, stats_locales_interco[,-c(1:2)], by="nom_upper", max_dist = 5, distance_col="distance")
CU <- CU[,-c(18,24,25)]  # on retire les colonnes "nom_upper" qui servaient juste en étape intermédiaire et la distance des mots pour le match


            ### D. Métropoles

# Import jeux recensement 
metropole <- read_excel("Data/interim/Step3_recherche_manuelle/metropole_pol.xlsx")

# Noms en majuscules
metropole$nom_upper <- toupper(metropole$nom)

# Match inexact
metropole <- stringdist_left_join(metropole, stats_locales_interco[,-c(1:2)], by="nom_upper", max_dist = 7, distance_col="distance")  %>% 
                    group_by(nom) %>% slice_min(distance)
metropole <- metropole[,-c(18,24,25)]


            ### E. CC  (2 col en moins : parti_politique et chef_executif)

# Import jeux recensement 
CC <- read_excel("Data/raw/CC.xlsx")

# On fait en sorte deminimiser les écarts entre les nos des coll pour optimiser les chances de match 
    # noms en majuscules
CC$nom_upper <- toupper(CC$nom)
    # sans accents
CC <- data.table::data.table(CC)
CC[, nom_upper := stringi::stri_trans_general (str = nom_upper, id = "Latin-ASCII")]
stats_locales_interco <- data.table::data.table(stats_locales_interco)      # de même pour le jeu à matcher
stats_locales_interco[, nom_upper := stringi::stri_trans_general (str = nom_upper, id = "Latin-ASCII")]
    # on retire les mots communs pour limiter écarts de match
stopwords = c("CC ", "CC DU ", "CC DE ", "CC DE LA ", "PAYS DE ", "PAYS DU ", "PAYS DES ", " COMMUNAUTE")
CC$nom_upper  =  removeWords(CC$nom_upper, stopwords)
stats_locales_interco$nom_upper <- removeWords(stats_locales_interco$nom_upper, stopwords)  

# Match inexact
CC <- stringdist_left_join(CC, stats_locales_interco[,-c(1:2)], by="nom_upper", max_dist = 4, distance_col="distance") 
    # qd pas de match on remplace la distance 'NA' par '0' pour ensuite garder 1 obs par CC et qu'il garde aussi les non matchs
CC$distance[is.na(CC$distance)] = 0
    # ensuite on ne garde qu'une obs par coll
CC <- CC %>% group_by(nom) %>% slice_min(distance)  
CC <- CC %>% group_by(nom) %>% distinct(distance, .keep_all = TRUE)
    # on clean
CC[c(5,19,42),17:23] = NA   #certaines obs ont matché mais en réalité ne sont pas (ex: PAYS MORNANTAIS vs. PAYS MORCENAIS) dc on remet NA.
CC <- CC[,-c(16,22,23)]     # on enlève les colonnes en trop (qui servaient en étape intermediaire)


            ### F. CA (idem)

# Import jeux recensement 
CA <- read_excel("Data/raw/CA.xlsx")

# On limite la casse
    # majuscules
CA$nom_upper <- toupper(CA$nom)
    # accents
CA <- data.table::data.table(CA)
CA[, nom_upper := stringi::stri_trans_general (str = nom_upper, id = "Latin-ASCII")]
    # stopwords
stopwords = c("CA ", "CA DE ", "CA DU ", " AGGLOMERATION", "CA DE LA ", "CA PAYS DE ")
CA$nom_upper  =  removeWords(CA$nom_upper, stopwords)
stats_locales_interco$nom_upper <- removeWords(stats_locales_interco$nom_upper, stopwords)  

# Match inexact
CA <- stringdist_left_join(CA, stats_locales_interco[,-c(1:2)], by="nom_upper", max_dist = 4, distance_col="distance") 
    # qd pas de match on remplace la distance 'NA' par '0' pour ensuite garder 1 obs par CA et qu'il garde aussi les non matchs
CA$distance[is.na(CA$distance)] = 0
    # ensuite on ne garde qu'une obs par coll
CA <- CA %>% group_by(nom) %>% slice_min(distance)  
CA <- CA %>% group_by(nom) %>% distinct(distance, .keep_all = TRUE)
    # on clean
CA[c(33,54),17:23] = NA 
CA <- CA[,-c(16,22,23)]


            ### G. Communes

# Import jeu recensement 
commune <- read_excel("Data/interim/Step3_recherche_manuelle/commune_pol_ajout_listesPo_datagouv.xlsx")

# Noms en majuscules et sans accents + on enlève les doublons
    # majuscules
stats_locales_commune$nom_upper <- toupper(stats_locales_commune$nom)
commune$nom_upper <- toupper(commune$nom)
    # pas d'accents
stats_locales_commune <- data.table::data.table(stats_locales_commune)
stats_locales_commune[, nom_upper := stringi::stri_trans_general (str = nom_upper, id = "Latin-ASCII")]
commune <- data.table::data.table(commune)
commune[, nom_upper := stringi::stri_trans_general (str = nom_upper, id = "Latin-ASCII")]
    # distinct() sur données additionnelles car certaines en double (ex: La Rochelle, La Rochette ...)
stats_locales_commune <- stats_locales_commune %>% distinct(nom , .keep_all=TRUE)

# Match
commune <- left_join(commune, stats_locales_commune[,-c(1:2)], by="nom_upper", copy=FALSE)
commune <- commune[,-18]




# ------------------------------- MANIPULATIONS DES BASES


# On renomme les variables du jeu initial de recensement
departement <- departement %>% rename(chef_executif = `chef de l'exécutif`, parti_politique = `parti politique`, url_ptf = `url-ptf`, nb_ptf = `nb-ptf`, url_datagouv = `url-datagouv`, nb_datagouv = `nb-datagouv`, id_datagouv = `id-datagouv`, id_ods = `id-ods`, pop_insee = `pop-insee`)
region <- region %>% rename(chef_executif = `chef de l'exécutif`, parti_politique = `parti politique`, url_ptf = `url-ptf`, nb_ptf = `nb-ptf`, url_datagouv = `url-datagouv`, nb_datagouv = `nb-datagouv`, id_datagouv = `id-datagouv`, id_ods = `id-ods`, pop_insee = `pop-insee`)
commune <- commune %>% rename(chef_executif = `chef de l'exécutif`, parti_politique = `parti politique`, url_ptf = `url-ptf`, nb_ptf = `nb-ptf`, url_datagouv = `url-datagouv`, nb_datagouv = `nb-datagouv`, id_datagouv = `id-datagouv`, id_ods = `id-ods`, pop_insee = `pop-insee`)
metropole <- metropole %>% rename(chef_executif = `chef de l'exécutif`, parti_politique = `parti politique`, url_ptf = `url-ptf`, nb_ptf = `nb-ptf`, url_datagouv = `url-datagouv`, nb_datagouv = `nb-datagouv`, id_datagouv = `id-datagouv`, id_ods = `id-ods`, pop_insee = `pop-insee`)
CU <- CU %>% rename(chef_executif = `chef de l'exécutif`, parti_politique = `parti politique`, url_ptf = `url-ptf`, nb_ptf = `nb-ptf`, url_datagouv = `url-datagouv`, nb_datagouv = `nb-datagouv`, id_datagouv = `id-datagouv`, id_ods = `id-ods`, pop_insee = `pop-insee`)
CA <- CA %>% rename(url_ptf = `url-ptf`, nb_ptf = `nb-ptf`, url_datagouv = `url-datagouv`, nb_datagouv = `nb-datagouv`, id_datagouv = `id-datagouv`, id_ods = `id-ods`, pop_insee = `pop-insee`)
CC <- CC %>% rename(url_ptf = `url-ptf`, nb_ptf = `nb-ptf`, url_datagouv = `url-datagouv`, nb_datagouv = `nb-datagouv`, id_datagouv = `id-datagouv`, id_ods = `id-ods`, pop_insee = `pop-insee`)


# On supprime les chefs de l'exécutif qui ne servaient qu'en étape intermédiaire pour récuperer le parti politique
departement <- departement[,-3]
region <- region[,-3]
commune <- commune[,-3]
metropole <- metropole[,-3]
CU <- CU[,-3]


# On regroupe les interco 2 à 2 (avec et sans parti politique)
CU_metropole <- merge(metropole, CU, all=TRUE)   # full outer join
CA_CC <- merge(CA, CC, all=TRUE)   


# On remplace les valeurs notées "null" dans les données socio éco (stats_locales) par de vrais NAs.
departement[departement == "null"] <- NA
region[region == "null"] <- NA
commune[commune == "null"] <- NA
CU_metropole[CU_metropole == "null"] <- NA
CA_CC[CA_CC == "null"] <- NA


# On met au bon format les variables
metropole[,c(1,7,9,12,16:21)] <- lapply(metropole[,c(1,7,9,12,16:21)], as.numeric) 
region[,c(1,7,9,12,16:25)] <- lapply(region[,c(1,7,9,12,16:25)], as.numeric) 
commune[,c(1,7,9,12,16:20)] <- lapply(commune[,c(1,7,9,12,16:20)], as.numeric) 
CU_metropole[,c(1,7,9,12,16:21)] <- lapply(CU_metropole[,c(1,7,9,12,16:21)], as.numeric) 
CA_CC[,c(1,7,8,11,15:20)] <- lapply(CA_CC[,c(1,7,8,11,15:20)], as.numeric) 




# ------------------------------- Export des jeux complets

rio::export(metropole, "./Data/process/metropole.csv")
rio::export(region, "./Data/process/region.csv")
rio::export(commune, "./Data/process/commune.csv")
rio::export(interco, "./Data/process/interco.csv")
rio::export(departement, "./Data/process/departement.csv")




#-------------------------------  AJOUT BUDGET : DEPENSES TOTALES 2019  -------------------------------#



# Import des bases
comptes_departement <- read_delim("Data/external/comptes_departements.csv", ";", escape_double = FALSE, trim_ws = TRUE)
comptes_region <- read_delim("Data/external/comptes_regions.csv", ";", escape_double = FALSE, trim_ws = TRUE)
comptes_commune <- read_delim("Data/external/comptes_communes.csv", ";", escape_double = FALSE, trim_ws = TRUE)
comptes_interco <- read_delim("Data/external/comptes_EPCI.csv", ";", escape_double = FALSE, trim_ws = TRUE)


# La colonne qui nous intérèsse est celle du budget "montant en euros par habitant"
comptes_departement <- comptes_departement[,c(10,16)] # on garde SIREN et budget
comptes_commune <- comptes_commune[,c(10,16)]


# Match départements
departement <- left_join(departement, comptes_departement, by="nom_upper", copy=FALSE)





















