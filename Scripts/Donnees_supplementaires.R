#-------------------------------  AJOUT DONNEES SOCIO ECO  -------------------------------#

library(tidyverse)


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
departement <- left_join(departement, stats_locales_departement[,-1], by="nom_upper", copy=FALSE)
departement <- departement[,-c(18:19)]
departement <- departement %>% rename(nom = `nom.x`)


            ### B. Régions

# Import jeu recensement 
region <- read_excel("Data/interim/Step3_recherche_manuelle/region_pol.xlsx")

# Noms en majuscules
stats_locales_region$nom_upper <- toupper(stats_locales_region$nom)
region$nom_upper <- toupper(region$nom)

# Match
region <- left_join(region, stats_locales_region[,-1], by="nom_upper", copy=FALSE)
region <- region[,-c(18:19)]
region <- region %>% rename(nom = `nom.x`)


            ### C. CU

# Import jeux recensement 
CU <- read_excel("Data/interim/Step3_recherche_manuelle/communaute_urbaine.xlsx")

# Noms en majuscules et sans accent
    # majuscules
stats_locales_interco$nom_upper <- toupper(stats_locales_interco$nom)
CU$nom_upper <- toupper(CU$nom)
    # pas d'accents
stats_locales_interco <- data.table::data.table(stats_locales_interco)
stats_locales_interco[, nom_upper := stringi::stri_trans_general (str = nom_upper, id = "Latin-ASCII")]
CU <- data.table::data.table(CU)
CU[, nom_upper := stringi::stri_trans_general (str = nom_upper, id = "Latin-ASCII")]

# Match : pour certaines organisations, les noms sont légèrement différents dans les 2 bases donc on harmonise pour qu'ils matchent
stats_locales_interco$nom_upper <- str_replace_all(stats_locales_interco$nom_upper, 
                                                     c("CU ANGERS LOIRE METROPOLE" = "ANGERS LOIRE METROPOLE", 
                                                       "CU DU GRAND REIMS" = "CU GRAND REIMS",
                                                       "GRENOBLE-ALPES-METROPOLE" = "GRENOBLE-ALPES METROPOLE"))
CU <- left_join(CU, stats_locales_interco[,-1], by="nom_upper", copy=FALSE)

# Puis quelques manips pour finaliser (suppression des variables intermediaires)
CU <- CU[,-c(18:19)]
CU <- CU %>% rename(nom = `nom.x`)


            ### C. Métropoles

# Import jeux recensement 
metropole <- read_excel("Data/interim/Step3_recherche_manuelle/metropole_pol.xlsx")

# Noms en majuscules et sans accent
    # majuscules
metropole$nom_upper <- toupper(metropole$nom)
    # pas d'accents
metropole <- data.table::data.table(metropole)
metropole[, nom_upper := stringi::stri_trans_general (str = nom_upper, id = "Latin-ASCII")]

# Match : pour certaines organisations, les noms sont légèrement différents dans les 2 bases donc on harmonise pour qu'ils matchent
metropole <- left_join(metropole, stats_locales_interco[,-1], by="nom_upper", copy=FALSE)

# Puis quelques manips pour finaliser (suppression des variables intermediaires)
metropole <- metropole[,-c(18:19)]
metropole <- metropole %>% rename(nom = `nom.x`)


            ### C. CC

# Import jeux recensement 
CC <- read_excel("Data/raw/CC.xlsx")

# Noms en majuscules et sans accent
    # majuscules
CC$nom_upper <- toupper(CC$nom)
    # pas d'accents
CC <- data.table::data.table(CC)
CC[, nom_upper := stringi::stri_trans_general (str = nom_upper, id = "Latin-ASCII")]

# Match : pour certaines organisations, les noms sont légèrement différents dans les 2 bases donc on harmonise pour qu'ils matchent
CC <- left_join(CC, stats_locales_interco[,-1], by="nom_upper", copy=FALSE)

# Puis quelques manips pour finaliser (suppression des variables intermediaires)
CC <- CC[,-c(18:19)]
CC <- CC %>% rename(nom = `nom.x`)


            ### D. Communes

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
commune <- left_join(commune, stats_locales_commune[,-1], by="nom_upper", copy=FALSE)
commune <- commune[,-c(18:19)]
commune <- commune %>% rename(nom = `nom.x`)



# ------------------------------- MANIPULATIONS DES BASES


# On renomme les variables du jeu initial de recensement
departement <- departement %>% rename(chef_executif = `chef de l'exécutif`, parti_politique = `parti politique`, url_ptf = `url-ptf`, nb_ptf = `nb-ptf`, url_datagouv = `url-datagouv`, nb_datagouv = `nb-datagouv`, id_datagouv = `id-datagouv`, id_ods = `id-ods`, pop_insee = `pop-insee`)
region <- region %>% rename(chef_executif = `chef de l'exécutif`, parti_politique = `parti politique`, url_ptf = `url-ptf`, nb_ptf = `nb-ptf`, url_datagouv = `url-datagouv`, nb_datagouv = `nb-datagouv`, id_datagouv = `id-datagouv`, id_ods = `id-ods`, pop_insee = `pop-insee`)
commune <- commune %>% rename(chef_executif = `chef de l'exécutif`, parti_politique = `parti politique`, url_ptf = `url-ptf`, nb_ptf = `nb-ptf`, url_datagouv = `url-datagouv`, nb_datagouv = `nb-datagouv`, id_datagouv = `id-datagouv`, id_ods = `id-ods`, pop_insee = `pop-insee`)
metropole <- metropole %>% rename(chef_executif = `chef de l'exécutif`, parti_politique = `parti politique`, url_ptf = `url-ptf`, nb_ptf = `nb-ptf`, url_datagouv = `url-datagouv`, nb_datagouv = `nb-datagouv`, id_datagouv = `id-datagouv`, id_ods = `id-ods`, pop_insee = `pop-insee`)
CU <- CU %>% rename(chef_executif = `chef de l'exécutif`, parti_politique = `parti politique`, url_ptf = `url-ptf`, nb_ptf = `nb-ptf`, url_datagouv = `url-datagouv`, nb_datagouv = `nb-datagouv`, id_datagouv = `id-datagouv`, id_ods = `id-ods`, pop_insee = `pop-insee`)


# On supprime les chefs de l'exécutif qui ne servaient qu'en étape intermédiaire pour récuperer le parti politique
departement <- departement[,-3]
region <- region[,-3]
commune <- commune[,-3]
metropole <- metropole[,-3]
CU <- CU[,-3]


# On rassemble les interco ensemble car trop peu d'observations sinon
interco <- merge(metropole, CU, all=TRUE)   # full outer join


# On remplace les valeurs notées "null" dans les données socio éco (stats_locales) par de vrais NAs.
departement[departement == "null"] <- NA
region[region == "null"] <- NA
commune[commune == "null"] <- NA
interco[interco == "null"] <- NA


# On met au bon format les variables
metropole[,c(1,7,9,12,16:20)] <- lapply(metropole[,c(1,7,9,12,16:20)], as.numeric) 
region[,c(1,7,9,12,16:20)] <- lapply(region[,c(1,7,9,12,16:20)], as.numeric) 
interco[,c(1,7,9,12,16:20)] <- lapply(interco[,c(1,7,9,12,16:20)], as.numeric) 
commune[,c(1,7,9,12,16:20)] <- lapply(commune[,c(1,7,9,12,16:20)], as.numeric) 




# ------------------------------- Export des jeux complets

rio::export(metropole, "./Data/process/metropole.csv")
rio::export(region, "./Data/process/region.csv")
rio::export(commune, "./Data/process/commune.csv")
rio::export(interco, "./Data/process/interco.csv")



#-------------------------------  AJOUT BUDGET DEPENSES TOTALES  -------------------------------#





























