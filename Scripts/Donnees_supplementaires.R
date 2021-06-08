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
usethis::edit_r_environ("user")

insee_query = file.path(insee_link)
data = get_insee(insee_query)




    # TEST PACKAGE INSEELOCALDATA

library(inseeLocalData)     # remotes::install_github("inseefrlab/inseeLocalData")

croisement <- "NA5_B-ENTR_INDIVIDUELLE"
jeu_donnees <- "GEO2017REE2017"
nivgeo <- "COM"
codgeo <- "51108"
modalite <- "all.all"

donneesAPI <- get_dataset(jeton = "DiaThy", jeu_donnees, croisement, modalite, nivgeo, codgeo)

donnees <- donneesAPI$donnees # pour accéder aux données
liste_code <- donneesAPI$liste_code # pour accéder aux nomenclatures
info_zone <- donneesAPI$info_zone # pour accéder aux données géographiques
source <- donneesAPI$source # pour accéder à la source





    # TEST AUTRE API COMPTES CONSOLIDES

api = GET("https://api/records/1.0/search/?dataset=ofgl-base-communes-consolidee&q=&sort=exer&facet=exer&facet=reg_name&facet=dep_name&facet=epci_name&facet=qpv&facet=com_name&facet=agregat")



    # URL DOSSIER COMPLET STATISTIQUES LOCALES COMMUNES

stats_locales_com <- read_delim("https://www.insee.fr/fr/statistiques/fichier/5359146/dossier_complet.zip", ";", escape_double = FALSE, trim_ws = TRUE)





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
departement <- read_excel("Data/interim/Harmonisation_politique/departement.xlsx")

# On met les noms des organisations en majuscules pour le match
stats_locales_departement$nom_upper <- toupper(stats_locales_departement$nom)
departement$nom_upper <- toupper(departement$nom)

# Match
departement <- left_join(departement, stats_locales_departement[,-c(1:2)], by="nom_upper", copy=FALSE)


            ### B. Régions

# Import jeu recensement 
region <- read_excel("Data/interim/Harmonisation_politique/region.xlsx")

# Noms en majuscules
stats_locales_region$nom_upper <- toupper(stats_locales_region$nom)
region$nom_upper <- toupper(region$nom)

# Match
region <- left_join(region, stats_locales_region[,-c(1:2)], by="nom_upper", copy=FALSE)


            ### C. CU

# Import jeux recensement 
CU <- read_excel("Data/interim/Harmonisation_politique/communaute_urbaine.xlsx")

# Noms en majuscules
CU$nom_upper <- toupper(CU$nom)
stats_locales_interco$nom_upper <- toupper(stats_locales_interco$nom)

# Match inexact quand les noms d'organisation ne correspondent pas tout à fait dans les 2 bases
CU <- stringdist_left_join(CU, stats_locales_interco[,-c(1:2)], by="nom_upper", max_dist = 5, distance_col="distance")
CU <- CU[,-c(24,25)]  # on retire une des 2 colonnes "nom_upper" qui servait d'étape intermédiaire et la distance des mots pour le match
CU <- CU %>% rename(nom_upper = nom_upper.x)


            ### D. Métropoles

# Import jeux recensement 
metropole <- read_excel("Data/interim/Harmonisation_politique/metropole.xlsx")

# Noms en majuscules
metropole$nom_upper <- toupper(metropole$nom)

# Match inexact
metropole <- stringdist_left_join(metropole, stats_locales_interco[,-c(1:2)], by="nom_upper", max_dist = 7, distance_col="distance")  %>% 
                    group_by(nom) %>% slice_min(distance)
metropole <- metropole[,-c(24,25)]
metropole <- metropole %>% rename(nom_upper = nom_upper.x)


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
CC <- CC[,-c(22,23)]     # on enlève les colonnes en trop (qui servaient en étape intermediaire)
CC <- CC %>% rename(nom_upper = nom_upper.x)


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
CA <- CA[,-c(22,23)]
CA <- CA %>% rename(nom_upper = nom_upper.x)


            ### G. Communes

# Import jeu recensement 
commune <- read_excel("Data/interim/Harmonisation_politique/commune.xlsx")

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
metropole[,c(1,7,9,12,16,18:21)] <- lapply(metropole[,c(1,7,9,12,16,18:21)], as.numeric) 
region[,c(1,7,9,12,16,18:25)] <- lapply(region[,c(1,7,9,12,16,18:25)], as.numeric) 
commune[,c(1,7,9,12,16,18:20)] <- lapply(commune[,c(1,7,9,12,16,18:20)], as.numeric) 
CU_metropole[,c(1,7,9,12,16,18:21)] <- lapply(CU_metropole[,c(1,7,9,12,16,18:21)], as.numeric) 
CA_CC[,c(1,6,8,11,15,17:21)] <- lapply(CA_CC[,c(1,6,8,11,15,17:21)], as.numeric) 




# ------------------------------- Export des jeux



#rio::export(region, "./Data/interim/Ajout_stats_locales_insee/region.xlsx")
#rio::export(departement, "./Data/interim/Ajout_stats_locales_insee/departement.xlsx")
#rio::export(commune, "./Data/interim/Ajout_stats_locales_insee/commune.xlsx")
#rio::export(CU_metropole, "./Data/interim/Ajout_stats_locales_insee/CU_metropole.xlsx")
#rio::export(CA_CC, "./Data/interim/Ajout_stats_locales_insee/CA_CC.xlsx")




#-------------------------------  AJOUT BUDGET : DEPENSES TOTALES 2019  -------------------------------#



# Import des bases
comptes_departement <- read_delim("Data/external/comptes_departements.csv", ";", escape_double = FALSE, trim_ws = TRUE)
comptes_region <- read_delim("Data/external/comptes_regions.csv", ";", escape_double = FALSE, trim_ws = TRUE)
comptes_commune <- read_delim("Data/external/comptes_communes.csv", ";", escape_double = FALSE, trim_ws = TRUE)
comptes_interco <- read_delim("Data/external/comptes_EPCI.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# On renomme la colonne du SIREN dans le jeu du budget (colonne pivot)
comptes_departement <- comptes_departement %>% rename(siren = `Code Siren Collectivité`)
comptes_region <- comptes_region %>% rename(siren = `Code Siren Collectivité`)
comptes_commune <- comptes_commune %>% rename(siren = `Code Siren Collectivité`)
comptes_interco <- comptes_interco %>% rename(siren = `Code Siren Collectivité`)

# Matchs
departement <- left_join(departement, comptes_departement[,c(10,16)], by="siren", copy=FALSE)  #du jeu comptes on ne prend que le siren et le budget
region <- left_join(region, comptes_region[,c(7,13)], by="siren", copy=FALSE)
commune <- left_join(commune, comptes_commune[,c(18,25)], by="siren", copy=FALSE)
CA_CC <- left_join(CA_CC, comptes_interco[,c(15,21)], by="siren", copy=FALSE)
CU_metropole <- left_join(CU_metropole, comptes_interco[,c(15,21)], by="siren", copy=FALSE)

# On renomme la colonne du budget par un nom exploitable
region <- region %>% rename(depenses_hab = `Montant en € par habitant`)
departement <- departement %>% rename(depenses_hab = `Montant en € par habitant`)
commune <- commune %>% rename(depenses_hab = `Montant en € par habitant`)
CA_CC <- CA_CC %>% rename(depenses_hab = `Montant en € par habitant`)
CU_metropole <- CU_metropole %>% rename(depenses_hab = `Montant en € par habitant`)





#-------------------------------  AJOUT NB ETUDIANTS DANS LA POP  -------------------------------#


# Import de la base du nombre d'étudiants
Nb_etudiants_pop <- read_delim("Data/external/Nb_etudiants_pop.csv", ";", escape_double = FALSE, trim_ws = TRUE)
    # on garde nom coll et variable nb d'étudiants
Nb_etudiants_pop <- Nb_etudiants_pop[, c(4,11)]
    # on renomme les colonnes
Nb_etudiants_pop <- Nb_etudiants_pop %>% rename(nom = `Unité géographique`,
                                                nb_etudiants = `Nombre total d’étudiants inscrits`)

# Préparation des jeux au match 
    # noms en majuscules et sans accents
Nb_etudiants_pop$nom_upper <- toupper(Nb_etudiants_pop$nom)  # maj
Nb_etudiants_pop <- data.table::data.table(Nb_etudiants_pop)   # accents
Nb_etudiants_pop[, nom_upper := stringi::stri_trans_general (str = nom_upper, id = "Latin-ASCII")]
        # régions
region <- data.table::data.table(region)   # accents pour le jeu initial des régions
region[, nom_upper := stringi::stri_trans_general (str = nom_upper, id = "Latin-ASCII")]
        # départements
departement <- data.table::data.table(departement)   # accents pour le jeu initial des départements
departement[, nom_upper := stringi::stri_trans_general (str = nom_upper, id = "Latin-ASCII")]
    # pour l'instant pour 1 coll il ya plusieurs obs : nb etudiants par sexe / par nature d'établissement 
    # donc on additionne les chiffres par coll
Nb_etudiants_pop <- Nb_etudiants_pop %>% group_by(nom_upper) %>% summarise_at(vars(nb_etudiants), list(nb_etudiants = sum))

# Matchs pour les niveaux géographiques dispo (regions, dep et communes)
region <- left_join(region, Nb_etudiants_pop, by="nom_upper", copy=FALSE)
departement <- left_join(departement, Nb_etudiants_pop, by="nom_upper", copy=FALSE)
commune <- left_join(commune, Nb_etudiants_pop, by="nom_upper", copy=FALSE)


# On transforme le nombre d'étudiants en taux pour que ça soit plus parlant
region <- region %>% mutate(part_etudiants = nb_etudiants/pop_insee*100)
region$part_etudiants <- round(region$part_etudiants,1)
departement <- departement %>% mutate(part_etudiants = nb_etudiants/pop_insee*100)
departement$part_etudiants <- round(departement$part_etudiants,1)
commune <- commune %>% mutate(part_etudiants = nb_etudiants/pop_insee*100)
commune$part_etudiants <- round(commune$part_etudiants,1)

# On supprime la colonne 'nb_etudiants' maintenant qu'on a le pourcentage
region <- region[,-28]
departement <- departement[,-26]
commune <- commune[,-23]





#-------------------------------  AJOUT URBANISATION  -------------------------------#



# Import de la base à matcher (urbain / rural en 5 modalités)
urbanisation_commune <- read_csv("Data/external/urbanisation_commune.csv")
    # on renomme les colonnes
urbanisation_commune <- urbanisation_commune %>% rename(COG = `Code géographique communal`,
                                                        niveau_rural = `Typologie urbain/rural`)

# Import des infos sur communes pour récuperer SIREN et matcher avec jeux initiaux
infos_commune <- read_csv("Data/external/infos_communes.csv")
    # on renomme SIREN en siren en vu du match
infos_commune <- infos_commune %>% rename(siren = SIREN)

# On ajoute la colonne du numéro SIREN pour matcher avec le jeu de l'analyse
urbanisation_commune <- left_join(urbanisation_commune, infos_commune[,3:4], by="COG", copy=FALSE)
    
# Match avec le jeu de l'analyse
commune <- left_join(commune, urbanisation_commune[,2:3], by="siren", copy=FALSE)



        ### On élève au niveau région et département


# On récupère les colonnes du COG département et niveau rural / urbain
urbanisation_departement <- commune[,c(16,25)] %>% arrange(depcode)

# On remplace 
Mode <- function(x) {
  ux <- unique(x)
  if(!anyDuplicated(x)){
      NA_character_ } else { 
     tbl <-   tabulate(match(x, ux))
     toString(ux[tbl==max(tbl)])
 }
}

urbanisation_departement <- urbanisation_departement %>% group_by(depcode) %>% mutate(Mode = Mode(niveau_rural))  #qd 1 obs / dep, renvoit NA et qd 2 possibilités (pas possible de trancher) renvoit NA aussi
urbanisation_departement$Mode[is.na(urbanisation_departement$Mode)] <- as.character(urbanisation_departement$niveau_rural[is.na(urbanisation_departement$Mode)])  # on affecte aux NA les valeurs du niveau_rural
urbanisation_departement <- unique(urbanisation_departement[,c(1,3)])
table(urbanisation_departement$depcode)   # 2 valeurs pour les départements n°4 et 49
urbanisation_departement <- urbanisation_departement %>% distinct(depcode, .keep_all=T)

# On affecte ces données au jeu des départements par un match via 'depcode'
departement <- left_join(departement, urbanisation_departement, by="depcode", copy=F)



            ### Même chose au niveau région


# On récupère les colonnes du COG département et niveau rural / urbain
urbanisation_region <- departement[,c(5,27)] %>% arrange(regcode)

# On remplace 
urbanisation_region <- urbanisation_region %>% group_by(regcode) %>% mutate(Mode2 = Mode(Mode))
urbanisation_region$Mode2[is.na(urbanisation_region$Mode2)] <- as.character(urbanisation_region$Mode[is.na(urbanisation_region$Mode2)])
urbanisation_region <- unique(urbanisation_region[,c(1,3)])

# On affecte ces données au jeu des départements par un match via 'regcode'
region <- left_join(region, urbanisation_region, by="regcode", copy=F)




# On exporte les bases complètes, avec toutes les informations nécessaires à l'analyse !
rio::export(region, "./Data/process/region.csv")
rio::export(departement, "./Data/process/departement.csv")
rio::export(commune, "./Data/process/commune.csv")
rio::export(CU_metropole, "./Data/process/CU_metropole.csv")
rio::export(CA_CC, "./Data/process/CA_CC.csv")










