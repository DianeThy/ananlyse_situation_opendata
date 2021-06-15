#### NOUVEAU SCRIPT POUR CONSTRUIRE LA BASE EN NE PASSANT QUE PAR DES LIENS POUR SIMPLIFIER LA MISE EN OEUVRE



#------------------------------- ON PART D'UN FICHIER AVEC TOUTES LES ORGAS DE FRANCE


# Fichier initial : toutes les orgas de France (collectivités + EPCI)
library(tidyverse)
regions <- read_csv("Data/external/infos_regions.csv")
departements <- read_csv("Data/external/infos_departements.csv")
communes <- read_csv("Data/external/infos_communes.csv")
epci <- read_csv("Data/external/infos_interco.csv")


# On ajoute les données de l'observatoire des territoires (nombre de jeux ouverts)

# Import de la base complète
observatoire_opendata_territoire <- read_csv("https://git.opendatafrance.net/observatoire/observatoire-data/-/raw/master/organizations.csv?inline=false")
  # on renomme la colonne de jointure et on la passe au format numérique
observatoire_opendata_territoire <- observatoire_opendata_territoire %>% rename(SIREN = siren)
observatoire_opendata_territoire$SIREN <- as.numeric(observatoire_opendata_territoire$SIREN)

# On sépare la base selon le type d'organisation
observatoire_reg <- observatoire_opendata_territoire %>% filter(type == "REG") #15
observatoire_dep <- observatoire_opendata_territoire %>% filter(type == "DEP") #61
observatoire_com <- observatoire_opendata_territoire %>% filter(type == "COM")  #351 obs
observatoire_epci <- observatoire_opendata_territoire %>% filter(type == "MET" | type == "CU" | type == "CC" | type == "CA")  # 17+5+43+99 = 164

# On match par le numéro (SIREN, INSEE)
regions <- left_join(regions, observatoire_reg[,-c(2:3)], by="SIREN", copy=FALSE)
departements <- left_join(departements, observatoire_dep[,-c(2:3)], by="SIREN", copy=FALSE)
communes <- left_join(communes, observatoire_com[,-c(2:3)], by="SIREN", copy=FALSE)

# Pour les EPCI on se rend compte qu'il y a des doublons donc on les supprime avant de matcher
doublons <- as.data.frame(table(epci$SIREN)) %>% arrange(desc(Freq))
epci <- epci %>% distinct(SIREN, .keep_all=TRUE)
    # match
epci <- left_join(epci, observatoire_epci[,-c(2:3)], by="SIREN", copy=FALSE)




#------------------------------- AJOUT POPULATION INSEE



# Manque la population des communes et interco non présentes dans le jeu opendata des territoires
    # on récupère les données d'OFGL dans lesquelles il y a la population
comptes_com <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-communes-consolidee/download/?format=csv&disjunctive.reg_name=true&disjunctive.dep_name=true&disjunctive.epci_name=true&disjunctive.tranche_population=true&disjunctive.tranche_revenu_imposable_par_habitant=true&disjunctive.com_name=true&disjunctive.agregat=true&refine.exer=2019&refine.agregat=D%C3%A9penses+totales&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")
comptes_epci <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-gfp-consolidee/download/?format=csv&disjunctive.dep_name=true&disjunctive.gfp_tranche_population=true&disjunctive.nat_juridique=true&disjunctive.mode_financement=true&disjunctive.gfp_tranche_revenu_imposable_par_habitant=true&disjunctive.epci_name=true&disjunctive.agregat=true&refine.exer=2019&refine.agregat=D%C3%A9penses+totales&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")


# On sélectionne les variables de la population et du SIREN
comptes_com <- comptes_com[,c(18,24)]
comptes_epci <- comptes_epci[,c(12,20)]

# On renomme les colonnes
comptes_com <- comptes_com %>% rename(SIREN = `Code Siren Collectivité`,
                                      `pop-insee` = `Population totale`)
comptes_epci <- comptes_epci %>% rename(SIREN = `Code Siren 2020 EPCI`,
                                      `pop-insee` = `Population totale`)

# On supprime les doublons pour les EPCI
comptes_epci <- comptes_epci %>% group_by(SIREN) %>% distinct(SIREN, .keep_all=TRUE)

# On ajoute la population à nos bases
communes <- left_join(communes, comptes_com, by = "SIREN", copy = FALSE)
epci <- left_join(epci, comptes_epci, by = "SIREN", copy = FALSE)

# On supprime l'ancienne colonne de la pop incomplète et on renomme
communes <- communes[,-14] %>% rename(`pop-insee` = `pop-insee.y`)
epci <- epci[,-13] %>% rename(`pop-insee` = `pop-insee.y`)

# On réordonne
communes <- communes[,c(1:13,18,14:17)]
epci <- epci[,c(1:12,17,13:16)]




#------------------------------- AJOUT INFORMATIONS CHEFS EXECUTIF RNE



# On importe les bases RNE de datagouv pour chaque type d'orga
RNE_reg <- read_delim("https://www.data.gouv.fr/fr/datasets/r/430e13f9-834b-4411-a1a8-da0b4b6e715c", "\t", escape_double = FALSE, trim_ws = TRUE)
RNE_dep <- read_delim("https://www.data.gouv.fr/fr/datasets/r/601ef073-d986-4582-8e1a-ed14dc857fba", "\t", escape_double = FALSE, trim_ws = TRUE)
RNE_com <- read_delim("https://www.data.gouv.fr/fr/datasets/r/d5f400de-ae3f-4966-8cb6-a85c70c6c24a", "\t", escape_double = FALSE, trim_ws = TRUE)
RNE_epci <- read_delim("https://www.data.gouv.fr/fr/datasets/r/41d95d7d-b172-4636-ac44-32656367cdc7", "\t", escape_double = FALSE, trim_ws = TRUE)

# On filtre pour ne garder que les chefs (et pas tous les élus)
RNE_reg <- RNE_reg %>% filter(`Libellé de la fonction` == "Président du conseil régional")
RNE_dep <- RNE_dep %>% filter(`Libellé de la fonction` == "Président du conseil départemental")
RNE_com <- RNE_com %>% filter(`Libellé de la fonction` == "Maire")
RNE_epci <- RNE_epci %>% filter(`Libellé de la fonction` == "Président du conseil communautaire")

# On renomme la colonne pivot (pour la jointure)
RNE_reg <- RNE_reg %>% rename(COG = `Code de la région`)
RNE_dep <- RNE_dep %>% rename(COG = `Code du département`)
RNE_com <- RNE_com %>% rename(COG = `Code de la commune`)
RNE_epci <- RNE_epci %>% rename(SIREN = `N° SIREN`)

# On joint les données par le numéro (COG ou SIREN) en ne gardant que qq infos du chef (nom, prénom, CSP et date de naissance)
regions <- left_join(regions, RNE_reg[,c(1,5,6,8,10)], by="COG", copy=FALSE)
departements <- left_join(departements, RNE_dep[,c(1,5,6,8,10)], by="COG", copy=FALSE)
communes <- left_join(communes, RNE_com[-c(9116, 12092),c(5,7,8,10,12)], by="COG", copy=FALSE)  #on retire les observations doublons
epci <- left_join(epci, RNE_epci[,c(5,9,10,12,14)], by="SIREN", copy=FALSE)

# On met les noms et prénoms au format tel que sous wikidata pour match (ex : Michel Dupont)
    # on regroupe les colonnes nom et prénom
regions$chef_executif <- paste(regions$`Prénom de l'élu`, regions$`Nom de l'élu`) 
regions[regions == "NA NA"] <- NA     #on remet des NA où il faut
    # on met la première lettre du nom en majuscule puis autres en minuscules
regions$chef_executif <- tolower(regions$chef_executif)
    # puis 1ère lettre en maj
library(tools)
regions$chef_executif <- toTitleCase(regions$chef_executif)
  # on retire les noms et prenoms séparés
regions <- regions[,-c(17:18)]


#  Mêmes manips pour les autres orgas :
    ## Départements
departements$chef_executif <- paste(departements$`Prénom de l'élu`, departements$`Nom de l'élu`) 
departements[departements == "NA NA"] <- NA     #on remet des NA où il faut
departements$chef_executif <- tolower(departements$chef_executif)
departements$chef_executif <- toTitleCase(departements$chef_executif)
departements <- departements[,-c(18:19)]
    ## Communes
communes$chef_executif <- paste(communes$`Prénom de l'élu`, communes$`Nom de l'élu`) 
communes[communes == "NA NA"] <- NA     #on remet des NA où il faut
communes$chef_executif <- tolower(communes$chef_executif)
communes$chef_executif <- toTitleCase(communes$chef_executif)
communes <- communes[,-c(19:20)]
    ## EPCI
epci$chef_executif <- paste(epci$`Prénom de l'élu`, epci$`Nom de l'élu`) 
epci[epci == "NA NA"] <- NA     #on remet des NA où il faut
epci$chef_executif <- tolower(epci$chef_executif)
epci$chef_executif <- toTitleCase(epci$chef_executif)
epci <- epci[,-c(18:19)]


# A partir de la date de naissance on récupère l'age de chaque chef
    # fonction qui récupère l'âge
library(lubridate)
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
    calc.age = interval(dob, age.day) / duration(num = 1, units = units)
    if (floor) return(as.integer(floor(calc.age)))
    return(calc.age)
}
    # implique un format de date à l'américaine puis on applique la fonction du calcul d'âge
regions$`Date de naissance` <- format(as.Date(regions$`Date de naissance`, format="%d/%m/%Y"),"%Y/%m/%d")
regions$age_chef <- age(regions$`Date de naissance`, units = "years")
    # on supprime la colonne de la date de naissance
regions <- regions[,-17]


## Mêmes manips pour les autres orgas :
    ## Départements
departements$`Date de naissance` <- format(as.Date(departements$`Date de naissance`, format="%d/%m/%Y"),"%Y/%m/%d")
departements$age_chef <- age(departements$`Date de naissance`, units = "years")
departements <- departements[,-18]
    ## Communes
communes$`Date de naissance` <- format(as.Date(communes$`Date de naissance`, format="%d/%m/%Y"),"%Y/%m/%d")
communes$age_chef <- age(communes$`Date de naissance`, units = "years")
communes <- communes[,-19]
    ## EPCI
epci$`Date de naissance` <- format(as.Date(epci$`Date de naissance`, format="%d/%m/%Y"),"%Y/%m/%d")
epci$age_chef <- age(epci$`Date de naissance`, units = "years")
epci <- epci[,-18]


# On renomme la colonne CSP
regions <- regions %>% rename(CSP_chef = `Libellé de la catégorie socio-professionnelle`)
departements <- departements %>% rename(CSP_chef = `Libellé de la catégorie socio-professionnelle`)
communes <- communes %>% rename(CSP_chef = `Libellé de la catégorie socio-professionnelle`)
epci <- epci %>% rename(CSP_chef = `Libellé de la catégorie socio-professionnelle`)




#------------------------------- AJOUT PARTIS POLITIQUES WIKIDATA



# Requête wikidata pour récupérer les partis politiques associés aux chefs
library(WikidataQueryServiceR)

# Puisque tous ne sont pas à jour on récupère une liste de tous les politicens français qui sont membres d'un ou + partis
wikidata_partis_po <- query_wikidata('SELECT DISTINCT ?chefLabel ?partiLabel
WHERE {
  ?chef wdt:P31 wd:Q5 .    #tous les êtres humains
  ?chef wdt:P106 wd:Q82955 .    #on limite à ceux qui sont politiciens 
  ?chef wdt:P27 wd:Q142  .    #on limite aux pers françaises
  ?chef wdt:P102 ?parti .     #on recup le parti
  ?chef wdt:P569 ?date .   #on recup la date de naissance
  FILTER(YEAR(?date) > 1920).    #on trie pour ne garder que les personnes "actuelles" (moins de 100 ans)
  SERVICE wikibase:label { bd:serviceParam wikibase:language "fr". }
}
ORDER BY ?chefLabel ')


# Maintenant on a qu'à matcher par le nom de l'élu avec le RNE (pour ça on met en majuscules et sans accent pour optimiser le match)
    # majuscules
wikidata_partis_po$chef_upper <- toupper(wikidata_partis_po$chefLabel)
    # pas d'accents
wikidata_partis_po <- data.table::data.table(wikidata_partis_po)
wikidata_partis_po[, chef_upper := stringi::stri_trans_general (str = chef_upper, id = "Latin-ASCII")]


# Même chose pour les noms d'élus des jeux des différentes orgas
    ## REGIONS
regions$chef_upper <- toupper(regions$chef_executif)
regions <- data.table::data.table(regions)
regions[, chef_upper := stringi::stri_trans_general (str = chef_upper, id = "Latin-ASCII")]
    ## DEPARTEMENTS
departements$chef_upper <- toupper(departements$chef_executif)
departements <- data.table::data.table(departements)
departements[, chef_upper := stringi::stri_trans_general (str = chef_upper, id = "Latin-ASCII")]
    ## COMMUNES
communes$chef_upper <- toupper(communes$chef_executif)
communes <- data.table::data.table(communes)
communes[, chef_upper := stringi::stri_trans_general (str = chef_upper, id = "Latin-ASCII")]
    ## EPCI
epci$chef_upper <- toupper(epci$chef_executif)
epci <- data.table::data.table(epci)
epci[, chef_upper := stringi::stri_trans_general (str = chef_upper, id = "Latin-ASCII")]


# On procède au match maintenant
    ## REGIONS
regions <- left_join(regions, wikidata_partis_po[,2:3], by="chef_upper", copy=FALSE) 
regions <- regions[,-20]  #on retire la colonne des noms en majuscules
    ## DEPARTEMENTS
departements <- left_join(departements, wikidata_partis_po[,2:3], by="chef_upper", copy=FALSE) 
departements <- departements[,-21]
    ## COMMUNES
communes <- left_join(communes, wikidata_partis_po[,2:3], by="chef_upper", copy=FALSE) 
communes <- communes[,-22]
    ## EPCI
epci <- left_join(epci, wikidata_partis_po[,2:3], by="chef_upper", copy=FALSE) 
epci <- epci[,-21]


# On renomme la colonne de la couleur politique
regions <- regions %>% rename(partis_po_chef = partiLabel)
departements <- departements %>% rename(partis_po_chef = partiLabel)
communes <- communes %>% rename(partis_po_chef = partiLabel)
epci <- epci %>% rename(partis_po_chef = partiLabel)


## Nombre de partis politiques manquants pour les orgas
regions %>% count(is.na(partiLabel))  # 3/17
departements %>% count(is.na(partiLabel))  #32/98
communes %>% count(is.na(partiLabel))  # 34386/34966
epci %>% count(is.na(partiLabel))  # 1106/1272




#------------------------------- AJOUT STATISTIQUES LOCALES INSEE



# Import du dossier complet (en fait ne contient pas toutes les variables et en plus que pour les communes :/)
#download.file("https://www.insee.fr/fr/statistiques/fichier/5359146/dossier_complet.zip", "dossier_complet.zip")
#unzip("dossier_complet.zip")
#stats_locales_INSEE <- read_delim("dossier_complet.csv", ";", trim_ws = TRUE)
#dictionnaire_stats_INSEE <- read_delim("meta_dossier_complet.csv", ";", trim_ws = TRUE)


# Import des bases depuis les fichiers téléchargés d'internet
stats_locales_reg <- read_delim("Data/external/stats_locales_region.csv", ";", escape_double = FALSE, trim_ws = TRUE, skip = 2)
stats_locales_dep <- read_delim("Data/external/stats_locales_departement.csv", ";", escape_double = FALSE, trim_ws = TRUE, skip = 2)
stats_locales_com <- read_delim("Data/external/stats_locales_commune.csv", ";", escape_double = FALSE, trim_ws = TRUE, skip = 2)
stats_locales_epci <- read_delim("Data/external/stats_locales_interco.csv", ";", escape_double = FALSE, trim_ws = TRUE, skip = 2)

# On renomme les colonnes
    ## REGIONS
names(stats_locales_reg)
stats_locales_reg <- stats_locales_reg %>% 
    rename(COG = Code,
           nom = Libellé,
           part_plus65 = `Part des pers. âgées de 65 ans ou + 2017`,
           niveau_vie = `Médiane du niveau de vie 2018`,
           part_diplomes = `Part des diplômés d'un BAC+5 ou plus dans la pop. non scolarisée de 15 ans ou + 2017`,
           taux_chomage = `Taux de chômage annuel moyen 2019`,
           nb_crea_entps = `Nb créations d'entreprises 2020`,
           nb_nuitees_hotels = `Nb de nuitées dans les hôtels de tourisme 2019`,
           flux_migration_res = `Flux principal de migration résidentielle 2017`,
           PIB_habitant = `Produit intérieur brut par habitant 2018`,
           tertiaire_VA = `Part du tertiaire marchand dans la VA 2018`)
    ## DEPARTEMENTS
names(stats_locales_dep)
stats_locales_dep <- stats_locales_dep %>% 
    rename(COG = Code,
           nom = Libellé,
           part_plus65 = `Part des pers. âgées de 65 ans ou + 2017`,
           niveau_vie = `Médiane du niveau de vie 2018`,
           part_diplomes = `Part des diplômés d'un BAC+5 ou plus dans la pop. non scolarisée de 15 ans ou + 2017`,
           taux_chomage = `Taux de chômage annuel moyen 2019`,
           nb_crea_entps = `Nb créations d'entreprises 2020`,
           nb_nuitees_hotels = `Nb de nuitées dans les hôtels de tourisme 2019`,
           flux_migration_res = `Flux principal de migration résidentielle 2017`)
    ## COMMUNES
names(stats_locales_com)
stats_locales_com <- stats_locales_com %>% 
    rename(COG = Code,
           nom = Libellé,
           part_plus65 = `Part des pers. âgées de 65 ans ou + 2017`,
           part_diplomes = `Part des diplômés d'un BAC+5 ou plus dans la pop. non scolarisée de 15 ans ou + 2017`,
           nb_crea_entps = `Nb créations d'entreprises 2020`,
           flux_migration_res = `Flux principal de migration résidentielle 2017`)
    ## EPCI
names(stats_locales_epci)
stats_locales_epci <- stats_locales_epci %>% 
    rename(SIREN = Code,
           nom = Libellé,
           part_plus65 = `Part des pers. âgées de 65 ans ou + 2017`,
           niveau_vie = `Médiane du niveau de vie 2018`,
           part_diplomes = `Part des diplômés d'un BAC+5 ou plus dans la pop. non scolarisée de 15 ans ou + 2017`,
           nb_crea_entps = `Nb créations d'entreprises 2020`,
           flux_migration_res = `Flux principal de migration résidentielle 2017`)


## On match par les COG pour collectivités et SIREN pour interco
regions <- left_join(regions, stats_locales_reg[,-2], by="COG", copy=FALSE)
departements <- left_join(departements, stats_locales_dep[,-2], by="COG", copy=FALSE)
communes <- left_join(communes, stats_locales_com[,-2], by="COG", copy=FALSE)
epci <- left_join(epci, stats_locales_epci[,-2], by="SIREN", copy=FALSE)




#-------------------------------  AJOUT BUDGET : DEPENSES TOTALES 2019  -------------------------------#



# Import des bases
comptes_reg <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-regions-consolidee/download/?format=csv&disjunctive.reg_name=true&disjunctive.agregat=true&refine.agregat=D%C3%A9penses+totales&refine.exer=2019&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")
comptes_dep <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-departements-consolidee/download/?format=csv&disjunctive.reg_name=true&disjunctive.dep_tranche_population=true&disjunctive.dep_name=true&disjunctive.agregat=true&refine.exer=2019&refine.agregat=D%C3%A9penses+totales&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")
comptes_com <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-communes-consolidee/download/?format=csv&disjunctive.reg_name=true&disjunctive.dep_name=true&disjunctive.epci_name=true&disjunctive.tranche_population=true&disjunctive.tranche_revenu_imposable_par_habitant=true&disjunctive.com_name=true&disjunctive.agregat=true&refine.exer=2019&refine.agregat=D%C3%A9penses+totales&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")
comptes_epci <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-gfp-consolidee/download/?format=csv&disjunctive.dep_name=true&disjunctive.gfp_tranche_population=true&disjunctive.nat_juridique=true&disjunctive.mode_financement=true&disjunctive.gfp_tranche_revenu_imposable_par_habitant=true&disjunctive.epci_name=true&disjunctive.agregat=true&refine.exer=2019&refine.agregat=D%C3%A9penses+totales&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")


# On filtre pour ne garder que les dépenses totales de l'année 2019 (UTD)
comptes_reg <- comptes_reg %>% filter(Exercice == 2019)
comptes_dep <- comptes_dep %>% filter(Exercice == 2019)
comptes_com <- comptes_com %>% filter(Exercice == 2019)
comptes_epci <- comptes_epci %>% filter(Exercice == 2019)


# On renomme la colonne du SIREN dans le jeu du budget (colonne pivot)
comptes_reg <- comptes_reg %>% rename(SIREN = `Code Siren Collectivité`)
comptes_dep <- comptes_dep %>% rename(SIREN = `Code Siren Collectivité`)
comptes_com <- comptes_com %>% rename(SIREN = `Code Siren Collectivité`)
comptes_epci <- comptes_epci %>% rename(SIREN = `Code Siren Collectivité`)


# Matchs
regions <- left_join(regions, comptes_reg[,c(7,13)], by="SIREN", copy=FALSE)
departements <- left_join(departements, comptes_dep[,c(10,16)], by="SIREN", copy=FALSE)  #du jeu comptes on ne prend que le SIREN et le budget
communes <- left_join(communes, comptes_com[,c(18,25)], by="SIREN", copy=FALSE)
epci <- left_join(epci, comptes_epci[,c(15,21)], by="SIREN", copy=FALSE)


# On renomme la colonne du budget par un nom exploitable
regions <- regions %>% rename(depenses_hab = `Montant en € par habitant`)
departements <- departements %>% rename(depenses_hab = `Montant en € par habitant`)
communes <- communes %>% rename(depenses_hab = `Montant en € par habitant`)
epci <- epci %>% rename(depenses_hab = `Montant en € par habitant`)






#-------------------------------  AJOUT NB ETUDIANTS DANS LA POP  -------------------------------#


# Import de la base du nombre d'étudiants
Nb_etudiants_pop <- read_delim("https://data.enseignementsup-recherche.gouv.fr/explore/dataset/fr-esr-atlas_regional-effectifs-d-etudiants-inscrits/download/?format=csv&disjunctive.rgp_formations_ou_etablissements=true&refine.rgp_formations_ou_etablissements=Total+des+formations+d%27enseignement+sup%C3%A9rieur&refine.rentree_universitaire=2018-19&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";", escape_double = FALSE, trim_ws = TRUE)

    # on garde le COG, le type de niveau géo et le nb d'étudiants
Nb_etudiants_pop <- Nb_etudiants_pop[, c(3,22,11)]
    # on renomme les colonnes
Nb_etudiants_pop <- Nb_etudiants_pop %>% rename(COG = `Identifiant de l’unité géographique`,
                                                nb_etudiants = `Nombre total d’étudiants inscrits`,
                                                niveau_geo = `Niveau géographique`)

# On sépare les données selon le niveau géographique car mêmes COG pour régions et départements
Nb_etudiants_reg <- Nb_etudiants_pop %>% filter(niveau_geo == "Région")
Nb_etudiants_dep <- Nb_etudiants_pop %>% filter(niveau_geo == "Département")
Nb_etudiants_com <- Nb_etudiants_pop %>% filter(niveau_geo == "Commune")

# 4 valeurs par collectivités (public/privé, filles/garçons) donc on somme les effectifs par COG
Nb_etudiants_reg <- Nb_etudiants_reg %>% group_by(COG) %>% summarise(nb_etudiants = sum(nb_etudiants)) %>% ungroup()
Nb_etudiants_dep <- Nb_etudiants_dep %>% group_by(COG) %>% summarise(nb_etudiants = sum(nb_etudiants)) %>% ungroup()
Nb_etudiants_com <- Nb_etudiants_com %>% group_by(COG) %>% summarise(nb_etudiants = sum(nb_etudiants)) %>% ungroup()

# Les COG des régions et départements contiennent la lettre (R ou D) donc match impossible : on extrait les 2 derniers chiffres
Nb_etudiants_reg$COG <- str_sub(Nb_etudiants_reg$COG,-2)  #on garde 2 chiffres
Nb_etudiants_dep$COG <- str_sub(Nb_etudiants_dep$COG,-3)  #on garde 3 chiffres pour gérer les DROM COM
Nb_etudiants_dep$COG <- gsub("^0", "", Nb_etudiants_dep$COG)  #puis on retire le 1er chiffre si c'est un 0

# Matchs pour les niveaux géographiques dispo (càd regions, dep et communes)
regions <- left_join(regions, Nb_etudiants_reg, by="COG", copy=FALSE)
departements <- left_join(departements, Nb_etudiants_dep, by="COG", copy=FALSE)
communes <- left_join(communes, Nb_etudiants_com, by="COG", copy=FALSE)

# On transforme le nombre d'étudiants en taux pour que ça soit plus parlant
regions <- regions %>% mutate(part_etudiants = nb_etudiants/`pop-insee`*100)
regions$part_etudiants <- round(regions$part_etudiants,1)
departements <- departements %>% mutate(part_etudiants = nb_etudiants/`pop-insee`*100)
departements$part_etudiants <- round(departements$part_etudiants,1)
communes <- communes %>% mutate(part_etudiants = nb_etudiants/`pop-insee`*100)
communes$part_etudiants <- round(communes$part_etudiants,1)


# On supprime la colonne 'nb_etudiants' maintenant qu'on a le pourcentage
regions <- regions[,-31]
departements <- departements[,-30]
communes <- communes[,-28]





#-------------------------------  AJOUT URBANISATION  -------------------------------#



# Import de la base à matcher (urbain / rural en 5 modalités)
library(curl)
curl_download("https://www.insee.fr/fr/statistiques/fichier/5039991/FET2021-D4.xlsx", "FET2021_D4.xlsx")
urbanisation_com <- read_excel("FET2021_D4.xlsx", col_names = TRUE, skip = 2, sheet='Figure 5')

# On renomme les colonnes
urbanisation_com <- urbanisation_com %>% rename(COG = `Code géographique communal`,
                                                        niveau_rural = `Typologie urbain/rural`)

# Match avec le jeu de l'analyse
communes <- left_join(communes, urbanisation_com, by="COG", copy=FALSE)



        ### On élève au niveau région et département


# On récupère les colonnes du COG département et niveau rural / urbain
urbanisation_dep <- communes[,c(6,29)] %>% arrange(code_departement)

# Fonction qui trouve le mode 
Mode <- function(x) {
  ux <- unique(x)
  if(!anyDuplicated(x)){
      x } else { 
     tbl <-   table(match(x, ux))
     toString(ux[tbl==max(tbl)])
 }
}

mode <- function(codes){
  which.max(tabulate(codes))
}


library(DescTools)
test <- urbanisation_dep %>% group_by(code_departement) %>% mutate(Mode = DescTools::Mode(niveau_rural))

test <- urbanisation_dep %>% group_by(code_departement) %>% summarise(Mode = DescTools::Mode(niveau_rural))

test <- with(urbanisation_dep, ave(seq_along(code_departement), index, FUN = seq_along))

test <- urbanisation_dep %>%
  group_by(code_departement) %>%
  summarise(Mode = mode(niveau_rural))




# On l'applique pour chaque département (on passe du niveau communal à départemental)
test <- urbanisation_dep %>% group_by(code_departement) %>% mutate(Mode = Mode(niveau_rural))  
        #PROBLEME : qd il y avait initialement (niveau communal) 1 obs par département alors le mode renvoie un NA, et qd 2 possibilités (pas possible de trancher) renvoie NA aussi

# Donc on affecte aux NA les valeurs de la colonne 'niveau_rural' càd au niveau des communes
urbanisation_dep$Mode[is.na(urbanisation_dep$Mode)] <- as.character(urbanisation_dep$niveau_rural[is.na(urbanisation_dep$Mode)])  
urbanisation_dep <- unique(urbanisation_dep[,c(1,3)])
table(urbanisation_dep$code_departement)   # 2 valeurs pour les départements n°4 et 49
urbanisation_dep <- urbanisation_dep %>% distinct(code_departement, .keep_all=T)

# On affecte ces données au jeu des départements par un match via 'code_departement'
departement <- left_join(departement, urbanisation_dep, by="code_departement", copy=F)



            ### Même chose au niveau région


# On récupère les colonnes du COG département et niveau rural / urbain
urbanisation_region <- departement[,c(5,27)] %>% arrange(regcode)

# On remplace 
urbanisation_region <- urbanisation_region %>% group_by(regcode) %>% mutate(Mode2 = Mode(Mode))
urbanisation_region$Mode2[is.na(urbanisation_region$Mode2)] <- as.character(urbanisation_region$Mode[is.na(urbanisation_region$Mode2)])
urbanisation_region <- unique(urbanisation_region[,c(1,3)])

# On affecte ces données au jeu des départements par un match via 'regcode'
region <- left_join(region, urbanisation_region, by="regcode", copy=F)






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





























