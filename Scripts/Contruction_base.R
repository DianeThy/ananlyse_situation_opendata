#### NOUVEAU SCRIPT POUR CONSTRUIRE LA BASE EN NE PASSANT QUE PAR DES LIENS POUR SIMPLIFIER LA MISE EN OEUVRE



###############################################################################################################################

######################################## CONSTITUTION DE LA BASE DE DONNEES ###################################################

###############################################################################################################################



#------------------------------- ON PART D'UN FICHIER AVEC TOUTES LES ORGAS DE FRANCE


# Fichier initial : toutes les orgas de France (collectivités + EPCI)
library(tidyverse)
regions <- read_csv("Data/raw/infos_regions.csv")
departements <- read_csv("Data/raw/infos_departements.csv")
communes <- read_csv("Data/raw/infos_communes.csv")
epci <- read_csv("Data/raw/infos_interco.csv")



#------------------------------- AJOUT DONNEES OBSERVATOIRE DES TERRITOIRES : FUTUR(S) Y


# Import de la base complète
library(googlesheets4)   #taper "1" en dessous
observatoire_opendata_territoire <- read_sheet("https://docs.google.com/spreadsheets/d/1yhcCDLrDsZzNxlPaIl25p_qN8WVPGCTstdIUxezDxtQ/edit#gid=614893302")


# Après vérification les données ne sont pas fiables : on importe une base avec des corrections pour joindre les 2 et avoir les bonnes infos de Yt
modifs_OODT <- read_csv("Data/external/modifs_recensement_OODT.csv")
    # on remplace les obs où le nombre de jeux sur propre plateforme renseigné était faux
observatoire_opendata_territoire[c(430,505,507,643),]$`nb-ptf` <- modifs_OODT[c(4,1,2,6),]$`nb-ptf`  # nb-ptf
    # on ajoute des obs pour les orgas où 0 jeux n'étaient recensés mais en réalité portail existe bien
observatoire_opendata_territoire <- rbind(observatoire_opendata_territoire, modifs_OODT[c(3,5,7:12),])

          
  # on renomme la colonne de jointure et on la passe au format numérique
observatoire_opendata_territoire <- observatoire_opendata_territoire %>% rename(SIREN = siren)
observatoire_opendata_territoire$SIREN <- as.numeric(observatoire_opendata_territoire$SIREN)

# On sépare la base selon le type d'organisation
observatoire_reg <- observatoire_opendata_territoire %>% filter(type == "REG") #15
observatoire_dep <- observatoire_opendata_territoire %>% filter(type == "DEP") #61
observatoire_com <- observatoire_opendata_territoire %>% filter(type == "COM")  #351 obs
observatoire_epci <- observatoire_opendata_territoire %>% filter(type == "MET" | type == "CU" | type == "CC" | type == "CA")  # 17+5+43+99 = 164

# On match par le numéro (SIREN, COG INSEE)
regions <- left_join(regions, observatoire_reg[,-c(2:3)], by="SIREN", copy=FALSE)
departements <- left_join(departements, observatoire_dep[,-c(2:3)], by="SIREN", copy=FALSE)
communes <- left_join(communes, observatoire_com[,-c(2:3)], by="SIREN", copy=FALSE)

# Pour les EPCI on se rend compte qu'il y a des doublons donc on les supprime avant de matcher
doublons <- as.data.frame(table(epci$SIREN)) %>% arrange(desc(Freq))
epci <- epci %>% distinct(SIREN, .keep_all=TRUE)
    # match
epci <- left_join(epci, observatoire_epci[,-c(2:3)], by="SIREN", copy=FALSE)




#------------------------------- AJOUT POPULATION 



# La population des orgas est incomplète dans le jeu opendata des territoires donc on récupère la colonne complète des données OFGL
    # on récupère les données d'OFGL dans lesquelles il y a la population
comptes_reg <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-regions-consolidee/download/?format=csv&disjunctive.reg_name=true&disjunctive.agregat=true&refine.agregat=D%C3%A9penses+totales&refine.exer=2019&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")
comptes_dep <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-departements-consolidee/download/?format=csv&disjunctive.reg_name=true&disjunctive.dep_tranche_population=true&disjunctive.dep_name=true&disjunctive.agregat=true&refine.exer=2019&refine.agregat=D%C3%A9penses+totales&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")
comptes_com <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-communes-consolidee/download/?format=csv&disjunctive.reg_name=true&disjunctive.dep_name=true&disjunctive.epci_name=true&disjunctive.tranche_population=true&disjunctive.tranche_revenu_imposable_par_habitant=true&disjunctive.com_name=true&disjunctive.agregat=true&refine.exer=2019&refine.agregat=D%C3%A9penses+totales&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")
comptes_epci <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-gfp-consolidee/download/?format=csv&disjunctive.dep_name=true&disjunctive.gfp_tranche_population=true&disjunctive.nat_juridique=true&disjunctive.mode_financement=true&disjunctive.gfp_tranche_revenu_imposable_par_habitant=true&disjunctive.epci_name=true&disjunctive.agregat=true&refine.exer=2019&refine.agregat=D%C3%A9penses+totales&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")


# On sélectionne les variables de la population et du SIREN
comptes_reg_pop <- comptes_reg[,c(7,12)]
comptes_dep_pop <- comptes_dep[,c(10,15)]
comptes_com_pop <- comptes_com[,c(18,24)]
comptes_epci_pop <- comptes_epci[,c(12,20)]

# On renomme les colonnes
comptes_reg_pop <- comptes_reg_pop %>% rename(SIREN = `Code Siren Collectivité`,
                                      `pop-insee` = `Population totale`)
comptes_dep_pop <- comptes_dep_pop %>% rename(SIREN = `Code Siren Collectivité`,
                                      `pop-insee` = `Population totale`)
comptes_com_pop <- comptes_com_pop %>% rename(SIREN = `Code Siren Collectivité`,
                                      `pop-insee` = `Population totale`)
comptes_epci_pop <- comptes_epci_pop %>% rename(SIREN = `Code Siren 2021 EPCI`,
                                      `pop-insee` = `Population totale`)

# On supprime les doublons pour les EPCI
comptes_epci_pop <- comptes_epci_pop %>% group_by(SIREN) %>% distinct(SIREN, .keep_all=TRUE)

# On ajoute la population à nos bases
regions <- left_join(regions, comptes_reg_pop, by = "SIREN", copy = FALSE)
departements <- left_join(departements, comptes_dep_pop, by = "SIREN", copy = FALSE)
communes <- left_join(communes, comptes_com_pop, by = "SIREN", copy = FALSE)
epci <- left_join(epci, comptes_epci_pop, by = "SIREN", copy = FALSE)

# On supprime l'ancienne colonne de la pop incomplète et on renomme
regions <- regions[,-12] %>% rename(`pop-insee` = `pop-insee.y`)
departements <- departements[,-13] %>% rename(`pop-insee` = `pop-insee.y`)
communes <- communes[,-14] %>% rename(`pop-insee` = `pop-insee.y`)
epci <- epci[,-13] %>% rename(`pop-insee` = `pop-insee.y`)

# On réordonne
regions <- regions[,c(1:11,16,12:15)]
departements <- departements[,c(1:12,17,13:16)]
communes <- communes[,c(1:13,18,14:17)]
epci <- epci[,c(1:12,17,13:16)]




#------------------------------- AJOUT INFORMATIONS CHEFS EXECUTIF RNE



# On importe les bases RNE (archives du 5 juillet 2019) de datagouv pour chaque type d'orga
RNE_reg <- read_delim("http://data.cquest.org/repertoire-national-des-elus/archives/20190705_4-rne-cr.txt.gz", "\t", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE, skip = 1)
RNE_corse <- read_delim("http://data.cquest.org/repertoire-national-des-elus/archives/20190705_5-rne-cac.txt.gz", "\t", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE, skip = 1)  #corse
RNE_dep <- read_delim("http://data.cquest.org/repertoire-national-des-elus/archives/20190705_3-rne-cd.txt.gz", "\t", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE, skip = 1)
RNE_epci <- read_delim("http://data.cquest.org/repertoire-national-des-elus/archives/20190705_2-rne-epci.txt.gz", "\t", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE, skip = 1)
RNE_com <- read_delim("http://data.cquest.org/repertoire-national-des-elus/archives/20190705_9-rne-maires.txt.gz", "\t", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE, skip = 1)


# On filtre pour ne garder que les chefs (et pas tous les élus)
RNE_reg <- RNE_reg %>% filter(`Libellé de fonction` == "Président du conseil régional")
RNE_corse <- RNE_corse %>% filter(`Libellé de fonction` == "Président Conseil exécutif de Corse")
RNE_dep <- RNE_dep %>% filter(`Libellé de fonction` == "Président du conseil départemental")
RNE_epci <- RNE_epci %>% filter(`Libellé de fonction` == "Président")

# Le COG des communes est en 2 parties (COG dep et COG com) donc on rassemble pour avoir code INSEE
    #pour la martinique COG dep = ZB, on remplace par "97" pour obtenir bon COG ensuite (dep + com)
RNE_com$`Code du département (Maire)` <- str_replace_all(RNE_com$`Code du département (Maire)`, "ZB", "97")
    #on rassemble les codes INSEE
RNE_com$COG <- paste(RNE_com$`Code du département (Maire)`, RNE_com$`Code Insee de la commune`, sep='')

# On renomme la colonne pivot (pour la jointure) pour les autres orgas
RNE_reg <- RNE_reg %>% rename(COG = `Code région`)
RNE_corse <- RNE_corse %>% rename(COG = `Code région`)
RNE_dep <- RNE_dep %>% rename(COG = `Code du département`)
RNE_epci <- RNE_epci %>% rename(SIREN = `N° SIREN`)

# Le SIREN de la métropole de Lyon a changé en 2020 donc on met à jour celui du RNE datant de 2019 pour match
RNE_epci[921,]$SIREN <- 200046977

# On met au format numérique les colonnes pivot qui le nécessitent
regions$COG <- as.numeric(regions$COG)
RNE_reg$COG <- as.numeric(RNE_reg$COG)

# On joint les données par le numéro (COG ou SIREN) en ne gardant que qq infos du chef (nom, prénom, CSP et date de naissance)
regions <- left_join(regions, rbind(unique(RNE_reg[,c(1,5,6,8,10)]),RNE_corse[,c(1,3,4,6,8)]), by="COG", copy=FALSE)  #on ajoute les élus des régions où on rbind la corse
departements <- left_join(departements, RNE_dep[,c(1,3,4,10,11)], by="COG", copy=FALSE)
communes <- left_join(communes, RNE_com[-c(34709),c(5,6,8,10,13)], by="COG", copy=FALSE)  #on retire une obs pour la martinique où 2 chefs (on garde le plus récent)
epci <- left_join(epci, RNE_epci[,c(2,7,8,10,12)], by="SIREN", copy=FALSE)


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
departements <- departements[,-19]
    ## Communes
communes$`Date de naissance` <- format(as.Date(communes$`Date de naissance`, format="%d/%m/%Y"),"%Y/%m/%d")
communes$age_chef <- age(communes$`Date de naissance`, units = "years")
communes <- communes[,-19]
    ## EPCI
epci$`Date de naissance` <- format(as.Date(epci$`Date de naissance`, format="%d/%m/%Y"),"%Y/%m/%d")
epci$age_chef <- age(epci$`Date de naissance`, units = "years")
epci <- epci[,-18]


# On renomme la colonne CSP
regions <- regions %>% rename(CSP_chef = `Libellé de la profession`)
departements <- departements %>% rename(CSP_chef = `Libellé de la profession`)
communes <- communes %>% rename(CSP_chef = `Libellé de la profession`)
epci <- epci %>% rename(CSP_chef = `Libellé de la profession`)




#------------------------------- AJOUT PARTIS POLITIQUES WIKIDATA



# Requête wikidata pour récupérer les partis politiques associés aux chefs
library(WikidataQueryServiceR)

# Puisque les chefs ne sont pas tous à jour on récupère une liste générale de tous les politiciens français vivants et leur(s) parti(s) associé(s)
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


# Maintenant on peut matcher par le nom de l'élu avec le RNE (pour ça on met en majuscules et sans accent pour optimiser le match)
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
regions %>% count(is.na(partis_po_chef))  # 2/17
departements %>% count(is.na(partis_po_chef))  #27/98
communes %>% count(is.na(partis_po_chef))  # 34265/34966
epci %>% count(is.na(partis_po_chef))  # 1079/1272




#------------------------------- AJOUT STATISTIQUES LOCALES INSEE



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
           taux_chomage = `Taux de chômage annuel moyen 2020`,
           nb_crea_entps = `Nb créations d'entreprises 2020`,
           nb_nuitees_hotels = `Nb de nuitées dans les hôtels de tourisme 2019`,
           flux_migration_res = `Flux principal de migration résidentielle 2017`,
           PIB_habitant = `Produit intérieur brut par habitant 2018`,
           primaire_VA = `Part de l'agriculture dans la VA 2018`,
           secondaire_VA = `Part de l'industrie dans la VA 2018`,
           tertiaire_marchand_VA = `Part du tertiaire marchand dans la VA 2018`,
           tertiaire_non_mar_VA = `Part du tertiaire non marchand dans la VA 2018`)
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

# On met au bon format les colonnes de jointure
stats_locales_reg$COG <- as.numeric(stats_locales_reg$COG)

## On match par les COG pour collectivités et SIREN pour interco
regions <- left_join(regions, stats_locales_reg[,-2], by="COG", copy=FALSE)
departements <- left_join(departements, stats_locales_dep[,-2], by="COG", copy=FALSE)
communes <- left_join(communes, stats_locales_com[,-2], by="COG", copy=FALSE)
epci <- left_join(epci, stats_locales_epci[,-2], by="SIREN", copy=FALSE)
    # le siren de la métropole de Lille ayant changé on attribue les valeurs nous mêmes car pas de match (old : 245900410, new : 200093201)
epci[1320,c(22:26)] <- stats_locales_epci[,c(3:7)] %>% filter(stats_locales_epci$nom == "Métropole Européenne de Lille")




#-------------------------------  AJOUT BUDGET : DEPENSES TOTALES 2019  -------------------------------#



# On repart des bases OFGL importées précédemment pour récupérer la population des orgas
    # On filtre pour ne garder que les dépenses totales de l'année 2019 (UTD)
comptes_reg_tot19 <- comptes_reg %>% filter(Exercice == 2019)
comptes_dep_tot19 <- comptes_dep %>% filter(Exercice == 2019)
comptes_com_tot19 <- comptes_com %>% filter(Exercice == 2019)
comptes_epci_tot19 <- comptes_epci %>% filter(Exercice == 2019)


# On renomme la colonne du SIREN dans le jeu du budget (colonne pivot)
comptes_reg_tot19 <- comptes_reg_tot19 %>% rename(SIREN = `Code Siren Collectivité`)
comptes_dep_tot19 <- comptes_dep_tot19 %>% rename(SIREN = `Code Siren Collectivité`)
comptes_com_tot19 <- comptes_com_tot19 %>% rename(SIREN = `Code Siren Collectivité`)
comptes_epci_tot19 <- comptes_epci_tot19 %>% rename(SIREN = `Code Siren Collectivité`)


# Matchs
regions <- left_join(regions, comptes_reg_tot19[,c(7,13)], by="SIREN", copy=FALSE)
departements <- left_join(departements, comptes_dep_tot19[,c(10,16)], by="SIREN", copy=FALSE)  #du jeu comptes on ne prend que le SIREN et le budget
communes <- left_join(communes, comptes_com_tot19[,c(18,25)], by="SIREN", copy=FALSE)
epci <- left_join(epci, comptes_epci_tot19[,c(15,21)], by="SIREN", copy=FALSE)


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

# On met au bon format les colonnes de jointure 
Nb_etudiants_reg$COG <- as.numeric(Nb_etudiants_reg$COG)

# Matchs pour les niveaux géographiques dispo (càd regions, dep et communes)
regions <- left_join(regions, Nb_etudiants_reg, by="COG", copy=FALSE)
departements <- left_join(departements, Nb_etudiants_dep, by="COG", copy=FALSE)
communes <- left_join(communes, Nb_etudiants_com, by="COG", copy=FALSE)

# On transforme le nombre d'étudiants en taux pour voir si indicateur plus parlant (sélection entre les 2 aux stats desc)
regions <- regions %>% mutate(part_etudiants = nb_etudiants/`pop-insee`*100)
regions$part_etudiants <- round(regions$part_etudiants,1)     # on arrondi
departements <- departements %>% mutate(part_etudiants = nb_etudiants/`pop-insee`*100)
departements$part_etudiants <- round(departements$part_etudiants,1)
communes <- communes %>% mutate(part_etudiants = nb_etudiants/`pop-insee`*100)
communes$part_etudiants <- round(communes$part_etudiants,1)






#-------------------------------  AJOUT URBANISATION  -------------------------------#



# Import de la base à matcher (urbain / rural en 5 modalités)
library(curl)
library(readxl)
curl_download("https://www.insee.fr/fr/statistiques/fichier/5039991/FET2021-D4.xlsx", "FET2021_D4.xlsx")
urbanisation_com <- read_excel("FET2021_D4.xlsx", col_names = TRUE, skip = 2, sheet='Figure 5')

# On renomme les colonnes
urbanisation_com <- urbanisation_com %>% rename(COG = `Code géographique communal`,
                                                niveau_rural = `Typologie urbain/rural`)

# Match avec le jeu de l'analyse
communes <- left_join(communes, urbanisation_com, by="COG", copy=FALSE)

# Pour plus de simplicité on remplace les catégories par un digit de 1 à 6 tel que :
  #- 1 : urbain dense 
  #- 2 : urbain densité intermédiaire
  #- 3 : rural sous forte influence d'un pôle
  #- 4 : rural sous faible influence d'un pôle
  #- 5 : rural autonome peu dense
  #- 6 : rural autonome très peu dense

communes$niveau_rural <- str_replace_all(communes$niveau_rural, c("urbain dense" = "1", "urbain densité intermédiaire" = "2", "rural sous forte influence d'un pôle" = "3", "rural sous faible influence d'un pôle" = "4", "rural autonome peu dense" = "5", "rural autonome très peu dense" = "6"))



        ### On élève au niveau région et département avec 3 méthodes :
                # - en calculant le mode par région et dép
                # - en utilisant la méthode de l'INSEE basée sur la densité
                # - en utilisant la méthode de Olivier Bouba Olga (http://geoconfluences.ens-lyon.fr/actualites/eclairage/grille-densite-zonage-aires-urbaines-definition-rural)



# A) Calcul du mode


    ### niveau départemental

# On récupère les colonnes du COG département et niveau rural / urbain
urbanisation_dep <- communes[,c(6,30)] %>% arrange(code_departement)

# On récupère le mode pour chaque département grâce au group_by()
library(DescTools)
urbanisation_dep <- urbanisation_dep %>% group_by(code_departement)
urbanisation_dep <- urbanisation_dep %>% summarise(Mode = Mode(niveau_rural))
urbanisation_dep <- urbanisation_dep %>% group_by(code_departement) %>% slice(1)  #doublon on choisi la modalité la plus urbaine (4 entre 4 et 5 par ex)

# On renomme la colonne de jointure en vu du match (COG)
urbanisation_dep <- urbanisation_dep %>% rename(COG = code_departement,
                                                niveau_rural_mode = Mode)   #et le niveau d'urbanisation que l'on ajoute

# On affecte ces données au jeu des départements par un match via le COG
departements <- left_join(departements, urbanisation_dep, by="COG", copy=F)


    ### niveau régionnal

# On récupère les colonnes du COG région et niveau rural / urbain
urbanisation_reg <- communes[,c(5,30)] %>% arrange(code_region)

# On trouve le mode de chaque région 
urbanisation_reg <- urbanisation_reg %>% group_by(code_region) 
urbanisation_reg <- urbanisation_reg %>% summarise(Mode = Mode(niveau_rural))

# On renomme les colonnes
urbanisation_reg <- urbanisation_reg %>% rename(COG = code_region,
                                                niveau_rural_mode = Mode)

# On ajoute un "0" avant les COG à un seul chiffre pour correspondre aux GOC du jeu 'regions'
library(stringr)
urbanisation_reg$COG <- sprintf("%02d", urbanisation_reg$COG)

# On met au même format les numéros COG des 2 dfs
urbanisation_reg$COG <- as.numeric(urbanisation_reg$COG)

# On affecte ces données au jeu des régions par un match via le COG
regions <- left_join(regions, urbanisation_reg, by="COG", copy=F)




# B) Méthode de zonage de l'INSEE (pondéré par la population, pas 1 commune = 1 voix)


# On récupère l'agrégation de la densité des communes par l'INSEE (on part de ce jeu pour "élever" aux niveaux supérieurs)
download.file("https://www.insee.fr/fr/statistiques/fichier/2114627/grille_densite_2020.zip", "grille_densite.zip")
unzip("grille_densite.zip")
urbanisation_INSEE <- read_excel("grille_densite_2020_agrege.xlsx")

# On a :
  #- 1 : très dense
  #- 2 : dense
  #- 3 : peu dense
  #- 4 : très peu dense

# On renomme et garde les colonnes intéressantes
urbanisation_INSEE <- urbanisation_INSEE[,-2] %>% rename(COG = `\nCode \nCommune\n`,
                                                densite = `Degré de \nDensité de la commune\n`, 
                                                code_region = `Région\n`,
                                                pop = `Population \nmunicipale \n2017`)

# On récupère le numéro de région en matchant aux données communales où l'on a cette info
urbanisation_INSEE <- left_join(urbanisation_INSEE, communes[,c(3,6)], by = "COG", copy = FALSE)

# Comme il s'agit d'un autre indicateur de densité (4 classes et pas 6) on l'ajoute aux données des communes, puis on choisira l'agrégation adaptée avec l'analyse exploratoire
communes <- left_join(communes, unique(urbanisation_INSEE[,c(1,2)]), by = "COG", copy = FALSE)  #on enlève les doublons



    ### niveau départemental

# On somme la population par départements et par type de densité (entre 1 et 4)
urbanisation_dep <- urbanisation_INSEE %>% group_by(code_departement,densite) %>% summarise(somme_pop = sum(pop))

# On calcule le pourcentage
urbanisation_dep <- urbanisation_dep %>% group_by(code_departement) %>% mutate(percent_pop = (somme_pop/sum(somme_pop) * 100))

# Création vble booléenne
urbanisation_dep <- urbanisation_dep %>% ungroup() %>% mutate(dens_is_1_2 = case_when((densite == 1 | densite == 2) ~ TRUE,
                                                                              TRUE ~ FALSE))

# Calcul somme et percent pour 1|2
df_is_1_2 <- urbanisation_dep[,-c(2,4)]
df_is_1_2 <- df_is_1_2 %>% group_by(code_departement, dens_is_1_2) %>% mutate(somme_pop_is_1_2 = sum(somme_pop))
df_is_1_2 <- df_is_1_2[,-2] %>% distinct()
df_is_1_2 <- df_is_1_2 %>% group_by(code_departement) %>% mutate(percent_pop_is_1_2 = (somme_pop_is_1_2/sum(somme_pop_is_1_2) * 100))
urbanisation_dep <- left_join(urbanisation_dep, df_is_1_2, by = c("code_departement", "dens_is_1_2"), copy = FALSE)

# On applique la règle de décision de l'INSEE en matière de densité
urbanisation_dep <- urbanisation_dep %>% group_by(code_departement) %>% mutate(niveau_rural_insee = case_when(densite == 1 & percent_pop > 50 ~ 1,
                                                                                                 dens_is_1_2 == TRUE & percent_pop_is_1_2 > 50 ~ 2,
                                                                                                 densite == 4 & percent_pop > 50 ~ 4,
                                                                                                 TRUE ~ 3))   #la première valeur par département est la bonne

# On garde la bonne valeur càd la première par département
urbanisation_dep <- urbanisation_dep %>%  group_by(code_departement) %>% slice(1)

# On match au jeu des départements
departements$COG <- as.numeric(departements$COG)
urbanisation_dep$code_departement <- as.numeric(urbanisation_dep$code_departement)
departements <- left_join(departements, urbanisation_dep[,c(1,8)], by = c("COG" = "code_departement"), copy = FALSE)



    ### niveau régionnal

# On somme la population par département et par type de densité (entre 1 et 4)
urbanisation_reg <- urbanisation_INSEE %>% group_by(code_region,densite) %>% summarise(somme_pop = sum(pop))

# On calcule le pourcentage
urbanisation_reg <- urbanisation_reg %>% group_by(code_region) %>% mutate(percent_pop = (somme_pop/sum(somme_pop) * 100))

# Création vble booléenne
urbanisation_reg <- urbanisation_reg %>% ungroup() %>% mutate(dens_is_1_2 = case_when((densite == 1 | densite == 2) ~ TRUE,
                                                                              TRUE ~ FALSE))

# Calcul somme et percent pour 1|2
df_is_1_2 <- urbanisation_reg[,-c(2,4)]
df_is_1_2 <- df_is_1_2 %>% group_by(code_region, dens_is_1_2) %>% mutate(somme_pop_is_1_2 = sum(somme_pop))
df_is_1_2 <- df_is_1_2[,-2] %>% distinct()
df_is_1_2 <- df_is_1_2 %>% group_by(code_region) %>% mutate(percent_pop_is_1_2 = (somme_pop_is_1_2/sum(somme_pop_is_1_2) * 100))
urbanisation_reg <- left_join(urbanisation_reg, df_is_1_2, by = c("code_region", "dens_is_1_2"), copy = FALSE)

# On applique la règle de décision de l'INSEE en matière de densité
urbanisation_reg <- urbanisation_reg %>% group_by(code_region) %>% mutate(niveau_rural_insee = case_when(densite == 1 & percent_pop > 50 ~ 1,
                                                                                                 dens_is_1_2 == TRUE & percent_pop_is_1_2 > 50 ~ 2,
                                                                                                 densite == 4 & percent_pop > 50 ~ 4,
                                                                                                 TRUE ~ 3))   #la première valeur par departement est la bonne

# On garde la bonne valeur càd la première par régions
urbanisation_reg <- urbanisation_reg %>%  group_by(code_region) %>% slice(1)

# On match au jeu des régions
regions <- left_join(regions, urbanisation_reg[,c(1,8)], by = c("COG" = "code_region"), copy = FALSE)




# C) Méthode d'Olivier Bouba Olga (part de la pop vivant en zone rurale)



# En repartant de la grille de densité de l'INSEE on regroupe les 4 modalités de densité en 2 classes (rural vs. urbain)
      # très dense et dense (càd 1 et 2) = urbain (càd 1) 
      # peu dense et très peu dense (càd 3 et 4) = rural (càd 2)
urbanisation_INSEE <- urbanisation_INSEE %>% mutate(densite_binaire = case_when(densite == 2 ~ 1, 
                                                                                  densite == 3 ~ 2, 
                                                                                  densite == 4 ~ 2))

    ### niveau départemental

# On somme la population par départements et par type de densité (urbain / rural)
urbanisation_dep <- urbanisation_INSEE %>% group_by(code_departement,densite_binaire) %>% summarise(somme_pop = sum(pop))

# On calcule le pourcentage
urbanisation_dep <- urbanisation_dep %>% group_by(code_departement) %>% mutate(percent_pop = (somme_pop/sum(somme_pop) * 100))

# On garde par département le pourcentage de population vivant dans une commune rurale (densite_binaire == 2) + on renomme
urbanisation_dep <- urbanisation_dep %>% filter(densite_binaire == 2) %>% rename(percent_pop_rurale = percent_pop)

# Puis on ajoute cette nouvelle variable aux données des départements
    # même format pour la colonne de jointure
urbanisation_dep$code_departement <- as.numeric(urbanisation_dep$code_departement)
departements$COG <- as.numeric(departements$COG)
    # match
departements <- left_join(departements, urbanisation_dep[,c(1,4)], by = c("COG" = "code_departement"), copy = FALSE)



    ### niveau régionnal

# On somme la population par régions et par type de densité (urbain / rural)
urbanisation_reg <- urbanisation_INSEE %>% group_by(code_region,densite_binaire) %>% summarise(somme_pop = sum(pop))

# On calcule le pourcentage
urbanisation_reg <- urbanisation_reg %>% group_by(code_region) %>% mutate(percent_pop = (somme_pop/sum(somme_pop) * 100))

# On garde par région le pourcentage de population vivant dans une commune rurale (densite_binaire == 2) + on renomme
urbanisation_reg <- urbanisation_reg %>% filter(densite_binaire == 2) %>% rename(percent_pop_rurale = percent_pop)

# Puis on ajoute cette nouvelle variable aux données des régions
regions <- left_join(regions, urbanisation_reg[,c(1,4)], by = c("COG" = "code_region"), copy = FALSE) 





# ------------------------------- MANIPULATIONS DES BASES



# On ajoute une colonne du type d'organisation pour les EPCI (à partir du jeu OFGL)
comptes_epci_type <- comptes_epci[,c(8,12)] %>% rename(type = `Nature juridique 2021 abrégée`, SIREN = `Code Siren 2021 EPCI`) %>% unique()
comptes_epci_type$type <-  str_replace_all(comptes_epci_type$type, c("MET69" = "M", "M" = "MET"))
epci <- left_join(epci, comptes_epci_type, by="SIREN")

# On retire toutes les variables qui ne sont pas utiles à l'analyse
          # chef de l'exécutif qui servait en étape intermédiaire pour récuperer le parti politique
          # les variables du jeu de l'observatoire des territoires avec les liens vers les portails open data etc.
regions <- regions[,-c(2:6,8,10,11,13:16,18)]
departements <- departements[,-c(2:4,6,7,9,11,12,14:17,19)]
communes <- communes[,-c(2:4,7,8,10,12,13,15:18,20)]
epci <- epci[,-c(2,3,6,7,9,11,12,14:17,19)]


# On renomme les variables de nos bases de données finales
regions <- regions %>% rename(nb_ptf = `nb-ptf`, 
                              nb_datagouv = `nb-datagouv`, 
                              pop_insee = `pop-insee`)
departements <- departements %>% rename(nb_ptf = `nb-ptf`, 
                                        nb_datagouv = `nb-datagouv`, 
                                        pop_insee = `pop-insee`)
communes <- communes %>% rename(nb_ptf = `nb-ptf`, 
                                nb_datagouv = `nb-datagouv`, 
                                pop_insee = `pop-insee`)
epci <- epci %>% rename(nb_ptf = `nb-ptf`, 
                        nb_datagouv = `nb-datagouv`, 
                        pop_insee = `pop-insee`)

# On remplace les NA mal notés (NULL, null, N/A etc.) par de vrais NAs.
regions[regions == "null" | regions == "N/A" | regions == "NULL" | regions == "NA"] <- NA
departements[departements == "null" | departements == "N/A" | departements == "NULL" | departements == "NA"] <- NA
communes[communes == "null" | communes == "N/A" | communes == "NULL" | communes == "NA"] <- NA
epci[epci == "null" | epci == "N/A" | epci == "NULL" | epci == "NA"] <- NA



    #--------------- Manips variables dépendantes


# Pour les orgas qui n'ouvrent pas de données on remplace les NA par 0 car pas valeur manquante mais plutôt : 0 données ouvertes
regions <- regions %>% mutate(nb_ptf = replace_na(nb_ptf, 0),
                           nb_datagouv = replace_na(nb_datagouv, 0))
departements <- departements %>% mutate(nb_ptf = replace_na(nb_ptf, 0),
                           nb_datagouv = replace_na(nb_datagouv, 0))
communes <- communes %>% mutate(nb_ptf = replace_na(nb_ptf, 0),
                           nb_datagouv = replace_na(nb_datagouv, 0))
epci <- epci %>% mutate(nb_ptf = replace_na(nb_ptf, 0),
                           nb_datagouv = replace_na(nb_datagouv, 0))


# On créé 3 variables : une pour savoir si l'orga est concernée par la loi Lemaire, une autre pour savoir si elle ouvre des données et une autre pour savoir COMBIEN de données elle ouvre (nb_ptf en priorité et si pas propre ptf alors on garde nb_datagouv)
    # obligation d'ouvrir ? Oui=1, Non=0  : communes, départements et EPCI ont tous pop > 3500 donc on la créé seulement pour les communes
min(regions$pop_insee)
min(departements$pop_insee, na.rm=T)
min(epci$pop_insee, na.rm=T)
    # empiriquement ouvre ? Oui=1, Non=0  : on la créé pour tous les types d'orgas car en pratique ce n'est pas tjs le cas

    ## Régions
regions$nb_publi <- case_when(regions$nb_ptf > 0 ~ regions$nb_ptf,
                              regions$nb_ptf == 0 ~ regions$nb_datagouv,
                              TRUE ~ 0)
regions$ouvre_data <- case_when(regions$nb_publi == 0 ~ 0,
                                regions$nb_publi > 0 ~ 1)
    ## Départements
departements$nb_publi <- case_when(departements$nb_ptf > 0 ~ departements$nb_ptf,
                                          departements$nb_ptf == 0 ~ departements$nb_datagouv,
                                          TRUE ~ 0)
departements$ouvre_data <- case_when(departements$nb_publi == 0 ~ 0,
                                     departements$nb_publi > 0 ~ 1)
    ## Communes
communes$obligation_ouvrir <- case_when(communes$pop_insee < 3500 ~ 0,
                                        communes$pop_insee >= 3500 ~ 1)
communes$nb_publi <- case_when(communes$nb_ptf > 0 ~ communes$nb_ptf,
                              communes$nb_ptf == 0 ~ communes$nb_datagouv,
                              TRUE ~ 0)
communes$ouvre_data <- case_when(communes$nb_publi == 0 ~ 0,
                                 communes$nb_publi > 0 ~ 1)
    ## EPCI
epci$nb_publi <- case_when(epci$nb_ptf > 0 ~ epci$nb_ptf,
                           epci$nb_ptf == 0 ~ epci$nb_datagouv,
                           TRUE ~ 0)
epci$ouvre_data <- case_when(epci$nb_publi == 0 ~ 0,
                             epci$nb_publi > 0 ~ 1)


# On réordonne pour avoir les variables à expliquer au début de la base 
    #(nb de jeux ouverts et aussi la vble binaire : ouverture de données ou pas?)
regions <- regions[,c(1,26,27,2:25)]
departements <- departements[,c(1,22,23,2:21)]
communes <- communes[,c(1,20,21,2:19)]
epci <- epci[,c(1,16:18,2:15)]


# Pour les régions il ne manque que 3 infos sur les chefs de l'exécutif donc on complète à la main pour avoir une base finie
regions[c(2,3),]$CSP_chef <- c("Professions Intermédiaires","Cadres et professions intellectuelles supérieures") #Alfred Marie-Jeanne pr Martinique de 2015 à 2021
regions[c(2,3),]$age_chef <- age(c("1936/11/15","1953/09/26"), units = "years") #Rodolphe Alexandre pr Guyane de 2015 à 2021
regions[c(2,3),]$partis_po_chef <- c("Mouvement indépendantiste martiniquais","Divers Gauche")


# Pareil pour les métropoles, manque infos pour 4 obs donc on ajoute manuellement
epci[c(1,1310),]$partis_po_chef <- c("Les républicains","Parti socialiste")  #Bordeaux MET (Patrick Bobet) et Grenoble MET
epci[1310,]$CSP_chef <- "Cadres et professions intellectuelles supérieures"  #Christophe Ferrari président Grenoble MET depuis 2014
epci[1310,]$age_chef <- age("1969/05/18")


# On met au bon format les variables
regions[,c(2:6,8,10:27)] <- lapply(regions[,c(2:6,8,10:27)], as.numeric) 
departements[,c(2:7,9,11:23)] <- lapply(departements[,c(2:7,9,11:23)], as.numeric) 
communes[,c(2:8,10,12:21)] <- lapply(communes[,c(2:8,10,12:21)], as.numeric)
epci[,c(3:9,11,13:18)] <- lapply(epci[,c(3:9,11,13:18)], as.numeric)


# On exporte les bases pour l'analyse
write.csv(regions,"./Data/process/regions.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(departements,"./Data/process/departements.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(communes,"./Data/process/communes.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(epci,"./Data/process/epci.csv", row.names = FALSE, fileEncoding = "UTF-8")



