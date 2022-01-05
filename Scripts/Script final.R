# Script analyse de la situation open data des territoires de France - Diane Thierry
# Aout 2021


# Packages nécessaires à l'analyse
packages = c("tidyverse","googlesheets4","tools","lubridate","WikidataQueryServiceR","curl","readxl","DescTools","stringr","summarytools","inspectdf","gridExtra","outliers","EnvStats","RColorBrewer","corrplot","rpart","caret","rpart.plot","randomForest","FactoMineR","factoextra","treemap","viridis","hrbrthemes","plotly","stats","MASS","pscl") # charger 'googlesheets4' à part si ne fonctionne pas
    
# Installation des packages si besoin et chargement des librairies
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)



###########################################################################################################

######################### CONSTITUTION DE LA BASE DE DONNEES ###############################################

############################################################################################################







#------------------------------- ON PART D'UN FICHIER AVEC TOUTES LES ORGAS DE FRANCE


# Fichier initial : toutes les orgas de France (collectivités + EPCI)
regions <- read_csv("https://www.data.gouv.fr/fr/datasets/r/cfd36469-30db-4ee9-a7e9-b98fbc71805c")
departements <- read_csv("https://www.data.gouv.fr/fr/datasets/r/4d8c420b-c412-4deb-a469-b722b195f9c7")
communes <- read_csv("https://www.data.gouv.fr/fr/datasets/r/863a5d6f-cc5c-40e0-a349-8a15719cb25e")
epci <- read_csv("https://www.data.gouv.fr/fr/datasets/r/e381a63d-09f5-44f5-9e72-7e889b899bf5")



#------------------------------- AJOUT DONNEES OBSERVATOIRE DES TERRITOIRES : FUTUR Y


# Import de la base complète : taper "1"
observatoire_opendata_territoire <- read_sheet("https://docs.google.com/spreadsheets/d/1yhcCDLrDsZzNxlPaIl25p_qN8WVPGCTstdIUxezDxtQ/edit#gid=614893302")


# Après vérification les données ne sont pas fiables : on importe une base avec des corrections pour Y et on les applique
modifs_OODT <- read_csv("Data/external/modifs_recensement_OODT.csv")
observatoire_opendata_territoire <- rbind(observatoire_opendata_territoire, modifs_OODT)

# On renomme la colonne de jointure et on la passe au format numérique
observatoire_opendata_territoire <- observatoire_opendata_territoire %>% rename(SIREN = siren)
observatoire_opendata_territoire$SIREN <- as.numeric(observatoire_opendata_territoire$SIREN)

# On sépare la base selon le type d'organisation
observatoire_reg <- observatoire_opendata_territoire %>% filter(type == "REG") #15
observatoire_dep <- observatoire_opendata_territoire %>% filter(type == "DEP") #61
observatoire_com <- observatoire_opendata_territoire %>% filter(type == "COM")  #353 obs
observatoire_epci <- observatoire_opendata_territoire %>% filter(type == "MET" | type == "CU" | type == "CC" | type == "CA")  # 17+5+43+99 = 168

# On match par les numéros de SIREN 
regions <- left_join(regions, observatoire_reg[,-c(2:3)], by="SIREN", na_matches="never")
departements <- left_join(departements[,-6], observatoire_dep[,-c(2:3)], by="SIREN", na_matches="never") #on retire le COG à 3 digits
communes <- left_join(communes[,-7], observatoire_com[,-c(2:3)], by="SIREN", na_matches="never") #de meme 
epci <- left_join(epci, observatoire_epci[,-c(2:3)], by="SIREN", na_matches="never")

# Pour Paris qui est à la fois une ville et un département mais n'est recensé qu'en commune sur les données OODT, on affecte les données nous meme
departements[77,8] <- c(276)
departements[77,10] <- c(451)




#------------------------------- AJOUT POPULATION 



# La population des territoires est incomplète dans le jeu OODT donc on récupère celle des données OFGL
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
                                      population = `Population totale`)
comptes_dep_pop <- comptes_dep_pop %>% rename(SIREN = `Code Siren Collectivité`,
                                      population = `Population totale`)
comptes_com_pop <- comptes_com_pop %>% rename(SIREN = `Code Siren Collectivité`,
                                      population = `Population totale`)
comptes_epci_pop <- comptes_epci_pop %>% rename(SIREN = `Code Siren 2021 EPCI`,
                                      population = `Population totale`)

# On supprime les doublons pour les EPCI
comptes_epci_pop <- comptes_epci_pop %>% group_by(SIREN) %>% distinct(SIREN, .keep_all=TRUE)

# On ajoute la population à nos bases
regions <- left_join(regions, comptes_reg_pop, by = "SIREN", na_matches="never")
departements <- left_join(departements, comptes_dep_pop, by = "SIREN", na_matches="never")
communes <- left_join(communes, comptes_com_pop, by = "SIREN", na_matches="never")
epci <- left_join(epci, comptes_epci_pop, by = "SIREN", na_matches="never")

# On supprime l'ancienne colonne de la pop incomplète et on renomme
regions <- regions[,-12] 
departements <- departements[,-13]
communes <- communes[,-14]
epci <- epci[,-13]

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
RNE_com <- read_delim("http://data.cquest.org/repertoire-national-des-elus/archives/20190705_9-rne-maires.txt.gz", "\t", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE, skip = 1)
RNE_epci <- read_delim("http://data.cquest.org/repertoire-national-des-elus/archives/20190705_2-rne-epci.txt.gz", "\t", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE, skip = 1)


# On filtre pour ne garder que les chefs (et pas tous les élus)
RNE_reg <- RNE_reg %>% filter(`Libellé de fonction` == "Président du conseil régional")
RNE_corse <- RNE_corse %>% filter(`Libellé de fonction` == "Président Conseil exécutif de Corse")
RNE_dep <- RNE_dep %>% filter(`Libellé de fonction` == "Président du conseil départemental")
RNE_epci <- RNE_epci %>% filter(`Libellé de fonction` == "Président")

# Le COG des communes est en 2 parties (COG dep et COG com) donc on rassemble pour avoir le code INSEE
RNE_com$COG <- paste(RNE_com$`Code du département (Maire)`, RNE_com$`Code Insee de la commune`, sep='')

# On renomme la colonne pivot (pour la jointure) pour les autres orgas
RNE_reg <- RNE_reg %>% rename(COG = `Code région`)
RNE_corse <- RNE_corse %>% rename(COG = `Code région`)
RNE_dep <- RNE_dep %>% rename(COG = `Code du département`)
RNE_epci <- RNE_epci %>% rename(SIREN = `N° SIREN`)

# Le SIREN de la métropole de Lyon entré dans le RNE est faux donc on le change pour le match (SIREN de la CU de Lyon 3è)
RNE_epci[921,]$SIREN <- 200046977

# On met au format numérique les colonnes pivot 
regions$COG <- as.numeric(regions$COG)
RNE_reg$COG <- as.numeric(RNE_reg$COG)
departements$COG <- as.numeric(departements$COG)
RNE_dep$COG <- as.numeric(RNE_dep$COG)
communes$COG <- as.numeric(communes$COG)
RNE_com$COG <- as.numeric(RNE_com$COG)
epci$SIREN <- as.numeric(epci$SIREN)
RNE_epci$SIREN <- as.numeric(RNE_epci$SIREN)

# On joint les données par le numéro (COG ou SIREN) en ne gardant que qq infos du chef (nom, prénom, CSP et date de naissance)
regions <- left_join(regions, rbind(unique(RNE_reg[,c(1,5,6,8,10)]),RNE_corse[,c(1,3,4,6,8)]), by="COG", na_matches="never")  #on ajoute les élus des régions où on rbind la corse
departements <- left_join(departements, RNE_dep[,c(1,3,4,10,11)], by="COG", na_matches="never")
communes <- left_join(communes, RNE_com[,c(5,6,8,10,13)], by="COG", na_matches="never")  #on retire une obs pour la martinique où 2 chefs (on garde le plus récent)
epci <- left_join(epci, RNE_epci[,c(2,7,8,10,12)], by="SIREN", na_matches="never")


# On met les noms et prénoms au format tel que sous wikidata pour match (ex : Michel Dupont)
    # on regroupe les colonnes nom et prénom
regions$chef_executif <- paste(regions$`Prénom de l'élu`, regions$`Nom de l'élu`) 
regions[regions == "NA NA"] <- NA     #on remet des NA où il faut
    # on met la première lettre du nom en majuscule puis autres en minuscules
regions$chef_executif <- tolower(regions$chef_executif)
    # puis 1ère lettre en maj
regions$chef_executif <- toTitleCase(regions$chef_executif)
  # on retire les noms et prenoms séparés
regions <- regions[,-c(17:18)]


#  Mêmes manips pour les autres organisations :
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




# Puisque les chefs ne sont pas tous à jour on récupère une liste générale de tous les politiciens français vivants et leur(s) parti(s) associé(s)
wikidata_partis_po <- query_wikidata('SELECT DISTINCT ?chefLabel ?partiLabel
WHERE {
  ?chef wdt:P31 wd:Q5 .    #tous les êtres humains
  ?chef wdt:P106 wd:Q82955 .    #on limite à ceux qui sont politiciens 
  ?chef wdt:P27 wd:Q142  .    #on limite aux pers françaises
  ?chef wdt:P102 ?parti .     #on recup le parti
  ?chef wdt:P569 ?date .   #on recup la date de naissance
  FILTER(YEAR(?date) > 1920).    #on trie pour ne garder que les politiciens "actuels" (moins de 100 ans)
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
regions <- left_join(regions, wikidata_partis_po[,2:3], by="chef_upper", na_matches="never") 
regions <- regions[,-20]  #on retire la colonne des noms en majuscules
    ## DEPARTEMENTS
departements <- left_join(departements, wikidata_partis_po[,2:3], by="chef_upper", na_matches="never") 
departements <- departements[,-21]
    ## COMMUNES
communes <- left_join(communes, wikidata_partis_po[,2:3], by="chef_upper", na_matches="never") 
communes <- communes[,-22]
    ## EPCI
epci <- left_join(epci, wikidata_partis_po[,2:3], by="chef_upper", na_matches="never") 
epci <- epci[,-21]


# On renomme la colonne de la couleur politique
regions <- regions %>% rename(partis_po_chef = partiLabel)
departements <- departements %>% rename(partis_po_chef = partiLabel)
communes <- communes %>% rename(partis_po_chef = partiLabel)
epci <- epci %>% rename(partis_po_chef = partiLabel)


## Nombre de partis politiques manquants pour les orgas
regions %>% count(is.na(partis_po_chef))  # 2/17
departements %>% count(is.na(partis_po_chef))  #29/100
communes %>% count(is.na(partis_po_chef))  # 34239/34931
epci %>% count(is.na(partis_po_chef))  # 1079/1261




#------------------------------- AJOUT STATISTIQUES LOCALES INSEE



# Import des bases depuis les fichiers téléchargés d'internet
stats_locales_reg <- read_excel("Data/external/stats_locales_regions.xlsx")
stats_locales_dep <- read_excel("Data/external/stats_locales_departements.xlsx")
stats_locales_com <- read_excel("Data/external/stats_locales_communes.xlsx")
stats_locales_epci <- read_excel("Data/external/stats_locales_epci.xlsx")

# On renomme les colonnes
    ## REGIONS
names(stats_locales_reg)
stats_locales_reg <- stats_locales_reg %>% 
    rename(COG = Code,
           nom = Libellé,
           part_plus65 = `Part des pers. âgées de 65 ans ou + 2018`,
           niveau_vie = `Médiane du niveau de vie 2018`,
           part_diplomes = `Part des diplômés d'un BAC+5 ou plus dans la pop. non scolarisée de 15 ans ou + 2018`,
           taux_chomage = `Taux de chômage annuel moyen 2020`,
           nb_crea_entps = `Nb créations d'entreprises 2020`,
           nb_nuitees_hotels = `Nb de nuitées dans les hôtels de tourisme 2019`,
           flux_migration_res = `Flux principal de migration résidentielle 2018`,
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
           part_plus65 = `Part des pers. âgées de 65 ans ou + 2018`,
           niveau_vie = `Médiane du niveau de vie 2018`,
           part_diplomes = `Part des diplômés d'un BAC+5 ou plus dans la pop. non scolarisée de 15 ans ou + 2018`,
           taux_chomage = `Taux de chômage annuel moyen 2020`,
           nb_crea_entps = `Nb créations d'entreprises 2020`,
           nb_nuitees_hotels = `Nb de nuitées dans les hôtels de tourisme 2019`,
           flux_migration_res = `Flux principal de migration résidentielle 2018`)
    ## COMMUNES
names(stats_locales_com)
stats_locales_com <- stats_locales_com %>% 
    rename(COG = Code,
           nom = Libellé,
           part_plus65 = `Part des pers. âgées de 65 ans ou + 2018`,
           part_diplomes = `Part des diplômés d'un BAC+5 ou plus dans la pop. non scolarisée de 15 ans ou + 2018`,
           nb_crea_entps = `Nb créations d'entreprises 2020`,
           flux_migration_res = `Flux principal de migration résidentielle 2018`)
    ## EPCI
names(stats_locales_epci)
stats_locales_epci <- stats_locales_epci %>% 
    rename(SIREN = Code,
           nom = Libellé,
           part_plus65 = `Part des pers. âgées de 65 ans ou + 2018`,
           niveau_vie = `Médiane du niveau de vie 2018`,
           part_diplomes = `Part des diplômés d'un BAC+5 ou plus dans la pop. non scolarisée de 15 ans ou + 2018`,
           nb_crea_entps = `Nb créations d'entreprises 2020`,
           flux_migration_res = `Flux principal de migration résidentielle 2018`)

# On met au bon format les colonnes de jointure
stats_locales_reg$COG <- as.numeric(stats_locales_reg$COG)
stats_locales_dep$COG <- as.numeric(stats_locales_dep$COG)
stats_locales_com$COG <- as.numeric(stats_locales_com$COG)
stats_locales_epci$SIREN <- as.numeric(stats_locales_epci$SIREN)

## On match par les COG pour les collectivités et SIREN pour les interco
regions <- left_join(regions, stats_locales_reg[,-2], by="COG", na_matches="never")
departements <- left_join(departements, stats_locales_dep[,-2], by="COG", na_matches="never")
communes <- left_join(communes, stats_locales_com[,-2], by="COG", na_matches="never")
epci <- left_join(epci, stats_locales_epci[,-2], by="SIREN", na_matches="never")




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
regions <- left_join(regions, comptes_reg_tot19[,c(7,13)], by="SIREN", na_matches="never")
departements <- left_join(departements, comptes_dep_tot19[,c(10,16)], by="SIREN", na_matches="never")  #du jeu des comptes on ne prend que le SIREN et le budget
communes <- left_join(communes, unique(comptes_com_tot19[,c(18,25)]), by="SIREN", na_matches="never")
epci <- left_join(epci, comptes_epci_tot19[,c(15,21)], by="SIREN", na_matches="never")


# On renomme la colonne du budget par un nom exploitable
regions <- regions %>% rename(depenses_hab = `Montant en € par habitant`)
departements <- departements %>% rename(depenses_hab = `Montant en € par habitant`)
communes <- communes %>% rename(depenses_hab = `Montant en € par habitant`)
epci <- epci %>% rename(depenses_hab = `Montant en € par habitant`)






#-------------------------------  AJOUT NB ETUDIANTS DANS LA POP  -------------------------------#


# Import de la base du nombre d'étudiants
Nb_etudiants_pop <- read_delim("https://data.enseignementsup-recherche.gouv.fr/explore/dataset/fr-esr-atlas_regional-effectifs-d-etudiants-inscrits/download/?format=csv&disjunctive.rgp_formations_ou_etablissements=true&refine.rgp_formations_ou_etablissements=Total+des+formations+d%27enseignement+sup%C3%A9rieur&refine.annee_universitaire=2018-19&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";", escape_double = FALSE, trim_ws = TRUE)

    # on garde le COG, le type de niveau géo et le nb d'étudiants
Nb_etudiants_pop <- Nb_etudiants_pop[, c(2,10,23)]
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

# Les COG des régions et départements contiennent la lettre (R ou D) donc match impossible : on extrait les 2 derniers chiffres de cette chaîne de caractères
Nb_etudiants_reg$COG <- str_sub(Nb_etudiants_reg$COG,-2)  #on garde 2 chiffres
Nb_etudiants_dep$COG <- str_sub(Nb_etudiants_dep$COG,-3)  #on garde 3 chiffres pour gérer les DROM COM
Nb_etudiants_dep$COG <- gsub("^0", "", Nb_etudiants_dep$COG)  #puis on retire le 1er chiffre si c'est un 0

# On met au bon format les colonnes de jointure 
Nb_etudiants_reg$COG <- as.numeric(Nb_etudiants_reg$COG)
Nb_etudiants_dep$COG <- as.numeric(Nb_etudiants_dep$COG)
Nb_etudiants_com$COG <- as.numeric(Nb_etudiants_com$COG)

# Matchs pour les niveaux géographiques disponibles (càd regions, départements et communes)
regions <- left_join(regions, Nb_etudiants_reg, by="COG", na_matches="never")
departements <- left_join(departements, Nb_etudiants_dep, by="COG", na_matches="never")
communes <- left_join(communes, Nb_etudiants_com, by="COG", na_matches="never")

# On transforme le nombre d'étudiants en taux pour voir si indicateur plus parlant (sélection entre les 2 pendant l'analyse exploratoire)
regions <- regions %>% mutate(part_etudiants = nb_etudiants/population*100)
regions$part_etudiants <- round(regions$part_etudiants,1)     # on arrondi
departements <- departements %>% mutate(part_etudiants = nb_etudiants/population*100)
departements$part_etudiants <- round(departements$part_etudiants,1)
communes <- communes %>% mutate(part_etudiants = nb_etudiants/population*100)
communes$part_etudiants <- round(communes$part_etudiants,1)






#-------------------------------  AJOUT URBANISATION  -------------------------------#


      # 1. Nouvelle définition du rural (x6 modalités)

# Import de la base à matcher 
curl_download("https://www.insee.fr/fr/statistiques/fichier/5039991/FET2021-D4.xlsx", "FET2021_D4.xlsx")
ruralite <- read_excel("FET2021_D4.xlsx", col_names = TRUE, skip = 2, sheet='Figure 5')

# On renomme les colonnes
ruralite <- ruralite %>% rename(COG = `Code géographique communal`,
                                niveau_rural = `Typologie urbain/rural`)

# Match avec le jeu de l'analyse
ruralite$COG <- as.numeric(ruralite$COG)
communes <- left_join(communes, ruralite, by="COG", na_matches="never")

# Pour plus de simplicité on remplace les catégories par un digit de 1 à 6 tel que :
  #- 1 : urbain dense 
  #- 2 : urbain densité intermédiaire
  #- 3 : rural sous forte influence d'un pôle
  #- 4 : rural sous faible influence d'un pôle
  #- 5 : rural autonome peu dense
  #- 6 : rural autonome très peu dense

communes$niveau_rural <- str_replace_all(communes$niveau_rural, c("urbain dense" = "1", "urbain densité intermédiaire" = "2", "rural sous forte influence d'un pôle" = "3", "rural sous faible influence d'un pôle" = "4", "rural autonome peu dense" = "5", "rural autonome très peu dense" = "6"))



      # 2. Grille de densité (x4 modalités)


# Import de la base à matcher 
download.file("https://www.insee.fr/fr/statistiques/fichier/2114627/grille_densite_2021.zip", "grille_densite.zip")
unzip("grille_densite.zip")
densite <- read_excel("grille_densite_2021_agrege.xlsx")

# On a :
  #- 1 : très dense
  #- 2 : dense
  #- 3 : peu dense
  #- 4 : très peu dense

# On renomme et garde les colonnes intéressantes
densite <- densite[,-2] %>% rename(COG = `\nCode \nCommune\n`,
                                   densite = `Degré de \nDensité de la commune\n`, 
                                   code_region = `Région\n`,
                                   pop = `Population \nmunicipale \n2018`)

# Comme il s'agit d'un autre indicateur de densité (4 classes et pas 6) on l'ajoute aux données des communes, puis on choisira l'agrégation adaptée avec l'analyse exploratoire
densite$COG <- as.numeric(densite$COG)
communes <- left_join(communes, unique(densite[,c(1,2)]), by = "COG", na_matches="never")  #on enlève les doublons
communes <- communes %>% rename(niveau_densite = densite)


        ### On élève aux niveaux régions et départements avec 3 méthodes :
                # - en calculant le mode à partir de la nouvelle définition du rural : "niveau_rural"
                # - en utilisant la méthode de l'INSEE à partir de la grille de densité : "niveau_densite"
                # - en utilisant la méthode de Olivier Bouba Olga à partir de la grille de densité : "niveau_densite"



# A) Calcul du mode


    ### niveau départemental

# On récupère les colonnes du COG département et niveau rural / urbain
ruralite_dep <- communes[,c(6,30)] %>% arrange(code_departement)
ruralite_dep$code_departement <- as.numeric(ruralite_dep$code_departement)
ruralite_dep$niveau_rural <- as.numeric(ruralite_dep$niveau_rural)

# On récupère le mode pour chaque département grâce au group_by()
ruralite_dep <- ruralite_dep %>% group_by(code_departement) %>% na.omit()
ruralite_dep <- ruralite_dep %>% summarise(Mode = Mode(niveau_rural))
ruralite_dep <- ruralite_dep %>% group_by(code_departement) %>% slice(1)  #doublon on choisi la modalité la plus urbaine (4 entre 4 et 5 par ex)

# On renomme la colonne de jointure en vu du match (COG)
ruralite_dep <- ruralite_dep %>% rename(COG = code_departement,
                                        niveau_rural = Mode)   #et le niveau d'urbanisation que l'on ajoute

# On affecte ces données au jeu des départements par un match via le COG
departements <- left_join(departements, ruralite_dep, by="COG", na_matches="never")


    ### niveau régionnal

# On récupère les colonnes du COG région et niveau rural / urbain
ruralite_reg <- communes[,c(5,30)] %>% arrange(code_region)
ruralite_reg$code_region <- as.numeric(ruralite_reg$code_region)
ruralite_reg$niveau_rural <- as.numeric(ruralite_reg$niveau_rural)

# On trouve le mode de chaque région 
ruralite_reg <- ruralite_reg %>% group_by(code_region) 
ruralite_reg <- ruralite_reg %>% summarise(Mode = Mode(niveau_rural))

# On renomme les colonnes
ruralite_reg <- ruralite_reg %>% rename(COG = code_region,
                                                niveau_rural = Mode)

# On ajoute un "0" avant les COG à un seul chiffre pour correspondre aux GOC du jeu 'regions'
ruralite_reg$COG <- sprintf("%02d", ruralite_reg$COG)

# On met au même format les numéros COG des 2 dfs
ruralite_reg$COG <- as.numeric(ruralite_reg$COG)

# On affecte ces données au jeu des régions par un match via le COG
regions <- left_join(regions, ruralite_reg, by="COG", copy=F)




# B) Méthode de zonage de l'INSEE (pondéré par la population, pas 1 commune = 1 voix)



    ### niveau départemental

# On récupère le numéro de département en matchant aux données communales où l'on a cette info
densite <- left_join(densite, communes[,c(3,6)], by = "COG", na_matches="never")

# On somme la population par départements et par type de densité (entre 1 et 4)
densite_dep <- densite %>% group_by(code_departement,densite) %>% summarise(somme_pop = sum(pop))

# On calcule le pourcentage
densite_dep <- densite_dep %>% group_by(code_departement) %>% mutate(percent_pop = (somme_pop/sum(somme_pop) * 100))

# Création d'une variable booléenne pour pouvoir appliquer la rgèle de décision
densite_dep <- densite_dep %>% ungroup() %>% mutate(dens_is_1_2 = case_when((densite == 1 | densite == 2) ~ TRUE,
                                                                              TRUE ~ FALSE))

# Calcul somme et percent pour 1|2
df_is_1_2 <- densite_dep[,-c(2,4)]
df_is_1_2 <- df_is_1_2 %>% group_by(code_departement, dens_is_1_2) %>% mutate(somme_pop_is_1_2 = sum(somme_pop))
df_is_1_2 <- df_is_1_2[,-2] %>% distinct()
df_is_1_2 <- df_is_1_2 %>% group_by(code_departement) %>% mutate(percent_pop_is_1_2 = (somme_pop_is_1_2/sum(somme_pop_is_1_2) * 100))
densite_dep <- left_join(densite_dep, df_is_1_2, by = c("code_departement", "dens_is_1_2"), na_matches="never")

# On applique la RDD de l'INSEE en matière de densité
densite_dep <- densite_dep %>% group_by(code_departement) %>% mutate(niveau_densite = case_when(densite == 1 & percent_pop > 50 ~ 1,
                                                                                                 dens_is_1_2 == TRUE & percent_pop_is_1_2 > 50 ~ 2,
                                                                                                 densite == 4 & percent_pop > 50 ~ 4,
                                                                                                 TRUE ~ 3))   #la première valeur par département est la bonne

# On garde la bonne valeur càd la première par département
densite_dep <- densite_dep %>%  group_by(code_departement) %>% slice(1)

# On match au jeu des départements
departements$COG <- as.numeric(departements$COG)
densite_dep$code_departement <- as.numeric(densite_dep$code_departement)
departements <- left_join(departements, densite_dep[,c(1,8)], by = c("COG" = "code_departement"), na_matches="never")



    ### niveau régionnal


# On somme la population par département et par type de densité (entre 1 et 4)
densite_reg <- densite %>% group_by(code_region,densite) %>% summarise(somme_pop = sum(pop))

# On calcule le pourcentage
densite_reg <- densite_reg %>% group_by(code_region) %>% mutate(percent_pop = (somme_pop/sum(somme_pop) * 100))

# Création de la variable booléenne
densite_reg <- densite_reg %>% ungroup() %>% mutate(dens_is_1_2 = case_when((densite == 1 | densite == 2) ~ TRUE,
                                                                              TRUE ~ FALSE))

# Calcul somme et percent pour 1|2
df_is_1_2 <- densite_reg[,-c(2,4)]
df_is_1_2 <- df_is_1_2 %>% group_by(code_region, dens_is_1_2) %>% mutate(somme_pop_is_1_2 = sum(somme_pop))
df_is_1_2 <- df_is_1_2[,-2] %>% distinct()
df_is_1_2 <- df_is_1_2 %>% group_by(code_region) %>% mutate(percent_pop_is_1_2 = (somme_pop_is_1_2/sum(somme_pop_is_1_2) * 100))
densite_reg <- left_join(densite_reg, df_is_1_2, by = c("code_region", "dens_is_1_2"), na_matches="never")

# On applique la RDD de l'INSEE en matière de densité
densite_reg <- densite_reg %>% group_by(code_region) %>% mutate(niveau_densite = case_when(densite == 1 & percent_pop > 50 ~ 1,
                                                                                           dens_is_1_2 == TRUE & percent_pop_is_1_2 > 50 ~ 2,
                                                                                           densite == 4 & percent_pop > 50 ~ 4,
                                                                                           TRUE ~ 3))   #la première valeur par departement est la bonne

# On garde la bonne valeur càd la première par régions
densite_reg <- densite_reg %>%  group_by(code_region) %>% slice(1)

# On match au jeu des régions
regions <- left_join(regions, densite_reg[,c(1,8)], by = c("COG" = "code_region"), na_matches="never")




# C) Méthode d'Olivier Bouba Olga (part de la pop vivant en zone rurale)



# En repartant de la grille de densité de l'INSEE on regroupe les 4 modalités de densité en 2 classes (rural vs. urbain)
      # très dense et dense (càd 1 et 2) = urbain (càd 1) 
      # peu dense et très peu dense (càd 3 et 4) = rural (càd 2)
densite <- densite %>% mutate(densite_binaire = case_when(densite == 1 ~ 1,
                                                          densite == 2 ~ 1, 
                                                          densite == 3 ~ 2, 
                                                          densite == 4 ~ 2))

    ### niveau départemental

# On somme la population par départements et par type de densité (urbain / rural)
pop_rurale_dep <- densite %>% group_by(code_departement,densite_binaire) %>% summarise(somme_pop = sum(pop))

# On calcule le pourcentage
pop_rurale_dep <- pop_rurale_dep %>% group_by(code_departement) %>% mutate(percent_pop = (somme_pop/sum(somme_pop) * 100))

# On garde par département le pourcentage de population vivant dans une commune rurale (densite_binaire == 2) + on renomme
pop_rurale_dep <- pop_rurale_dep %>% filter(densite_binaire == 2) %>% rename(percent_pop_rurale = percent_pop)

# Puis on ajoute cette nouvelle variable aux données des départements
pop_rurale_dep$code_departement <- as.numeric(pop_rurale_dep$code_departement) #bon format
departements <- left_join(departements, pop_rurale_dep[,c(1,4)], by = c("COG" = "code_departement"), na_matches="never")

# DEP 75,92,93,94 ; NA quand 0% rurale avec filtre dep rural, donc on affecte le taux 
departements <- departements %>% mutate(percent_pop_rurale = case_when(is.na(percent_pop_rurale) ~ 0,
                                                                       TRUE ~ percent_pop_rurale))



    ### niveau régionnal

# On somme la population par régions et par type de densité (urbain / rural)
pop_rurale_reg <- densite %>% group_by(code_region,densite_binaire) %>% summarise(somme_pop = sum(pop))

# On calcule le pourcentage
pop_rurale_reg <- pop_rurale_reg %>% group_by(code_region) %>% mutate(percent_pop = (somme_pop/sum(somme_pop) * 100))

# On garde par région le pourcentage de population vivant dans une commune rurale (densite_binaire == 2) + on renomme
pop_rurale_reg <- pop_rurale_reg %>% filter(densite_binaire == 2) %>% rename(percent_pop_rurale = percent_pop)

# Puis on ajoute cette nouvelle variable aux données des régions
regions <- left_join(regions, pop_rurale_reg[,c(1,4)], by = c("COG" = "code_region"), na_matches="never") 





# ------------------------------- MANIPULATIONS DES BASES



# On retire toutes les variables qui ne sont pas utiles à l'analyse
          # les numéros COG et SIREN qui servaient en étape intermédiaire pour ajouter les données à notre base initiale
          # chef de l'exécutif qui servait en étape intermédiaire pour récuperer le parti politique
          # les variables du jeu de l'observatoire des territoires avec les liens vers les portails open data etc.
regions <- regions[,-c(2:6,8,10,11,13:16,18)]
departements <- departements[,-c(2:4,6,7,9,11,12,14:17,19)]
communes <- communes[,-c(2:4,7,8,10,12,13,15:18,20)]
epci <- epci[,-c(2,6,7,9,11,12,14:17,19)]


# On renomme les variables de nos bases de données finales
regions <- regions %>% rename(nb_ptf = `nb-ptf`, 
                              nb_datagouv = `nb-datagouv`)
departements <- departements %>% rename(nb_ptf = `nb-ptf`, 
                                        nb_datagouv = `nb-datagouv`)
communes <- communes %>% rename(nb_ptf = `nb-ptf`, 
                                nb_datagouv = `nb-datagouv`)
epci <- epci %>% rename(nb_ptf = `nb-ptf`, 
                        nb_datagouv = `nb-datagouv`)


# On remplace les NA mal notés (NULL, null, N/A - résultat non disponible etc.) par de vrais NAs.
regions[regions == "null" | regions == "N/A - résultat non disponible" | regions == "NULL" | regions == "NA"] <- NA
departements[departements == "null" | departements == "N/A - résultat non disponible" | departements == "NULL" | departements == "NA"] <- NA
communes[communes == "null" | communes == "N/A - résultat non disponible" | communes == "NULL" | communes == "NA"] <- NA
epci[epci == "null" | epci == "N/A - résultat non disponible" | epci == "NULL" | epci == "NA"] <- NA



    #--------------- Manips variables dépendantes


# Pour les orgas qui n'ouvrent pas de données on remplace les NA par 0 car pas valeur manquante mais plutôt : 0 donnée ouverte
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
min(regions$population)
min(departements$population, na.rm=T)
min(epci$population, na.rm=T)
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
communes$obligation_ouvrir <- case_when(communes$population < 3500 ~ 0,
                                        communes$population >= 3500 ~ 1)
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
epci <- epci[,c(1,2,17,18,3:16)]



    #--------------- Compléter infos manquantes quand il en manque peu dans les bases (REG, DEP, MET)


# On concentre l'analyse sur la France métropolitaine donc on supprime les territoires d'outre mer
regions <- regions[-c(1:5,27:29),]
departements <- departements[-c(37,38,86,121:124),] #on supprime aussi la MET de Lyon où bcp de NA car nouveau département pas tjs recensé
departements <- departements[-118,]
communes <- communes %>% filter(code_departement < 96)
epci <- epci %>% filter(code_departement < 96)


# On complète les valeurs des départements pour concentrer l'analyse sur eux
  # nb de NA
NA_dep <- as.data.frame(apply(is.na(departements), 2, sum)) %>% 
                        rename(nb_NA = `apply(is.na(departements), 2, sum)`) %>%
                        mutate(percent_NA = nb_NA/nrow(departements)*100) %>% 
                        mutate(percent_NA = round(percent_NA, 2)) #manque toutes infos sur 2 chefs + 22 partis po
View(NA_dep)
  # ajout infos sur les chefs qd valeur manquante
departements[91,c(8:10)] <- communes[29457,c(9:11)]
departements[c(3,4,7,8,10,16,23,34,35,40,43,54,57,58,73,85,96,97,104,108,109,117),]$partis_po_chef <- c("Union des démocrates et indépendants","Parti socialiste","Parti socialiste","Les Républicains","divers droite", "Les Républicains","Les Républicains","Les Républicains","Les Républicains","Parti socialiste","Les Républicains","Parti socialiste","divers droite","divers droite","Les Républicains","sans étiquette","Les Républicains","Les Républicains","Les Républicains","Les Républicains","Les Républicains","Les Républicains")


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









###########################################################################################################

###################################### ANALYSE EXPLORATOIRE ###############################################

############################################################################################################








# On importe la base des départements
departements <- read_csv("Data/process/departements.csv")

# Les variables :
vbles_quanti <- c("nb_publi","nb_ptf","nb_datagouv","taux_chomage","part_plus65","part_diplomes","depenses_hab","part_etudiants","percent_pop_rurale","population","age_chef","niveau_vie","nb_crea_entps","nb_nuitees_hotels","nb_etudiants")
vbles_quali <- c("nom","ouvre_data","niveau_rural","niveau_densite","flux_migration_res","partis_po_chef","CSP_chef")

# On met au bon format les variables qualitatives
departements[,c("ouvre_data","niveau_rural","niveau_densite","flux_migration_res","partis_po_chef","CSP_chef")] <- lapply(departements[,c("ouvre_data","niveau_rural","niveau_densite","flux_migration_res","partis_po_chef","CSP_chef")], as.factor)



#----------------------- Harmonisation couleur politique


# On regarde tous les partis politiques de la base
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

# partis po harmonisés, on supprime les doublons (quand qq1 avait plusieurs partis renseignés, dû aux noms des mouvements qui changent)
departements <- departements %>% unique()




#----------------------- Harmonisation CSP du chef de l'exécutif


# On regarde toutes les CSP de la base
as.data.frame(table(departements$CSP_chef)) %>% arrange(desc(Freq))

# On remplace par le niveau 1 des CSP, c'est-à-dire le plus général
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
niveau_rural <- as.data.frame(table(departements$niveau_rural)) %>% rename(num = Var1) 
niveau_rural <- niveau_rural %>% mutate(description = c("urbain dense","urbain densité intermédiaire", "rural sous forte influence d'un pôle", "rural sous faible influence d'un pôle", "rural autonome peu dense", "rural autonome très peu dense"),
                                        variable = "niveau_rural")
# Description niveau_densite
niveau_densite <- as.data.frame(table(departements$niveau_densite)) %>% rename(num = Var1)
niveau_densite <- niveau_densite %>% mutate(description = c("très dense", "dense", "peu dense"),
                                            variable = "niveau_densite")
# Description CSP_chef
CSP_chef <- as.data.frame(table(departements$CSP_chef)) %>% rename(num = Var1) 
CSP_chef <- CSP_chef %>% mutate(description = c("Agriculteurs exploitants","Artisans, commerçants et chefs d'entreprise","Cadres et professions intellectuelles supérieures","Professions Intermédiaires", "Employés","Retraités","Autres personnes sans activité professionnelle"),
                                variable = "CSP_chef") 
# Description flux_migration_res
flux_migration_res <- as.data.frame(table(departements$flux_migration_res)) %>% rename(num = Var1)
  # on va recouper avec le nom du département (import base)
infos_dep <- read_csv("https://www.data.gouv.fr/fr/datasets/r/4d8c420b-c412-4deb-a469-b722b195f9c7")
infos_dep$COG <- as.character(infos_dep$COG)
flux_migration_res <- left_join(flux_migration_res, infos_dep[,c(1,3)], by = c("num" = "COG")) %>% rename(description = nom)
flux_migration_res <- flux_migration_res %>% mutate(variable = "flux_migration_res") 
flux_migration_res[20,]$description <- "Corse"  # pour la Corse on met à la main car pas dans infos_dep

# Description code_region
code_region <- as.data.frame(table(departements$code_region)) %>% rename(num = Var1)
  # on va recouper avec le nom de la région (import base)
infos_reg <- read_csv("https://www.data.gouv.fr/fr/datasets/r/cfd36469-30db-4ee9-a7e9-b98fbc71805c")
infos_reg$COG <- as.character(infos_reg$COG)
code_region <- left_join(code_region, infos_reg[,c(1,3)], by = c("num" = "COG")) %>% rename(description = nom)
code_region <- code_region %>% mutate(variable = "code_region")


# On met tout ça ensemble pour avoir un dictionnaire des variables
dico_variables <- rbind(niveau_rural, niveau_densite, CSP_chef, flux_migration_res, code_region)
dico_variables <- dico_variables[,c(4,1,3,2)]
View(dico_variables)




                ### A) Analyse non supervisée





#----------------------- Statistiques descriptives


# Statistiques
view(dfSummary(departements))

# Répartitions des variables qualis
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

grid.arrange(g1, g2, g3, g4, ncol=2, nrow = 2)
grid.arrange(g5, g6, g7, g8, g9, g10, g11, g12, g13, ncol=3, nrow = 3)

# Pour savoir le nombre de points potentiellement atypiques sur le violin plot pour nb_crea_entps
  #on compte le nombre d'observations supérieures au 9è decile
departements %>% filter(nb_crea_entps > (quantile(nb_crea_entps, probs = .9))) %>% count()  #10


# Test de Grubbs quand 1 point potentiellement atypique
    # taux_chomage
grubbs.test(departements$taux_chomage, type=10, two.sided = TRUE)
order(departements$taux_chomage)  #outlier qd taux ≥ 12.5, obs n°65 càd Pyrénées Orientales


# Test de Rosner quand plus d'1 point potentiellement atypique
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
departements$nb_publi <- as.integer(departements$nb_publi)
plot(table(departements$nb_publi), xlab="nombre de publications open data", ylab="densité", main="", col="#666666")


# Variables explicatives
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



# Corrélations (matrice de Spearman) des variables quantitatives
cor1 <- cor(departements_sans_outliers[,c("nb_publi","taux_chomage","part_plus65","part_diplomes","depenses_hab","part_etudiants","percent_pop_rurale","population","age_chef","niveau_vie","nb_crea_entps","nb_nuitees_hotels","nb_etudiants")], use="complete.obs", method=c("spearman"))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor1, method="color", col=col(200), 
             type="upper",
             addCoef.col = "black")
      # 10 corrélations moyennes (0.5<x=<0.6) :
      # 24 corrélations fortes (x>0.6) :


# Dépendances des variables qualitatives
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
departements_sans_outliers <- departements_sans_outliers %>% mutate(part_diplomes_5.65 = case_when(part_diplomes >= 5.65 ~ 1,
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




#----------------------- Diagrammes croisés



# On remplace les modalités des partis politiques par des integers
departements_sans_outliers$pol2 <- str_replace_all(departements_sans_outliers$partis_po_chef, c("Gauche"="1", "Droite"="2", "sans étiquette" = "3"))

# Croisements Y (nb_publi) avec variables qualitatives
t1=tapply(departements_sans_outliers$nb_publi, departements_sans_outliers$niveau_rural, median) %>% sort
t2=tapply(departements_sans_outliers$nb_publi, departements_sans_outliers$niveau_densite, median) %>% sort()
t4=tapply(departements_sans_outliers$nb_publi, departements_sans_outliers$pol2, median) %>% sort()
t5=tapply(departements_sans_outliers$nb_publi, departements_sans_outliers$CSP_chef, median) %>% sort()

# Graphiques
par(mfrow=c(2,2))
barplot(t1[-1], horiz=TRUE, xlim=c(0,50), main="Nombre de jeux ouverts médian selon le 
        niveau de ruralité", col=brewer.pal(n = 5, name = "Blues"), cex.names=1, cex.main=1.1, col.main="#0033CC")

barplot(t2, horiz=TRUE, xlim=c(0,50), main="Nombre de jeux ouverts médian selon le 
        niveau de densité", col = brewer.pal(n = 3, name = "Reds"), cex.names=1, cex.main=1.1, col.main="#FF6600")

barplot(t4, horiz=TRUE, xlim=c(0,50), main="Nombre de jeux ouverts médian selon la 
        couleur politique", col=brewer.pal(n = 3, name = "Purples"), cex.names=1, cex.main=1.1, col.main="#756BB1")

barplot(t5, horiz=TRUE, xlim=c(0,50), main="Nombre de jeux ouverts médian selon la 
        CSP du chef de l'exécutif", col=brewer.pal(n = 7, name = "Greens"), cex.names=1, cex.main=1.1, col.main="#006600") #Cat 2 sous représentées donc attention aux conclusions

table(departements_sans_outliers$niveau_rural)
table(departements_sans_outliers$niveau_densite)
table(departements_sans_outliers$pol2)
table(departements_sans_outliers$CSP_chef)




#----------------------- Treemap


# On récupère les noms des régions
departements_sans_outliers$code_region <- as.character(departements_sans_outliers$code_region)
departements_sans_outliers <- left_join(departements_sans_outliers, infos_reg[,c(1,3)], by = c("code_region" = "COG"))
departements_sans_outliers <- departements_sans_outliers %>% rename(region = nom.y)

# Data pour savoir les départements de quelles régions ouvrent le plus de données
t6=tapply(departements_sans_outliers$nb_publi, departements_sans_outliers$region, mean) 
t6 <- t6 %>% as.data.frame() %>% mutate(nom = rownames(t6))
t6$label <- paste(t6$nom, "-", round(t6$.,0), "publications", sep=" ")

# Plot
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

table(departements_sans_outliers$code_region) #Nb d'obs trop différent d'une région à l'autre, pas de conclusions.




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



  # niveau_ruralité et ouvre_data
    # légende
departements_sans_outliers$ouvre_data <- str_replace_all(departements_sans_outliers$ouvre_data, "1", "oui")
departements_sans_outliers$ouvre_data <- str_replace_all(departements_sans_outliers$ouvre_data, "0", "non")
    # table
t7 <- departements_sans_outliers %>% group_by(ouvre_data) %>% count(niveau_densite)
  # plot
g1 <- ggplot(t7, aes(fill=ouvre_data, y=n, x=niveau_densite)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T, name="Ouvre ?", option="E", direction=1) +
    theme_ipsum() +
    xlab("") + ylab("Nombre de départements") + ggtitle("Ouverture de données selon 
le niveau de densité") +
    theme(axis.title.y = element_text(size=12)) + ylim(0, 50)


  # partis_po_chef et ouvre_data
t8 <- departements_sans_outliers %>% group_by(ouvre_data) %>% count(pol2)
  # plot
g2 <- ggplot(t8, aes(fill=ouvre_data, y=n, x=pol2)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T, name="Ouvre ?", option="E", direction=1) +
    theme_ipsum() +
    xlab("") + ylab("Nombre de départements") + ggtitle("Ouverture de données selon 
la couleur politique") +
    theme(axis.title.y = element_text(size=12))+ ylim(0, 50)


  # CSP_chef et ouvre_data
t9 <- departements_sans_outliers %>% group_by(ouvre_data) %>% count(CSP_chef)
  # plot
g3 <- ggplot(t9, aes(fill=ouvre_data, y=n, x=CSP_chef)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T, name="Ouvre ?", option="E", direction=1) +
    theme_ipsum() +
    xlab("") + ylab("Nombre de départements") + ggtitle("Ouverture de données selon 
la CSP du chef") +
    theme(axis.title.y = element_text(size=12))+ ylim(0, 50)

grid.arrange(g1,g2,g3, ncol=3, nrow=1)


# Comparaison des niveaux de ruralité avec et sans outliers
g1 <- ggplot(t7, aes(fill=ouvre_data, y=n, x=niveau_densite)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T, name="Ouvre ?", option="E", direction=1) +
    theme_ipsum() +
    xlab("") + ylab("Nombre de départements") + ggtitle("Ouverture de données selon 
le niveau de densité sans outliers") +
    theme(axis.title.y = element_text(size=12)) + ylim(0, 47)

    # tables
departements$ouvre_data <- str_replace_all(departements$ouvre_data, "1", "oui")
departements$ouvre_data <- str_replace_all(departements$ouvre_data, "0", "non")
t7bis <- departements %>% group_by(ouvre_data) %>% count(niveau_densite)
  # plot
g4 <- ggplot(t7bis, aes(fill=ouvre_data, y=n, x=niveau_densite)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T, name="Ouvre ?", option="E", direction=1) +
    theme_ipsum() +
    xlab("") + ylab("Nombre de départements") + ggtitle("Ouverture de données selon 
le niveau de densité avec outliers") +
    theme(axis.title.y = element_text(size=12)) + ylim(0, 47)

grid.arrange(g1,g4, ncol = 2, nrow = 1)



#-------------------- Export des bases

rio::export(departements, "./Data/process/dep_analyse_explo.csv")
rio::export(departements_sans_outliers[,1:27], "./Data/process/dep_analyse_explo_sans_outliers.csv")





###########################################################################################################

############################################# MODELISATIONS ###############################################

############################################################################################################







# On importe la base des départements
departements_sans_outliers <- read_csv("Data/process/dep_analyse_explo_sans_outliers.csv")

# On met au bon format les variables catégorielles
departements_sans_outliers[,c("part_diplomes_5.65","code_region","CSP_chef","niveau_rural","niveau_densite","partis_po_chef")] <- lapply(departements_sans_outliers[,c("part_diplomes_5.65","code_region","CSP_chef","niveau_rural","niveau_densite","partis_po_chef")], as.factor)
departements[,c("code_region","CSP_chef","niveau_rural","niveau_densite","partis_po_chef")] <- lapply(departements[,c("code_region","CSP_chef","niveau_rural","niveau_densite","partis_po_chef")], as.factor)

# De même pour Y
departements_sans_outliers$nb_publi <- as.integer(departements_sans_outliers$nb_publi)
y <- departements_sans_outliers$nb_publi




#-----------------------  Sélection de variables 


# Méthode bacward, forward et both pour trouver les variables qu'il faut garder pour le modèle
  # Forward
modele <- glm((nb_publi ~ 1), data = departements_sans_outliers)
modele.forward <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data = departements_sans_outliers, direction = "forward")
summary(modele.forward)

  # Bacward
modele <- glm((nb_publi ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data= departements_sans_outliers)
modele.backward <- step(modele,scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data = departements_sans_outliers, direction = "backward")
summary(modele.backward)  #choix vble car 4 sont correlées : on prend la plus significative

  # Both
modele <- glm((nb_publi ~ 1), data = departements_sans_outliers)
modele.both <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data = departements_sans_outliers, direction = "both")
summary(modele.both)

 


#-----------------------  Premier modèle (variables sélectionnées par les méthodes)

# Régression de poisson
modele <- glm(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65+nb_crea_entps, data = departements_sans_outliers, family = poisson(link=log))
summary(modele)
AIC(modele)


# Vérification des conditions

    # on vérifie l'adéquation entre la distribution théorique et observée
mean(departements_sans_outliers$nb_publi)
set.seed(1234) # pour simuler tjs les mêmes comptages
theoretic_count <- rpois(72, 13.44)  # calcul des comptages théoriques
tc_df <- data.frame(theoretic_count)  # on incorpore dans un df

    # on plot simultanément les comptages observés et les comptages théoriques
ggplot(departements_sans_outliers, aes(nb_publi))+
   geom_bar(fill="#1E90FF")+
   geom_bar(data=tc_df, aes(theoretic_count,fill="#1E90FF", alpha=0.5))+
    labs(title="Comptages théoriques et comptages observés",
        x ="Nombre de publications", y = "Fréquence")+
   theme_classic()+
   theme(legend.position="none")  #surdispersion et inflation en zero


# On compare la moyenne et la variance de la série
var(departements_sans_outliers$nb_publi)
mean(departements_sans_outliers$nb_publi)

# On caclule le ratio de dispersion
disp <- modele$deviance/modele$df.residual
disp  #sur dispersion car disp >1



# Pour pallier au problème on régresse en loi quasi poisson
modele_qp <- glm(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65, data = departements_sans_outliers, family = quasipoisson(link=log))
summary(modele_qp)

# Puis avec la binomiale négative
nb_fit <- glm.nb(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65, data = departements_sans_outliers)
summary(nb_fit)
AIC(nb_fit)

# On regarde la distribution de Y
table(departements_sans_outliers$nb_publi) #26 Y=0 soit 36%



#-----------------------  Modèles à inflation de zéro 


#--- Sélection de variables pour la partie binaire


# Méthode bacward, forward et both pour trouver les variables qu'il faut garder pour le modèle
  # Forward
modele <- glm((ouvre_data ~ 1), data = departements_sans_outliers)
modele.forward <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data = departements_sans_outliers, direction = "forward")
summary(modele.forward)

  # Bacward
modele <- glm((ouvre_data ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data= departements_sans_outliers)
modele.backward <- step(modele,scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data = departements_sans_outliers, direction = "backward")
summary(modele.backward)  

  # Both
modele <- glm((ouvre_data ~ 1), data = departements_sans_outliers)
modele.both <- step(modele, scope = list(lower = ~ 1, upper = ~ taux_chomage+CSP_chef+niveau_rural+niveau_densite+part_plus65+part_diplomes+depenses_hab+part_etudiants+percent_pop_rurale+population+age_chef+niveau_vie+nb_crea_entps+nb_nuitees_hotels+nb_etudiants+partis_po_chef+dynamisme+pauvrete+part_diplomes_5.65), data = departements_sans_outliers, direction = "both")
summary(modele.both)


#--- Estimations

    # Loi de poisson
fm_zip <- zeroinfl(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65 | CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65+age_chef, data=departements_sans_outliers)
summary(fm_zip) #on retire les variables non significatives
fm_zip2 <- zeroinfl(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65 | part_diplomes_5.65+age_chef, data=departements_sans_outliers)
summary(fm_zip2)
fm_zip3 <- zeroinfl(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65 | part_diplomes_5.65, data=departements_sans_outliers)
summary(fm_zip3)
AIC(fm_zip3)
    # Loi binomiale négative
fm_zinb <- zeroinfl(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65 | CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65+age_chef, dist="negbin", data=departements_sans_outliers)
summary(fm_zinb) #rien n'est significatif
fm_zinb2 <- zeroinfl(nb_publi ~ niveau_densite+part_diplomes_5.65 | 1, dist="negbin", data=departements_sans_outliers)
summary(fm_zinb2) #rien n'est significatif
AIC(fm_zinb2)


    
#-----------------------  Modèle double hurdle


# Loi de poisson
mod.hurdle <- hurdle(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65 | CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65+age_chef, data=departements_sans_outliers, dist="poisson", zero.dist="binomial")
summary(mod.hurdle)

mod.hurdle2 <- hurdle(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65 | part_diplomes_5.65+age_chef, data=departements_sans_outliers, dist="poisson", zero.dist="binomial")
summary(mod.hurdle2)

mod.hurdle3 <- hurdle(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65 | part_diplomes_5.65, data=departements_sans_outliers, dist="poisson", zero.dist="binomial")
summary(mod.hurdle3)
AIC(mod.hurdle3)


# Loi négative binomial
mod.hurdle.nb <- hurdle(nb_publi ~ CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65 | CSP_chef+niveau_densite+partis_po_chef+part_diplomes_5.65+age_chef, data= departements_sans_outliers, dist="negbin")
summary(mod.hurdle.nb) 

mod.hurdle.nb2 <- hurdle(nb_publi ~ niveau_densite+part_diplomes_5.65 | CSP_chef+part_diplomes_5.65+age_chef, data= departements_sans_outliers, dist="negbin")
summary(mod.hurdle.nb2) 

mod.hurdle.nb3 <- hurdle(nb_publi ~ niveau_densite+part_diplomes_5.65 | part_diplomes_5.65+age_chef, data= departements_sans_outliers, dist="negbin")
summary(mod.hurdle.nb3) #mieux Poisson car log(tetha) pas significatif

mod.hurdle.nb4 <- hurdle(nb_publi ~ niveau_densite+part_diplomes_5.65 | part_diplomes_5.65, data= departements_sans_outliers, dist="negbin")
summary(mod.hurdle.nb4) #mieux Poisson car log(tetha) pas significatif
AIC(mod.hurdle.nb4)



#-----------------------  Comparaisons des estimations


#--- Test de surdispersion
odTest(nb_fit)  #binomial négatif plus adapté


#--- Test de Vuong
vuong(modele, fm_zip3) #z-stat>2 dc ZIP mieux que poisson
vuong(nb_fit, fm_zinb2)  #mitigé

vuong(modele, mod.hurdle3) #loi de Poisson
vuong(nb_fit, mod.hurdle.nb4) #loi binomiale négative

vuong(fm_zip3, mod.hurdle3) #loi de Poisson
vuong(fm_zinb2, mod.hurdle.nb4) #loi binomiale négative


#--- Résultats des modèles parmi lesquels il faut choisir
    # significativité log(theta)
summary(fm_zinb2) #log(theta) pas signif donc fm_zip mieux  
summary(mod.hurdle.nb4) 
    # valeurs des coefficients
summary(fm_zip3) 
summary(mod.hurdle3) 




#-----------------------  Modèle final : Double hurdle en loi de Poisson



# Calcul du pseudo R2 
rd <- ifelse(y==0,0,y*log(y/mod.hurdle3$fitted.values))-(y-mod.hurdle3$fitted.values) 
print(rd) 
DS = 2 * sum(rd) 
print(DS)
a0 <- log(mean(y)) 
print(a0) 
rd0 <- ifelse(y==0,0,y *log(y)) - y * a0 - (y - exp(a0)) 
D0 <- 2 * sum(rd0) 
print(D0)
R2 <- (D0-DS)/D0 
print(R2)


# Interpretations des résultats
exp(coef((mod.hurdle3)))


# Résidus pour voir les départements les mieux ou moins bien prédits
res <- as.data.frame(residuals(mod.hurdle3))
res$nom <- departements_sans_outliers$nom
res$abs_res <- abs(res$re)

