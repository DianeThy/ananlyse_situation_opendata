#------------------------------- 1er IMPORT BASE COMPLETE

library(tidyverse)
library(readxl)
opendata <- read_excel("C:/Users/diane/Desktop/Analyse Diane/Data/raw/Indicateurs ODATER v2 (up-to-date).xlsx")

# On sépare la base selon le type d'organisation
commune <- opendata %>% filter(type == "COM")  #351 obs
communaute_urbaine <- opendata %>% filter(type == "CU") #5 
departement <- opendata %>% filter(type == "DEP") #61
metropole <- opendata %>% filter(type == "MET") #17
region <- opendata %>% filter(type == "REG") #15
CC <- opendata %>% filter(type == "CC") #43
CA <- opendata %>% filter(type == "CA") #99

# On exporte
#rio::export(commune, "./Data/raw/commune.xlsx")
#rio::export(communaute_urbaine, "./Data/raw/communaute_urbaine.xlsx")
#rio::export(departement, "./Data/raw/departement.xlsx")
#rio::export(metropole, "./Data/raw/metropole.xlsx")
#rio::export(region, "./Data/raw/region.xlsx")
#rio::export(CA, "./Data/raw/CA.xlsx")
#rio::export(CC, "./Data/raw/CC.xlsx")


#------------------------------- DECOMPTES AVANT AJOUT RNE

# Import des bases passées sous wikidata pour compte du nb d'infos qu'il manque
    # DEPARTEMENT
departement_pol <- read_csv("C:/Users/diane/Desktop/Analyse Diane/Data/interim/Wikidata_1er_passage/departement_pol.csv")
departement_pol %>% count(is.na(`parti politique`)) #30
NA_CE_DEP <- departement_pol %>% group_by(siren) %>% count(is.na(`chef de l'exécutif`))
NA_CE_DEP <- NA_CE_DEP[-62,]
colnames(NA_CE_DEP) <- c("A","B","C")
NA_CE_DEP %>% filter(B =="TRUE") %>% nrow()

    # COMMUNE
commune_pol <- read_csv("C:/Users/diane/Desktop/Analyse Diane/Data/interim/Wikidata_1er_passage/commune_pol.csv")
    # compte nb d'infos manquantes pour ces 2 colonnes
commune_pol %>% count(is.na(`parti politique`)) #289
NA_CE_COM <- commune_pol %>% group_by(siren) %>% count(is.na(`chef de l'exécutif`))
NA_CE_COM <- NA_CE_COM[-c(352:353),]
colnames(NA_CE_COM) <- c("A","B","C")
NA_CE_COM %>% filter(B =="TRUE") %>% nrow()

    # METROPOLE
metropole_pol <- read_csv("C:/Users/diane/Desktop/Analyse Diane/Data/interim/Wikidata_1er_passage/metropole_pol.csv")
    # compte nb d'infos manquantes pour ces 2 colonnes
metropole_pol %>% count(is.na(`parti politique`)) #289
NA_CE_MET <- metropole_pol %>% group_by(siren) %>% count(is.na(`chef de l'exécutif`))
NA_CE_MET <- NA_CE_MET[-18,]
colnames(NA_CE_MET) <- c("A","B","C")
NA_CE_MET %>% filter(B =="TRUE") %>% nrow()


#------------------------------- DECOMPTES APRES AJOUT RNE
    
# Import commune_pol où ont été ajoutés chefs de l'exécutif et parti politique de wikidata (via openrefine)
commune_pol2 <- read_csv("C:/Users/diane/Desktop/Analyse Diane/Data/interim/Wikidata_2e_passage_ajout_chefs_AnneLaure/commune_pol2.csv")
    # compte nb d'infos manquantes pour ces 2 colonnes
commune_pol2 %>% count(is.na(`parti politique`)) #264
NA_CE_COM2 <- commune_pol2 %>% group_by(siren) %>% count(is.na(`chef de l'exécutif`))
NA_CE_COM2 <- NA_CE_COM2[-c(352:353),]
colnames(NA_CE_COM2) <- c("A","B","C")
NA_CE_COM2 %>% filter(B =="TRUE") %>% nrow()





    ### Obtention parti politique des communes (à partir des listes présentes sur données des élections municipales)


#------------------------- Manips base datagouv

# Import
municipales_datagouv <- read_excel("C:/Users/diane/Desktop/Analyse Diane/Data/external/livre-des-listes-et-candidats_12-04.xlsx")

# On enlève les colonnes non utiles
municipales_datagouv <- municipales_datagouv[,-c(1:3,4:7,9,10,13,14)]

# On rassemble nom et prénom en 1 colonne
municipales_datagouv$chef_executif <- paste(municipales_datagouv$`Prénom candidat`,municipales_datagouv$`Nom candidat`)

# On met les noms au même format que base commune_pol2 (ex : Michel Dupond)
    # tout en minuscule
municipales_datagouv$chef_executif <- tolower(municipales_datagouv$chef_executif)
    # puis 1ère lettre en maj
library(tools)
municipales_datagouv$chef_executif <- toTitleCase(municipales_datagouv$chef_executif)

# On retire les noms et prenoms séparés
municipales_datagouv <- municipales_datagouv[,-c(2:3)]

# On enlève les accents des noms pour le match (ex : Christophe Béchu vs. Christophe Bechu)
municipales_datagouv <- data.table::data.table(municipales_datagouv)
municipales_datagouv[, chef_executif := stringi::stri_trans_general (str = chef_executif, id = "Latin-ASCII")]

# On enlève les doublons (maires qui se présentent dans 2 communes différentes par ex) mais on garde s'ils ont plusieurs listes
municipales_datagouv <- municipales_datagouv %>% group_by(chef_executif) %>% distinct(, .keep_all=TRUE) %>% ungroup()
    # on retire ceux qui se présentent sans liste (NA)
municipales_datagouv <- na.omit(municipales_datagouv, cols="Nuance Liste")


#------------------------- Manips base communes

# On renomme les colonnes
commune_pol2 <- commune_pol2 %>% rename (chef_executif = `chef de l'exécutif`, parti_politique = `parti politique`)

# On enlève aussi les accents
commune_pol2 <- data.table::data.table(commune_pol2)
commune_pol2[, chef_executif := stringi::stri_trans_general (str = chef_executif, id = "Latin-ASCII")]


#------------------------- Match

# On ne garde que les chefs de l'éxecutif pour lesquels il manque la parti
manque_chef <- commune_pol2 %>% filter(is.na(parti_politique), .keep_all=TRUE)
    # Puis on match
manque_chef <- left_join(manque_chef[,-4], municipales_datagouv, by="chef_executif", copy=FALSE) #sans "parti_politique" qui est vide

# On réordonne les colonnes
manque_chef <- manque_chef[,c(1:3,17,4:16)]

# On renomme
manque_chef <- manque_chef %>% rename (parti_politique = `Nuance Liste`)


# On re-assemble la base (chefs pour lesquels on avait déjà le parti politique + ceux desquels on vient de récupérer la liste sur datagouv)
    # dans notre base initiale on ne garde que les chefs qui ont l'info du parti politique
commune_pol2 <- na.omit(commune_pol2, cols="parti_politique")
    # on assemble avec chefs datagouv desquels on a la liste
commune_pol2 <- rbind(commune_pol2, manque_chef)


# Compte nombre d'infos manquantes après ces manipulations
commune_pol2 %>% count(is.na(parti_politique)) #103

# Compte nombre de partis différents
table(commune_pol2$parti_politique)

# Export base complétée
#rio::export(commune_pol2,"./Data/interim/Step3_recherche_manuelle/commune_pol_ajout_listesPo_datagouv.xlsx")




# ------------------------------- HARMONISATION DES PARTIS POLITIQUES



    ### Pour chaque chef de l'exec qui sont rattachés à plusieurs partis politiques, ceux-ci sont mis en plusieurs lignes et donc avec des NA pour les autres colonnes. On comble ces vides en affiliant le chef de l'exec concerné et toutes les infos qui vont avec. 
            # - d'abord on enlève les observations qui contiennent des NA qui ne sont pas sur la colonne des chefs de l'executif
            # - ensuite on affecte aux NA la valeur de l'obs précédente
            # - on remet les jeux ensemble

    ### on met le numéro SIREN au format numérique car caractère actuellement

    ### et on harmonise les noms des partis politiques (DVD / Divers droite / Divers Droite etC.)


# COMMUNES
commune_pol2 <- commune_pol2 %>% mutate_at(vars(siren, nom, chef_executif, parti_politique, type, regcode), zoo::na.locf) # on remplace les NA seulement pour les 6 premières colonnes qui n'ont pas de valeur manquante excepté pour les chefs de l'exec qui ont plusieurs partis po. Alors que pour les autres il peut manquer des infos à la commune donc il ne faudrait pas que celle d'une autre orga lui soit affectée.
commune_pol2 <- commune_pol2  %>% group_by(nom) %>% mutate_all(funs(zoo::na.locf(., na.rm = FALSE))) #ensuite on peut appliquer na.locf par groupe pour pallier à ce pb.
commune_pol2$siren <- as.numeric(commune_pol2$siren)


# REGIONS
    # import
region_pol <- read_excel("C:/Users/diane/Desktop/Analyse Diane/Data/interim/Step3_recherche_manuelle/region_pol.xlsx")
    # remplace NA des variables où NA lié seulement à parti po nombreux
region_pol <- region_pol %>% mutate_at(vars(siren, nom, `chef de l'exécutif`, `parti politique`, type, regcode), zoo::na.locf)
region_pol <- region_pol  %>% group_by(nom) %>% mutate_all(funs(zoo::na.locf(., na.rm = FALSE))) 
    # on met au bon format le num SIREN
region_pol$siren <- as.numeric(region_pol$siren)


# METROPOLES
    # import
library(readxl)
metropole_pol <- read_excel("C:/Users/diane/Desktop/Analyse Diane/Data/interim/Step3_recherche_manuelle/metropole_pol.xlsx")
    # remplace NA des variables où NA lié seulement à parti po nombreux
metropole_pol <- metropole_pol %>% mutate_at(vars(siren, nom, `chef de l'exécutif`, `parti politique`, type, regcode), zoo::na.locf)
metropole_pol <- metropole_pol  %>% group_by(nom) %>% mutate_all(funs(zoo::na.locf(., na.rm = FALSE))) 
    # on met au bon format le num SIREN
metropole_pol$siren <- as.numeric(metropole_pol$siren)


# DEPARTEMENTS
    # import
departement_pol2 <- read_excel("C:/Users/diane/Desktop/Analyse Diane/Data/interim/Step3_recherche_manuelle/departement_pol2.xlsx")
    # remplace NA des variables où NA lié seulement à parti po nombreux
departement_pol2 <- departement_pol2 %>% mutate_at(vars(siren, nom, `chef de l'exécutif`, `parti politique`, type, regcode), zoo::na.locf)
departement_pol2 <- departement_pol2  %>% group_by(nom) %>% mutate_all(funs(zoo::na.locf(., na.rm = FALSE))) 
    # on met au bon format le num SIREN
departement_pol2$siren <- as.numeric(departement_pol2$siren)


# CU : pas utile car pas de passage sur wikidata et pas pluralité de partis po trouvés à la main
    # en revanche on importe quand même pour l'exporter en xlsx et mettre au bon format les variables
communaute_urbaine <- read_excel("C:/Users/diane/Desktop/Analyse Diane/Data/interim/Step3_recherche_manuelle/communaute_urbaine.xlsx")
    # on met au bon format le num SIREN
communaute_urbaine$siren <- as.numeric(communaute_urbaine$siren)



# ------------------------------- 


# Nombre de partis différents et liste
dep_po <- as.data.frame(table(departement_pol2[,4])) %>% arrange(desc(Freq))
reg_po <- as.data.frame(table(region_pol[,4])) %>% arrange(desc(Freq))
com_po <- as.data.frame(table(commune_pol2[,4])) %>% arrange(desc(Freq))
met_po <- as.data.frame(table(metropole_pol[,4])) %>% arrange(desc(Freq))
cu_po <- as.data.frame(table(communaute_urbaine[,4])) %>% arrange(desc(Freq))
tally(dep_po) + tally(reg_po) + tally(com_po) + tally(met_po) + tally(cu_po)  # 91

# Pour un même parti on trouve plusieurs appellations (ex: UMP = Union pour un mouvement populaire, LDVG = Divers Gauche etc.). 
    # Donc on remplace les noms pour harmoniser ce champ.

# On rassemble temporairement les df
commune_pol2 <- commune_pol2 %>% rename(`parti politique` = parti_politique, `chef de l'exécutif` = chef_executif)
    # on met les mêmes format aux variable spour pouvoir assembler les observations
departement_pol2[,c(1,8,10,13:15,17)] <- lapply(departement_pol2[,c(1,8,10,13:15,17)], as.numeric) 
region_pol[,c(1,8,10,13:15,17)] <- lapply(region_pol[,c(1,8,10,13:15,17)], as.numeric)            # runer 2x si fonctionne pas
commune_pol2[,c(1,8,10,13:15,17)] <- lapply(commune_pol2[,c(1,8,10,13:15,17)], as.numeric) 
metropole_pol[,c(1,8,10,13:15,17)] <- lapply(metropole_pol[,c(1,8,10,13:15,17)], as.numeric) 
communaute_urbaine[,c(1,8,10,13:15,17)] <- lapply(communaute_urbaine[,c(1,8,10,13:15,17)], as.numeric)
    # rbind
toutes_orgas <- rbind(region_pol, commune_pol2, departement_pol2, metropole_pol, communaute_urbaine)
    
# On remplace les partis en 2 temps pour éviter que "Europe Écologie Les Verts" devienne "Europe Écologie Europe Écologie Les Verts" par ex
toutes_orgas$`parti politique` <- str_replace_all(toutes_orgas$`parti politique`, 
                                                     c("Europe Écologie Les Verts" = "passage_intermediaire",
                                                       "LSOC" = "Parti socialiste"))

toutes_orgas$`parti politique` <- str_replace_all(toutes_orgas$`parti politique`, 
                                                     c("UMP" = "Union pour un mouvement populaire", 
                                                       "Parti républicain" = "Les Républicains",
                                                       "LDVG" = "Divers Gauche",
                                                       "LDVD" = "Divers Droite",
                                                       "divers droite" = "Divers Droite",
                                                       "Les Republicains" = "Les Républicains",
                                                       "LLR" = "Les Républicains",
                                                       "sans étiquette" = "Sans étiquette",
                                                       "LDVC" = "Divers Centre",
                                                       "LDIV" = "Sans étiquette",
                                                       "SOC" = "Parti socialiste",
                                                       "Les Verts" = "Europe Écologie Les Verts",
                                                       "LVEC" = "Europe Écologie Les Verts",
                                                       "LREM" = "La République en marche",
                                                       "La République En Marche" = "La République en marche",
                                                       "LUDI" = "Union des démocrates et indépendants"))

toutes_orgas$`parti politique` <- str_replace_all(toutes_orgas$`parti politique`, 
                                                     c("passage_intermediaire" = "Europe Écologie Les Verts"))

orga_po <- as.data.frame(table(toutes_orgas[,4])) %>% arrange(desc(Freq))  # 49 soit 42 de moins


# On divise les jeux selon le type d'orga
commune_pol2 <- toutes_orgas %>% filter(type == "COM")
communaute_urbaine <- toutes_orgas %>% filter(type == "CU")
departement_pol2 <- toutes_orgas %>% filter(type == "DEP") 
metropole_pol <- toutes_orgas %>% filter(type == "MET") 
region_pol <- toutes_orgas %>% filter(type == "REG")


# On exporte les bases harmonisées du parti politique
rio::export(commune_pol2,"./Data/interim/Harmonisation_politique/commune.xlsx")
rio::export(region_pol,"./Data/interim/Harmonisation_politique/region.xlsx")
rio::export(metropole_pol,"./Data/interim/Harmonisation_politique/metropole.xlsx")
rio::export(departement_pol2,"./Data/interim/Harmonisation_politique/departement.xlsx")
rio::export(communaute_urbaine,"./Data/interim/Harmonisation_politique/communaute_urbaine.xlsx")








##########################################################
##### ESSAI REQUETES WIKIDATA
##########################################################




install.packages("WikidataR","WikidataQueryServiceR")
library(devtools)
devtools::install_github("TS404/WikidataR")
library(WikidataR)
library(WikidataQueryServiceR)
install.packages("ellipsis", version="0.3.2")
library(tidyverse)


# aide sur le service requetes
?WDQS


            # Requetes pour obtenir chef de l'executif et son/ses parti(s) politique(s)


# Régions
reg_wiki <- query_wikidata('SELECT DISTINCT ?regionLabel ?siren ?COG ?chef_execLabel ?parti_politiqueLabel
WHERE {
  ?region wdt:P31 wd:Q36784 .
  ?region wdt:P1616 ?siren .
  ?region wdt:P2585 ?COG .
  ?region wdt:P6 ?chef_exec. 
  OPTIONAL { ?chef_exec wdt:P102 ?parti_politique. } 
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],fr,en". }
}
ORDER BY ?regionLabel')


# Départements
dep_wiki <- query_wikidata('SELECT DISTINCT ?departementLabel ?siren ?chef_execLabel ?parti_politiqueLabel
WHERE {
  ?departement wdt:P31 wd:Q36784 .
  OPTIONAL { ?departement wdt:P6 ?chef_exec. }
  OPTIONAL { ?departement wdt:P1616 ?siren }
  OPTIONAL { ?departement wdt:P102 ?parti_politique. }
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],fr,en". }
}')




# Ajout infos wikidata
write_wikidata(items      = c("Q3113706","Q1120190"),
               properties = "P6",
               values     = c("Q65586080","Q544725"),
               qual.properties = "qal580", #ne fonctionne pas
               qual.values = "+2020-05-18T00:00:00Z/11",
               format     = "api",
               api.username = ,  # Enter your Wikimedia username here
               api.token  =  #REDACTED# Find yours from https://tools.wmflabs.org/quickstatements/#/user
               )









#################################### POUBELLE


## package pour récupérer données wikidata
library(tidywikidatar)  #https://medium.com/european-data-journalism-network/a-new-r-package-for-exploring-the-wealth-of-information-stored-by-wikidata-fe85e82b6440 
tw_set_cache_folder(path = fs::path(fs::path_home_r(),
                                    "R",
                                    "tw_data"))
# chef executif pas dispo pour CC et CA donc on regroupe CA+CC et CU+metropoles séparemment




















###############################################################################################################################

############################################ BROUILLON STATISTIQUES  ########################################################

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
  # on met le tout dans une même base
base <- rbind(reg2,dep2,com2,epci2)



                ### A) Régions


    ## 1. Variables une à une


# VARIABLES QUANTI


# Quand plusieurs partis po pour 1 orga alors plusieurs obs. Pour ne pas fausser l'analyse exploratoire sur les autres variables (quanti) on retire la colonne du parti_po et on supprime les doublons : 1 orga = 1obs
base_unique <- base %>% select(-partis_po_chef) %>% unique()


#----------- Points atypiques


ggplot(regions_unique, aes(x=nb_publi, y=frequency(nb_publi))) +
  geom_violin(trim=FALSE) +
  geom_boxplot(width=0.1)

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















# Pour les régions il ne manque que 3 infos sur les chefs de l'exécutif donc on complète à la main pour avoir une base finie
regions[c(2,3),]$CSP_chef <- c("Professions Intermédiaires","Cadres et professions intellectuelles supérieures") #Alfred Marie-Jeanne pr Martinique de 2015 à 2021
regions[c(2,3),]$age_chef <- age(c("1936/11/15","1953/09/26"), units = "years") #Rodolphe Alexandre pr Guyane de 2015 à 2021
regions[c(2,3),]$partis_po_chef <- c("Mouvement indépendantiste martiniquais","Divers Gauche")

