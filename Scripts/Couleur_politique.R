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
#rio::export(commune, "commune.xlsx")
#rio::export(communaute_urbaine, "communaute_urbaine.xlsx")
#rio::export(departement, "departement.xlsx")
#rio::export(metropole, "metropole.xlsx")
#rio::export(region, "region.xlsx")
#rio::export(CA, "CA.xlsx")
#rio::export(CC, "CC.xlsx")


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

# On enlève les doublons (maires qui se présentent dans 2 communes différentes par ex)
#municipales_datagouv <- municipales_datagouv %>% group_by(chef_executif) %>% distinct(chef_executif, .keep_all=TRUE) %>% ungroup()
    # plutot on retire ceux qui se présentent sans liste (NA) mais on garde ceux qui se présentent avec des listes différentes pour cluster
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
    # on trie le nom de la commune par ordre alphabétique croissant
#commune_pol2 <- commune_pol2 %>% arrange(nom)  # en fait non parce que qd plusieurs partis pour 1 chef alors les infos ne seront plus au bon endroit


# Compte nombre d'infos manquantes après ces manipulations
commune_pol2 %>% count(is.na(parti_politique)) #103

# Compte nombre de partis différents
table(commune_pol2$parti_politique)

# Export base complétée
#rio::export(commune_pol2,"commune_pol_ajout_listesPo_datagouv.xlsx")






    ### Pour chaque chef de l'exec qui sont rattachés à plusieurs partis politiques, ceux-ci sont mis en plusieurs lignes et donc avec des NA pour les autres colonnes. On comble ces vides en affiliant le chef de l'exec concerné et toutes les infos qui vont avec. 
            # - d'abord on enlève les observations qui contiennent des NA qui ne sont pas sur la colonne des chefs de l'executif
            # - ensuite on affecte aux NA la valeur de l'obs précédente
            # - on remet les jeux ensemble

    # + on met le numéro SIREN au format numérique car caractère actuellement


# COMMUNES
commune_pol2 <- commune_pol2 %>% mutate_at(vars(siren, nom, chef_executif, parti_politique, type, regcode), zoo::na.locf) # on remplace les NA seulement pour les 6 premières colonnes qui n'ont pas de valeur manquante excepté pour les chefs de l'exec qui ont plusieurs partis po. Alors que pour les autres il peut manquer des infos à la commune donc il ne faudrait pas que celle d'une autre orga lui soit affectée.
commune_pol2 <- commune_pol2  %>% group_by(nom) %>% mutate_all(funs(zoo::na.locf(., na.rm = FALSE))) #ensuite on peut appliquer na.locf par groupe pour pallier à ce pb.
commune_pol2$siren <- as.numeric(commune_pol2$siren)


# REGIONS
    # import
region_pol <- read_csv("C:/Users/diane/Desktop/Analyse Diane/Data/interim/Step3_recherche_manuelle/region_pol.csv")
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
departement_pol2 <- read_csv("C:/Users/diane/Desktop/Analyse Diane/Data/interim/Step3_recherche_manuelle/departement_pol2.csv")
    # remplace NA des variables où NA lié seulement à parti po nombreux
departement_pol2 <- departement_pol2 %>% mutate_at(vars(siren, nom, `chef de l'exécutif`, `parti politique`, type, regcode), zoo::na.locf)
departement_pol2 <- departement_pol2  %>% group_by(nom) %>% mutate_all(funs(zoo::na.locf(., na.rm = FALSE))) 
    # on met au bon format le num SIREN
departement_pol2$siren <- as.numeric(departement_pol2$siren)


# CU : pas utile car pas de passage sur wikidata et pas pluralité de partis po trouvés à la main
    # en revanche on importe quand même pour l'exporter en xlsx et mettre au bon format les variables
communaute_urbaine <- read_csv("C:/Users/diane/Desktop/Analyse Diane/Data/interim/Step3_recherche_manuelle/communaute_urbaine.csv")
    # on met au bon format le num SIREN
communaute_urbaine$siren <- as.numeric(communaute_urbaine$siren)



# On remplace les bases exportées dans les dossiers
#rio::export(commune_pol2,"commune_pol_ajout_listesPo_datagouv.xlsx")
#rio::export(region_pol,"region_pol.xlsx")
#rio::export(metropole_pol,"metropole_pol.xlsx")
#rio::export(departement_pol2,"departement_pol2.xlsx")
#rio::export(communaute_urbaine,"communaute_urbaine.xlsx")



# ------------------------------- HARMONISATION DES PARTIS POLITIQUES



library(tidywikidatar)  #https://medium.com/european-data-journalism-network/a-new-r-package-for-exploring-the-wealth-of-information-stored-by-wikidata-fe85e82b6440 
tw_set_cache_folder(path = fs::path(fs::path_home_r(),
                                    "R",
                                    "tw_data"))
# chef executif pas dispo pour CC et CA donc on regroupe CA+CC et CU+metropoles séparemment



# ------------------------------- HARMONISATION DES PARTIS POLITIQUES


# Import des bases avec couleur politique
commune_pol2 <- read_excel("Data/interim/Step3_recherche_manuelle/commune_pol_ajout_listesPo_datagouv.xlsx")
departement_pol2 <- read_excel("Data/interim/Step3_recherche_manuelle/departement_pol2.xlsx")
metropole_pol <- read_excel("Data/interim/Step3_recherche_manuelle/metropole_pol.xlsx")
region_pol <- read_excel("Data/interim/Step3_recherche_manuelle/region_pol.xlsx")
communaute_urbaine <- read_excel("Data/interim/Step3_recherche_manuelle/communaute_urbaine.xlsx")

# Nombre de partis différents et liste
dep_po <- as.data.frame(table(departement_pol2[,4])) %>% arrange(desc(Freq))
reg_po <- as.data.frame(table(region_pol[,4])) %>% arrange(desc(Freq))
com_po <- as.data.frame(table(commune_pol2[,4])) %>% arrange(desc(Freq))
met_po <- as.data.frame(table(metropole_pol[,4])) %>% arrange(desc(Freq))
cu_po <- as.data.frame(table(communaute_urbaine[,4])) %>% arrange(desc(Freq))
nb_partis_renseignes <- tally(dep_po) + tally(reg_po) + tally(com_po) + tally(met_po) + tally(cu_po)  # 91

# Pour un même parti on trouve plusieurs appellations (ex: UMP = Union pour un mouvement populaire, LDVG = Divers Gauche etc.). Donc on remplace les noms pour harmoniser ce champ.
    # on rassemble temporairement les df
commune_pol2 <- commune_pol2 %>% rename(`parti politique` = parti_politique, `chef de l'exécutif` = chef_executif)
toutes_orgas <- rbind(region_pol, commune_pol2, departement_pol2, metropole_pol, communaute_urbaine)
    # on remplace les partis
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
orga_po <- as.data.frame(table(toutes_orgas[,4])) %>% arrange(desc(Freq))  # 49 soit 42 de moins


# On divise les jeux selon le type d'orga
commune_pol2 <- toutes_orgas %>% filter(type == "COM")
communaute_urbaine <- toutes_orgas %>% filter(type == "CU")
departement_pol2 <- toutes_orgas %>% filter(type == "DEP") 
metropole_pol <- toutes_orgas %>% filter(type == "MET") 
region_pol <- toutes_orgas %>% filter(type == "REG")


# On réexporte
rio::export(commune_pol2,"commune_pol_ajout_listesPo_datagouv.xlsx")
rio::export(region_pol,"region_pol.xlsx")
rio::export(metropole_pol,"metropole_pol.xlsx")
rio::export(departement_pol2,"departement_pol2.xlsx")
rio::export(communaute_urbaine,"communaute_urbaine.xlsx")







