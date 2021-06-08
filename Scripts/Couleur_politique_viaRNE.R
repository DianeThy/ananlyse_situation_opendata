#### NOUVEAU SCRIPT POUR OBTENIR COULEUR POLITIQUE EN PASSANT PAR WIKIDATA SEULEMENT POUR PARTIS PO
#### ON OBTIENT LES CHEFS DE L'EXEC DIRECTEMENT VIA LE RNE



#------------------------------- ON PART D'UN FICHIER AVEC TOUTES LES ORGAS DE FRANCE


# Fichier initial : toutes les orgas de France (collectivités + EPCI)
library(tidyverse)
regions <- read_csv("Data/external/infos_regions.csv")
departements <- read_csv("Data/external/infos_departements.csv")
communes <- read_csv("Data/external/infos_communes.csv")
epci <- read_csv("Data/external/infos_interco.csv")



# On ajoute les données de l'observatoire des territoires (nombre de jeux ouverts)

# Import de la base complète
library(readxl)
observatoire_opendata_territoire <- read_excel("C:/Users/diane/Desktop/Analyse Diane/Data/raw/Indicateurs ODATER v2 (up-to-date).xlsx")
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
doublons <- as.data.frame(table(epci$SIREN))
epci <- epci %>% distinct(SIREN, .keep_all=TRUE)
    # match
epci <- left_join(epci, observatoire_epci[,-c(2:3)], by="SIREN", copy=FALSE)




#------------------------------- AJOUT CHEFS EXECUTIF RNE



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

# Régions
wiki_reg <- query_wikidata('SELECT DISTINCT ?regionLabel ?siren ?COG ?chef_execLabel ?parti_politiqueLabel
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
wiki_dep <- query_wikidata('SELECT DISTINCT ?depLabel ?siren ?chef_execLabel ?parti_politiqueLabel
WHERE {
  ?dep wdt:P31 wd:Q6465 .
  ?dep wdt:P1616 ?siren .
  ?dep wdt:P6 ?chef_exec. 
  OPTIONAL { ?chef_exec wdt:P102 ?parti_politique. } 
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],fr,en". }
}
ORDER BY ?depLabel')


# Communes 
wiki_com <- query_wikidata('SELECT DISTINCT ?comLabel ?siren ?chef_execLabel ?parti_politiqueLabel
WHERE {
  ?com wdt:P31 wd:Q484170 .
  ?com wdt:P1616 ?siren .
  ?com wdt:P6 ?chef_exec. 
  OPTIONAL { ?chef_exec wdt:P102 ?parti_politique. } 
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],fr,en". }
}
ORDER BY ?comLabel')


# EPCI
wiki_epci <- query_wikidata('SELECT DISTINCT ?epciLabel ?siren ?chef_execLabel ?parti_politiqueLabel
WHERE {
  ?epci wdt:P31 wd:Q18706073 .
  ?epci wdt:P1616 ?siren .
  ?epci wdt:P6 ?chef_exec. 
  OPTIONAL { ?chef_exec wdt:P102 ?parti_politique. } 
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],fr,en". }
}
ORDER BY ?epciLabel')



#------------------------------- AJOUT STATISTIQUES LOCALES INSEE



# avec un url pour toutes les communes
library(readxl)
stats_locales_com <- read_excel("https://statistiques-locales.insee.fr/87baca9b-a98c-443d-a808-641aa994d618")


rio::export(regions, "./Data/regions.xlsx")








