### UTILISATION WIKIDATAR PACKAGE ###


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




