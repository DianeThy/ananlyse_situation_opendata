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
  OPTIONAL { ?region wdt:P1616 ?siren }
  OPTIONAL { ?region wdt:P2585 ?COG } 
  OPTIONAL { ?region wdt:P6 ?chef_exec. }
  OPTIONAL { ?region wdt:P102 ?parti_politique. } 
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




query_wikidata('SELECT DISTINCT ?regionLabel ?siren ?COG ?chef_execLabel ?parti_politiqueLabel
WHERE {
  ?region wdt:P31 wd:Q36784 .
  ?person wdt:P31 wd: Q5
  OPTIONAL { ?region wdt:P1616 ?siren }
  OPTIONAL { ?region wdt:P2585 ?COG } 
  OPTIONAL { ?region wdt:P6 ?chef_exec. }
  OPTIONAL { ?chef_exec wdt:P102 ?parti_politique. } 
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],fr,en". }
}
ORDER BY ?regionLabel')





