        # Construction de la base

## import infos coll
## jointure données OODT
## jointure RNE
## jointure wikidata
## jointure stats locales INSEE
## jointure nb_etudiants
## jointure comptes OFGL
## jointure ruralité (INSEE)
## manipulations des bases

        # Analyse exploratoire

## 1. Regions
### Variables une à une
    ### quanti : outliers (boxplots, test), distrib (histo, test), corrélations (matrice, ACP, arbre), stats desc (summary)
    ### quali : 
### Relations entre les variables explicatives

### Relations des variables explicatives avec Y


df <- data.frame(time = 1:10,
                 a = cumsum(rnorm(10)),
                 b = cumsum(rnorm(10)),
                 c = cumsum(rnorm(10)))
df <- data.table::melt(df ,  id.vars = 'time', variable.name = 'series')
