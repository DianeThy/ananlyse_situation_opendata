### Statistiques descriptives ###


library(tidyverse)

# Import des bases finalisées
region <- read_csv("Data/process/region.csv")
departement <- read_csv("Data/process/departement.csv")
commune <- read_csv("Data/process/commune.csv")
CU_metropole <- read_csv("Data/process/CU_metropole.csv")
CA_CC <- read_csv("Data/process/CA_CC.csv")

# Format des variables
str(region)

