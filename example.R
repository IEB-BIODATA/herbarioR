library(httr)
library(jsonlite)
library(dplyr)
library(stringdist)
source("functions.R")


#example
#create a list of species to test
species2test <- c("Tupa mucronata", "Crassula paludosa", "Dysphania multifida", "Mimulus longipes", "Plantago cantagallensis")


#Aplying the function#just replace the object species2test
test <- get_species_data(species2test, known_species)

test


