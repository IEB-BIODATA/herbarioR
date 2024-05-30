library(httr)
library(jsonlite)
library(dplyr)
library(stringdist)
source("functions.R")


#Examples
#Example 1
#create a list of species to test
species2test <- c("Plantachina quenoexiste","Tupa mucronata", "Crassula paludosa", "Dysphania multifida", "Mimulus longipes","Lathyrus ovalifolius var. mucronatus")


#Aplying the function
#just replace the object species2test
(test <- get_species_data(species2test, known_species))


#Example 2 cactus
cactus <- read.csv("cactus.chile.csv", header = TRUE, stringsAsFactors = FALSE)


(test2 <- get_species_data(cactus$Specie.name, known_species))

dim(test2)
dim(cactus)
no_catalog <- cactus[!cactus$Specie.name %in% known_species,]
