library(httr)
library(jsonlite)
library(dplyr)
library(stringdist)
source("functions.R")


#example
species2test <- c("Nothofagus alpina", "Eucryphia glutinoosa", "Caldcluvia paniculeta")

test <- get_species_data(species2test, known_species)

test
