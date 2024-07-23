.onLoad <- function(libname, pkgname) {
  criterio <- fromJSON("https://api.herbariodigital.cl/menu/?format=json&lang=en&paginated=false")
  fulldb_nombres <- fromJSON("https://api.herbariodigital.cl/finder/%20/?format=json")
  
  message("los datos se han cargado correctamente \n", pkgname, " Inizializado. \n")
  pkgnm<-pkgname
  
  tabla_maestra <- criterio[["region"]]
  tabla_maestra$keywords <- c('AYP','TAR','ANT','ATA','COQ','VAL','RME','LBO',
                              'MAU','NUB','BIO','ARA','LRI','LLA','AIS','MAG','IPA','JFE','IDE')
  
  conservation <- criterio[["conservation_state"]]
  conservation$type <- "conservation_state"
  
  criterios <- criterio[!names(criterio) %in% c("region", "conservation_state")]
  tx_db <- dplyr::bind_rows(criterios, .id = "type") %>%
    dplyr::mutate(type = gsub("\\..*", "", type))
  
  rm(criterios)
  
  db_nombres <- fulldb_nombres[fulldb_nombres$type != "common_name",]
  
  full_tx_db <- dplyr::bind_rows(tx_db, db_nombres)
  
  # Assign variables to the package environment
  assign("criterio", criterio, envir = asNamespace(pkgname))
  assign("fulldb_nombres", fulldb_nombres, envir = asNamespace(pkgname))
  assign("tabla_maestra", tabla_maestra, envir = asNamespace(pkgname))
  assign("conservation", conservation, envir = asNamespace(pkgname))
  assign("tx_db", tx_db, envir = asNamespace(pkgname))
  assign("db_nombres", db_nombres, envir = asNamespace(pkgname))
  assign("full_tx_db", full_tx_db, envir = asNamespace(pkgname))
}

