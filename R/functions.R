
###prueba de cambios

#' @import dplyr
#' @import stringdist
#' @import jsonlite
#' @import glue

source("R/zzz.r")

pkgnm<-"herbarioR"
#' @title get_closest_match
#'
#' @description
#' Herramienta de fuzzy match, comparando un input, con una base de datos local
#' @param species_name cadena de caracteres (Nombre de genero o nombre científico) la cual quiero buscar en la base de datos.
#' @param database La base de datos en la cual quiero ejercer la busqueda, si no está determinada por el usuario, selecciona una interna del paquete.
#' @param max_dist Distancia Máxima la cual puede haber entre un elemento de la base de datos y el input para que apareza en el dataframe final.
#' @details
#' Esta funcion no diferencia entre mayusculas y minusculas, por lo que, es lo mismo "nothofagus", "NOTHOFAGUS" y "nOtHoFaGuS"
#' Esta funcion es una aplicación de la afind del paquete stringdist. Dentro de la misma, existe una normalización de las distancias para así asegurar un match con hasta 5 letras erroneas. Automaticamente elimina nombres no existentes
#'
#' @returns DataFrame. Las filas de la base de datos que concuerdan suficientemente con el input
#' @examples
#' get_closest_match("nothofagus")
#' Map(get_closest_match(), c("notomircia", "notofagus"))
#' @export
#'
get_closest_match <- function(species_name, database = NULL, max_dist = 0.15) {
  if (is.null(database)) {
    known_species_df <- get("db_nombres", envir = asNamespace(pkgnm))
    } else {
    known_species_df <- database}
  species_name <- tolower(as.character(species_name))
  known_species <- tolower(as.character(known_species_df$name))
  distances <- afind(known_species, species_name, method = "jw", weight = c(a = 1, b = 1, t = 1))$distance
  if (min(distances) <= max_dist) {
    distances <- log(distances + 1)
    distances <- (distances - min(distances)) / (max(distances) - min(distances))
    matched_df<- known_species_df[which(distances == (min(distances))), ]
    matched_df <- cbind.data.frame(species_name, matched_df)
    rownames(matched_df) <- NULL
    colnames(matched_df) <- c("input_name", colnames(known_species_df))
    return(matched_df)
  } else {
    return(NA)
  }
}




#' @title get_taxonomy
#'
#' @description
#' Nos permite recuperar la taxonomía para un nombre, ya sea valido o sinonimo.
#' @param species_name nombre o nombres a buscar
#' @returns Dataframe. Describiendo la taxonomia a detalle
#' @examples
#' get_taxonomy(c("tupa mucronata", "myrceugenia planipes"))
#'
#' @export

get_taxonomy <- function(species_name, s=F) {
  tax<-c("genus", "family","order","classname","division")
  para<-paste0(tax, ".name")
  param <- c("unique_taxon_id", "scientific_name", tax)
  p<-paste0(param,".id")
  api_url<-"https://api.herbariodigital.cl/"
  workname <- na_omit_list(lapply(species_name, get_closest_match))
  workname<-bind_rows(workname)
  if(s==T){
    workname<-workname[workname$type=="synonym"|workname$type == "species" & workname$determined, ]
  }else{
    workname<-workname[workname$type == "species" & workname$determined, ] }
  workname<-workname[workname$type == "synonymy" | (workname$type == "species" & workname$determined), ]
  workname<-split(workname, seq(nrow(workname)))
  df <- lapply(workname, function(x) {
    url <- paste0(api_url,x$type,"/",x$id, "/?format=json")
    a <- fetch_data(url, species = x$name, flat = T)
    if(x$type=="species"){
      a <-as.data.frame(a[param])
      c<-a[,para]
      colnames(c)<-tax
      rownames(c)<-NULL
      rownames(x)<-NULL
      x <- x[rep(seq_len(nrow(x)), each = nrow(a)), ]
      a<-cbind.data.frame(x,c)
      return(a)
    }else if(x$type=="synonymy"){
      a<-a$species
      a<-a[param]
      a<-as.data.frame.list(a)
      a<-a[para]
      colnames(a)<-tax
      rownames(a)<-NULL
      rownames(x)<-NULL
      a<-cbind.data.frame(x,a)
      return(a)
    }else{return(NA)}} )
  df<-na_omit_list(df)
  df <- bind_rows(df)
  df[,"source"] <- NULL
  rownames(df)<-NULL
  names(df)[names(df)=="classname"]<-"class"
  df["determined"]<-NULL
  return(df)
}
#' @title get_valid_name
#'
#' @description
#' Nos permite obtener el nombre actualizado para uno o varios sinonimos.
#' @param name Nombre o nombres a buscar.
#' @param use Formato de input para la funcion. puede ser "user" o "nested". Automaticamente es "user"
#' @details
#' En caso de usar nested, el input es el output de la funcion "get_closest_match". La opción "nested" existe para el uso interno de otras funciones dentro de este mismo paquete y no se recomienda al usuario final utilizar esta opcion.
#'
#'
#' @returns Dataframe. Conteniendo el Input_name y el nombre actualizado
#' @examples
#' get_valid_name("myrceugenia")
#' get_valid_name(c("acacia caven", "tupa mucronata"))
#' @export

get_valid_name <- function(name, use = "user") {
  if(use=="user" & is.null(ncol(name))){
    workname <- na_omit_list(lapply(name, get_closest_match))
    workname<-bind_rows(workname)
    workname<-split(workname, seq(nrow(workname)))
  }else if(use=="nested" & ncol(name)==4){
    workname<-bind_rows(name)
    workname<-split(workname, seq(nrow(workname)))
  }else{
    warning("Error: Unvalid data format. Look at docs.")
  }
  colnam <- c('unique_taxon_id','scientific_name','genus','specific_epithet','scientific_name_authorship',
              'subspecies','ssp_authorship','variety','variety_authorship','form',
              'form_authorship','kingdom','division','classname','family','order',
              'habit','determined','common_names','status','minimum_height','maximum_height',
              'conservation_state','id_mma','region','herbarium_url')
  if (is.null(workname)) {
    stop("Error: Unable to retrieve data for the provided name.")
  }
  output<-lapply(workname, function(z){
    if (z$type == "synonymy") {
      url <- paste0("https://api.herbariodigital.cl/synonymy/",
                    z$id, "/?format=json")
      data_fetch <- fetch_data(url, species = z$name)
      match <- as.data.frame(z[c("id", "input_name")])
      colnames(match) <- c("id_synonym", "Input_Name")
      sp <- data_fetch$species
      if (use == "user") {
        sp <- as.data.frame(sp[c("unique_taxon_id", "name")])
        colnames(sp) <- c("valid_id", "valid_name")
      } else if (use == "nested") {
        sp <- sp[, intersect(colnam, colnames(unique_taxon_id$species))]
      }
      synonyms_df <- cbind.data.frame(match, sp)
      return(synonyms_df)
    } else {
      message(paste0(z$name, ": Input is already a valid name, not a synonym."))
      return(NA)
    }})
  return(bind_rows(na_omit_list(output)))
}


#' @title get_synonyms
#'
#' @description
#' se obtienenen los sinonimos en base a los nombres actualizados
#' @param name nombre o nombres a buscar
#' @returns Dataframe. La primera fila siempre son los nombres actualizados, (input) y hacia abajo, sus respectivos sinonimos
#' @examples
#' get_synonyms(c("Myrceugenia planipes", "Phycella angustifolia"))
#' get_synonyms("Austrocedrus chilensis")
#'
#' @export

get_synonyms <- function(name) {
  if(is.null(ncol(name))){
    workname <- na_omit_list(lapply(name, get_closest_match))
    workname<-bind_rows(workname)
    workname<-workname[workname$determined,]
    workname<-split(workname, seq(nrow(workname)))
    output_syn<-lapply(workname, function(syn){
      if (syn$type == "species") {
        url <- paste0("https://api.herbariodigital.cl/species/",
                      syn$id, "/?format=json")
        data <- fetch_data(url)
        if (!is.null(data)) {
          synonyms <- data$synonyms
          if (length(synonyms) > 0) {
            syn_names <- paste0("synonym_", seq_along(synonyms))
            synonyms_df <- data.frame(t(synonyms))
            colnames(synonyms_df) <- syn_names
          } else {
            synonyms_df <- data.frame(matrix("None synonyms found", ncol = 1, nrow = 1))
            colnames(synonyms_df)<-"synonym_1"
          }
          result <- data.frame(id = data$unique_taxon_id, name = data$name, synonyms_df)
          return(result)
        } else {
          return(NA)
        }
      } else {
        message(paste0(syn$name,": Input is a synonym, not a species."))
        return(NA)
      }  })
    return((bind_rows(na_omit_list(output_syn))))
  }
}



na_omit_list<-function(x){
  is_na<-function(y){
    !all(is.na(y))}
  output<-Filter(is_na, x)
  return(output)
}

fetch_data <- function(url, species=NULL, flat= F) {
  require(jsonlite)
  response <- tryCatch(
    expr = {
      jsondata<-fromJSON(url, flatten= flat)
      if (!is.null(species)){
        message(species, ": Data fetched correctly")
      }
      return(jsondata)
    },
    error = function(e) {
      warning("Error fetching data from API:", e$message)
      return(NULL)
    }
  )
  return(response)
}


#' @title get_distribution
#'
#' @description
#' Nos da la prescencia o auscencia de una o mas especies
#'
#' @returns Dataframe.El input, el nombre que mejor coincide y las regiones que está presente.
#' @details
#' Cada region se presenta como una columna individual, y su nombre está codificado según los keywords de 3 letras
#' habituales del Catalogo Taxonómico de la Plantas Vasculares de Chile, publicado en la revista Gayana Botánica (R. Rodriguez, C. Marticorena. et al. 2018)
#'
#' @param species nombre o nombres a buscar.
#' @examples
#' get_distribution("Calceolaria")
#' get_distribution(c("Baccharis linearis", "Mutisia rosea"))
#' @export

get_distribution <- function(species, sinonimos=NULL) {
  columns_to_exclude <- c("Input_Name", "id", "name")
  tabla_maestra<-get("tabla_maestra", envir = asNamespace(pkgnm))
  worklist<-bind_rows(na_omit_list(lapply(species, get_closest_match)))
  if(!is.null(sinonimos) && sinonimos==T){
    worklist<-worklist[worklist$type=="synonym"|(worklist$type == "species" && worklist$determined==T), ]
  }else if(is.null(sinonimos)) {
    worklist<-worklist[worklist$determined==T, ] }
  worklist<-split(worklist, seq(nrow(worklist)))
  df <- lapply(worklist, function(x) {
    url <- paste0("https://api.herbariodigital.cl/", x$type, "/", x$id, "/?format=json")
    result <- tryCatch(fetch_data(url, species = x$name), error = function(e) {
      warning(paste("Error fetching data for", x$name, ":", e$message))
      return(NULL)
    })
    if (x$type == "synonymy") {
      result<-result$species
      result_region<-use_ta_ma(result$region$id, reverse = T)
      result<-data.frame(id=result$unique_taxon_id,
                         name=result$name,
                         distribution=paste0(result_region, collapse = ","))
    } else if (x$type == "species") {
      result_region<-use_ta_ma(result$region$id, reverse = T)
      result<-data.frame(id=result$unique_taxon_id,
                         name=result$name,
                         distribution=paste0(result_region, collapse = ","))
    }
    result<-cbind.data.frame(x$input_name, result)
    result[tabla_maestra$keywords] <- Map(function(x) grepl(x, result$distribution), tabla_maestra$keywords)
    result$distribution<-NULL
    return(result)
  })
  df<-bind_rows(df)
  names(df)[names(df) == "x$input_name"] <- "Input_Name"
  df_1<-df[columns_to_exclude]
  df_2<-df[, !names(df) %in% columns_to_exclude]
  df_2<-df_2[, sapply(df_2, function(col) any(col))]
  df<-cbind.data.frame(df_1, df_2)
  return(df)
}



use_ta_ma<-function(input_kyw, reverse=F){
  if(reverse){
    input_kyw<-(input_kyw)
    region <- get("tabla_maestra", envir = asNamespace(pkgnm)) %>%
      filter(id %in% input_kyw) %>%
      pull(keywords)
    return(region)
  }else if(!reverse){
    input_kyw<-toupper(input_kyw)
    region <- get("tabla_maestra", envir = asNamespace(pkgnm)) %>%
      filter(keywords %in% input_kyw) %>%
      pull(id)
    return(region)
  }else{
    return(NA)
  }


}



generate_urls <- function(search=NULL, region=NULL) {
  base_url <- "https://api.herbariodigital.cl/species_list/?format=json&paginated=false"
  if (!is.null(search) && length(search) > 0) {
    matchs <- purrr::map_df(search, ~ get_closest_match(species_name = .x, database = get("tx_db", envir = asNamespace(pkgnm))))
    urls <- matchs %>%
      mutate(url = glue("{base_url}&{type}={unique_taxon_id}")) %>%
      mutate(identifier = glue("{type}_{name}")) %>%
      select(url, identifier) %>%
      as_tibble()
  } else {
    urls <- tibble(url = base_url, identifier = "base")
  }
  if (!is.null(region)) {
    region_ids <- use_ta_ma(region)
    urls <- urls %>%
      tidyr::crossing(region_id = region_ids) %>%
      mutate(url = glue("{url}&region={region_id}"),
             identifier = glue("{identifier}_region_{region_id}"))
  }

  return(list(urls$url, urls$identifier))
}


calculate_percentages <- function(df, by, cuenta = FALSE) {
  df <- df[df$type == "species", ]
  ids <- colnames(df)[grepl("id$", colnames(df))]
  spp <- colnames(df)[grepl("^(species|sample)", colnames(df))]
  df[ids] <- NULL
  df[spp] <- NULL
  counter <- table(df[[by]], df$status.name)
  total_counts <- rowSums(counter)
  percentages <- sweep(counter, 1, total_counts, FUN = "/") * 100
  unique_values <- unique(df[[by]])
  df_out <- data.frame(matrix(ncol = 4, nrow = length(unique_values)))
  colnames(df_out) <- c(by, "%nativo", "%endemico", "%introducido")
  df_out[[by]] <- unique_values
  for (tipo in c("Nativa", "Endémica", "Introducida")) {
    col_name <- switch(tipo,
                       "Nativa" = "%nativo",
                       "Endémica" = "%endemico",
                       "Introducida" = "%introducido")
    if (tipo %in% colnames(percentages)) {
      df_out[[col_name]] <- ifelse(is.na(percentages[, tipo]), 0, percentages[, tipo])
    } else {
      df_out[[col_name]] <- 0
    }
  }
  if (cuenta) {
    df_out$taxa_count <- as.vector(total_counts)
  }
  return(df_out)
}


#' @title porcentajes_fl_cl
#'
#' @description
#' Calcula los porcentajes de taxones Nativos, Endemicos, o Introducidos por grupo.
#'
#' @returns List. Cada elemento de una lista trae un DataFrame el cual contiene toda la información para el grupo.
#' @param search string. Taxon o Taxones a buscar
#' @param region string. Regiones en las cuales centrar la búsqueda.
#' @param by string. Criterio en el cual puedo hacer la separación. Default Familia
#' @param cuenta Logical. Agrega una columna al DataFrame final. Si es TRUE, me dice el numero de taxones infraespecifícos
#' @details
#' Esta funcion presenta parametros complejos, pero tanto el parametro by,
#' como el subset pueden ser las siguientes opciones:
#' ·division
#' ·classname
#' ·order
#' ·family
#' ·genus
#' ·plant_habit
#' ·env_habit
#' ·cycle
#' ·conservation_state
#' El parametro by y el subset NO PUEDEN SER EL MISMO CRITERIO.
#' @examples
#' porcentajes_fl_cl(search=c("asteraceae", "apiales"), by="genus", cuenta = T, subset="plant_habit")
#'
#' @export
#'
porcentajes_fl_cl <- function(search = NULL, region = NULL,
                              by = "family", subset = NULL, cuenta = FALSE) {
  if (!is.null(subset) && subset == by) {
    stop("Subset cannot be equal to 'by'.")}
  tx_db<-get("tx_db", envir = asNamespace(pkgnm))
  tax_db<- tx_db[tx_db$type %in% c('division', 'classname', 'order',
                                         'family', 'genus', 'plant_habit', 'env_habit',
                                         'cycle', 'conservation_state'), ]
  url_info <- generate_urls(search, region)
  Dataset_Catalogo <- Map(function(id, url) {
    data <- fetch_data(url=url, species = id, flat = T)
    return(data)
  }, url_info[[2]], url_info[[1]])
  results_list <- lapply(Dataset_Catalogo, function(df) {
    if (!is.null(subset)) {
      subset_values <- unique(tx_db[tx_db$type == subset, "name"])
      subset_results <- lapply(subset_values, function(val) {
        df_subset <- df[df[[subset]] == val, ]
        calculate_percentages(df_subset, by, cuenta = cuenta)
      })
    } else if(is.null(subset)) {
      subset_results <- calculate_percentages(df, by, cuenta = cuenta)
    }
    return(subset_results)
  })
  return(results_list)
}

#' @title get_voucher_data
#'
#' @description
#' permite obtener el numero total de Vouchers (muestras de herbario) digitalizados para una o mas especies.
#' @details
#' Se puede obtener el numero total de vouchers, con su respectivo codigo de herbario (ej. CONC:001, ULS:005)
#' @param input_name str. Nombre o nombres a buscar
#' @param links Logical. Si links igual a TRUE, se obtienen 2 columnas nuevas, las cuales son los hipervinculos a las imagenes escaladas a diferentes porcentajes.
#' @returns Dataframe. 2 columnas el nombre científico y el código de identificación de herbario.
#' @examples
#' get_voucher_data(input_name="Acrisione cymosa", links = T)
#' @export
#'
get_voucher_data<-function(input_name, links=F, s=F){
  url_voucher<-lapply(input_name, function(x){
    match<-get_closest_match(species_name = x)
    if(s==T){
      match<-match[match$type=="synonym"|match$type == "species" & match$determined, ]
    }else{
      match<-match[match$type == "species" & match$determined, ] }
    api_url<-"https://api.herbariodigital.cl/"
    url <- paste0(api_url,match$type,"/",match$id, "/?format=json&lang=en")
    output<-cbind.data.frame(url,match[,c(1,3,4)] )
    return(output)})
  url_voucher<-bind_rows(na_omit_list(url_voucher))
  url_voucher<-split(url_voucher, seq(nrow(url_voucher)))
  output<-lapply(url_voucher, function(x){
    data_voucher<-fetch_data(x$url, x$name)
    if(x$type=="synonymy"){
      data_voucher<-data_voucher$species
    }
    id<-data.frame(busqueda= x$input_name,database_id=data_voucher$unique_taxon_id, name=data_voucher$name)
    if(length(data_voucher$vouchers)==0){
      vouchers<-data.frame(VoucherID=NA,id=NA,
                           image_resized_10=NA,image_resized_60=NA)
    }else if(length(data_voucher$vouchers)>0){
      vouchers<-data_voucher[["vouchers"]]
      names(vouchers)[names(vouchers) == "code"] <- "VoucherID"
    }
    data_voucher<-cbind.data.frame(id, vouchers)
    return(data_voucher)})
  output<-bind_rows(output)
  columnas<-c("busqueda", "database_id", "name", "VoucherID")
  if(links){
    columnas<-c(columnas, "image_resized_10","image_resized_60")  }
  output<-output[columnas]
  return(output)
}

#' @title get_species_by_regions
#'
#' @description
#' Obtener una lista de especies por region
#'
#' @returns Lista. Cada lista tiene las especies por grupo y region.
#' @param search str. Grupo o grupos a consultar. Pueden ser: genero, familia, o cualquier taxón superior.
#' @param region str. Regiones en las cual buscar. el formato es usando un keyword de 3 letras.
#' @examples
#' get_species_by_region(search="asteraceae", region=c("BIO", "ARA"))
#'
#' @export
get_species_by_region<-function(search, region=NULL){
  urls<-generate_urls(search, region)
  output_data<-Map(function(id, url){
    data<-fetch_data(url = url, species = id)
    data<-data[data$type=="species",]
    data<-data[c("unique_taxon_id", "name", "genus_name","status",
                 "habit", "determined" )]
    data$status<-data$status$name
    return(data)
  }, urls[[2]], urls[[1]])
  return(output_data)
}


    ###################################
   ###                            ####
  ###      ####                  3333
 ###                            ####
###################################
