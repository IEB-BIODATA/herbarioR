require(jsonlite, quietly = T)
require(dplyr, quietly = T)
require(stringdist, quietly = T)
###############################
# Function to get the closest match for a species name
get_closest_match <- function(species_name, max_dist = 0.15) {
  
  known_species_df <- read.csv("herbarium_public_finder_view.csv", 
                               header = FALSE, 
                               stringsAsFactors = FALSE,
                               sep = ",")
  known_species <- as.character(known_species_df$V2)
  if (!(tolower(species_name) %in% tolower(known_species))) {
    message(species_name, ": This name might not have been added to the catalog yet")
  }
  distances <- stringdist::stringdist(tolower(species_name), 
                                      tolower(known_species), 
                                      method = "jw",
                                      q=4)
  min_dist <- min(distances)
  if (min_dist <= max_dist) {
    hj<-known_species[which.min(distances)]
    hj<-known_species_df[known_species_df$V2==hj,]
    hj<-cbind.data.frame(species_name, hj)
    rownames(hj)<-NULL
    colnames(hj)<-c("input_name","id_match", "matched_name", "type")
    return(hj)
  } else if (min_dist >= max_dist) {
    return(NA)
  }
}

################################################################################
######get_taxonomy
get_taxonomy <- function(species_name) {
  param <- c("id", "scientific_name", "genus",
             "family","order","class_name","division","kingdom")
  p<-paste0(param,".id")
  api_url<-"https://api.herbariodigital.cl/species_list/?format=json&search="
  workname <- na_omit_list(lapply(species_name, get_closest_match))
  df <- lapply(workname, function(x) {
    if(x$type=="species"){
      url <- paste0(api_url, 
                    URLencode(x$matched_name)) 
      a <- fetch_data(url, species = x$matched_name)$results
      a<-a[a$determined==T,]
      a <- a[, param] 
      a <- flatten(a)
      rownames(a)<-NULL
      rownames(x)<-NULL
      x <- x[rep(seq_len(nrow(x)), each = nrow(a)), ]
      a<-cbind.data.frame(x,a)
        return(a)
      }else if(x$type=="synonymy"){
        a<-get_valid_name(x, use ="nested")
        a<-a[,param]
        a<-flatten(a)
        rownames(a)<-NULL
        rownames(x)<-NULL
        a<-cbind.data.frame(x,a)
        return(a)
    }} )
  
  df <- bind_rows(df, .id = "source")
  df[, p] <- NULL
  df[,"source"] <- NULL
  rownames(df)<-NULL
  colnames(df)<-c("input_name","id_match",
                "matched_name","type","valid_name_id",
                "valid_name","kingdom","genus",
                "family","order","class_name","division")
df<-df[,c("type","input_name","id_match","matched_name",
          "valid_name_id","valid_name","genus","family",
          "order","class_name","division","kingdom")]
  return(df)
}
########################################################################################################
########get valid name in base of synonyms
####use="user"
###use="nested"

get_valid_name <- function(name, use = "user") {
  if(use=="user" & length(name)==1){
    data <- get_closest_match(name)
  }else if(use=="nested" & length(name)==4){
    data<-name
  }else{
    stop("Error: Unvalid data format. Look at docs.")
  }
  colnam <- c('id','scientific_name','genus','specific_epithet','scientific_name_authorship',
              'subspecies','ssp_authorship','variety','variety_authorship','form',
              'form_authorship','kingdom','division','class_name','family','order',
              'habit','determined','common_names','status','minimum_height','maximum_height',
              'conservation_state','id_mma','region','herbarium_url')
  if (is.null(data)) {
    stop("Error: Unable to retrieve data for the provided name.")
  }
  if (data$type == "synonymy") {
    url <- paste0("https://api.herbariodigital.cl/synonymy/", 
                  data$id_match, "/?format=json")
    data_fetch <- fetch_data(url, species = data$matched_name)
    match <- as.data.frame(data_fetch[c("id", "name")])
    colnames(match) <- c("id_synonym", "Input_Name")
    sp <- data_fetch$species
    if (use == "user") {
      sp <- sp[, c("id", "name")]
      colnames(sp) <- c("valid_id", "valid_name")
    } else if (use == "nested") {
      sp <- sp[, intersect(colnam, colnames(sp))]
    }
    output <- cbind.data.frame(match, sp)
    return(output)
  } else {
    message(paste0(name, ": Input is already a valid name, not a synonym."))
    return(NA)
  }
}
########################################################################################
get_synonyms <- function(name) {
  if(length(name)==1){
  name_match <- get_closest_match(name)
  if (name_match$type == "species") {
    url <- paste0("https://api.herbariodigital.cl/species/", 
                  name_match$id_match, "/?format=json")
    data <- fetch_data(url)
    
    # Check if data was fetched successfully
    if (!is.null(data)) {
      # Extract id, name, and synonyms
      synonyms <- data$synonyms
      if (length(synonyms) > 0) {
        syn_names <- paste0("synonym_", seq_along(synonyms))
        synonyms_df <- data.frame(t(synonyms))
        colnames(synonyms_df) <- syn_names
      } else {
        synonyms_df <- data.frame(matrix("None synonyms found", ncol = 1, nrow = 1))
        colnames(synonyms_df)<-"synonyms"
      }
      result <- data.frame(input=name,id = data$id, name = data$name, synonyms_df)
      return(result)
    } else {
      return(NA)
    }
  } else {
    message(paste0(name,": Input is a synonym, not a species."))
    return(NA)
  }}else{
    stop("This function doesn't admit various rows.\nplease use lapply\nusage:\n   output<-lapply(species2test,get_synonyms)")
  }
}

################################helpers

na_omit_list<-function(x){
  is_na<-function(y){
    !all(is.na(y))}
  output<-Filter(is_na, x)
  return(output)
}
###################################
fetch_data <- function(url, species) {
  require(jsonlite)
  response <- tryCatch(
    expr = {
      jsondata<-fromJSON(url)
      message(species, ": Data fetched correctly")
      return(jsondata)
    },
    error = function(e) {
      message("Error fetching data from API:", e$message)
      return(NULL)
    }
  )
  return(response)
}
###############################