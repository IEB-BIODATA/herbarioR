
## API Documentation: https://api.herbariodigital.cl/swagger-ui/


# Define a function to get data by a vector of species names with fuzzy matching
get_species_data <- function(species_names, known_species_file, max_dist = 0.2) {

  # Read the known species from the CSV file
  known_species_df <- read.csv("known_species.csv", header = FALSE, stringsAsFactors = FALSE)

  # Reading the names
  known_species <- as.character(known_species_df$V2)

  # Ensure the input vector is a character vector
  species_names <- as.character(species_names)

  # Function to get the closest match for a species name
  get_closest_match <- function(species_name, known_species, max_dist) {
    if (is.na(species_name) || species_name == "") {
      return(NA)  # Ignore if matched name is NA or empty
    }
    distances <- stringdist::stringdist(species_name, known_species, method = "jw")
    min_dist <- min(distances)
    closest_match <- known_species[which.min(distances)]
    if (min_dist <= max_dist && !is.na(closest_match) && closest_match != "") {
      return(closest_match)
    } else {
      if (min_dist > 0.1) {
      return(NA)  # Set MatchedName to NA if min_dist is greater than 0.1
    }  
    }
  }

  # Initialize the results data frame with species_names and MatchedName
  results <- data.frame(
    species_names = species_names,
    MatchedName = sapply(species_names, get_closest_match, known_species, max_dist),
    stringsAsFactors = FALSE
  )
  
  # Remove rows with NA matched names
  results <- results[!is.na(results$MatchedName), ]
  
  # Remove rows with empty matched names
  results <- results[results$MatchedName != "", ]
  
  # Prepare to store the extracted data
  final_results <- data.frame()

  # Retrieve specific fields from each URL using GET requests
  for (i in seq_along(results$MatchedName)) {
    matched_name <- results$MatchedName[i]
    original_name <- results$species_names[i]
    
    url <- paste0("https://api.herbariodigital.cl/species_list/?format=json&search=", URLencode(matched_name))
    
    response <- tryCatch({
      GET(url)
    }, error = function(e) {
      message(paste("Error occurred while fetching data for", matched_name, ":", conditionMessage(e)))
      return(NULL)
    })
    
    if (!is.null(response) && status_code(response) == 200) {
      content <- content(response, "text")
      parsed_content <- fromJSON(content)
      
      if (length(parsed_content$results) > 0) {
        # Extract specific fields from the results
        fields_to_extract <- c("id","scientific_name","type")
        extracted_data <- parsed_content$results[, fields_to_extract, drop = FALSE]
        
        # Add original and matched names to the extracted data
        extracted_data$OriginalName <- original_name
        extracted_data$MatchedName <- matched_name
        
        # Bind the extracted data to the final results data frame
        final_results <- bind_rows(final_results, extracted_data)
      } else {
        message(paste("No results found for matched name:", matched_name))
      }
    } else {
      message(paste("Request failed for URL:", url, "with status code:", status_code(response)))
    }
  }
  
  return(final_results)
}


##IN PROGRESS
# Define a function to get data for a list of names

# Function to get the closest match for a species name
get_closest_match <- function(species_name, max_dist = 0.15) {
  require(stringdist)
  known_species_df <- read.csv("known_species_id_type.csv", 
                               header = FALSE, 
                               stringsAsFactors = FALSE,
                               sep = ";")
  known_species <- as.character(known_species_df$V2)
  distances <- stringdist::stringdist(tolower(species_name), 
                                      tolower(known_species), 
                                      method = "jw",
                                      q=4)
  min_dist <- min(distances)
  val<-distances <= max_dist
  if (min_dist <= max_dist) {
    hj<-known_species[which.min(distances)]
    hj<-known_species_df[known_species_df$V2==hj,]
    hj<-cbind.data.frame(species_name, hj)
    colnames(hj)<-c("input_name","id_match", "matched_name", "type")
    return(hj)
  } else if (min_dist >= max_dist) {
    return(NA)
  }
}


  
#####################################################################################


get_taxonomy <- function(species_name) {
  require(jsonlite)
  require(dplyr)
  param <- c("id", "scientific_name", "genus",
             "family", "order", "class_name",
             "division", "kingdom")
  p <- paste0(param, ".id")
  api_url<-"https://api.herbariodigital.cl/species_list/?format=json&search="
  
  workname <- lapply(species_name, get_closest_match)
  df <- lapply(workname, function(x) {
    url <- paste0(api_url, 
                  URLencode(x$matched_name)) 
    a <- fromJSON(url)$results
    a<-a[a$determined==T,]
    if (!is.null(a)) {
      if (any(a$type == "synonymy")&nrow(a)==1) {
        b<-a$species[[1]]
        a[,param]<-b[,param]
      }
      present_columns <- param[param %in% names(a)]
      a <- a[, present_columns, drop = FALSE] 
      a <- flatten(a)
      rownames(a)<-NULL
      rownames(x)<-NULL
      a<-cbind.data.frame(x,a)
      return(a)
    } 
  })
  
  df <- bind_rows(df, .id = "source")
  df[, p[p %in% names(df)]] <- NULL
  df[,"source"] <- NULL
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





########################################################################################################
#### Define a function to get data for a single Family
