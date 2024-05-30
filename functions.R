
## API Documentation: https://api.herbariodigital.cl/swagger-ui/


# Define a function to get data by a vector of species names with fuzzy matching
get_species_data <- function(species_names, known_species_file, max_dist = 0.2) {

  # Read the known species from the CSV file
  known_species_df <- read.csv("known_species.csv", header = FALSE, stringsAsFactors = FALSE)

  # Reading the names
  known_species <- as.character(known_species_df$V1)

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
  known_species_df <- read.csv("known_species.csv", 
                               header = FALSE, 
                               stringsAsFactors = FALSE)
  known_species <- as.character(known_species_df$V1)
  distances <- stringdist::stringdist(tolower(species_name), 
                                      tolower(known_species), 
                                      method = "jw",
                                      q=4)
  min_dist <- min(distances)
  val<-distances <= max_dist
  if (min_dist <= max_dist) {
    return(known_species[which.min(distances)])
  } else if (min_dist >= max_dist) {
    return(NA)
  }
}


  

get_taxonomy <- function(species_name) {
  require(jsonlite)
  require(dplyr)
  workname<- sapply(species_name, 
                   get_closest_match)
  param<-c("id","scientific_name",  "genus_name", 
           "family", "order", "class_name",
           "division", "kingdom")
  p<-paste0(param, ".id")
  url <- paste0("https://api.herbariodigital.cl/species_list/?format=json&search=", 
               URLencode(workname))
  df<-lapply(url,function(x) {
    a<-fromJSON(x)$results
    #a<-a[a$type=="species",]
    a$genus<-NULL
    a<-a[,param]
    a<-flatten(a)
  })
  df<-bind_rows(df)
  df[,p]<-NULL
  return(df)
}
  # Assuming get_closest_match is defined elsewhere in your code
  workname <- sapply(species_name, get_closest_match)
  
  param <- c("id", "scientific_name", "genus_name", 
             "family", "order", "class_name",
             "division", "kingdom")
  p <- paste0(param, ".id")
  
  url <- paste0("https://api.herbariodigital.cl/species_list/?format=json&search=", 
                URLencode(workname))
  
  df <- lapply(url, function(x) {
    # Try to get data from the API
    tryCatch({
      a <- fromJSON(x)$results
      if (!is.null(a)) {
        #a <- a[a$type == "species", ]
        a$genus <- NULL
        # Check which columns are present before subsetting
        present_columns <- param[param %in% names(a)]
        a <- a[, present_columns, drop = FALSE] 
        a <- flatten(a)
        return(a)
      } else {
        warning(paste("No results found for URL:", x))
        return(NULL)
      }
    }, error = function(e) {
      warning(paste("Error processing URL:", x, "Message:", e$message))
      return(NULL)
    })
  })
  
  df <- bind_rows(df, .id = "source")
  df[, p[p %in% names(df)]] <- NULL
  
  return(df)
}



#### Define a function to get data for a single Family
