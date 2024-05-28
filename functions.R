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
#### Define a function to get data for a single ID
get_id_data <- function(id) {
  url <- paste0("https://api.herbariodigital.cl/species/", id, "/?format=json")
  response <- GET(url)
  
  if (status_code(response) == 200) {
    content <- fromJSON(content(response, "text"), flatten = TRUE)
    return(content)
  } else {
    message(paste("Request failed for ID:", id))
    return(NULL)
  }
}

# Define a function to loop through a list of IDs and collect the data
collect_data <- function(id_list) {
  results <- lapply(id_list, get_species_data)
  # Remove NULL results
  results <- results[!sapply(results, is.null)]
  return(results)
}

# Define a function to convert the collected data into a data frame
create_data_frame <- function(results) {
  # Initialize vectors to store components
  ids <- vector("list", length(results))
  scientific_names <- vector("list", length(results))
  scientific_name_authorships <- vector("list", length(results))
  synonyms <- vector("list", length(results))
  
  # Function to safely extract a component, returning NA if not found
  safe_extract <- function(x, name) {
    if (!is.null(x[[name]])) {
      return(x[[name]])
    } else {
      return(NA)
    }
  }
  
  # Iterate over each element in results and extract components
  for (i in seq_along(results)) {
    ids[[i]] <- safe_extract(results[[i]], "id")
    scientific_names[[i]] <- safe_extract(results[[i]], "scientific_name")
    scientific_name_authorships[[i]] <- safe_extract(results[[i]], "scientific_name_authorship")
    synonyms[[i]] <- safe_extract(results[[i]], "synonyms")
  }
  
  # Combine into a data frame
  df <- data.frame(
    id = unlist(ids),
    scientific_name = unlist(scientific_names),
    scientific_name_authorship = unlist(scientific_name_authorships),
    stringsAsFactors = FALSE
  )
  
  # Split synonyms into separate columns
  max_synonyms <- max(lengths(synonyms))
  for (i in 1:max_synonyms) {
    df[[paste0("synonym_", i)]] <- sapply(synonyms, function(x) ifelse(length(x) >= i, x[[i]], NA))
  }
  
  print(df)
}