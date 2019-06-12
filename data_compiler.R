# Compiles the following information using function information_finder from value_scraper
information_finder <- function(vector, pattern){
  # Loads required packages and functions
  require(stringr, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  
  # Otherwise all the other standard values wanted should be right after the information tag (pattern) and =
  if(pattern != "^\t\tcontinent=\\{"){
    if(!any(str_detect(vector, pattern))){
      return(NA)
    } else {
      # Finds the element in the current list where the pattern exists
      data <- vector[which(str_detect(vector, pattern))]
      
      # Takes the value as it comes after the splitting symbol "="
      data <- unlist(str_split(data, pattern = "="))[2]
    }
    
    # Since continent is a vector of six 0s and 1s this one is different
  } else {
    data <- vector[which(str_detect(vector, pattern))+1]
    
    data <- str_trim(str_sub(data, start = 4), side = "right")
    
    data <- str_extract(unlist(str_split(data, " ")), "[0-9]")
    
    if(any(data == 1)){
      data <- which(data == 1)[1]  
    } else {
      return(NA)
    }
  }
  return(data)
}


province_information_scraper <- function(list_vector){
  # Loads required packages and functions
  require(stringr, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  
  information_indices <- str_detect(list_vector, pattern = "=") & !str_detect(list_vector, pattern = "[{}]")
  
  information <- list_vector[information_indices]  
  
  # Just takes the data that is current, removes the history information that exist in a province
  information_indices <- str_detect(information, pattern = "^\t\t[a-z]")
  
  information <- information[information_indices]
  
  clean_information <- str_replace_all(information, pattern = "[\t\"]", replacement = "")
  
  clean_information_matrix <- matrix(unlist(str_split(string = clean_information, pattern = "=")), ncol = length(clean_information), byrow = FALSE)
  
  var_names <- clean_information_matrix[1,]
  
  # Checks duplicates
  if(any(table(var_names)>1)){
    duplicates <- table(var_names)[which(table(var_names)>1)]
    
    for(name in names(duplicates)){
      var_names[var_names == name] <- paste(var_names[var_names == name], 1:duplicates[names(duplicates) == name], sep = "_") 
    }
  }
  
  clean_information_data <- as.data.frame(t(data.frame(values = clean_information_matrix[2,], row.names = var_names)), stringsAsFactors = FALSE)
  
  num_indices <- sapply(clean_information_data, FUN = function(x){!is.na(suppressWarnings(as.numeric(x)))})
  
  clean_information_data[,num_indices] <- sapply(clean_information_data[,num_indices], FUN = as.numeric)
  
  return(clean_information_data)  
}

meta_information_scraper <- function(vector){
  # Searches the vector of meta data for the pieces of information. 
  # Does not look at multiple lines of information linked to {}.
  information_indices <- str_detect(vector, pattern = "=") & !str_detect(vector, pattern = "[{}]")
  
  information <- vector[information_indices]  
  
  # Cleans the tab formats from the information
  clean_information <- str_replace_all(information, pattern = "[\t\"]", replacement = "")
  
  # Removes flags by identifying dates in the cells, without removing the vital DATE information
  clean_information <- clean_information[!(str_detect(clean_information, "[0-9]{4}\\.[0-9]{1,2}\\.[0-9]") & 
                                             !str_detect(clean_information, pattern = "date"))]
  
  # Transforms the vector to a matrix of the same information where the values are in one row and the "name" in the other
  clean_information_matrix <- matrix(unlist(str_split(string = clean_information, pattern = "=")), 
                                     ncol = length(clean_information), 
                                     byrow = FALSE)
  
  # Removes the new (for 1.23) comparison stats
  clean_information_matrix <- clean_information_matrix[,!(clean_information_matrix[1,] %in% c("value", "id", "comparison", "localization", 
                                                                                              "key", "selector", "sample_count", "sample_value",
                                                                                              "member", "name"))]
  
  var_names <- clean_information_matrix[1,]
  
  if(sum(var_names %in% "date")>1){
    clean_information_matrix <- clean_information_matrix[,-which(var_names == "date")[2]]
    
    var_names <- var_names[-which(var_names == "date")[2]]
  }
  
  clean_information_data <- as.data.frame(t(data.frame(values = clean_information_matrix[2,], row.names = var_names)), stringsAsFactors = FALSE)
  
  num_indices <- sapply(clean_information_data, FUN = function(x){!is.na(suppressWarnings(as.numeric(x)))})
  
  clean_information_data[,num_indices] <- sapply(clean_information_data[,num_indices], FUN = as.numeric)
  
  return(clean_information_data) 
}

country_information_compiler <- function(x){
  # Loads required packages and functions
  require(stringr, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  
  # Identifies lists of information and subsets (removes) them
  # a <- which(str_detect(x, pattern = "[A-Z]*=\\{"))[-1]
  a <- which(str_detect(x, pattern = "\\{"))[-1]
  b <- which(str_detect(x, pattern = "\\}"))[-length(a)]
  
  lists <- unlist(apply(X = cbind(a,b), MARGIN = 1, FUN = function(x){x[1]:x[2]}))
  
  information <- x[-c(1,lists)]
  
  tag <- str_extract(x[1], "[A-Z]{3}")
  
  # Cleans the tab formats from the information
  clean_information <- str_replace_all(information, pattern = "[\t\"]", replacement = "")
  
  # Transforms the vector to a matrix of the same information where the values are in one row and the "name" in the other
  clean_information_matrix <- matrix(unlist(str_split(string = clean_information, pattern = "=")), 
                                     ncol = length(clean_information), 
                                     byrow = FALSE)
  
  var_names <- clean_information_matrix[1,]
  
  # Checks duplicates
  if(any(table(var_names)>1)){
    duplicates <- table(var_names)[which(table(var_names)>1)]
    
    for(name in names(duplicates)){
      var_names[var_names == name] <- paste(var_names[var_names == name], 1:duplicates[names(duplicates) == name], sep = "_") 
    }
  }
  
  clean_information_data <- as.data.frame(t(data.frame(values = clean_information_matrix[2,], row.names = var_names)), stringsAsFactors = FALSE)
  
  clean_information_data <- cbind(tag, clean_information_data, stringsAsFactors = FALSE)
  
  num_indices <- sapply(clean_information_data, FUN = function(x){!is.na(suppressWarnings(as.numeric(x)))})
  
  clean_information_data[,num_indices] <- sapply(clean_information_data[,num_indices], FUN = as.numeric)
  
  return(clean_information_data)
}

province_information_compiler <- function(x){
  # Loads required packages and functions
  require(stringr, quietly = TRUE)
  require(dplyr, quietly = TRUE)

  PID <- as.numeric(str_extract(x[1], "[0-9]+"))
  hre <- as.character(information_finder(x, "hre="))
  
  other_information <- province_information_scraper(x)
  
  data <- cbind(PID, hre, other_information)
  
  return(data)
}
