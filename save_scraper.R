save_processing <- function(save){
  # Sources the different functions needed
  source("data_compiler.R")
  
  # Imports a list of tags and responding names of nation
  tags <- read.csv2("tags.csv")
  
  ##########################################################################
  ### Splits the save file into the province parts, starting at the first province
  ### Last list object contains the last province information + all the rest of the save...
  # Detects where to start the individual province data
  starts <- which(str_detect(save, pattern = "^-[0-9]+=\\{"))
  
  # As starts portion off the different provinces, ends are the one line before
  ends <- c((starts-1)[-1], length(save))
  
  # Splits the data into a list of their individual province information
  indices <- do.call(list, mapply(seq, starts, ends))
  province_data_split <- lapply(indices, FUN = function(x){save[x]})
  
  ##########################################################################
  ### Splits the save file into the nations parts, starting at the first nation
  ### Last list object contains the last nation information + all the rest of the save...
  # Detects where to start looking for country data
  start <- which(str_detect(save, pattern = "^countries=\\{"))
  
  # Detects position of } as it defines end of information block.
  # As the game progresses, } without any tabs are inserted resulting in the following 
  # solution to make the last list-object takes the whole rest of the save to the end. 
  end <- which(str_detect(save, pattern = "^\\}"))
  
  end <- end[length(end)]
  
  # Subsets just the country data, might be unecessary.
  country_data <- save[start:end]
  
  # Same as above with provinces
  starts <- which(str_detect(country_data, pattern = "^\t[A-Z]{3}=\\{"))
  ends <- c((starts-1)[-1], length(country_data))
  
  indices <- do.call(list, mapply(seq, starts, ends))
  country_data_split <- lapply(indices, FUN = function(x){country_data[x]})
  
  # Removes original save and indices to save at least some working space
  rm(list = c("end", "ends", "start", "starts", "indices", "save"))
  
  ##########################################################################

  # Compiles the province data to a list
  data <- lapply(province_data_split, FUN = province_information_compiler)
  
  # Takes all information in the list and concatenate into a data frame
  province_data <- data.frame(matrix(unlist(data), ncol=ncol(data[[1]]), byrow=TRUE, dimnames = list(NULL, colnames(data[[1]]))))
  
  # Compiles the country data to a list
  data <- lapply(country_data_split, FUN = country_information_compiler)
  
  # Takes all information in the list and concatenate into a data frame
  country_data <- data.frame(matrix(unlist(data), ncol=ncol(data[[1]]), byrow=TRUE, dimnames = list(NULL, colnames(data[[1]]))))
  
  # Converts the numerical country data to numeric
  country_data[,4:10] <- apply(apply(country_data[,4:10], MARGIN = 2, FUN = as.character), MARGIN = 2, FUN = as.numeric)
  
  # Merges with the province data for each of the countries' capitals
  country_data <- country_data %>% left_join(province_data, by = "capital")
  
  # Merges with the country name data from tags
  country_data <- country_data %>% inner_join(tags, by = "Tag")
  
  # Returns a data set with all countries that have a continent value (NA usually indicate that they do not exist at the time of the save)
  return(country_data[which(!is.na(country_data$continent)), ])  
}

