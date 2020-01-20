############################################################
###
### Europa Universalis Save Analysis App
###      FUNCTIONS
###
###   Author: Canadice
###   Revised: 2019-06-07
###
############################################################


############################
### Compile data
############################
data_compiler <- function(vector_data){
  # Gets position of all starting and ending braces in the file
  start_brace <- which(str_detect(vector_data, pattern = "\\{"))
  end_brace <- which(str_detect(vector_data, pattern = "\\}"))
  
  # If there only exist one brace in the vector then it returns itself
  if(length(start_brace) <= 1){
    return(vector_data)
  }
  
  # If the first value contains a brace then skip
  if(start_brace[1] == 1){
    start_brace <- start_brace[-1]
  }
  
  # Creates a grouped indice for the groups of information
  indices <- list(NA)
  j <- 1
  for(i in 1:length(start_brace)){
    if((i+1) > length(start_brace)){
      indices[[j]] <- start_brace[i]:end_brace[i]
    } else if(end_brace[i] < start_brace[i + 1]){
      indices[[j]] <- start_brace[i]:end_brace[i]
      j <- j + 1
    } else {
      start_brace[i+1] <- start_brace[i]
    }
  }
  
  split_data_list <- lapply(X = indices, FUN = function(x){vector_data[x]})
  
  other_data <- vector_data[which(!((1:length(vector_data)) %in% unlist(indices)))]
  
  if(length(other_data) == 2){
    return(split_data_list)
  } else {
    return(append(other_data, split_data_list))  
  }
}


############################
### Structure data
############################
data_structurer <- function(save){
  # Removes all the \t text from the file
  save <- str_replace_all(save, pattern = "[\t\"]", replacement = "")
  
  
  # Checks for lines with two starting braces
  append <- which(str_detect(save, pattern = "\\{\\{"))
  if(length(append) > 0){
    for(i in 1:length(append)){
      save <- c(save[1:(append[i]-1)], 
              str_replace(save[append[i]], pattern = "\\{\\{", replacement = "\\{"), 
              "{", 
              save[(append[i]+1):length(save)])
    }
  }
  
  split_data_list <- data_compiler(save)
  
  names <- str_extract_all(lapply(split_data_list, `[[`, 1), pattern = "[a-z_]+=")
  
  names <- lapply(names, str_extract, pattern = "[a-z_]+")
  
  split_data_list <- lapply(X = split_data_list, FUN = data_compiler)
  
  names(split_data_list) <- names
  
  return(split_data_list)
}

############################
### Gather data
############################
information_gather <- function(data_list){
  
  data_list <- lapply(data_list, FUN = function(data_sub_list){
    
    index <- which(unlist(lapply(data_sub_list, FUN = function(data_sub_sub_list){
      all(str_detect(data_sub_sub_list, pattern = "=") & !str_detect(data_sub_sub_list, pattern = "[{}]"))
    })))
    
    str_split(data_sub_list[index], pattern = "=")
  })
  
  data_list <- lapply(data_list, FUN = function(x){
    data <- matrix(unlist(x), nrow = 2)
    data_frame <- data.frame(data, stringsAsFactors = FALSE)[-1,]
    
    colnames(data_frame) <- data[1,]
    
    data_frame
  })
  
  variables <- unlist(lapply(data_list, colnames))
  
  data_frame <- as.data.frame(matrix("", ncol = length(unique(variables))), stringsAsFactors = FALSE)
  colnames(data_frame) <- unique(variables)
  
  for(i in 1:length(data_list)){
    x <- data_list[[i]]
    
    index <- which(colnames(data_frame) %in% colnames(x))
    
    temp <- data_frame[1,]
    
    temp[, index] <- x[, colnames(data_frame)[index]]
    
    data_frame <- rbind(data_frame, temp)
    
    if(i == length(data_list)){
      data_frame <- data_frame[-1,]
    }
  }
  
  num_indices <- apply(data_frame, 
                       MARGIN = 2, 
                       FUN = function(x){
                         !any(str_detect(x, pattern = "[a-zA-Z]"))
                       })
  
  data_frame[,num_indices] <- suppressWarnings(sapply(data_frame[, num_indices], FUN = as.numeric))

  return(data_frame)
}


############################
### Province data
############################
province_compiler <- function(provinces){
  names <- str_extract_all(lapply(provinces, `[[`, 1), pattern = "[0-9]+")
  provinces <- lapply(X = provinces, FUN = data_compiler)
  names(provinces) <- names
  
  province_data <- information_gather(provinces)

  # Adds province ID
  province_data$ID <- as.numeric(unlist(names))
  
  # Removes provinces that seem to be empty?
  province_data <- province_data[which(!(province_data$owner == "")),]
  
  return(province_data)
}

############################
### Country data
############################

country_compiler <- function(countries){
  names <- str_extract_all(lapply(countries, `[[`, 1), pattern = "[A-Z]+")
  countries <- lapply(X = countries, FUN = data_compiler)
  names(countries) <- names
  
  country_data <- information_gather(countries)
  
  country_data$tag <- c("", unlist(names))
  
  country_data <- country_data[which(!is.na(country_data$num_of_cities)),]  
  
  # Counts the army size of a nation
  army_variables <- c("num_of_banners", "num_of_janissaries",
                      "num_of_regulars", "num_of_mercenaries",
                      "num_of_streltsy", "num_of_cossacks")
  
  country_data$army_size <- rowSums(country_data[, army_variables[army_variables %in% colnames(country_data)]],
                                    na.rm = TRUE)
  
  return(country_data)
}

############################
### Meta-data
############################

############################
### Time-series data
############################
ts_compiler <- function(ts){
  ts_list <- lapply(ts, FUN = data_compiler)
  
  ts_list <- lapply(ts_list, FUN = function(x){
    if(length(x) == 3){
      NULL
    } else {
      tag <- str_extract(unlist(x), pattern = "[A-Z]+")
      
      data <- x[[4]][2]
      
      data <- str_split(data, pattern = "[ =]")
      
      data <- lapply(data, FUN = function(x){
        data <- matrix(unlist(x[-length(x)]), nrow = 2)
        data_frame <- data.frame(data, stringsAsFactors = FALSE)[-1,]
        
        names(data_frame) <- data[1,]
        
        data_frame
      })
      data <- as.data.frame(t(unlist(data)), stringsAsFactors = FALSE)
      data$tag <- tag[!is.na(tag)]
      
      data
    }
  })
  
  ts_list <- ts_list[!unlist(lapply(ts_list, FUN = is.null))]
  
  variables <- unlist(lapply(ts_list, colnames))
  
  data_frame <- as.data.frame(matrix("", ncol = length(unique(variables))), stringsAsFactors = FALSE)
  colnames(data_frame) <- unique(variables)
  
  for(i in 1:length(ts_list)){
    x <- ts_list[[i]]
    
    index <- which(colnames(data_frame) %in% colnames(x))
    
    temp <- data_frame[1,]
    
    temp[, index] <- x[, colnames(data_frame)[index]]
    
    data_frame <- rbind(data_frame, temp)
    
    if(i == length(ts_list)){
      data_frame <- data_frame[-1,]
    }
  }
  
  num_indices <- apply(data_frame, 
                       MARGIN = 2, 
                       FUN = function(x){
                         !any(str_detect(x, pattern = "[a-zA-Z]"))
                       })
  
  data_frame[,num_indices] <- suppressWarnings(sapply(data_frame[, num_indices], FUN = as.numeric))
  
  data_frame <- gather(data_frame, "year", "value", -"tag")
  
  data_frame$year <- as.numeric(data_frame$year)
  
  return(data_frame)
}

############################
### Save scraper
############################
save_processing <- function(save){
  # Imports a list of tags and responding names of nation
  tags <- read.csv2("tags.csv", stringsAsFactors = FALSE)
  
  ##########################################################################
  ### Subsetting
  ##########################################################################
  
  structure <- data_structurer(save)
  
  rm(save)
  
  ##########################################################################
  ### Provinces
  ##########################################################################
  provinces <- province_compiler(structure$provinces)
  
  ##########################################################################
  ### Countries
  ##########################################################################
  countries <- country_compiler(structure$countries)
  
  # Merges with the province data for each of the countries' capitals
  countries <- countries %>% 
    left_join(provinces[, c("ID", "hre_liberated")], by = c("capital" = "ID"))
  
  # Merges with the country names from the tags
  if(any(str_detect(structure$mod_enabled, pattern = "Voltaire"))){
    countries <- countries %>% 
      inner_join(tags[tags$Region == "Voltaire",], by = c("tag" = "Tag"))  
  } else {
    countries <- countries %>% 
      inner_join(tags[tags$Region != "Voltaire",], by = c("tag" = "Tag"))  
  }
  
  ##########################################################################
  ### Meta-data
  ##########################################################################
  meta_list <- structure[c("date", 
                          "save_game",
                          "current_age",
                          "start_date",
                          "savegame_versions",
                          "multi_player")]
  
  meta_list$savegame_versions <- str_extract(meta_list$savegame_versions, 
                                             "[0-9]{1}.[0-9]{2}.[0-9]{1}.[0-9]{1}")[!is.na(str_extract(meta_list$savegame_versions, 
                                             "[0-9]{1}.[0-9]{2}.[0-9]{1}.[0-9]{1}"))]
  
  meta_list <- str_split(meta_list, pattern = "=")
  
  meta_list <- lapply(meta_list, FUN = function(x){
    data <- matrix(unlist(x), nrow = 2)
    data_frame <- data.frame(data, stringsAsFactors = FALSE)[-1,]
    
    names(data_frame) <- data[1,]
    
    data_frame
  })
  
  meta_data <- unlist(meta_list)
  
  names(meta_data)[5] <- "name"
  
  meta_data <- as.data.frame(t(meta_data))
  
  ##########################################################################
  ### Time-series data
  ##########################################################################
  ts_lists <- list(income = structure$income_statistics, 
                   nation_size = structure$nation_size_statistics,
                   score = structure$score_statistics,
                   inflation = structure$inflation_statistics)
    
  ts_lists <- lapply(ts_lists, ts_compiler)
  
  ##########################################################################
  ### Aggregating to end
  ##########################################################################
  resulting_data <- list(meta = meta_data, province = provinces, 
                         country = countries, time_series = ts_lists)
  
  return(resulting_data)  
}



# # Time-series


































  





