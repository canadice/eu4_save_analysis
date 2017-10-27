save_processing <- function(save){
  # Sources the different functions needed
  source("data_compiler.R")
  
  # Imports a list of tags and responding names of nation
  tags <- read.csv2("tags.csv")
  
  ##########################################################################
  ### Splits the save file into the nations parts, starting at the first nation
  ### Last list object contains the last nation information + all the rest of the save...
  # Detects where to start looking for country data
  start_country <- which(str_detect(save, pattern = "^countries=\\{"))
  
  # Detects position of } as it defines end of information block.
  # As the game progresses, } without any tabs are inserted resulting in the following 
  # solution to make the last list-object takes the whole rest of the save to the end. 
  # Subsets just the country data, might be unecessary.
  country_data <- save[start_country:length(save)]
  
  # Same as above with provinces
  starts <- which(str_detect(country_data, pattern = "^\t[A-Z]{3}=\\{"))
  ends <- c((starts-1)[-1], length(country_data))
  
  indices <- do.call(list, mapply(seq, starts, ends))
  country_data_split <- lapply(indices, FUN = function(x){country_data[x]})
  
  ##########################################################################
  ### Splits the save file into the province parts, starting at the first province
  ### Last list object contains the last province information + all the rest of the save...
  # Detects where to start the individual province data
  starts_provinces <- which(str_detect(save, pattern = "^-[0-9]+=\\{"))
  
  # As starts portion off the different provinces, ends are the one line before
  # The list of provinces end where countries start so the last province will end on the line before
  ends_provinces <- c((starts_provinces-1)[-1], start_country-1)
  
  # Splits the data into a list of their individual province information
  indices <- do.call(list, mapply(seq, starts_provinces, ends_provinces))
  province_data_split <- lapply(indices, FUN = function(x){save[x]})
  
  # Removes original save and indices to save at least some working space
  rm(list = c("ends", "starts", "starts_provinces", "ends_provinces", "start_country", "indices", "save"))
  
  ##########################################################################

  # Compiles the province data to a list, parallel processing to speed the list up
  cl <- makeCluster(getOption("cl.cores", 4))
  clusterExport(cl, varlist = c("information_finder", "province_information_scraper"))
  
  data <- parLapply(cl = cl, X = province_data_split, fun = province_information_compiler)
  stopCluster(cl)
  
  # Takes all information in the list and concatenate into a data frame
  province_data <- data %>% 
    Reduce(function(dtf1,dtf2) suppressWarnings(bind_rows(dtf1,dtf2)), .)
  
  # Finds the column index for sorting
  buildings <- which(colnames(province_data) %in% c("marketplace", "workshop", "temple", "barracks", "shipyard", "fort_15th",
                                                   "courthouse", "dock", "regimental_camp", "fort_16th",
                                                   "cathedral", "university", "trade_depot", "grand_shipyard", "training_fields", "fort_17th",
                                                   "stock_exchange", "counting_house", "town_hall", "drydock", "conscription_center", "fort_18th",
                                                   "wharf", "weapons", "textile", "plantations", "tradecompany")
                     )

  fort <- which(str_detect(colnames(x = province_data), pattern = "^fort_inf"))
  cores <- which(str_detect(colnames(x = province_data), pattern = "^core"))
  claims <- which(str_detect(colnames(x = province_data), pattern = "^claim"))
  base <- which(str_detect(colnames(x = province_data), pattern = "^base"))
  PID <- which(str_detect(colnames(x = province_data), pattern = "^PID"))
  originals <- which(str_detect(colnames(x = province_data), pattern = "^original"))
  info <- which(colnames(province_data) %in% c("name", "culture", "religion", "capital", "trade_goods", "trade_power",
                                               "trade", "local_autonomy", "hre", "owner")
  )

  ordering_index <- c(PID, info, base, buildings, fort, cores, claims, originals)
  
  province_data <- province_data[,c(ordering_index, (1:ncol(province_data))[-ordering_index])]
  
  # Compiles the country data to a list
  data <- lapply(country_data_split, FUN = country_information_compiler)
  
  # Takes all information in the list and concatenate into a data frame
  country_data <- data.frame(matrix(unlist(data), ncol=ncol(data[[1]]), byrow=TRUE, dimnames = list(NULL, colnames(data[[1]]))))
  
  # Converts the numerical country data to numeric
  country_data[,c(4:10, 12)] <- apply(apply(country_data[,c(4:10, 12)], MARGIN = 2, FUN = as.character), MARGIN = 2, FUN = as.numeric)
  
  # Merges with the province data for each of the countries' capitals
  country_data <- country_data %>% left_join(province_data[, c("PID", "hre")], by = c("capital" = "PID"))
  
  # Merges with the country name data from tags
  country_data <- country_data %>% inner_join(tags, by = c("tag" = "Tag"))
  
  ind <- c("tag", "Name") 
  
  country_data <- country_data[, c(ind, colnames(country_data)[!colnames(country_data) %in% ind])]
  
  resulting_data <- list(province = province_data, country = country_data[which(!is.na(country_data$continent)),])
  
  # Returns a data set with all countries that have a continent value (NA usually indicate that they do not exist at the time of the save)
  return(resulting_data)  
}

