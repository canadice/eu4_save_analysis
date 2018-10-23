save_processing <- function(save){
  # Sources the different functions needed
  source("data_compiler.R")
  
  # Imports a list of tags and responding names of nation
  tags <- read.csv2("tags.csv")
  
  ##########################################################################
  ### Subsetting
  ##########################################################################
  
  ############################
  ### META
  ############################
  ### Starts the splitting for meta-data
  # The first part of data existing in the save is area data, meta-data is located above
  meta_end <- which(str_detect(save, pattern = "^map_area_data"))[1]
  
  meta <- save[1:(meta_end - 1)]
  
  ############################
  ### Area data
  ############################
  ### Starts the splitting for area data
  area_end <- which(str_detect(save, pattern = "^total_military_power"))[1]
  
  area_data <- save[meta_end:(area_end-1)]
  
  ############################
  ### Country
  ############################
  ### Splits the save file into the nations parts, starting at the first nation
  ### Last list object contains the last nation information + all the rest of the save...
  # Detects where to start looking for country data
  start_country <- which(str_detect(save, pattern = "^countries=\\{"))
  
  # Detects position of } as it defines end of information block.
  # As the game progresses, } without any tabs are inserted resulting in the following 
  # solution to make the last list-object takes the whole rest of the save to the end. 
  # Subsets just the country data, might be unecessary.
  # 1.23 added active advisors to the save which screws with finding positions of nations' tags
  country_data <- save[start_country:(which(str_detect(save, pattern = "active_advisors=\\{"))-1)]
  
  starts <- which(str_detect(country_data, pattern = "^\t[A-Z]{3}=\\{"))
  ends <- c((starts-1)[-1], length(country_data))
  
  indices <- do.call(list, mapply(seq, starts, ends))
  country_data_split <- lapply(indices, FUN = function(x){country_data[x]})
  
  ############################
  ### Province
  ############################
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
  
  ############################
  ### Cleaning
  ############################
  # Removes original save and indices to save at least some working space
  rm(list = c("meta_end", "area_end", "ends", "starts", "starts_provinces", "ends_provinces", "start_country", "indices", "save"))
  
  ##########################################################################
  ### Structuring & scraping
  ##########################################################################
  
  ############################
  ### Meta data
  ############################  
  meta_data <- meta_information_scraper(vector = meta)
  
  ############################
  ### Provinces data
  ############################  
  # Compiles the province data to a list, parallel processing to speed the list up
  cl <- makeCluster(getOption("cl.cores", 4))
  clusterExport(cl, varlist = c("information_finder", "province_information_scraper"))
  
  data <- parLapply(cl = cl, X = province_data_split, fun = province_information_compiler)
  stopCluster(cl)
  
  # Takes all information in the list and concatenate into a data frame
  province_data <- data %>% 
    Reduce(function(dtf1,dtf2) suppressWarnings(bind_rows(dtf1,dtf2)), .)
  ###############
  ### Development
  ###############
  province_data$development <- province_data$base_tax + province_data$base_manpower + province_data$base_production
  
  ###############
  ### Structure
  ###############
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
  base <- which(str_detect(colnames(x = province_data), pattern = "^base") | str_detect(colnames(x = province_data), pattern = "development") | str_detect(colnames(x = province_data), pattern = "improve_count"))
  PID <- which(str_detect(colnames(x = province_data), pattern = "^PID"))
  originals <- which(str_detect(colnames(x = province_data), pattern = "^original"))
  info <- which(colnames(province_data) %in% c("name", "culture", "religion", "capital", "trade_goods", "trade_power",
                                               "trade", "center_of_trade", "local_autonomy", "hre", "owner")
  )

  ordering_index <- c(PID, info, base, buildings, fort, cores, claims, originals)
  
  province_data <- province_data[,c(ordering_index, (1:ncol(province_data))[-ordering_index])]
  
  ############################
  ### Country data
  ############################ 
  # Compiles the country data to a list
  cl <- makeCluster(getOption("cl.cores", 4))
  clusterExport(cl, varlist = c("information_finder"))
  
  data <- parLapply(cl = cl, X = country_data_split, fun = country_information_compiler)
  stopCluster(cl)

  # Takes all information in the list and concatenate into a data frame
  country_data <- data %>% 
    Reduce(function(dtf1,dtf2) suppressWarnings(bind_rows(dtf1,dtf2)), .)
  
  # Merges with the province data for each of the countries' capitals
  country_data <- country_data %>% left_join(province_data[, c("PID", "hre")], by = c("capital" = "PID"))
  
  # Merges with the country name data from tags
  country_data <- country_data %>% inner_join(tags, by = c("tag" = "Tag"))
  
  ind <- c("tag", "Name") 
  
  country_data <- country_data[, c(ind, colnames(country_data)[!colnames(country_data) %in% ind])]
  
  # Subsets nations that exist at the current save
  country_data <- country_data[which(country_data$capped_development > 0),]
  
  resulting_data <- list(meta = meta_data, province = province_data, country = country_data)
  
  
  return(resulting_data)  
}

